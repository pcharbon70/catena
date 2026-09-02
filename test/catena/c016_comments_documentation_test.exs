defmodule Catena.C016CommentsDocumentationTest do
  use ExUnit.Case, async: true

  alias Catena.{Interface, LanguageLifecycle, LanguageVersion, SourceSpan}
  alias Catena.Comment.{Attachment, Target}
  alias Catena.Layout.{LineBreak, Semicolon, Token}

  @tag obligations: ~w(CM-OBL-001 CM-OBL-011 CM-OBL-012)
  test "0.1.12 is a source-only comment revision with exact abstract frontends" do
    assert LanguageVersion.latest() == "0.1.45"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45)

    refute "0.1.12" in LanguageVersion.compilable_revisions()
    refute "0.1.12" in LanguageVersion.interface_versions()
    refute "0.1.12" in LanguageVersion.artifact_versions()
    refute "0.1.12" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} ==
             LanguageLifecycle.state("comments-and-documentation-comments", "0.1.12")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-12-comments-and-documentation-comments")
      )

    assert change["affects"] == ~w(source-acceptance static-meaning diagnostics)

    assert {:error,
            %{
              id: "EDN001",
              details: %{
                frontend: "comments-and-documentation-comments",
                required: "0.1.12"
              }
            }} = Catena.scan_comment("// old", language_selection: selection("0.1.11"))

    assert {:ok, %{selection: %{language_revision: "0.1.11"}}} = Catena.resolve_layout([])
    assert {:ok, %{selection: %{language_revision: "0.1.45"}}} = Catena.decode_source_text("")
    assert {:error, _diagnostic} = Interface.decode(forged_interface())

    assert Catena.scan_comment("// deterministic") == Catena.scan_comment("// deterministic")
    ordinary = scan!("// deterministic")
    assert Catena.resolve_comments([ordinary]) == Catena.resolve_comments([ordinary])
  end

  @tag obligations: ~w(CM-OBL-002 CM-OBL-003 CM-OBL-004)
  test "line and block delimiters have stable documentation edge cases" do
    assert %{kind: :ordinary, form: :line, body: " ordinary"} = scan!("// ordinary")
    assert %{kind: :documentation, form: :line, body: "docs"} = scan!("/// docs")
    assert %{kind: :ordinary, form: :line, body: "// ordinary"} = scan!("//// ordinary")

    assert %{kind: :ordinary, form: :line, body: "! not inner docs"} =
             scan!("//! not inner docs")

    assert %{kind: :documentation, form: :block, body: "docs"} = scan!("/** docs */")
    assert %{kind: :ordinary, form: :block, body: "*"} = scan!("/***/")
    assert %{kind: :ordinary, form: :block, body: ""} = scan!("/**/")

    assert %{kind: :ordinary, form: :block, body: "! not inner docs "} =
             scan!("/*! not inner docs */")

    assert {:ok, result} = Catena.scan_comment("// first\nnext")
    assert result.next_unit_index == 8
    assert Enum.at(decode!("// first\nnext").units, result.next_unit_index).scalar == ?\n
  end

  @tag obligations: ~w(CM-OBL-003 CM-OBL-009)
  test "block comments nest iteratively and report unterminated depth" do
    source = "/* outer /* inner /** still nested */ tail */ outer */x"
    assert {:ok, result} = Catena.scan_comment(source)
    assert result.comment.kind == :ordinary
    assert result.comment.form == :block
    assert Enum.at(decode!(source).units, result.next_unit_index).scalar == ?x

    nested = String.duplicate("/*", 2_000) <> String.duplicate("*/", 2_000)
    assert {:ok, %{next_unit_index: 8_000}} = Catena.scan_comment(nested)

    assert {:error,
            %{
              id: "CMT002",
              span: %SourceSpan{byte_start: 0, byte_end: 2},
              details: %{reason: "unterminated_block_comment", remaining_depth: 2}
            }} = Catena.scan_comment("/* outer /* inner")

    assert {:error, %{id: "CMT001", details: %{reason: "not_a_comment"}}} =
             Catena.scan_comment("value")

    assert {:error, %{id: "CMT001", path: "$.unit_index"}} =
             Catena.scan_comment("// ok", unit_index: 99)
  end

  @tag obligations: ~w(CM-OBL-002 CM-OBL-004 CM-OBL-009)
  test "comment text preserves Unicode scalars and original CRLF byte spans" do
    source = "// e\u0301\r\nnext"
    assert {:ok, result} = Catena.scan_comment(source)
    assert result.comment.body == " e\u0301"
    refute result.comment.body == " é"
    assert result.comment.span.byte_end == byte_size("// e\u0301")
    assert result.next_unit_index == 5

    newline = Enum.at(decode!(source).units, result.next_unit_index)
    assert newline.scalar == ?\n

    assert newline.span ==
             span(byte_size("// e\u0301"), byte_size("// e\u0301\r\n"), 1, 6, 2, 1)
  end

  @tag obligations: ~w(CM-OBL-005 CM-OBL-006)
  test "documentation normalization removes only defined edges and common indentation" do
    source = "/** \n\t  alpha\n\t  * beta\n */"
    comment = scan!(source)

    assert comment.kind == :documentation
    assert comment.body == "alpha\n* beta"
    assert Enum.map(comment.body_units, & &1.scalar) |> List.to_string() == comment.body
    assert Enum.count(comment.units, &(&1.scalar == ?\n)) == 3
    assert length(comment.line_breaks) == 3

    assert scan!("/// one leading space").body == "one leading space"
    assert scan!("///\tkept tab").body == "\tkept tab"
    assert scan!("/** * decorative */").body == "* decorative"
  end

  @tag obligations: ~w(CM-OBL-004 CM-OBL-007 CM-OBL-010)
  test "every comment-internal LF participates in the C015 layout classifier" do
    hard = [token(:left, 0), scan!("/* a\nb\n\nc */"), token(:right, 20)]
    soft = [token(:left, 0, join_after: true), scan!("/* a\nb */"), token(:right, 20)]

    continued = [
      token(:open, 0, delimiter: {:open, :paren, :continued}),
      scan!("/* a\nb */"),
      token(:close, 20, delimiter: {:close, :paren})
    ]

    assert {:ok, hard_result} = Catena.resolve_comments(hard)
    assert [_, hard_comment, _] = hard_result.events

    assert Enum.map(hard_comment.line_breaks, & &1.classification) ==
             [:separator, :blank, :blank]

    assert {:ok, soft_result} = Catena.resolve_comments(soft)
    assert [_, soft_comment, _] = soft_result.events
    assert Enum.map(soft_comment.line_breaks, & &1.classification) == [:soft]
    assert soft_comment.units == Enum.at(soft, 1).units
    assert soft_comment.body_units == Enum.at(soft, 1).body_units

    assert {:ok, continued_result} = Catena.resolve_comments(continued)
    assert [_, continued_comment, _] = continued_result.events
    assert Enum.map(continued_comment.line_breaks, & &1.classification) == [:soft]
  end

  @tag obligations: ~w(CM-OBL-005 CM-OBL-006 CM-OBL-008)
  test "adjacent outer documentation attaches to the next supplied declaration" do
    source = "/// First\n/// Second\nname"
    decoded = decode!(source)
    assert {:ok, first} = Catena.scan_comment(source)
    first_break = Enum.at(decoded.units, first.next_unit_index)
    second_index = first.next_unit_index + 1
    assert {:ok, second} = Catena.scan_comment(source, unit_index: second_index)
    second_break = Enum.at(decoded.units, second.next_unit_index)
    target = %Target{id: {:function, :name}, span: Enum.at(decoded.units, -1).span}

    events = [
      first.comment,
      %LineBreak{unit: first_break},
      second.comment,
      %LineBreak{unit: second_break},
      target,
      token(:name, second.next_unit_index + 1)
    ]

    assert {:ok, result} = Catena.resolve_comments(events)

    assert [
             %Attachment{
               target_id: {:function, :name},
               comments: [_, _],
               body: "First\nSecond",
               markdown_profile: "commonmark-0.31.2",
               raw_html_policy: :preserve_source_never_execute_unsanitized,
               doctest_policy: :explicit_only,
               doctest_info_string: "catena doctest"
             }
           ] = result.attachments

    assert length(result.events) == length(events)
  end

  @tag obligations: ~w(CM-OBL-006 CM-OBL-008)
  test "CommonMark source and exact doctest opt-in remain inert metadata" do
    body = "<script>alert(1)</script>\n\n```catena doctest\nvalue\n```"
    comment = scan!("/** " <> body <> " */")
    line_break = hd(decode!("\n").units)
    target = %Target{id: :documented, span: span(100, 101, 2, 1, 2, 2)}

    assert {:ok, %{attachments: [attachment]}} =
             Catena.resolve_comments([comment, %LineBreak{unit: line_break}, target])

    assert attachment.body == body
    assert attachment.markdown_profile == "commonmark-0.31.2"
    assert attachment.raw_html_policy == :preserve_source_never_execute_unsanitized
    assert attachment.doctest_info_string == "catena doctest"
    refute Map.has_key?(Map.from_struct(attachment), :rendered_html)
    refute Map.has_key?(Map.from_struct(attachment), :execution_result)
  end

  @tag obligations: ~w(CM-OBL-008 CM-OBL-009)
  test "misplaced and unattached documentation fails with DOC001" do
    doc = scan!("/// docs")
    ordinary = scan!("// ordinary")
    line = %LineBreak{unit: hd(decode!("\n").units)}
    target = %Target{id: :declaration, span: span(20, 21, 2, 1, 2, 2)}

    assert_doc_error([doc, target], "missing_line_break_before_target")
    assert_doc_error([doc, line, line, target], "blank_line_before_target")
    assert_doc_error([doc, ordinary], "ordinary_comment_before_target")

    assert_doc_error(
      [doc, line, %Semicolon{unit: hd(decode!(";").units)}],
      "intervening_event"
    )

    assert_doc_error([doc, line, token(:value, 20)], "intervening_event")
    assert_doc_error([doc, line], "end_of_input")
  end

  defp assert_doc_error(events, reason) do
    assert {:error, %{id: "DOC001", details: %{reason: ^reason}}} =
             Catena.resolve_comments(events)
  end

  defp scan!(source) do
    assert {:ok, result} = Catena.scan_comment(source)
    result.comment
  end

  defp decode!(source) do
    assert {:ok, decoded} = Catena.decode_source_text(source)
    decoded
  end

  defp token(value, offset, options \\ []) do
    %Token{
      value: value,
      span: span(offset, offset + 1, 1, offset + 1, 1, offset + 2),
      join_before: Keyword.get(options, :join_before, false),
      join_after: Keyword.get(options, :join_after, false),
      delimiter: Keyword.get(options, :delimiter, :none)
    }
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp forged_interface do
    %{
      "format" => "catena-interface",
      "version" => "0.1.12",
      "edition" => "0.1",
      "language_revision" => "0.1.12",
      "previews" => [],
      "origin" => "test://c016",
      "exports" => [],
      "types" => [],
      "traits" => [],
      "effects" => [],
      "specifications" => [],
      "digest" => String.duplicate("0", 64)
    }
    |> JSON.encode!()
  end

  defp span(byte_start, byte_end, line_start, column_start, line_end, column_end) do
    %SourceSpan{
      byte_start: byte_start,
      byte_end: byte_end,
      line_start: line_start,
      column_start: column_start,
      line_end: line_end,
      column_end: column_end
    }
  end
end
