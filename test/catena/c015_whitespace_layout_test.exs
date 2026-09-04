defmodule Catena.C015WhitespaceLayoutTest do
  use ExUnit.Case, async: true

  alias Catena.{Interface, LanguageLifecycle, LanguageVersion, SourceSpan, SourceText}
  alias Catena.Layout.{LineBreak, Semicolon, Token, Whitespace}

  @tag obligations: ~w(LY-OBL-001 LY-OBL-010)
  test "0.1.11 is a source-only whitespace-and-layout revision" do
    assert LanguageVersion.latest() == "0.1.47"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47)

    refute "0.1.11" in LanguageVersion.compilable_revisions()
    refute "0.1.11" in LanguageVersion.interface_versions()
    refute "0.1.11" in LanguageVersion.artifact_versions()
    refute "0.1.11" in LanguageVersion.signed_format_versions()
    assert {:ok, :stable} == LanguageLifecycle.state("whitespace-and-layout", "0.1.11")

    assert {:error,
            %{id: "EDN001", details: %{frontend: "whitespace-and-layout", required: "0.1.11"}}} =
             Catena.resolve_layout([], language_selection: selection("0.1.10"))

    assert {:error, _diagnostic} = Interface.decode(forged_interface())
  end

  @tag obligations: ~w(LY-OBL-002 LY-OBL-008 LY-OBL-009)
  test "only ASCII space, tab, and C013 logical LF are layout whitespace" do
    assert {:ok, decoded} = Catena.decode_source_text(" \t\r\n")
    [space, tab, newline] = decoded.units

    assert {:ok, result} =
             Catena.resolve_layout([
               %Whitespace{units: [space, tab]},
               %LineBreak{unit: newline}
             ])

    assert [%Whitespace{}, %LineBreak{classification: :blank}] = result.events
    assert newline.span == span(2, 4, 1, 3, 2, 1)

    for scalar <- [?\v, ?\f, 0x00A0, 0x0085, 0x200E, 0x200F, 0x2028, 0x2029, 0x3000] do
      unit = source_unit(scalar)

      assert {:error,
              %{
                id: "LAY001",
                span: %SourceSpan{byte_start: 0, byte_end: byte_end},
                details: %{reason: "prohibited_whitespace", scalar: scalar_label}
              }} = Catena.resolve_layout([%Whitespace{units: [unit]}])

      assert byte_end == byte_size(<<scalar::utf8>>)
      assert scalar_label == codepoint(scalar)
    end

    assert {:error, %{id: "LAY001", details: %{reason: "empty_whitespace_event"}}} =
             Catena.resolve_layout([%Whitespace{units: []}])

    assert {:error,
            %{
              id: "LAY001",
              span: %SourceSpan{byte_start: 0, byte_end: 1},
              details: %{reason: "invalid_line_break", scalar: "U+0020"}
            }} = Catena.resolve_layout([%LineBreak{unit: ascii_unit(?\s, 0)}])
  end

  @tag obligations: ~w(LY-OBL-003 LY-OBL-010)
  test "indentation and tab width never create layout structure" do
    compact = [token(:left, 0), line(1), token(:right, 2)]

    indented = [
      token(:left, 0),
      line(1),
      whitespace([?\s, ?\s, ?\t, ?\s], 2),
      token(:right, 6)
    ]

    assert classifications(compact) == [:separator]
    assert classifications(indented) == [:separator]

    assert {:ok, result} = Catena.resolve_layout(indented)
    assert [%Token{}, %LineBreak{}, %Whitespace{units: units}, %Token{}] = result.events
    assert Enum.map(units, & &1.scalar) == [?\s, ?\s, ?\t, ?\s]
  end

  @tag obligations: ~w(LY-OBL-004 LY-OBL-005 LY-OBL-010)
  test "logical newlines and semicolons are lossless hard separators" do
    events = [
      line(0),
      token(:first, 1),
      line(2),
      line(3),
      token(:second, 4),
      semicolon(5),
      token(:third, 6),
      line(7)
    ]

    assert {:ok, result} = Catena.resolve_layout(events)
    assert classifications(result.events) == [:blank, :separator, :blank, :separator]
    assert Enum.count(result.events, &match?(%Semicolon{}, &1)) == 1

    assert {:ok, no_final_newline} = Catena.resolve_layout([token(:complete, 0)])
    assert no_final_newline.events == [token(:complete, 0)]
  end

  @tag obligations: ~w(LY-OBL-006 LY-OBL-010)
  test "token capabilities continue incomplete forms across one or more lines" do
    trailing = [
      token(:left, 0),
      token(:equals, 1, join_after: true),
      line(2),
      line(3),
      token(:right, 4)
    ]

    leading = [
      token(:left, 0),
      line(1),
      token(:pipe, 2, join_before: true),
      token(:right, 3)
    ]

    assert classifications(trailing) == [:soft, :soft]
    assert classifications(leading) == [:soft]
  end

  @tag obligations: ~w(LY-OBL-007)
  test "continued and block delimiter frames have distinct newline behavior" do
    continued = [
      token(:open_paren, 0, delimiter: {:open, :paren, :continued}),
      line(1),
      token(:item, 2),
      line(3),
      token(:close_paren, 4, delimiter: {:close, :paren})
    ]

    block = [
      token(:open_block, 0, delimiter: {:open, :block, :block}),
      line(1),
      token(:first, 2),
      line(3),
      token(:second, 4),
      line(5),
      token(:close_block, 6, delimiter: {:close, :block})
    ]

    mixed = [
      token(:open_paren, 0, delimiter: {:open, :paren, :continued}),
      line(1),
      token(:open_block, 2, delimiter: {:open, :block, :block}),
      line(3),
      token(:body, 4),
      token(:close_block, 5, delimiter: {:close, :block}),
      line(6),
      token(:close_paren, 7, delimiter: {:close, :paren})
    ]

    assert classifications(continued) == [:soft, :soft]
    assert classifications(block) == [:separator, :separator, :separator]
    assert classifications(mixed) == [:soft, :separator, :soft]
  end

  @tag obligations: ~w(LY-OBL-007 LY-OBL-008)
  test "delimiter failures use stable diagnostics and the offending spans" do
    assert {:error,
            %{
              id: "LAY002",
              span: %SourceSpan{byte_start: 0},
              details: %{reason: "unexpected_close"}
            }} =
             Catena.resolve_layout([token(:close, 0, delimiter: {:close, :paren})])

    assert {:error,
            %{
              id: "LAY002",
              span: %SourceSpan{byte_start: 1},
              details: %{reason: "mismatched_close"}
            }} =
             Catena.resolve_layout([
               token(:open, 0, delimiter: {:open, :paren, :continued}),
               token(:close, 1, delimiter: {:close, :bracket})
             ])

    assert {:error,
            %{
              id: "LAY002",
              span: %SourceSpan{byte_start: 0},
              details: %{reason: "unclosed_delimiter"}
            }} =
             Catena.resolve_layout([
               token(:open, 0, delimiter: {:open, :paren, :continued})
             ])
  end

  @tag obligations: ~w(LY-OBL-006 LY-OBL-008)
  test "semicolon, separators, and EOF cannot interrupt required continuation" do
    assert {:error, %{id: "LAY003", details: %{reason: "semicolon_interrupts_continuation"}}} =
             Catena.resolve_layout([
               token(:equals, 0, join_after: true),
               semicolon(1),
               token(:right, 2)
             ])

    assert {:error, %{id: "LAY003", details: %{reason: "missing_left_expression"}}} =
             Catena.resolve_layout([token(:pipe, 0, join_before: true)])

    assert {:error, %{id: "LAY003", details: %{reason: "eof_interrupts_continuation"}}} =
             Catena.resolve_layout([token(:equals, 0, join_after: true), line(1)])
  end

  @tag obligations: ~w(LY-OBL-009 LY-OBL-010 LY-OBL-011)
  test "opaque tokens shield token-owned whitespace and repeated resolution is deterministic" do
    opaque = token({:string, "first\nsecond\u00A0"}, 0)
    events = [opaque, line(1), token(:next, 2)]

    assert {:ok, first} = Catena.resolve_layout(events)
    assert {:ok, second} = Catena.resolve_layout(events)
    assert first == second
    assert first.events == [opaque, %{line(1) | classification: :separator}, token(:next, 2)]
  end

  defp classifications(events) do
    resolved =
      case Catena.resolve_layout(events) do
        {:ok, result} -> result.events
        %Catena.Layout.Result{} = result -> result.events
      end

    for %LineBreak{classification: classification} <- resolved, do: classification
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

  defp whitespace(scalars, offset) do
    units =
      scalars
      |> Enum.with_index(offset)
      |> Enum.map(fn {scalar, index} ->
        %SourceText.Unit{
          scalar: scalar,
          span: span(index, index + 1, 1, index + 1, 1, index + 2)
        }
      end)

    %Whitespace{units: units}
  end

  defp line(offset), do: %LineBreak{unit: ascii_unit(?\n, offset)}
  defp semicolon(offset), do: %Semicolon{unit: ascii_unit(?;, offset)}

  defp ascii_unit(scalar, offset) do
    %SourceText.Unit{
      scalar: scalar,
      span: span(offset, offset + 1, 1, offset + 1, 1, offset + 2)
    }
  end

  defp source_unit(scalar) do
    source = <<scalar::utf8>>
    assert {:ok, decoded} = Catena.decode_source_text(source)
    hd(decoded.units)
  end

  defp codepoint(scalar) do
    "U+" <> (scalar |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(4, "0"))
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp forged_interface do
    %{
      "format" => "catena-interface",
      "version" => "0.1.11",
      "edition" => "0.1",
      "language_revision" => "0.1.11",
      "previews" => [],
      "origin" => "test://c015",
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
