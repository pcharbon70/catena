defmodule Catena.C013SourceTextTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Catena.{
    CanonicalJCS,
    Interface,
    LanguageInfo,
    LanguageLifecycle,
    LanguageVersion,
    SourceSpan
  }

  alias Catena.Package.Manifest

  @kernel_fixture Path.expand("../fixtures/c010-kernel.catena", __DIR__)

  @tag obligations: ~w(ST-OBL-002 ST-OBL-003 ST-OBL-006)
  test "well-formed Unicode scalars are preserved without normalization" do
    source = "Aé" <> <<0x1D11E::utf8>> <> "e\u0301\uFDD0\u0378\uFFFD\uFEFF"
    assert {:ok, decoded} = Catena.decode_source_text(source)

    assert decoded.source == source
    assert decoded.text == source
    assert Enum.map(decoded.units, & &1.scalar) == String.to_charlist(source)
    assert decoded.selection.language_revision == "0.1.36"
    assert Enum.at(decoded.units, 4).span == span(8, 10, 1, 5, 1, 6)

    composed = "é"
    decomposed = "e\u0301"
    assert {:ok, composed_text} = Catena.decode_source_text(composed)
    assert {:ok, decomposed_text} = Catena.decode_source_text(decomposed)
    assert composed_text.text == composed
    assert decomposed_text.text == decomposed
    refute composed_text.text == decomposed_text.text
  end

  @tag obligations: ~w(ST-OBL-005 ST-OBL-007 ST-OBL-008)
  test "LF and CRLF become logical LF units with original-byte scalar spans" do
    supplementary_scalar = <<0x1D11E::utf8>>
    source = "a\r\né\n\t" <> supplementary_scalar
    assert {:ok, decoded} = Catena.decode_source_text(source)
    assert decoded.text == "a\né\n\t" <> supplementary_scalar

    [a, first_newline, e_acute, second_newline, tab, supplementary] = decoded.units
    assert a.span == span(0, 1, 1, 1, 1, 2)
    assert first_newline.scalar == ?\n
    assert first_newline.span == span(1, 3, 1, 2, 2, 1)
    assert e_acute.span == span(3, 5, 2, 1, 2, 2)
    assert second_newline.span == span(5, 6, 2, 2, 3, 1)
    assert tab.span == span(6, 7, 3, 1, 3, 2)
    assert supplementary.span == span(7, 11, 3, 2, 3, 3)
    assert decoded.eof_span == span(11, 11, 3, 3, 3, 3)
  end

  @tag obligations: ~w(ST-OBL-005 ST-OBL-006)
  test "mixed endings and optional final newlines are accepted but only LF and CRLF are newlines" do
    assert {:ok, decoded} = Catena.decode_source_text("one\ntwo\r\nthree")
    assert decoded.text == "one\ntwo\nthree"
    assert Enum.count(decoded.units, &(&1.scalar == ?\n)) == 2

    separators = "\u0085\u2028\u2029"
    assert {:ok, separator_text} = Catena.decode_source_text(separators)
    assert separator_text.text == separators
    assert Enum.count(separator_text.units, &(&1.scalar == ?\n)) == 0

    for source <- ["\r", "a\rb", "a\r"] do
      assert {:error, %{id: "SRC003", span: %SourceSpan{}}} =
               Catena.decode_source_text(source)
    end
  end

  @tag obligations: ~w(ST-OBL-002 ST-OBL-003 ST-OBL-009)
  test "malformed UTF-8 is rejected without replacement or encoding fallback" do
    malformed = [
      {<<0x80>>, "unexpected_continuation_byte", 1},
      {<<0xC0, 0x80>>, "overlong_leading_byte", 1},
      {<<0xC1, 0xBF>>, "overlong_leading_byte", 1},
      {<<0xF5, 0x80, 0x80, 0x80>>, "invalid_leading_byte", 1},
      {<<0xFF>>, "invalid_leading_byte", 1},
      {<<0xC2>>, "truncated_sequence", 1},
      {<<0xE1, 0x80>>, "truncated_sequence", 2},
      {<<0xF1, 0x80, 0x80>>, "truncated_sequence", 3},
      {<<0xC2, 0x20>>, "invalid_continuation_or_scalar_range", 1},
      {<<0xE1, 0x80, 0x20>>, "invalid_continuation_or_scalar_range", 1},
      {<<0xF1, 0x80, 0x80, 0x20>>, "invalid_continuation_or_scalar_range", 1},
      {<<0xE0, 0x9F, 0x80>>, "invalid_continuation_or_scalar_range", 1},
      {<<0xF0, 0x8F, 0x80, 0x80>>, "invalid_continuation_or_scalar_range", 1},
      {<<0xED, 0xA0, 0x80>>, "invalid_continuation_or_scalar_range", 1},
      {<<0xF4, 0x90, 0x80, 0x80>>, "invalid_continuation_or_scalar_range", 1}
    ]

    for {source, expected_reason, byte_end} <- malformed do
      assert {:error,
              %{
                id: "SRC001",
                details: %{reason: ^expected_reason},
                span: %SourceSpan{byte_start: 0, byte_end: ^byte_end}
              }} = Catena.decode_source_text(source)
    end

    assert {:error,
            %{
              id: "SRC001",
              span: %SourceSpan{byte_start: 2, byte_end: 3, column_start: 2, column_end: 3}
            }} = Catena.decode_source_text("é" <> <<0xFF>>)
  end

  @tag obligations: ~w(ST-OBL-002 ST-OBL-004 ST-OBL-009)
  test "leading BOMs and alternate Unicode encoding signatures are rejected distinctly" do
    assert {:error,
            %{
              id: "SRC002",
              details: %{reason: "leading_bom"},
              span: %SourceSpan{byte_start: 0, byte_end: 3}
            }} = Catena.decode_source_text(<<0xEF, 0xBB, 0xBF, ?x>>)

    for {source, detected} <- [
          {<<0xFE, 0xFF, 0x00, 0x61>>, "UTF-16BE"},
          {<<0xFF, 0xFE, 0x61, 0x00>>, "UTF-16LE"},
          {<<0x00, 0x00, 0xFE, 0xFF, 0x00, 0x00, 0x00, 0x61>>, "UTF-32BE"},
          {<<0xFF, 0xFE, 0x00, 0x00, 0x61, 0x00, 0x00, 0x00>>, "UTF-32LE"}
        ] do
      assert {:error,
              %{id: "SRC001", details: %{reason: "unsupported_encoding", detected: ^detected}}} =
               Catena.decode_source_text(source)
    end
  end

  @tag obligations: ~w(ST-OBL-001 ST-OBL-010)
  test "0.1.9 remains source-text-only after cumulative lexical revisions" do
    assert LanguageVersion.latest() == "0.1.36"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36)

    refute "0.1.9" in LanguageVersion.compilable_revisions()
    refute "0.1.9" in LanguageVersion.interface_versions()
    refute "0.1.9" in LanguageVersion.artifact_versions()
    refute "0.1.9" in LanguageVersion.signed_format_versions()
    assert {:ok, :stable} == LanguageLifecycle.state("source-text", "0.1.9")

    info = LanguageInfo.document()
    assert info["current"]["language_revision"] == "0.1.36"
    assert Enum.any?(info["features"], &(&1["id"] == "source-text"))

    assert {:error, %{id: "EDN001", details: %{frontend: "source-text"}}} =
             Catena.decode_source_text("x",
               language_selection: selection("0.1.8")
             )

    assert {:error, %{id: "EDN001", details: %{frontend: "json-ast"}}} =
             Catena.check_json(minimal_json(), language_selection: selection("0.1.9"))

    assert {:error, %{id: "EDN001"}} =
             Catena.check_kernel(File.read!(@kernel_fixture),
               language_selection: selection("0.1.9")
             )

    assert {:error, %{id: "EDN001", details: %{frontend: "package-ir"}}} =
             %{
               "format" => "catena-package-manifest",
               "version" => "0.1.7",
               "edition" => "0.1",
               "language_revision" => "0.1.9",
               "previews" => []
             }
             |> JSON.encode!()
             |> Manifest.decode()

    assert {:error, _diagnostic} = Interface.decode(forged_0_1_9_interface())

    assert_raise FunctionClauseError, fn ->
      LanguageVersion.default_artifact_version("0.1.9", "0.1.9")
    end

    assert_raise FunctionClauseError, fn -> CanonicalJCS.payload("manifest", "0.1.9", %{}) end
  end

  @tag obligations: ~w(ST-OBL-007 ST-OBL-008 ST-OBL-010)
  test "empty input and the check-source-text command expose deterministic envelope facts" do
    assert {:ok, empty} = Catena.decode_source_text("")
    assert empty.text == ""
    assert empty.units == []
    assert empty.eof_span == span(0, 0, 1, 1, 1, 1)

    path =
      Path.join(System.tmp_dir!(), "catena-c013-#{System.unique_integer([:positive])}.catena")

    File.write!(path, "é\r\n")
    on_exit(fn -> File.rm(path) end)

    first = capture_io(fn -> Catena.CLI.main(["check-source-text", path]) end)
    second = capture_io(fn -> Catena.CLI.main(["check-source-text", path]) end)
    assert first == second
    assert {:ok, output} = JSON.decode(String.trim(first))

    assert output == %{
             "status" => "ok",
             "edition" => "0.1",
             "language_revision" => "0.1.36",
             "byte_count" => 4,
             "scalar_count" => 2,
             "newline_count" => 1
           }
  end

  defp selection(revision) do
    %{edition: "0.1", language_revision: revision, previews: []}
  end

  defp minimal_json do
    JSON.encode!(%{
      "version" => "0.1.1",
      "module" => "C013NoArtifact",
      "exports" => [],
      "definitions" => []
    })
  end

  defp forged_0_1_9_interface do
    payload = %{
      "format" => "catena-interface",
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.9",
      "previews" => [],
      "required_previews" => [],
      "origin" => "test://c013-forged",
      "module" => "C013Forged",
      "types" => [],
      "values" => []
    }

    digest =
      payload
      |> Catena.CanonicalJSON.encode()
      |> then(&:crypto.hash(:sha256, &1))
      |> Base.encode16(case: :lower)

    payload
    |> Map.put("digest", digest)
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
