defmodule Catena.C017LiteralGrammarTest do
  use ExUnit.Case, async: false

  alias Catena.{Interface, LanguageLifecycle, LanguageVersion, SourceSpan}
  alias Catena.Literal.{Numeric, Piece}

  @tag obligations: ~w(LT-OBL-001 LT-OBL-010 LT-OBL-011 LT-OBL-012)
  test "0.1.13 is an exact deterministic source-only literal revision" do
    assert LanguageVersion.latest() == "0.1.29"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29)

    refute "0.1.13" in LanguageVersion.compilable_revisions()
    refute "0.1.13" in LanguageVersion.interface_versions()
    refute "0.1.13" in LanguageVersion.artifact_versions()
    refute "0.1.13" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("literal-grammar", "0.1.13")

    change =
      Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-13-literal-grammar"))

    assert change["affects"] == ~w(source-acceptance diagnostics)
    assert String.contains?(change["specification"], "literal-forms-and-boundaries.md#")

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "literal-grammar", required: "0.1.13"}
            }} = Catena.scan_literal("true", language_selection: selection("0.1.12"))

    assert {:ok, %{selection: %{language_revision: "0.1.12"}}} =
             Catena.scan_comment("// pinned")

    assert Catena.scan_literal("r#\"stable\"#") == Catena.scan_literal("r#\"stable\"#")
    assert {:error, _diagnostic} = Interface.decode(forged_interface())
  end

  @tag obligations: ~w(LT-OBL-002 LT-OBL-009)
  test "boolean keywords and unit-index boundaries are exact" do
    source = "name true false tail"
    assert {:ok, first} = Catena.scan_literal(source, unit_index: 5)
    assert first.literal.kind == :boolean
    assert first.literal.form == :keyword
    assert first.literal.payload == true
    assert first.literal.lexeme == "true"
    assert first.next_unit_index == 9

    assert {:ok, second} = Catena.scan_literal(source, unit_index: 10)
    assert second.literal.payload == false
    assert second.next_unit_index == 15

    assert {:error, %{id: "LIT001", details: %{reason: "not_a_literal"}}} =
             Catena.scan_literal("true_value")

    assert {:error, %{id: "LIT001", path: "$.unit_index"}} =
             Catena.scan_literal("true", unit_index: 99)
  end

  @tag obligations: ~w(LT-OBL-002 LT-OBL-003 LT-OBL-010)
  test "integer bases, separators, and decimal floats expose normalized exact metadata" do
    assert %{kind: :integer, form: {:integer, 2}, payload: %Numeric{} = binary} =
             literal!("0b1010_0110")

    assert binary.base == 2
    assert binary.integral_digits == "10100110"
    assert binary.integer_value == 166

    assert literal!("0o7_5").payload.integer_value == 61
    assert literal!("1_000").payload.integer_value == 1_000
    assert literal!("0xDeAd_BeEf").payload.integer_value == 0xDEADBEEF

    assert %{kind: :float, form: :decimal_float, payload: %Numeric{} = dotted} =
             literal!("12_3.4_5e-6_7")

    assert dotted == %Numeric{
             base: 10,
             integral_digits: "123",
             fractional_digits: "45",
             exponent_sign: :minus,
             exponent_digits: "67",
             integer_value: nil
           }

    assert literal!("9E+2").payload.exponent_sign == :plus
    assert literal!("9e2").payload.fractional_digits == nil

    assert {:ok, %{literal: %{kind: :integer}, next_unit_index: 1}} =
             Catena.scan_literal("1.")
  end

  @tag obligations: ~w(LT-OBL-003 LT-OBL-009)
  test "malformed numeric spellings fail instead of silently changing value" do
    cases = %{
      "0b" => "missing_numeric_digits",
      "0b2" => "invalid_digit_for_base",
      "0x1.0" => "based_float_not_supported",
      "00" => "redundant_leading_zero",
      "0_1" => "redundant_leading_zero",
      "1__0" => "repeated_numeric_separator",
      "1_" => "misplaced_numeric_separator",
      "1._0" => "invalid_fraction_digits",
      "1.0_" => "misplaced_numeric_separator",
      "1e" => "missing_numeric_digits",
      "1e+" => "missing_numeric_digits",
      "1e2name" => "invalid_numeric_suffix",
      "0Xff" => "invalid_numeric_suffix"
    }

    Enum.each(cases, fn {source, reason} ->
      assert {:error, %{id: "LIT003", details: %{reason: ^reason}}} =
               Catena.scan_literal(source)
    end)

    for source <- ["-1", "+1", ".5", "NaN", "Infinity"] do
      assert {:error, %{id: "LIT001"}} = Catena.scan_literal(source)
    end
  end

  @tag obligations: ~w(LT-OBL-004 LT-OBL-005 LT-OBL-006 LT-OBL-010)
  test "cooked text decodes the closed escape set without normalization" do
    literal = literal!("\"é\\0\\t\\n\\r\\\\\\\"\\'\\x41\\u{1F600}\"")

    assert literal.kind == :text
    assert literal.form == :cooked
    assert literal.payload == "é\0\t\n\r\\\"'A😀"
    refute String.starts_with?(literal.payload, "é")

    assert Enum.map(literal.pieces, & &1.kind) ==
             [:verbatim, :verbatim | List.duplicate(:escape, 9)]

    assert Enum.all?(literal.pieces, &match?(%Piece{span: %SourceSpan{}}, &1))

    for {source, reason} <- [
          {"\"\\q\"", "unknown_escape"},
          {"\"\\x8F\"", "non_ascii_text_hex_escape"},
          {"\"\\u{}\"", "invalid_unicode_escape_length"},
          {"\"\\u{D800}\"", "invalid_unicode_scalar"},
          {"\"a\\\nb\"", "backslash_line_continuation"},
          {"\"a\nb\"", "cooked_line_break"}
        ] do
      assert {:error, %{id: "LIT003", details: %{reason: ^reason}}} =
               Catena.scan_literal(source)
    end

    assert {:error, %{id: "LIT002", details: %{reason: "unterminated_cooked_literal"}}} =
             Catena.scan_literal("\"open")
  end

  @tag obligations: ~w(LT-OBL-004 LT-OBL-005 LT-OBL-007 LT-OBL-010)
  test "raw text uses exact arbitrary hash delimiters and owns internal line breaks" do
    literal = literal!("r##\"one\n\"#two\"##tail")
    assert literal.form == {:raw, 2}
    assert literal.payload == "one\n\"#two"
    assert length(literal.owned_line_breaks) == 1
    assert literal.owned_line_breaks |> hd() |> Map.fetch!(:scalar) == ?\n
    assert Enum.all?(literal.pieces, &(&1.kind == :verbatim))
    assert literal!("r\"\\n\"").payload == "\\n"

    hashes = String.duplicate("#", 2_000)
    source = "r" <> hashes <> "\"large\"" <> hashes
    assert literal!(source).form == {:raw, 2_000}

    assert {:error,
            %{id: "LIT002", details: %{reason: "unterminated_raw_literal", hash_count: 2}}} =
             Catena.scan_literal("r##\"value\"#")
  end

  @tag obligations: ~w(LT-OBL-004 LT-OBL-005 LT-OBL-006 LT-OBL-009)
  test "characters are one scalar while byte literals decode exact octets" do
    assert literal!("'😀'").payload == 0x1F600
    assert literal!("'\\u{301}'").payload == 0x301
    assert literal!("'\\''").payload == ?'

    for source <- ["''", "'ab'", "'é'"] do
      assert {:error, %{id: "LIT003", details: %{reason: "invalid_character_arity"}}} =
               Catena.scan_literal(source)
    end

    assert literal!("b\"A\\xFF\"").payload == <<?A, 0xFF>>
    assert literal!("br\"A\nB\"").payload == <<?A, ?\n, ?B>>

    for {source, reason} <- [
          {"b\"é\"", "non_ascii_cooked_byte"},
          {"br\"é\"", "non_ascii_raw_byte"},
          {"b\"\\u{41}\"", "unicode_escape_in_byte_literal"}
        ] do
      assert {:error, %{id: "LIT003", details: %{reason: ^reason}}} =
               Catena.scan_literal(source)
    end
  end

  @tag obligations: ~w(LT-OBL-005 LT-OBL-007 LT-OBL-009 LT-OBL-010)
  test "logical LF ownership and original CRLF and multibyte spans remain lossless" do
    source = "r\"a\r\né\" next"
    assert {:ok, result} = Catena.scan_literal(source)
    literal = result.literal

    assert literal.payload == "a\né"
    assert literal.lexeme == "r\"a\né\""
    assert literal.span.byte_end == byte_size("r\"a\r\né\"")
    assert result.next_unit_index == 6

    [line_break] = literal.owned_line_breaks

    assert line_break.span == %SourceSpan{
             byte_start: 3,
             byte_end: 5,
             line_start: 1,
             column_start: 4,
             line_end: 2,
             column_end: 1
           }

    assert %Catena.SourceText.Unit{} = line_break
    assert List.last(literal.pieces).span.byte_end == byte_size("r\"a\r\né")
  end

  @tag obligations: ~w(LT-OBL-002 LT-OBL-006 LT-OBL-008 LT-OBL-009)
  test "published literal limits have accepted boundaries and stable refusal diagnostics" do
    accepted_text = "\"" <> String.duplicate("a", 65_536) <> "\""
    refused_bytes = "b\"" <> String.duplicate("a", 65_537) <> "\""
    assert byte_size(literal!(accepted_text).payload) == 65_536

    assert {:error,
            %{
              id: "LIM004",
              span: %SourceSpan{},
              details: %{
                limit_id: "decoded_literal_bytes",
                configured: 65_536,
                observed: 65_537,
                unit: "bytes"
              }
            }} = Catena.scan_literal(refused_bytes)

    accepted_decimal = String.duplicate("9", 4_096)
    refused_decimal = String.duplicate("9", 4_097)
    assert literal!(accepted_decimal).payload.integer_value > 0

    assert {:error, %{id: "LIM002", span: %SourceSpan{}, details: %{observed: 4_097}}} =
             Catena.scan_literal(refused_decimal)

    assert literal!("0x" <> String.duplicate("f", 3_401)).payload.integer_value > 0

    assert {:error, %{id: "LIM002", details: %{observed: 4_097}}} =
             Catena.scan_literal("0x" <> String.duplicate("f", 3_402))
  end

  @tag obligations: ~w(LT-OBL-002 LT-OBL-011 LT-OBL-012)
  test "non-atomic, symbolic, interpolated, and byte-character forms stay excluded" do
    for source <- [":atom", "[1]", "{1, 2}", "%{a: 1}", "b'a'", "f\"value\"", "rf\"value\""] do
      assert {:error, %{id: "LIT001", details: %{reason: "not_a_literal"}}} =
               Catena.scan_literal(source)
    end

    refute Map.has_key?(Map.from_struct(literal!("\"value\"")), :interpolation)
    refute function_exported?(Catena, :parse_source, 2)
    refute function_exported?(Catena, :compile_source, 2)
  end

  defp literal!(source) do
    assert {:ok, result} = Catena.scan_literal(source)
    result.literal
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp forged_interface do
    %{
      "format" => "catena-interface",
      "version" => "0.1.13",
      "edition" => "0.1",
      "language_revision" => "0.1.13",
      "previews" => [],
      "origin" => "test://c017",
      "exports" => [],
      "types" => [],
      "traits" => [],
      "effects" => [],
      "specifications" => [],
      "digest" => String.duplicate("0", 64)
    }
    |> JSON.encode!()
  end
end
