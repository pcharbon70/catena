defmodule Catena.C018NumericLiteralSemanticsTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Numeric}
  alias Catena.Literal.Numeric
  alias Catena.Numeric.Meaning

  @max_finite_source "179769313486231570814527423731704356798070567525844996598917476803157260780028538760589558632766878171540458953514382464234321326889464182768467546703537516986049910576551282076245490090389328944075868508455133942304583236903222948165808559332123348274797826204144723168738177180919299881250404026184124858368.0"
  @halfway_magnitude_source "179769313486231580793728971405303415079934132710037826936173778980444968292764750946649017977587207096330286416692887910946555547851940402630657488671505820681908902000708383676273854845817711531764475730270069855571366959622842914819860834936475292719074168444365510704342711559699508093042880177904174497792.0"
  @max_finite_bits 0x7FEFFFFFFFFFFFFF
  @min_normal_bits 0x0010000000000000
  @max_subnormal_bits 0x000FFFFFFFFFFFFF

  @tag obligations: ~w(NM-OBL-001 NM-OBL-014)
  test "0.1.14 is an exact deterministic numeric-meaning revision" do
    assert LanguageVersion.latest() == "0.1.18"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18)

    refute "0.1.14" in LanguageVersion.compilable_revisions()
    refute "0.1.14" in LanguageVersion.interface_versions()
    refute "0.1.14" in LanguageVersion.artifact_versions()
    refute "0.1.14" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("numeric-literal-semantics", "0.1.14")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-14-numeric-literal-semantics")
      )

    assert change["affects"] == ~w(static-meaning diagnostics)

    assert String.contains?(
             change["specification"],
             "numeric-literal-semantics/numeric-types-and-literal-typing.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "numeric-literal-semantics", required: "0.1.14"}
            }} =
             Catena.elaborate_numeric_literal(numeric!("1"),
               language_selection: selection("0.1.13")
             )

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} =
             Catena.scan_literal("1.0")

    assert Catena.elaborate_numeric_literal(numeric!("1.5")) ==
             Catena.elaborate_numeric_literal(numeric!("1.5"))
  end

  @tag obligations: ~w(NM-OBL-002 NM-OBL-009)
  test "integer meanings are exact mathematical Int values from every base" do
    for {source, value} <- [
          {"0b1010_0110", 166},
          {"0o7_5", 61},
          {"1_000", 1_000},
          {"0xDeAd_BeEf", 0xDEADBEEF}
        ] do
      assert {:ok, %Meaning{kind: :integer, type: :Int, value: ^value}} =
               Catena.elaborate_numeric_literal(numeric!(source))
    end

    large = Integer.pow(2, 700) + 1

    assert {:ok, %Meaning{kind: :integer, type: :Int, value: ^large}} =
             Catena.elaborate_numeric_literal(numeric!(Integer.to_string(large)))

    negated = Catena.Numeric.negate(meaning!(Integer.to_string(large)))
    assert negated.value == -large
    assert negated.type == :Int
  end

  @tag obligations: ~w(NM-OBL-003)
  test "float domain is finite binary64 with signed zero and no exceptional values" do
    assert {:ok, %Meaning{kind: :decimal, type: :Float, value: 1.0}} =
             Catena.elaborate_numeric_literal(numeric!("1.0"))

    assert float_bits(meaning!("0.0").value) == 0

    negative_zero = Catena.Numeric.negate(meaning!("0.0"))
    assert negative_zero.value == 0.0
    assert float_bits(negative_zero.value) == 0x8000000000000000

    for source <- ["NaN", "Infinity", "-Infinity", "inf"] do
      assert {:error, %{id: "LIT001"}} = Catena.scan_literal(source)
    end

    assert Catena.Numeric.__info__(:functions) |> Keyword.keys() |> Enum.uniq() |> Enum.sort() ==
             ~w(elaborate negate)a
  end

  @tag obligations: ~w(NM-OBL-004 NM-OBL-005 NM-OBL-006)
  test "literals are monomorphic without constraints, defaulting, or coercion" do
    assert {:ok, integer} = Catena.elaborate_numeric_literal(numeric!("42"))
    assert {:ok, decimal} = Catena.elaborate_numeric_literal(numeric!("42.0"))

    assert {integer.type, decimal.type} == {:Int, :Float}
    assert Map.keys(Map.from_struct(decimal)) |> Enum.sort() == [:kind, :type, :value]

    constrained =
      Catena.elaborate_numeric_literal(numeric!("42"),
        language_selection: selection("0.1.14"),
        expected_type: :Float
      )

    assert {:ok, %{type: :Int}} = constrained

    refute Catena.Numeric.__info__(:functions)
           |> Keyword.has_key?(:coerce)

    refute Catena.Numeric.__info__(:functions)
           |> Keyword.has_key?(:default)

    refute function_exported?(Catena, :coerce_numeric_literal, 2)
  end

  @tag obligations: ~w(NM-OBL-007)
  test "negation elaborates totally on Int and sign-flips Float" do
    assert Catena.Numeric.negate(meaning!("5")).value == -5

    negative_zero = Catena.Numeric.negate(meaning!("0.0"))
    assert negative_zero.value == 0.0
    assert float_bits(negative_zero.value) == 0x8000000000000000

    back_to_zero = Catena.Numeric.negate(negative_zero)
    assert back_to_zero.value == 0.0
    assert float_bits(back_to_zero.value) == 0

    two_pow_200 = Integer.pow(2, 200)
    assert Catena.Numeric.negate(meaning!(Integer.to_string(two_pow_200))).value == -two_pow_200

    assert Catena.Numeric.negate(meaning!("1.5")).value == -1.5
    assert float_bits(Catena.Numeric.negate(meaning!("1.5")).value) == 0xBFF8000000000000
  end

  @tag obligations: ~w(NM-OBL-008)
  test "pattern grammar stays unsigned and outside numeric elaboration" do
    for source <- ["-1", "+1", "-1.5", "-0.0"] do
      assert {:error, %{id: "LIT001", details: %{reason: "not_a_literal"}}} =
               Catena.scan_literal(source)
    end

    assert Map.keys(Map.from_struct(meaning!("1"))) |> Enum.sort() == [:kind, :type, :value]

    refute Catena.Numeric.__info__(:functions)
           |> Keyword.has_key?(:pattern)
  end

  @tag obligations: ~w(NM-OBL-010 NM-OBL-011)
  test "decimal conversion is exact and correctly rounded with ties to even" do
    assert {:ok, %Meaning{value: 0.1}} = Catena.elaborate_numeric_literal(numeric!("0.1"))
    assert float_bits(meaning!("0.1").value) == 0x3FB999999999999A

    assert float_bits(meaning!("1." <> fraction_53(1)).value) == float_bits(1.0)
    assert meaning!("1." <> fraction_53(3)).value == 1 + :math.pow(2, -51)

    assert {:ok, %Meaning{value: value}} =
             Catena.elaborate_numeric_literal(numeric!("1_2.3_4e-1_0"))

    assert value == 12.34e-10
  end

  @tag obligations: ~w(NM-OBL-011)
  test "subnormal results and underflow to zero are valid" do
    assert float_bits(meaning!("4.9406564584124654e-324").value) == 1
    assert float_bits(meaning!("2.4703282292062327e-324").value) == 0
    assert float_bits(meaning!("0." <> exact_half_subnormal()).value) == 0
    assert meaning!("1.0e-400").value == 0.0
    assert float_bits(meaning!(@max_finite_source).value) == @max_finite_bits

    halfway_digits = Integer.to_string((Integer.pow(2, 53) - 1) * Integer.pow(5, 1075))

    below_halfway_digits =
      ((Integer.pow(2, 53) - 1) * Integer.pow(5, 1075) - 1) |> Integer.to_string()

    assert float_bits(meaning!("0." <> pad(halfway_digits, 1075)).value) == @min_normal_bits

    assert float_bits(meaning!("0." <> pad(below_halfway_digits, 1075)).value) ==
             @max_subnormal_bits
  end

  @tag obligations: ~w(NM-OBL-012)
  test "overflow is static invalidity at the exact halfway boundary" do
    assert float_bits(meaning!("1.7976931348623157e308").value) == @max_finite_bits
    assert float_bits(meaning!(@max_finite_source).value) == @max_finite_bits

    for source <- ["1.0e400", "1.7976931348623159e308", @halfway_magnitude_source] do
      assert {:error, %{id: "NUM001", details: %{reason: "decimal_literal_overflow"}}} =
               Catena.elaborate_numeric_literal(numeric!(source))
    end
  end

  @tag obligations: ~w(NM-OBL-013 IL-OBL-013)
  test "LIM005 accepts 4096 component digits and refuses the next" do
    accepted = "1." <> String.duplicate("0", 4_094) <> "1"
    refused = "1." <> String.duplicate("0", 4_095) <> "1"

    assert {:ok, %Meaning{value: 1.0}} = Catena.elaborate_numeric_literal(numeric!(accepted))

    assert {:error,
            %{
              id: "LIM005",
              details: %{
                limit_id: "decimal_literal_component_digits",
                minimum_supported: 4_096,
                configured: 4_096,
                observed: 4_097,
                unit: "decimal_digits"
              }
            }} = Catena.elaborate_numeric_literal(numeric!(refused))

    assert {:error, %{id: "LIM005"}} =
             Catena.elaborate_numeric_literal(numeric!("1e" <> String.duplicate("9", 4_096)))
  end

  @tag obligations: ~w(NM-OBL-001 NM-OBL-014)
  test "elaboration stays deterministic and outside later phases" do
    numeric = numeric!("2.5")

    assert Catena.elaborate_numeric_literal(numeric) ==
             Catena.elaborate_numeric_literal(numeric, language_selection: selection("0.1.14"))

    overflow = numeric!("1.0e500")

    assert {:error, %{id: "NUM001"}} = Catena.elaborate_numeric_literal(overflow)

    assert Catena.elaborate_numeric_literal(overflow) ==
             Catena.elaborate_numeric_literal(overflow)

    assert is_integer(meaning!("7").value)
    assert is_float(meaning!("7.0").value)

    refute function_exported?(Catena, :parse_source, 2)
    refute function_exported?(Catena, :compile_source, 2)
    refute function_exported?(Catena, :evaluate_arithmetic, 1)
  end

  defp numeric!(source) do
    assert {:ok, %{literal: %{payload: %Numeric{} = numeric}}} = Catena.scan_literal(source)
    numeric
  end

  defp meaning!(source) do
    assert {:ok, %Meaning{} = meaning} =
             Catena.elaborate_numeric_literal(numeric!(source))

    meaning
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp float_bits(value) do
    <<bits::unsigned-big-integer-size(64)>> = <<value::float-big>>
    bits
  end

  defp fraction_53(scalar) do
    (scalar * Integer.pow(5, 53)) |> Integer.to_string() |> pad(53)
  end

  defp exact_half_subnormal, do: pad(Integer.to_string(Integer.pow(5, 1075)), 1075)

  defp pad(digits, width), do: String.pad_leading(digits, width, "0")
end
