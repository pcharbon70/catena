defmodule Catena.Numeric do
  @moduledoc """
  The source-only Catena 0.1.14 numeric literal elaborator.

  One scanned numeric token (exact C017 components) is elaborated into its
  typed meaning: an unbounded mathematical `Int` value or a finite binary64
  `Float` value produced by one correctly rounded exact conversion. It is
  not a lexer, parser, program type checker, arithmetic evaluator, or
  numeric library.
  """

  alias Catena.{Diagnostic, ImplementationLimits, LanguageSelection, LanguageVersion}
  alias Catena.Literal.Numeric

  import Bitwise

  @numeric_revision "0.1.14"

  # A decimal magnitude at or above 10^310 is certainly beyond the finite
  # binary64 range, and one at or below 10^-325 is certainly below half the
  # smallest subnormal. Both shortcuts avoid constructing 10^huge powers.
  @certain_overflow_log10 310
  @certain_underflow_log10 -325

  defmodule Meaning do
    @moduledoc "The typed meaning of one elaborated numeric literal."

    @enforce_keys [:kind, :type, :value]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            kind: :integer | :decimal,
            type: :Int | :Float,
            value: integer() | float()
          }
  end

  @spec elaborate(Numeric.t(), keyword()) ::
          {:ok, Meaning.t()} | {:error, Diagnostic.t()}
  def elaborate(%Numeric{} = numeric, options \\ []) when is_list(options) do
    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)) do
      elaborate_numeric(numeric, selection)
    end
  end

  @spec negate(Meaning.t()) :: Meaning.t()
  def negate(%Meaning{kind: :integer, value: value}),
    do: %Meaning{kind: :integer, type: :Int, value: -value}

  def negate(%Meaning{kind: :decimal, value: value}),
    do: %Meaning{kind: :decimal, type: :Float, value: -value}

  defp resolve_selection(nil),
    do: require_numeric_revision(LanguageVersion.legacy_selection(@numeric_revision))

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_numeric_revision(resolved)
    end
  end

  defp require_numeric_revision(
         %LanguageSelection{language_revision: @numeric_revision} = selection
       ),
       do: {:ok, selection}

  defp require_numeric_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "numeric elaboration requires language revision #{@numeric_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @numeric_revision,
         frontend: "numeric-literal-semantics"
       }
     )}
  end

  defp elaborate_numeric(%Numeric{integer_value: value}, _selection) when is_integer(value),
    do: {:ok, %Meaning{kind: :integer, type: :Int, value: value}}

  defp elaborate_numeric(%Numeric{integer_value: nil} = numeric, _selection) do
    integral = numeric.integral_digits
    fractional = numeric.fractional_digits || ""
    exponent = numeric.exponent_digits || ""

    with :ok <-
           ImplementationLimits.validate_decimal_component_digits(
             byte_size(integral) + byte_size(fractional) + byte_size(exponent)
           ) do
      mantissa = String.to_integer(integral <> fractional)
      scaled_exponent = exponent_value(numeric) - byte_size(fractional)

      case decimal_to_float(mantissa, scaled_exponent) do
        {:ok, value} -> {:ok, %Meaning{kind: :decimal, type: :Float, value: value}}
        {:error, _} = error -> error
      end
    end
  end

  defp exponent_value(%Numeric{exponent_digits: nil}), do: 0

  defp exponent_value(%Numeric{exponent_sign: :minus, exponent_digits: digits}),
    do: -String.to_integer(digits)

  defp exponent_value(%Numeric{exponent_sign: _sign, exponent_digits: digits}),
    do: String.to_integer(digits)

  defp decimal_to_float(0, _e10), do: {:ok, 0.0}

  defp decimal_to_float(mantissa, e10) do
    log10_magnitude = e10 + decimal_digit_count(mantissa)

    cond do
      log10_magnitude >= @certain_overflow_log10 -> overflow()
      log10_magnitude <= @certain_underflow_log10 -> {:ok, 0.0}
      true -> convert_exact(mantissa, e10)
    end
  end

  defp convert_exact(mantissa, e10) do
    {n, d} = exact_ratio(mantissa, e10)

    e2 = ilog2_ratio(n, d)

    if e2 >= -1022 do
      q = scaled_round(n, d, 52 - e2)
      {significand, exponent} = normalize_significand(q, e2)

      if exponent > 1023 do
        overflow()
      else
        {:ok, from_parts(exponent + 1023, significand - (1 <<< 52))}
      end
    else
      q = scaled_round(n, d, 1074)

      cond do
        q == 0 -> {:ok, 0.0}
        q >= 1 <<< 52 -> {:ok, from_parts(1, 0)}
        true -> {:ok, from_parts(0, q)}
      end
    end
  end

  defp exact_ratio(mantissa, e10) when e10 >= 0, do: {mantissa * Integer.pow(10, e10), 1}
  defp exact_ratio(mantissa, e10), do: {mantissa, Integer.pow(10, -e10)}

  defp scaled_round(n, d, shift) when shift >= 0,
    do: round_half_even(n <<< shift, d)

  defp scaled_round(n, d, shift),
    do: round_half_even(n, d <<< -shift)

  defp round_half_even(a, b) do
    quotient = div(a, b)
    remainder = rem(a, b)
    twice = 2 * remainder

    cond do
      twice < b -> quotient
      twice > b -> quotient + 1
      band(quotient, 1) == 0 -> quotient
      true -> quotient + 1
    end
  end

  defp ilog2_ratio(n, d) do
    estimate = bit_length(n) - bit_length(d) - 1

    if ratio_at_least_power_of_two?(n, d, estimate + 1) do
      estimate + 1
    else
      estimate
    end
  end

  defp bit_length(n) when is_integer(n) and n > 0, do: bit_length(n, 0)

  defp bit_length(shifted, acc) when shifted > 0, do: bit_length(shifted >>> 1, acc + 1)
  defp bit_length(_shifted, acc), do: acc

  defp ratio_at_least_power_of_two?(n, d, p) when p >= 0, do: n >= d <<< p
  defp ratio_at_least_power_of_two?(n, d, p) when p < 0, do: n <<< -p >= d

  defp normalize_significand(q, e2) when q == 1 <<< 53, do: {1 <<< 52, e2 + 1}
  defp normalize_significand(q, e2), do: {q, e2}

  defp from_parts(exponent_field, fraction) do
    bits = exponent_field <<< 52 ||| fraction

    <<value::float-big>> = <<bits::unsigned-big-integer-size(64)>>
    value
  end

  defp decimal_digit_count(integer), do: integer |> Integer.to_string() |> byte_size()

  defp overflow do
    {:error,
     Diagnostic.new("NUM001", "decimal literal rounds beyond the finite binary64 range",
       details: %{reason: "decimal_literal_overflow"}
     )}
  end
end
