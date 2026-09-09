defmodule Catena.Standard.Numeric do
  @moduledoc "Explicit checked numeric library; internal roles do not adopt public vocabulary."
  alias Catena.Standard.Numeric.{Binary64, Decimal}
  alias Catena.Standard.Outcomes, as: Outcome
  alias Catena.Foreign.Budget

  @failures [
    :zero_divisor,
    :overflow,
    :domain,
    :inexact,
    :decimal_precision,
    :malformed_number,
    :nonfinite
  ]
  def profile,
    do: %{
      version: "0.1.67",
      float: :finite_binary64,
      rounding: :nearest_even,
      printing: :exact_decimal,
      decimal_digits: 4096,
      decimal_scale: {-1024, 1024},
      parser_bytes: 4096,
      algebraic_functions: [:sqrt],
      transcendental_functions: []
    }

  def failure_schema, do: {:variant, Map.new(@failures, &{Atom.to_string(&1), :unit})}
  def failure(reason) when reason in @failures, do: {:catena_variant, reason, :unit}

  def run(operation, value, limits) do
    with :ok <- Budget.check({operation, value}, limits) do
      case execute(operation, value) do
        {:ok, value} ->
          finish(Outcome.success(value), limits)

        {:error, reason} when reason in @failures ->
          finish(Outcome.failure(failure(reason)), limits)

        error ->
          error
      end
    end
  end

  defp finish(value, limits), do: with(:ok <- Budget.check(value, limits), do: {:ok, value})

  defp execute({:primitive, op}, {a, b})
       when op in [:add, :subtract, :multiply] and
              ((is_float(a) and is_float(b)) or (is_integer(a) and is_integer(b))),
       do: {:ok, __MODULE__.Primitive.apply(op, a, b)}

  defp execute(:euclidean, {a, b}) when is_integer(a) and is_integer(b) do
    if b == 0 do
      {:error, :zero_divisor}
    else
      r = Integer.mod(a, abs(b))
      {:ok, {div(a - r, b), r}}
    end
  end

  defp execute({:integer, op}, {a, b}) when is_integer(a) and is_integer(b) do
    case op do
      :add -> {:ok, a + b}
      :subtract -> {:ok, a - b}
      :multiply -> {:ok, a * b}
      _ -> {:error, :unsupported_operation}
    end
  end

  defp execute({:float, op}, {a, b}) when is_float(a) and is_float(b),
    do: Binary64.operate(op, a, b)

  defp execute(:sqrt, value) when is_float(value), do: Binary64.sqrt(value)

  defp execute({:int_to_float, mode}, value)
       when is_integer(value) and mode in [:exact, :nearest_even] do
    with {:ok, result} <- Binary64.round_ratio(value, 1) do
      {n, d} = Binary64.ratio(result)
      if mode == :exact and n != value * d, do: {:error, :inexact}, else: {:ok, result}
    end
  end

  defp execute({:float_to_int, mode}, value) when is_float(value) do
    {n, d} = Binary64.ratio(value)
    Decimal.round_integer(n, d, mode)
  end

  defp execute(:format_float, value) when is_float(value), do: {:ok, Binary64.format(value)}
  defp execute(:parse_float, value) when is_binary(value), do: Binary64.parse(value)
  defp execute(:float_from_bits, value), do: Binary64.from_bits(value)
  defp execute(:float_bits, value) when is_float(value), do: {:ok, Binary64.bits(value)}
  defp execute(:decimal, {coefficient, scale}), do: Decimal.new(coefficient, scale)

  defp execute({:decimal_pair, op, context}, {ac, ascale, bc, bscale}) do
    with {:ok, a} <- Decimal.new(ac, ascale),
         {:ok, b} <- Decimal.new(bc, bscale),
         do: Decimal.operate(op, a, b, context)
  end

  defp execute(:decimal_parts, value) do
    with {:ok, coefficient, scale} <- Decimal.parts(value), do: {:ok, {coefficient, scale}}
  end

  defp execute({:decimal, op, context}, {a, b}), do: Decimal.operate(op, a, b, context)

  defp execute({:rescale, context}, value) do
    with {:ok, n, d} <- Decimal.ratio(value), do: Decimal.quantize(n, d, context)
  end

  defp execute({:float_to_decimal, context}, value) when is_float(value) do
    {n, d} = Binary64.ratio(value)
    Decimal.quantize(n, d, context)
  end

  defp execute({:decimal_to_float, mode}, value) when mode in [:exact, :nearest_even] do
    with {:ok, n, d} <- Decimal.ratio(value), {:ok, result} <- Binary64.round_ratio(n, d) do
      {a, b} = Binary64.ratio(result)
      if mode == :exact and a * d != n * b, do: {:error, :inexact}, else: {:ok, result}
    end
  end

  defp execute(_, _), do: {:error, :invalid_numeric_operation}
end
