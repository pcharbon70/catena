defmodule Catena.Standard.Numeric.Decimal do
  @moduledoc "Ordinary scaled-integer decimal values with explicit output scale and rounding."
  @identity :"catena://numeric-contract/0.1.67::CatenaNumericRoles::Decimal"
  @modes [:half_even, :half_away, :toward_zero, :floor, :ceiling, :exact]
  def modes, do: @modes

  def context(precision, scale, rounding)
      when is_integer(precision) and precision in 1..4096 and
             is_integer(scale) and scale in -1024..1024 and rounding in @modes,
      do: {:ok, %{version: "0.1.67", precision: precision, scale: scale, rounding: rounding}}

  def context(_, _, _), do: {:error, :invalid_decimal_context}

  def verify_context(context) do
    case context(context.precision, context.scale, context.rounding) do
      {:ok, ^context} -> :ok
      _ -> {:error, :invalid_decimal_context}
    end
  rescue
    _ -> {:error, :invalid_decimal_context}
  end

  def new(coefficient, scale)
      when is_integer(coefficient) and is_integer(scale) and scale in -1024..1024 do
    if abs(coefficient) < Integer.pow(10, 4096),
      do: {:ok, {:catena_adt, @identity, 0, {coefficient, scale}}},
      else: {:error, :decimal_precision}
  end

  def new(_, _), do: {:error, :invalid_decimal}

  def parts({:catena_adt, @identity, 0, {coefficient, scale}} = value) do
    with {:ok, ^value} <- new(coefficient, scale), do: {:ok, coefficient, scale}
  end

  def parts(_), do: {:error, :invalid_decimal}

  def ratio(value) do
    with {:ok, n, scale} <- parts(value) do
      if scale >= 0,
        do: {:ok, n, Integer.pow(10, scale)},
        else: {:ok, n * Integer.pow(10, -scale), 1}
    end
  end

  def round_integer(n, d, mode)
      when is_integer(n) and is_integer(d) and d > 0 and mode in @modes do
    q = div(abs(n), d)
    r = rem(abs(n), d)

    increment =
      case mode do
        :half_even -> 2 * r > d or (2 * r == d and rem(q, 2) == 1)
        :half_away -> 2 * r >= d
        :toward_zero -> false
        :floor -> n < 0 and r != 0
        :ceiling -> n > 0 and r != 0
        :exact -> false
      end

    cond do
      mode == :exact and r != 0 -> {:error, :inexact}
      true -> {:ok, (q + if(increment, do: 1, else: 0)) * if(n < 0, do: -1, else: 1)}
    end
  end

  def round_integer(_, _, _), do: {:error, :invalid_rounding}

  def quantize(n, d, context) do
    with :ok <- verify_context(context) do
      scale = context.scale

      {n, d} =
        if scale >= 0, do: {n * Integer.pow(10, scale), d}, else: {n, d * Integer.pow(10, -scale)}

      with {:ok, coefficient} <- round_integer(n, d, context.rounding) do
        if abs(coefficient) < Integer.pow(10, context.precision),
          do: new(coefficient, scale),
          else: {:error, :decimal_precision}
      end
    end
  end

  def operate(op, a, b, context) do
    with :ok <- verify_context(context), {:ok, an, ad} <- ratio(a), {:ok, bn, bd} <- ratio(b) do
      case op do
        :add -> quantize(an * bd + bn * ad, ad * bd, context)
        :subtract -> quantize(an * bd - bn * ad, ad * bd, context)
        :multiply -> quantize(an * bn, ad * bd, context)
        :divide when bn == 0 -> {:error, :zero_divisor}
        :divide -> quantize(an * bd * if(bn < 0, do: -1, else: 1), ad * abs(bn), context)
        _ -> {:error, :unsupported_operation}
      end
    end
  end
end
