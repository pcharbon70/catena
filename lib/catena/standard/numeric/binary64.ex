defmodule Catena.Standard.Numeric.Binary64 do
  @moduledoc "Finite binary64 operations with exact rational rounding, independent of host libm."
  import Bitwise
  @fraction 1 <<< 52
  def bits(value) when is_float(value) do
    <<bits::unsigned-64>> = <<value::float-64>>
    bits
  end

  def from_bits(bits) when is_integer(bits) and bits >= 0 and bits < 1 <<< 64 do
    if band(bits >>> 52, 2047) == 2047 do
      {:error, :nonfinite}
    else
      <<value::float-64>> = <<bits::unsigned-64>>
      {:ok, value}
    end
  end

  def from_bits(_), do: {:error, :invalid_bits}
  def sign(value), do: bits(value) >>> 63

  def ratio(value) when is_float(value) do
    bits = bits(value)
    field = band(bits >>> 52, 2047)
    fraction = band(bits, @fraction - 1)
    {n, e} = if field == 0, do: {fraction, -1074}, else: {fraction + @fraction, field - 1075}
    n = if bits >>> 63 == 1, do: -n, else: n
    if e >= 0, do: {n <<< e, 1}, else: {n, 1 <<< -e}
  end

  def round_ratio(n, d, zero_sign \\ 0) when is_integer(n) and is_integer(d) and d > 0 do
    sign = if n < 0, do: 1, else: if(n == 0, do: zero_sign, else: 0)
    n = abs(n)

    if n == 0 do
      from_bits(sign <<< 63)
    else
      exponent = log2_ratio(n, d)
      shift = if exponent < -1022, do: 1074, else: 52 - exponent
      {a, b} = if shift >= 0, do: {n <<< shift, d}, else: {n, d <<< -shift}
      q = nearest_even(a, b)
      encode(sign, exponent, q)
    end
  end

  defp encode(sign, exponent, q) when exponent < -1022,
    do: from_bits((sign <<< 63) + q)

  defp encode(sign, exponent, q) do
    {q, exponent} = if q >= 2 * @fraction, do: {q >>> 1, exponent + 1}, else: {q, exponent}

    if exponent > 1023,
      do: {:error, :overflow},
      else: from_bits((sign <<< 63) + ((exponent + 1023) <<< 52) + q - @fraction)
  end

  defp log2_ratio(n, d) do
    e = bit_length(n) - bit_length(d)
    below = if e >= 0, do: n < d <<< e, else: n <<< -e < d
    if below, do: e - 1, else: e
  end

  defp bit_length(n) do
    binary = :binary.encode_unsigned(n)
    <<first, _::binary>> = binary
    8 * (byte_size(binary) - 1) + small_bits(first, 0)
  end

  defp small_bits(0, count), do: count
  defp small_bits(n, count), do: small_bits(n >>> 1, count + 1)

  def nearest_even(n, d) do
    q = div(n, d)
    r = rem(n, d) * 2
    if r > d or (r == d and rem(q, 2) == 1), do: q + 1, else: q
  end

  def operate(op, a, b) when is_float(a) and is_float(b) do
    {an, ad} = ratio(a)
    {bn, bd} = ratio(b)

    case op do
      :add ->
        round_ratio(an * bd + bn * ad, ad * bd, band(sign(a), sign(b)))

      :subtract ->
        round_ratio(an * bd - bn * ad, ad * bd, band(sign(a), bxor(sign(b), 1)))

      :multiply ->
        round_ratio(an * bn, ad * bd, bxor(sign(a), sign(b)))

      :divide when bn == 0 ->
        {:error, :zero_divisor}

      :divide ->
        round_ratio(an * bd * if(bn < 0, do: -1, else: 1), ad * abs(bn), bxor(sign(a), sign(b)))

      _ ->
        {:error, :unsupported_operation}
    end
  end

  # Find the adjacent binary64 values around the exact root. Compare squared
  # exact midpoints to select nearest/even; no approximate sqrt is trusted.
  def sqrt(value) when is_float(value) do
    {n, d} = ratio(value)

    cond do
      n < 0 ->
        {:error, :domain}

      n == 0 ->
        {:ok, value}

      true ->
        low = root_floor(n, d, 0, 0x7FEFFFFFFFFFFFFF)
        {:ok, a} = from_bits(low)
        {:ok, b} = from_bits(low + 1)
        {an, ad} = ratio(a)
        {bn, bd} = ratio(b)
        midpoint_n = an * bd + bn * ad
        midpoint_d = 2 * ad * bd
        lhs = n * midpoint_d * midpoint_d
        rhs = d * midpoint_n * midpoint_n
        if lhs > rhs or (lhs == rhs and rem(low, 2) == 1), do: {:ok, b}, else: {:ok, a}
    end
  end

  defp root_floor(_, _, low, high) when low == high, do: low

  defp root_floor(n, d, low, high) do
    mid = div(low + high + 1, 2)
    {:ok, value} = from_bits(mid)
    {a, b} = ratio(value)

    if a * a * d <= n * b * b,
      do: root_floor(n, d, mid, high),
      else: root_floor(n, d, low, mid - 1)
  end

  # Exact decimal expansion is deliberately specified instead of a host-specific
  # shortest printer. At most 1074 fractional places are needed for binary64.
  def format(value) when is_float(value) do
    {n, d} = ratio(value)
    prefix = if sign(value) == 1, do: "-", else: ""
    exponent = bit_length(d) - 1
    digits = Integer.to_string(abs(n) * Integer.pow(5, exponent))

    if exponent == 0 do
      prefix <> digits <> ".0"
    else
      digits = String.pad_leading(digits, exponent + 1, "0")
      position = byte_size(digits) - exponent
      <<whole::binary-size(^position), fraction::binary>> = digits
      fraction = String.trim_trailing(fraction, "0")
      prefix <> whole <> "." <> if(fraction == "", do: "0", else: fraction)
    end
  end

  def parse(text) when is_binary(text) and byte_size(text) <= 4096 do
    case Regex.run(~r/\A([+-]?)([0-9]+)(?:\.([0-9]+))?(?:[eE]([+-]?[0-9]+))?\z/, text,
           capture: :all_but_first
         ) do
      nil ->
        {:error, :malformed_number}

      captures ->
        [sign, whole, fraction, exponent] = Enum.take(captures ++ ["", "", "", ""], 4)
        exponent = if exponent == "", do: 0, else: String.to_integer(exponent)
        coefficient = String.to_integer(whole <> fraction)
        power = exponent - byte_size(fraction)
        negative = if sign == "-", do: 1, else: 0
        magnitude = byte_size(String.trim_leading(whole <> fraction, "0")) + power

        cond do
          coefficient == 0 ->
            from_bits(negative <<< 63)

          magnitude >= 310 ->
            {:error, :overflow}

          magnitude <= -325 ->
            from_bits(negative <<< 63)

          true ->
            coefficient = if negative == 1, do: -coefficient, else: coefficient

            if power >= 0,
              do: round_ratio(coefficient * Integer.pow(10, power), 1),
              else: round_ratio(coefficient, Integer.pow(10, -power))
        end
    end
  end

  def parse(_), do: {:error, :malformed_number}
end
