defmodule Catena.NumericLibraryTest do
  use ExUnit.Case, async: true
  import Bitwise
  alias Catena.Standard.Numeric, as: N
  alias Catena.Standard.Numeric.{Binary64, Decimal}
  alias Catena.Standard.Outcomes, as: O

  @moduletag obligations:
               ~w(NL-OBL-001 NL-OBL-002 NL-OBL-003 NL-OBL-004 NL-OBL-005 NL-OBL-006 NL-OBL-007 NL-OBL-008 NL-OBL-009 NL-OBL-010)
  @limits %{nodes: 1000, bytes: 100_000, depth: 100}
  defp run(op, value), do: N.run(op, value, @limits)

  test "Euclidean identities include both divisor signs and very large integers" do
    for a <- -60..60, b <- -20..20, b != 0 do
      assert {:ok, {:catena_adt, _, 1, {{q, r}}}} = run(:euclidean, {a, b})
      assert a == b * q + r
      assert r >= 0 and r < abs(b)
    end

    big = Integer.pow(10, 5000)
    assert {:ok, {:catena_adt, _, 1, {{q, r}}}} = run(:euclidean, {-big, -17})
    assert -big == -17 * q + r
    assert {:ok, result} = run(:euclidean, {1, 0})
    assert result == O.failure(N.failure(:zero_divisor))
    assert {:error, :invalid_numeric_operation} = run({:integer, :add}, {1, 1.0})
  end

  test "binary64 signed zero, subnormal ties, finite maximum and typed faults" do
    {:ok, tiny} = Binary64.from_bits(1)
    {:ok, negative_zero} = Binary64.from_bits(1 <<< 63)
    {:ok, maximum} = Binary64.from_bits(0x7FEFFFFFFFFFFFFF)
    assert {:ok, value} = Binary64.operate(:multiply, tiny, 0.5)
    assert Binary64.bits(value) == 0
    assert {:ok, value} = Binary64.operate(:multiply, -tiny, 0.5)
    assert Binary64.bits(value) == 1 <<< 63
    assert {:ok, value} = Binary64.operate(:add, negative_zero, negative_zero)
    assert Binary64.bits(value) == 1 <<< 63
    assert {:ok, value} = Binary64.operate(:subtract, negative_zero, 0.0)
    assert Binary64.bits(value) == 1 <<< 63
    assert run({:float, :multiply}, {maximum, 2.0}) == {:ok, O.failure(N.failure(:overflow))}

    assert run({:float, :divide}, {1.0, negative_zero}) ==
             {:ok, O.failure(N.failure(:zero_divisor))}

    for bits <- [0x7FF0000000000000, 0x7FF8000000000001, 0xFFF0000000000000] do
      assert run(:float_from_bits, bits) == {:ok, O.failure(N.failure(:nonfinite))}
    end
  end

  test "exact decimal formatting round trips finite bit patterns including extreme exponents" do
    patterns =
      [
        0,
        1 <<< 63,
        1,
        2,
        3,
        0xFFFFFFFFFFFFF,
        0x10000000000000,
        0x7FEFFFFFFFFFFFFF,
        0x3FB999999999999A,
        0x4340000000000000
      ] ++ for(i <- 1..2000, do: rem(i * 6_364_136_223_846_793_005, 1 <<< 64))

    for bits <- patterns, band(bits >>> 52, 2047) != 2047 do
      {:ok, value} = Binary64.from_bits(bits)
      text = Binary64.format(value)
      assert {:ok, parsed} = Binary64.parse(text)
      assert Binary64.bits(parsed) == bits
    end

    assert Binary64.format(0.1) == "0.1000000000000000055511151231257827021181583404541015625"
    assert {:ok, -0.0} = Binary64.parse("-1e-9999999999999999")
    assert {:error, :overflow} = Binary64.parse("1e99999999999999999")

    for malformed <- ["NaN", "inf", "1 ", " 1", "1_0", ".1", "1.", <<255>>] do
      assert {:error, :malformed_number} = Binary64.parse(malformed)
    end

    assert {:ok, _} = Binary64.parse(String.duplicate("0", 4095) <> "1")
    assert {:error, :malformed_number} = Binary64.parse(String.duplicate("0", 4096) <> "1")
  end

  test "explicit conversions report loss and preserve large integer rounding boundaries" do
    assert run({:int_to_float, :exact}, (1 <<< 53) + 1) == {:ok, O.failure(N.failure(:inexact))}

    assert run({:int_to_float, :nearest_even}, (1 <<< 53) + 1) ==
             {:ok, O.success(9_007_199_254_740_992.0)}

    assert run({:int_to_float, :nearest_even}, 1 <<< 20000) ==
             {:ok, O.failure(N.failure(:overflow))}

    for {mode, expected} <- [
          half_even: -2,
          half_away: -3,
          toward_zero: -2,
          floor: -3,
          ceiling: -2
        ] do
      assert run({:float_to_int, mode}, -2.5) == {:ok, O.success(expected)}
    end

    assert run({:float_to_int, :exact}, -2.5) == {:ok, O.failure(N.failure(:inexact))}
    assert run({:float_to_int, :exact}, -0.0) == {:ok, O.success(0)}
  end

  test "decimal scale is retained and arithmetic rounds once to an explicit context" do
    {:ok, a} = Decimal.new(125, 2)
    {:ok, b} = Decimal.new(10, 1)

    for {mode, coefficient} <- [
          half_even: 12,
          half_away: 13,
          toward_zero: 12,
          floor: 12,
          ceiling: 13
        ] do
      {:ok, context} = Decimal.context(3, 1, mode)
      {:ok, wanted} = Decimal.new(coefficient, 1)
      assert run({:rescale, context}, a) == {:ok, O.success(wanted)}
    end

    {:ok, exact} = Decimal.context(3, 1, :exact)
    assert run({:rescale, exact}, a) == {:ok, O.failure(N.failure(:inexact))}
    {:ok, ctx} = Decimal.context(4, 2, :half_even)
    {:ok, wanted} = Decimal.new(225, 2)
    assert run({:decimal, :add, ctx}, {a, b}) == {:ok, O.success(wanted)}
    {:ok, zero} = Decimal.new(0, 0)
    assert run({:decimal, :divide, ctx}, {a, zero}) == {:ok, O.failure(N.failure(:zero_divisor))}
    {:ok, small} = Decimal.context(1, 2, :half_even)
    assert run({:rescale, small}, a) == {:ok, O.failure(N.failure(:decimal_precision))}
    assert {:error, :invalid_decimal_context} = Decimal.verify_context(Map.put(ctx, :extra, true))
    {:ok, one_tenth} = Decimal.new(1, 1)
    assert run({:decimal_to_float, :exact}, one_tenth) == {:ok, O.failure(N.failure(:inexact))}
    assert run({:decimal_to_float, :nearest_even}, one_tenth) == {:ok, O.success(0.1)}
    assert run({:float_to_decimal, ctx}, 0.1) == {:ok, O.success(elem(Decimal.new(10, 2), 1))}
    for scale <- [-1024, 1024], do: assert({:ok, _} = Decimal.new(1, scale))
    assert {:error, _} = Decimal.new(1, 1025)
    assert {:error, _} = Decimal.context(4097, 0, :half_even)
  end

  test "sqrt matches exact midpoint inequalities and the independent host oracle" do
    for bits <-
          [0, 1, 2, 3, 0xFFFFFFFFFFFFF, 0x10000000000000, 0x7FEFFFFFFFFFFFFF] ++
            for(i <- 1..300, do: rem(i * 6_364_136_223_846_793_005, 0x7FF0000000000000)) do
      {:ok, value} = Binary64.from_bits(bits)
      {:ok, root} = Binary64.sqrt(value)
      assert Binary64.bits(root) == Binary64.bits(:math.sqrt(value))

      if bits > 0 do
        root_bits = Binary64.bits(root)
        {:ok, previous} = Binary64.from_bits(root_bits - 1)
        {:ok, next} = Binary64.from_bits(root_bits + 1)
        {n, d} = Binary64.ratio(value)
        {r, rd} = Binary64.ratio(root)

        for {neighbor, side} <- [{previous, :lower}, {next, :upper}] do
          {a, ad} = Binary64.ratio(neighbor)
          mid_n = a * rd + r * ad
          mid_d = 2 * ad * rd
          comparison = n * mid_d * mid_d - d * mid_n * mid_n
          assert if(side == :lower, do: comparison >= 0, else: comparison <= 0)
        end
      end
    end

    assert run(:sqrt, -1.0) == {:ok, O.failure(N.failure(:domain))}
    assert {:ok, zero} = Binary64.sqrt(-0.0)
    assert Binary64.sign(zero) == 1
    assert {:error, :invalid_numeric_operation} = run(:sin, 1.0)
  end

  test "complete carrier budgets refuse input and output independently of numeric failure" do
    assert {:error, :byte_budget_exhausted} =
             N.run({:integer, :multiply}, {1 <<< 500, 1 <<< 500}, %{
               nodes: 100,
               bytes: 100,
               depth: 20
             })

    assert {:error, :byte_budget_exhausted} =
             N.run(:format_float, 0.1, %{nodes: 100, bytes: 30, depth: 20})

    assert {:error, :invalid_validation_budget} = N.run(:sqrt, 1.0, %{})
  end

  test "binary arithmetic matches independent Python exact-rational vectors" do
    vectors = JSON.decode!(File.read!("test/fixtures/numeric-rational-vectors.json"))

    operations = %{
      "add" => :add,
      "subtract" => :subtract,
      "multiply" => :multiply,
      "divide" => :divide
    }

    for [op, ab, bb, expected] <- vectors do
      {:ok, a} = Binary64.from_bits(ab)
      {:ok, b} = Binary64.from_bits(bb)
      actual = Binary64.operate(operations[op], a, b)

      case expected do
        "overflow" ->
          assert actual == {:error, :overflow}

        "zero_divisor" ->
          assert actual == {:error, :zero_divisor}

        expected ->
          assert {:ok, value} = actual
          assert Binary64.bits(value) == expected
      end
    end
  end
end
