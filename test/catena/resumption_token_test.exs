defmodule Catena.ResumptionTokenTest do
  use ExUnit.Case, async: true

  alias Catena.Runtime.ResumptionToken

  test "a resumption token can be consumed exactly once" do
    token = ResumptionToken.new()
    refute ResumptionToken.consumed?(token)
    assert :ok = ResumptionToken.consume!(token)
    assert ResumptionToken.consumed?(token)
    assert_raise ArgumentError, fn -> ResumptionToken.consume!(token) end
  end

  test "concurrent consumption has one winner" do
    token = ResumptionToken.new()

    results =
      1..8
      |> Task.async_stream(fn _ ->
        try do
          ResumptionToken.consume!(token)
        rescue
          ArgumentError -> :already_consumed
        end
      end)
      |> Enum.map(fn {:ok, result} -> result end)

    assert Enum.count(results, &(&1 == :ok)) == 1
    assert Enum.count(results, &(&1 == :already_consumed)) == 7
  end
end
