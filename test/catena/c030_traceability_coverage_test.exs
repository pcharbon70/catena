defmodule Catena.C030TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    EO-OBL-001 EO-OBL-002 EO-OBL-003 EO-OBL-004 EO-OBL-005 EO-OBL-006
    EO-OBL-007 EO-OBL-008
  )

  test "every tagged obligation is a known evaluation order obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the EO-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every evaluation order obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "evaluation order obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c030_evaluation_order_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/EO-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
