defmodule Catena.C140TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    EA-OBL-001 EA-OBL-002 EA-OBL-003 EA-OBL-004 EA-OBL-005 EA-OBL-006
    EA-OBL-007
  )

  test "every tagged obligation is a known excluded-advanced-types obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the EA-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every excluded-advanced-types obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "excluded-advanced-types obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c140_excluded_advanced_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/EA-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
