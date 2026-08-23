defmodule Catena.C023TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    AB-OBL-001 AB-OBL-002 AB-OBL-003 AB-OBL-004 AB-OBL-005 AB-OBL-006 AB-OBL-007
  )

  test "every tagged obligation is a known abstraction obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the AB-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every abstraction obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "abstraction obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c023_abstraction_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/AB-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
