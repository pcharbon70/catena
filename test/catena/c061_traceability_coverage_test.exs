defmodule Catena.C061TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    NR-OBL-001 NR-OBL-002 NR-OBL-003 NR-OBL-004 NR-OBL-005 NR-OBL-006
    NR-OBL-007 NR-OBL-008
  )

  test "every tagged obligation is a known numeric-relationships obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the NR-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every numeric-relationships obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "numeric-relationships obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c061_numeric_relationships_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/NR-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
