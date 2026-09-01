defmodule Catena.C067TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    DU-OBL-001 DU-OBL-002 DU-OBL-003 DU-OBL-004 DU-OBL-005 DU-OBL-006
    DU-OBL-007 DU-OBL-008
  )

  test "every tagged obligation is a known dynamic-and-unsafe-boundaries obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the DU-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every dynamic-and-unsafe-boundaries obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "dynamic-and-unsafe-boundaries obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c067_dynamic_unsafe_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/DU-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
