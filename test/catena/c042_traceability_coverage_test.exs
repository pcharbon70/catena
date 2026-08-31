defmodule Catena.C042TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    CO-OBL-001 CO-OBL-002 CO-OBL-003 CO-OBL-004 CO-OBL-005 CO-OBL-006
    CO-OBL-007 CO-OBL-008
  )

  test "every tagged obligation is a known collections obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the CO-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every collections obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "collections obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c042_collections_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/CO-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
