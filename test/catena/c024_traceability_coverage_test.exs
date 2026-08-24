defmodule Catena.C024TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    CY-OBL-001 CY-OBL-002 CY-OBL-003 CY-OBL-004 CY-OBL-005 CY-OBL-006 CY-OBL-007
    CY-OBL-008 CY-OBL-009 CY-OBL-010
  )

  test "every tagged obligation is a known cycles obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the CY-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every cycles obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "cycles obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c024_module_cycles_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/CY-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
