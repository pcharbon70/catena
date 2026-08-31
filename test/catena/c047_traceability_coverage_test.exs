defmodule Catena.C047TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    LC-OBL-001 LC-OBL-002 LC-OBL-003 LC-OBL-004 LC-OBL-005 LC-OBL-006 LC-OBL-007
    LC-OBL-008 LC-OBL-009 LC-OBL-010 LC-OBL-011 LC-OBL-012 LC-OBL-013 LC-OBL-014
  )

  test "every tagged obligation is a known list-comprehensions obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the LC-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every list-comprehensions obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "list-comprehensions obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c047_list_comprehensions_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/LC-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
