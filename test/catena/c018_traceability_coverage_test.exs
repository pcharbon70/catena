defmodule Catena.C018TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    NM-OBL-001 NM-OBL-002 NM-OBL-003 NM-OBL-004 NM-OBL-005 NM-OBL-006 NM-OBL-007
    NM-OBL-008 NM-OBL-009 NM-OBL-010 NM-OBL-011 NM-OBL-012 NM-OBL-013 NM-OBL-014
  )

  test "every tagged obligation is a known numeric obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the NM-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every numeric obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "numeric obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c018_numeric_literal_semantics_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/NM-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
