defmodule Catena.C014TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    ID-OBL-001 ID-OBL-002 ID-OBL-003 ID-OBL-004 ID-OBL-005 ID-OBL-006
    ID-OBL-007 ID-OBL-008 ID-OBL-009 ID-OBL-010 ID-OBL-011 ID-OBL-012
    ID-OBL-013
  )

  test "every tagged obligation is a known identifier obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the ID-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every identifier obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "identifier obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c014_identifiers_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/ID-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
