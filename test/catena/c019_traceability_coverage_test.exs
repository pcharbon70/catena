defmodule Catena.C019TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    OP-OBL-001 OP-OBL-002 OP-OBL-003 OP-OBL-004 OP-OBL-005 OP-OBL-006 OP-OBL-007
    OP-OBL-008 OP-OBL-009 OP-OBL-010 OP-OBL-011 OP-OBL-012 OP-OBL-013 OP-OBL-014
    OP-OBL-015 OP-OBL-016
  )

  test "every tagged obligation is a known operator obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the OP-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every operator obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "operator obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c019_operators_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/OP-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
