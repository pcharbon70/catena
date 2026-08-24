defmodule Catena.C027TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    EN-OBL-001 EN-OBL-002 EN-OBL-003 EN-OBL-004 EN-OBL-005 EN-OBL-006
    EN-OBL-007 EN-OBL-008 EN-OBL-009 EN-OBL-010
  )

  test "every tagged obligation is a known entry points obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the EN-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every entry points obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "entry points obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c027_entry_points_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/EN-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
