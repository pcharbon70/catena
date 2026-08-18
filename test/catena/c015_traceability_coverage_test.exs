defmodule Catena.C015TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    LY-OBL-001 LY-OBL-002 LY-OBL-003 LY-OBL-004 LY-OBL-005 LY-OBL-006
    LY-OBL-007 LY-OBL-008 LY-OBL-009 LY-OBL-010 LY-OBL-011
  )

  test "every tagged obligation is a known layout obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the LY-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every layout obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "layout obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c015_whitespace_layout_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/LY-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
