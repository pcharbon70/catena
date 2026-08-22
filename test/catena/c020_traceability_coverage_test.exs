defmodule Catena.C020TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    FU-OBL-001 FU-OBL-002 FU-OBL-003 FU-OBL-004 FU-OBL-005 FU-OBL-006
    FU-OBL-007 FU-OBL-008 FU-OBL-009 FU-OBL-010 FU-OBL-011 FU-OBL-012
  )

  test "every tagged obligation is a known file-unit obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the FU-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every file-unit obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "file-unit obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c020_file_unit_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/FU-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
