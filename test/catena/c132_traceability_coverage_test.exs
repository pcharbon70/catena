defmodule Catena.C132TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    PP-OBL-001 PP-OBL-002 PP-OBL-003 PP-OBL-004 PP-OBL-005 PP-OBL-006
    PP-OBL-007 PP-OBL-008
  )

  test "every tagged obligation is a known progress-and-preservation obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the PP-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every progress-and-preservation obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "progress-and-preservation obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c132_progress_preservation_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/PP-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
