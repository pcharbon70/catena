defmodule Catena.C013TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    ST-OBL-001 ST-OBL-002 ST-OBL-003 ST-OBL-004 ST-OBL-005
    ST-OBL-006 ST-OBL-007 ST-OBL-008 ST-OBL-009 ST-OBL-010
  )

  test "every tagged obligation is a known source-text obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the ST-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every source-text obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "source-text obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c013_source_text_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/ST-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
