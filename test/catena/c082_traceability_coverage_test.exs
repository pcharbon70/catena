defmodule Catena.C082TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    TL-OBL-001 TL-OBL-002 TL-OBL-003 TL-OBL-004 TL-OBL-005 TL-OBL-006
    TL-OBL-007
  )

  test "every tagged obligation is a known top-level-effects obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the TL-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every top-level-effects obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "top-level-effects obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c082_top_level_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/TL-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
