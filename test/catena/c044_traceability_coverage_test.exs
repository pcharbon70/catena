defmodule Catena.C044TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    PC-OBL-001 PC-OBL-002 PC-OBL-003 PC-OBL-004 PC-OBL-005 PC-OBL-006
    PC-OBL-007 PC-OBL-008 PC-OBL-009
  )

  test "every tagged obligation is a known pattern-contexts obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the PC-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every pattern-contexts obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "pattern-contexts obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c044_pattern_contexts_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/PC-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
