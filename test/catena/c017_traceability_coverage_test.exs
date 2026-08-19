defmodule Catena.C017TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    LT-OBL-001 LT-OBL-002 LT-OBL-003 LT-OBL-004 LT-OBL-005 LT-OBL-006
    LT-OBL-007 LT-OBL-008 LT-OBL-009 LT-OBL-010 LT-OBL-011 LT-OBL-012
  )

  test "every tagged obligation is a known literal obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the LT-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every literal obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "literal obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c017_literal_grammar_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/LT-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
