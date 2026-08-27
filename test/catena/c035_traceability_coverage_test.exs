defmodule Catena.C035TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    EQ-OBL-001 EQ-OBL-002 EQ-OBL-003 EQ-OBL-004 EQ-OBL-005 EQ-OBL-006
    EQ-OBL-007 EQ-OBL-008
  )

  test "every tagged obligation is a known equality obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the EQ-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every equality obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "equality obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c035_equality_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/EQ-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
