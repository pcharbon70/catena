defmodule Catena.C031TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    BS-OBL-001 BS-OBL-002 BS-OBL-003 BS-OBL-004 BS-OBL-005 BS-OBL-006
    BS-OBL-007 BS-OBL-008
  )

  test "every tagged obligation is a known bindings obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the BS-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every bindings obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "bindings obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c031_bindings_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/BS-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
