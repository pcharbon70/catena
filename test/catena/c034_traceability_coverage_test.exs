defmodule Catena.C034TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    RT-OBL-001 RT-OBL-002 RT-OBL-003 RT-OBL-004 RT-OBL-005 RT-OBL-006
    RT-OBL-007 RT-OBL-008
  )

  test "every tagged obligation is a known recursion obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the RT-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every recursion obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "recursion obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c034_recursion_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/RT-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
