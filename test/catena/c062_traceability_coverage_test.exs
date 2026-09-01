defmodule Catena.C062TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    AN-OBL-001 AN-OBL-002 AN-OBL-003 AN-OBL-004 AN-OBL-005 AN-OBL-006
    AN-OBL-007 AN-OBL-008
  )

  test "every tagged obligation is a known aliases-and-newtypes obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the AN-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every aliases-and-newtypes obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "aliases-and-newtypes obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c062_aliases_newtypes_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/AN-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
