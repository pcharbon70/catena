defmodule Catena.C066TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    RN-OBL-001 RN-OBL-002 RN-OBL-003 RN-OBL-004 RN-OBL-005 RN-OBL-006
    RN-OBL-007 RN-OBL-008
  )

  test "every tagged obligation is a known name-resolution obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the RN-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every name-resolution obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "name-resolution obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c066_name_resolution_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/RN-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
