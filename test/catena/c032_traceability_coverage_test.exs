defmodule Catena.C032TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    FC-OBL-001 FC-OBL-002 FC-OBL-003 FC-OBL-004 FC-OBL-005 FC-OBL-006
    FC-OBL-007 FC-OBL-008
  )

  test "every tagged obligation is a known functions obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the FC-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every functions obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "functions obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c032_functions_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/FC-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
