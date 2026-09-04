defmodule Catena.C081TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    XB-OBL-001 XB-OBL-002 XB-OBL-003 XB-OBL-004 XB-OBL-005 XB-OBL-006
    XB-OBL-007
  )

  test "every tagged obligation is a known exception-boundary obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the XB-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every exception-boundary obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "exception-boundary obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c081_exception_boundary_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/XB-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
