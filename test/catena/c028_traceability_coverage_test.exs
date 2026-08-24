defmodule Catena.C028TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    CP-OBL-001 CP-OBL-002 CP-OBL-003 CP-OBL-004 CP-OBL-005 CP-OBL-006
    CP-OBL-007 CP-OBL-008 CP-OBL-009 CP-OBL-010
  )

  test "every tagged obligation is a known compatibility obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the CP-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every compatibility obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "compatibility obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c028_api_compat_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/CP-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
