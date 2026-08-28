defmodule Catena.C036TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    FT-OBL-001 FT-OBL-002 FT-OBL-003 FT-OBL-004 FT-OBL-005 FT-OBL-006
    FT-OBL-007 FT-OBL-008
  )

  test "every tagged obligation is a known failure obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the FT-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every failure obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "failure obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c036_failure_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/FT-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
