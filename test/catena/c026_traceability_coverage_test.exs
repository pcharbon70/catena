defmodule Catena.C026TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    PL-OBL-001 PL-OBL-002 PL-OBL-003 PL-OBL-004 PL-OBL-005 PL-OBL-006
    PL-OBL-007 PL-OBL-008 PL-OBL-009 PL-OBL-010
  )

  test "every tagged obligation is a known prelude obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the PL-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every prelude obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "prelude obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c026_prelude_policy_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/PL-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
