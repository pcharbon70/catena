defmodule Catena.C021TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    NS-OBL-001 NS-OBL-002 NS-OBL-003 NS-OBL-004 NS-OBL-005 NS-OBL-006 NS-OBL-007
    NS-OBL-008 NS-OBL-009 NS-OBL-010 NS-OBL-011 NS-OBL-012 NS-OBL-013 NS-OBL-014
  )

  test "every tagged obligation is a known namespace obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the NS-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every namespace obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "namespace obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c021_namespaces_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/NS-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
