defmodule Catena.C041TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    SR-OBL-001 SR-OBL-002 SR-OBL-003 SR-OBL-004 SR-OBL-005 SR-OBL-006
    SR-OBL-007 SR-OBL-008
  )

  test "every tagged obligation is a known records obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the SR-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every records obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "records obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c041_records_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/SR-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
