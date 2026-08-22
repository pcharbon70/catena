defmodule Catena.C022TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    IM-OBL-001 IM-OBL-002 IM-OBL-003 IM-OBL-004 IM-OBL-005 IM-OBL-006 IM-OBL-007
    IM-OBL-008 IM-OBL-009 IM-OBL-010 IM-OBL-011 IM-OBL-012 IM-OBL-013
  )

  test "every tagged obligation is a known import/export obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the IM-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every import/export obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "import/export obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c022_import_exports_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/IM-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
