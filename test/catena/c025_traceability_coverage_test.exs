defmodule Catena.C025TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    PK-OBL-001 PK-OBL-002 PK-OBL-003 PK-OBL-004 PK-OBL-005 PK-OBL-006
    PK-OBL-007 PK-OBL-008 PK-OBL-009 PK-OBL-010 PK-OBL-011 PK-OBL-012
  )

  test "every tagged obligation is a known package obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the PK-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every package obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "package obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c025_package_deps_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/PK-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
