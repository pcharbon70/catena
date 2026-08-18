defmodule Catena.C016TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    CM-OBL-001 CM-OBL-002 CM-OBL-003 CM-OBL-004 CM-OBL-005 CM-OBL-006
    CM-OBL-007 CM-OBL-008 CM-OBL-009 CM-OBL-010 CM-OBL-011 CM-OBL-012
  )

  test "every tagged obligation is a known comment obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the CM-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every comment obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "comment obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c016_comments_documentation_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/CM-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
