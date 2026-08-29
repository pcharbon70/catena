defmodule Catena.C038TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    CE-OBL-001 CE-OBL-002 CE-OBL-003 CE-OBL-004 CE-OBL-005 CE-OBL-006
    CE-OBL-007 CE-OBL-008
  )

  test "every tagged obligation is a known compile-time obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the CE-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every compile-time obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "compile-time obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c038_compile_time_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/CE-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
