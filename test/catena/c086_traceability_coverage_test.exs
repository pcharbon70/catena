defmodule Catena.C086TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    RC-OBL-001 RC-OBL-002 RC-OBL-003 RC-OBL-004 RC-OBL-005 RC-OBL-006
    RC-OBL-007 RC-OBL-008
  )

  test "every tagged obligation is a known selective-receive obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the RC-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every selective-receive obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "selective-receive obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c086_selective_receive_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/RC-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
