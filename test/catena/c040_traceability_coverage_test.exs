defmodule Catena.C040TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    BM-OBL-001 BM-OBL-002 BM-OBL-003 BM-OBL-004 BM-OBL-005 BM-OBL-006
    BM-OBL-007 BM-OBL-008
  )

  test "every tagged obligation is a known data-model obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the BM-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every data-model obligation has focused executable coverage" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    uncovered = MapSet.difference(expected, covered)

    assert Enum.empty?(uncovered),
           "data-model obligations lack coverage: #{inspect(MapSet.to_list(uncovered))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c040_data_model_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/BM-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
