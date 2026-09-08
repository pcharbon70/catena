defmodule Catena.TaskTimeTraceabilityTest do
  use ExUnit.Case, async: true

  @expected ~w(TM-OBL-001 TM-OBL-002 TM-OBL-003 TM-OBL-004 TM-OBL-005 TM-OBL-006 TM-OBL-007 TM-OBL-008)
  test "time obligation tags form the known inventory; tags do not prove semantics" do
    sources = Path.wildcard("test/catena/task_*_test.exs") -- [__ENV__.file]

    tags =
      sources
      |> Enum.map_join("\n", &File.read!/1)
      |> then(&Regex.scan(~r/@tag\s+obligations:\s*~w\(([^)]*)\)/, &1, capture: :all_but_first))
      |> Enum.flat_map(fn [text] -> Regex.scan(~r/TM-OBL-\d+/, text) |> Enum.map(&hd/1) end)

    assert MapSet.new(tags) == MapSet.new(@expected)
  end
end
