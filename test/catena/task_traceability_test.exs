defmodule Catena.TaskTraceabilityTest do
  use ExUnit.Case, async: true

  @expected ~w(OT-OBL-001 OT-OBL-002 OT-OBL-003 OT-OBL-004 OT-OBL-005 OT-OBL-006 OT-OBL-007 OT-OBL-008 OT-OBL-009 OT-OBL-010 OT-OBL-011 OT-OBL-012)
  test "process lifetime obligation tags form the known inventory; tags do not prove semantics" do
    sources = Path.wildcard("test/catena/task_*_test.exs") -- [__ENV__.file]

    tags =
      sources
      |> Enum.map_join("\n", &File.read!/1)
      |> then(&Regex.scan(~r/@tag\s+obligations:\s*~w\(([^)]*)\)/, &1, capture: :all_but_first))
      |> Enum.flat_map(fn [text] -> Regex.scan(~r/OT-OBL-\d+/, text) |> Enum.map(&hd/1) end)

    assert MapSet.new(tags) == MapSet.new(@expected)
  end
end
