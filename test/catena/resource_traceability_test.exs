defmodule Catena.ResourceTraceabilityTest do
  use ExUnit.Case, async: true

  @expected ~w(RS-OBL-001 RS-OBL-002 RS-OBL-003 RS-OBL-004 RS-OBL-005 RS-OBL-006 RS-OBL-007 RS-OBL-008 RS-OBL-009 RS-OBL-010)
  test "resource obligation tags form the known inventory; tags do not prove semantics" do
    sources = Path.wildcard("test/catena/resource_*_test.exs") -- [__ENV__.file]

    tags =
      sources
      |> Enum.map_join("\n", &File.read!/1)
      |> then(&Regex.scan(~r/@tag\s+obligations:\s*~w\(([^)]*)\)/, &1, capture: :all_but_first))
      |> Enum.flat_map(fn [text] -> Regex.scan(~r/RS-OBL-\d+/, text) |> Enum.map(&hd/1) end)

    assert MapSet.new(tags) == MapSet.new(@expected)
  end
end
