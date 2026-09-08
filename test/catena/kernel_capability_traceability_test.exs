defmodule Catena.KernelCapabilityTraceabilityTest do
  use ExUnit.Case, async: true

  @expected ~w(CK-OBL-001 CK-OBL-002 CK-OBL-003 CK-OBL-004 CK-OBL-005 CK-OBL-006 CK-OBL-007 CK-OBL-008)
  test "capability obligation tags form the known inventory; tags do not prove semantics" do
    sources = [
      "test/catena/kernel_capability_integration_test.exs",
      "test/catena/c047_capability_comprehension_test.exs"
    ]

    tags =
      sources
      |> Enum.map_join("\n", &File.read!/1)
      |> then(&Regex.scan(~r/@tag\s+obligations:\s*~w\(([^)]*)\)/, &1, capture: :all_but_first))
      |> Enum.flat_map(fn [text] -> Regex.scan(~r/CK-OBL-\d+/, text) |> Enum.map(&hd/1) end)

    assert MapSet.new(tags) == MapSet.new(@expected)
  end
end
