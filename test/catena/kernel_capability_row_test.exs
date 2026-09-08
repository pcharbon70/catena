defmodule Catena.KernelCapabilityRowTest do
  use ExUnit.Case, async: true

  alias Catena.Kernel.CapabilityRow

  @left %{slot: "origin/handler/left", family: "origin/State", arguments: [:integer]}
  @right %{slot: "origin/handler/right", family: "origin/State", arguments: [:integer]}
  @log %{slot: "origin/handler/log", family: "origin/Log", arguments: []}

  test "repetition coalesces one identity and retains distinct same-family capabilities" do
    assert {:ok, [@left]} = CapabilityRow.normalize([@left, @left, @left])
    assert {:ok, [@left, @right]} = CapabilityRow.union([@right], [@left, @right])
  end

  test "closed-row union satisfies the finite semilattice laws independently of input order" do
    rows =
      for mask <- 0..7 do
        [@left, @right, @log]
        |> Enum.with_index()
        |> Enum.filter(fn {_entry, bit} -> Bitwise.band(mask, Bitwise.bsl(1, bit)) != 0 end)
        |> Enum.map(&elem(&1, 0))
      end

    for a <- rows do
      assert CapabilityRow.union(a, []) == CapabilityRow.normalize(a)
      assert CapabilityRow.union(a, a) == CapabilityRow.normalize(a)
      assert CapabilityRow.normalize(Enum.reverse(a)) == CapabilityRow.normalize(a)

      for b <- rows do
        assert CapabilityRow.union(a, b) == CapabilityRow.union(b, a)

        for c <- rows do
          {:ok, ab} = CapabilityRow.union(a, b)
          {:ok, bc} = CapabilityRow.union(b, c)
          assert CapabilityRow.union(ab, c) == CapabilityRow.union(a, bc)
        end
      end
    end
  end

  test "handling removes only the selected identity and rejects an absent identity" do
    assert {:ok, [@right]} = CapabilityRow.subtract([@left, @right, @left], @left.slot)

    assert {:error, %{id: "T002", details: %{slot: "missing"}}} =
             CapabilityRow.subtract([@left, @right], "missing")
  end

  test "a repeated slot cannot change family or type arguments in either order" do
    for conflict <- [%{@left | family: "other/State"}, %{@left | arguments: [:boolean]}] do
      for row <- [[@left, conflict], [conflict, @left]] do
        assert {:error, %{id: "T002", details: %{slot: "origin/handler/left"}}} =
                 CapabilityRow.normalize(row)
      end
    end
  end

  test "malformed and open rows reject instead of silently discarding fields or constraints" do
    for row <- [
          nil,
          %{},
          %{entries: [@left], tail: "e"},
          [nil],
          [%{@left | slot: ""}],
          [%{@left | family: ""}],
          [%{@left | arguments: :invalid}],
          [Map.put(@left, :tail, "e")]
        ] do
      assert {:error, %{id: "T002"}} = CapabilityRow.normalize(row)
    end

    assert {:error, %{id: "T002"}} = CapabilityRow.union([], :invalid)
    assert {:error, %{id: "T002"}} = CapabilityRow.subtract([@left], nil)
  end
end
