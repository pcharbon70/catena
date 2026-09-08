defmodule Catena.KernelCapabilityBindingTest do
  use ExUnit.Case, async: true
  alias Catena.Kernel.{CapabilityBinding, CapabilityRow}

  @a %{slot: "formal-a", family: "State", arguments: [:integer]}
  @b %{slot: "formal-b", family: "State", arguments: [:integer]}
  @actual %{slot: "actual", family: "State", arguments: [:integer]}

  test "structural identities are deterministic and encode components without delimiter collisions" do
    triples =
      for origin <- ["a", "a/b"],
          owner <- ["b/c", "c"],
          path <- [[0], [1], [0, 1]],
          do: {origin, owner, path}

    ids =
      Enum.map(triples, fn {origin, owner, path} ->
        assert {:ok, id} = CapabilityBinding.identity(origin, owner, path)
        assert {:ok, ^id} = CapabilityBinding.identity(origin, owner, path)
        assert JSON.decode!(id) == [origin, owner, path]
        id
      end)

    assert Enum.uniq(ids) == ids

    for {origin, owner, path} <- [
          {"", "x", [0]},
          {"x", "", [0]},
          {"x", "y", []},
          {"x", "y", [-1]},
          {"x", "y", ["0"]},
          {"x", "y", [0.0]},
          {<<255>>, "y", [0]}
        ] do
      assert {:error, %{id: "T002"}} = CapabilityBinding.identity(origin, owner, path)
    end
  end

  test "static selection preserves ambiguity, qualifier compatibility, and order independence" do
    for visible <- [[@a, @b], [@b, @a]] do
      assert {:error, %{details: %{candidates: ["formal-a", "formal-b"]}}} =
               CapabilityBinding.select(visible, "State", [:integer])

      assert {:ok, @b} = CapabilityBinding.select(visible, "State", [:integer], "formal-b")

      assert {:error, %{id: "T002"}} =
               CapabilityBinding.select(visible, "State", [:boolean], "formal-b")

      assert {:error, %{id: "T002"}} =
               CapabilityBinding.select(visible, "State", [:integer], "hidden")
    end

    assert {:ok, @a} = CapabilityBinding.select([@a, @a], "State", [:integer])
    assert {:error, %{id: "T002"}} = CapabilityBinding.select([], "State", [:integer])
    assert {:error, %{id: "T002"}} = CapabilityBinding.select([@a], nil, [])
  end

  test "instantiation coalesces same actuals while preserving distinct caller identities" do
    assert {:ok, [@actual]} =
             CapabilityRow.instantiate([@a, @b], %{"formal-a" => @actual, "formal-b" => @actual})

    other = %{@actual | slot: "other"}

    assert {:ok, [@actual, ^other]} =
             CapabilityRow.instantiate([@a, @b], %{"formal-a" => @actual, "formal-b" => other})

    assert CapabilityRow.instantiate([@b, @a], %{}) == CapabilityRow.normalize([@a, @b])
  end

  test "substitution is simultaneous and cannot chase or capture its own targets" do
    assert {:ok, [@a, @b]} =
             CapabilityRow.instantiate([@a, @b], %{"formal-a" => @b, "formal-b" => @a})

    assert {:ok, [@actual, @b]} =
             CapabilityRow.instantiate([@a, @b], %{"formal-a" => @b, "formal-b" => @actual})
  end

  test "forged substitutions reject changed descriptors, unknown formals and target conflicts" do
    for substitutions <- [
          nil,
          %{"missing" => @actual},
          %{"formal-a" => nil},
          %{"formal-a" => %{@actual | family: "Other"}},
          %{"formal-a" => %{@actual | arguments: [:boolean]}}
        ] do
      assert {:error, %{id: "T002"}} = CapabilityRow.instantiate([@a], substitutions)
    end

    other = %{slot: "other", family: "Log", arguments: []}

    assert {:error, %{id: "T002"}} =
             CapabilityRow.instantiate([@a, other], %{"formal-a" => %{@actual | slot: "other"}})
  end
end
