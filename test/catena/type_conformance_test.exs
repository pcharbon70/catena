defmodule Catena.TypeConformanceTest do
  use ExUnit.Case, async: true

  alias Catena.Type.{Advanced, Declarative, Infer, Row, Trait, Unify}

  @tag obligations: ~w(TS-OBL-008 TS-OBL-009 TS-OBL-039 TS-OBL-041 TS-OBL-042)
  test "bounded declarative oracle agrees with inference acceptance" do
    expressions = [
      %{tag: :integer, value: 1, path: "$"},
      %{tag: :boolean, value: true, path: "$"},
      function("x", variable("x")),
      function("x", %{tag: :integer, value: 1, path: "$"}),
      call(function("x", variable("x")), %{tag: :integer, value: 1, path: "$"})
    ]

    for expression <- expressions, type <- Declarative.universe(1) do
      assert algorithm_accepts?(expression, type) == Declarative.typable?(expression, type)
    end
  end

  @tag obligations: ~w(TS-OBL-014 TS-OBL-016)
  test "unique rows ignore order and reject duplicates" do
    assert Row.unique([{"x", :integer}, {"y", :boolean}]) ==
             Row.unique([{"y", :boolean}, {"x", :integer}])

    error =
      assert_raise Catena.TypeError, fn -> Row.unique([{"x", :integer}, {"x", :boolean}]) end

    assert error.diagnostic.id == "T005"
  end

  @tag obligations: ~w(TS-OBL-015 TS-OBL-016)
  test "effect rows preserve duplicate labels and lexical identity" do
    row = Row.effects([{"State", :left}, {"State", :right}])
    assert length(row.occurrences) == 2
    assert Row.remove_effect(row, {"State", :left}).occurrences == [{"State", :right}]

    error =
      assert_raise Catena.TypeError, fn ->
        Row.union_effects(Row.effects([], :left_tail), Row.effects([], :right_tail))
      end

    assert error.diagnostic.id == "T004"
  end

  @tag obligations: ~w(TS-OBL-017 TS-OBL-018 TS-OBL-019 TS-OBL-020 TS-OBL-022)
  test "trait registry enforces ownership, non-overlap, and associated types" do
    registry = Trait.new() |> Trait.add_trait("Collection.Element", 1)

    instance = %{
      trait: "Collection.Element",
      arguments: [{"Data", "List"}],
      owner: "Data",
      context: [],
      associated_types: %{"Item" => :integer}
    }

    registry = Trait.add_instance(registry, instance)

    assert {:ok, :integer} =
             Trait.associated_type(registry, "Collection.Element", [{"Data", "List"}], "Item")

    error = assert_raise Catena.TypeError, fn -> Trait.add_instance(registry, instance) end
    assert error.diagnostic.id == "T007"
  end

  @tag obligations: ~w(TS-OBL-026 TS-OBL-027 TS-OBL-028)
  test "rigid GADT existentials cannot escape their branch" do
    assert :ok = Advanced.assert_no_escape!({:var, :ordinary}, MapSet.new([:existential]))

    error =
      assert_raise Catena.TypeError, fn ->
        Advanced.assert_no_escape!({:skolem, :existential}, MapSet.new([:existential]))
      end

    assert error.diagnostic.id == "T009"

    assert Advanced.branch(MapSet.new([{:a, :integer}]), MapSet.new([:existential]), & &1).generalize? ==
             false
  end

  defp algorithm_accepts?(expression, expected) do
    {_typed, inferred, state} = Infer.infer(expression, %{}, %{next: 100, substitution: %{}})
    _substitution = Unify.unify(inferred, expected, state.substitution)
    true
  rescue
    Catena.TypeError -> false
  end

  defp variable(name), do: %{tag: :variable, name: name, path: "$"}

  defp function(parameter, body),
    do: %{tag: :function, parameter: parameter, body: body, path: "$"}

  defp call(callee, argument), do: %{tag: :call, callee: callee, arguments: [argument], path: "$"}
end
