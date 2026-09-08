defmodule Catena.SupervisionModelTest do
  use ExUnit.Case, async: true
  alias Catena.Supervision.Model

  defp tree(strategy, policies \\ [:permanent, :permanent, :permanent]) do
    children =
      Enum.zip(~w(a b c), policies)
      |> Enum.map(fn {id, restart} -> %{id: id, restart: restart} end)

    {:ok, state} = Model.new(children, strategy, 2, 5)
    state
  end

  test "three restart strategies stop in reverse order and start in declared order" do
    for {strategy, expected} <- [
          one_for_one: [{:start, "b", 1}],
          one_for_all: [
            {:stop, "c", 0},
            {:stop, "a", 0},
            {:start, "a", 1},
            {:start, "b", 1},
            {:start, "c", 1}
          ],
          rest_for_one: [{:stop, "c", 0}, {:start, "b", 1}, {:start, "c", 1}]
        ] do
      assert {:ok, state, ^expected} = Model.exit_child(tree(strategy), "b", 0, :failed, 10)
      assert {:error, :stale_child} = Model.exit_child(state, "b", 0, :failed, 10)
    end
  end

  test "temporary children are never restarted including collateral shutdown" do
    state = tree(:one_for_all, [:permanent, :temporary, :transient])

    assert {:ok, next, [{:stop, "c", 0}, {:stop, "b", 0}, {:start, "a", 1}, {:start, "c", 1}]} =
             Model.exit_child(state, "a", 0, :normal, 0)

    refute Enum.any?(next.children, &(&1.id == "b"))
    assert {:ok, _, []} = Model.exit_child(state, "b", 0, :failure, 0)

    for reason <- [:normal, :shutdown, {:shutdown, 5}] do
      assert {:ok, _, []} = Model.exit_child(state, "c", 0, reason, 0)
    end

    assert {:ok, _, [_ | _]} = Model.exit_child(state, "c", 0, :failure, 0)
  end

  test "restart window includes the exact boundary and stops the tree above intensity" do
    state = tree(:one_for_one)
    {:ok, state, _} = Model.exit_child(state, "a", 0, :failure, 10)
    {:ok, state, _} = Model.exit_child(state, "a", 1, :failure, 15)

    assert {:ok, closed, [{:stop, "c", 0}, {:stop, "b", 0}, {:tree_stopped, :restart_intensity}]} =
             Model.exit_child(state, "a", 2, :failure, 15)

    assert closed.phase == :closed
    assert {:error, :closed_tree} = Model.exit_child(closed, "a", 2, :failure, 16)
    assert {:ok, _, [{:start, "a", 3}]} = Model.exit_child(state, "a", 2, :failure, 16)
    assert {:error, :backward_clock} = Model.exit_child(state, "a", 2, :failure, 14)
  end

  test "explicit stop is ordered and malformed policy descriptions are rejected" do
    assert {:ok, _, [{:stop, "c", 0}, {:stop, "b", 0}, {:stop, "a", 0}]} =
             Model.stop(tree(:one_for_one))

    for children <- [
          [],
          [%{id: "", restart: :permanent}],
          [%{id: "a", restart: :unknown}],
          [%{id: "a", restart: :permanent}, %{id: "a", restart: :temporary}]
        ] do
      assert {:error, :invalid_description} = Model.new(children, :one_for_one, 2, 5)
    end

    assert {:error, _} = Model.new([%{id: "a", restart: :permanent}], :arbitrary, 2, 5)
    assert {:error, _} = Model.new([%{id: "a", restart: :permanent}], :one_for_one, 0, 5)
  end
end
