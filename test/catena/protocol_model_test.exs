defmodule Catena.ProtocolModelTest do
  use ExUnit.Case, async: true
  alias Catena.Protocol.Model

  test "schema mismatch closes before payload exchange" do
    state = Model.new("schema-a", 2)
    assert {:error, :invalid_transition} = Model.step(state, {:request, :caller, 4, 10})

    assert {:ok, closed, [{:refused, :schema_mismatch}]} =
             Model.step(state, {:negotiate, "schema-b"})

    assert closed.pending == %{}
    assert {:error, :invalid_transition} = Model.step(closed, {:request, :caller, 4, 10})
  end

  test "overlapping requests correlate by generation and release admission only after terminal observation" do
    state = ready(2)
    assert {:ok, one, [{:transmit, 0, 10}]} = Model.step(state, {:request, :first, 10, 7})
    assert {:ok, two, [{:transmit, 1, 20}]} = Model.step(one, {:request, :second, 20, 8})

    assert {:ok, ^two, [{:refused, :third, :overloaded}]} =
             Model.step(two, {:request, :third, 30, 9})

    assert {:ok, answered, [{:complete, :second, 1, {:replied, 21}}]} =
             Model.step(two, {:reply, 1, 21})

    assert {:error, :already_terminal} = Model.step(answered, {:reply, 1, 22})

    assert {:ok, ^answered, [{:refused, :third, :overloaded}]} =
             Model.step(answered, {:request, :third, 30, 9})

    assert {:ok, answered, [{:observed, 1, {:replied, 21}}]} = Model.step(answered, {:observe, 1})
    assert {:error, :not_completed} = Model.step(answered, {:observe, 1})
    assert {:ok, three, [{:transmit, 2, 30}]} = Model.step(answered, {:request, :third, 30, 9})
    assert Map.keys(three.pending) |> Enum.sort() == [0, 2]
    assert {:error, :unknown_correlation} = Model.step(three, {:reply, 100, 7})
  end

  test "every ordering of reply cancellation and expiry selects exactly one completion" do
    {:ok, waiting, _} = Model.step(ready(1), {:request, :caller, 5, 10})
    assert {:error, :not_due} = Model.step(waiting, {:expire, 0})
    assert {:ok, due, []} = Model.step(waiting, {:advance, 10})

    for events <- permutations([{:reply, 0, 6}, {:cancel, 0}, {:expire, 0}]) do
      {state, completed} =
        Enum.reduce(events, {due, []}, fn event, {current, commands} ->
          case Model.step(current, event) do
            {:ok, next, output} -> {next, commands ++ output}
            {:error, :already_terminal} -> {current, commands}
          end
        end)

      assert length(completed) == 1
      assert state.pending == %{}
    end
  end

  test "peer loss completes pending requests once and does not resurrect prior replies" do
    {:ok, first, _} = Model.step(ready(3), {:request, :a, 1, 0})
    {:ok, second, _} = Model.step(first, {:request, :b, 2, 0})
    {:ok, answered, _} = Model.step(second, {:reply, 0, 9})
    assert {:ok, closed, [{:complete, :b, 1, :peer_lost}]} = Model.step(answered, :peer_lost)
    assert {:error, :invalid_transition} = Model.step(closed, {:reply, 1, 8})
    assert {:error, :invalid_transition} = Model.step(closed, :peer_lost)
    assert closed.pending == %{}
  end

  test "time is monotonic and expiry never resets the budget" do
    {:ok, state, _} = Model.step(ready(1), {:request, :a, 1, 100})
    {:ok, state, []} = Model.step(state, {:advance, 50})
    assert {:error, :invalid_transition} = Model.step(state, {:advance, 49})
    assert {:error, :not_due} = Model.step(state, {:expire, 0})
    {:ok, state, []} = Model.step(state, {:advance, 100})
    assert {:ok, _, [{:complete, :a, 0, :timed_out}]} = Model.step(state, {:expire, 0})
    assert {:error, :invalid_transition} = Model.step(state, {:request, :b, 1, -1})
  end

  defp ready(capacity) do
    {:ok, state, []} = Model.new("schema", capacity) |> Model.step({:negotiate, "schema"})
    state
  end

  defp permutations([]), do: [[]]

  defp permutations(items),
    do: for(item <- items, tail <- permutations(List.delete(items, item)), do: [item | tail])
end
