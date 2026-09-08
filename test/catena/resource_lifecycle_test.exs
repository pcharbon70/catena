defmodule Catena.ResourceLifecycleTest do
  use ExUnit.Case, async: true
  alias Catena.Resource.Lifecycle, as: L

  @tag obligations: ~w(RS-OBL-002 RS-OBL-003)
  test "nested scopes register successful acquisition only and release in reverse order" do
    state =
      run([
        {:open, "outer", 10},
        {:begin_acquire, "a"},
        {:acquired, "a"},
        {:open, "inner", 10},
        {:use, "owner", "a"},
        {:begin_acquire, "b"},
        {:acquired, "b"},
        {:begin_acquire, "c"},
        {:acquired, "c"},
        {:finish, {:ok, 3}}
      ])

    assert state.release.id == "c"
    assert {:error, :invalid_transition} = L.step(state, {:release_result, "b", :ok})
    state = events(state, [{:release_result, "c", :ok}, {:release_result, "b", :ok}])
    assert state.stack == ["outer"]
    assert [%{outcome: {:ok, 3}}] = state.completed

    state =
      events(state, [{:use, "owner", "a"}, {:finish, {:ok, 9}}, {:release_result, "a", :ok}])

    assert state.stack == []
    assert Enum.map(state.completed, & &1.outcome) == [{:ok, 3}, {:ok, 9}]
    assert for({:release_started, id, _} <- state.trace, do: id) == ["c", "b", "a"]
    assert {:error, :invalid_transition} = L.step(state, {:release_result, "a", :ok})
  end

  @tag obligations: ~w(RS-OBL-005)
  test "every admitted primary outcome survives successful mandatory cleanup" do
    for outcome <- [
          {:ok, 7},
          {:failure, :bad_value},
          {:abort, 8},
          {:trap, :panic},
          {:cancelled, :stop},
          {:exit, :shutdown}
        ] do
      state = owned() |> events([{:finish, outcome}, {:release_result, "r", :ok}])
      assert [%{primary: ^outcome, outcome: ^outcome, secondary: []}] = state.completed
    end
  end

  @tag obligations: ~w(RS-OBL-002)
  test "failed acquisition is never released and cannot manufacture an entry" do
    state =
      owned()
      |> events([
        {:begin_acquire, "failed"},
        {:acquire_failed, "failed", {:failure, :unavailable}}
      ])

    assert state.resources["failed"].status == :acquisition_failed
    assert state.release.id == "r"
    state = events(state, [{:release_result, "r", :ok}])
    assert [%{outcome: {:failure, :unavailable}}] = state.completed

    assert {:error, :invalid_acquisition_failure} =
             L.step(run([{:open, "s", 10}]), {:acquire_failed, nil, {:trap, :bad}})
  end

  @tag obligations: ~w(RS-OBL-005)
  test "cleanup preserves a primary trap and records ordered release failures" do
    for primary <- [
          {:ok, 1},
          {:failure, :typed},
          {:abort, 2},
          {:trap, :original},
          {:cancelled, :stop},
          {:exit, :shutdown}
        ] do
      state =
        owned()
        |> events([
          {:begin_acquire, "s"},
          {:acquired, "s"},
          {:finish, primary},
          {:release_result, "s", {:error, :first}},
          {:release_result, "r", {:error, :second}}
        ])

      assert [%{primary: ^primary, outcome: actual, secondary: [{"s", :first}, {"r", :second}]}] =
               state.completed

      assert actual ==
               if(elem(primary, 0) == :trap,
                 do: primary,
                 else: {:trap, {:mandatory_release_failed, {"s", :first}}}
               )
    end
  end

  @tag obligations: ~w(RS-OBL-006)
  test "ownership, lexical lifetime, duplicate registration and cleanup reentrancy are enforced" do
    state = owned()
    assert {:error, :invalid_resource_use} = L.step(state, {:use, "other", "r"})
    assert {:error, :invalid_acquisition} = L.step(state, {:begin_acquire, "r"})
    closing = events(state, [{:finish, {:ok, 1}}])
    assert {:error, :invalid_scope_open} = L.step(closing, {:open, "nested", 10})
    assert {:error, :invalid_acquisition} = L.step(closing, {:begin_acquire, "again"})
    assert {:error, :invalid_resource_use} = L.step(closing, {:use, "owner", "r"})
    closed = events(closing, [{:release_result, "r", :ok}, {:open, "next", 10}])
    assert {:error, :invalid_resource_use} = L.step(closed, {:use, "owner", "r"})
    assert {:error, :invalid_acquisition} = L.step(closed, {:begin_acquire, "r"})
  end

  @tag obligations: ~w(RS-OBL-007)
  test "cancellation is idempotent, safe-point observed and masked during bounded cleanup" do
    state = owned() |> events([{:cancel, :first}, {:cancel, :second}, :safe_point])
    assert state.scopes["scope"].primary == {:cancelled, :first}
    assert state.release.deadline == 10
    assert {:ok, ^state} = L.step(state, {:cancel, :third})
    assert {:error, :invalid_transition} = L.step(state, :release_deadline)
    state = events(state, [{:advance, 10}, :release_deadline])
    assert state.resources["r"].status == :release_failed

    assert [
             %{
               primary: {:cancelled, :first},
               outcome: {:trap, {:mandatory_release_failed, {"r", :deadline_exhausted}}}
             }
           ] = state.completed

    normal =
      owned() |> events([{:finish, {:ok, 3}}, {:cancel, :late}, {:release_result, "r", :ok}])

    assert [%{outcome: {:ok, 3}}] = normal.completed
  end

  @tag obligations: ~w(RS-OBL-008)
  test "deadline and release completion have exactly one winner across both eligible orders" do
    state = owned() |> events([{:finish, {:ok, 1}}, {:advance, 10}])
    {:ok, completed} = L.step(state, {:release_result, "r", :ok})
    assert {:error, :invalid_transition} = L.step(completed, :release_deadline)
    assert [%{outcome: {:ok, 1}}] = completed.completed
    {:ok, exhausted} = L.step(state, :release_deadline)
    assert {:error, :invalid_transition} = L.step(exhausted, {:release_result, "r", :ok})
    assert [%{outcome: {:trap, _}}] = exhausted.completed
  end

  @tag obligations: ~w(RS-OBL-009)
  test "external loss claims no release and unadmitted foreign registration rejects" do
    for kind <- [:process_kill, :vm_loss] do
      state = owned() |> events([{:external_loss, kind}])
      assert state.resources["r"].status == :lost
      assert state.completed == []
      assert {:error, :owner_lost} = L.step(state, {:finish, {:ok, 1}})
      refute Enum.any?(state.trace, &match?({:released, _, _}, &1))
    end

    assert {:error, :invalid_transition} = L.step(owned(), {:foreign_register, "foreign"})
  end

  defp owned, do: run([{:open, "scope", 10}, {:begin_acquire, "r"}, {:acquired, "r"}])
  defp run(events), do: events(L.new("owner"), events)

  defp events(state, events),
    do:
      Enum.reduce(events, state, fn event, state ->
        assert {:ok, next} = L.step(state, event)
        next
      end)
end
