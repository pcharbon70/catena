defmodule Catena.TaskLifecycleTest do
  use ExUnit.Case, async: true
  alias Catena.Task.Lifecycle, as: L

  @tag obligations: ~w(OT-OBL-006)
  test "monitor identities are independent and termination is published after cleanup" do
    s =
      run([
        {:spawn, 0, 1, :raw},
        {:monitor, 0, 1, :a},
        {:monitor, 0, 1, :b},
        {:finish, 1, {:ok, 7}}
      ])

    assert s.signals == []
    s = run(s, [{:cleanup_done, 1, :ok}, {:deliver, 0}, {:deliver, 0}])

    assert Enum.sort(s.tasks[0].observations) == [
             {:down, :a, 1, {:ok, 7}},
             {:down, :b, 1, {:ok, 7}}
           ]

    assert {:error, :invalid_completion} = L.step(s, {:finish, 1, {:trap, 9}})
    s = run(s, [{:monitor, 0, 1, :late}, {:deliver, 0}])
    assert List.last(s.tasks[0].observations) == {:down, :late, 1, :absent}
  end

  @tag obligations: ~w(OT-OBL-007)
  test "demonitor suppresses pending delivery and flush removes only its delivered observation" do
    base =
      run([
        {:spawn, 0, 1, :raw},
        {:monitor, 0, 1, :a},
        {:finish, 1, {:ok, 0}},
        {:cleanup_done, 1, :ok}
      ])

    assert run(base, [{:demonitor, 0, :a, false}, {:deliver, 0}]).tasks[0].observations == []
    delivered = run(base, [{:deliver, 0}])
    assert length(run(delivered, [{:demonitor, 0, :a, false}]).tasks[0].observations) == 1
    assert run(delivered, [{:demonitor, 0, :a, true}]).tasks[0].observations == []
    assert {:error, :invalid_monitor_owner} = L.step(base, {:demonitor, 1, :a, true})
  end

  @tag obligations: ~w(OT-OBL-008 OT-OBL-009)
  test "links are symmetric idempotent and unlink generations defeat stale signals" do
    s = run([{:spawn, 0, 1, :raw}, {:link, 0, 1}, {:link, 1, 0}])
    assert map_size(s.links) == 1
    old = s.links[{0, 1}]

    s =
      run(s, [{:finish, 1, {:trap, 9}}, {:cleanup_done, 1, :ok}, {:unlink, 0, 1}, {:link, 0, 1}])

    refute s.links[{0, 1}] == old
    s = run(s, [{:deliver, 0}])
    assert s.tasks[0].phase == :running
    s = run(s, [{:deliver, 0}])
    assert s.tasks[0].outcome == {:exit, {1, :absent}}
    assert s.tasks[0].phase == :closing
  end

  @tag obligations: ~w(OT-OBL-008 OT-OBL-010)
  test "normal links do not terminate a nontrapping peer and trapping is read at delivery" do
    for outcome <- [{:ok, 3}, {:trap, 3}] do
      s =
        run([{:spawn, 0, 1, :raw}, {:link, 0, 1}, {:finish, 1, outcome}, {:cleanup_done, 1, :ok}])

      observed = run(s, [{:trap_exits, 0, true}, {:deliver, 0}])
      assert observed.tasks[0].observations == [{:exit, 1, outcome}]
      assert observed.tasks[0].phase == :running
      direct = run(s, [{:deliver, 0}])
      assert direct.tasks[0].phase == if(elem(outcome, 0) == :ok, do: :running, else: :closing)
    end
  end

  @tag obligations: ~w(OT-OBL-003 OT-OBL-012)
  test "normal structured join waits for owned completion observation and leaves raw children isolated" do
    s =
      run([
        {:spawn, 0, 1, :owned},
        {:spawn, 0, 2, :raw},
        {:finish, 0, {:ok, 10}},
        {:cleanup_done, 0, :ok}
      ])

    assert s.tasks[0].phase == :closing
    s = run(s, [{:finish, 1, {:ok, 20}}, {:cleanup_done, 1, :ok}])
    assert s.tasks[0].phase == :closing
    s = run(s, [{:deliver, 0}])
    assert s.tasks[0].phase == :terminal
    assert s.tasks[2].phase == :running
  end

  @tag obligations: ~w(OT-OBL-004)
  test "first observed owned failure replaces pending normal join and cancels siblings cooperatively" do
    s =
      run([
        {:spawn, 0, 1, :owned},
        {:spawn, 0, 2, :owned},
        {:finish, 0, {:ok, 10}},
        {:cleanup_done, 0, :ok},
        {:finish, 1, {:trap, 7}},
        {:cleanup_done, 1, :ok},
        {:deliver, 0}
      ])

    assert s.tasks[0].outcome == {:exit, {:child, 1, {:trap, 7}}}
    assert s.tasks[2].phase == :running
    refute is_nil(s.tasks[2].cancellation)
    s = run(s, [{:safe_point, 2}, {:cleanup_done, 2, {:error, 99}}, {:deliver, 0}])
    assert s.tasks[0].phase == :terminal
    assert s.tasks[0].outcome == {:exit, {:child, 1, {:trap, 7}}}
    assert length(s.tasks[0].observations) == 2
  end

  @tag obligations: ~w(OT-OBL-011)
  test "cancellation request is idempotent and the safe point competes with completion" do
    s = run([{:spawn, 0, 1, :owned}, {:cancel, 0, 1, 7}, {:cancel, 0, 1, 8}])
    assert {:error, :invalid_owner} = L.step(s, {:cancel, 1, 0, 3})
    cancelled = run(s, [{:safe_point, 1}, {:cleanup_done, 1, :ok}])
    assert cancelled.tasks[1].outcome == {:cancelled, 7}
    completed = run(s, [{:finish, 1, {:ok, 42}}, {:cleanup_done, 1, :ok}])
    assert completed.tasks[1].outcome == {:ok, 42}
    assert {:error, :invalid_safe_point} = L.step(completed, {:safe_point, 1})
  end

  @tag obligations: ~w(OT-OBL-005)
  test "mandatory cleanup error is terminal and cannot replace a primary trap" do
    for {primary, expected} <- [
          {{:ok, 1}, {:trap, {:mandatory_release_failed, 9}}},
          {{:trap, 7}, {:trap, 7}}
        ] do
      s = run([{:finish, 0, primary}, {:cleanup_done, 0, {:error, 9}}])
      assert s.tasks[0].phase == :terminal
      assert s.tasks[0].outcome == expected
      assert {:error, :invalid_cleanup_completion} = L.step(s, {:cleanup_done, 0, :ok})
    end
  end

  defp run(events), do: run(L.new(), events)

  defp run(state, events),
    do:
      Enum.reduce(events, state, fn event, state ->
        assert {:ok, next} = L.step(state, event)
        next
      end)
end
