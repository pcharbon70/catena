defmodule Catena.TaskMonitorTest do
  use ExUnit.Case, async: false
  alias Catena.Task.{Runtime, Monitor}

  @labels %{
    completed: "done",
    trapped: "fault",
    cancelled: "stopped",
    exited: "exit",
    absent: "missing",
    runtime_failure: "runtime",
    external_loss: "lost"
  }

  @tag obligations: ~w(OT-OBL-006)
  test "completion uses caller-selected closed variant labels with independent observations" do
    target =
      spawn(fn ->
        receive do
          :finish -> :ok
        end
      end)

    assert {{:ok, results}, _} =
             Runtime.scope(
               fn scope ->
                 a = Monitor.start(scope, target)
                 b = Monitor.start(scope, target)
                 send(target, :finish)
                 [Monitor.observe(scope, a, @labels), Monitor.observe(scope, b, @labels)]
               end,
               1_000_000_000
             )

    assert results == [{:catena_variant, "done", :unit}, {:catena_variant, "done", :unit}]
    assert Monitor.valid_labels?(@labels)
    refute Monitor.valid_labels?(Map.put(@labels, :absent, "done"))
    assert {:variant, %{tail: nil, fields: fields}} = Monitor.type(@labels)
    assert fields["fault"] == :integer
  end

  @tag obligations: ~w(OT-OBL-006)
  test "late registration classifies absence and an observed handle cannot be consumed twice" do
    {target, native} = spawn_monitor(fn -> :ok end)
    assert_receive {:DOWN, ^native, :process, ^target, :normal}, 1000

    assert {{:ok, :unit}, _} =
             Runtime.scope(
               fn scope ->
                 handle = Monitor.start(scope, target)

                 assert {:catena_variant, "missing", :unit} =
                          Monitor.observe(scope, handle, @labels)

                 assert catch_error(Monitor.observe(scope, handle, @labels)) ==
                          {:catena_trap, :inactive_task_monitor}

                 :unit
               end,
               1_000_000_000
             )
  end

  @tag obligations: ~w(OT-OBL-007)
  test "scope exit unregisters an observer without owning or terminating its target" do
    target =
      spawn(fn ->
        receive do
          :finish -> :ok
        end
      end)

    try do
      assert {{:ok, 42}, _} =
               Runtime.scope(
                 fn scope ->
                   Monitor.start(scope, target)
                   42
                 end,
                 1_000_000_000
               )

      assert Process.alive?(target)
    after
      send(target, :finish)
    end
  end

  @tag obligations: ~w(OT-OBL-007)
  test "demonitor invalidates a pending observation and preserves user messages" do
    target =
      spawn(fn ->
        receive do
          :finish -> :ok
        end
      end)

    assert {{:ok, :unit}, _} =
             Runtime.scope(
               fn scope ->
                 handle = Monitor.start(scope, target)
                 send(self(), {:user_message, 23})
                 assert :unit = Monitor.demonitor(scope, handle)
                 send(target, :finish)

                 assert catch_error(Monitor.observe(scope, handle, @labels)) ==
                          {:catena_trap, :inactive_task_monitor}

                 assert_receive {:user_message, 23}, 0
                 :unit
               end,
               1_000_000_000
             )

    refute_receive {:DOWN, _, _, _, _}, 0
  end

  @tag obligations: ~w(OT-OBL-006)
  test "unknown host exit terms are classified without widening the typed payload" do
    target =
      spawn(fn ->
        receive do
          :finish -> exit(%{untyped: self()})
        end
      end)

    assert {{:ok, {:catena_variant, "runtime", :unit}}, _} =
             Runtime.scope(
               fn scope ->
                 handle = Monitor.start(scope, target)
                 send(target, :finish)
                 Monitor.observe(scope, handle, @labels)
               end,
               1_000_000_000
             )
  end
end
