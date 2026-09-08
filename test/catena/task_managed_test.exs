defmodule Catena.TaskManagedTest do
  use ExUnit.Case, async: false
  alias Catena.Task.Managed, as: M
  @grace 1_000_000_000
  @labels %{
    completed: "done",
    trapped: "fault",
    cancelled: "stopped",
    exited: "exit",
    absent: "missing",
    runtime_failure: "runtime",
    external_loss: "lost"
  }

  @tag obligations: ~w(OT-OBL-010 OT-OBL-012)
  test "trapped linked failure is a typed observation and leaves user payloads separate" do
    owner = self()

    child =
      M.spawn_actor(
        fn ->
          M.receive_message()
          :erlang.error({:catena_trap, 99})
        end,
        @grace
      )

    parent =
      M.spawn_actor(
        fn ->
          M.trapping(fn ->
            link = M.link(child)
            send(owner, :linked)
            assert {:payload, 7} = M.receive_message()
            send(owner, {:observed, M.observe(link, @labels)})
          end)
        end,
        @grace
      )

    parent_pid = M.pid(parent)
    monitor = Process.monitor(parent_pid)
    assert_receive :linked, 1000
    M.send_message(parent, {:payload, 7})
    M.send_message(child, :unit)
    assert_receive {:observed, {:catena_variant, "fault", 99}}, 1000
    assert_receive {:DOWN, ^monitor, :process, ^parent_pid, :normal}, 1000
  end

  @tag obligations: ~w(OT-OBL-005 OT-OBL-008)
  test "nontrapping link failure requests resource cleanup before managed termination" do
    owner = self()

    child =
      M.spawn_actor(
        fn ->
          M.receive_message()
          :erlang.error({:catena_trap, 99})
        end,
        @grace
      )

    parent =
      M.spawn_actor(
        fn ->
          Catena.Resource.Runtime.run(
            55,
            fn n -> send(owner, {:released, n}) end,
            fn _, _ ->
              M.link(child)
              send(owner, :linked)
              M.receive_message()
            end,
            @grace
          )
        end,
        @grace
      )

    parent_pid = M.pid(parent)
    monitor = Process.monitor(parent_pid)
    assert_receive :linked, 1000
    M.send_message(child, :unit)
    assert_receive {:released, 55}, 1000

    assert_receive {:DOWN, ^monitor, :process, ^parent_pid,
                    {:catena_managed_exit, {:linked, _, {:catena_managed_trap, 99}}}},
                   1000
  end

  @tag obligations: ~w(OT-OBL-008)
  test "a link requested only by the failing peer still terminates its surviving peer" do
    owner = self()

    survivor =
      M.spawn_actor(
        fn ->
          Catena.Resource.Runtime.run(
            66,
            fn n -> send(owner, {:released, n}) end,
            fn _, _ ->
              send(owner, :ready)
              M.receive_message()
            end,
            @grace
          )
        end,
        @grace
      )

    pid = M.pid(survivor)
    monitor = Process.monitor(pid)
    assert_receive :ready, 1000

    failed =
      M.spawn_actor(
        fn ->
          M.link(survivor)
          :erlang.error({:catena_trap, 88})
        end,
        @grace
      )

    assert is_pid(M.pid(failed))
    assert_receive {:released, 66}, 1000

    assert_receive {:DOWN, ^monitor, :process, ^pid,
                    {:catena_managed_exit, {:linked, _, {:catena_managed_trap, 88}}}},
                   1000
  end

  @tag obligations: ~w(OT-OBL-009)
  test "link registration reestablishes a native link removed by the other peer" do
    owner = self()

    first =
      M.spawn_actor(
        fn ->
          second = M.receive_message()
          old = M.link(second)
          send(owner, :first_linked)
          M.receive_message()
          refute old == M.link(second)
          :erlang.error({:catena_trap, 77})
        end,
        @grace
      )

    second =
      M.spawn_actor(
        fn ->
          M.receive_message()
          link = M.link(first)
          M.unlink(link)
          send(owner, :second_unlinked)
          M.receive_message()
        end,
        @grace
      )

    pid = M.pid(second)
    monitor = Process.monitor(pid)
    M.send_message(first, second)
    assert_receive :first_linked, 1000
    M.send_message(second, :unit)
    assert_receive :second_unlinked, 1000
    M.send_message(first, :unit)

    assert_receive {:DOWN, ^monitor, :process, ^pid,
                    {:catena_managed_exit, {:linked, _, {:catena_managed_trap, 77}}}},
                   1000
  end

  @tag obligations: ~w(OT-OBL-008)
  test "normal linked termination does not terminate a nontrapping peer" do
    owner = self()

    child =
      M.spawn_actor(
        fn ->
          M.receive_message()
          :unit
        end,
        @grace
      )

    parent =
      M.spawn_actor(
        fn ->
          M.link(child)
          send(owner, :linked)
          M.receive_message()
        end,
        @grace
      )

    child_pid = M.pid(child)
    child_monitor = Process.monitor(child_pid)
    parent_pid = M.pid(parent)
    parent_monitor = Process.monitor(parent_pid)
    assert_receive :linked, 1000
    M.send_message(child, :unit)
    assert_receive {:DOWN, ^child_monitor, :process, ^child_pid, :normal}, 1000
    assert Process.alive?(parent_pid)
    M.send_message(parent, :unit)
    assert_receive {:DOWN, ^parent_monitor, :process, ^parent_pid, :normal}, 1000
  end

  @tag obligations: ~w(OT-OBL-006 OT-OBL-009)
  test "unlink invalidates its generation and relinking a dead peer observes absence" do
    owner = self()

    child =
      M.spawn_actor(
        fn ->
          M.receive_message()
          :unit
        end,
        @grace
      )

    parent =
      M.spawn_actor(
        fn ->
          M.trapping(fn ->
            old = M.link(child)
            assert old == M.link(child)
            assert :unit = M.unlink(old)
            send(owner, :unlinked)
            M.receive_message()
            current = M.link(child)
            refute current == old
            assert catch_error(M.observe(old, @labels)) == {:catena_trap, :stale_managed_link}
            send(owner, {:observed, M.observe(current, @labels)})
          end)
        end,
        @grace
      )

    child_pid = M.pid(child)
    child_monitor = Process.monitor(child_pid)
    parent_pid = M.pid(parent)
    parent_monitor = Process.monitor(parent_pid)
    assert_receive :unlinked, 1000
    M.send_message(child, :unit)
    assert_receive {:DOWN, ^child_monitor, :process, ^child_pid, :normal}, 1000
    M.send_message(parent, :unit)
    assert_receive {:observed, {:catena_variant, "missing", :unit}}, 1000
    assert_receive {:DOWN, ^parent_monitor, :process, ^parent_pid, :normal}, 1000
  end
end
