defmodule Catena.TaskOtpRelationshipTest do
  use ExUnit.Case, async: false

  @tag obligations: ~w(OT-OBL-006)
  test "independent monitors observe one termination each and late registration reports absence" do
    child =
      spawn(fn ->
        receive do
          :finish -> :ok
        end
      end)

    a = Process.monitor(child)
    b = Process.monitor(child)
    send(child, :finish)
    assert_receive {:DOWN, ^a, :process, ^child, :normal}, 1000
    assert_receive {:DOWN, ^b, :process, ^child, :normal}, 1000
    refute_receive {:DOWN, ^a, :process, ^child, _}, 0
    late = Process.monitor(child)
    assert_receive {:DOWN, ^late, :process, ^child, :noproc}, 1000
  end

  @tag obligations: ~w(OT-OBL-007)
  test "demonitor flush removes its notification without consuming an unrelated message" do
    child = spawn(fn -> :ok end)
    ref = Process.monitor(child)
    send(self(), {:user_message, 7})
    assert Process.demonitor(ref, [:flush])
    assert_receive {:user_message, 7}, 1000
    refute_receive {:DOWN, ^ref, :process, ^child, _}, 20
  end

  @tag obligations: ~w(OT-OBL-008 OT-OBL-010)
  test "a linked failure reaches a trapping peer as an exit observation" do
    parent = self()

    {observer, monitor} =
      spawn_monitor(fn ->
        Process.flag(:trap_exit, true)

        child =
          spawn_link(fn ->
            receive do
              :fail -> exit(:probe_failure)
            end
          end)

        send(child, :fail)

        receive do
          {:EXIT, ^child, reason} -> send(parent, {:observed_exit, reason})
        end
      end)

    assert_receive {:observed_exit, :probe_failure}, 1000
    assert_receive {:DOWN, ^monitor, :process, ^observer, :normal}, 1000
  end

  @tag obligations: ~w(OT-OBL-008)
  test "normal linked completion spares a nontrapping peer but abnormal completion terminates it" do
    parent = self()

    {normal, monitor} =
      spawn_monitor(fn ->
        {child, child_monitor} = :erlang.spawn_opt(fn -> :ok end, [:link, :monitor])

        receive do
          {:DOWN, ^child_monitor, :process, ^child, :normal} -> :ok
        end

        send(parent, :normal_link_survived)
      end)

    assert_receive :normal_link_survived, 1000
    assert_receive {:DOWN, ^monitor, :process, ^normal, :normal}, 1000

    {abnormal, failed} =
      spawn_monitor(fn ->
        spawn_link(fn -> exit(:linked_failure) end)

        receive do
          :never -> :ok
        end
      end)

    assert_receive {:DOWN, ^failed, :process, ^abnormal, :linked_failure}, 1000
  end

  @tag obligations: ~w(OT-OBL-009)
  test "unlink before child failure prevents linked termination" do
    parent = self()

    {observer, monitor} =
      spawn_monitor(fn ->
        child =
          spawn_link(fn ->
            receive do
              :fail -> exit(:removed_link)
            end
          end)

        child_monitor = Process.monitor(child)
        Process.unlink(child)
        send(child, :fail)

        receive do
          {:DOWN, ^child_monitor, :process, ^child, :removed_link} -> :ok
        end

        send(parent, :unlink_survived)
      end)

    assert_receive :unlink_survived, 1000
    assert_receive {:DOWN, ^monitor, :process, ^observer, :normal}, 1000
  end
end
