defmodule Catena.TaskTimeRuntimeTest do
  use ExUnit.Case, async: false
  alias Catena.Task.{Runtime, Time}

  @tag obligations: ~w(TM-OBL-004)
  test "deadline origins are private to a live scope and reject foreign owners" do
    assert {{:ok, deadline}, _} =
             Runtime.scope(
               fn scope ->
                 deadline = Runtime.deadline(scope, 0)
                 assert :unit = Runtime.wait_until(deadline)
                 owner = self()

                 {pid, monitor} =
                   spawn_monitor(fn ->
                     send(owner, {:foreign, catch_error(Runtime.wait_until(deadline))})
                   end)

                 assert_receive {:foreign, {:catena_trap, :invalid_deadline_origin}}, 1000
                 assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 1000
                 deadline
               end,
               1_000_000_000
             )

    assert catch_error(Runtime.wait_until(deadline)) == {:catena_trap, :invalid_deadline_origin}
    forged = {Time, self(), nil, nil, 0}
    assert catch_error(Runtime.wait_until(forged)) == {:catena_trap, :invalid_deadline_origin}
  end

  @tag obligations: ~w(TM-OBL-002)
  test "large exact durations do not overflow or get silently clamped to one host interval" do
    assert {{:ok, :unit}, _} =
             Runtime.scope(
               fn scope ->
                 duration = :erlang.bsl(1, 256)
                 before = System.monotonic_time(:nanosecond)
                 deadline = Runtime.deadline(scope, duration)
                 after_time = System.monotonic_time(:nanosecond)
                 instant = Runtime.deadline_instant(deadline)
                 assert instant >= before + duration
                 assert instant <= after_time + duration
                 assert Time.remaining_ms(instant) == 4_294_967_295
                 refute Time.expired?(instant)

                 assert catch_error(Runtime.deadline(scope, -1)) ==
                          {:catena_trap, :invalid_duration}

                 assert catch_error(Runtime.deadline(scope, 0.5)) ==
                          {:catena_trap, :invalid_duration}

                 :unit
               end,
               1_000_000_000
             )
  end

  @tag obligations: ~w(TM-OBL-007)
  test "repeated cancellation cannot interrupt masked release or hide finalizer expiry" do
    owner = self()

    assert {{:exit, {:child, _, {:trap, {:mandatory_release_failed, :deadline_exhausted}}}},
            evidence} =
             Runtime.scope(
               fn scope ->
                 child =
                   Runtime.start(scope, fn ->
                     Catena.Resource.Runtime.run(
                       11,
                       fn _ ->
                         send(owner, :releasing)

                         receive do
                           :never -> :unit
                         end
                       end,
                       fn _, _ ->
                         send(owner, :ready)
                         spin()
                       end,
                       20_000_000
                     )
                   end)

                 assert_receive :ready, 1000
                 Runtime.cancel(child, 7)
                 assert_receive :releasing, 1000
                 Runtime.cancel(child, 8)
                 :unit
               end,
               1_000_000_000
             )

    assert [{_, {:trap, {:mandatory_release_failed, :deadline_exhausted}}}] = evidence
    refute_receive :releasing, 0
  end

  @tag obligations: ~w(TM-OBL-008)
  test "completed relative and absolute waits preserve unrelated messages without timer replies" do
    marker = make_ref()
    send(self(), {:user, marker})

    assert {{:ok, :unit}, []} =
             Runtime.scope(
               fn scope ->
                 for _ <- 1..5 do
                   assert :unit = Runtime.sleep(scope, 1)
                   deadline = Runtime.deadline(scope, 1)
                   assert :unit = Runtime.wait_until(deadline)
                 end

                 :unit
               end,
               1_000_000_000
             )

    assert {:messages, [{:user, ^marker}]} = Process.info(self(), :messages)
    assert_receive {:user, ^marker}
  end

  defp spin do
    Runtime.checkpoint()
    spin()
  end
end
