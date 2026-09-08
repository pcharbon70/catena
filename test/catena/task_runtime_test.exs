defmodule Catena.TaskRuntimeTest do
  use ExUnit.Case, async: false
  alias Catena.Task.Runtime, as: T

  @tag obligations: ~w(OT-OBL-003)
  test "registration precedes immediate completion and normal join waits for every owned worker" do
    assert {{:ok, 42}, evidence} =
             T.scope(
               fn scope ->
                 for n <- 1..20, do: T.start(scope, fn -> n end)
                 42
               end,
               1_000_000_000
             )

    assert Enum.sort(Enum.map(evidence, fn {_, {:ok, n}} -> n end)) == Enum.to_list(1..20)
  end

  @tag obligations: ~w(OT-OBL-005 OT-OBL-011)
  test "cooperative cancellation finishes resource cleanup before owned completion" do
    owner = self()

    assert {{:exit, {:child, _, {:cancelled, 7}}}, [{_, {:cancelled, 7}}]} =
             T.scope(
               fn scope ->
                 handle =
                   T.start(scope, fn ->
                     Catena.Resource.Runtime.run(
                       11,
                       fn payload -> send(owner, {:released, payload}) end,
                       fn _finish, _handle ->
                         send(owner, :ready)
                         spin()
                       end,
                       1_000_000_000
                     )
                   end)

                 receive do
                   :ready -> :ok
                 end

                 T.cancel(handle, 7)
                 42
               end,
               1_000_000_000
             )

    assert_receive {:released, 11}, 1000
  end

  @tag obligations: ~w(OT-OBL-005 OT-OBL-011)
  test "parent cancellation during acquisition joins descendants and releases only registered resources" do
    owner = self()

    assert {{:exit, {:child, _, {:cancelled, 7}}}, _} =
             T.scope(
               fn outer ->
                 parent =
                   T.start(outer, fn ->
                     Catena.Resource.Runtime.run(
                       11,
                       fn n -> send(owner, {:released, n}) end,
                       fn _, _ ->
                         T.scope(
                           fn inner ->
                             T.start(inner, fn -> spin() end)

                             acquire = fn ->
                               send(owner, :acquiring)
                               spin()
                             end

                             Catena.Resource.Runtime.run(
                               acquire.(),
                               fn n -> send(owner, {:released, n}) end,
                               fn _, _ -> :unit end,
                               1_000_000_000
                             )
                           end,
                           1_000_000_000
                         )
                         |> T.value()
                       end,
                       1_000_000_000
                     )
                   end)

                 assert_receive :acquiring, 1000
                 T.cancel(parent, 7)
                 :unit
               end,
               1_000_000_000
             )

    assert_receive {:released, 11}, 1000
    refute_receive {:released, _}, 0
  end

  @tag obligations: ~w(OT-OBL-004 OT-OBL-005)
  test "mandatory release failure remains a terminal task outcome" do
    owner = self()

    assert {{:exit, {:child, _, {:trap, {:mandatory_release_failed, 99}}}}, _} =
             T.scope(
               fn scope ->
                 handle =
                   T.start(scope, fn ->
                     Catena.Resource.Runtime.run(
                       11,
                       fn _ -> :erlang.error({:catena_trap, 99}) end,
                       fn _, _ ->
                         send(owner, :ready)
                         spin()
                       end,
                       1_000_000_000
                     )
                   end)

                 receive do
                   :ready -> :ok
                 end

                 T.cancel(handle, 7)
                 :ok
               end,
               1_000_000_000
             )
  end

  @tag obligations: ~w(OT-OBL-011)
  test "noncooperative cancellation reports forced shutdown without inventing cleanup" do
    owner = self()

    assert {{:exit, {:child, _, {:exit, :shutdown_deadline_exhausted}}}, _} =
             T.scope(
               fn scope ->
                 handle =
                   T.start(scope, fn ->
                     send(owner, :ready)

                     receive do
                       :never -> :ok
                     end
                   end)

                 receive do
                   :ready -> :ok
                 end

                 T.cancel(handle, 7)
                 :ok
               end,
               1_000_000
             )
  end

  @tag obligations: ~w(OT-OBL-004)
  test "a child failure cancels an already registered sibling and preserves the first failure" do
    owner = self()

    assert {{:exit, {:child, _, {:trap, 99}}}, evidence} =
             T.scope(
               fn scope ->
                 T.start(scope, fn ->
                   send(owner, :ready)
                   spin()
                 end)

                 receive do
                   :ready -> :ok
                 end

                 T.start(scope, fn -> :erlang.error({:catena_trap, 99}) end)
                 :ok
               end,
               1_000_000_000
             )

    assert length(evidence) == 2
    assert Enum.any?(evidence, fn {_, result} -> match?({:cancelled, _}, result) end)
  end

  @tag obligations: ~w(OT-OBL-004 OT-OBL-011)
  test "expiring one child requests sibling cleanup rather than killing every live child" do
    owner = self()

    assert {{:exit, {:child, _, {:exit, :shutdown_deadline_exhausted}}}, evidence} =
             T.scope(
               fn scope ->
                 blocked =
                   T.start(scope, fn ->
                     send(owner, :blocked_ready)

                     receive do
                       :never -> :ok
                     end
                   end)

                 T.start(scope, fn ->
                   Catena.Resource.Runtime.run(
                     22,
                     fn payload -> send(owner, {:released, payload}) end,
                     fn _, _ ->
                       send(owner, :sibling_ready)
                       spin()
                     end,
                     1_000_000_000
                   )
                 end)

                 receive do
                   :blocked_ready -> :ok
                 end

                 receive do
                   :sibling_ready -> :ok
                 end

                 T.cancel(blocked, 7)
                 :ok
               end,
               100_000_000
             )

    assert_receive {:released, 22}, 1000
    assert Enum.any?(evidence, fn {_, outcome} -> match?({:cancelled, _}, outcome) end)
  end

  @tag obligations: ~w(OT-OBL-005 OT-OBL-011)
  test "an owner observes child failure at a safe point and unwinds its live resource" do
    owner = self()

    assert {{:exit, {:child, _, {:trap, 99}}}, _} =
             T.scope(
               fn scope ->
                 Catena.Resource.Runtime.run(
                   33,
                   fn payload -> send(owner, {:owner_released, payload}) end,
                   fn _, _ ->
                     T.start(scope, fn -> :erlang.error({:catena_trap, 99}) end)
                     spin()
                   end,
                   1_000_000_000
                 )
               end,
               1_000_000_000
             )

    assert_receive {:owner_released, 33}, 1000
    refute_receive {:task_failed, _, _}, 0
    assert catch_error(T.safe_point()) == {:catena_trap, :missing_owned_task_context}
  end

  @tag obligations: ~w(OT-OBL-002)
  test "completed scope tokens cannot launch detached late work" do
    assert {{:ok, scope}, []} = T.scope(fn scope -> scope end, 1_000_000)

    assert catch_error(T.start(scope, fn -> :unexpected end)) ==
             {:catena_trap, :invalid_task_scope_owner}
  end

  test "cancelling a blocked sleeper preserves user messages and releases its resource" do
    owner = self()

    assert {{:exit, {:child, _, {:cancelled, 7}}}, _} =
             T.scope(
               fn scope ->
                 child =
                   T.start(scope, fn ->
                     result =
                       T.scope(
                         fn inner ->
                           Catena.Resource.Runtime.run(
                             44,
                             fn payload -> send(owner, {:sleep_released, payload}) end,
                             fn _, _ ->
                               send(self(), {:user_message, 19})
                               send(owner, {:sleeping, self()})
                               T.sleep(inner, 10_000_000_000)
                             end,
                             1_000_000_000
                           )
                         end,
                         1_000_000_000
                       )

                     assert_receive {:user_message, 19}, 0
                     T.value(result)
                   end)

                 receive do
                   {:sleeping, _pid} -> :ok
                 end

                 T.cancel(child, 7)
                 :ok
               end,
               1_000_000_000
             )

    assert_receive {:sleep_released, 44}, 1000
  end

  defp spin do
    T.safe_point()
    spin()
  end
end
