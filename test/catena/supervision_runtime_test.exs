defmodule Catena.SupervisionRuntimeTest do
  use ExUnit.Case, async: false
  alias Catena.Task.Managed

  defp child(id, owner, restart \\ :permanent) do
    body = fn ->
      send(owner, {:started, id, self()})

      try do
        case Managed.receive_message() do
          :finish -> :unit
          :fail -> :erlang.error({:catena_trap, :deliberate})
        end
      after
        send(owner, {:cleaned, id})
      end
    end

    %{
      id: id,
      start: {Managed, :start_link, [body, 100_000_000]},
      restart: restart,
      shutdown: 1_000,
      type: :worker,
      modules: [Managed]
    }
  end

  defp start(children, strategy, intensity \\ 5) do
    old = Process.flag(:trap_exit, true)

    {:ok, sup} =
      Catena.Supervision.Runtime.start_link(
        %{strategy: strategy, intensity: intensity, period: 5},
        children
      )

    on_exit(fn ->
      if Process.alive?(sup), do: :gen_server.stop(sup, :shutdown, 5_000)
    end)

    {sup, old}
  end

  defp target(sup, id) do
    {^id, pid, :worker, _} = List.keyfind(:supervisor.which_children(sup), id, 0)
    {Managed, pid}
  end

  test "OTP policies agree with the independent model for a failed middle child" do
    owner = self()

    for {strategy, restarts, stops} <- [
          {:one_for_one, [:b], []},
          {:one_for_all, [:a, :b, :c], [:c, :a]},
          {:rest_for_one, [:b, :c], [:c]}
        ] do
      {sup, old} = start(Enum.map([:a, :b, :c], &child(&1, owner)), strategy)
      for id <- [:a, :b, :c], do: assert_receive({:started, ^id, _})
      Managed.send_message(target(sup, :b), :fail)
      assert_receive {:cleaned, :b}
      for id <- stops, do: assert_receive({:cleaned, ^id})
      for id <- restarts, do: assert_receive({:started, ^id, _})
      :gen_server.stop(sup, :shutdown, 5_000)
      for id <- [:c, :b, :a], do: assert_receive({:cleaned, ^id})
      assert_receive {:EXIT, ^sup, :shutdown}
      Process.flag(:trap_exit, old)
    end
  end

  test "temporary and normally completed transient children are not restarted" do
    {sup, old} =
      start([child(:a, self(), :temporary), child(:b, self(), :transient)], :one_for_all)

    assert_receive {:started, :a, _}
    assert_receive {:started, :b, _}
    Managed.send_message(target(sup, :a), :fail)
    Managed.send_message(target(sup, :b), :finish)
    assert_receive {:cleaned, :a}
    assert_receive {:cleaned, :b}
    refute_receive {:started, _, _}
    :gen_server.stop(sup, :shutdown, 5_000)
    assert_receive {:EXIT, ^sup, :shutdown}
    Process.flag(:trap_exit, old)
  end

  test "restart storms terminate the tree instead of retrying indefinitely" do
    {sup, old} = start([child(:a, self())], :one_for_one, 1)
    assert_receive {:started, :a, _}
    Managed.send_message(target(sup, :a), :fail)
    assert_receive {:cleaned, :a}
    assert_receive {:started, :a, _}
    Managed.send_message(target(sup, :a), :fail)
    assert_receive {:cleaned, :a}
    assert_receive {:EXIT, ^sup, :shutdown}
    refute Process.alive?(sup)
    Process.flag(:trap_exit, old)
  end

  test "startup failure rolls back earlier children before returning failure" do
    old = Process.flag(:trap_exit, true)
    first = child(:a, self())
    bad = %{first | id: :bad, start: {__MODULE__, :missing_start, []}}

    assert {:error, {:shutdown, {:failed_to_start_child, :bad, _}}} =
             Catena.Supervision.Runtime.start_link(
               %{strategy: :one_for_one, intensity: 1, period: 5},
               [first, bad]
             )

    assert_receive {:started, :a, worker}
    assert_receive {:cleaned, :a}
    refute Process.alive?(worker)

    receive do
      {:EXIT, _, _} -> :ok
    after
      100 -> :ok
    end

    Process.flag(:trap_exit, old)
  end

  test "noncooperative shutdown force terminates the managed worker within its explicit grace" do
    owner = self()

    body = fn ->
      send(owner, {:started_uncooperative, self()})

      receive do
        :never -> :unit
      end
    end

    spec = %{
      id: :stuck,
      start: {Managed, :start_link, [body, 0]},
      restart: :temporary,
      shutdown: 1_000,
      type: :worker,
      modules: [Managed]
    }

    {sup, old} = start([spec], :one_for_one)
    assert_receive {:started_uncooperative, worker}
    {Managed, broker} = target(sup, :stuck)
    monitor = Process.monitor(broker)
    :gen_server.stop(sup, :shutdown, 5_000)

    assert_receive {:DOWN, ^monitor, :process, ^broker,
                    {:catena_managed_exit, :shutdown_deadline_exhausted}}

    refute Process.alive?(worker)
    assert_receive {:EXIT, ^sup, :shutdown}
    Process.flag(:trap_exit, old)
  end

  test "a child that fails immediately during every start reaches the intensity bound" do
    owner = self()

    body = fn ->
      send(owner, {:attempt, self()})
      :erlang.error({:catena_trap, :startup_fault})
    end

    spec = %{
      id: :failing,
      start: {Managed, :start_link, [body, 10_000_000]},
      restart: :permanent,
      shutdown: 1_000,
      type: :worker,
      modules: [Managed]
    }

    old = Process.flag(:trap_exit, true)

    result =
      Catena.Supervision.Runtime.start_link(%{strategy: :one_for_one, intensity: 1, period: 5}, [
        spec
      ])

    assert {:ok, sup} = result
    assert_receive {:attempt, first}
    assert_receive {:attempt, second}
    assert first != second
    assert_receive {:EXIT, ^sup, :shutdown}
    refute Process.alive?(first)
    refute Process.alive?(second)
    refute_receive {:attempt, _}
    Process.flag(:trap_exit, old)
  end

  test "supervisor loss is routed through the managed owner instead of killing its user worker directly" do
    owner = self()
    body = fn -> :erlang.error({:catena_trap, :immediate}) end

    spec = %{
      id: :failing,
      start: {Managed, :start_link, [body, 10_000_000]},
      restart: :permanent,
      shutdown: 1_000,
      type: :worker,
      modules: [Managed]
    }

    actor =
      Managed.spawn_actor(
        fn ->
          Managed.trapping(fn ->
            link =
              Managed.start_supervision(%{strategy: :one_for_one, intensity: 1, period: 5}, [spec])

            labels = %{
              completed: :completed,
              trapped: :trap,
              exited: :exit,
              cancelled: :cancelled,
              runtime_failure: :runtime,
              external_loss: :external,
              absent: :absent
            }

            send(owner, {:supervision_observed, Managed.observe(link, labels)})
            :finish = Managed.receive_message()
            :unit
          end)
        end,
        1_000_000_000
      )

    pid = Managed.pid(actor)
    monitor = Process.monitor(pid)
    assert_receive {:supervision_observed, {:catena_variant, :runtime, :unit}}
    Managed.send_message(actor, :finish)
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}
  end

  test "managed owner completion joins supervised children before publishing its result" do
    owner = self()

    actor =
      Managed.spawn_actor(
        fn ->
          Managed.start_supervision(%{strategy: :one_for_one, intensity: 1, period: 5}, [
            child(:owned, owner)
          ])

          :finish = Managed.receive_message()
          :unit
        end,
        1_000_000_000
      )

    pid = Managed.pid(actor)
    monitor = Process.monitor(pid)
    assert_receive {:started, :owned, worker}
    Managed.send_message(actor, :finish)
    assert_receive {:cleaned, :owned}
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}
    refute Process.alive?(worker)
  end
end
