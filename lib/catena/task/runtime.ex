defmodule Catena.Task.Runtime do
  @moduledoc "Experimental owned-worker runtime; checked task-core integration is required before language admission."

  def scope(body, grace_ns) when is_function(body, 1) do
    scope_cps(fn scope, finish -> finish.(body.(scope)) end, grace_ns)
  end

  def scope_cps(body, grace_ns)
      when is_function(body, 2) and is_integer(grace_ns) and grace_ns >= 0 do
    owner = self()
    token = make_ref()
    {manager, monitor} = spawn_monitor(fn -> manager(owner, token, grace_ns) end)
    scope = {__MODULE__, owner, manager, token}
    previous = Process.get({__MODULE__, :scopes}, %{})
    Process.put({__MODULE__, :scopes}, Map.put(previous, token, manager))

    primary =
      try do
        outcome =
          classify(fn ->
            body.(scope, fn value ->
              Process.put({__MODULE__, :completed, token}, true)
              value
            end)
          end)

        case {outcome, Process.get({__MODULE__, :completed, token}, false)} do
          {{:ok, value}, false} -> {:abort, value}
          {outcome, _} -> outcome
        end
      after
        Process.put({__MODULE__, :scopes}, previous)
        Process.delete({__MODULE__, :completed, token})
      end

    send(manager, {:join, owner, token, primary})

    await_join(manager, monitor, token)
  end

  def value({{:ok, value}, _evidence}), do: value
  def value({{:abort, value}, _evidence}), do: value
  def value({{:trap, reason}, _evidence}), do: :erlang.error({:catena_trap, reason})
  def value({{:cancelled, reason}, _evidence}), do: throw({:catena_resource_cancelled, reason})
  def value({{:exit, reason}, _evidence}), do: exit({:catena_resource_exit, reason})

  defp await_join(manager, monitor, token) do
    worker = Process.get({__MODULE__, :worker})
    scopes = Process.get({__MODULE__, :scopes}, %{})

    managed =
      case Catena.Task.Managed.context() do
        {_, token} -> token
        _ -> nil
      end

    receive do
      {^token, :joined, outcome, evidence} ->
        receive do
          {:DOWN, ^monitor, :process, ^manager, _reason} -> :ok
        end

        receive do
          {:task_failed, ^token, _} -> :ok
        after
          0 -> :ok
        end

        Enum.each(evidence, fn {child, result} ->
          Catena.Effect.Runtime.trace({:task_child_completed, child, result})
        end)

        {outcome, evidence}

      {:DOWN, ^monitor, :process, ^manager, reason} ->
        {{:trap, {:task_manager_lost, reason}}, []}

      {:managed_control, ^managed, :stop, reason} when is_reference(managed) ->
        send(manager, {:join, self(), token, {:exit, reason}})
        await_join(manager, monitor, token)

      {^worker, :cancel, reason} when is_reference(worker) ->
        send(manager, {:join, self(), token, {:cancelled, reason}})
        await_join(manager, monitor, token)

      {:task_failed, scope, reason} when is_map_key(scopes, scope) ->
        send(manager, {:join, self(), token, {:exit, reason}})
        await_join(manager, monitor, token)
    end
  end

  def start({__MODULE__, owner, manager, token} = scope, body)
      when owner == self() and is_function(body, 0) do
    child = scope_call(scope, {:start, body, :computation})
    {__MODULE__, owner, manager, token, child}
  end

  def start(_, _), do: :erlang.error({:catena_trap, :invalid_task_scope_owner})

  def start_observer({__MODULE__, owner, manager, token} = scope, body)
      when owner == self() and is_function(body, 0) do
    child = scope_call(scope, {:start, body, :observer})
    {__MODULE__, owner, manager, token, child}
  end

  def observer_ready do
    send(
      Process.get({__MODULE__, :manager}),
      {Process.get({__MODULE__, :worker}), :observer_ready}
    )
  end

  def scope_call({__MODULE__, owner, manager, token} = scope, operation) when owner == self() do
    check_scope(scope)
    reply = :erlang.alias()
    monitor = Process.monitor(manager)
    worker = Process.get({__MODULE__, :worker})
    scopes = Process.get({__MODULE__, :scopes}, %{})

    managed =
      case Catena.Task.Managed.context() do
        {_, token} -> token
        _ -> nil
      end

    case operation do
      {:start, body, kind} -> send(manager, {:start, owner, token, reply, body, kind})
      _ -> send(manager, {:scope_call, owner, token, reply, operation})
    end

    try do
      receive do
        {^reply, {:ok, value}} ->
          value

        {^reply, {:error, reason}} ->
          :erlang.error({:catena_trap, reason})

        {:DOWN, ^monitor, :process, ^manager, reason} ->
          :erlang.error({:catena_trap, {:task_manager_lost, reason}})

        {:managed_control, ^managed, :stop, reason} when is_reference(managed) ->
          exit({:catena_resource_exit, reason})

        {^worker, :cancel, reason} when is_reference(worker) ->
          throw({:catena_resource_cancelled, reason})

        {:task_failed, scope_token, reason} when is_map_key(scopes, scope_token) ->
          exit({:catena_resource_exit, reason})
      end
    after
      :erlang.unalias(reply)
      Process.demonitor(monitor, [:flush])

      receive do
        {^reply, _} -> :ok
      after
        0 -> :ok
      end
    end
  end

  defp check_scope({__MODULE__, owner, manager, token}) do
    unless owner == self() and is_pid(manager) and is_reference(token) and
             Process.get({__MODULE__, :scopes}, %{})[token] == manager,
           do: :erlang.error({:catena_trap, :invalid_task_scope_owner})
  end

  def cancel({__MODULE__, owner, manager, token, child}, reason) when owner == self() do
    check_scope({__MODULE__, owner, manager, token})
    send(manager, {:cancel, owner, token, child, reason})
    :ok
  end

  def cancel(_, _), do: :erlang.error({:catena_trap, :invalid_task_owner})

  def deadline({__MODULE__, owner, manager, token} = scope, duration)
      when is_integer(duration) and duration >= 0 do
    check_scope(scope)
    {Catena.Task.Time, owner, manager, token, now() + duration}
  end

  def deadline(_, duration) when not is_integer(duration) or duration < 0,
    do: :erlang.error({:catena_trap, :invalid_duration})

  def deadline(_, _), do: :erlang.error({:catena_trap, :invalid_task_scope_owner})

  def deadline_instant({Catena.Task.Time, owner, manager, token, instant})
      when is_integer(instant) do
    unless owner == self() and is_pid(manager) and is_reference(token) and
             Process.get({__MODULE__, :scopes}, %{})[token] == manager,
           do: :erlang.error({:catena_trap, :invalid_deadline_origin})

    instant
  end

  def deadline_instant(_), do: :erlang.error({:catena_trap, :invalid_deadline_origin})
  def wait_until(deadline), do: sleep_until(deadline_instant(deadline))

  def sleep({__MODULE__, owner, manager, token}, duration)
      when owner == self() and is_integer(duration) and duration >= 0 do
    unless Process.get({__MODULE__, :scopes}, %{})[token] == manager,
      do: :erlang.error({:catena_trap, :invalid_task_scope_owner})

    sleep_until(now() + duration)
  end

  def sleep(_, duration) when not is_integer(duration) or duration < 0,
    do: :erlang.error({:catena_trap, :invalid_duration})

  def sleep(_, _), do: :erlang.error({:catena_trap, :invalid_task_scope_owner})

  defp sleep_until(deadline) do
    worker = Process.get({__MODULE__, :worker})
    scopes = Process.get({__MODULE__, :scopes}, %{})

    managed =
      case Catena.Task.Managed.context() do
        {_, token} -> token
        _ -> nil
      end

    receive do
      {:managed_control, ^managed, :stop, reason} when is_reference(managed) ->
        exit({:catena_resource_exit, reason})

      {^worker, :cancel, reason} when is_reference(worker) ->
        throw({:catena_resource_cancelled, reason})

      {:task_failed, token, reason} when is_map_key(scopes, token) ->
        exit({:catena_resource_exit, reason})
    after
      wait(deadline) -> if now() < deadline, do: sleep_until(deadline), else: :unit
    end
  end

  def receive_context,
    do: {Process.get({__MODULE__, :worker}), Process.get({__MODULE__, :scopes}, %{})}

  def checkpoint do
    Catena.Task.Managed.checkpoint()

    if Process.get({__MODULE__, :worker}) != nil or
         map_size(Process.get({__MODULE__, :scopes}, %{})) > 0, do: safe_point(), else: :ok
  end

  def safe_point do
    worker = Process.get({__MODULE__, :worker})
    scopes = Process.get({__MODULE__, :scopes}, %{})

    managed =
      case Catena.Task.Managed.context() do
        {_, token} -> token
        _ -> nil
      end

    if is_nil(worker) and map_size(scopes) == 0,
      do: :erlang.error({:catena_trap, :missing_owned_task_context})

    receive do
      {:managed_control, ^managed, :stop, reason} when is_reference(managed) ->
        exit({:catena_resource_exit, reason})

      {^worker, :cancel, reason} when is_reference(worker) ->
        throw({:catena_resource_cancelled, reason})

      {:task_failed, token, reason} when is_map_key(scopes, token) ->
        exit({:catena_resource_exit, reason})
    after
      0 -> :ok
    end
  end

  defp classify(body) do
    try do
      {:ok, body.()}
    catch
      :error, {:catena_trap, reason} -> {:trap, reason}
      :throw, {:catena_resource_cancelled, reason} -> {:cancelled, reason}
      :exit, {:catena_resource_exit, reason} -> {:exit, reason}
      kind, reason -> {:trap, {:foreign_task_failure, kind, reason}}
    end
  end

  defp manager(owner, token, grace) do
    Process.flag(:trap_exit, true)
    owner_monitor = Process.monitor(owner)

    loop(%{
      owner: owner,
      token: token,
      owner_monitor: owner_monitor,
      grace: grace,
      workers: %{},
      monitors: %{},
      primary: nil,
      evidence: [],
      joining: false
    })
  end

  defp loop(state) do
    if state.joining and Enum.all?(state.workers, fn {_, w} -> w.done end) do
      send(state.owner, {state.token, :joined, state.primary, Enum.reverse(state.evidence)})
      :ok
    else
      receive do
        {:start, owner, token, request, body, kind}
        when owner == state.owner and token == state.token ->
          if is_nil(state.primary) do
            worker_token = make_ref()
            manager = self()

            {pid, monitor} =
              :erlang.spawn_opt(
                fn ->
                  receive do
                    {^worker_token, :start} ->
                      Process.put({__MODULE__, :worker}, worker_token)
                      Process.put({__MODULE__, :manager}, manager)
                      outcome = classify(body)
                      send(manager, {worker_token, :result, outcome})
                  end
                end,
                [:link, :monitor]
              )

            worker = %{
              pid: pid,
              token: worker_token,
              monitor: monitor,
              result: nil,
              done: false,
              cancelled: false,
              deadline: nil,
              forced: false,
              kind: kind,
              waiter: nil,
              observed: false,
              inactive: false,
              ready: false,
              ready_waiter: nil
            }

            state = %{
              state
              | workers: Map.put(state.workers, worker_token, worker),
                monitors: Map.put(state.monitors, monitor, worker_token)
            }

            send(request, {request, {:ok, worker_token}})
            send(pid, {worker_token, :start})
            loop(state)
          else
            send(request, {request, {:error, :task_scope_closing}})
            loop(state)
          end

        {:scope_call, owner, token, reply, operation}
        when owner == state.owner and token == state.token ->
          loop(scope_operation(state, reply, operation))

        {:cancel, owner, token, child, reason}
        when owner == state.owner and token == state.token ->
          loop(request_cancel(state, child, reason))

        {:join, owner, token, primary} when owner == state.owner and token == state.token ->
          chosen =
            if is_nil(state.primary) or (normal?(state.primary) and not normal?(primary)),
              do: primary,
              else: state.primary

          state = %{state | joining: true, primary: chosen}

          state =
            Enum.reduce(state.workers, state, fn {child, w}, acc ->
              if w.kind == :observer, do: request_cancel(acc, child, :scope_closed), else: acc
            end)

          state = if normal?(state.primary), do: state, else: cancel_all(state, state.primary)
          loop(state)

        {worker_token, :observer_ready} when is_map_key(state.workers, worker_token) ->
          w = state.workers[worker_token]
          if w.ready_waiter != nil, do: send(w.ready_waiter, {w.ready_waiter, {:ok, :unit}})
          loop(put_in(state.workers[worker_token].ready, true))

        {worker_token, :result, outcome} when is_map_key(state.workers, worker_token) ->
          if state.workers[worker_token].forced,
            do: loop(state),
            else: loop(put_in(state.workers[worker_token].result, outcome))

        {:DOWN, monitor, :process, _, reason} when monitor == state.owner_monitor ->
          Enum.each(state.workers, fn {_, worker} ->
            unless worker.done, do: Process.exit(worker.pid, :kill)
          end)

          exit({:owner_lost, reason})

        {:DOWN, monitor, :process, _, reason} when is_map_key(state.monitors, monitor) ->
          child = state.monitors[monitor]
          worker = state.workers[child]
          outcome = worker.result || {:exit, {:external_loss, reason}}
          state = put_in(state.workers[child].done, true)

          if worker.waiter != nil,
            do: send(worker.waiter, {worker.waiter, observer_result(worker, outcome)})

          state =
            if worker.kind == :computation,
              do: %{state | evidence: [{child, outcome} | state.evidence]},
              else: state

          state =
            if worker.kind == :computation and not normal?(outcome) and
                 (is_nil(state.primary) or normal?(state.primary)) do
              send(state.owner, {:task_failed, state.token, {:child, child, outcome}})
              cancel_all(%{state | primary: {:exit, {:child, child, outcome}}}, outcome)
            else
              state
            end

          loop(state)

        {:EXIT, _, _} ->
          loop(state)
      after
        wait(next_deadline(state)) ->
          state =
            Enum.reduce(state.workers, state, fn {child, worker}, acc ->
              if worker.done or is_nil(worker.deadline) or worker.deadline > now() do
                acc
              else
                Process.unlink(worker.pid)
                Process.exit(worker.pid, :kill)
                acc = put_in(acc.workers[child].result, {:exit, :shutdown_deadline_exhausted})
                acc = put_in(acc.workers[child].deadline, nil)
                put_in(acc.workers[child].forced, true)
              end
            end)

          loop(state)
      end
    end
  end

  defp scope_operation(state, reply, {operation, {__MODULE__, owner, manager, token, child}})
       when owner == state.owner and manager == self() and token == state.token do
    case state.workers[child] do
      nil ->
        send(reply, {reply, {:error, :invalid_task_monitor}})
        state

      w ->
        case operation do
          :ready when w.kind == :observer ->
            if w.ready do
              send(reply, {reply, {:ok, :unit}})
              state
            else
              put_in(state.workers[child].ready_waiter, reply)
            end

          :resolve ->
            send(reply, {reply, {:ok, w.pid}})
            state

          :demonitor when w.kind == :observer ->
            state = put_in(state.workers[child].inactive, true)
            send(reply, {reply, {:ok, :unit}})
            request_cancel(state, child, :demonitored)

          :observe when w.kind == :observer and not w.observed and not w.inactive ->
            state = put_in(state.workers[child].observed, true)

            if w.done do
              send(reply, {reply, observer_result(w, w.result)})
              state
            else
              put_in(state.workers[child].waiter, reply)
            end

          _ ->
            send(reply, {reply, {:error, :inactive_task_monitor}})
            state
        end
    end
  end

  defp scope_operation(state, reply, _) do
    send(reply, {reply, {:error, :invalid_task_monitor}})
    state
  end

  defp observer_result(%{inactive: false}, {:ok, {:role, _, _} = value}), do: {:ok, value}
  defp observer_result(_, _), do: {:error, :inactive_task_monitor}

  defp normal?({:ok, _}), do: true
  defp normal?(_), do: false

  defp request_cancel(state, child, reason) do
    case state.workers[child] do
      %{done: false, cancelled: false} = worker ->
        send(worker.pid, {worker.token, :cancel, reason})
        state = put_in(state.workers[child].cancelled, true)

        put_in(
          state.workers[child].deadline,
          now() + div(state.grace + 999_999, 1_000_000) * 1_000_000
        )

      _ ->
        state
    end
  end

  defp cancel_all(state, reason),
    do: Enum.reduce(Map.keys(state.workers), state, &request_cancel(&2, &1, reason))

  defp next_deadline(state) do
    state.workers
    |> Map.values()
    |> Enum.reject(&(&1.done or is_nil(&1.deadline)))
    |> Enum.map(& &1.deadline)
    |> Enum.min(fn -> nil end)
  end

  defp now, do: System.monotonic_time(:nanosecond)
  defp wait(nil), do: :infinity
  defp wait(deadline), do: min(div(max(deadline - now(), 0) + 999_999, 1_000_000), 4_294_967_295)
end
