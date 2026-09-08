defmodule Catena.Protocol.Session do
  @moduledoc "Scope-owned local protocol adapter used by the exact checked application boundary."
  alias Catena.Protocol.Contract
  alias Catena.Task.Runtime

  def with_session(scope, contract, peer, capacity, handshake_ns, body)
      when is_pid(peer) and is_integer(capacity) and capacity > 0 and
             is_integer(handshake_ns) and handshake_ns >= 0 and is_function(body, 1) do
    with :ok <- Contract.validate(contract),
         true <- node(peer) == node(),
         {:ok, wire_roles} <- Contract.wire_roles(contract) do
      ready = :erlang.alias()
      owner = self()

      try do
        Runtime.start(scope, fn ->
          initialize(owner, ready, %{contract | roles: wire_roles}, peer, capacity, handshake_ns)
        end)

        case wait(ready, nil) do
          {:ok, pid, token} ->
            key = {__MODULE__, make_ref()}
            session = {__MODULE__, owner, pid, token, key}
            Process.put(key, %{capacity: capacity, requests: %{}})

            try do
              body.(session)
            after
              state = Process.delete(key)
              Enum.each(Map.keys(state.requests), &dispose_alias/1)
              send(pid, {token, :close})
            end

          {:error, _} = error ->
            error
        end
      after
        dispose_alias(ready)
      end
    else
      _ -> {:error, :invalid_protocol_contract}
    end
  end

  def submit(session, payload, duration) when is_integer(duration) and duration >= 0 do
    {_, _, _, _, key} = session
    state = owner_state!(session)

    if map_size(state.requests) >= state.capacity do
      {:error, :overloaded}
    else
      result = :erlang.alias()
      Process.put(key, %{state | requests: Map.put(state.requests, result, nil)})

      case call(session, {:request, result, payload, duration}) do
        {:ok, id} ->
          current = owner_state!(session)
          Process.put(key, %{current | requests: Map.put(current.requests, result, id)})
          {:ok, {__MODULE__, session, id, result}}

        {:error, _} = error ->
          forget(session, result)
          error
      end
    end
  end

  def submit(_, _, _), do: {:error, :invalid_duration}

  def await({__MODULE__, session, id, result}) do
    state = owner_state!(session)

    if Map.fetch(state.requests, result) != {:ok, id} do
      {:error, :invalid_request_handle}
    else
      {_, _, pid, token, _} = session

      try do
        wait(result, pid)
      after
        send(pid, {token, :observed, id})
        forget(session, result)
      end
    end
  end

  def cancel({__MODULE__, session, id, result}) do
    state = owner_state!(session)

    if Map.fetch(state.requests, result) == {:ok, id},
      do: call(session, {:cancel, id}),
      else: {:error, :invalid_request_handle}
  end

  defp owner_state!({__MODULE__, owner, _pid, _token, key}) when owner == self() do
    Process.get(key) || :erlang.error({:catena_trap, :expired_protocol_session})
  end

  defp owner_state!(_), do: :erlang.error({:catena_trap, :foreign_protocol_owner})

  defp forget({_, _, _, _, key} = session, result) do
    state = owner_state!(session)
    Process.put(key, %{state | requests: Map.delete(state.requests, result)})
    dispose_alias(result)
  end

  defp call({_, _, pid, token, _} = session, operation) do
    owner_state!(session)
    reply = :erlang.alias()

    try do
      send(pid, {token, :call, reply, operation})
      wait(reply, pid)
    after
      dispose_alias(reply)
    end
  end

  defp wait(reply, pid) do
    monitor = if is_pid(pid), do: Process.monitor(pid), else: nil
    {worker, scopes} = Runtime.receive_context()

    managed =
      case Catena.Task.Managed.context() do
        {_, token} -> token
        _ -> nil
      end

    try do
      result =
        receive do
          {^reply, result} ->
            result

          {:DOWN, ^monitor, :process, ^pid, _} when is_reference(monitor) ->
            {:error, :session_lost}

          {^worker, :cancel, reason} when is_reference(worker) ->
            throw({:catena_resource_cancelled, reason})

          {:task_failed, scope, reason} when is_map_key(scopes, scope) ->
            exit({:catena_resource_exit, reason})

          {:managed_control, ^managed, :stop, reason} when is_reference(managed) ->
            exit({:catena_resource_exit, reason})
        end

      Runtime.checkpoint()
      result
    after
      if monitor, do: Process.demonitor(monitor, [:flush])
    end
  end

  defp dispose_alias(reference) do
    :erlang.unalias(reference)

    receive do
      {^reference, _} -> :ok
    after
      0 -> :ok
    end
  end

  defp initialize(_owner, ready, contract, peer, capacity, handshake_ns) do
    token = make_ref()
    monitor = Process.monitor(peer)
    {worker, _} = Runtime.receive_context()
    send(peer, {:catena_variant, contract.roles.negotiate, self()})
    deadline = System.monotonic_time(:nanosecond) + handshake_ns

    try do
      case negotiate(contract, peer, monitor, worker, deadline) do
        :ok ->
          send(ready, {ready, {:ok, self(), token}})

          loop(%{
            token: token,
            worker: worker,
            contract: contract,
            peer: peer,
            monitor: monitor,
            capacity: capacity,
            next: 0,
            pending: %{},
            completed: MapSet.new()
          })

        {:error, _} = error ->
          send(ready, {ready, error})
          :unit
      end
    after
      Process.demonitor(monitor, [:flush])
    end
  end

  defp negotiate(contract, peer, monitor, worker, deadline) do
    ready_role = contract.roles.ready

    receive do
      {:catena_variant, ^ready_role, identity} ->
        if identity == contract.wire_identity, do: :ok, else: {:error, :schema_mismatch}

      {:DOWN, ^monitor, :process, ^peer, _} ->
        {:error, :peer_lost}

      {^worker, :cancel, reason} ->
        throw({:catena_resource_cancelled, reason})
    after
      Catena.Task.Time.remaining_ms(deadline) ->
        if Catena.Task.Time.expired?(deadline),
          do: {:error, :negotiation_timeout},
          else: negotiate(contract, peer, monitor, worker, deadline)
    end
  end

  defp loop(state) do
    Runtime.checkpoint()
    token = state.token
    worker = state.worker
    monitor = state.monitor
    peer = state.peer
    reply_role = state.contract.roles.reply

    wait_ms =
      state.pending
      |> Map.values()
      |> Enum.map(& &1.deadline)
      |> case do
        [] -> :infinity
        deadlines -> Catena.Task.Time.remaining_ms(Enum.min(deadlines))
      end

    receive do
      {^token, :call, reply, {:request, result, payload, duration}} ->
        cond do
          not Contract.valid_payload?(state.contract, :request, payload) ->
            send(reply, {reply, {:error, :invalid_payload}})
            loop(state)

          map_size(state.pending) + MapSet.size(state.completed) >= state.capacity ->
            send(reply, {reply, {:error, :overloaded}})
            loop(state)

          true ->
            id = state.next
            request = %{result: result, deadline: System.monotonic_time(:nanosecond) + duration}
            send(peer, {:catena_variant, state.contract.roles.request, {id, payload, self()}})
            send(reply, {reply, {:ok, id}})
            loop(%{state | next: id + 1, pending: Map.put(state.pending, id, request)})
        end

      {:catena_variant, ^reply_role, {id, payload}} ->
        if Contract.valid_payload?(state.contract, :response, payload) do
          loop(complete(state, id, {:ok, payload}))
        else
          loop(complete(state, id, {:error, :invalid_response}))
        end

      {^token, :call, reply, {:cancel, id}} ->
        if Map.has_key?(state.pending, id) do
          next = complete(state, id, {:error, :cancelled})
          send(reply, {reply, :ok})
          loop(next)
        else
          send(reply, {reply, {:error, :already_terminal}})
          loop(state)
        end

      {^token, :observed, id} ->
        loop(%{state | completed: MapSet.delete(state.completed, id)})

      {^token, :close} ->
        finish_all(state, :cancelled)

      {:DOWN, ^monitor, :process, ^peer, _} ->
        finish_all(state, :peer_lost)

      {^worker, :cancel, reason} ->
        finish_all(state, :cancelled)
        throw({:catena_resource_cancelled, reason})
    after
      wait_ms ->
        now = System.monotonic_time(:nanosecond)

        expired =
          state.pending
          |> Enum.filter(fn {_, request} -> request.deadline <= now end)
          |> Enum.map(&elem(&1, 0))
          |> Enum.sort()

        loop(Enum.reduce(expired, state, &complete(&2, &1, {:error, :timed_out})))
    end
  end

  defp complete(state, id, outcome) do
    case Map.pop(state.pending, id) do
      {nil, _} ->
        state

      {request, pending} ->
        send(request.result, {request.result, outcome})
        %{state | pending: pending, completed: MapSet.put(state.completed, id)}
    end
  end

  defp finish_all(state, reason) do
    state.pending
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.each(fn {_, request} ->
      send(request.result, {request.result, {:error, reason}})
    end)

    :unit
  end
end
