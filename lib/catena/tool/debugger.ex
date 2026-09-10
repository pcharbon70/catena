defmodule Catena.Tool.Debugger.Handle do
  @moduledoc false
  @enforce_keys [:owner, :server, :token]
  defstruct [:owner, :server, :token]
end

defmodule Catena.Tool.Debugger do
  @moduledoc "Bounded source-aware debugging sessions over verified Catena artifacts."

  use GenServer

  alias Catena.Debugging
  alias Catena.Tool.Debugger.Handle

  @version "0.1.89"
  @event_kinds ~w(checkpoint return closure handler process-spawn process-exit message-send message-receive foreign-enter foreign-exit cancellation failure derivation unavailable)a
  @identity_keys ~w(pid parent child sender receiver process)a
  @redacted_keys ~w(value values payload message secret arguments)a

  def profile do
    %{
      version: @version,
      maximum_events: 10_000,
      maximum_event_bytes: 16_384,
      event_kinds: @event_kinds,
      values: :redacted_by_default,
      source_authority: :verified_debug_sidecar,
      breakpoints: :explicit_cooperative_checkpoints,
      trace_delivery: :bounded_drop_oldest,
      trace_perturbation: :observable,
      timing: :host_relative_nonsemantic,
      erased_declarations: :external_evidence_only,
      stripped_origins: :unavailable,
      optimized_values: :explicitly_unavailable,
      public_source: :held_for_p109
    }
  end

  def open(artifact, input, options \\ []) do
    maximum = Keyword.get(options, :maximum_events, 1_024)

    with true <- Keyword.keys(options) -- [:maximum_events] == [],
         true <- is_integer(maximum) and maximum in 1..profile().maximum_events,
         :ok <- Debugging.verify(artifact, input, build_options(artifact)),
         owner = self(),
         token = make_ref(),
         {:ok, server} <- GenServer.start(__MODULE__, {owner, token, artifact, input, maximum}) do
      {:ok, %Handle{owner: owner, server: server, token: token}}
    else
      false -> {:error, :invalid_debug_session}
      {:error, _} = error -> error
    end
  end

  def close(%Handle{} = handle), do: owner_call(handle, :close)

  def nodes(%Handle{} = handle), do: owner_call(handle, :nodes)

  def add_breakpoint(%Handle{} = handle, node),
    do: owner_call(handle, {:add_breakpoint, node})

  def remove_breakpoint(%Handle{} = handle, node),
    do: owner_call(handle, {:remove_breakpoint, node})

  def continue(%Handle{} = handle, pause), do: owner_call(handle, {:continue, pause})
  def snapshot(%Handle{} = handle), do: owner_call(handle, :snapshot)
  def profile_snapshot(%Handle{} = handle), do: owner_call(handle, :profile_snapshot)

  def checkpoint(%Handle{} = handle, node, attributes \\ %{}),
    do: session_call(handle, {:checkpoint, node, attributes}, :infinity)

  def event(handle, kind, node \\ nil, attributes \\ %{})

  def event(%Handle{} = handle, kind, node, attributes)
      when kind in @event_kinds,
      do: session_call(handle, {:event, kind, node, attributes})

  def event(_, _, _, _), do: {:error, :invalid_debug_event}

  def run(%Handle{} = handle, node, fun) when is_function(fun, 0) do
    with :ok <- checkpoint(handle, node) do
      try do
        result = fun.()
        :ok = event(handle, :return, node, %{outcome: :ok})
        {:ok, result}
      catch
        class, reason ->
          stack = __STACKTRACE__
          _ = event(handle, :failure, node, %{reason: class})
          report = crash_report(handle, class, reason, stack)
          {:trap, report}
      end
    end
  end

  def crash_report(%Handle{} = handle, class, _reason, stack) when is_list(stack) do
    session_call(handle, {:crash_report, class, stack})
  end

  def evidence(%Handle{} = handle, id), do: session_call(handle, {:evidence, id})

  @impl true
  def init({owner, token, artifact, input, maximum}) do
    monitor = Process.monitor(owner)

    {:ok,
     %{
       owner: owner,
       owner_monitor: monitor,
       token: token,
       artifact: artifact,
       input: input,
       maximum: maximum,
       breakpoints: MapSet.new(),
       events: :queue.new(),
       event_count: 0,
       dropped: 0,
       sequence: 0,
       processes: %{},
       next_process: 1,
       pauses: %{},
       started: System.monotonic_time()
     }}
  end

  @impl true
  def handle_call({token, command}, from, %{token: token} = state) do
    dispatch(command, from, state)
  end

  def handle_call(_, _, state), do: {:reply, {:error, :invalid_debug_session}, state}

  @impl true
  def handle_info(
        {:DOWN, monitor, :process, owner, _},
        %{owner_monitor: monitor, owner: owner} = state
      ),
      do: {:stop, :normal, state}

  def handle_info(_, state), do: {:noreply, state}

  defp dispatch(:close, {owner, _}, %{owner: owner} = state),
    do: {:stop, :normal, :ok, release_pauses(state)}

  defp dispatch(:nodes, {owner, _}, %{owner: owner} = state) do
    nodes =
      state.artifact.sidecar.nodes
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {id, origin} -> %{id: id, origin: visible_origin(state, origin)} end)

    {:reply, {:ok, nodes}, state}
  end

  defp dispatch({:add_breakpoint, node}, {owner, _}, %{owner: owner} = state) do
    if valid_node?(state, node) and state.artifact.sidecar.profile.mode == :sidecar do
      {:reply, :ok, %{state | breakpoints: MapSet.put(state.breakpoints, node)}}
    else
      {:reply, {:error, :unavailable_breakpoint_origin}, state}
    end
  end

  defp dispatch({:remove_breakpoint, node}, {owner, _}, %{owner: owner} = state),
    do: {:reply, :ok, %{state | breakpoints: MapSet.delete(state.breakpoints, node)}}

  defp dispatch({:continue, pause}, {owner, _}, %{owner: owner} = state) do
    case Map.pop(state.pauses, pause) do
      {nil, _} ->
        {:reply, {:error, :unknown_pause}, state}

      {waiting, pauses} ->
        GenServer.reply(waiting, :ok)
        {:reply, :ok, %{state | pauses: pauses}}
    end
  end

  defp dispatch(:snapshot, {owner, _}, %{owner: owner} = state),
    do: {:reply, {:ok, snapshot_of(state)}, state}

  defp dispatch(:profile_snapshot, {owner, _}, %{owner: owner} = state),
    do: {:reply, {:ok, profile_of(state)}, state}

  defp dispatch({:checkpoint, node, attributes}, {caller, _} = from, state) do
    with :ok <- validate_event(state, node, attributes) do
      {event, state} = record(state, :checkpoint, node, attributes, caller)

      if MapSet.member?(state.breakpoints, node) do
        pause = make_ref()
        send(state.owner, {:catena_debug_paused, pause, event})
        {:noreply, %{state | pauses: Map.put(state.pauses, pause, from)}}
      else
        {:reply, :ok, state}
      end
    else
      error -> {:reply, error, state}
    end
  end

  defp dispatch({:event, kind, node, attributes}, {caller, _}, state) do
    with :ok <- validate_event(state, node, attributes) do
      {_event, state} = record(state, kind, node, attributes, caller)
      {:reply, :ok, state}
    else
      error -> {:reply, error, state}
    end
  end

  defp dispatch({:crash_report, class, stack}, _from, state) do
    frames =
      case Debugging.frames(state.artifact, state.input, stack,
             build: build_options(state.artifact)
           ) do
        {:ok, frames} -> frames
        _ -> []
      end

    report = %{
      version: @version,
      class: if(class in [:error, :exit, :throw], do: class, else: :unknown),
      reason: :redacted,
      frames: frames,
      trace: snapshot_of(state),
      erased_runtime_values: :unavailable
    }

    {:reply, report, state}
  end

  defp dispatch({:evidence, id}, _from, state),
    do:
      {:reply,
       Debugging.evidence_link(
         state.artifact,
         state.input,
         id,
         build_options(state.artifact)
       ), state}

  defp dispatch(_, _, state), do: {:reply, {:error, :debug_owner_denied}, state}

  defp validate_event(state, node, attributes) do
    cond do
      not (is_nil(node) or valid_node?(state, node)) ->
        {:error, :unknown_debug_node}

      not is_map(attributes) ->
        {:error, :invalid_debug_event}

      byte_size(:erlang.term_to_binary(attributes)) > profile().maximum_event_bytes ->
        {:error, :debug_event_limit}

      true ->
        :ok
    end
  rescue
    _ -> {:error, :invalid_debug_event}
  end

  defp record(state, kind, node, attributes, actor) do
    {attributes, state} = sanitize(attributes, state)
    {process, state} = identity(actor, state)
    sequence = state.sequence + 1

    event = %{
      id:
        Catena.Calling.Descriptor.digest(
          {state.artifact.sidecar.binary_digest, sequence, kind, node}
        ),
      sequence: sequence,
      kind: kind,
      node: node,
      origin: node && visible_origin(state, state.artifact.sidecar.nodes[node]),
      process: process,
      attributes: attributes,
      elapsed_native: System.monotonic_time() - state.started
    }

    state = %{state | sequence: sequence}

    {events, count, dropped} =
      enqueue(state.events, state.event_count, state.dropped, event, state.maximum)

    {event, %{state | events: events, event_count: count, dropped: dropped}}
  end

  defp sanitize(attributes, state) do
    Enum.reduce(attributes, {%{}, state}, fn {key, value}, {safe, current} ->
      cond do
        key in @redacted_keys ->
          {Map.put(safe, key, :redacted), current}

        key in @identity_keys and is_pid(value) ->
          {identity, current} = identity(value, current)
          {Map.put(safe, key, identity), current}

        true ->
          {Map.put(safe, key, Catena.Runtime.Secret.redact(value)), current}
      end
    end)
  end

  defp identity(pid, state) do
    case Map.fetch(state.processes, pid) do
      {:ok, id} ->
        {id, state}

      :error ->
        id = "process-#{state.next_process}"

        {id,
         %{
           state
           | processes: Map.put(state.processes, pid, id),
             next_process: state.next_process + 1
         }}
    end
  end

  defp enqueue(queue, count, dropped, event, maximum) when count < maximum,
    do: {:queue.in(event, queue), count + 1, dropped}

  defp enqueue(queue, count, dropped, event, _maximum) do
    {{:value, _}, queue} = :queue.out(queue)
    {:queue.in(event, queue), count, dropped + 1}
  end

  defp snapshot_of(state) do
    %{
      version: @version,
      artifact: state.artifact.sidecar.binary_digest,
      source: state.artifact.sidecar.source_digest,
      events: :queue.to_list(state.events),
      dropped: state.dropped,
      active_pauses: map_size(state.pauses),
      process_count: map_size(state.processes),
      trace_complete: state.dropped == 0,
      perturbs_execution: true,
      timing_semantic: false
    }
  end

  defp profile_of(state) do
    groups =
      state.events
      |> :queue.to_list()
      |> Enum.group_by(&{&1.node, &1.kind})
      |> Enum.map(fn {{node, kind}, events} ->
        %{
          node: node,
          kind: kind,
          origin: List.first(events).origin,
          observations: length(events),
          first_elapsed_native: List.first(events).elapsed_native,
          last_elapsed_native: List.last(events).elapsed_native
        }
      end)
      |> Enum.sort_by(&{&1.node || -1, &1.kind})

    %{
      version: @version,
      groups: groups,
      dropped: state.dropped,
      complete: state.dropped == 0,
      perturbing: true,
      portable_timing: false
    }
  end

  defp visible_origin(%{artifact: %{sidecar: %{profile: %{mode: :stripped}}}}, _), do: nil
  defp visible_origin(_, origin), do: origin

  defp valid_node?(state, node),
    do: is_integer(node) and Map.has_key?(state.artifact.sidecar.nodes, node)

  defp release_pauses(state) do
    Enum.each(state.pauses, fn {_pause, waiting} ->
      GenServer.reply(waiting, {:error, :debug_session_closed})
    end)

    %{state | pauses: %{}}
  end

  defp build_options(%{sidecar: %{profile: profile}}),
    do: Enum.map(profile, fn {key, value} -> {key, value} end)

  defp build_options(_), do: []

  defp owner_call(%Handle{owner: owner} = handle, command) when owner == self(),
    do: session_call(handle, command)

  defp owner_call(_, _), do: {:error, :debug_owner_denied}

  defp session_call(%Handle{server: server, token: token}, command, timeout \\ 5_000) do
    GenServer.call(server, {token, command}, timeout)
  catch
    :exit, _ -> {:error, :expired_debug_session}
  end
end
