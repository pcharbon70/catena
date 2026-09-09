defmodule Catena.Foreign.Session do
  @moduledoc false
  use GenServer
  alias Catena.Foreign.{Adapter, Budget, Callback, Codec, Control, Descriptor, NativeValue}

  def start(owner, grants, limits, options) do
    maximum = Keyword.get(options, :max_operations, 1024)
    callbacks = Keyword.get(options, :allow_callbacks, false)
    sensitive = Keyword.get(options, :sensitive, false)

    with {:ok, selection} <- Descriptor.selection(options),
         true <-
           is_list(grants) and Budget.valid?(limits) and is_integer(maximum) and maximum > 0 and
             is_boolean(callbacks) and is_boolean(sensitive),
         true <-
           Enum.all?(
             grants,
             &(Descriptor.verify(&1) == :ok and
                 &1.version in admitted_versions(selection.language_revision))
           ),
         native_grants <- Keyword.get(options, :native_grants, []),
         true <- native_grants == [] or selection.language_revision == "0.1.62",
         {:ok, native_values} <- NativeValue.prepare_grants(native_grants, maximum) do
      GenServer.start(
        __MODULE__,
        {owner, grants, limits, maximum, callbacks, Keyword.fetch!(options, :release_token),
         native_values, sensitive}
      )
    else
      _ -> {:error, :invalid_foreign_scope_setup}
    end
  end

  defp admitted_versions("0.1.61"), do: ["0.1.61"]
  defp admitted_versions("0.1.62"), do: ["0.1.61", "0.1.62"]

  @impl true
  def init({owner, grants, limits, maximum, callbacks, release_token, native_values, sensitive}) do
    Process.flag(:trap_exit, true)
    if sensitive, do: Process.flag(:sensitive, true)
    monitor = Process.monitor(owner)

    {:ok,
     %{
       owner: owner,
       sensitive: sensitive,
       release_token: release_token,
       native_values:
         native_values
         |> Enum.with_index()
         |> Map.new(fn {entry, index} -> {make_ref(), Map.put(entry, :index, index)} end),
       owner_monitor: monitor,
       scope: nil,
       grants: Map.new(grants, &{Descriptor.id(&1), &1}),
       limits: limits,
       maximum: maximum,
       allow_callbacks: callbacks,
       count: 0,
       calls: %{},
       callbacks: %{},
       workers: %{},
       events: [],
       waiter: nil
     }}
  end

  @impl true
  def format_status(%{state: %{sensitive: true}} = status),
    do: Map.new(status, fn {key, _} -> {key, :redacted} end)

  def format_status(status), do: status

  @impl true
  def handle_call(
        {:bind, {Adapter, owner, manager, _} = scope},
        {owner, _},
        %{owner: owner, scope: nil} = state
      )
      when manager == self(),
      do: {:reply, :ok, %{state | scope: scope}}

  def handle_call({scope, operation}, {owner, _} = from, %{scope: scope, owner: owner} = state),
    do: operation(operation, from, state)

  def handle_call({:release, token}, _, %{release_token: token} = state) do
    evidence = %{interrupted_workers: map_size(state.workers), external_effects: :possible}
    state = shutdown(state)
    {:stop, :normal, {:ok, evidence}, state}
  end

  def handle_call({:native_send, handle, message}, _, state) do
    with {:ok, %{role: %{kind: :local_process, mailbox: codec}, value: pid}} <-
           native(state, handle),
         true <- state.count < state.maximum,
         {:ok, payload} <- Codec.to_native(codec, message, state.limits) do
      # As with C010 send, acceptance is not delivery or liveness acknowledgement.
      send(pid, payload)
      state = %{state | count: state.count + 1}
      {:reply, {:ok, :unit}, event(state, {:native_send, handle.token})}
    else
      false -> {:reply, {:error, :foreign_operation_limit}, state}
      {:error, _} = error -> {:reply, error, state}
      _ -> {:reply, {:error, :native_role_operation_forbidden}, state}
    end
  end

  # A callback token is intentionally usable by its trusted foreign recipient,
  # whereas scope, call and revocation authority never transfer with it.
  def handle_call({:enter_callback, token, native}, from, state) do
    case Map.get(state.callbacks, token) do
      %{active: true, busy: false, description: description} = callback ->
        if state.count >= state.maximum do
          {:reply, {:error, :foreign_operation_limit}, state}
        else
          case Codec.decode(description.input, native, state.limits) do
            {:ok, _} ->
              state = put_in(state.callbacks[token], %{callback | busy: true})
              state = event(state, {:callback_entered, token})

              {state, _} =
                worker(state, {:callback, token, from}, fn _ ->
                  case Callback.invoke(description, native, state.limits) do
                    {:ok, value} -> {:completed, value}
                    {:error, reason} -> {:trap, {:callback_boundary, reason}}
                  end
                end)

              {:noreply, state}

            error ->
              {:reply, error, state}
          end
        end

      %{active: true, busy: true} ->
        {:reply, {:error, :callback_reentry}, state}

      _ ->
        {:reply, {:error, :revoked_foreign_callback}, state}
    end
  end

  def handle_call(_, _, state), do: {:reply, {:error, :foreign_authority_denied}, state}

  defp operation({:native_fetch, index}, _, state) do
    case Enum.find(state.native_values, fn {_, entry} -> entry.index == index end) do
      {token, entry} ->
        {:reply, {:ok, %NativeValue{scope: state.scope, token: token, role: entry.role}}, state}

      nil ->
        {:reply, {:error, :unknown_native_grant}, state}
    end
  end

  defp operation({:authorize, declaration}, _, state),
    do: {:reply, granted(state, declaration), state}

  defp operation({:start, declaration, arguments}, _, state) do
    with :ok <- granted(state, declaration),
         true <- state.count < state.maximum,
         {:ok, native} <- arguments(state, declaration.arguments, arguments) do
      token = make_ref()
      state = event(state, {:foreign_requested, token, declaration.effect, declaration.identity})

      {state, worker} =
        worker(state, {:call, token}, fn control ->
          # Recheck identity immediately before entry, not only at grant setup.
          with :ok <- Descriptor.verify(declaration),
               :ok <- Control.checkpoint(control) do
            {module, function} = declaration.host
            result = apply(module, function, native ++ [control])

            case Codec.decode(declaration.result, result, state.limits) do
              {:ok, semantic} -> {:completed, semantic}
              {:error, reason} -> {:trap, {:foreign_result_boundary, reason}}
            end
          else
            {:error, reason} -> {:trap, reason}
          end
        end)

      call = %{worker: worker, outcome: :pending, cancellation: false}
      state = put_in(state.calls[token], call)
      {:reply, {:ok, {__MODULE__, state.scope, token}}, state}
    else
      false -> {:reply, {:error, :foreign_operation_limit}, state}
      error -> {:reply, error, state}
    end
  end

  defp operation({:poll, handle}, _, state) do
    case call(state, handle) do
      {:ok, _, call} -> {:reply, {:ok, call.outcome}, state}
      error -> {:reply, error, state}
    end
  end

  defp operation({:await, handle, milliseconds}, from, state) do
    case call(state, handle) do
      {:ok, token, %{outcome: :pending}} when milliseconds > 0 and state.waiter == nil ->
        timer =
          Process.send_after(self(), {:wait_expired, token}, min(milliseconds, 4_294_967_295))

        {:noreply, %{state | waiter: {token, from, timer}}}

      {:ok, _, call} ->
        {:reply, {:ok, call.outcome}, state}

      error ->
        {:reply, error, state}
    end
  end

  defp operation({:cancel, handle, reason}, _, state) do
    with :ok <- Budget.check(reason, state.limits),
         {:ok, token, call} <- call(state, handle) do
      cond do
        call.outcome != :pending ->
          {:reply, {:ok, {:already_terminal, call.outcome}}, state}

        call.cancellation ->
          {:reply, {:ok, :already_requested}, state}

        true ->
          %{pid: pid, control: control} = state.workers[call.worker]
          send(pid, {Control, control, :cancel, reason})
          state = put_in(state.calls[token].cancellation, true)
          {:reply, {:ok, :requested}, event(state, {:foreign_cancellation_requested, token})}
      end
    else
      error -> {:reply, error, state}
    end
  end

  defp operation({:callback, description}, _, state) do
    with true <- state.allow_callbacks,
         true <- map_size(state.callbacks) < state.maximum,
         :ok <- Callback.verify(description, state.limits) do
      token = make_ref()
      callback = %{description: description, active: true, busy: false}
      state = put_in(state.callbacks[token], callback)
      {:reply, {:ok, {Callback, state.scope, token}}, event(state, {:callback_created, token})}
    else
      false -> {:reply, {:error, :callback_authority_or_limit}, state}
      error -> {:reply, error, state}
    end
  end

  defp operation({:revoke, {Callback, scope, token}}, _, %{scope: scope} = state) do
    case Map.get(state.callbacks, token) do
      %{active: true} ->
        state = put_in(state.callbacks[token].active, false)
        {:reply, :ok, event(state, {:callback_revoked, token})}

      _ ->
        {:reply, {:error, :invalid_foreign_callback_handle}, state}
    end
  end

  defp operation(:events, _, state), do: {:reply, {:ok, Enum.reverse(state.events)}, state}

  defp operation(_, _, state), do: {:reply, {:error, :invalid_foreign_operation}, state}

  defp granted(state, declaration) do
    if state.grants[Descriptor.id(declaration)] == declaration and
         Descriptor.verify(declaration) == :ok, do: :ok, else: {:error, :foreign_authority_denied}
  rescue
    _ -> {:error, :foreign_authority_denied}
  end

  defp arguments(state, schemas, values)
       when is_list(values) and length(schemas) == length(values) do
    # Callback handles carry authority and are checked separately. All data arguments
    # share a whole-vector preflight before individual schema conversion.
    data =
      Enum.zip(schemas, values)
      |> Enum.reject(fn {schema, _} ->
        match?({:callback, _, _}, schema) or match?({:native, _}, schema)
      end)

    with :ok <- Budget.check(List.to_tuple(Enum.map(data, &elem(&1, 1))), state.limits) do
      Enum.zip(schemas, values)
      |> Enum.reduce_while({:ok, []}, fn {schema, value}, {:ok, acc} ->
        case argument(state, schema, value) do
          {:ok, converted} -> {:cont, {:ok, [converted | acc]}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, reversed} ->
          native = Enum.reverse(reversed)

          data_native =
            Enum.zip(schemas, native)
            |> Enum.reject(fn {schema, _} ->
              match?({:callback, _, _}, schema) or match?({:native, _}, schema)
            end)

          with :ok <-
                 Budget.check(List.to_tuple(Enum.map(data_native, &elem(&1, 1))), state.limits),
               do: {:ok, native}

        error ->
          error
      end
    end
  end

  defp arguments(_, _, _), do: {:error, :foreign_argument_arity}

  defp argument(state, {:native, role}, handle) do
    with {:ok, %{role: ^role, value: value}} <- native(state, handle),
         do: {:ok, value},
         else: (_ -> {:error, :invalid_native_handle})
  end

  defp argument(state, {:callback, input, output}, {Callback, scope, token})
       when scope == state.scope do
    case Map.get(state.callbacks, token) do
      %{active: true, description: %{input: ^input, output: ^output}} ->
        manager = self()

        {:ok,
         fn native ->
           result =
             try do
               GenServer.call(manager, {:enter_callback, token, native}, :infinity)
             catch
               :exit, _ -> {:error, :expired_foreign_callback}
             end

           case result do
             {:completed, value} ->
               value

             {:trap, {:callback_boundary, {:execution_failure, :error, {:catena_trap, reason}}}} ->
               :erlang.error({:catena_trap, reason})

             other ->
               :erlang.error({:catena_trap, {:foreign_callback, other}})
           end
         end}

      _ ->
        {:error, :invalid_foreign_callback_handle}
    end
  end

  defp argument(_, {:callback, _, _}, _), do: {:error, :invalid_foreign_callback_handle}
  defp argument(state, codec, semantic), do: Codec.encode(codec, semantic, state.limits)

  defp native(state, %NativeValue{scope: scope, token: token, role: role})
       when scope == state.scope do
    case Map.get(state.native_values, token) do
      %{role: ^role} = entry -> {:ok, entry}
      _ -> {:error, :invalid_native_handle}
    end
  end

  defp native(_, _), do: {:error, :invalid_native_handle}

  defp call(state, {__MODULE__, scope, token}) when scope == state.scope do
    case Map.get(state.calls, token) do
      nil -> {:error, :invalid_foreign_call_handle}
      value -> {:ok, token, value}
    end
  end

  defp call(_, _), do: {:error, :invalid_foreign_call_handle}

  defp worker(state, kind, body) do
    server = self()
    control = make_ref()

    {pid, monitor} =
      :erlang.spawn_opt(
        fn ->
          if state.sensitive, do: Process.flag(:sensitive, true)

          outcome =
            try do
              body.({Control, self(), control})
            catch
              :throw, {Control, ^control, reason} ->
                {:cancelled, reason, :external_effects_possible}

              :error, {:catena_trap, reason} ->
                {:trap, reason}

              class, reason ->
                {:trap, {:foreign_failure, class, reason}}
            end

          outcome =
            case outcome do
              {:trap, reason} ->
                case Budget.check(reason, state.limits) do
                  :ok -> outcome
                  _ -> {:trap, :unrepresentable_foreign_failure}
                end

              {:cancelled, reason, _} ->
                case Budget.check(reason, state.limits) do
                  :ok -> outcome
                  _ -> {:trap, :unrepresentable_foreign_failure}
                end

              other ->
                other
            end

          send(server, {:worker_result, self(), control, outcome})
        end,
        [:link, :monitor]
      )

    worker = %{pid: pid, control: control, kind: kind, result: nil}
    {%{state | workers: Map.put(state.workers, monitor, worker), count: state.count + 1}, monitor}
  end

  @impl true
  def handle_info({:worker_result, pid, control, outcome}, state) do
    case Enum.find(state.workers, fn {_, worker} ->
           worker.pid == pid and worker.control == control
         end) do
      {monitor, worker} -> {:noreply, put_in(state.workers[monitor], %{worker | result: outcome})}
      nil -> {:noreply, state}
    end
  end

  def handle_info({:DOWN, monitor, :process, _, _}, %{owner_monitor: monitor} = state),
    do: {:stop, :normal, shutdown(state)}

  def handle_info({:DOWN, monitor, :process, _, reason}, state) do
    case Map.pop(state.workers, monitor) do
      {nil, _} ->
        {:noreply, state}

      {worker, workers} ->
        outcome = worker.result || {:trap, {:foreign_worker_exit, reason}}
        state = %{state | workers: workers}

        case worker.kind do
          {:call, token} ->
            state = put_in(state.calls[token].outcome, outcome)
            {:noreply, state |> event({:foreign_terminal, token, outcome}) |> wake(token)}

          {:callback, token, from} ->
            GenServer.reply(from, outcome)
            state = put_in(state.callbacks[token].busy, false)
            {:noreply, event(state, {:callback_terminal, token, outcome})}
        end
    end
  end

  def handle_info({:wait_expired, token}, %{waiter: {token, from, _}} = state) do
    GenServer.reply(from, {:ok, :pending})
    {:noreply, %{state | waiter: nil}}
  end

  def handle_info(_, state), do: {:noreply, state}

  defp wake(state, token) do
    case state.waiter do
      {^token, from, timer} ->
        Process.cancel_timer(timer)
        GenServer.reply(from, {:ok, state.calls[token].outcome})
        %{state | waiter: nil}

      _ ->
        state
    end
  end

  defp event(state, event), do: %{state | events: [event | state.events]}

  defp shutdown(state) do
    Enum.each(state.workers, fn {_, worker} ->
      Process.unlink(worker.pid)
      Process.exit(worker.pid, :kill)
    end)

    Enum.each(state.workers, fn {monitor, worker} ->
      receive do
        {:DOWN, ^monitor, :process, _, _} -> :ok
      end

      case worker.kind do
        {:callback, _, from} -> GenServer.reply(from, {:error, :expired_foreign_callback})
        _ -> :ok
      end
    end)

    %{state | workers: %{}}
  end
end
