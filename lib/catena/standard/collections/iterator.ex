defmodule Catena.Standard.Collections.Iterator do
  @moduledoc "Owned pull protocol over explicit typed foreign grants; internal role API."
  alias Catena.Foreign.{Adapter, Codec, Descriptor}
  alias Catena.Standard.Outcomes, as: Outcome

  def describe(pull, release, state_schema, item_schema) do
    with :ok <- Descriptor.verify(pull),
         :ok <- Descriptor.verify(release),
         true <- pull.version == "0.1.61" and release.version == "0.1.61",
         {:ok, state} <- Codec.new({:data, state_schema}),
         {:ok, item} <- Codec.new({:data, item_schema}),
         {:ok, step} <-
           Codec.new(
             {:data,
              {:variant,
               %{"end" => state_schema, "yield" => {:tuple, [item_schema, state_schema]}}}}
           ),
         {:ok, unit} <- Codec.new({:data, :unit}),
         true <- pull.arguments == [state] and pull.result == step,
         true <- release.arguments == [state] and release.result == unit do
      {:ok, %{version: "0.1.65", pull: pull, release: release, state: state, item: item}}
    else
      _ -> {:error, :invalid_pull_description}
    end
  end

  def verify(description) do
    with {:data, state} <- description.state.schema,
         {:data, item} <- description.item.schema,
         {:ok, ^description} <- describe(description.pull, description.release, state, item) do
      :ok
    else
      _ -> {:error, :invalid_pull_description}
    end
  rescue
    _ -> {:error, :invalid_pull_description}
  end

  def run(description, initial, grants, limits, body, options \\ []) when is_function(body, 1) do
    with :ok <- verify(description),
         {:ok, _} <- Codec.from_native(description.state, initial, limits),
         true <- is_list(grants) and description.pull in grants and description.release in grants,
         {:ok, timeout, max_steps} <- options(options) do
      owner = self()
      token = make_ref()
      release_token = make_ref()
      ready = make_ref()

      {worker, monitor} =
        spawn_monitor(fn ->
          owner_monitor = Process.monitor(owner)

          result =
            Adapter.run(
              grants,
              limits,
              fn scope ->
                Process.put({__MODULE__, :started}, true)
                send(owner, {ready, :ok})

                loop(%{
                  owner: owner,
                  owner_monitor: owner_monitor,
                  token: token,
                  release_token: release_token,
                  scope: scope,
                  description: description,
                  state: initial,
                  phase: :open,
                  release_result: nil,
                  count: 0,
                  max_steps: max_steps,
                  timeout: timeout,
                  limits: limits
                })
              end,
              max_operations: max_steps + 1
            )

          case result do
            {:finish, reply, result} ->
              send(reply, {reply, result})

            _ ->
              if not Process.get({__MODULE__, :started}, false), do: send(owner, {ready, result})
          end
        end)

      handle = {__MODULE__, owner, worker, token, timeout + 100}

      try do
        receive do
          {^ready, :ok} ->
            Catena.Resource.Runtime.run(
              {worker, release_token},
              fn {pid, key} ->
                case request(pid, {:release, key}, timeout * 2 + 100) do
                  :ok -> :ok
                  error -> :erlang.error({:catena_trap, {:iterator_cleanup_failed, error}})
                end
              end,
              fn _, _ -> body.(handle) end,
              (timeout * 2 + 200) * 1_000_000
            )

          {^ready, error} ->
            error

          {:DOWN, ^monitor, :process, ^worker, reason} ->
            {:error, {:iterator_start_failed, reason}}
        after
          timeout + 100 -> {:error, :iterator_start_timeout}
        end
      after
        Process.exit(worker, :kill)
        Process.demonitor(monitor, [:flush])
      end
    else
      false -> {:error, :pull_authority_denied}
      error -> error
    end
  end

  def next(handle), do: public(handle, :next)
  def close(handle), do: public(handle, :close)

  def collect(handle, max_items) when is_integer(max_items) and max_items in 0..1_000_000,
    do:
      with(
        {:ok, _, limits} <- public(handle, :metadata),
        {:ok, values} <- collect(handle, max_items, []),
        :ok <- Catena.Foreign.Budget.check(values, limits),
        do: {:ok, values}
      )

  def collect(_, _), do: {:error, :invalid_collection_limit}

  defp collect(handle, 0, reversed) do
    with :ok <- close(handle), do: {:ok, Enum.reverse(reversed)}
  end

  defp collect(handle, remaining, reversed) do
    case next(handle) do
      {:ok, {:catena_adt, _, 0, {}}} -> {:ok, Enum.reverse(reversed)}
      {:ok, {:catena_adt, _, 1, {value}}} -> collect(handle, remaining - 1, [value | reversed])
      error -> error
    end
  end

  def fold_while(handle, callback, initial, schema) do
    with {:ok, %{schema: {:data, item}}, limits} <- public(handle, :metadata),
         {:ok, accumulator} <- Codec.new({:data, schema}),
         {:ok, _} <- Codec.from_native(accumulator, initial, limits),
         {:ok, input} <- Codec.new({:data, {:tuple, [schema, item]}}),
         {:ok, output} <-
           Codec.new({:data, {:variant, %{"continue" => schema, "stop" => schema}}}) do
      Catena.Standard.Collections.Callback.run(callback, input, output, limits, fn step ->
        reduce(handle, step, initial)
      end)
    end
  end

  defp reduce(handle, step, accumulator) do
    case next(handle) do
      {:ok, {:catena_adt, _, 0, {}}} ->
        {:ok, accumulator}

      {:ok, {:catena_adt, _, 1, {value}}} ->
        case step.({accumulator, value}) do
          {:catena_variant, :continue, next} -> reduce(handle, step, next)
          {:catena_variant, :stop, next} -> with :ok <- close(handle), do: {:ok, next}
        end

      error ->
        error
    end
  end

  defp public({__MODULE__, owner, worker, token, timeout}, operation) when owner == self(),
    do: request(worker, {token, operation}, timeout)

  defp public(_, _), do: {:error, :invalid_iterator_owner}

  defp request(worker, operation, timeout) do
    # Alias replies are dropped after timeout; a late private reply cannot leak
    # into the caller's mailbox or be mistaken for another operation.
    alias_ref = :erlang.alias()
    monitor = Process.monitor(worker)
    send(worker, {:request, self(), alias_ref, operation})

    try do
      receive do
        {^alias_ref, result} -> result
        {:DOWN, ^monitor, :process, ^worker, _} -> {:error, :expired_iterator}
      after
        timeout -> {:error, :iterator_wait_exhausted}
      end
    after
      :erlang.unalias(alias_ref)
      Process.demonitor(monitor, [:flush])
    end
  end

  defp loop(state) do
    receive do
      {:request, sender, reply, {token, operation}}
      when sender == state.owner and token == state.token and
             operation in [:next, :close, :metadata] ->
        {result, state} =
          case operation do
            :next -> step(state)
            :close -> release(state, :closed)
            :metadata -> {{:ok, state.description.item, state.limits}, state}
          end

        send(reply, {reply, result})
        loop(state)

      {:request, _, reply, {:release, token}} when token == state.release_token ->
        {result, _} = release(state, :closed)
        {:finish, reply, result}

      {:DOWN, monitor, :process, owner, _}
      when monitor == state.owner_monitor and owner == state.owner ->
        release(state, :closed)

      _ ->
        loop(state)
    end
  end

  defp step(%{phase: :ended} = state), do: {absent(state), state}

  defp step(%{phase: phase} = state) when phase != :open,
    do: {{:error, :closed_iterator}, state}

  defp step(%{count: count, max_steps: max} = state) when count >= max,
    do: {{:error, :pull_step_limit}, %{state | phase: :failed}}

  defp step(state) do
    state = %{state | count: state.count + 1}

    case invoke(state, state.description.pull) do
      {:ok, {:catena_variant, :yield, {value, next}}} ->
        result = Outcome.present(value)

        case Catena.Foreign.Budget.check(result, state.limits) do
          :ok -> {{:ok, result}, %{state | state: next}}
          error -> {error, %{state | state: next, phase: :failed}}
        end

      {:ok, {:catena_variant, :end, next}} ->
        {result, state} = release(%{state | state: next}, :ended)
        {if(result == :ok, do: absent(state), else: result), state}

      error ->
        {error, %{state | phase: :failed}}
    end
  end

  defp absent(state) do
    result = Outcome.absent()
    with :ok <- Catena.Foreign.Budget.check(result, state.limits), do: {:ok, result}
  end

  defp release(%{release_result: result} = state, _) when not is_nil(result), do: {result, state}

  defp release(state, phase) do
    result =
      case invoke(state, state.description.release) do
        {:ok, :unit} -> :ok
        error -> error
      end

    {result, %{state | phase: phase, release_result: result}}
  end

  defp invoke(state, declaration) do
    with {:ok, call} <- Adapter.start(state.scope, declaration, [state.state]) do
      case Adapter.await(state.scope, call, state.timeout) do
        {:ok, {:completed, semantic}} ->
          Codec.to_native(declaration.result, semantic, state.limits)

        {:ok, :pending} ->
          Adapter.cancel(state.scope, call, :pull_deadline)
          {:error, :pull_deadline}

        {:ok, outcome} ->
          {:error, {:pull_outcome, outcome}}

        error ->
          error
      end
    end
  end

  defp options(options) do
    timeout = Keyword.get(options, :timeout_ms, 1000)
    max_steps = Keyword.get(options, :max_steps, 1024)

    if Keyword.keyword?(options) and
         length(Keyword.keys(options)) == length(Enum.uniq(Keyword.keys(options))) and
         Enum.all?(Keyword.keys(options), &(&1 in [:timeout_ms, :max_steps])) and
         is_integer(timeout) and timeout in 1..1_000_000 and
         is_integer(max_steps) and max_steps in 1..1_000_000,
       do: {:ok, timeout, max_steps},
       else: {:error, :invalid_pull_options}
  rescue
    _ -> {:error, :invalid_pull_options}
  end
end
