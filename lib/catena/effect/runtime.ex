defmodule Catena.Effect.Runtime do
  @moduledoc false

  @trace_key :__catena_c005_effect_trace__

  @doc "Foreign work retains its checked effect request and invokes a continuation only on typed success."
  def foreign_request(scope, declaration, arguments, continuation, limits, wait_ms) do
    unless is_list(arguments) and length(arguments) == length(declaration.arguments) and
             Catena.Foreign.Budget.check(List.to_tuple(arguments), limits) == :ok do
      :erlang.error({:catena_trap, :foreign_argument_vector_boundary})
    end

    trace({:foreign_request, declaration.effect, declaration.identity})

    semantic =
      Enum.zip(declaration.arguments, arguments)
      |> Enum.map(fn {codec, native} ->
        case Catena.Foreign.Codec.from_native(codec, native, limits) do
          {:ok, value} -> value
          {:error, reason} -> :erlang.error({:catena_trap, {:foreign_argument_boundary, reason}})
        end
      end)

    result = Catena.Foreign.Adapter.call(scope, declaration, semantic, wait_ms)
    trace({:foreign_outcome, declaration.effect, result})

    case result do
      {:ok, {:completed, value}} ->
        case Catena.Foreign.Codec.to_native(declaration.result, value, limits) do
          {:ok, native} -> continuation.(native)
          {:error, reason} -> :erlang.error({:catena_trap, {:foreign_result_boundary, reason}})
        end

      {:ok, {:trap, reason}} ->
        :erlang.error({:catena_trap, reason})

      {:ok, {:cancelled, reason, _}} ->
        throw({:catena_resource_cancelled, reason})

      {:ok, :pending} ->
        :erlang.error({:catena_trap, :foreign_wait_expired_effects_possible})

      {:error, reason} ->
        :erlang.error({:catena_trap, reason})
    end
  end

  @spec request(map(), String.t(), String.t(), atom(), [term()], (term() -> term())) :: term()
  def request(handlers, capability, family, operation, arguments, continuation) do
    trace({:request, family, capability, operation})

    case Map.fetch(handlers, capability) do
      {:ok, handler} -> handler.(operation, arguments, continuation)
      :error -> raise "unhandled Catena request #{family}.#{operation} through #{capability}"
    end
  end

  @spec new_resumption((term() -> term())) :: {reference(), (term() -> term())}
  def new_resumption(continuation), do: {:atomics.new(1, []), continuation}

  @spec resume({reference(), (term() -> term())}, term()) :: term()
  def resume({token, continuation}, value) do
    case :atomics.compare_exchange(token, 1, 0, 1) do
      :ok ->
        trace(:resume)
        continuation.(value)

      _actual ->
        raise "Catena affine resumption was resumed more than once"
    end
  end

  @spec capture_trace((-> term())) :: {term(), [term()]}
  def capture_trace(function) when is_function(function, 0) do
    previous = Process.get(@trace_key, :__catena_trace_absent__)
    Process.put(@trace_key, [])

    try do
      result = function.()
      {result, Process.get(@trace_key) |> Enum.reverse()}
    after
      case previous do
        :__catena_trace_absent__ -> Process.delete(@trace_key)
        value -> Process.put(@trace_key, value)
      end
    end
  end

  @spec trace(term()) :: :ok
  def trace(event) do
    event =
      if Catena.Runtime.Secret.context?(),
        do: :secret_activity,
        else: Catena.Runtime.Secret.redact(event)

    case Process.get(@trace_key) do
      events when is_list(events) -> Process.put(@trace_key, [event | events])
      _other -> :ok
    end

    :ok
  end
end
