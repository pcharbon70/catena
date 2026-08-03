defmodule Catena.Effect.Runtime do
  @moduledoc false

  @trace_key :__catena_c005_effect_trace__

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
    case Process.get(@trace_key) do
      events when is_list(events) -> Process.put(@trace_key, [event | events])
      _other -> :ok
    end

    :ok
  end
end
