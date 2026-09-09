defmodule Catena.Standard.List do
  @moduledoc "Stack-safe standard List operations used by Catena 0.1.4 trait evidence."

  @spec map((term() -> term()), list()) :: list()
  def map(callback, subject) when is_function(callback, 1) and is_list(subject) do
    subject
    |> Enum.reduce([], fn item, reversed -> [callback.(item) | reversed] end)
    |> :lists.reverse()
  end

  @spec summarize((term() -> (term() -> term())), term(), list()) :: term()
  def summarize(callback, initial, subject)
      when is_function(callback, 1) and is_list(subject) do
    Enum.reduce(subject, initial, fn item, accumulator ->
      callback.(accumulator).(item)
    end)
  end

  @doc "Internal pure fold protocol with no callbacks after an explicit stop."
  def fold_while(callback, initial, subject) when is_function(callback, 2) and is_list(subject) do
    Enum.reduce_while(subject, initial, fn value, acc ->
      case callback.(acc, value) do
        {:continue, next} -> {:cont, next}
        {:stop, next} -> {:halt, next}
        _ -> :erlang.error({:catena_trap, :invalid_fold_step})
      end
    end)
  end
end
