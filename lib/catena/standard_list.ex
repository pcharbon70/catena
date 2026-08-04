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
end
