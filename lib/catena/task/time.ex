defmodule Catena.Task.Time do
  @moduledoc "Experimental monotonic deadline conversion for checked managed receives."
  @max_wait 4_294_967_295
  def deadline(duration) when is_integer(duration) and duration >= 0 do
    case Catena.Task.Managed.context() do
      {_, _} -> System.monotonic_time(:nanosecond) + duration
      _ -> :erlang.error({:catena_trap, :invalid_managed_context})
    end
  end

  def deadline(_), do: :erlang.error({:catena_trap, :invalid_duration})

  def remaining_ms(deadline),
    do:
      min(
        @max_wait,
        div(max(0, deadline - System.monotonic_time(:nanosecond)) + 999_999, 1_000_000)
      )

  def expired?(deadline), do: System.monotonic_time(:nanosecond) >= deadline
end
