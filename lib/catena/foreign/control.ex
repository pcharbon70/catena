defmodule Catena.Foreign.Control do
  @moduledoc "Cooperative worker cancellation; observing it does not imply rollback."
  def checkpoint({__MODULE__, owner, token}) when owner == self() do
    receive do
      {__MODULE__, ^token, :cancel, reason} -> throw({__MODULE__, token, reason})
    after
      0 -> :ok
    end
  end

  def checkpoint(_), do: :erlang.error({:catena_trap, :invalid_foreign_control_owner})
end
