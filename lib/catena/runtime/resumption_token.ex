defmodule Catena.Runtime.ResumptionToken do
  @moduledoc "Runtime backstop for the statically affine resumption invariant."

  defstruct [:cell]

  @type t :: %__MODULE__{cell: reference()}

  @spec new() :: t()
  def new do
    cell = :atomics.new(1, signed: false)
    :ok = :atomics.put(cell, 1, 0)
    %__MODULE__{cell: cell}
  end

  @spec consume!(t()) :: :ok
  def consume!(%__MODULE__{cell: cell}) do
    case :atomics.compare_exchange(cell, 1, 0, 1) do
      :ok -> :ok
      0 -> :ok
      1 -> raise ArgumentError, "resumption has already been consumed"
    end
  end

  @spec consumed?(t()) :: boolean()
  def consumed?(%__MODULE__{cell: cell}), do: :atomics.get(cell, 1) == 1
end
