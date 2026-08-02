defmodule Catena.Type.Scheme do
  @moduledoc "A rank-1 universally quantified type scheme."

  alias Catena.Type

  @enforce_keys [:variables, :type]
  defstruct [:variables, :type]

  @type t :: %__MODULE__{variables: [non_neg_integer()], type: Type.t()}

  @spec mono(Type.t()) :: t()
  def mono(type), do: %__MODULE__{variables: [], type: type}

  @spec free(t(), map()) :: MapSet.t(non_neg_integer())
  def free(%__MODULE__{variables: variables, type: type}, substitution) do
    type
    |> Type.apply(Map.drop(substitution, variables))
    |> Type.free()
    |> MapSet.difference(MapSet.new(variables))
  end
end
