defmodule Catena.TypedCore do
  @moduledoc "Substitution and normalization helpers for elaborated core nodes."

  alias Catena.Type
  alias Catena.Type.Scheme

  @spec apply_substitution(term(), map()) :: term()
  def apply_substitution(%Scheme{} = scheme, substitution) do
    blocked = Map.drop(substitution, scheme.variables)
    %{scheme | type: Type.apply(scheme.type, blocked)}
  end

  def apply_substitution(%{} = value, substitution) do
    value
    |> Enum.map(fn
      {:type, type} -> {:type, Type.apply(type, substitution)}
      {key, item} -> {key, apply_substitution(item, substitution)}
    end)
    |> Map.new()
  end

  def apply_substitution(value, substitution) when is_list(value),
    do: Enum.map(value, &apply_substitution(&1, substitution))

  def apply_substitution(value, _substitution), do: value
end
