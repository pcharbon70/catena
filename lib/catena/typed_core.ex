defmodule Catena.TypedCore do
  @moduledoc "Substitution and normalization helpers for elaborated core nodes."

  alias Catena.Type
  alias Catena.Effect.Row
  alias Catena.Type.Scheme

  @spec apply_substitution(term(), map()) :: term()
  def apply_substitution(%Scheme{} = scheme, substitution) do
    blocked = Map.drop(substitution, scheme.variables)
    %{scheme | type: Type.apply(scheme.type, blocked)}
  end

  def apply_substitution(%Row{} = row, substitution) do
    entries =
      Enum.map(row.entries, fn entry ->
        %{entry | arguments: Enum.map(entry.arguments, &Type.apply(&1, substitution))}
      end)

    Row.new(entries, row.tail)
  end

  def apply_substitution(
        %{family: _family, capability: _capability, arguments: arguments} = value,
        substitution
      )
      when is_list(arguments) do
    value
    |> Map.put(:arguments, Enum.map(arguments, &Type.apply(&1, substitution)))
    |> apply_map_substitution(substitution)
  end

  def apply_substitution(%{parameters: parameters, result: result} = value, substitution)
      when is_list(parameters) do
    value
    |> Map.put(:parameters, Enum.map(parameters, &Type.apply(&1, substitution)))
    |> Map.put(:result, Type.apply(result, substitution))
    |> apply_map_substitution(substitution)
  end

  def apply_substitution(%{} = value, substitution) do
    apply_map_substitution(value, substitution)
  end

  def apply_substitution(value, substitution) when is_list(value),
    do: Enum.map(value, &apply_substitution(&1, substitution))

  def apply_substitution(value, _substitution), do: value

  defp apply_map_substitution(value, substitution) do
    value
    |> Enum.map(fn
      {:type, type} -> {:type, Type.apply(type, substitution)}
      {:result, type} -> {:result, Type.apply(type, substitution)}
      {key, item} -> {key, apply_substitution(item, substitution)}
    end)
    |> Map.new()
  end
end
