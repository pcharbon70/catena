defmodule Catena.Type do
  @moduledoc "Type representation and scheme operations for the principal core."

  import Kernel, except: [apply: 2]

  alias Catena.Type.Scheme

  @type variable :: {:var, non_neg_integer()}
  @type t ::
          :integer
          | :boolean
          | variable()
          | {:skolem, term()}
          | {:function, t(), t()}
          | {:tuple, [t()]}

  @spec free(t()) :: MapSet.t(non_neg_integer())
  def free({:var, id}), do: MapSet.new([id])
  def free({:function, parameter, result}), do: MapSet.union(free(parameter), free(result))

  def free({:tuple, elements}),
    do: Enum.reduce(elements, MapSet.new(), &MapSet.union(free(&1), &2))

  def free(type) when type in [:integer, :boolean], do: MapSet.new()
  def free({:skolem, _name}), do: MapSet.new()

  @spec apply(t(), map()) :: t()
  def apply({:var, id} = variable, substitution) do
    case Map.fetch(substitution, id) do
      {:ok, type} -> apply(type, substitution)
      :error -> variable
    end
  end

  def apply({:function, parameter, result}, substitution),
    do: {:function, apply(parameter, substitution), apply(result, substitution)}

  def apply({:tuple, elements}, substitution),
    do: {:tuple, Enum.map(elements, &apply(&1, substitution))}

  def apply(type, _substitution), do: type

  @spec free_environment(map(), map()) :: MapSet.t(non_neg_integer())
  def free_environment(environment, substitution) do
    Enum.reduce(environment, MapSet.new(), fn {_name, %Scheme{} = scheme}, acc ->
      MapSet.union(acc, Scheme.free(scheme, substitution))
    end)
  end

  @spec generalize(map(), t(), map()) :: Scheme.t()
  def generalize(environment, type, substitution) do
    type = apply(type, substitution)
    variables = MapSet.difference(free(type), free_environment(environment, substitution))
    %Scheme{variables: variables |> MapSet.to_list() |> Enum.sort(), type: type}
  end

  @spec normalize(t()) :: term()
  def normalize(type) do
    {normalized, _names} = normalize(type, %{})
    normalized
  end

  defp normalize({:var, id}, names) do
    case Map.fetch(names, id) do
      {:ok, name} ->
        {{:variable, name}, names}

      :error ->
        name = variable_name(map_size(names))
        {{:variable, name}, Map.put(names, id, name)}
    end
  end

  defp normalize({:function, parameter, result}, names) do
    {parameter, names} = normalize(parameter, names)
    {result, names} = normalize(result, names)
    {{:function, parameter, result}, names}
  end

  defp normalize({:tuple, elements}, names) do
    {elements, names} = Enum.map_reduce(elements, names, &normalize/2)
    {{:tuple, elements}, names}
  end

  defp normalize(type, names), do: {type, names}

  defp variable_name(index) when index < 26, do: <<?a + index>>
  defp variable_name(index), do: "t#{index}"
end
