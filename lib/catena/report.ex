defmodule Catena.Report do
  @moduledoc false

  alias Catena.Type
  alias Catena.Type.Scheme

  def module(core) do
    %{
      version: core.version,
      module: core.module,
      profile: Atom.to_string(core.profile),
      types:
        Enum.map(Map.get(core, :data, %{types: []}).types, fn type ->
          %{
            id: type.id,
            name: type.name,
            visibility: Atom.to_string(type.visibility),
            inhabitation: Atom.to_string(type.inhabitation),
            variance: Enum.map(type.variance, &Atom.to_string/1),
            positive: type.positive?,
            regular: type.regular?
          }
        end),
      definitions:
        Enum.map(core.definitions, fn definition ->
          %{name: definition.name, scheme: scheme(definition.scheme)}
        end)
    }
  end

  def diagnostic(diagnostic) do
    %{
      id: diagnostic.id,
      message: diagnostic.message,
      path: diagnostic.path,
      details: diagnostic.details
    }
  end

  defp scheme(%Scheme{variables: variables, type: type}) do
    %{quantified: length(variables), type: printable(Type.normalize(type))}
  end

  defp printable(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.map(&printable/1)

  defp printable(value) when is_list(value), do: Enum.map(value, &printable/1)
  defp printable(value) when is_atom(value), do: Atom.to_string(value)
  defp printable(value), do: value
end
