defmodule Catena.ValueBoundary.Fixed do
  @moduledoc "Normalizes verified fixed-kernel nominal metadata for the shared nominal codec."

  def describe(core, name) do
    with :ok <- Catena.Kernel.Verifier.verify(core),
         true <- name in core.exports.values,
         %{signature: type} <- Enum.find(core.definitions, &(&1.name == name)),
         true <- Catena.Kernel.Type.closed?(type) do
      types =
        Map.new(core.data.types, fn {name, declaration} ->
          id = identity(core, name)

          constructors =
            Enum.with_index(declaration.constructors, fn constructor, index ->
              %{
                id: id <> "::" <> constructor.name,
                type_id: id,
                name: constructor.name,
                index: index,
                visibility: if(name in core.exports.types, do: :transparent, else: :private),
                gadt?: false,
                existential_ids: MapSet.new(),
                universal_count: length(declaration.parameters),
                variables: declaration.parameters,
                fields: Enum.map(constructor.fields, &%{type: normalize(&1, core)})
              }
            end)

          {id, %{constructors: constructors}}
        end)

      {:ok,
       %{
         core: core,
         export: name,
         type: normalize(type, core),
         layout: :fixed,
         data: %{types_by_id: types}
       }}
    else
      _ -> {:error, :invalid_boundary_description}
    end
  rescue
    _ -> {:error, :invalid_boundary_description}
  end

  defp normalize({:variable, name}, _), do: {:var, name}

  defp normalize({:nominal, name, arguments}, core),
    do: {:nominal, identity(core, name), Enum.map(arguments, &normalize(&1, core))}

  defp normalize({:tuple, types}, core), do: {:tuple, Enum.map(types, &normalize(&1, core))}

  defp normalize({tag, %{fields: fields, tail: nil}}, core) when tag in [:record, :variant],
    do: {tag, Map.new(fields, fn {label, type} -> {label, normalize(type, core)} end)}

  defp normalize(type, _), do: type
  defp identity(core, name), do: core.origin <> "::" <> core.module <> "::" <> name
end
