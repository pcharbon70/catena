defmodule Catena.Derive do
  @moduledoc "Compiler-generated C002 datatype operations."

  alias Catena.Type.Scheme

  @spec folds(map()) :: [map()]
  def folds(data) do
    data.types
    |> Enum.filter(&("fold" in &1.derivations and &1.visibility == :transparent))
    |> Enum.map(&fold_definition/1)
  end

  defp fold_definition(type) do
    result_variable = {:var, type.arity}

    handler_types =
      Enum.map(type.constructors, fn constructor ->
        Enum.reduce(Enum.reverse(constructor.fields), result_variable, fn field, result ->
          {:function, field.type, result}
        end)
      end)

    datatype = {:nominal, type.id, Enum.map(variable_ids(type.arity), &{:var, &1})}

    fold_type =
      Enum.reduce(
        Enum.reverse(handler_types ++ [datatype]),
        result_variable,
        &{:function, &1, &2}
      )

    handler_names = Enum.map(type.constructors, &"handler_#{&1.index}")
    parameters = handler_names ++ ["value"]

    %{
      name: "#{type.name}.fold",
      parameters: parameters,
      expression: %{
        tag: :derived_fold,
        datatype: type,
        handler_names: handler_names,
        value_name: "value",
        type: fold_type,
        provenance: :compiler_derived,
        path: type.path
      },
      scheme: %Scheme{variables: variable_ids(type.arity + 1), type: fold_type},
      generated?: true,
      provenance: :compiler_derived,
      path: type.path
    }
  end

  defp variable_ids(0), do: []
  defp variable_ids(count), do: Enum.to_list(0..(count - 1))
end
