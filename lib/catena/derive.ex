defmodule Catena.Derive do
  @moduledoc "Compiler-generated C002 datatype operations."

  alias Catena.Type.Scheme

  @spec folds(map()) :: [map()]
  def folds(data) do
    data.types
    |> Enum.filter(&("fold" in &1.derivations and &1.visibility == :transparent))
    |> Enum.map(&fold_definition/1)
  end

  @spec capabilities(map(), [map()]) :: [map()]
  def capabilities(data, plans) do
    Enum.flat_map(plans, fn plan ->
      type = Map.fetch!(data.types_by_id, plan.type_id)

      if plan.capability == "CollectingMapper" do
        collect_helpers(type, plan)
      else
        target_indexes = Enum.map(plan.targets, &parameter_index!(type, &1))
        {parameters, operation_type} = capability_type(plan.capability, type, target_indexes)

        [
          %{
            name: plan.function,
            parameters: parameters,
            expression: %{
              tag: :derived_capability,
              capability: plan.capability,
              datatype: type,
              target_indexes: target_indexes,
              type: operation_type,
              provenance: :compiler_derived,
              path: plan.path
            },
            scheme: %Scheme{variables: variables_in(operation_type), type: operation_type},
            kind: :value,
            condition: nil,
            clause_definition?: false,
            generated?: true,
            provenance: :compiler_derived,
            path: plan.path
          }
        ]
      end
    end)
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

  defp capability_type(capability, type, _target_indexes)
       when capability in ~w(Equatable Orderable) do
    datatype = datatype(type, variable_ids(type.arity))
    result = if capability == "Equatable", do: :boolean, else: :integer
    {~w(left right), function_type([datatype, datatype], result)}
  end

  defp capability_type("Mapper", type, [target]) do
    source = variable_ids(type.arity)
    destination = type.arity
    output = List.replace_at(source, target, destination)
    callback = {:function, {:var, target}, {:var, destination}}

    {~w(callback subject),
     function_type([callback, datatype(type, source)], datatype(type, output))}
  end

  defp capability_type("TwoSlotMapper", type, [first, second]) do
    source = variable_ids(type.arity)
    first_destination = type.arity
    second_destination = type.arity + 1

    output =
      source
      |> List.replace_at(first, first_destination)
      |> List.replace_at(second, second_destination)

    callbacks = [
      {:function, {:var, first}, {:var, first_destination}},
      {:function, {:var, second}, {:var, second_destination}}
    ]

    {~w(first_callback second_callback subject),
     function_type(callbacks ++ [datatype(type, source)], datatype(type, output))}
  end

  defp capability_type("Reducible", type, [target]) do
    variables = variable_ids(type.arity)
    accumulator = type.arity
    callback = {:function, {:var, accumulator}, {:function, {:var, target}, {:var, accumulator}}}

    {~w(callback initial subject),
     function_type(
       [callback, {:var, accumulator}, datatype(type, variables)],
       {:var, accumulator}
     )}
  end

  defp datatype(type, variables),
    do: {:nominal, type.id, Enum.map(variables, &{:var, &1})}

  defp function_type(parameters, result),
    do: Enum.reduce(Enum.reverse(parameters), result, &{:function, &1, &2})

  defp parameter_index!(type, name) do
    case Enum.find_index(type.parameters, &(&1.name == name)) do
      nil -> raise ArgumentError, "unknown derived parameter #{name}"
      index -> index
    end
  end

  defp variables_in(type),
    do: type |> variables_in(MapSet.new()) |> MapSet.to_list() |> Enum.sort()

  defp variables_in({:var, id}, variables), do: MapSet.put(variables, id)

  defp variables_in({:function, parameter, result}, variables),
    do: result |> variables_in(variables_in(parameter, variables))

  defp variables_in({:nominal, _id, arguments}, variables),
    do: Enum.reduce(arguments, variables, &variables_in/2)

  defp variables_in(_type, variables), do: variables

  defp collect_helpers(type, plan) do
    result_variable = {:var, type.arity}

    handler_types =
      Enum.map(type.constructors, fn constructor ->
        Enum.reduce(Enum.reverse(constructor.fields), result_variable, fn field, result ->
          {:function, field.type, result}
        end)
      end)

    datatype = datatype(type, variable_ids(type.arity))
    eliminator_type = function_type(handler_types ++ [datatype], result_variable)
    handler_names = Enum.map(type.constructors, &"handler_#{&1.index}")

    eliminator = %{
      name: "#{type.name}.__eliminate",
      parameters: handler_names ++ ["value"],
      expression: %{
        tag: :derived_eliminator,
        capability: "CollectingMapper",
        datatype: type,
        handler_names: handler_names,
        value_name: "value",
        type: eliminator_type,
        provenance: :compiler_derived,
        path: plan.path
      },
      scheme: %Scheme{variables: variable_ids(type.arity + 1), type: eliminator_type},
      kind: :value,
      condition: nil,
      clause_definition?: false,
      generated?: true,
      linker_only?: true,
      provenance: :compiler_derived,
      path: plan.path
    }

    constructors =
      Enum.map(type.constructors, fn constructor ->
        constructor_type =
          function_type(Enum.map(constructor.fields, & &1.type), constructor.result)

        %{
          name: "#{type.name}.__construct.#{constructor.index}",
          parameters: Enum.map(constructor.fields, &"field#{&1.index}"),
          expression: %{
            tag: :derived_constructor,
            capability: "CollectingMapper",
            datatype: type,
            constructor: constructor,
            type: constructor_type,
            provenance: :compiler_derived,
            path: plan.path
          },
          scheme: %Scheme{variables: constructor.variables, type: constructor_type},
          kind: :value,
          condition: nil,
          clause_definition?: false,
          generated?: true,
          linker_only?: true,
          provenance: :compiler_derived,
          path: plan.path
        }
      end)

    [eliminator | constructors]
  end
end
