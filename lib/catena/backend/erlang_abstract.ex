defmodule Catena.Backend.ErlangAbstract do
  @moduledoc "Lower verified Catena typed core to supported Erlang Abstract Format."

  @spec lower(map()) :: [term()]
  def lower(core) do
    annotation = annotation(core)
    module = safe_atom(core.module)
    globals = Map.new(core.definitions, &{&1.name, length(&1.parameters)})

    exports =
      core.definitions
      |> Enum.filter(&(&1.name in core.exports))
      |> Enum.map(fn definition ->
        {safe_atom(definition.name), length(definition.parameters)}
      end)

    functions = Enum.map(core.definitions, &lower_definition(&1, globals, annotation))

    [
      {:attribute, annotation, :file,
       {String.to_charlist(Map.get(core, :source, "<catena-json>")), 1}},
      {:attribute, annotation, :module, module},
      {:attribute, annotation, :export, exports}
      | functions
    ]
  end

  defp lower_definition(definition, globals, annotation) do
    {parameters, body} = unwrap_parameters(definition.expression, definition.parameters, [])
    environment = Map.new(parameters, fn name -> {name, variable_atom(name)} end)
    arguments = Enum.map(parameters, &{:var, annotation, variable_atom(&1)})
    expression = lower_expression(body, environment, globals, annotation)
    clause = {:clause, annotation, arguments, [], [expression]}
    {:function, annotation, safe_atom(definition.name), length(parameters), [clause]}
  end

  defp unwrap_parameters(expression, [], parameters), do: {Enum.reverse(parameters), expression}

  defp unwrap_parameters(
         %{tag: :function, parameter: parameter, body: body},
         [parameter | rest],
         parameters
       ),
       do: unwrap_parameters(body, rest, [parameter | parameters])

  defp unwrap_parameters(_expression, _parameters, _accumulator),
    do: raise(ArgumentError, "typed core does not match declared definition parameters")

  defp lower_expression(%{tag: :integer, value: value}, _environment, _globals, annotation),
    do: {:integer, annotation, value}

  defp lower_expression(%{tag: :boolean, value: value}, _environment, _globals, annotation),
    do: {:atom, annotation, value}

  defp lower_expression(%{tag: :variable, name: name}, environment, globals, annotation) do
    case Map.fetch(environment, name) do
      {:ok, variable} -> {:var, annotation, variable}
      :error -> curried_global(name, Map.fetch!(globals, name), annotation)
    end
  end

  defp lower_expression(
         %{tag: :function, parameter: parameter, body: body},
         environment,
         globals,
         annotation
       ) do
    variable = variable_atom(parameter)

    clause =
      {:clause, annotation, [{:var, annotation, variable}], [],
       [lower_expression(body, Map.put(environment, parameter, variable), globals, annotation)]}

    {:fun, annotation, {:clauses, [clause]}}
  end

  defp lower_expression(
         %{tag: :call, callee: %{tag: :variable, name: name}, arguments: arguments},
         environment,
         globals,
         annotation
       )
       when not is_map_key(environment, name) and
              is_map_key(globals, name) and
              :erlang.map_get(name, globals) == length(arguments) do
    {:call, annotation, {:atom, annotation, safe_atom(name)},
     Enum.map(arguments, &lower_expression(&1, environment, globals, annotation))}
  end

  defp lower_expression(
         %{tag: :call, callee: callee, arguments: arguments},
         environment,
         globals,
         annotation
       ) do
    Enum.reduce(
      arguments,
      lower_expression(callee, environment, globals, annotation),
      fn argument, current ->
        {:call, annotation, current,
         [lower_expression(argument, environment, globals, annotation)]}
      end
    )
  end

  defp lower_expression(
         %{tag: :let, name: name, value: value, body: body},
         environment,
         globals,
         annotation
       ) do
    variable = variable_atom(name)

    {:block, annotation,
     [
       {:match, annotation, {:var, annotation, variable},
        lower_expression(value, environment, globals, annotation)},
       lower_expression(body, Map.put(environment, name, variable), globals, annotation)
     ]}
  end

  defp lower_expression(%{tag: :tuple, elements: elements}, environment, globals, annotation),
    do:
      {:tuple, annotation,
       Enum.map(elements, &lower_expression(&1, environment, globals, annotation))}

  defp lower_expression(
         %{tag: :annotate, expression: expression},
         environment,
         globals,
         annotation
       ),
       do: lower_expression(expression, environment, globals, annotation)

  defp curried_global(name, 0, annotation),
    do: {:fun, annotation, {:function, safe_atom(name), 0}}

  defp curried_global(name, arity, annotation) do
    variables = Enum.map(1..arity, &String.to_atom("Curry#{&1}"))

    body =
      {:call, annotation, {:atom, annotation, safe_atom(name)},
       Enum.map(variables, &{:var, annotation, &1})}

    Enum.reduce(Enum.reverse(variables), body, fn variable, inner ->
      clause = {:clause, annotation, [{:var, annotation, variable}], [], [inner]}
      {:fun, annotation, {:clauses, [clause]}}
    end)
  end

  defp annotation(core) do
    core
    |> Map.get(:source, "<catena-json>")
    |> String.to_charlist()
    |> :erl_anno.set_file(:erl_anno.new(1))
  end

  defp variable_atom(name), do: String.to_atom("V_" <> name)
  defp safe_atom(name), do: String.to_atom(name)
end
