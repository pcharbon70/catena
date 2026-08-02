defmodule Catena.Backend.ErlangAbstract do
  @moduledoc "Lower verified Catena typed core to Erlang/OTP 29 Abstract Format."

  @spec lower(map(), keyword()) :: [term()]
  def lower(core, options \\ []) do
    annotation = annotation(core)
    module = safe_atom(core.module)
    layout = Keyword.get(options, :layout, :compact)
    globals = Map.new(core.definitions, &{&1.name, length(&1.parameters)})

    exports =
      core.definitions
      |> Enum.filter(&(&1.name in core.exports))
      |> Enum.map(fn definition ->
        {safe_atom(definition.name), length(definition.parameters)}
      end)

    functions = Enum.map(core.definitions, &lower_definition(&1, globals, annotation, layout))

    [
      {:attribute, annotation, :file,
       {String.to_charlist(Map.get(core, :source, "<catena-json>")), 1}},
      {:attribute, annotation, :module, module},
      {:attribute, annotation, :export, exports}
      | functions
    ]
  end

  defp lower_definition(
         %{expression: %{tag: :derived_fold} = fold} = definition,
         _globals,
         annotation,
         layout
       ) do
    arguments = Enum.map(definition.parameters, &{:var, annotation, variable_atom(&1)})
    value = {:var, annotation, variable_atom(fold.value_name)}

    clauses =
      Enum.zip(fold.datatype.constructors, fold.handler_names)
      |> Enum.map(fn {constructor, handler_name} ->
        variables =
          Enum.map(constructor.fields, &{:var, annotation, field_variable(constructor, &1.index)})

        pattern = constructor_pattern(constructor, variables, annotation, layout)
        handler = {:var, annotation, variable_atom(handler_name)}

        body =
          Enum.reduce(variables, handler, fn variable, current ->
            {:call, annotation, current, [variable]}
          end)

        {:clause, annotation, [pattern], [], [body]}
      end)

    body = {:case, annotation, value, clauses}
    clause = {:clause, annotation, arguments, [], [body]}
    {:function, annotation, safe_atom(definition.name), length(arguments), [clause]}
  end

  defp lower_definition(definition, globals, annotation, layout) do
    {parameters, body} = unwrap_parameters(definition.expression, definition.parameters, [])
    environment = Map.new(parameters, fn name -> {name, variable_atom(name)} end)
    arguments = Enum.map(parameters, &{:var, annotation, variable_atom(&1)})
    expression = lower_expression(body, environment, globals, annotation, layout)
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

  defp lower_expression(
         %{tag: :integer, value: value},
         _environment,
         _globals,
         annotation,
         _layout
       ),
       do: {:integer, annotation, value}

  defp lower_expression(
         %{tag: :boolean, value: value},
         _environment,
         _globals,
         annotation,
         _layout
       ),
       do: {:atom, annotation, value}

  defp lower_expression(%{tag: :variable, name: name}, environment, globals, annotation, _layout) do
    case Map.fetch(environment, name) do
      {:ok, variable} -> {:var, annotation, variable}
      :error -> curried_global(name, Map.fetch!(globals, name), annotation)
    end
  end

  defp lower_expression(
         %{tag: :function, parameter: parameter, body: body},
         environment,
         globals,
         annotation,
         layout
       ) do
    variable = variable_atom(parameter)

    clause =
      {:clause, annotation, [{:var, annotation, variable}], [],
       [
         lower_expression(
           body,
           Map.put(environment, parameter, variable),
           globals,
           annotation,
           layout
         )
       ]}

    {:fun, annotation, {:clauses, [clause]}}
  end

  defp lower_expression(
         %{tag: :call, callee: %{tag: :variable, name: name}, arguments: arguments},
         environment,
         globals,
         annotation,
         layout
       )
       when not is_map_key(environment, name) and is_map_key(globals, name) and
              :erlang.map_get(name, globals) == length(arguments) do
    {:call, annotation, {:atom, annotation, safe_atom(name)},
     Enum.map(arguments, &lower_expression(&1, environment, globals, annotation, layout))}
  end

  defp lower_expression(
         %{tag: :call, callee: callee, arguments: arguments},
         environment,
         globals,
         annotation,
         layout
       ) do
    Enum.reduce(
      arguments,
      lower_expression(callee, environment, globals, annotation, layout),
      fn argument, current ->
        {:call, annotation, current,
         [lower_expression(argument, environment, globals, annotation, layout)]}
      end
    )
  end

  defp lower_expression(
         %{tag: :let, name: name, value: value, body: body},
         environment,
         globals,
         annotation,
         layout
       ) do
    variable = variable_atom(name)

    {:block, annotation,
     [
       {:match, annotation, {:var, annotation, variable},
        lower_expression(value, environment, globals, annotation, layout)},
       lower_expression(body, Map.put(environment, name, variable), globals, annotation, layout)
     ]}
  end

  defp lower_expression(
         %{tag: :tuple, elements: elements},
         environment,
         globals,
         annotation,
         layout
       ) do
    {:tuple, annotation,
     Enum.map(elements, &lower_expression(&1, environment, globals, annotation, layout))}
  end

  defp lower_expression(
         %{tag: :annotate, expression: expression},
         environment,
         globals,
         annotation,
         layout
       ),
       do: lower_expression(expression, environment, globals, annotation, layout)

  defp lower_expression(%{tag: :construct} = expression, environment, globals, annotation, layout) do
    evaluations =
      expression.arguments
      |> Enum.with_index()
      |> Enum.map(fn {argument, evaluation_index} ->
        {field_index, value} =
          case argument do
            %{expression: value, field_index: field_index} -> {field_index, value}
            %{field_index: field_index} = value -> {field_index, value}
          end

        variable = construction_variable(expression.path, evaluation_index)

        {%{field_index: field_index, variable: variable},
         {:match, annotation, {:var, annotation, variable},
          lower_expression(value, environment, globals, annotation, layout)}}
      end)

    ordered_values =
      evaluations
      |> Enum.map(&elem(&1, 0))
      |> Enum.sort_by(& &1.field_index)
      |> Enum.map(&{:var, annotation, &1.variable})

    value = constructor_value(expression.constructor, ordered_values, annotation, layout)
    bindings = Enum.map(evaluations, &elem(&1, 1))
    if bindings == [], do: value, else: {:block, annotation, bindings ++ [value]}
  end

  defp lower_expression(
         %{tag: :match, scrutinee: scrutinee, clauses: clauses, path: path},
         environment,
         globals,
         annotation,
         layout
       ) do
    variable = match_variable(path)
    scrutinee = lower_expression(scrutinee, environment, globals, annotation, layout)

    decision =
      lower_clause_chain(
        variable,
        expand_clauses(clauses),
        environment,
        globals,
        annotation,
        layout,
        0
      )

    {:block, annotation,
     [{:match, annotation, {:var, annotation, variable}, scrutinee}, decision]}
  end

  defp lower_clause_chain(
         _variable,
         [],
         _environment,
         _globals,
         annotation,
         _layout,
         _depth
       ) do
    {:call, annotation,
     {:remote, annotation, {:atom, annotation, :erlang}, {:atom, annotation, :error}},
     [{:atom, annotation, :catena_invalid_typed_value}]}
  end

  defp lower_clause_chain(
         variable,
         [clause | rest],
         environment,
         globals,
         annotation,
         layout,
         depth
       ) do
    {pattern, bindings} = lower_pattern(clause.pattern, annotation, layout, %{})
    branch_environment = Map.merge(environment, bindings)
    body = lower_expression(clause.body, branch_environment, globals, annotation, layout)

    fallback =
      lower_clause_chain(
        variable,
        rest,
        environment,
        globals,
        annotation,
        layout,
        depth + 1
      )

    case clause.guard do
      nil ->
        {:case, annotation, {:var, annotation, variable},
         [
           {:clause, annotation, [pattern], [], [body]},
           {:clause, annotation, [{:var, annotation, :_}], [], [fallback]}
         ]}

      guard ->
        fallback_variable = String.to_atom("#{variable}_Fallback_#{depth}")

        fallback_function =
          {:fun, annotation, {:clauses, [{:clause, annotation, [], [], [fallback]}]}}

        fallback_call = {:call, annotation, {:var, annotation, fallback_variable}, []}
        guard = lower_expression(guard, branch_environment, globals, annotation, layout)

        guarded_body =
          {:case, annotation, guard,
           [
             {:clause, annotation, [{:atom, annotation, true}], [], [body]},
             {:clause, annotation, [{:atom, annotation, false}], [], [fallback_call]}
           ]}

        decision =
          {:case, annotation, {:var, annotation, variable},
           [
             {:clause, annotation, [pattern], [], [guarded_body]},
             {:clause, annotation, [{:var, annotation, :_}], [], [fallback_call]}
           ]}

        {:block, annotation,
         [
           {:match, annotation, {:var, annotation, fallback_variable}, fallback_function},
           decision
         ]}
    end
  end

  defp lower_pattern(%{tag: :wildcard}, annotation, _layout, bindings),
    do: {{:var, annotation, :_}, bindings}

  defp lower_pattern(%{tag: :bind, name: name}, annotation, _layout, bindings) do
    variable = variable_atom(name)
    {{:var, annotation, variable}, Map.put(bindings, name, variable)}
  end

  defp lower_pattern(%{tag: :integer, value: value}, annotation, _layout, bindings),
    do: {{:integer, annotation, value}, bindings}

  defp lower_pattern(%{tag: :boolean, value: value}, annotation, _layout, bindings),
    do: {{:atom, annotation, value}, bindings}

  defp lower_pattern(%{tag: :tuple, elements: elements}, annotation, layout, bindings) do
    {patterns, bindings} = lower_patterns(elements, annotation, layout, bindings)
    {{:tuple, annotation, patterns}, bindings}
  end

  defp lower_pattern(
         %{tag: :constructor, constructor: constructor, patterns: patterns},
         annotation,
         layout,
         bindings
       ) do
    {patterns, bindings} = lower_patterns(patterns, annotation, layout, bindings)
    {constructor_pattern(constructor, patterns, annotation, layout), bindings}
  end

  defp lower_pattern(%{tag: :as, pattern: pattern, name: name}, annotation, layout, bindings) do
    {pattern, bindings} = lower_pattern(pattern, annotation, layout, bindings)
    variable = variable_atom(name)

    {{:match, annotation, {:var, annotation, variable}, pattern},
     Map.put(bindings, name, variable)}
  end

  defp lower_patterns(patterns, annotation, layout, bindings) do
    Enum.map_reduce(patterns, bindings, fn pattern, current ->
      lower_pattern(pattern, annotation, layout, current)
    end)
  end

  defp constructor_value(constructor, values, annotation, :uniform) do
    {:tuple, annotation,
     [
       {:atom, annotation, :catena_adt},
       {:atom, annotation, safe_atom(constructor.type_id)},
       {:integer, annotation, constructor.index},
       {:tuple, annotation, values}
     ]}
  end

  defp constructor_value(constructor, [], annotation, :compact),
    do: {:atom, annotation, constructor_atom(constructor)}

  defp constructor_value(constructor, values, annotation, :compact),
    do: {:tuple, annotation, [{:atom, annotation, constructor_atom(constructor)} | values]}

  defp constructor_pattern(constructor, values, annotation, layout),
    do: constructor_value(constructor, values, annotation, layout)

  defp expand_clauses(clauses) do
    Enum.flat_map(clauses, fn clause ->
      Enum.map(expand_pattern(clause.pattern), &%{clause | pattern: &1})
    end)
  end

  defp expand_pattern(%{tag: :or, alternatives: alternatives}),
    do: Enum.flat_map(alternatives, &expand_pattern/1)

  defp expand_pattern(%{tag: :tuple, elements: elements} = pattern) do
    elements |> Enum.map(&expand_pattern/1) |> cartesian() |> Enum.map(&%{pattern | elements: &1})
  end

  defp expand_pattern(%{tag: :constructor, patterns: patterns} = pattern) do
    patterns |> Enum.map(&expand_pattern/1) |> cartesian() |> Enum.map(&%{pattern | patterns: &1})
  end

  defp expand_pattern(%{tag: :as, pattern: inner} = pattern),
    do: Enum.map(expand_pattern(inner), &%{pattern | pattern: &1})

  defp expand_pattern(pattern), do: [pattern]

  defp cartesian([]), do: [[]]
  defp cartesian([head | tail]), do: for(item <- head, rest <- cartesian(tail), do: [item | rest])

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

  defp construction_variable(path, index),
    do: String.to_atom("C_Field_#{sanitize(path)}_#{index}")

  defp match_variable(path), do: String.to_atom("C_Match_#{sanitize(path)}")

  defp field_variable(constructor, index),
    do: String.to_atom("C_Fold_#{constructor.index}_#{index}")

  defp sanitize(path), do: String.replace(path, ~r/[^A-Za-z0-9]/, "_")
  defp constructor_atom(constructor), do: safe_atom("#{constructor.type_id}::#{constructor.name}")
  defp variable_atom(name), do: String.to_atom("V_" <> name)
  defp safe_atom(name), do: String.to_atom(name)
end
