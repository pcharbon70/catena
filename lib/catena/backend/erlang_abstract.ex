defmodule Catena.Backend.ErlangAbstract do
  @moduledoc "Lower verified Catena typed core to Erlang/OTP 29 Abstract Format."

  alias Catena.Diagnostic

  @spec lower(map(), keyword()) :: [term()]
  def lower(core, options \\ []) do
    annotation = annotation(core)
    module = safe_atom(core.module)
    layout = Keyword.get(options, :layout, :compact)

    globals =
      core.definitions
      |> Map.new(&{&1.name, length(&1.parameters)})
      |> Map.merge(imported_condition_globals(core))
      |> Map.put(:condition_lowering, Keyword.get(options, :condition_lowering, :auto))

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

  @doc "Lowers already typed clauses through the selective-receive condition harness."
  @spec lower_receive!(map(), [map()], keyword()) :: term()
  def lower_receive!(core, clauses, options) when is_list(clauses) do
    message_type = Keyword.get(options, :message_type)

    if is_nil(message_type) or not closed_type?(message_type) do
      condition_fail(
        "CND006",
        "receive lowering requires one explicit closed message type",
        "$.receive"
      )
    end

    annotation = annotation(core)
    layout = Keyword.get(options, :layout, :compact)

    globals =
      core.definitions
      |> Map.new(&{&1.name, length(&1.parameters)})
      |> Map.merge(imported_condition_globals(core))
      |> Map.put(:condition_lowering, :native)

    lowered =
      Enum.map(clauses, fn clause ->
        if contains_or_pattern?(clause.pattern) do
          condition_fail(
            "CND006",
            "the receive harness does not admit or-pattern expansion",
            clause.path
          )
        end

        {pattern, bindings} = lower_pattern(clause.pattern, annotation, layout, %{})
        body = lower_expression(clause.body, bindings, globals, annotation, layout)

        guards =
          case clause.guard do
            nil ->
              []

            %{condition_evidence: %{native: true, expanded_core: expanded}} ->
              [[lower_native(expanded, bindings, annotation)]]

            _guard ->
              condition_fail(
                "CND006",
                "receive conditions must lower to portable native guard operations",
                clause.path
              )
          end

        {:clause, annotation, [pattern], guards, [body]}
      end)

    {:receive, annotation, lowered}
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

  defp lower_expression(
         %{tag: :unary, operator: operator, operand: operand},
         environment,
         globals,
         annotation,
         layout
       ) do
    {:op, annotation, erlang_operator(operator),
     lower_expression(operand, environment, globals, annotation, layout)}
  end

  defp lower_expression(
         %{tag: :binary, operator: operator, left: left, right: right},
         environment,
         globals,
         annotation,
         layout
       ) do
    {:op, annotation, erlang_operator(operator),
     lower_expression(left, environment, globals, annotation, layout),
     lower_expression(right, environment, globals, annotation, layout)}
  end

  defp lower_expression(%{tag: :variable, name: name}, environment, globals, annotation, _layout) do
    case Map.fetch(environment, name) do
      {:ok, variable} ->
        {:var, annotation, variable}

      :error ->
        case Map.fetch!(globals, name) do
          arity when is_integer(arity) ->
            curried_global(name, arity, annotation)

          {:remote, module, function, arity} ->
            curried_remote(module, function, arity, annotation)
        end
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
         %{tag: :call, callee: %{tag: :variable, name: name} = callee, arguments: arguments},
         environment,
         globals,
         annotation,
         layout
       ) do
    lowered_arguments =
      Enum.map(arguments, &lower_expression(&1, environment, globals, annotation, layout))

    case {Map.has_key?(environment, name), Map.get(globals, name)} do
      {false, arity} when is_integer(arity) and arity == length(arguments) ->
        {:call, annotation, {:atom, annotation, safe_atom(name)}, lowered_arguments}

      {false, {:remote, module, function, arity}} when arity == length(arguments) ->
        {:call, annotation,
         {:remote, annotation, {:atom, annotation, module}, {:atom, annotation, function}},
         lowered_arguments}

      _other ->
        Enum.reduce(
          arguments,
          lower_expression(callee, environment, globals, annotation, layout),
          fn argument, current ->
            {:call, annotation, current,
             [lower_expression(argument, environment, globals, annotation, layout)]}
          end
        )
    end
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
        clauses,
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

    alternatives = expand_pattern(clause.pattern)
    {_first_pattern, first_bindings} = lower_pattern(hd(alternatives), annotation, layout, %{})
    binding_names = first_bindings |> Map.keys() |> Enum.sort()
    branch_environment = Map.merge(environment, first_bindings)
    body = lower_expression(clause.body, branch_environment, globals, annotation, layout)
    continuation_variable = String.to_atom("#{variable}_Clause_#{depth}")

    continuation_arguments =
      Enum.map(binding_names, &{:var, annotation, Map.fetch!(first_bindings, &1)})

    continuation_clauses =
      continuation_clauses(
        clause,
        continuation_arguments,
        body,
        fallback,
        branch_environment,
        globals,
        annotation,
        layout
      )

    continuation = {:fun, annotation, {:clauses, continuation_clauses}}

    pattern_clauses =
      Enum.map(alternatives, fn alternative ->
        {pattern, bindings} = lower_pattern(alternative, annotation, layout, %{})
        arguments = Enum.map(binding_names, &{:var, annotation, Map.fetch!(bindings, &1)})
        call = {:call, annotation, {:var, annotation, continuation_variable}, arguments}
        {:clause, annotation, [pattern], [], [call]}
      end)

    decision =
      {:case, annotation, {:var, annotation, variable},
       pattern_clauses ++ [{:clause, annotation, [{:var, annotation, :_}], [], [fallback]}]}

    {:block, annotation,
     [
       {:match, annotation, {:var, annotation, continuation_variable}, continuation},
       decision
     ]}
  end

  defp continuation_clauses(
         %{guard: nil},
         arguments,
         body,
         _fallback,
         _environment,
         _globals,
         annotation,
         _layout
       ),
       do: [{:clause, annotation, arguments, [], [body]}]

  defp continuation_clauses(
         %{guard: guard},
         arguments,
         body,
         fallback,
         environment,
         globals,
         annotation,
         layout
       ) do
    lowering = Map.fetch!(globals, :condition_lowering)
    evidence = Map.get(guard, :condition_evidence)
    native? = not is_nil(evidence) and evidence.native

    if lowering in [:auto, :native] and native? do
      native_guard = lower_native(evidence.expanded_core, environment, annotation)

      [
        {:clause, annotation, arguments, [[native_guard]], [body]},
        {:clause, annotation, arguments, [], [fallback]}
      ]
    else
      ordinary_guard =
        if is_nil(evidence) do
          lower_expression(guard, environment, globals, annotation, layout)
        else
          lower_native(evidence.expanded_core, environment, annotation)
        end

      guarded_body =
        {:case, annotation, ordinary_guard,
         [
           {:clause, annotation, [{:atom, annotation, true}], [], [body]},
           {:clause, annotation, [{:atom, annotation, false}], [], [fallback]}
         ]}

      [{:clause, annotation, arguments, [], [guarded_body]}]
    end
  end

  defp lower_native(%{tag: :integer, value: value}, _environment, annotation),
    do: {:integer, annotation, value}

  defp lower_native(%{tag: :boolean, value: value}, _environment, annotation),
    do: {:atom, annotation, value}

  defp lower_native(%{tag: :variable, name: name}, environment, annotation),
    do: {:var, annotation, Map.fetch!(environment, name)}

  defp lower_native(
         %{tag: :unary, operator: operator, operand: operand},
         environment,
         annotation
       ),
       do:
         {:op, annotation, erlang_operator(operator),
          lower_native(operand, environment, annotation)}

  defp lower_native(
         %{tag: :binary, operator: operator, left: left, right: right},
         environment,
         annotation
       ),
       do:
         {:op, annotation, erlang_operator(operator), lower_native(left, environment, annotation),
          lower_native(right, environment, annotation)}

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

  defp erlang_operator(:not), do: :not
  defp erlang_operator(:negate), do: :-
  defp erlang_operator(:and), do: :andalso
  defp erlang_operator(:or), do: :orelse
  defp erlang_operator(:equal), do: :"=:="
  defp erlang_operator(:not_equal), do: :"=/="
  defp erlang_operator(:less), do: :<
  defp erlang_operator(:less_equal), do: :"=<"
  defp erlang_operator(:greater), do: :>
  defp erlang_operator(:greater_equal), do: :>=
  defp erlang_operator(:add), do: :+
  defp erlang_operator(:subtract), do: :-
  defp erlang_operator(:multiply), do: :*

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

  defp curried_remote(module, function, 0, annotation) do
    {:fun, annotation, {:function, module, function, 0}}
  end

  defp curried_remote(module, function, arity, annotation) do
    variables = Enum.map(1..arity, &String.to_atom("RemoteCurry#{&1}"))

    body =
      {:call, annotation,
       {:remote, annotation, {:atom, annotation, module}, {:atom, annotation, function}},
       Enum.map(variables, &{:var, annotation, &1})}

    Enum.reduce(Enum.reverse(variables), body, fn variable, inner ->
      clause = {:clause, annotation, [{:var, annotation, variable}], [], [inner]}
      {:fun, annotation, {:clauses, [clause]}}
    end)
  end

  defp imported_condition_globals(core) do
    core
    |> Map.get(:conditions, %{})
    |> Map.get(:imported, %{})
    |> Map.new(fn {alias_name, record} ->
      [module, function] = String.split(record.id, ".", parts: 2)
      {alias_name, {:remote, safe_atom(module), safe_atom(function), length(record.parameters)}}
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

  defp closed_type?({:var, _id}), do: false
  defp closed_type?({:skolem, _id}), do: false

  defp closed_type?(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.all?(&closed_type?/1)

  defp closed_type?(list) when is_list(list), do: Enum.all?(list, &closed_type?/1)
  defp closed_type?(_type), do: true

  defp contains_or_pattern?(%{tag: :or}), do: true

  defp contains_or_pattern?(%{tag: :tuple, elements: elements}),
    do: Enum.any?(elements, &contains_or_pattern?/1)

  defp contains_or_pattern?(%{tag: :constructor, patterns: patterns}),
    do: Enum.any?(patterns, &contains_or_pattern?/1)

  defp contains_or_pattern?(%{tag: :as, pattern: pattern}), do: contains_or_pattern?(pattern)
  defp contains_or_pattern?(_pattern), do: false

  defp condition_fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
