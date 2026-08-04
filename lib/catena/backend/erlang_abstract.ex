defmodule Catena.Backend.ErlangAbstract do
  @moduledoc "Lower verified Catena C001-C006 runtime core to Erlang/OTP 29 Abstract Format."

  alias Catena.Diagnostic

  @spec lower(map(), keyword()) :: [term()]
  def lower(core, options \\ []) do
    annotation = annotation(core)
    module = safe_atom(core.module)
    layout = Keyword.get(options, :layout, :compact)

    runtime_definitions = Enum.reject(core.definitions, &Map.get(&1, :verification_only?, false))

    globals =
      runtime_definitions
      |> Map.new(&{&1.name, length(&1.parameters)})
      |> Map.merge(imported_condition_globals(core))
      |> Map.put(:condition_lowering, Keyword.get(options, :condition_lowering, :auto))
      |> Map.put(:effect_handlers, get_in(core, [:effects, :handlers]) || %{})
      |> Map.put(
        :effect_definitions,
        Map.new(runtime_definitions, &{&1.name, effect_definition?(&1)})
      )

    exports =
      runtime_definitions
      |> Enum.filter(&(&1.name in core.exports or Map.get(&1, :linker_only?, false)))
      |> Enum.map(fn definition ->
        {safe_atom(definition.name), length(definition.parameters)}
      end)

    handler_exports =
      core.effects.exported_handlers
      |> Enum.flat_map(fn handler ->
        arity = length(handler.parameters)

        [
          {handler_dispatch_atom(handler.name), arity + 1},
          {handler_return_atom(handler.name), arity + 2}
        ]
      end)

    exports = exports ++ handler_exports

    functions =
      Enum.flat_map(runtime_definitions, fn definition ->
        definition
        |> lower_definition(globals, annotation, layout)
        |> List.wrap()
      end)

    handler_functions =
      Enum.flat_map(
        core.effects.exported_handlers,
        &lower_handler_helpers(&1, globals, annotation, layout)
      )

    attributes =
      [
        {:attribute, annotation, :file,
         {String.to_charlist(Map.get(core, :source, "<catena-json>")), 1}},
        {:attribute, annotation, :module, module},
        {:attribute, annotation, :export, exports}
      ] ++ unused_effect_wrapper_attribute(core, annotation)

    attributes ++ functions ++ handler_functions
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
         %{expression: %{tag: tag} = fold} = definition,
         _globals,
         annotation,
         layout
       )
       when tag in [:derived_fold, :derived_eliminator] do
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

  defp lower_definition(
         %{expression: %{tag: :derived_constructor, constructor: constructor}} = definition,
         _globals,
         annotation,
         layout
       ) do
    arguments = Enum.map(definition.parameters, &{:var, annotation, variable_atom(&1)})
    body = constructor_value(constructor, arguments, annotation, layout)
    clause = {:clause, annotation, arguments, [], [body]}
    {:function, annotation, safe_atom(definition.name), length(arguments), [clause]}
  end

  defp lower_definition(
         %{expression: %{tag: :derived_capability} = derived} = definition,
         _globals,
         annotation,
         layout
       ) do
    arguments = Enum.map(definition.parameters, &{:var, annotation, variable_atom(&1)})
    body = lower_derived_capability(derived, definition.parameters, annotation, layout)
    clause = {:clause, annotation, arguments, [], [body]}
    {:function, annotation, safe_atom(definition.name), length(arguments), [clause]}
  end

  defp lower_definition(%{generated?: false} = definition, globals, annotation, layout) do
    if effect_definition?(definition) do
      {parameters, body} = unwrap_parameters(definition.expression, definition.parameters, [])
      environment = Map.new(parameters, fn name -> {name, variable_atom(name)} end)
      arguments = Enum.map(parameters, &{:var, annotation, variable_atom(&1)})
      handlers_variable = :__Catena_Effect_Handlers
      continuation_variable = :__Catena_Effect_Continuation
      handlers = {:var, annotation, handlers_variable}
      continuation = {:var, annotation, continuation_variable}

      worker_expression =
        lower_cps(body, environment, handlers, globals, annotation, layout, continuation)

      worker_arguments =
        arguments ++
          [
            {:var, annotation, handlers_variable},
            {:var, annotation, continuation_variable}
          ]

      worker_clause = {:clause, annotation, worker_arguments, [], [worker_expression]}

      worker =
        {:function, annotation, cps_worker_atom(definition.name), length(worker_arguments),
         [worker_clause]}

      identity = cps_identity(annotation)

      wrapper_expression =
        {:call, annotation, {:atom, annotation, cps_worker_atom(definition.name)},
         arguments ++ [{:map, annotation, []}, identity]}

      wrapper_clause = {:clause, annotation, arguments, [], [wrapper_expression]}

      wrapper =
        {:function, annotation, safe_atom(definition.name), length(arguments), [wrapper_clause]}

      [wrapper, worker]
    else
      lower_direct_definition(definition, globals, annotation, layout)
    end
  end

  defp lower_definition(definition, globals, annotation, layout) do
    lower_direct_definition(definition, globals, annotation, layout)
  end

  defp lower_direct_definition(definition, globals, annotation, layout) do
    {parameters, body} = unwrap_parameters(definition.expression, definition.parameters, [])
    environment = Map.new(parameters, fn name -> {name, variable_atom(name)} end)
    arguments = Enum.map(parameters, &{:var, annotation, variable_atom(&1)})
    expression = lower_expression(body, environment, globals, annotation, layout)
    clause = {:clause, annotation, arguments, [], [expression]}
    {:function, annotation, safe_atom(definition.name), length(parameters), [clause]}
  end

  defp lower_handler_helpers(handler, globals, annotation, layout) do
    parameter_variables =
      handler.parameters
      |> Enum.with_index()
      |> Enum.map(fn {_parameter, index} ->
        cps_variable(handler.path, "abi_parameter_#{index}")
      end)

    parameter_environment =
      Enum.zip(handler.parameters, parameter_variables)
      |> Enum.reduce(%{}, fn {parameter, variable}, current ->
        Map.put(current, parameter.name, variable)
      end)

    outer_handlers_variable = cps_variable(handler.path, "abi_outer_handlers")
    outer_handlers = {:var, annotation, outer_handlers_variable}

    dispatch =
      cps_handler_dispatch(
        handler,
        parameter_environment,
        outer_handlers,
        globals,
        annotation,
        layout,
        {:exported_handler, handler.id}
      )

    dispatch_arguments =
      Enum.map(parameter_variables, &{:var, annotation, &1}) ++ [outer_handlers]

    dispatch_function =
      {:function, annotation, handler_dispatch_atom(handler.name), length(dispatch_arguments),
       [{:clause, annotation, dispatch_arguments, [], [dispatch]}]}

    return_variable = cps_variable(handler.path, "abi_return")

    return_body =
      lower_cps(
        handler.return_clause.body,
        Map.put(parameter_environment, handler.return_clause.parameter, return_variable),
        outer_handlers,
        globals,
        annotation,
        layout,
        cps_identity(annotation)
      )

    return_arguments =
      dispatch_arguments ++ [{:var, annotation, return_variable}]

    return_function =
      {:function, annotation, handler_return_atom(handler.name), length(return_arguments),
       [{:clause, annotation, return_arguments, [], [return_body]}]}

    [dispatch_function, return_function]
  end

  defp lower_derived_capability(%{capability: "Equatable"}, [left, right], annotation, _layout) do
    {:op, annotation, :"=:=", {:var, annotation, variable_atom(left)},
     {:var, annotation, variable_atom(right)}}
  end

  defp lower_derived_capability(%{capability: "Orderable"}, [left, right], annotation, _layout) do
    left = {:var, annotation, variable_atom(left)}
    right = {:var, annotation, variable_atom(right)}

    {:if, annotation,
     [
       {:clause, annotation, [], [[{:op, annotation, :<, left, right}]],
        [{:integer, annotation, -1}]},
       {:clause, annotation, [], [[{:op, annotation, :"=:=", left, right}]],
        [{:integer, annotation, 0}]},
       {:clause, annotation, [], [[{:atom, annotation, true}]], [{:integer, annotation, 1}]}
     ]}
  end

  defp lower_derived_capability(
         %{capability: capability, datatype: datatype, target_indexes: targets},
         parameters,
         annotation,
         layout
       )
       when capability in ~w(Mapper TwoSlotMapper CollectingMapper) do
    subject = parameters |> List.last() |> variable_atom() |> then(&{:var, annotation, &1})
    callbacks = parameters |> Enum.drop(-1) |> Enum.map(&{:var, annotation, variable_atom(&1)})

    clauses =
      Enum.map(datatype.constructors, fn constructor ->
        fields =
          Enum.map(constructor.fields, &{:var, annotation, field_variable(constructor, &1.index)})

        values =
          Enum.zip(constructor.fields, fields)
          |> Enum.map(fn {field, value} ->
            case field_target(field.type, targets) do
              nil -> value
              target -> apply_curried(Enum.at(callbacks, target), [value], annotation)
            end
          end)

        {:clause, annotation, [constructor_pattern(constructor, fields, annotation, layout)], [],
         [constructor_value(constructor, values, annotation, layout)]}
      end)

    {:case, annotation, subject, clauses}
  end

  defp lower_derived_capability(
         %{capability: "Reducible", datatype: datatype, target_indexes: targets},
         [callback_name, initial_name, subject_name],
         annotation,
         layout
       ) do
    callback = {:var, annotation, variable_atom(callback_name)}
    initial = {:var, annotation, variable_atom(initial_name)}
    subject = {:var, annotation, variable_atom(subject_name)}

    clauses =
      Enum.map(datatype.constructors, fn constructor ->
        fields =
          Enum.map(constructor.fields, &{:var, annotation, field_variable(constructor, &1.index)})

        result =
          Enum.zip(constructor.fields, fields)
          |> Enum.reduce(initial, fn {field, value}, accumulator ->
            if is_nil(field_target(field.type, targets)),
              do: accumulator,
              else: apply_curried(callback, [accumulator, value], annotation)
          end)

        {:clause, annotation, [constructor_pattern(constructor, fields, annotation, layout)], [],
         [result]}
      end)

    {:case, annotation, subject, clauses}
  end

  defp field_target({:var, index}, targets), do: Enum.find_index(targets, &(&1 == index))
  defp field_target(_type, _targets), do: nil

  defp apply_curried(function, arguments, annotation) do
    Enum.reduce(arguments, function, fn argument, current ->
      {:call, annotation, current, [argument]}
    end)
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

  defp lower_cps(
         %{tag: tag} = expression,
         environment,
         _handlers,
         globals,
         annotation,
         layout,
         continuation
       )
       when tag in [:integer, :boolean, :variable] do
    apply_cps_continuation(
      continuation,
      lower_expression(expression, environment, globals, annotation, layout),
      annotation
    )
  end

  defp lower_cps(
         %{tag: :unary, operator: operator, operand: operand},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_values([operand], environment, handlers, globals, annotation, layout, fn [value] ->
      apply_cps_continuation(
        continuation,
        {:op, annotation, erlang_operator(operator), value},
        annotation
      )
    end)
  end

  defp lower_cps(
         %{tag: :binary, operator: :and, left: left, right: right, path: path},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    variable = cps_variable(path, "and_left")

    branch =
      {:case, annotation, {:var, annotation, variable},
       [
         {:clause, annotation, [{:atom, annotation, false}], [],
          [apply_cps_continuation(continuation, {:atom, annotation, false}, annotation)]},
         {:clause, annotation, [{:atom, annotation, true}], [],
          [lower_cps(right, environment, handlers, globals, annotation, layout, continuation)]}
       ]}

    lower_cps(
      left,
      environment,
      handlers,
      globals,
      annotation,
      layout,
      cps_function([variable], branch, annotation)
    )
  end

  defp lower_cps(
         %{tag: :binary, operator: :or, left: left, right: right, path: path},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    variable = cps_variable(path, "or_left")

    branch =
      {:case, annotation, {:var, annotation, variable},
       [
         {:clause, annotation, [{:atom, annotation, true}], [],
          [apply_cps_continuation(continuation, {:atom, annotation, true}, annotation)]},
         {:clause, annotation, [{:atom, annotation, false}], [],
          [lower_cps(right, environment, handlers, globals, annotation, layout, continuation)]}
       ]}

    lower_cps(
      left,
      environment,
      handlers,
      globals,
      annotation,
      layout,
      cps_function([variable], branch, annotation)
    )
  end

  defp lower_cps(
         %{tag: :binary, operator: operator, left: left, right: right},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_values(
      [left, right],
      environment,
      handlers,
      globals,
      annotation,
      layout,
      fn [left_value, right_value] ->
        apply_cps_continuation(
          continuation,
          {:op, annotation, erlang_operator(operator), left_value, right_value},
          annotation
        )
      end
    )
  end

  defp lower_cps(
         %{tag: :tuple, elements: elements},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_values(elements, environment, handlers, globals, annotation, layout, fn values ->
      apply_cps_continuation(continuation, {:tuple, annotation, values}, annotation)
    end)
  end

  defp lower_cps(
         %{tag: :let, name: name, value: value, body: body},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    variable = variable_atom(name)

    next =
      cps_function(
        [variable],
        lower_cps(
          body,
          Map.put(environment, name, variable),
          handlers,
          globals,
          annotation,
          layout,
          continuation
        ),
        annotation
      )

    lower_cps(value, environment, handlers, globals, annotation, layout, next)
  end

  defp lower_cps(
         %{tag: :call, callee: %{tag: :variable, name: name}, arguments: arguments} = expression,
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    local? = Map.has_key?(environment, name)
    arity = Map.get(globals, name)
    effectful? = get_in(globals, [:effect_definitions, name]) == true

    if not local? and effectful? and is_integer(arity) and arity == length(arguments) do
      lower_cps_values(
        arguments,
        environment,
        handlers,
        globals,
        annotation,
        layout,
        fn argument_values ->
          call_handlers =
            alias_handlers(
              handlers,
              Map.get(expression, :effect_bindings, []),
              annotation
            )

          {:call, annotation, {:atom, annotation, cps_worker_atom(name)},
           argument_values ++ [call_handlers, continuation]}
        end
      )
    else
      lower_cps_call(
        expression,
        environment,
        handlers,
        globals,
        annotation,
        layout,
        continuation
      )
    end
  end

  defp lower_cps(
         %{tag: :call, callee: callee, arguments: arguments},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_call(
      %{tag: :call, callee: callee, arguments: arguments},
      environment,
      handlers,
      globals,
      annotation,
      layout,
      continuation
    )
  end

  defp lower_cps(
         %{tag: :annotate, expression: expression},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ),
       do: lower_cps(expression, environment, handlers, globals, annotation, layout, continuation)

  defp lower_cps(
         %{tag: :request} = expression,
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_values(
      expression.arguments,
      environment,
      handlers,
      globals,
      annotation,
      layout,
      fn values ->
        capability = expression.selected_capability

        remote_call(
          Catena.Effect.Runtime,
          :request,
          [
            handlers,
            abstract_term(capability.capability),
            abstract_term(capability.family),
            {:atom, annotation, safe_atom(expression.operation)},
            list_ast(values, annotation),
            continuation
          ],
          annotation
        )
      end
    )
  end

  defp lower_cps(
         %{tag: :resume, resumption: resumption, value: value},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_values([value], environment, handlers, globals, annotation, layout, fn [reply] ->
      resumed =
        remote_call(
          Catena.Effect.Runtime,
          :resume,
          [{:var, annotation, Map.fetch!(environment, resumption)}, reply],
          annotation
        )

      apply_cps_continuation(continuation, resumed, annotation)
    end)
  end

  defp lower_cps(
         %{tag: :handle} = expression,
         environment,
         outer_handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    handler = Map.fetch!(globals.effect_handlers, expression.handler)

    lower_cps_values(
      expression.arguments,
      environment,
      outer_handlers,
      globals,
      annotation,
      layout,
      fn handler_values ->
        handler_outer_handlers =
          alias_handlers(
            outer_handlers,
            Map.get(expression, :handler_effect_bindings, []),
            annotation
          )

        parameter_environment =
          Enum.zip(handler.parameters, handler_values)
          |> Enum.reduce(environment, fn {parameter, {:var, _, variable}}, current ->
            Map.put(current, parameter.name, variable)
          end)

        dispatch =
          if Map.get(handler, :imported?, false) do
            remote_call(
              safe_atom(handler.module),
              handler_dispatch_atom(handler.name),
              handler_values ++ [handler_outer_handlers],
              annotation
            )
          else
            cps_handler_dispatch(
              handler,
              parameter_environment,
              handler_outer_handlers,
              globals,
              annotation,
              layout,
              expression.path
            )
          end

        inner_handlers =
          remote_call(
            Map,
            :put,
            [
              handler_outer_handlers,
              abstract_term(expression.selected_capability.capability),
              dispatch
            ],
            annotation
          )

        return_variable = cps_variable(expression.path, "return")

        return_body =
          if Map.get(handler, :imported?, false) do
            remote_call(
              safe_atom(handler.module),
              handler_return_atom(handler.name),
              handler_values ++
                [handler_outer_handlers, {:var, annotation, return_variable}],
              annotation
            )
          else
            lower_cps(
              handler.return_clause.body,
              Map.put(parameter_environment, handler.return_clause.parameter, return_variable),
              handler_outer_handlers,
              globals,
              annotation,
              layout,
              cps_identity(annotation)
            )
          end

        return_continuation =
          cps_function(
            [return_variable],
            {:block, annotation,
             [
               runtime_trace({:return, handler.id}, annotation),
               return_body
             ]},
            annotation
          )

        handled_result =
          {:block, annotation,
           [
             runtime_trace(
               {:handle, handler.id, expression.selected_capability.capability},
               annotation
             ),
             lower_cps(
               expression.expression,
               environment,
               inner_handlers,
               globals,
               annotation,
               layout,
               return_continuation
             )
           ]}

        apply_cps_continuation(continuation, handled_result, annotation)
      end
    )
  end

  defp lower_cps(
         %{tag: :construct} = expression,
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    arguments = Enum.map(expression.arguments, &Map.get(&1, :expression, &1))

    lower_cps_values(arguments, environment, handlers, globals, annotation, layout, fn values ->
      ordered_values =
        expression.arguments
        |> Enum.zip(values)
        |> Enum.sort_by(fn {argument, _value} -> argument.field_index end)
        |> Enum.map(&elem(&1, 1))

      apply_cps_continuation(
        continuation,
        constructor_value(expression.constructor, ordered_values, annotation, layout),
        annotation
      )
    end)
  end

  defp lower_cps(
         %{tag: :match, scrutinee: scrutinee, clauses: clauses, path: path},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    variable = match_variable(path)

    decision =
      lower_cps_clause_chain(
        variable,
        clauses,
        environment,
        handlers,
        globals,
        annotation,
        layout,
        continuation,
        0
      )

    lower_cps(
      scrutinee,
      environment,
      handlers,
      globals,
      annotation,
      layout,
      cps_function([variable], decision, annotation)
    )
  end

  defp lower_cps(
         %{tag: :function} = expression,
         environment,
         _handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    if effect_control?(expression.body) do
      cps_fail("effectful anonymous functions are outside the 0.1.5 bootstrap corpus")
    end

    apply_cps_continuation(
      continuation,
      lower_expression(expression, environment, globals, annotation, layout),
      annotation
    )
  end

  defp lower_cps(expression, environment, _handlers, globals, annotation, layout, continuation) do
    if effect_control?(expression) do
      cps_fail("unsupported effectful expression #{inspect(expression.tag)}")
    end

    apply_cps_continuation(
      continuation,
      lower_expression(expression, environment, globals, annotation, layout),
      annotation
    )
  end

  defp lower_cps_call(
         %{callee: callee, arguments: arguments},
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation
       ) do
    lower_cps_values(
      [callee | arguments],
      environment,
      handlers,
      globals,
      annotation,
      layout,
      fn [callee_value | argument_values] ->
        result = Enum.reduce(argument_values, callee_value, &{:call, annotation, &2, [&1]})
        apply_cps_continuation(continuation, result, annotation)
      end
    )
  end

  defp lower_cps_values(
         expressions,
         environment,
         handlers,
         globals,
         annotation,
         layout,
         callback
       ) do
    do_lower_cps_values(
      expressions,
      environment,
      handlers,
      globals,
      annotation,
      layout,
      callback,
      [],
      0
    )
  end

  defp do_lower_cps_values(
         [],
         _environment,
         _handlers,
         _globals,
         _annotation,
         _layout,
         callback,
         values,
         _index
       ),
       do: callback.(Enum.reverse(values))

  defp do_lower_cps_values(
         [expression | rest],
         environment,
         handlers,
         globals,
         annotation,
         layout,
         callback,
         values,
         index
       ) do
    variable = cps_variable(Map.get(expression, :path, "cps"), "value_#{index}")

    next =
      cps_function(
        [variable],
        do_lower_cps_values(
          rest,
          environment,
          handlers,
          globals,
          annotation,
          layout,
          callback,
          [{:var, annotation, variable} | values],
          index + 1
        ),
        annotation
      )

    lower_cps(expression, environment, handlers, globals, annotation, layout, next)
  end

  defp cps_handler_dispatch(
         handler,
         environment,
         outer_handlers,
         globals,
         annotation,
         layout,
         scope
       ) do
    continuation_variable = cps_variable({scope, handler.path}, "continuation")

    clauses =
      Enum.map(handler.operation_clauses, fn clause ->
        parameter_variables =
          clause.parameters
          |> Enum.with_index()
          |> Enum.map(fn {_name, index} ->
            cps_variable({scope, clause.path}, "parameter_#{index}")
          end)

        resumption_variable = cps_variable({scope, clause.path}, "resumption")

        clause_environment =
          Enum.zip(clause.parameters, parameter_variables)
          |> Enum.reduce(environment, fn {name, variable}, current ->
            Map.put(current, name, variable)
          end)
          |> Map.put(clause.resumption, resumption_variable)

        create_resumption =
          remote_call(
            Catena.Effect.Runtime,
            :new_resumption,
            [{:var, annotation, continuation_variable}],
            annotation
          )

        body =
          lower_cps(
            clause.body,
            clause_environment,
            outer_handlers,
            globals,
            annotation,
            layout,
            cps_identity(annotation)
          )

        block =
          {:block, annotation,
           [
             runtime_trace({:clause, handler.id, clause.operation}, annotation),
             {:match, annotation, {:var, annotation, resumption_variable}, create_resumption},
             if(contains_resume?(clause.body, clause.resumption),
               do: {:atom, annotation, :ok},
               else: runtime_trace({:abort, handler.id, clause.operation}, annotation)
             ),
             body
           ]}

        {:clause, annotation,
         [
           {:atom, annotation, safe_atom(clause.operation)},
           list_pattern(parameter_variables, annotation),
           {:var, annotation, continuation_variable}
         ], [], [block]}
      end)

    {:fun, annotation, {:clauses, clauses}}
  end

  defp effect_definition?(definition) do
    uses = get_in(definition, [:uses, :row])

    match?(%Catena.Effect.Row{entries: [_ | _]}, uses) or
      (match?(%Catena.Effect.Row{}, uses) and not is_nil(uses.tail)) or
      effect_control?(definition.expression)
  end

  defp effect_control?(%{tag: tag}) when tag in [:request, :handle, :resume], do: true
  defp effect_control?(%{tag: :function, body: body}), do: effect_control?(body)

  defp effect_control?(%{tag: :call, callee: callee, arguments: arguments}),
    do: effect_control?(callee) or Enum.any?(arguments, &effect_control?/1)

  defp effect_control?(%{tag: :let, value: value, body: body}),
    do: effect_control?(value) or effect_control?(body)

  defp effect_control?(%{tag: :tuple, elements: elements}),
    do: Enum.any?(elements, &effect_control?/1)

  defp effect_control?(%{tag: :annotate, expression: expression}), do: effect_control?(expression)
  defp effect_control?(%{tag: :unary, operand: operand}), do: effect_control?(operand)

  defp effect_control?(%{tag: :binary, left: left, right: right}),
    do: effect_control?(left) or effect_control?(right)

  defp effect_control?(%{tag: :construct, arguments: arguments}),
    do: Enum.any?(arguments, &effect_control?(Map.get(&1, :expression, &1)))

  defp effect_control?(%{tag: :match, scrutinee: scrutinee, clauses: clauses}),
    do:
      effect_control?(scrutinee) or
        Enum.any?(clauses, fn clause ->
          (not is_nil(clause.guard) and effect_control?(clause.guard)) or
            effect_control?(clause.body)
        end)

  defp effect_control?(_expression), do: false

  defp cps_identity(annotation) do
    variable = :__catena_cps_identity
    cps_function([variable], {:var, annotation, variable}, annotation)
  end

  defp cps_worker_atom(name), do: safe_atom("__catena_cps_#{name}")
  defp handler_dispatch_atom(name), do: safe_atom("__catena_handler_dispatch_#{name}")
  defp handler_return_atom(name), do: safe_atom("__catena_handler_return_#{name}")

  defp unused_effect_wrapper_attribute(core, annotation) do
    unused =
      core.definitions
      |> Enum.reject(&Map.get(&1, :verification_only?, false))
      |> Enum.filter(&(effect_definition?(&1) and &1.name not in core.exports))
      |> Enum.map(&{safe_atom(&1.name), length(&1.parameters)})

    if unused == [],
      do: [],
      else: [{:attribute, annotation, :compile, {:nowarn_unused_function, unused}}]
  end

  defp alias_handlers(handlers, bindings, annotation) do
    Enum.reduce(bindings, handlers, fn binding, current ->
      selected =
        remote_call(
          Map,
          :fetch!,
          [handlers, abstract_term(binding.selected)],
          annotation
        )

      remote_call(
        Map,
        :put,
        [current, abstract_term(binding.declared), selected],
        annotation
      )
    end)
  end

  defp runtime_trace(event, annotation),
    do: remote_call(Catena.Effect.Runtime, :trace, [abstract_term(event)], annotation)

  defp contains_resume?(%{tag: :resume, resumption: name}, name), do: true

  defp contains_resume?(%{} = expression, name) do
    expression
    |> Map.drop([:path, :tag, :type, :effects, :resumption_evidence])
    |> Map.values()
    |> Enum.any?(&contains_resume?(&1, name))
  end

  defp contains_resume?(values, name) when is_list(values),
    do: Enum.any?(values, &contains_resume?(&1, name))

  defp contains_resume?(_value, _name), do: false

  defp cps_fail(message) do
    raise Catena.TypeError, diagnostic: Diagnostic.new("CPS001", message)
  end

  defp cps_function(variables, body, annotation) do
    {:fun, annotation,
     {:clauses,
      [
        {:clause, annotation, Enum.map(variables, &{:var, annotation, &1}), [], [body]}
      ]}}
  end

  defp apply_cps_continuation(continuation, value, annotation),
    do: {:call, annotation, continuation, [value]}

  defp remote_call(module, function, arguments, annotation) do
    {:call, annotation,
     {:remote, annotation, {:atom, annotation, module}, {:atom, annotation, function}}, arguments}
  end

  defp abstract_term(term), do: :erl_parse.abstract(term)

  defp list_ast(values, annotation),
    do: Enum.reduce(Enum.reverse(values), {nil, annotation}, &{:cons, annotation, &1, &2})

  defp list_pattern(variables, annotation) do
    variables
    |> Enum.map(&{:var, annotation, &1})
    |> list_ast(annotation)
  end

  defp cps_variable(path, suffix),
    do: String.to_atom("__CPS_#{:erlang.phash2({path, suffix})}_#{suffix}")

  defp lower_cps_clause_chain(
         _variable,
         [],
         _environment,
         _handlers,
         _globals,
         annotation,
         _layout,
         _continuation,
         _depth
       ) do
    {:call, annotation,
     {:remote, annotation, {:atom, annotation, :erlang}, {:atom, annotation, :error}},
     [{:atom, annotation, :catena_invalid_typed_value}]}
  end

  defp lower_cps_clause_chain(
         variable,
         [clause | rest],
         environment,
         handlers,
         globals,
         annotation,
         layout,
         continuation,
         depth
       ) do
    fallback =
      lower_cps_clause_chain(
        variable,
        rest,
        environment,
        handlers,
        globals,
        annotation,
        layout,
        continuation,
        depth + 1
      )

    alternatives = expand_pattern(clause.pattern)
    {_first_pattern, first_bindings} = lower_pattern(hd(alternatives), annotation, layout, %{})
    binding_names = first_bindings |> Map.keys() |> Enum.sort()
    branch_environment = Map.merge(environment, first_bindings)

    body =
      lower_cps(
        clause.body,
        branch_environment,
        handlers,
        globals,
        annotation,
        layout,
        continuation
      )

    continuation_variable = String.to_atom("#{variable}_CPS_Clause_#{depth}")

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

    pattern_continuation = {:fun, annotation, {:clauses, continuation_clauses}}

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
       {:match, annotation, {:var, annotation, continuation_variable}, pattern_continuation},
       decision
     ]}
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
