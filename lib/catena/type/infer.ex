defmodule Catena.Type.Infer do
  @moduledoc "Algorithm W with C002-C006 data, condition, trait, effect, and assurance elaboration."

  alias Catena.Effect.Row
  alias Catena.{Categorical, Condition, Data, Derive, Diagnostic, Effect, LanguageVersion, Type}
  alias Catena.Pattern.Coverage
  alias Catena.Type.{Advanced, Parser, Scheme, Unify}

  @type state :: %{next: non_neg_integer(), substitution: map()}
  @condition_versions LanguageVersion.from(:clause_conditions)

  @spec module(map(), keyword()) :: map()
  def module(ast, options \\ []) do
    missing =
      MapSet.difference(MapSet.new(ast.exports), MapSet.new(Enum.map(ast.definitions, & &1.name)))

    if MapSet.size(missing) > 0 do
      fail("T001", "exports have no definitions: #{Enum.join(missing, ", ")}", "$.exports")
    end

    data = Data.elaborate(ast, Keyword.get(options, :interfaces, []))
    conditions = Condition.prepare!(ast, data, options)
    categorical = Categorical.prepare!(ast, data, Keyword.get(options, :interfaces, []))
    effects = Effect.prepare!(ast, data, Keyword.get(options, :interfaces, []))
    derived = Derive.folds(data) ++ Derive.capabilities(data, categorical.derivations)

    definition_effects =
      Map.new(ast.definitions, fn definition ->
        {definition.name,
         Effect.uses!(effects, definition.signature, data, definition.name, definition.path)}
      end)

    initial_environment =
      derived
      |> Map.new(&{&1.name, &1.scheme})
      |> Map.merge(conditions.schemes)

    initial = %{next: 10_000, substitution: %{}}

    {definitions, environment, state} =
      Enum.reduce(ast.definitions, {[], initial_environment, initial}, fn definition,
                                                                          {definitions,
                                                                           environment, state} ->
        if definition.name in ast.exports and is_nil(definition.signature) do
          fail("T008", "exported value #{definition.name} requires a signature", definition.path)
        end

        expression = wrap_parameters(definition)
        gadt? = contains_gadt_match?(expression, data)
        match? = contains_match?(expression)

        if gadt? and is_nil(definition.signature) do
          fail("T010", "a definition matching a GADT requires a signature", definition.path)
        end

        inference_options =
          options
          |> Keyword.take([:coverage_budget, :fact_budget])
          |> Keyword.put(
            :conditions,
            if(ast.frontend_version in @condition_versions, do: conditions, else: nil)
          )

        uses = Map.fetch!(definition_effects, definition.name)

        inference_options =
          inference_options
          |> Keyword.put(:effects, effects)
          |> Keyword.put(:capabilities, uses.capabilities)
          |> Keyword.put(:global_effects, definition_effects)

        {typed, scheme, state} =
          if match? and not is_nil(definition.signature) do
            infer_signed_definition(definition, environment, state, data, inference_options)
          else
            infer_definition(definition, expression, environment, state, data, inference_options)
          end

        core_definition = %{
          name: definition.name,
          parameters: definition.parameters,
          expression: Catena.TypedCore.apply_substitution(typed, state.substitution),
          scheme: scheme,
          kind: definition.kind,
          condition: Condition.definition_evidence(conditions, definition.name),
          clause_definition?: definition.clause_definition?,
          generated?: false,
          verification_only?: Map.get(definition, :verification_only?, false),
          uses: uses,
          effect_row: apply_row_substitution(body_effects(typed), state.substitution),
          verified_uses_row: apply_row_substitution(uses.row, state.substitution),
          path: definition.path
        }

        unless Row.matches_declaration?(
                 core_definition.effect_row,
                 core_definition.verified_uses_row
               ) do
          fail(
            "EFX008",
            "definition #{definition.name} effects do not match its uses row",
            definition.path
          )
        end

        {[core_definition | definitions], Map.put(environment, definition.name, scheme), state}
      end)

    definitions = Enum.reverse(definitions) ++ derived
    environment = Enum.reduce(derived, environment, &Map.put(&2, &1.name, &1.scheme))

    {typed_handlers, state} =
      infer_handlers(effects, environment, state, data, definition_effects)

    typed_handler_map = Map.new(typed_handlers, &{&1.name, &1})
    handlers = Map.merge(effects.handlers, typed_handler_map)

    exported_handlers =
      Enum.map(effects.exported_handlers, &Map.fetch!(handlers, &1.name))

    effects = %{effects | handlers: handlers, exported_handlers: exported_handlers}

    derived_exports =
      derived |> Enum.reject(&Map.get(&1, :linker_only?, false)) |> Enum.map(& &1.name)

    %{
      version: ast.version,
      frontend_version: ast.frontend_version,
      origin: ast.origin,
      module: ast.module,
      exports: ast.exports ++ derived_exports,
      type_exports: ast.type_exports,
      definitions: definitions,
      environment: environment,
      data: data,
      conditions: conditions,
      categorical: categorical,
      effects: effects,
      profile:
        if(
          Enum.any?(
            data.types,
            &Enum.any?(&1.constructors, fn c ->
              c.gadt? or MapSet.size(c.existential_ids) > 0
            end)
          ),
          do: :annotation_directed,
          else: :principal_core
        ),
      next: state.next
    }
  end

  defp infer_definition(definition, expression, environment, state, data, options) do
    {typed, inferred_type, state} =
      infer(expression, environment, state, %{
        data: data,
        coverage_options: options,
        conditions: Keyword.get(options, :conditions),
        effects: Keyword.get(options, :effects),
        capabilities: Keyword.get(options, :capabilities, []),
        global_effects: Keyword.get(options, :global_effects, %{}),
        effect_values: %{},
        resumptions: %{},
        top_level_parameters: length(definition.parameters)
      })

    inferred_type = Type.apply(inferred_type, state.substitution)

    case definition.signature do
      nil ->
        {typed, Type.generalize(environment, inferred_type, state.substitution), state}

      signature ->
        declared =
          Parser.parse_scheme(signature, definition.path <> ".signature", data.types_by_name)

        {expected, state} = skolemize(declared, state)
        substitution = Unify.unify(inferred_type, expected, state.substitution, definition.path)
        {typed, declared, %{state | substitution: substitution}}
    end
  end

  defp infer_signed_definition(definition, environment, state, data, options) do
    declared =
      Parser.parse_scheme(
        definition.signature,
        definition.path <> ".signature",
        data.types_by_name
      )

    {expected, state} = skolemize(declared, state)

    {parameter_types, result_type} =
      split_function!(expected, length(definition.parameters), definition.path)

    local_environment =
      Enum.zip(definition.parameters, parameter_types)
      |> Enum.reduce(environment, fn {name, type}, env ->
        Map.put(env, name, Scheme.mono(type))
      end)

    {typed_body, body_type, state} =
      infer(definition.body, local_environment, state, %{
        data: data,
        coverage_options: options,
        conditions: Keyword.get(options, :conditions),
        effects: Keyword.get(options, :effects),
        capabilities: Keyword.get(options, :capabilities, []),
        global_effects: Keyword.get(options, :global_effects, %{}),
        effect_values: %{},
        resumptions: %{},
        expected: result_type,
        signed?: true,
        top_level_parameters: 0
      })

    substitution = Unify.unify(body_type, result_type, state.substitution, definition.path)

    typed =
      Enum.zip(definition.parameters, parameter_types)
      |> Enum.reverse()
      |> Enum.reduce(typed_body, fn {parameter, parameter_type}, body ->
        %{
          tag: :function,
          parameter: parameter,
          body: body,
          type: {:function, parameter_type, body.type},
          latent_effects: effects_of(body),
          effects: Row.empty(),
          path: definition.path
        }
      end)

    {typed, declared, %{state | substitution: substitution}}
  end

  defp split_function!(type, 0, _path), do: {[], type}

  defp split_function!({:function, parameter, result}, count, path) when count > 0 do
    {parameters, final} = split_function!(result, count - 1, path)
    {[parameter | parameters], final}
  end

  defp split_function!(_type, _count, path),
    do: fail("T002", "signature has fewer parameters than the definition", path)

  @spec infer(map(), map(), state()) :: {map(), Type.t(), state()}
  def infer(expression, environment, state),
    do: infer(expression, environment, state, %{data: empty_data()})

  defp infer(%{tag: :integer} = expression, _environment, state, _context),
    do: {expression |> Map.put(:type, :integer) |> put_effects(Row.empty()), :integer, state}

  defp infer(%{tag: :boolean} = expression, _environment, state, _context),
    do: {expression |> Map.put(:type, :boolean) |> put_effects(Row.empty()), :boolean, state}

  defp infer(
         %{tag: :unary, operator: operator, operand: operand, path: path} = expression,
         environment,
         state,
         context
       ) do
    {typed_operand, operand_type, state} =
      infer(operand, environment, state, Map.delete(context, :expected))

    expected = if operator == :not, do: :boolean, else: :integer
    result = expected
    substitution = Unify.unify(operand_type, expected, state.substitution, path)

    typed =
      expression
      |> Map.put(:operand, typed_operand)
      |> Map.put(:type, result)
      |> put_effects(effects_of(typed_operand))

    {typed, result, %{state | substitution: substitution}}
  end

  defp infer(
         %{tag: :binary, operator: operator, left: left, right: right, path: path} = expression,
         environment,
         state,
         context
       ) do
    child_context = Map.delete(context, :expected)
    {typed_left, left_type, state} = infer(left, environment, state, child_context)
    {typed_right, right_type, state} = infer(right, environment, state, child_context)

    {operand_type, result_type, state} =
      case operator do
        operator when operator in [:and, :or] ->
          substitution = Unify.unify(left_type, :boolean, state.substitution, path)
          substitution = Unify.unify(right_type, :boolean, substitution, path)
          {:boolean, :boolean, %{state | substitution: substitution}}

        operator
        when operator in [
               :add,
               :subtract,
               :multiply,
               :less,
               :less_equal,
               :greater,
               :greater_equal
             ] ->
          substitution = Unify.unify(left_type, :integer, state.substitution, path)
          substitution = Unify.unify(right_type, :integer, substitution, path)
          result = if operator in [:add, :subtract, :multiply], do: :integer, else: :boolean
          {:integer, result, %{state | substitution: substitution}}

        operator when operator in [:equal, :not_equal] ->
          substitution = Unify.unify(left_type, right_type, state.substitution, path)
          compared = Type.apply(left_type, substitution)

          unless compared in [:integer, :boolean] do
            fail("CND003", "condition equality is defined only for Int and Bool", path)
          end

          {compared, :boolean, %{state | substitution: substitution}}
      end

    typed =
      expression
      |> Map.put(:left, typed_left)
      |> Map.put(:right, typed_right)
      |> Map.put(:operand_type, operand_type)
      |> Map.put(:type, result_type)
      |> put_effects(Row.union(effects_of(typed_left), effects_of(typed_right)))

    {typed, result_type, state}
  end

  defp infer(%{tag: :variable, name: name, path: path} = expression, environment, state, context) do
    case Map.fetch(environment, name) do
      {:ok, scheme} ->
        {type, replacements, state} = instantiate_with_substitution(scheme, state)
        type = Type.refine(type, Map.get(context, :refinements, %{}))

        latent =
          (Map.get(Map.get(context, :effect_values, %{}), name) ||
             get_in(Map.get(context, :global_effects, %{}), [name, :row]) || Row.empty())
          |> apply_row_substitution(replacements)

        typed =
          expression
          |> Map.put(:type, type)
          |> Map.put(:latent_effects, latent)
          |> put_effects(Row.empty())

        {typed, type, state}

      :error ->
        fail("T001", "unbound value #{name}", path)
    end
  end

  defp infer(
         %{tag: :function, parameter: parameter, body: body} = expression,
         environment,
         state,
         context
       ) do
    remaining_top_level_parameters = Map.get(context, :top_level_parameters, 0)
    {parameter_type, state} = fresh(state)
    local_environment = Map.put(environment, parameter, Scheme.mono(parameter_type))

    {typed_body, body_type, state} =
      infer(
        body,
        local_environment,
        state,
        context
        |> Map.delete(:expected)
        |> Map.put(:top_level_parameters, max(remaining_top_level_parameters - 1, 0))
        |> Map.update(
          :effect_values,
          %{parameter => Row.empty()},
          &Map.put(&1, parameter, Row.empty())
        )
      )

    body_row = effects_of(typed_body)

    if remaining_top_level_parameters == 0 and not Row.equal?(body_row, Row.empty()) do
      local_capabilities =
        context
        |> Map.get(:capabilities, [])
        |> Enum.reject(&Map.get(&1, :abstract?, false))
        |> MapSet.new(& &1.capability)

      if Enum.any?(body_row.entries, &MapSet.member?(local_capabilities, &1.capability)) do
        fail(
          "EFX003",
          "a locally handled capability escapes in a function value",
          expression.path
        )
      else
        fail("CPS001", "effectful anonymous functions are outside Catena 0.1.5", expression.path)
      end
    end

    type = {:function, Type.apply(parameter_type, state.substitution), body_type}

    typed =
      expression
      |> Map.put(:body, typed_body)
      |> Map.put(:type, type)
      |> Map.put(:latent_effects, effects_of(typed_body))
      |> put_effects(Row.empty())

    {typed, type, state}
  end

  defp infer(
         %{tag: :call, callee: callee, arguments: arguments, path: path} = expression,
         environment,
         state,
         context
       ) do
    {typed_callee, callee_type, state} =
      infer(callee, environment, state, Map.delete(context, :expected))

    {typed_arguments, result_type, state} =
      Enum.reduce(arguments, {[], callee_type, state}, fn argument,
                                                          {typed_arguments, current, state} ->
        {typed_argument, argument_type, state} =
          infer(argument, environment, state, Map.delete(context, :expected))

        {result_type, state} = fresh(state)

        substitution =
          Unify.unify(current, {:function, argument_type, result_type}, state.substitution, path)

        {[typed_argument | typed_arguments], Type.apply(result_type, substitution),
         %{state | substitution: substitution}}
      end)

    state =
      case Map.get(context, :expected) do
        nil ->
          state

        expected ->
          %{state | substitution: Unify.unify(result_type, expected, state.substitution, path)}
      end

    result_type = Type.apply(result_type, state.substitution)

    typed =
      expression
      |> Map.put(:callee, typed_callee)
      |> Map.put(:arguments, Enum.reverse(typed_arguments))
      |> Map.put(:type, result_type)

    {call_effect, effect_bindings} =
      callee_effects!(typed_callee, context, path, state.substitution)

    evaluation_effects =
      [effects_of(typed_callee), call_effect | Enum.map(typed.arguments, &effects_of/1)]
      |> Row.union_all()

    typed =
      typed
      |> Map.put(:effect_bindings, effect_bindings)
      |> put_effects(evaluation_effects)

    {typed, result_type, state}
  end

  defp infer(
         %{tag: :let, name: name, value: value, body: body} = expression,
         environment,
         state,
         context
       ) do
    child_context = Map.delete(context, :expected)
    {typed_value, value_type, state} = infer(value, environment, state, child_context)
    scheme = Type.generalize(environment, value_type, state.substitution)

    effect_values =
      Map.put(
        Map.get(context, :effect_values, %{}),
        name,
        Map.get(typed_value, :latent_effects, Row.empty())
      )

    {typed_body, body_type, state} =
      infer(
        body,
        Map.put(environment, name, scheme),
        state,
        Map.put(context, :effect_values, effect_values)
      )

    typed =
      expression
      |> Map.put(:value, typed_value)
      |> Map.put(:body, typed_body)
      |> Map.put(:scheme, scheme)
      |> Map.put(:type, body_type)
      |> put_effects(Row.union(effects_of(typed_value), effects_of(typed_body)))

    {typed, body_type, state}
  end

  defp infer(%{tag: :tuple, elements: elements} = expression, environment, state, context) do
    {typed_elements, types, state} =
      Enum.reduce(elements, {[], [], state}, fn element, {typed, types, state} ->
        {typed_element, type, state} =
          infer(element, environment, state, Map.delete(context, :expected))

        {[typed_element | typed], [type | types], state}
      end)

    type = {:tuple, Enum.reverse(types)}

    typed_elements = Enum.reverse(typed_elements)

    typed =
      expression
      |> Map.put(:elements, typed_elements)
      |> Map.put(:type, type)
      |> put_effects(typed_elements |> Enum.map(&effects_of/1) |> Row.union_all())

    {typed, type, state}
  end

  defp infer(
         %{tag: :annotate, expression: annotated, signature: signature, path: path} = expression,
         environment,
         state,
         context
       ) do
    declared = Parser.parse_scheme(signature, path <> ".signature", context.data.types_by_name)
    {expected, state} = skolemize(declared, state)

    {typed, inferred, state} =
      infer(annotated, environment, state, Map.delete(context, :expected))

    substitution = Unify.unify(inferred, expected, state.substitution, path)
    type = Type.apply(expected, substitution)

    {expression
     |> Map.put(:expression, typed)
     |> Map.put(:type, type)
     |> put_effects(effects_of(typed)), type, %{state | substitution: substitution}}
  end

  defp infer(%{tag: :request, path: path} = expression, environment, state, context) do
    effects = Map.get(context, :effects) || fail("EFX001", "request requires AST 0.1.5", path)

    {capability, operation} =
      Effect.resolve_request!(effects, expression, Map.get(context, :capabilities, []))

    if length(expression.arguments) != length(operation.parameters) do
      fail("EFX007", "request #{expression.operation} has the wrong arity", path)
    end

    {typed_arguments, state} =
      Enum.zip(expression.arguments, operation.parameters)
      |> Enum.map_reduce(state, fn {argument, expected}, current ->
        {typed, inferred, next} =
          infer(argument, environment, current, Map.delete(context, :expected))

        if match?({:function, _, _}, expected) and
             not Row.equal?(Map.get(typed, :latent_effects, Row.empty()), Row.empty()) do
          fail("EFX002", "operation function arguments must be effect free", argument.path)
        end

        substitution = effect_unify!(inferred, expected, next.substitution, argument.path)
        {typed, %{next | substitution: substitution}}
      end)

    request_effect =
      Row.new([
        %{
          family: capability.family,
          family_name: capability.family_name,
          arguments: capability.arguments,
          capability: capability.capability,
          name: capability.name,
          abstract?: Map.get(capability, :abstract?, false)
        }
      ])

    row = Row.union(Row.union_all(Enum.map(typed_arguments, &effects_of/1)), request_effect)

    typed =
      expression
      |> Map.put(:arguments, typed_arguments)
      |> Map.put(:selected_capability, capability)
      |> Map.put(:operation_evidence, operation)
      |> Map.put(:type, operation.result)
      |> put_effects(row)

    {typed, operation.result, state}
  end

  defp infer(%{tag: :handle, path: path} = expression, environment, state, context) do
    effects = Map.get(context, :effects) || fail("EFX001", "handler requires AST 0.1.5", path)
    handler = Effect.handler!(effects, expression.handler, path)

    {fresh_variables, state} = fresh_many(length(handler.variables), state)
    replacements = Enum.zip(handler.variables, fresh_variables) |> Map.new()
    handler = instantiate_handler(handler, replacements)

    {handler_clause_row, handler_effect_bindings} =
      instantiate_call_row!(
        Map.get(handler, :uses_row, Row.empty()),
        Map.get(context, :capabilities, []),
        path
      )

    if length(expression.arguments) != length(handler.parameters) do
      fail("EFX007", "handler #{handler.name} has the wrong argument count", path)
    end

    {typed_arguments, state} =
      Enum.zip(expression.arguments, handler.parameters)
      |> Enum.map_reduce(state, fn {argument, parameter}, current ->
        {typed, inferred, next} =
          infer(argument, environment, current, Map.delete(context, :expected))

        substitution =
          effect_unify!(inferred, parameter.parsed_type, next.substitution, argument.path)

        {typed, %{next | substitution: substitution}}
      end)

    capability = %{
      family: handler.family,
      family_name: handler.family_name,
      arguments: handler.arguments,
      capability: "local://#{path}",
      name: expression.capability,
      abstract?: false,
      path: path
    }

    inner_context =
      Map.update(context, :capabilities, [capability], &(&1 ++ [capability]))

    {typed_expression, inferred, state} =
      infer(
        expression.expression,
        environment,
        state,
        Map.put(inner_context, :expected, handler.input)
      )

    substitution = effect_unify!(inferred, handler.input, state.substitution, path)
    output = Type.apply(handler.output, substitution)

    row =
      [
        Row.union_all(Enum.map(typed_arguments, &effects_of/1)),
        Row.subtract(effects_of(typed_expression), capability.capability),
        handler_clause_row
      ]
      |> Row.union_all()

    typed =
      expression
      |> Map.put(:expression, typed_expression)
      |> Map.put(:arguments, typed_arguments)
      |> Map.put(:handler_evidence, handler)
      |> Map.put(:handler_effect_bindings, handler_effect_bindings)
      |> Map.put(:selected_capability, capability)
      |> Map.put(:type, output)
      |> put_effects(row)

    {typed, output, %{state | substitution: substitution}}
  end

  defp infer(
         %{tag: :resume, resumption: name, path: path} = expression,
         environment,
         state,
         context
       ) do
    resumption =
      Map.get(Map.get(context, :resumptions, %{}), name) ||
        fail("RES001", "unknown resumption #{name}", path)

    {typed_value, inferred, state} =
      infer(expression.value, environment, state, Map.delete(context, :expected))

    substitution = effect_unify!(inferred, resumption.reply, state.substitution, path)
    output = Type.apply(resumption.output, substitution)

    typed =
      expression
      |> Map.put(:value, typed_value)
      |> Map.put(:resumption_evidence, resumption)
      |> Map.put(:type, output)
      |> put_effects(effects_of(typed_value))

    {typed, output, %{state | substitution: substitution}}
  end

  defp infer(%{tag: :construct} = expression, environment, state, context) do
    constructor = Data.resolve_constructor!(context.data, expression.constructor, expression.path)
    {constructor, state} = instantiate_constructor(constructor, state, :construction)

    {typed_arguments, state} =
      infer_constructor_arguments(expression, constructor, environment, state, context)

    typed =
      expression
      |> Map.put(:constructor, constructor)
      |> Map.put(:arguments, typed_arguments)
      |> Map.put(:type, constructor.result)
      |> put_effects(
        typed_arguments
        |> Enum.map(&Map.get(&1, :expression, &1))
        |> Enum.map(&effects_of/1)
        |> Row.union_all()
      )

    {typed, constructor.result, state}
  end

  defp infer(%{tag: :match} = expression, environment, state, context) do
    {typed_scrutinee, scrutinee_type, state} =
      infer(expression.scrutinee, environment, state, Map.delete(context, :expected))

    {match_type, state} =
      case Map.get(context, :expected) do
        nil -> fresh(state)
        expected -> {expected, state}
      end

    {typed_clauses, state} =
      Enum.map_reduce(expression.clauses, state, fn clause, current_state ->
        {typed_pattern, bindings, pattern_state, refinements} =
          infer_pattern(clause.pattern, scrutinee_type, current_state, context.data)

        branch_environment = refine_environment(environment, refinements)

        branch_environment =
          Map.merge(
            branch_environment,
            Map.new(bindings, fn {name, type} -> {name, Scheme.mono(type)} end)
          )

        branch_context =
          context
          |> Map.put(:refinements, refinements)
          |> Map.put(:expected, Type.refine(match_type, refinements))

        {typed_guard, pattern_state} =
          case clause.guard do
            nil ->
              {nil, pattern_state}

            guard ->
              {typed, guard_type, next} =
                infer(
                  guard,
                  branch_environment,
                  pattern_state,
                  Map.delete(branch_context, :expected)
                )

              if not is_nil(Map.get(branch_context, :conditions)) and
                   Type.apply(guard_type, next.substitution) != :boolean do
                fail("CND002", "clause conditions must have type Bool", clause.path <> ".guard")
              end

              substitution =
                Unify.unify(guard_type, :boolean, next.substitution, clause.path <> ".guard")

              typed =
                case Map.get(branch_context, :conditions) do
                  nil ->
                    typed

                  conditions ->
                    evidence = Condition.guard!(typed, conditions, clause.path <> ".guard")
                    Map.put(typed, :condition_evidence, evidence)
                end

              {typed, %{next | substitution: substitution}}
          end

        {typed_body, body_type, branch_state} =
          infer(clause.body, branch_environment, pattern_state, branch_context)

        expected_branch = Type.refine(match_type, refinements)

        rigid =
          bindings
          |> Map.values()
          |> Enum.reduce(MapSet.new(), fn type, variables ->
            MapSet.union(variables, existential_skolems(type))
          end)

        Advanced.assert_no_escape!(body_type, rigid)

        substitution =
          Unify.unify(body_type, expected_branch, branch_state.substitution, clause.path)

        next_state = %{current_state | next: branch_state.next, substitution: substitution}

        {Map.merge(clause, %{
           pattern: typed_pattern,
           guard: typed_guard,
           body: typed_body,
           refinements: refinements
         }), next_state}
      end)

    coverage =
      Coverage.check!(
        typed_clauses,
        Type.apply(scrutinee_type, state.substitution),
        context.data,
        Keyword.put(Map.get(context, :coverage_options, []), :path, expression.path)
      )

    result_type = Type.apply(match_type, state.substitution)

    rigid =
      typed_clauses
      |> Enum.flat_map(fn clause -> Map.values(pattern_bindings(clause.pattern)) end)
      |> Enum.reduce(MapSet.new(), fn type, variables ->
        MapSet.union(variables, existential_skolems(type))
      end)

    Advanced.assert_no_escape!(result_type, rigid)

    typed =
      expression
      |> Map.put(:scrutinee, typed_scrutinee)
      |> Map.put(:clauses, coverage.clauses)
      |> Map.put(:decision_tree, %{
        tag:
          if(is_nil(Map.get(context, :conditions)),
            do: :ordered_decision,
            else: :ordered_guard_tree
          ),
        exhaustive?: true,
        guard_once?: true,
        false_falls_through?: true,
        clauses: coverage.clauses
      })
      |> Map.put(:type, result_type)
      |> put_effects(
        Row.union(
          effects_of(typed_scrutinee),
          typed_clauses
          |> Enum.flat_map(fn clause -> [clause.guard, clause.body] end)
          |> Enum.reject(&is_nil/1)
          |> Enum.map(&effects_of/1)
          |> Row.union_all()
        )
      )

    {typed, typed.type, state}
  end

  defp infer_constructor_arguments(expression, constructor, environment, state, context) do
    case {expression.field_style, constructor.field_style} do
      {:positional, :positional} ->
        if length(expression.arguments) != length(constructor.fields) do
          fail(
            "A003",
            "constructor #{constructor.qualified} has the wrong arity",
            expression.path
          )
        end

        Enum.zip(expression.arguments, constructor.fields)
        |> Enum.map_reduce(state, fn {argument, field}, current ->
          {typed, inferred, next} =
            infer(argument, environment, current, Map.delete(context, :expected))

          substitution = Unify.unify(inferred, field.type, next.substitution, argument.path)
          {Map.put(typed, :field_index, field.index), %{next | substitution: substitution}}
        end)

      {:named, :named} ->
        expected_names = MapSet.new(constructor.fields, & &1.name)
        supplied_names = MapSet.new(expression.arguments, & &1.name)

        if expected_names != supplied_names do
          fail("A003", "named construction must supply every field exactly once", expression.path)
        end

        fields = Map.new(constructor.fields, &{&1.name, &1})

        {source_order, state} =
          Enum.map_reduce(expression.arguments, state, fn argument, current ->
            field = Map.fetch!(fields, argument.name)

            {typed, inferred, next} =
              infer(argument.expression, environment, current, Map.delete(context, :expected))

            substitution = Unify.unify(inferred, field.type, next.substitution, argument.path)

            {%{name: argument.name, field_index: field.index, expression: typed},
             %{next | substitution: substitution}}
          end)

        {source_order, state}

      _ ->
        fail("A003", "constructor field style does not match its declaration", expression.path)
    end
  end

  defp infer_pattern(pattern, expected, state, data) do
    do_infer_pattern(pattern, expected, state, data, %{}, %{})
  end

  defp do_infer_pattern(
         %{tag: :wildcard} = pattern,
         expected,
         state,
         _data,
         bindings,
         refinements
       ),
       do: {Map.put(pattern, :type, expected), bindings, state, refinements}

  defp do_infer_pattern(
         %{tag: :bind, name: name} = pattern,
         expected,
         state,
         _data,
         bindings,
         refinements
       ) do
    bindings = put_binding!(bindings, name, Type.refine(expected, refinements), pattern.path)
    {Map.put(pattern, :type, expected), bindings, state, refinements}
  end

  defp do_infer_pattern(%{tag: :integer} = pattern, expected, state, _data, bindings, refinements) do
    substitution =
      Unify.unify(Type.refine(expected, refinements), :integer, state.substitution, pattern.path)

    {Map.put(pattern, :type, :integer), bindings, %{state | substitution: substitution},
     refinements}
  end

  defp do_infer_pattern(%{tag: :boolean} = pattern, expected, state, _data, bindings, refinements) do
    substitution =
      Unify.unify(Type.refine(expected, refinements), :boolean, state.substitution, pattern.path)

    {Map.put(pattern, :type, :boolean), bindings, %{state | substitution: substitution},
     refinements}
  end

  defp do_infer_pattern(
         %{tag: :tuple, elements: elements} = pattern,
         expected,
         state,
         data,
         bindings,
         refinements
       ) do
    {types, state} = fresh_many(length(elements), state)
    substitution = Unify.unify(expected, {:tuple, types}, state.substitution, pattern.path)

    {typed, bindings, state, refinements} =
      infer_pattern_list(
        elements,
        types,
        %{state | substitution: substitution},
        data,
        bindings,
        refinements
      )

    {%{pattern | elements: typed} |> Map.put(:type, {:tuple, types}), bindings, state,
     refinements}
  end

  defp do_infer_pattern(
         %{tag: :constructor} = pattern,
         expected,
         state,
         data,
         bindings,
         refinements
       ) do
    constructor = Data.resolve_constructor!(data, pattern.constructor, pattern.path)
    {constructor, state} = instantiate_constructor(constructor, state, :pattern)

    {state, refinements} =
      refine_unify(constructor.result, expected, state, refinements, pattern.path)

    pattern_fields = normalize_pattern_fields!(pattern, constructor)
    field_types = Enum.map(constructor.fields, &Type.refine(&1.type, refinements))

    {typed_patterns, bindings, state, refinements} =
      infer_pattern_list(pattern_fields, field_types, state, data, bindings, refinements)

    typed =
      pattern
      |> Map.put(:constructor, constructor)
      |> Map.put(:patterns, typed_patterns)
      |> Map.put(:type, expected)

    {typed, bindings, state, refinements}
  end

  defp do_infer_pattern(
         %{tag: :as, pattern: inner, name: name} = pattern,
         expected,
         state,
         data,
         bindings,
         refinements
       ) do
    {typed, bindings, state, refinements} =
      do_infer_pattern(inner, expected, state, data, bindings, refinements)

    bindings = put_binding!(bindings, name, Type.refine(expected, refinements), pattern.path)
    {%{pattern | pattern: typed} |> Map.put(:type, expected), bindings, state, refinements}
  end

  defp do_infer_pattern(
         %{tag: :or, alternatives: alternatives} = pattern,
         expected,
         state,
         data,
         bindings,
         refinements
       ) do
    {typed, branch_bindings, state, branch_refinements} =
      Enum.reduce(alternatives, {[], nil, state, nil}, fn alternative,
                                                          {typed, prior, current, prior_refs} ->
        {checked, alternative_bindings, next, alternative_refs} =
          do_infer_pattern(alternative, expected, current, data, bindings, refinements)

        if prior && not same_bindings?(prior, alternative_bindings) do
          fail(
            "M003",
            "or-pattern alternatives must bind the same names at the same types",
            pattern.path
          )
        end

        if prior_refs && prior_refs != alternative_refs do
          fail(
            "M003",
            "or-pattern alternatives must establish identical type refinements",
            pattern.path
          )
        end

        {[checked | typed], alternative_bindings, next, alternative_refs}
      end)

    {%{pattern | alternatives: Enum.reverse(typed)} |> Map.put(:type, expected), branch_bindings,
     state, branch_refinements}
  end

  defp infer_pattern_list(patterns, types, state, data, bindings, refinements) do
    Enum.zip(patterns, types)
    |> Enum.reduce({[], bindings, state, refinements}, fn {pattern, type},
                                                          {typed, bindings, current, refs} ->
      {checked, bindings, next, refs} =
        do_infer_pattern(pattern, type, current, data, bindings, refs)

      {[checked | typed], bindings, next, refs}
    end)
    |> then(fn {typed, bindings, state, refs} -> {Enum.reverse(typed), bindings, state, refs} end)
  end

  defp infer_handlers(effects, environment, state, data, global_effects) do
    effects.handlers
    |> Map.values()
    |> Enum.reject(&Map.get(&1, :imported?, false))
    |> Enum.sort_by(& &1.name)
    |> Enum.map_reduce(state, fn handler, current_state ->
      {fresh_variables, current_state} = fresh_many(length(handler.variables), current_state)
      replacements = Enum.zip(handler.variables, fresh_variables) |> Map.new()
      instantiated = instantiate_handler(handler, replacements)

      handler_environment =
        Enum.reduce(instantiated.parameters, environment, fn parameter, current ->
          Map.put(current, parameter.name, Scheme.mono(parameter.parsed_type))
        end)

      base_context = %{
        data: data,
        coverage_options: [],
        conditions: nil,
        effects: effects,
        capabilities: Map.get(instantiated, :uses_capabilities, []),
        global_effects: global_effects,
        effect_values: %{},
        resumptions: %{}
      }

      return_environment =
        Map.put(
          handler_environment,
          instantiated.return_clause.parameter,
          Scheme.mono(instantiated.input)
        )

      {typed_return, return_type, current_state} =
        infer(
          instantiated.return_clause.body,
          return_environment,
          current_state,
          Map.put(base_context, :expected, instantiated.output)
        )

      substitution =
        effect_unify!(
          return_type,
          instantiated.output,
          current_state.substitution,
          instantiated.return_clause.path
        )

      current_state = %{current_state | substitution: substitution}

      {typed_clauses, current_state} =
        Enum.map_reduce(instantiated.operation_clauses, current_state, fn clause, clause_state ->
          family = Enum.find(Map.values(effects.families), &(&1.id == instantiated.family))
          operation = Map.fetch!(family.operations, clause.operation)
          operation = instantiate_operation(operation, family, instantiated.arguments)

          clause_environment =
            Enum.zip(clause.parameters, operation.parameters)
            |> Enum.reduce(handler_environment, fn {name, type}, current ->
              Map.put(current, name, Scheme.mono(type))
            end)

          resumption = %{
            id: "resumption://#{clause.path}",
            name: clause.resumption,
            reply: operation.result,
            output: instantiated.output,
            affine?: true
          }

          clause_context =
            base_context
            |> Map.put(:expected, instantiated.output)
            |> Map.put(:resumptions, %{clause.resumption => resumption})

          {typed_body, body_type, clause_state} =
            infer(clause.body, clause_environment, clause_state, clause_context)

          substitution =
            effect_unify!(body_type, instantiated.output, clause_state.substitution, clause.path)

          expected_row = Map.get(instantiated, :uses_row, Row.empty())

          unless Row.subset?(effects_of(typed_body), expected_row) do
            fail("EFX008", "handler clause effects do not match its uses row", clause.path)
          end

          {Map.merge(clause, %{
             body: typed_body,
             operation_evidence: operation,
             resumption_evidence: resumption
           }), %{clause_state | substitution: substitution}}
        end)

      expected_row = Map.get(instantiated, :uses_row, Row.empty())

      unless Row.subset?(effects_of(typed_return), expected_row) do
        fail(
          "EFX008",
          "handler return effects do not match its uses row",
          instantiated.return_clause.path
        )
      end

      observed_row =
        [effects_of(typed_return) | Enum.map(typed_clauses, &effects_of(&1.body))]
        |> Row.union_all()

      unless Row.matches_declaration?(observed_row, expected_row) do
        fail("EFX008", "handler effects do not match its uses row", instantiated.path)
      end

      typed_handler = %{
        instantiated
        | return_clause: %{instantiated.return_clause | body: typed_return},
          operation_clauses: typed_clauses
      }

      {typed_handler, current_state}
    end)
  end

  defp instantiate_operation(operation, family, arguments) do
    replacements = Enum.zip(family.parameter_ids, arguments) |> Map.new()

    %{
      operation
      | parameters:
          Enum.map(operation.parameters, &Effect.substitute_parameters(&1, replacements)),
        result: Effect.substitute_parameters(operation.result, replacements)
    }
  end

  defp instantiate_handler(handler, replacements) do
    %{
      handler
      | arguments: Enum.map(handler.arguments, &Type.apply(&1, replacements)),
        input: Type.apply(handler.input, replacements),
        output: Type.apply(handler.output, replacements),
        parameters:
          Enum.map(handler.parameters, fn parameter ->
            %{parameter | parsed_type: Type.apply(parameter.parsed_type, replacements)}
          end),
        uses_row:
          handler
          |> Map.get(:uses_row, Row.empty())
          |> apply_row_substitution(replacements)
    }
  end

  defp apply_row_substitution(%Row{} = row, substitution) do
    entries =
      Enum.map(row.entries, fn entry ->
        %{entry | arguments: Enum.map(entry.arguments, &Type.apply(&1, substitution))}
      end)

    Row.new(entries, row.tail)
  end

  defp callee_effects!(callee, context, path, substitution) do
    row =
      callee
      |> Map.get(:latent_effects, Row.empty())
      |> apply_row_substitution(substitution)

    instantiate_call_row!(row, Map.get(context, :capabilities, []), path)
  end

  defp instantiate_call_row!(%Row{} = row, capabilities, path) do
    {entries, bindings} =
      Enum.map_reduce(row.entries, [], fn entry, bindings ->
        candidates =
          Enum.filter(capabilities, fn capability ->
            capability.family == entry.family and
              Enum.map(capability.arguments, &Type.normalize/1) ==
                Enum.map(entry.arguments, &Type.normalize/1)
          end)

        candidates =
          case Map.get(entry, :name) do
            nil -> candidates
            name -> Enum.filter(candidates, &(Map.get(&1, :name) == name))
          end

        case candidates do
          [capability] ->
            {capability,
             [
               %{declared: entry.capability, selected: capability.capability}
               | bindings
             ]}

          [] ->
            fail("EFX004", "call requires an unavailable #{entry.family_name} capability", path)

          many ->
            names = Enum.map_join(many, ", ", &(Map.get(&1, :name) || &1.capability))
            fail("EFX005", "call has ambiguous #{entry.family_name} capabilities: #{names}", path)
        end
      end)

    {Row.new(entries, row.tail), Enum.reverse(bindings)}
  end

  defp put_effects(expression, %Row{} = row),
    do: Map.put(expression, :effects, Row.normalize(row))

  defp effects_of(nil), do: Row.empty()
  defp effects_of(expression), do: Map.get(expression, :effects, Row.empty())

  defp body_effects(%{tag: :function, body: body}), do: body_effects(body)
  defp body_effects(expression), do: effects_of(expression)

  defp effect_unify!(left, right, substitution, path) do
    Unify.unify(left, right, substitution, path)
  rescue
    _error in Catena.TypeError ->
      fail(
        "EFX007",
        "effect request, handler, or resumption types are inconsistent",
        path
      )
  end

  defp normalize_pattern_fields!(pattern, constructor) do
    case {pattern.field_style, constructor.field_style} do
      {:positional, :positional} ->
        if length(pattern.fields) != length(constructor.fields) do
          fail("M003", "constructor pattern has the wrong arity", pattern.path)
        end

        pattern.fields

      {:named, :named} ->
        supplied = Map.new(pattern.fields, &{&1.name, &1.pattern})
        known = MapSet.new(constructor.fields, & &1.name)

        if not MapSet.subset?(MapSet.new(Map.keys(supplied)), known) or
             (not pattern.rest? and map_size(supplied) != length(constructor.fields)) do
          fail(
            "M003",
            "named pattern fields are incomplete or unknown; use rest explicitly",
            pattern.path
          )
        end

        Enum.map(constructor.fields, fn field ->
          Map.get(supplied, field.name, %{tag: :wildcard, path: pattern.path})
        end)

      _ ->
        fail(
          "M003",
          "constructor pattern field style does not match its declaration",
          pattern.path
        )
    end
  end

  defp refine_unify(left, right, state, refinements, path) do
    left = left |> Type.apply(state.substitution) |> Type.refine(refinements)
    right = right |> Type.apply(state.substitution) |> Type.refine(refinements)

    cond do
      left == right ->
        {state, refinements}

      match?({:skolem, _}, left) ->
        bind_refinement(left, right, state, refinements, path)

      match?({:skolem, _}, right) ->
        bind_refinement(right, left, state, refinements, path)

      match?({:nominal, _, _}, left) and match?({:nominal, _, _}, right) ->
        {:nominal, left_id, left_args} = left
        {:nominal, right_id, right_args} = right

        if left_id != right_id or length(left_args) != length(right_args) do
          fail("T002", "incompatible constructor result and scrutinee type", path)
        end

        Enum.zip(left_args, right_args)
        |> Enum.reduce({state, refinements}, fn {l, r}, {current, refs} ->
          refine_unify(l, r, current, refs, path)
        end)

      true ->
        substitution = Unify.unify(left, right, state.substitution, path)
        {%{state | substitution: substitution}, refinements}
    end
  end

  defp bind_refinement({:skolem, {:existential, _, _}}, _type, _state, _refinements, path),
    do: fail("T009", "a rigid constructor existential cannot be solved by its context", path)

  defp bind_refinement({:skolem, id}, type, state, refinements, path) do
    case Map.fetch(refinements, id) do
      {:ok, existing} -> refine_unify(existing, type, state, refinements, path)
      :error -> {state, Map.put(refinements, id, type)}
    end
  end

  defp refine_environment(environment, refinements) do
    Map.new(environment, fn {name, %Scheme{} = scheme} ->
      {name, %{scheme | type: Type.refine(scheme.type, refinements)}}
    end)
  end

  defp instantiate_constructor(constructor, state, mode) do
    {replacements, state} =
      Enum.reduce(constructor.variables, {%{}, state}, fn id, {replacements, current} ->
        if mode == :pattern and MapSet.member?(constructor.existential_ids, id) do
          skolem = {:skolem, {:existential, constructor.id, id, current.next}}
          {Map.put(replacements, id, skolem), %{current | next: current.next + 1}}
        else
          {fresh, next} = fresh(current)
          {Map.put(replacements, id, fresh), next}
        end
      end)

    instantiated = %{
      constructor
      | fields: Enum.map(constructor.fields, &%{&1 | type: Type.apply(&1.type, replacements)}),
        result: Type.apply(constructor.result, replacements),
        variables: [],
        existential_ids: MapSet.new()
    }

    {instantiated, state}
  end

  @spec instantiate(Scheme.t(), state()) :: {Type.t(), state()}
  def instantiate(%Scheme{variables: variables, type: type}, state) do
    {instantiated, _replacements, state} =
      instantiate_with_substitution(%Scheme{variables: variables, type: type}, state)

    {instantiated, state}
  end

  defp instantiate_with_substitution(%Scheme{variables: variables, type: type}, state) do
    {replacements, state} =
      Enum.reduce(variables, {%{}, state}, fn variable, {replacements, state} ->
        {fresh, state} = fresh(state)
        {Map.put(replacements, variable, fresh), state}
      end)

    {Type.apply(type, replacements), replacements, state}
  end

  defp skolemize(%Scheme{variables: variables, type: type}, state) do
    replacements =
      variables
      |> Enum.with_index(state.next)
      |> Map.new(fn {variable, id} -> {variable, {:skolem, id}} end)

    {Type.apply(type, replacements), %{state | next: state.next + length(variables)}}
  end

  defp fresh(state), do: {{:var, state.next}, %{state | next: state.next + 1}}

  defp fresh_many(count, state) do
    Enum.map_reduce(List.duplicate(nil, count), state, fn _, current -> fresh(current) end)
  end

  defp wrap_parameters(definition) do
    Enum.reduce(Enum.reverse(definition.parameters), definition.body, fn parameter, body ->
      %{tag: :function, parameter: parameter, body: body, path: definition.path}
    end)
  end

  defp put_binding!(bindings, name, type, path) do
    if Map.has_key?(bindings, name),
      do: fail("M003", "pattern binds #{name} more than once", path)

    Map.put(bindings, name, type)
  end

  defp same_bindings?(left, right) do
    Map.keys(left) |> Enum.sort() == Map.keys(right) |> Enum.sort() and
      Enum.all?(left, fn {name, type} ->
        Type.normalize(type) == Type.normalize(Map.fetch!(right, name))
      end)
  end

  defp contains_gadt_match?(%{tag: :match, clauses: clauses}, data) do
    Enum.any?(clauses, &gadt_pattern?(&1.pattern, data)) or
      Enum.any?(clauses, &contains_gadt_match?(&1.body, data))
  end

  defp contains_gadt_match?(%{tag: :function, body: body}, data),
    do: contains_gadt_match?(body, data)

  defp contains_gadt_match?(%{tag: :call, callee: callee, arguments: arguments}, data),
    do:
      contains_gadt_match?(callee, data) or Enum.any?(arguments, &contains_gadt_match?(&1, data))

  defp contains_gadt_match?(%{tag: :let, value: value, body: body}, data),
    do: contains_gadt_match?(value, data) or contains_gadt_match?(body, data)

  defp contains_gadt_match?(%{tag: :tuple, elements: elements}, data),
    do: Enum.any?(elements, &contains_gadt_match?(&1, data))

  defp contains_gadt_match?(%{tag: :annotate, expression: expression}, data),
    do: contains_gadt_match?(expression, data)

  defp contains_gadt_match?(%{tag: :unary, operand: operand}, data),
    do: contains_gadt_match?(operand, data)

  defp contains_gadt_match?(%{tag: :binary, left: left, right: right}, data),
    do: contains_gadt_match?(left, data) or contains_gadt_match?(right, data)

  defp contains_gadt_match?(%{tag: :request, arguments: arguments}, data),
    do: Enum.any?(arguments, &contains_gadt_match?(&1, data))

  defp contains_gadt_match?(%{tag: :handle, expression: expression, arguments: arguments}, data),
    do:
      contains_gadt_match?(expression, data) or
        Enum.any?(arguments, &contains_gadt_match?(&1, data))

  defp contains_gadt_match?(%{tag: :resume, value: value}, data),
    do: contains_gadt_match?(value, data)

  defp contains_gadt_match?(_expression, _data), do: false

  defp contains_match?(%{tag: :match}), do: true
  defp contains_match?(%{tag: :function, body: body}), do: contains_match?(body)

  defp contains_match?(%{tag: :call, callee: callee, arguments: arguments}),
    do: contains_match?(callee) or Enum.any?(arguments, &contains_match?/1)

  defp contains_match?(%{tag: :let, value: value, body: body}),
    do: contains_match?(value) or contains_match?(body)

  defp contains_match?(%{tag: :tuple, elements: elements}),
    do: Enum.any?(elements, &contains_match?/1)

  defp contains_match?(%{tag: :annotate, expression: expression}), do: contains_match?(expression)
  defp contains_match?(%{tag: :unary, operand: operand}), do: contains_match?(operand)

  defp contains_match?(%{tag: :binary, left: left, right: right}),
    do: contains_match?(left) or contains_match?(right)

  defp contains_match?(%{tag: :request, arguments: arguments}),
    do: Enum.any?(arguments, &contains_match?/1)

  defp contains_match?(%{tag: :handle, expression: expression, arguments: arguments}),
    do: contains_match?(expression) or Enum.any?(arguments, &contains_match?/1)

  defp contains_match?(%{tag: :resume, value: value}), do: contains_match?(value)

  defp contains_match?(_expression), do: false

  defp gadt_pattern?(%{tag: :constructor, constructor: reference}, data) do
    case Map.get(data.constructors, reference) do
      %{gadt?: value, existential_ids: existentials} ->
        value or MapSet.size(existentials) > 0

      _ ->
        false
    end
  end

  defp gadt_pattern?(%{tag: :tuple, elements: elements}, data),
    do: Enum.any?(elements, &gadt_pattern?(&1, data))

  defp gadt_pattern?(%{tag: :as, pattern: pattern}, data), do: gadt_pattern?(pattern, data)

  defp gadt_pattern?(%{tag: :or, alternatives: alternatives}, data),
    do: Enum.any?(alternatives, &gadt_pattern?(&1, data))

  defp gadt_pattern?(_pattern, _data), do: false

  defp empty_data, do: %{types_by_name: %{}, types_by_id: %{}, constructors: %{}}

  defp pattern_bindings(%{tag: :bind, name: name, type: type}), do: %{name => type}

  defp pattern_bindings(%{tag: :as, pattern: pattern, name: name, type: type}),
    do: Map.put(pattern_bindings(pattern), name, type)

  defp pattern_bindings(%{tag: :tuple, elements: elements}), do: merge_pattern_bindings(elements)

  defp pattern_bindings(%{tag: :constructor, patterns: patterns}),
    do: merge_pattern_bindings(patterns)

  defp pattern_bindings(%{tag: :or, alternatives: [first | _]}),
    do: pattern_bindings(first)

  defp pattern_bindings(_pattern), do: %{}

  defp merge_pattern_bindings(patterns),
    do: Enum.reduce(patterns, %{}, &Map.merge(&2, pattern_bindings(&1)))

  defp existential_skolems({:skolem, {:existential, _, _, _} = id}),
    do: MapSet.new([id])

  defp existential_skolems(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.reduce(MapSet.new(), &MapSet.union(existential_skolems(&1), &2))
  end

  defp existential_skolems(list) when is_list(list),
    do: Enum.reduce(list, MapSet.new(), &MapSet.union(existential_skolems(&1), &2))

  defp existential_skolems(_type), do: MapSet.new()

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
