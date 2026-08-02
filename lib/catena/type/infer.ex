defmodule Catena.Type.Infer do
  @moduledoc "Algorithm W plus the annotation-directed C002 datatype boundary."

  alias Catena.{Categorical, Condition, Data, Derive, Diagnostic, Type}
  alias Catena.Pattern.Coverage
  alias Catena.Type.{Advanced, Parser, Scheme, Unify}

  @type state :: %{next: non_neg_integer(), substitution: map()}

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
    derived = Derive.folds(data) ++ Derive.capabilities(data, categorical.derivations)

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
            if(ast.frontend_version in ~w(0.3 0.4), do: conditions, else: nil)
          )

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
          path: definition.path
        }

        {[core_definition | definitions], Map.put(environment, definition.name, scheme), state}
      end)

    definitions = Enum.reverse(definitions) ++ derived
    environment = Enum.reduce(derived, environment, &Map.put(&2, &1.name, &1.scheme))

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
        conditions: Keyword.get(options, :conditions)
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
        expected: result_type,
        signed?: true
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
    do: {Map.put(expression, :type, :integer), :integer, state}

  defp infer(%{tag: :boolean} = expression, _environment, state, _context),
    do: {Map.put(expression, :type, :boolean), :boolean, state}

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

    typed = expression |> Map.put(:operand, typed_operand) |> Map.put(:type, result)
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

    {typed, result_type, state}
  end

  defp infer(%{tag: :variable, name: name, path: path} = expression, environment, state, context) do
    case Map.fetch(environment, name) do
      {:ok, scheme} ->
        {type, state} = instantiate(scheme, state)
        type = Type.refine(type, Map.get(context, :refinements, %{}))
        {Map.put(expression, :type, type), type, state}

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
    {parameter_type, state} = fresh(state)
    local_environment = Map.put(environment, parameter, Scheme.mono(parameter_type))

    {typed_body, body_type, state} =
      infer(body, local_environment, state, Map.delete(context, :expected))

    type = {:function, Type.apply(parameter_type, state.substitution), body_type}
    {expression |> Map.put(:body, typed_body) |> Map.put(:type, type), type, state}
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

    typed =
      expression
      |> Map.put(:callee, typed_callee)
      |> Map.put(:arguments, Enum.reverse(typed_arguments))
      |> Map.put(:type, result_type)

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

    {typed_body, body_type, state} =
      infer(body, Map.put(environment, name, scheme), state, context)

    typed =
      expression
      |> Map.put(:value, typed_value)
      |> Map.put(:body, typed_body)
      |> Map.put(:scheme, scheme)
      |> Map.put(:type, body_type)

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

    {expression |> Map.put(:elements, Enum.reverse(typed_elements)) |> Map.put(:type, type), type,
     state}
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

    {expression |> Map.put(:expression, typed) |> Map.put(:type, type), type,
     %{state | substitution: substitution}}
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
    {replacements, state} =
      Enum.reduce(variables, {%{}, state}, fn variable, {replacements, state} ->
        {fresh, state} = fresh(state)
        {Map.put(replacements, variable, fresh), state}
      end)

    {Type.apply(type, replacements), state}
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
