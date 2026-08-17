defmodule Catena.Kernel.Checker do
  @moduledoc "Integrated type, row, effect, process, and elaboration judgment for kernel 0.1.8."

  alias Catena.{Diagnostic, ImplementationLimits}
  alias Catena.Kernel.Type

  @type state :: %{next: non_neg_integer(), substitution: map()}

  @spec check(map(), keyword()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def check(module, options \\ []) do
    try do
      case ImplementationLimits.validate_source_arities(module) do
        :ok -> :ok
        {:error, diagnostic} -> throw({:kernel_diagnostic, diagnostic})
      end

      imported = imported_processes!(module, Keyword.get(options, :interfaces, []))
      local = local_processes(module)
      processes = Map.merge(imported, local)

      globals =
        Map.new(module.definitions, fn definition ->
          {definition.name,
           {:scheme, definition.variables, definition.signature, definition.uses}}
        end)

      initial = %{next: 0, substitution: %{}}
      semantics = prepare_declarations!(module, globals)

      base_context =
        Map.merge(semantics, %{
          mailbox: nil,
          processes: processes,
          public_processes: MapSet.new(module.exports.processes),
          resumptions: %{}
        })

      {handlers, initial} =
        check_handlers!(module.handlers, globals, base_context, initial)

      base_context = %{base_context | handlers: handlers}

      {definitions, state} =
        Enum.map_reduce(module.definitions, initial, fn definition, state ->
          check_definition!(definition, globals, base_context, state)
        end)

      {typed_processes, state} =
        Enum.map_reduce(module.processes, state, fn process, state ->
          check_process!(process, globals, base_context, state)
        end)

      core = %{
        format: :kernel_core,
        version: "0.1.8",
        frontend_format: "0.1.8",
        frontend_version: "0.1.8",
        edition: module.edition,
        language_revision: module.language_revision,
        previews: module.previews,
        required_previews: [],
        origin: module.origin,
        module: module.module,
        source: module.source,
        span: module.span,
        exports: module.exports,
        imports: module.imports,
        data: semantics.data,
        definitions: definitions,
        processes: typed_processes,
        process_entries: processes,
        traits: semantics.traits,
        instances: semantics.instances,
        effects: semantics.effects,
        handlers: handlers,
        diagnostics: [],
        profile: :formal_semantic_kernel,
        next: state.next
      }

      case ImplementationLimits.validate_source_arities(core) do
        :ok ->
          case Catena.Kernel.Verifier.verify(core) do
            :ok ->
              {:ok, core}

            {:error, reason} ->
              {:error, Diagnostic.new("I001", "kernel-core verification failed: #{reason}")}
          end

        {:error, diagnostic} ->
          {:error, diagnostic}
      end
    catch
      {:kernel_diagnostic, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
    end
  end

  defp prepare_declarations!(module, globals) do
    data = prepare_data!(module.data)
    validate_public_surface!(module, data)

    traits =
      Map.new(module.traits, fn trait ->
        ensure_named_unique!(trait.methods, & &1.name, "trait method", "TRT001", trait.span)

        Enum.each(trait.methods, fn method ->
          validate_known_types!(method.type, data.types, method.span)
          unbound = MapSet.delete(Type.variables(method.type), trait.parameter)

          if MapSet.size(unbound) > 0 do
            fail!("TRT001", "trait method contains an unbound type variable", method.span)
          end
        end)

        {trait.name, %{trait | methods: Map.new(trait.methods, &{&1.name, &1})}}
      end)

    instances =
      Enum.reduce(module.instances, %{}, fn instance, instances ->
        trait =
          case Map.fetch(traits, instance.trait) do
            {:ok, trait} -> trait
            :error -> fail!("TRT001", "instance names an unknown trait", instance.span)
          end

        unless Type.closed?(instance.head) do
          fail!("TRT002", "kernel instance head must be closed", instance.span)
        end

        validate_known_types!(instance.head, data.types, instance.span)

        ensure_named_unique!(
          instance.methods,
          & &1.name,
          "instance method",
          "TRT003",
          instance.span
        )

        expected_names = trait.methods |> Map.keys() |> MapSet.new()
        actual_names = instance.methods |> Enum.map(& &1.name) |> MapSet.new()

        unless expected_names == actual_names do
          fail!("TRT003", "instance methods are incomplete or unknown", instance.span)
        end

        methods =
          Map.new(instance.methods, fn method ->
            implementation =
              case Map.fetch(globals, method.definition) do
                {:ok, {:scheme, _variables, type, _uses}} ->
                  type

                _ ->
                  fail!("TRT003", "trait method implementation is not a definition", method.span)
              end

            declared = trait.methods[method.name].type
            expected = Type.substitute(declared, %{trait.parameter => instance.head})

            unless implementation == expected do
              fail!(
                "TRT003",
                "trait method implementation has an incompatible signature",
                method.span
              )
            end

            {method.name, method.definition}
          end)

        key = {instance.trait, instance.head}

        if Map.has_key?(instances, key) do
          fail!("TRT004", "overlapping kernel trait instances", instance.span)
        end

        Map.put(instances, key, %{instance | methods: methods})
      end)

    effects =
      Map.new(module.effects, fn effect ->
        ensure_named_unique!(
          effect.operations,
          & &1.name,
          "effect operation",
          "EFX001",
          effect.span
        )

        Enum.each(effect.operations, fn operation ->
          Enum.each(operation.parameters, &validate_known_types!(&1, data.types, operation.span))
          validate_known_types!(operation.result, data.types, operation.span)
        end)

        {effect.name, %{effect | operations: Map.new(effect.operations, &{&1.name, &1})}}
      end)

    Enum.each(module.definitions, fn definition ->
      validate_effect_entries!(definition.uses, effects, definition.span)
      validate_type_effects!(definition.signature, effects, definition.span)
      validate_known_types!(definition.signature, data.types, definition.span)
    end)

    Enum.each(module.processes, fn process ->
      Enum.each(process.parameters, &validate_type_effects!(&1.type, effects, &1.span))
      validate_type_effects!(process.mailbox, effects, process.span)
      Enum.each(process.parameters, &validate_known_types!(&1.type, data.types, &1.span))
      validate_known_types!(process.mailbox, data.types, process.span)
    end)

    handlers =
      Map.new(module.handlers, fn handler ->
        unless Map.has_key?(effects, handler.effect) do
          fail!("EFX006", "handler names an unknown effect", handler.span)
        end

        validate_known_types!(handler.input, data.types, handler.span)
        validate_known_types!(handler.output, data.types, handler.span)

        Enum.each(handler.operations, fn operation ->
          Enum.each(operation.parameters, &validate_known_types!(&1.type, data.types, &1.span))
        end)

        {handler.name, handler}
      end)

    %{data: data, traits: traits, instances: instances, effects: effects, handlers: handlers}
  end

  defp prepare_data!(declarations) do
    types = Map.new(declarations, &{&1.name, &1})

    Enum.each(declarations, fn data ->
      parameters = MapSet.new(data.parameters)

      Enum.each(data.constructors, fn constructor ->
        Enum.each(constructor.fields, fn field ->
          validate_known_types!(field, types, constructor.span)

          unless MapSet.subset?(Type.variables(field), parameters) do
            fail!("A002", "constructor field contains an unbound type variable", constructor.span)
          end
        end)
      end)
    end)

    constructors =
      for data <- declarations,
          constructor <- data.constructors,
          into: %{} do
        {constructor.name,
         Map.merge(constructor, %{type_name: data.name, parameters: data.parameters})}
      end

    %{types: types, constructors: constructors}
  end

  defp validate_public_surface!(module, data) do
    public_types = MapSet.new(module.exports.types)

    exposed =
      Enum.flat_map(module.definitions, fn definition ->
        if definition.name in module.exports.values, do: [definition.signature], else: []
      end) ++
        Enum.flat_map(module.processes, fn process ->
          if process.name in module.exports.processes do
            [process.mailbox | Enum.map(process.parameters, & &1.type)]
          else
            []
          end
        end) ++
        Enum.flat_map(module.data, fn declaration ->
          if declaration.name in module.exports.types do
            Enum.flat_map(declaration.constructors, & &1.fields)
          else
            []
          end
        end)

    private_reference =
      exposed
      |> Enum.flat_map(&nominal_names/1)
      |> Enum.find(&(not MapSet.member?(public_types, &1)))

    if private_reference do
      fail!(
        "A002",
        "public kernel signature references unexported type #{private_reference}",
        module.span
      )
    end

    Enum.each(public_types, fn name ->
      unless Map.has_key?(data.types, name) do
        fail!("A002", "exported type #{name} has no data declaration", module.span)
      end
    end)
  end

  defp nominal_names({:nominal, name, arguments}),
    do: [name | Enum.flat_map(arguments, &nominal_names/1)]

  defp nominal_names({:tuple, elements}), do: Enum.flat_map(elements, &nominal_names/1)

  defp nominal_names({:function, parameter, _effects, result}),
    do: nominal_names(parameter) ++ nominal_names(result)

  defp nominal_names({tag, %{fields: fields}}) when tag in [:record, :variant],
    do: fields |> Map.values() |> Enum.flat_map(&nominal_names/1)

  defp nominal_names({:process, mailbox}), do: nominal_names(mailbox)
  defp nominal_names(_type), do: []

  defp check_handlers!(handlers, globals, context, state) do
    Enum.map_reduce(handlers, state, fn handler, state ->
      effect = Map.fetch!(context.effects, handler.effect)

      ensure_named_unique!(
        handler.operations,
        & &1.operation,
        "handler operation",
        "EFX006",
        handler.span
      )

      expected = effect.operations |> Map.keys() |> MapSet.new()
      actual = handler.operations |> Enum.map(& &1.operation) |> MapSet.new()

      unless expected == actual do
        fail!("EFX006", "handler operation clauses are incomplete or unknown", handler.span)
      end

      return_environment =
        Map.put(globals, handler.return.parameter, {:mono, handler.input})

      return_context = %{context | resumptions: %{}}

      {return_body, _type, return_effects, state} =
        infer(
          handler.return.body,
          return_environment,
          return_context,
          state,
          handler.output
        )

      unless return_effects == [] do
        fail!("EFX006", "kernel handler return clause must be effect free", handler.return.span)
      end

      {operations, state} =
        Enum.map_reduce(handler.operations, state, fn clause, state ->
          operation = Map.fetch!(effect.operations, clause.operation)

          if Enum.map(clause.parameters, & &1.type) != operation.parameters do
            fail!("EFX006", "handler operation parameters do not match the effect", clause.span)
          end

          resume_count = count_resumes(clause.body, clause.resumption)

          if resume_count > 1 do
            fail!("RES002", "affine resumption is used more than once", clause.span)
          end

          environment =
            Enum.reduce(clause.parameters, globals, fn parameter, environment ->
              Map.put(environment, parameter.name, {:mono, parameter.type})
            end)

          resumption = %{argument: operation.result, result: handler.output}
          clause_context = %{context | resumptions: %{clause.resumption => resumption}}

          {body, _type, effects, state} =
            infer(clause.body, environment, clause_context, state, handler.output)

          unless effects == [] do
            fail!("EFX006", "kernel handler operation clause must be effect free", clause.span)
          end

          {Map.merge(clause, %{
             body: normalize_expression(body, state),
             resumption_type: resumption
           }), state}
        end)

      typed = %{
        handler
        | return: %{handler.return | body: normalize_expression(return_body, state)},
          operations: Map.new(operations, &{&1.operation, &1})
      }

      {typed, state}
    end)
    |> then(fn {handlers, state} -> {Map.new(handlers, &{&1.name, &1}), state} end)
  end

  defp imported_processes!(module, interfaces) do
    by_module = Map.new(interfaces, &{Map.get(&1, :module), &1})

    Enum.reduce(module.imports, %{}, fn import, entries ->
      case Map.fetch(by_module, import.module) do
        {:ok, %{format: :kernel_interface, digest: digest, processes: processes}}
        when digest == import.digest ->
          Enum.reduce(processes, entries, fn process, acc ->
            entry = %{
              identity: process.identity,
              module: import.module,
              name: process.name,
              parameters: process.parameters,
              mailbox: process.mailbox,
              arity: process.arity,
              spawn_symbol: process.spawn_symbol,
              imported?: true,
              span: import.span
            }

            Map.put(acc, import.module <> "." <> process.name, entry)
          end)

        {:ok, _interface} ->
          fail!(
            "PRC004",
            "imported process interface digest does not match #{import.module}",
            import.span
          )

        :error ->
          fail!("PRC004", "missing process interface for #{import.module}", import.span)
      end
    end)
  end

  defp local_processes(module) do
    Map.new(module.processes, fn process ->
      entry = %{
        identity: module.origin <> "#" <> module.module <> "." <> process.name,
        module: module.module,
        name: process.name,
        parameters: Enum.map(process.parameters, & &1.type),
        mailbox: process.mailbox,
        arity: length(process.parameters),
        spawn_symbol: "__catena_spawn_#{process.name}",
        imported?: false,
        span: process.span
      }

      {process.name, entry}
    end)
  end

  defp check_definition!(definition, globals, context, state) do
    {expression, type, effects, state} =
      infer(definition.expression, globals, context, state, definition.signature)

    type = apply_type(type, state)
    effects = canonical_effects(effects)
    declared = canonical_effects(definition.uses)

    unless type_equal?(type, definition.signature, state) do
      fail!("T002", "definition result does not match its signature", definition.span)
    end

    unless effects == declared do
      fail!(
        "T002",
        "definition effects do not match its uses row",
        definition.span,
        %{actual: encode_effects(effects), declared: encode_effects(declared)}
      )
    end

    {%{
       name: definition.name,
       signature: definition.signature,
       variables: definition.variables,
       uses: declared,
       expression: normalize_expression(expression, state),
       arity: function_arity(definition.signature),
       span: definition.span
     }, state}
  end

  defp check_process!(process, globals, base_context, state) do
    unless Type.closed?(process.mailbox) do
      fail!("PRC001", "process mailbox type must be closed", process.span)
    end

    unless sendable_type?(process.mailbox, base_context.data) do
      fail!("PRC002", "process mailbox type is not sendable", process.span)
    end

    Enum.each(process.parameters, fn parameter ->
      unless Type.closed?(parameter.type) and sendable_type?(parameter.type, base_context.data) do
        fail!(
          "PRC002",
          "process parameter #{parameter.name} is not closed and sendable",
          parameter.span
        )
      end
    end)

    environment =
      Enum.reduce(process.parameters, globals, fn parameter, environment ->
        Map.put(environment, parameter.name, {:mono, parameter.type})
      end)

    context = %{base_context | mailbox: process.mailbox, resumptions: %{}}
    {body, type, effects, state} = infer(process.body, environment, context, state, :unit)
    effects = canonical_effects(effects)

    unless type_equal?(type, :unit, state) do
      fail!("PRC001", "process entry must return Unit", process.span)
    end

    unless Enum.all?(effects, &(&1 == :process)) do
      fail!("PRC003", "process entry leaves an ordinary effect unhandled", process.span)
    end

    {%{
       name: process.name,
       mailbox: process.mailbox,
       parameters: process.parameters,
       body: normalize_expression(body, state),
       effects: effects,
       identity: process_entry!(base_context.processes, process.name, process.span).identity,
       spawn_symbol: "__catena_spawn_#{process.name}",
       public?: MapSet.member?(base_context.public_processes, process.name),
       span: process.span
     }, state}
  end

  defp infer(expression, environment, context, state, expected \\ nil) do
    {typed, type, effects, state} = do_infer(expression, environment, context, state, expected)

    state =
      if is_nil(expected) or type == :bottom do
        state
      else
        unify!(type, expected, state, expression.span)
      end

    type = if type == :bottom and not is_nil(expected), do: expected, else: type

    {Map.merge(typed, %{type: apply_type(type, state), effects: canonical_effects(effects)}),
     type, effects, state}
  end

  defp do_infer(%{tag: :integer} = expression, _environment, _context, state, _expected),
    do: {expression, :integer, [], state}

  defp do_infer(%{tag: :boolean} = expression, _environment, _context, state, _expected),
    do: {expression, :boolean, [], state}

  defp do_infer(%{tag: :unit} = expression, _environment, _context, state, _expected),
    do: {expression, :unit, [], state}

  defp do_infer(
         %{tag: :variable, name: name} = expression,
         environment,
         _context,
         state,
         _expected
       ) do
    case Map.fetch(environment, name) do
      {:ok, {:mono, type}} ->
        {expression, type, [], state}

      {:ok, {:scheme, variables, type, uses}} ->
        {type, state} = instantiate(type, variables, state)
        {expression, type, canonical_effects(uses), state}

      :error ->
        fail!("T001", "unbound value #{name}", expression.span)
    end
  end

  defp do_infer(
         %{tag: :function, parameter: name, parameter_type: parameter, body: body} = expression,
         environment,
         context,
         state,
         expected
       ) do
    expected_result =
      case apply_type(expected, state) do
        {:function, expected_parameter, _effects, result} ->
          state = unify!(parameter, expected_parameter, state, expression.span)
          {result, state}

        _ ->
          {nil, state}
      end

    {expected_result, state} = expected_result
    environment = Map.put(environment, name, {:mono, apply_type(parameter, state)})

    {body, result, body_effects, state} =
      infer(body, environment, context, state, expected_result)

    type =
      {:function, apply_type(parameter, state), canonical_effects(body_effects),
       apply_type(result, state)}

    {%{expression | body: body, parameter_type: apply_type(parameter, state)}, type, [], state}
  end

  defp do_infer(%{tag: :call} = expression, environment, context, state, _expected) do
    {callee, callee_type, effects, state} = infer(expression.callee, environment, context, state)

    {arguments, result_type, effects, state} =
      Enum.reduce(expression.arguments, {[], callee_type, effects, state}, fn argument,
                                                                              {arguments,
                                                                               function_type,
                                                                               effects, state} ->
        case apply_type(function_type, state) do
          {:function, parameter, latent, result} ->
            {argument, _type, argument_effects, state} =
              infer(argument, environment, context, state, parameter)

            {[argument | arguments], result, combine_effects(effects, argument_effects, latent),
             state}

          actual ->
            fail!("T002", "attempted to call non-function type #{inspect(actual)}", argument.span)
        end
      end)

    {%{expression | callee: callee, arguments: Enum.reverse(arguments)}, result_type, effects,
     state}
  end

  defp do_infer(%{tag: :let} = expression, environment, context, state, expected) do
    {value, value_type, value_effects, state} =
      infer(expression.value, environment, context, state)

    {binding, entry, state} =
      generalize_binding(
        value_type,
        environment,
        state,
        expression.value,
        canonical_effects(value_effects)
      )

    environment = Map.put(environment, expression.name, entry)

    {body, body_type, body_effects, state} =
      infer(expression.body, environment, context, state, expected)

    {Map.merge(expression, %{value: value, body: body, binding: binding}), body_type,
     combine_effects(value_effects, body_effects), state}
  end

  defp do_infer(%{tag: :sequence} = expression, environment, context, state, expected) do
    {first, _first_type, first_effects, state} =
      infer(expression.first, environment, context, state)

    {second, type, second_effects, state} =
      infer(expression.second, environment, context, state, expected)

    {%{expression | first: first, second: second}, type,
     combine_effects(first_effects, second_effects), state}
  end

  defp do_infer(%{tag: :tuple} = expression, environment, context, state, expected) do
    expected_elements =
      case apply_type(expected, state) do
        {:tuple, elements} when length(elements) == length(expression.elements) -> elements
        _ -> List.duplicate(nil, length(expression.elements))
      end

    {elements, types, effects, state} =
      expression.elements
      |> Enum.zip(expected_elements)
      |> Enum.reduce({[], [], [], state}, fn {element, expected_element},
                                             {elements, types, effects, state} ->
        {element, type, element_effects, state} =
          infer(element, environment, context, state, expected_element)

        {[element | elements], [type | types], combine_effects(effects, element_effects), state}
      end)

    {%{expression | elements: Enum.reverse(elements)}, {:tuple, Enum.reverse(types)}, effects,
     state}
  end

  defp do_infer(%{tag: :annotate} = expression, environment, context, state, _expected) do
    {annotated, _type, effects, state} =
      infer(expression.expression, environment, context, state, expression.annotation)

    {%{expression | expression: annotated}, expression.annotation, effects, state}
  end

  defp do_infer(
         %{tag: :unary, operator: operator} = expression,
         environment,
         context,
         state,
         _expected
       ) do
    {operand_type, result_type} =
      case operator do
        :not -> {:boolean, :boolean}
        :negate -> {:integer, :integer}
      end

    {operand, _type, effects, state} =
      infer(expression.operand, environment, context, state, operand_type)

    {%{expression | operand: operand}, result_type, effects, state}
  end

  defp do_infer(%{tag: :binary} = expression, environment, context, state, _expected) do
    {left, left_type, left_effects, state} = infer(expression.left, environment, context, state)
    expected_right = binary_operand_type!(expression.operator, left_type, expression.span)
    state = unify!(left_type, expected_right, state, expression.left.span)

    {right, _right_type, right_effects, state} =
      infer(expression.right, environment, context, state, expected_right)

    result_type = binary_result_type(expression.operator)

    {%{expression | left: left, right: right}, result_type,
     combine_effects(left_effects, right_effects), state}
  end

  defp do_infer(%{tag: :record} = expression, environment, context, state, _expected) do
    {fields, types, effects, state} =
      Enum.reduce(expression.fields, {[], %{}, [], state}, fn field,
                                                              {fields, types, effects, state} ->
        {value, type, value_effects, state} =
          infer(field.expression, environment, context, state)

        {[%{field | expression: value} | fields], Map.put(types, field.label, type),
         combine_effects(effects, value_effects), state}
      end)

    {%{expression | fields: Enum.reverse(fields)}, {:record, %{fields: types, tail: nil}},
     effects, state}
  end

  defp do_infer(%{tag: :select} = expression, environment, context, state, _expected) do
    {record, record_type, effects, state} = infer(expression.record, environment, context, state)
    {field_type, _row} = present_field!(record_type, expression.label, state, expression.span)
    {%{expression | record: record}, field_type, effects, state}
  end

  defp do_infer(%{tag: :update} = expression, environment, context, state, _expected) do
    {record, record_type, record_effects, state} =
      infer(expression.record, environment, context, state)

    {field_type, _row} = present_field!(record_type, expression.label, state, expression.span)

    {value, _type, value_effects, state} =
      infer(expression.value, environment, context, state, field_type)

    {%{expression | record: record, value: value}, apply_type(record_type, state),
     combine_effects(record_effects, value_effects), state}
  end

  defp do_infer(%{tag: :extend} = expression, environment, context, state, _expected) do
    {record, record_type, record_effects, state} =
      infer(expression.record, environment, context, state)

    row = record_row!(record_type, state, expression.span)

    if row.tail do
      fail!("T005", "record extension requires a closed row in kernel 0.1.8", expression.span)
    end

    if Map.has_key?(row.fields, expression.label) do
      fail!("T005", "record extension requires an absent field", expression.span)
    end

    {value, value_type, value_effects, state} =
      infer(expression.value, environment, context, state)

    result = {:record, %{row | fields: Map.put(row.fields, expression.label, value_type)}}

    {%{expression | record: record, value: value}, result,
     combine_effects(record_effects, value_effects), state}
  end

  defp do_infer(%{tag: :restrict} = expression, environment, context, state, _expected) do
    {record, record_type, effects, state} = infer(expression.record, environment, context, state)
    {_field_type, row} = present_field!(record_type, expression.label, state, expression.span)
    result = {:record, %{row | fields: Map.delete(row.fields, expression.label)}}
    {%{expression | record: record}, result, effects, state}
  end

  defp do_infer(%{tag: :inject} = expression, environment, context, state, expected) do
    expected_row =
      case apply_type(expected, state) do
        {:variant, row} -> row
        _ -> nil
      end

    expected_payload =
      if expected_row, do: Map.get(expected_row.fields, expression.label), else: nil

    if expected_row && is_nil(expected_payload) do
      fail!("T005", "variant label is absent from the expected row", expression.span)
    end

    {payload, payload_type, effects, state} =
      infer(expression.payload, environment, context, state, expected_payload)

    type =
      if expected_row do
        {:variant, expected_row}
      else
        {:variant, %{fields: %{expression.label => payload_type}, tail: "$inferred"}}
      end

    {%{expression | payload: payload}, type, effects, state}
  end

  defp do_infer(%{tag: :construct} = expression, environment, context, state, _expected) do
    constructor =
      case Map.fetch(context.data.constructors, expression.constructor) do
        {:ok, constructor} -> constructor
        :error -> fail!("A002", "unknown constructor #{expression.constructor}", expression.span)
      end

    if length(expression.arguments) != length(constructor.fields) do
      fail!("A002", "constructor has the wrong argument count", expression.span)
    end

    {type_arguments, state} = fresh_many(length(constructor.parameters), state)
    substitution = constructor.parameters |> Enum.zip(type_arguments) |> Map.new()
    fields = Enum.map(constructor.fields, &Type.substitute(&1, substitution))

    {typed_arguments, effects, state} =
      expression.arguments
      |> Enum.zip(fields)
      |> Enum.reduce({[], [], state}, fn {argument, field}, {arguments, effects, state} ->
        {argument, _type, argument_effects, state} =
          infer(argument, environment, context, state, field)

        {[argument | arguments], combine_effects(effects, argument_effects), state}
      end)

    result = {:nominal, constructor.type_name, type_arguments}

    typed =
      expression
      |> Map.put(:arguments, Enum.reverse(typed_arguments))
      |> Map.put(:selected_constructor, constructor)

    {typed, result, effects, state}
  end

  defp do_infer(%{tag: :match} = expression, environment, context, state, expected) do
    {scrutinee, scrutinee_type, scrutinee_effects, state} =
      infer(expression.scrutinee, environment, context, state)

    {clauses, result_type, clause_effects, state} =
      infer_clauses(
        expression.clauses,
        scrutinee_type,
        environment,
        context,
        state,
        expected
      )

    check_coverage!(scrutinee_type, clauses, state, expression.span, context)

    {%{expression | scrutinee: scrutinee, clauses: clauses}, result_type,
     combine_effects(scrutinee_effects, clause_effects), state}
  end

  defp do_infer(%{tag: :trait_call} = expression, environment, context, state, _expected) do
    trait =
      case Map.fetch(context.traits, expression.trait) do
        {:ok, trait} -> trait
        :error -> fail!("TRT001", "unknown trait #{expression.trait}", expression.span)
      end

    method =
      case Map.fetch(trait.methods, expression.method) do
        {:ok, method} -> method
        :error -> fail!("TRT001", "unknown trait method #{expression.method}", expression.span)
      end

    [first | remaining] = expression.arguments
    {first, head_type, effects, state} = infer(first, environment, context, state)
    head_type = apply_type(head_type, state)

    instance =
      case Map.fetch(context.instances, {trait.name, head_type}) do
        {:ok, instance} ->
          instance

        :error ->
          fail!(
            "TRT005",
            "no coherent trait instance matches the first argument",
            expression.span
          )
      end

    method_type = Type.substitute(method.type, %{trait.parameter => head_type})

    {arguments, result_type, effects, state} =
      infer_applied_arguments(
        [first | remaining],
        method_type,
        environment,
        context,
        state,
        effects,
        first
      )

    typed =
      expression
      |> Map.put(:arguments, arguments)
      |> Map.put(:selected_instance, %{trait: trait.name, head: head_type})
      |> Map.put(:selected_definition, Map.fetch!(instance.methods, expression.method))

    {typed, result_type, effects, state}
  end

  defp do_infer(%{tag: :request} = expression, environment, context, state, _expected) do
    effect =
      case Map.fetch(context.effects, expression.effect) do
        {:ok, effect} -> effect
        :error -> fail!("EFX001", "unknown effect #{expression.effect}", expression.span)
      end

    operation =
      case Map.fetch(effect.operations, expression.operation) do
        {:ok, operation} ->
          operation

        :error ->
          fail!("EFX002", "unknown effect operation #{expression.operation}", expression.span)
      end

    if length(expression.arguments) != length(operation.parameters) do
      fail!("EFX007", "effect request has the wrong argument count", expression.span)
    end

    {arguments, effects, state} =
      expression.arguments
      |> Enum.zip(operation.parameters)
      |> Enum.reduce({[], [], state}, fn {argument, parameter}, {arguments, effects, state} ->
        {argument, _type, argument_effects, state} =
          infer(argument, environment, context, state, parameter)

        {[argument | arguments], combine_effects(effects, argument_effects), state}
      end)

    typed = Map.put(expression, :arguments, Enum.reverse(arguments))
    {typed, operation.result, combine_effects(effects, [{:effect, effect.name}]), state}
  end

  defp do_infer(%{tag: :handle} = expression, environment, context, state, _expected) do
    handler =
      case Map.fetch(context.handlers, expression.handler) do
        {:ok, handler} -> handler
        :error -> fail!("EFX006", "unknown handler #{expression.handler}", expression.span)
      end

    {handled, _type, effects, state} =
      infer(expression.expression, environment, context, state, handler.input)

    occurrence = {:effect, handler.effect}

    case remove_effect(effects, occurrence) do
      {:ok, residual} ->
        {Map.merge(expression, %{expression: handled, selected_handler: handler}), handler.output,
         residual, state}

      :error ->
        fail!(
          "EFX004",
          "handled effect is absent from the expression effect row",
          expression.span
        )
    end
  end

  defp do_infer(%{tag: :resume} = expression, environment, context, state, _expected) do
    resumption =
      case Map.fetch(context.resumptions, expression.resumption) do
        {:ok, resumption} ->
          resumption

        :error ->
          fail!("RES001", "resume is valid only in its handler operation clause", expression.span)
      end

    {value, _type, effects, state} =
      infer(expression.expression, environment, context, state, resumption.argument)

    {Map.merge(expression, %{expression: value, resumption_type: resumption}), resumption.result,
     effects, state}
  end

  defp do_infer(%{tag: :spawn} = expression, environment, context, state, _expected) do
    entry = process_entry!(context.processes, expression.entry, expression.span)

    if length(expression.arguments) != entry.arity do
      fail!("PRC003", "spawn argument count does not match process entry", expression.span)
    end

    {arguments, effects, state} =
      expression.arguments
      |> Enum.zip(entry.parameters)
      |> Enum.reduce({[], [], state}, fn {argument, parameter}, {arguments, effects, state} ->
        {argument, _type, argument_effects, state} =
          infer(argument, environment, context, state, parameter)

        {[argument | arguments], combine_effects(effects, argument_effects), state}
      end)

    typed = %{expression | arguments: Enum.reverse(arguments)} |> Map.put(:selected_entry, entry)
    {typed, {:process, entry.mailbox}, combine_effects(effects, [:process]), state}
  end

  defp do_infer(%{tag: :self} = expression, _environment, context, state, _expected) do
    if is_nil(context.mailbox) do
      fail!("PRC003", "self is valid only inside a process entry", expression.span)
    end

    {expression, {:process, context.mailbox}, [], state}
  end

  defp do_infer(%{tag: :send} = expression, environment, context, state, _expected) do
    {target, target_type, target_effects, state} =
      infer(expression.left, environment, context, state)

    mailbox =
      case apply_type(target_type, state) do
        {:process, mailbox} -> mailbox
        _ -> fail!("PRC003", "send target is not a process handle", expression.left.span)
      end

    {message, _message_type, message_effects, state} =
      infer(expression.right, environment, context, state, mailbox)

    typed =
      Map.merge(expression, %{
        left: target,
        right: message,
        mailbox: apply_type(mailbox, state)
      })

    {typed, :unit, combine_effects(target_effects, message_effects, [:process]), state}
  end

  defp do_infer(%{tag: :receive} = expression, environment, context, state, expected) do
    if is_nil(context.mailbox) do
      fail!("PRC003", "receive is valid only inside a process entry", expression.span)
    end

    {clauses, result_type, effects, state} =
      infer_clauses(expression.clauses, context.mailbox, environment, context, state, expected)

    {Map.merge(expression, %{clauses: clauses, mailbox: context.mailbox}), result_type,
     combine_effects(effects, [:process]), state}
  end

  defp do_infer(%{tag: :trap} = expression, environment, context, state, _expected) do
    {reason, reason_type, effects, state} =
      infer(expression.expression, environment, context, state)

    reason_type = apply_type(reason_type, state)

    unless Type.closed?(reason_type) and sendable_type?(reason_type, context.data) do
      fail!("PRC002", "trap reason must have a closed sendable type", expression.span)
    end

    {Map.merge(expression, %{expression: reason, reason_type: reason_type}), :bottom, effects,
     state}
  end

  defp infer_clauses(clauses, scrutinee_type, environment, context, state, expected) do
    {result_type, state} = if expected, do: {expected, state}, else: fresh(state)

    {clauses, effects, state} =
      Enum.reduce(clauses, {[], [], state}, fn clause, {clauses, effects, state} ->
        {pattern, bindings, state} = infer_pattern(clause.pattern, scrutinee_type, state, context)

        branch_environment =
          Enum.reduce(bindings, environment, fn {name, type}, environment ->
            Map.put(environment, name, {:mono, type})
          end)

        {guard, guard_effects, state} =
          case clause.guard do
            nil ->
              {nil, [], state}

            guard ->
              unless portable_condition?(guard) do
                fail!(
                  "CND001",
                  "clause condition is outside the portable condition core",
                  guard.span
                )
              end

              {guard, _type, guard_effects, state} =
                infer(guard, branch_environment, context, state, :boolean)

              unless guard_effects == [] do
                fail!("CND001", "clause condition must be effect free", guard.span)
              end

              {guard, guard_effects, state}
          end

        {body, _body_type, body_effects, state} =
          infer(clause.body, branch_environment, context, state, result_type)

        typed = %{clause | pattern: pattern, guard: guard, body: body}
        {[typed | clauses], combine_effects(effects, guard_effects, body_effects), state}
      end)

    {Enum.reverse(clauses), apply_type(result_type, state), effects, state}
  end

  defp infer_pattern(pattern, expected, state, context) do
    expected = apply_type(expected, state)

    case pattern do
      %{tag: :wildcard} ->
        {Map.put(pattern, :type, expected), %{}, state}

      %{tag: :bind, name: name} ->
        {Map.put(pattern, :type, expected), %{name => expected}, state}

      %{tag: :integer} ->
        state = unify!(expected, :integer, state, pattern.span)
        {Map.put(pattern, :type, :integer), %{}, state}

      %{tag: :boolean} ->
        state = unify!(expected, :boolean, state, pattern.span)
        {Map.put(pattern, :type, :boolean), %{}, state}

      %{tag: :tuple, elements: elements} ->
        {types, state} = tuple_pattern_types(expected, length(elements), state, pattern.span)

        {elements, bindings, state} =
          elements
          |> Enum.zip(types)
          |> Enum.reduce({[], %{}, state}, fn {element, type}, {elements, bindings, state} ->
            {element, next_bindings, state} = infer_pattern(element, type, state, context)
            {[element | elements], merge_bindings!(bindings, next_bindings, element.span), state}
          end)

        {Map.merge(pattern, %{elements: Enum.reverse(elements), type: {:tuple, types}}), bindings,
         state}

      %{tag: :variant, label: label, pattern: payload} ->
        row = variant_row!(expected, state, pattern.span)

        case Map.fetch(row.fields, label) do
          {:ok, payload_type} ->
            {payload, bindings, state} = infer_pattern(payload, payload_type, state, context)
            {Map.merge(pattern, %{pattern: payload, type: {:variant, row}}), bindings, state}

          :error ->
            fail!("M003", "variant pattern label is absent from the row", pattern.span)
        end

      %{tag: :constructor, constructor: name, patterns: patterns} ->
        constructor =
          case Map.fetch(context.data.constructors, name) do
            {:ok, constructor} -> constructor
            :error -> fail!("A002", "unknown constructor #{name}", pattern.span)
          end

        if length(patterns) != length(constructor.fields) do
          fail!("M003", "constructor pattern has the wrong arity", pattern.span)
        end

        {type_arguments, state} = fresh_many(length(constructor.parameters), state)
        result = {:nominal, constructor.type_name, type_arguments}
        state = unify!(expected, result, state, pattern.span)
        substitution = constructor.parameters |> Enum.zip(type_arguments) |> Map.new()
        fields = Enum.map(constructor.fields, &Type.substitute(&1, substitution))

        {patterns, bindings, state} =
          patterns
          |> Enum.zip(fields)
          |> Enum.reduce({[], %{}, state}, fn {field_pattern, field_type},
                                              {patterns, bindings, state} ->
            {field_pattern, next_bindings, state} =
              infer_pattern(field_pattern, field_type, state, context)

            {[field_pattern | patterns],
             merge_bindings!(bindings, next_bindings, field_pattern.span), state}
          end)

        typed =
          pattern
          |> Map.put(:patterns, Enum.reverse(patterns))
          |> Map.put(:selected_constructor, constructor)
          |> Map.put(:type, apply_type(result, state))

        {typed, bindings, state}

      %{tag: :as, pattern: inner, name: name} ->
        {inner, bindings, state} = infer_pattern(inner, expected, state, context)
        bindings = merge_bindings!(bindings, %{name => expected}, pattern.span)
        {Map.merge(pattern, %{pattern: inner, type: expected}), bindings, state}

      %{tag: :or, alternatives: alternatives} ->
        {alternatives, binding_sets, state} =
          Enum.reduce(alternatives, {[], [], state}, fn alternative,
                                                        {alternatives, binding_sets, state} ->
            {alternative, bindings, state} =
              infer_pattern(alternative, expected, state, context)

            {[alternative | alternatives], [bindings | binding_sets], state}
          end)

        [first | rest] = Enum.reverse(binding_sets)

        unless Enum.all?(rest, &(Map.keys(&1) |> Enum.sort() == Map.keys(first) |> Enum.sort())) do
          fail!("M003", "or-pattern alternatives bind different names", pattern.span)
        end

        Enum.each(rest, fn bindings ->
          Enum.each(first, fn {name, type} ->
            unless type_equal?(type, Map.fetch!(bindings, name), state) do
              fail!("M003", "or-pattern alternatives bind incompatible types", pattern.span)
            end
          end)
        end)

        {Map.merge(pattern, %{alternatives: Enum.reverse(alternatives), type: expected}), first,
         state}
    end
  end

  defp check_coverage!(scrutinee_type, clauses, state, span, context) do
    type = apply_type(scrutinee_type, state)
    wildcard? = Enum.any?(clauses, &(is_nil(&1.guard) and catch_all_pattern?(&1.pattern)))

    covered =
      clauses
      |> Enum.filter(&is_nil(&1.guard))
      |> Enum.flat_map(&covered_heads(&1.pattern))
      |> MapSet.new()

    exhaustive? =
      cond do
        wildcard? ->
          true

        match?({:variant, %{tail: nil}}, type) ->
          MapSet.equal?(covered, MapSet.new(Map.keys(elem(type, 1).fields)))

        type == :boolean ->
          MapSet.equal?(covered, MapSet.new([true, false]))

        match?({:nominal, _, _}, type) ->
          {:nominal, name, _arguments} = type

          case Map.fetch(context.data.types, name) do
            {:ok, data} ->
              constructors = data.constructors |> Enum.map(& &1.name) |> MapSet.new()
              MapSet.equal?(covered, constructors)

            :error ->
              false
          end

        true ->
          false
      end

    unless exhaustive?, do: fail!("M001", "match is not exhaustive", span)
  end

  defp covered_heads(%{tag: :variant, label: label, pattern: pattern}) do
    if irrefutable_pattern?(pattern), do: [label], else: []
  end

  defp covered_heads(%{tag: :boolean, value: value}), do: [value]

  defp covered_heads(%{tag: :constructor, constructor: constructor, patterns: patterns}) do
    if Enum.all?(patterns, &irrefutable_pattern?/1), do: [constructor], else: []
  end

  defp covered_heads(%{tag: :as, pattern: pattern}), do: covered_heads(pattern)

  defp covered_heads(%{tag: :or, alternatives: alternatives}),
    do: Enum.flat_map(alternatives, &covered_heads/1)

  defp covered_heads(_pattern), do: []

  defp catch_all_pattern?(%{tag: tag}) when tag in [:wildcard, :bind], do: true
  defp catch_all_pattern?(%{tag: :as, pattern: pattern}), do: catch_all_pattern?(pattern)

  defp catch_all_pattern?(%{tag: :or, alternatives: alternatives}),
    do: Enum.any?(alternatives, &catch_all_pattern?/1)

  defp catch_all_pattern?(_pattern), do: false

  defp irrefutable_pattern?(%{tag: tag}) when tag in [:wildcard, :bind], do: true

  defp irrefutable_pattern?(%{tag: :tuple, elements: elements}),
    do: Enum.all?(elements, &irrefutable_pattern?/1)

  defp irrefutable_pattern?(%{tag: :as, pattern: pattern}),
    do: irrefutable_pattern?(pattern)

  defp irrefutable_pattern?(%{tag: :or, alternatives: alternatives}),
    do: Enum.any?(alternatives, &irrefutable_pattern?/1)

  defp irrefutable_pattern?(_pattern), do: false

  defp tuple_pattern_types({:tuple, types}, arity, state, _span) when length(types) == arity,
    do: {types, state}

  defp tuple_pattern_types({:inference, _id} = expected, arity, state, span) do
    {types, state} = fresh_many(arity, state)
    state = unify!(expected, {:tuple, types}, state, span)
    {types, state}
  end

  defp tuple_pattern_types(_expected, _arity, _state, span),
    do: fail!("M003", "tuple pattern does not match the scrutinee type", span)

  defp variant_row!({:variant, row}, _state, _span), do: row

  defp variant_row!(_type, _state, span),
    do: fail!("M003", "variant pattern requires a variant scrutinee", span)

  defp merge_bindings!(left, right, span) do
    duplicate = Map.keys(left) |> Enum.find(&Map.has_key?(right, &1))
    if duplicate, do: fail!("M003", "pattern binds #{duplicate} more than once", span)
    Map.merge(left, right)
  end

  defp present_field!(type, label, state, span) do
    row = record_row!(type, state, span)

    case Map.fetch(row.fields, label) do
      {:ok, field_type} -> {field_type, row}
      :error -> fail!("T005", "record field #{label} is absent", span)
    end
  end

  defp record_row!(type, state, span) do
    case apply_type(type, state) do
      {:record, row} -> row
      _ -> fail!("T005", "record operation requires a record", span)
    end
  end

  defp binary_operand_type!(operator, left_type, span) do
    cond do
      operator in [:and, :or] ->
        :boolean

      operator in [:add, :subtract, :multiply, :less, :less_equal, :greater, :greater_equal] ->
        :integer

      operator in [:equal, :not_equal] and left_type in [:integer, :boolean] ->
        left_type

      operator in [:equal, :not_equal] ->
        fail!("T002", "kernel equality accepts only Int or Bool", span)
    end
  end

  defp binary_result_type(operator)
       when operator in [
              :and,
              :or,
              :equal,
              :not_equal,
              :less,
              :less_equal,
              :greater,
              :greater_equal
            ],
       do: :boolean

  defp binary_result_type(operator) when operator in [:add, :subtract, :multiply], do: :integer

  defp process_entry!(processes, name, span) do
    case Map.fetch(processes, name) do
      {:ok, entry} -> entry
      :error -> fail!("PRC004", "unknown process entry #{name}", span)
    end
  end

  defp infer_applied_arguments(
         [first | remaining],
         method_type,
         environment,
         context,
         state,
         effects,
         _typed_first
       ) do
    case apply_type(method_type, state) do
      {:function, parameter, latent, result} ->
        state = unify!(first.type, parameter, state, first.span)

        Enum.reduce(
          remaining,
          {[first], result, combine_effects(effects, latent), state},
          fn argument, {arguments, function_type, effects, state} ->
            case apply_type(function_type, state) do
              {:function, parameter, latent, result} ->
                {argument, _type, argument_effects, state} =
                  infer(argument, environment, context, state, parameter)

                {arguments ++ [argument], result,
                 combine_effects(effects, argument_effects, latent), state}

              _actual ->
                fail!("TRT003", "trait method received too many arguments", argument.span)
            end
          end
        )

      _actual ->
        fail!("TRT003", "trait method is not callable", first.span)
    end
  end

  defp instantiate(type, variables, state) do
    {substitution, state} =
      Enum.reduce(variables, {%{}, state}, fn variable, {substitution, state} ->
        {fresh, state} = fresh(state)
        {Map.put(substitution, variable, fresh), state}
      end)

    {Type.substitute(type, substitution), state}
  end

  defp generalize_binding(type, environment, state, expression, effects) do
    type = apply_type(type, state)

    if effects == [] and (non_expansive?(expression) or not effect_control?(expression)) do
      environment_inference_variables =
        environment
        |> Map.values()
        |> Enum.flat_map(fn
          {:mono, environment_type} ->
            inference_variables(apply_type(environment_type, state))

          {:scheme, _variables, environment_type, _uses} ->
            inference_variables(apply_type(environment_type, state))
        end)
        |> MapSet.new()

      generalized_inference_variables =
        type
        |> inference_variables()
        |> MapSet.new()
        |> MapSet.difference(environment_inference_variables)
        |> MapSet.to_list()
        |> Enum.sort()

      generated_variables = Enum.map(generalized_inference_variables, &"$let#{&1}")

      state =
        generalized_inference_variables
        |> Enum.zip(generated_variables)
        |> Enum.reduce(state, fn {id, variable}, state ->
          %{state | substitution: Map.put(state.substitution, id, {:variable, variable})}
        end)

      type = apply_type(type, state)
      environment_type_variables = environment_type_variables(environment, state)

      named_variables =
        type
        |> Type.variables()
        |> MapSet.difference(environment_type_variables)
        |> MapSet.to_list()
        |> Enum.sort()

      variables = Enum.sort(Enum.uniq(named_variables ++ generated_variables))
      binding = %{variables: variables, type: type}
      {binding, {:scheme, variables, type, []}, state}
    else
      binding = %{variables: [], type: type}
      {binding, {:mono, type}, state}
    end
  end

  defp inference_variables({:inference, id}), do: [id]

  defp inference_variables({:tuple, elements}),
    do: Enum.flat_map(elements, &inference_variables/1)

  defp inference_variables({:function, parameter, _effects, result}),
    do: inference_variables(parameter) ++ inference_variables(result)

  defp inference_variables({tag, %{fields: fields}}) when tag in [:record, :variant],
    do: fields |> Map.values() |> Enum.flat_map(&inference_variables/1)

  defp inference_variables({:process, mailbox}), do: inference_variables(mailbox)

  defp inference_variables({:nominal, _name, arguments}),
    do: Enum.flat_map(arguments, &inference_variables/1)

  defp inference_variables(_type), do: []

  defp environment_type_variables(environment, state) do
    environment
    |> Map.values()
    |> Enum.reduce(MapSet.new(), fn
      {:mono, type}, variables ->
        MapSet.union(variables, Type.variables(apply_type(type, state)))

      {:scheme, bound, type, _uses}, variables ->
        free =
          type
          |> apply_type(state)
          |> Type.variables()
          |> MapSet.difference(MapSet.new(bound))

        MapSet.union(variables, free)
    end)
  end

  defp non_expansive?(%{tag: tag}) when tag in [:integer, :boolean, :unit, :variable, :function],
    do: true

  defp non_expansive?(%{tag: :tuple, elements: elements}),
    do: Enum.all?(elements, &non_expansive?/1)

  defp non_expansive?(%{tag: :record, fields: fields}),
    do: Enum.all?(fields, &non_expansive?(&1.expression))

  defp non_expansive?(%{tag: :construct, arguments: arguments}),
    do: Enum.all?(arguments, &non_expansive?/1)

  defp non_expansive?(%{tag: :annotate, expression: expression}),
    do: non_expansive?(expression)

  defp non_expansive?(_expression), do: false

  defp effect_control?(%{tag: tag}) when tag in [:request, :handle, :resume], do: true
  defp effect_control?(%Catena.SourceSpan{}), do: false

  defp effect_control?(%{} = value),
    do: value |> Map.values() |> Enum.any?(&effect_control?/1)

  defp effect_control?(values) when is_list(values), do: Enum.any?(values, &effect_control?/1)
  defp effect_control?(_value), do: false

  defp fresh(state), do: {{:inference, state.next}, %{state | next: state.next + 1}}

  defp fresh_many(count, state) do
    Enum.map_reduce(1..count//1, state, fn _index, state -> fresh(state) end)
  end

  defp unify!(left, right, state, span) do
    left = apply_type(left, state)
    right = apply_type(right, state)

    cond do
      left == right ->
        state

      left == :bottom or right == :bottom ->
        state

      match?({:inference, _}, left) ->
        bind_inference!(left, right, state, span)

      match?({:inference, _}, right) ->
        bind_inference!(right, left, state, span)

      match?({:tuple, _}, left) and match?({:tuple, _}, right) ->
        unify_tuples!(left, right, state, span)

      match?({:function, _, _, _}, left) and match?({:function, _, _, _}, right) ->
        unify_functions!(left, right, state, span)

      match?({:process, _}, left) and match?({:process, _}, right) ->
        unify!(elem(left, 1), elem(right, 1), state, span)

      row_type?(left) and row_type?(right) ->
        unify_rows!(left, right, state, span)

      match?({:nominal, _, _}, left) and match?({:nominal, _, _}, right) ->
        unify_nominal!(left, right, state, span)

      true ->
        fail!("T002", "incompatible kernel types", span, %{
          left: inspect(left),
          right: inspect(right)
        })
    end
  end

  defp bind_inference!({:inference, id}, type, state, span) do
    if occurs?(id, type, state) do
      fail!("T003", "infinite type in kernel inference", span)
    end

    %{state | substitution: Map.put(state.substitution, id, type)}
  end

  defp occurs?(id, type, state) do
    case apply_type(type, state) do
      {:inference, ^id} ->
        true

      {:tuple, elements} ->
        Enum.any?(elements, &occurs?(id, &1, state))

      {:function, parameter, _effects, result} ->
        occurs?(id, parameter, state) or occurs?(id, result, state)

      {tag, %{fields: fields}} when tag in [:record, :variant] ->
        Enum.any?(fields, fn {_label, field_type} -> occurs?(id, field_type, state) end)

      {:process, mailbox} ->
        occurs?(id, mailbox, state)

      {:nominal, _name, arguments} ->
        Enum.any?(arguments, &occurs?(id, &1, state))

      _ ->
        false
    end
  end

  defp unify_tuples!({:tuple, left}, {:tuple, right}, state, span) do
    if length(left) != length(right), do: fail!("T002", "tuple arity mismatch", span)

    Enum.zip(left, right)
    |> Enum.reduce(state, fn {left, right}, state -> unify!(left, right, state, span) end)
  end

  defp unify_functions!({:function, lp, le, lr}, {:function, rp, re, rr}, state, span) do
    unless canonical_effects(le) == canonical_effects(re) do
      fail!("T002", "function effect rows are incompatible", span)
    end

    state |> then(&unify!(lp, rp, &1, span)) |> then(&unify!(lr, rr, &1, span))
  end

  defp unify_rows!({tag, left}, {tag, right}, state, span) do
    left_labels = MapSet.new(Map.keys(left.fields))
    right_labels = MapSet.new(Map.keys(right.fields))
    common = MapSet.intersection(left_labels, right_labels)

    cond do
      is_nil(left.tail) and is_nil(right.tail) and left_labels != right_labels ->
        fail!("T005", "closed rows have different labels", span)

      is_nil(left.tail) and not MapSet.subset?(right_labels, left_labels) ->
        fail!("T005", "row contains a label absent from the closed row", span)

      is_nil(right.tail) and not MapSet.subset?(left_labels, right_labels) ->
        fail!("T005", "row contains a label absent from the closed row", span)

      true ->
        Enum.reduce(common, state, fn label, state ->
          unify!(Map.fetch!(left.fields, label), Map.fetch!(right.fields, label), state, span)
        end)
    end
  end

  defp unify_rows!(_left, _right, _state, span),
    do: fail!("T005", "record and variant rows are distinct", span)

  defp unify_nominal!({:nominal, name, left}, {:nominal, name, right}, state, span) do
    if length(left) != length(right), do: fail!("T002", "nominal type arity mismatch", span)

    Enum.zip(left, right)
    |> Enum.reduce(state, fn {left, right}, state -> unify!(left, right, state, span) end)
  end

  defp unify_nominal!(_left, _right, _state, span),
    do: fail!("T002", "nominal types are distinct", span)

  defp apply_type(nil, _state), do: nil

  defp apply_type({:inference, id} = type, state) do
    case Map.fetch(state.substitution, id) do
      {:ok, replacement} -> apply_type(replacement, state)
      :error -> type
    end
  end

  defp apply_type({:tuple, elements}, state),
    do: {:tuple, Enum.map(elements, &apply_type(&1, state))}

  defp apply_type({:function, parameter, effects, result}, state),
    do:
      {:function, apply_type(parameter, state), canonical_effects(effects),
       apply_type(result, state)}

  defp apply_type({tag, %{fields: fields} = row}, state) when tag in [:record, :variant] do
    {tag,
     %{row | fields: Map.new(fields, fn {label, type} -> {label, apply_type(type, state)} end)}}
  end

  defp apply_type({:process, mailbox}, state), do: {:process, apply_type(mailbox, state)}

  defp apply_type({:nominal, name, arguments}, state),
    do: {:nominal, name, Enum.map(arguments, &apply_type(&1, state))}

  defp apply_type(type, _state), do: type

  defp type_equal?(left, right, state), do: apply_type(left, state) == apply_type(right, state)
  defp row_type?({tag, %{}}) when tag in [:record, :variant], do: true
  defp row_type?(_type), do: false

  defp canonical_effects(effects) do
    process = if :process in effects, do: [:process], else: []
    ordinary = effects |> Enum.reject(&(&1 == :process)) |> Enum.sort_by(&inspect/1)
    process ++ ordinary
  end

  defp combine_effects(effect_lists), do: effect_lists |> List.flatten() |> canonical_effects()
  defp combine_effects(first, second), do: combine_effects([first, second])
  defp combine_effects(first, second, third), do: combine_effects([first, second, third])

  defp encode_effects(effects) do
    Enum.map(effects, fn
      :process -> "Process"
      {:effect, name} -> name
    end)
  end

  defp normalize_expression(%Catena.SourceSpan{} = span, _state), do: span

  defp normalize_expression(expression, state) when is_map(expression) do
    Map.new(expression, fn
      {:type, type} -> {:type, apply_type(type, state)}
      {key, value} -> {key, normalize_expression(value, state)}
    end)
  end

  defp normalize_expression(values, state) when is_list(values),
    do: Enum.map(values, &normalize_expression(&1, state))

  defp normalize_expression(value, _state), do: value

  defp function_arity({:function, _parameter, _effects, result}), do: 1 + function_arity(result)
  defp function_arity(_type), do: 0

  defp remove_effect(effects, occurrence) do
    case Enum.split_while(effects, &(&1 != occurrence)) do
      {_before, []} -> :error
      {before, [_occurrence | after_effects]} -> {:ok, before ++ after_effects}
    end
  end

  defp validate_effect_entries!(entries, effects, span) do
    Enum.each(entries, fn
      :process ->
        :ok

      {:effect, name} ->
        unless Map.has_key?(effects, name) do
          fail!("EFX001", "unknown effect #{name} in effect row", span)
        end
    end)
  end

  defp validate_type_effects!({:function, parameter, entries, result}, effects, span) do
    validate_effect_entries!(entries, effects, span)
    validate_type_effects!(parameter, effects, span)
    validate_type_effects!(result, effects, span)
  end

  defp validate_type_effects!({:tuple, elements}, effects, span),
    do: Enum.each(elements, &validate_type_effects!(&1, effects, span))

  defp validate_type_effects!({tag, %{fields: fields}}, effects, span)
       when tag in [:record, :variant],
       do:
         Enum.each(fields, fn {_label, type} ->
           validate_type_effects!(type, effects, span)
         end)

  defp validate_type_effects!({:process, mailbox}, effects, span),
    do: validate_type_effects!(mailbox, effects, span)

  defp validate_type_effects!({:nominal, _name, arguments}, effects, span),
    do: Enum.each(arguments, &validate_type_effects!(&1, effects, span))

  defp validate_type_effects!(_type, _effects, _span), do: :ok

  defp validate_known_types!(type, types, span) do
    case type do
      primitive when primitive in [:integer, :boolean, :unit, :bottom] ->
        :ok

      {:variable, _name} ->
        :ok

      {:inference, _id} ->
        :ok

      {:tuple, elements} ->
        Enum.each(elements, &validate_known_types!(&1, types, span))

      {:function, parameter, _effects, result} ->
        validate_known_types!(parameter, types, span)
        validate_known_types!(result, types, span)

      {tag, %{fields: fields}} when tag in [:record, :variant] ->
        Enum.each(fields, fn {_label, field_type} ->
          validate_known_types!(field_type, types, span)
        end)

      {:process, mailbox} ->
        validate_known_types!(mailbox, types, span)

      {:nominal, name, arguments} ->
        case Map.fetch(types, name) do
          {:ok, data} when length(arguments) == length(data.parameters) ->
            Enum.each(arguments, &validate_known_types!(&1, types, span))

          {:ok, _data} ->
            fail!("A002", "nominal type #{name} has the wrong arity", span)

          :error ->
            fail!("A002", "unknown nominal type #{name}", span)
        end
    end
  end

  defp sendable_type?(type, data), do: sendable_type?(type, data, MapSet.new())

  defp sendable_type?(type, _data, _seen) when type in [:integer, :boolean, :unit], do: true

  defp sendable_type?({:tuple, elements}, data, seen),
    do: Enum.all?(elements, &sendable_type?(&1, data, seen))

  defp sendable_type?({tag, %{fields: fields, tail: nil}}, data, seen)
       when tag in [:record, :variant],
       do: Enum.all?(fields, fn {_label, field} -> sendable_type?(field, data, seen) end)

  defp sendable_type?({:process, mailbox}, data, seen),
    do: Type.closed?(mailbox) and sendable_type?(mailbox, data, seen)

  defp sendable_type?({:nominal, name, arguments} = type, data, seen) do
    if MapSet.member?(seen, type) do
      true
    else
      with {:ok, declaration} <- Map.fetch(data.types, name),
           true <- length(arguments) == length(declaration.parameters) do
        substitution = declaration.parameters |> Enum.zip(arguments) |> Map.new()
        seen = MapSet.put(seen, type)

        Enum.all?(declaration.constructors, fn constructor ->
          Enum.all?(constructor.fields, fn field ->
            field |> Type.substitute(substitution) |> sendable_type?(data, seen)
          end)
        end)
      else
        _ -> false
      end
    end
  end

  defp sendable_type?(_type, _data, _seen), do: false

  defp ensure_named_unique!(values, key, label, id, span) do
    keys = Enum.map(values, key)

    if length(keys) != length(Enum.uniq(keys)) do
      fail!(id, "duplicate #{label}", span)
    end
  end

  defp count_resumes(%{tag: :resume, resumption: name, expression: expression}, name),
    do: 1 + count_resumes(expression, name)

  defp count_resumes(%Catena.SourceSpan{}, _name), do: 0

  defp count_resumes(%{} = value, name),
    do: value |> Map.values() |> Enum.map(&count_resumes(&1, name)) |> Enum.sum()

  defp count_resumes(values, name) when is_list(values),
    do: values |> Enum.map(&count_resumes(&1, name)) |> Enum.sum()

  defp count_resumes(_value, _name), do: 0

  defp portable_condition?(%{tag: tag}) when tag in [:integer, :boolean, :variable], do: true
  defp portable_condition?(%{tag: :unary, operand: operand}), do: portable_condition?(operand)

  defp portable_condition?(%{tag: :binary, left: left, right: right}),
    do: portable_condition?(left) and portable_condition?(right)

  defp portable_condition?(%{tag: :tuple, elements: elements}),
    do: Enum.all?(elements, &portable_condition?/1)

  defp portable_condition?(%{tag: :annotate, expression: expression}),
    do: portable_condition?(expression)

  defp portable_condition?(_expression), do: false

  defp fail!(id, message, span, details \\ %{}) do
    throw({:kernel_diagnostic, Diagnostic.new(id, message, span: span, details: details)})
  end
end
