defmodule Catena.Kernel.Verifier do
  @moduledoc "Inference-independent verifier for the typed 0.1.8 semantic kernel."

  alias Catena.Kernel.Type

  @spec verify(map()) :: :ok | {:error, String.t()}
  def verify(%{format: :kernel_core, version: "0.1.8"} = core) do
    with :ok <- verify_data(core),
         :ok <- verify_exports(core),
         :ok <- verify_process_index(core),
         :ok <- verify_handlers(core),
         :ok <- verify_definitions(core),
         :ok <- verify_processes(core) do
      :ok
    end
  rescue
    _error -> {:error, "malformed kernel-core evidence"}
  end

  def verify(_core), do: {:error, "wrong kernel-core format or version"}

  defp verify_data(core) do
    declarations = core.data.types

    expected_constructors =
      for {_name, declaration} <- declarations,
          constructor <- declaration.constructors,
          into: %{} do
        {constructor.name,
         Map.merge(constructor, %{
           type_name: declaration.name,
           parameters: declaration.parameters
         })}
      end

    valid? =
      is_map(declarations) and core.data.constructors == expected_constructors and
        Enum.all?(declarations, fn {name, declaration} ->
          parameters = MapSet.new(declaration.parameters)

          name == declaration.name and declaration.constructors != [] and
            length(declaration.parameters) == length(Enum.uniq(declaration.parameters)) and
            Enum.all?(declaration.constructors, fn constructor ->
              Enum.all?(constructor.fields, fn field ->
                known_type?(field, declarations) and
                  MapSet.subset?(Type.variables(field), parameters)
              end)
            end)
        end)

    if valid?, do: :ok, else: {:error, "datatype evidence is inconsistent"}
  end

  defp verify_exports(core) do
    definitions = MapSet.new(core.definitions, & &1.name)
    processes = MapSet.new(core.processes, & &1.name)
    types = MapSet.new(Map.keys(core.data.types))

    cond do
      not MapSet.subset?(MapSet.new(core.exports.values), definitions) ->
        {:error, "value export is missing"}

      not MapSet.subset?(MapSet.new(core.exports.processes), processes) ->
        {:error, "process export is missing"}

      not MapSet.subset?(MapSet.new(core.exports.types), types) ->
        {:error, "type export is missing"}

      true ->
        :ok
    end
  end

  defp verify_process_index(core) do
    valid? =
      Enum.all?(core.processes, fn process ->
        case Map.fetch(core.process_entries, process.name) do
          {:ok, entry} ->
            entry.identity == process.identity and entry.module == core.module and
              entry.name == process.name and entry.mailbox == process.mailbox and
              entry.parameters == Enum.map(process.parameters, & &1.type) and
              entry.arity == length(process.parameters) and not entry.imported? and
              entry.spawn_symbol == process.spawn_symbol

          :error ->
            false
        end
      end)

    if valid?, do: :ok, else: {:error, "process-entry index is inconsistent"}
  end

  defp verify_handlers(core) do
    context = %{mailbox: nil, resumptions: %{}}

    Enum.reduce_while(core.handlers, :ok, fn {name, handler}, :ok ->
      effect = Map.get(core.effects, handler.effect)

      valid? =
        name == handler.name and not is_nil(effect) and
          MapSet.new(Map.keys(handler.operations)) == MapSet.new(Map.keys(effect.operations)) and
          verify_handler_return(handler, context, core) and
          Enum.all?(handler.operations, fn {operation_name, clause} ->
            verify_handler_operation(operation_name, clause, handler, effect, context, core)
          end)

      if valid?, do: {:cont, :ok}, else: {:halt, {:error, "handler #{name} is inconsistent"}}
    end)
  end

  defp verify_handler_return(handler, context, core) do
    environment = %{handler.return.parameter => handler.input}

    case verify_expression(handler.return.body, environment, context, core) do
      {:ok, type, []} -> type == handler.output
      _ -> false
    end
  end

  defp verify_handler_operation(operation_name, clause, handler, effect, context, core) do
    operation = Map.get(effect.operations, operation_name)

    if operation do
      parameters = Enum.map(clause.parameters, & &1.type)
      resumption = %{argument: operation.result, result: handler.output}

      environment =
        clause.parameters
        |> Map.new(&{&1.name, &1.type})

      clause_context = %{context | resumptions: %{clause.resumption => resumption}}

      parameters == operation.parameters and clause.resumption_type == resumption and
        count_resumes(clause.body, clause.resumption) <= 1 and
        match?(
          {:ok, type, []} when type == handler.output,
          verify_expression(clause.body, environment, clause_context, core)
        )
    else
      false
    end
  end

  defp verify_definitions(core) do
    Enum.reduce_while(core.definitions, :ok, fn definition, :ok ->
      valid? =
        definition.arity == function_arity(definition.signature) and
          known_type?(definition.signature, core.data.types) and
          case verify_expression(definition.expression, %{}, empty_context(), core) do
            {:ok, type, effects} ->
              type == definition.signature and effects == canonical_effects(definition.uses)

            :error ->
              false
          end

      if valid?,
        do: {:cont, :ok},
        else: {:halt, {:error, "#{definition.name} fails independent type/effect verification"}}
    end)
  end

  defp verify_processes(core) do
    Enum.reduce_while(core.processes, :ok, fn process, :ok ->
      environment = Map.new(process.parameters, &{&1.name, &1.type})
      context = %{empty_context() | mailbox: process.mailbox}

      valid? =
        Type.closed?(process.mailbox) and sendable?(process.mailbox, core.data) and
          Enum.all?(process.parameters, fn parameter ->
            Type.closed?(parameter.type) and sendable?(parameter.type, core.data)
          end) and
          process.public? == process.name in core.exports.processes and
          case verify_expression(process.body, environment, context, core) do
            {:ok, :unit, effects} ->
              effects == canonical_effects(process.effects) and
                Enum.all?(effects, &(&1 == :process))

            _ ->
              false
          end

      if valid?,
        do: {:cont, :ok},
        else: {:halt, {:error, "#{process.name} fails independent process verification"}}
    end)
  end

  defp verify_expression(
         %{span: %Catena.SourceSpan{}, type: recorded_type, effects: recorded_effects} =
           expression,
         environment,
         context,
         core
       )
       when is_list(recorded_effects) do
    with true <- selection_evidence?(expression, core),
         {:ok, type, effects} <- derive_expression(expression, environment, context, core),
         effects <- canonical_effects(effects),
         true <- type == recorded_type,
         true <- effects == recorded_effects do
      {:ok, type, effects}
    else
      _ -> :error
    end
  end

  defp verify_expression(_expression, _environment, _context, _core), do: :error

  defp derive_expression(%{tag: :integer}, _environment, _context, _core),
    do: {:ok, :integer, []}

  defp derive_expression(%{tag: :boolean}, _environment, _context, _core),
    do: {:ok, :boolean, []}

  defp derive_expression(%{tag: :unit}, _environment, _context, _core),
    do: {:ok, :unit, []}

  defp derive_expression(%{tag: :variable, name: name} = expression, environment, _context, core) do
    case Map.fetch(environment, name) do
      {:ok, {:mono, type}} ->
        if type == expression.type, do: {:ok, type, []}, else: :error

      {:ok, {:scheme, variables, type}} ->
        if scheme_instance?(type, expression.type, variables),
          do: {:ok, expression.type, []},
          else: :error

      {:ok, type} ->
        if type == expression.type, do: {:ok, type, []}, else: :error

      :error ->
        case Enum.find(core.definitions, &(&1.name == name)) do
          nil ->
            :error

          definition ->
            if scheme_instance?(definition.signature, expression.type, definition.variables) do
              {:ok, expression.type, canonical_effects(definition.uses)}
            else
              :error
            end
        end
    end
  end

  defp derive_expression(%{tag: :function} = expression, environment, context, core) do
    environment = Map.put(environment, expression.parameter, expression.parameter_type)

    with {:ok, result, body_effects} <-
           verify_expression(expression.body, environment, context, core) do
      {:ok, {:function, expression.parameter_type, body_effects, result}, []}
    end
  end

  defp derive_expression(%{tag: :call} = expression, environment, context, core) do
    with {:ok, callee_type, callee_effects} <-
           verify_expression(expression.callee, environment, context, core) do
      Enum.reduce_while(expression.arguments, {:ok, callee_type, callee_effects}, fn argument,
                                                                                     {:ok,
                                                                                      function_type,
                                                                                      effects} ->
        case function_type do
          {:function, parameter, latent, result} ->
            case verify_expression(argument, environment, context, core) do
              {:ok, ^parameter, argument_effects} ->
                {:cont, {:ok, result, combine_effects(effects, argument_effects, latent)}}

              _ ->
                {:halt, :error}
            end

          _ ->
            {:halt, :error}
        end
      end)
    end
  end

  defp derive_expression(%{tag: :let} = expression, environment, context, core) do
    with {:ok, value_type, value_effects} <-
           verify_expression(expression.value, environment, context, core),
         {:ok, entry} <-
           verify_let_binding(expression, value_type, value_effects, environment),
         {:ok, body_type, body_effects} <-
           verify_expression(
             expression.body,
             Map.put(environment, expression.name, entry),
             context,
             core
           ) do
      {:ok, body_type, combine_effects(value_effects, body_effects)}
    end
  end

  defp derive_expression(%{tag: :sequence} = expression, environment, context, core) do
    with {:ok, _first_type, first_effects} <-
           verify_expression(expression.first, environment, context, core),
         {:ok, second_type, second_effects} <-
           verify_expression(expression.second, environment, context, core) do
      {:ok, second_type, combine_effects(first_effects, second_effects)}
    end
  end

  defp derive_expression(%{tag: :tuple} = expression, environment, context, core) do
    with {:ok, types, effects} <-
           verify_expressions(expression.elements, environment, context, core) do
      {:ok, {:tuple, types}, effects}
    end
  end

  defp derive_expression(%{tag: :annotate} = expression, environment, context, core) do
    with {:ok, type, effects} <-
           verify_expression(expression.expression, environment, context, core),
         true <- type == expression.annotation do
      {:ok, expression.annotation, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :unary} = expression, environment, context, core) do
    {parameter, result} =
      case expression.operator do
        :not -> {:boolean, :boolean}
        :negate -> {:integer, :integer}
      end

    case verify_expression(expression.operand, environment, context, core) do
      {:ok, ^parameter, effects} -> {:ok, result, effects}
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :binary} = expression, environment, context, core) do
    with {:ok, left_type, left_effects} <-
           verify_expression(expression.left, environment, context, core),
         {:ok, right_type, right_effects} <-
           verify_expression(expression.right, environment, context, core),
         {:ok, operand, result} <- binary_signature(expression.operator, left_type),
         true <- left_type == operand and right_type == operand do
      {:ok, result, combine_effects(left_effects, right_effects)}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :record} = expression, environment, context, core) do
    {expressions, labels} =
      Enum.map_reduce(expression.fields, [], fn field, labels ->
        {field.expression, [field.label | labels]}
      end)

    with true <- length(labels) == length(Enum.uniq(labels)),
         {:ok, types, effects} <- verify_expressions(expressions, environment, context, core) do
      fields = expression.fields |> Enum.map(& &1.label) |> Enum.zip(types) |> Map.new()
      {:ok, {:record, %{fields: fields, tail: nil}}, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :select} = expression, environment, context, core) do
    with {:ok, {:record, row}, effects} <-
           verify_expression(expression.record, environment, context, core),
         {:ok, type} <- Map.fetch(row.fields, expression.label) do
      {:ok, type, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :update} = expression, environment, context, core) do
    with {:ok, {:record, row} = type, record_effects} <-
           verify_expression(expression.record, environment, context, core),
         {:ok, field_type} <- Map.fetch(row.fields, expression.label),
         {:ok, ^field_type, value_effects} <-
           verify_expression(expression.value, environment, context, core) do
      {:ok, type, combine_effects(record_effects, value_effects)}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :extend} = expression, environment, context, core) do
    with {:ok, {:record, row}, record_effects} <-
           verify_expression(expression.record, environment, context, core),
         true <- is_nil(row.tail),
         false <- Map.has_key?(row.fields, expression.label),
         {:ok, value_type, value_effects} <-
           verify_expression(expression.value, environment, context, core) do
      result = {:record, %{row | fields: Map.put(row.fields, expression.label, value_type)}}
      {:ok, result, combine_effects(record_effects, value_effects)}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :restrict} = expression, environment, context, core) do
    with {:ok, {:record, row}, effects} <-
           verify_expression(expression.record, environment, context, core),
         true <- Map.has_key?(row.fields, expression.label) do
      {:ok, {:record, %{row | fields: Map.delete(row.fields, expression.label)}}, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :inject} = expression, environment, context, core) do
    with {:variant, row} <- expression.type,
         {:ok, field_type} <- Map.fetch(row.fields, expression.label),
         {:ok, ^field_type, effects} <-
           verify_expression(expression.payload, environment, context, core) do
      {:ok, expression.type, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :construct} = expression, environment, context, core) do
    constructor = Map.fetch!(core.data.constructors, expression.constructor)

    with {:nominal, name, arguments} <- expression.type,
         true <- name == constructor.type_name,
         true <- length(arguments) == length(constructor.parameters),
         substitution = constructor.parameters |> Enum.zip(arguments) |> Map.new(),
         expected = Enum.map(constructor.fields, &Type.substitute(&1, substitution)),
         {:ok, actual, effects} <-
           verify_expressions(expression.arguments, environment, context, core),
         true <- actual == expected do
      {:ok, expression.type, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :match} = expression, environment, context, core) do
    with {:ok, scrutinee_type, scrutinee_effects} <-
           verify_expression(expression.scrutinee, environment, context, core),
         {:ok, result_type, clause_effects} <-
           verify_clauses(expression.clauses, scrutinee_type, environment, context, core),
         true <- exhaustive?(scrutinee_type, expression.clauses, core) do
      {:ok, result_type, combine_effects(scrutinee_effects, clause_effects)}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :trait_call} = expression, environment, context, core) do
    trait = Map.fetch!(core.traits, expression.trait)
    method = Map.fetch!(trait.methods, expression.method)

    with [first | _rest] <- expression.arguments,
         {:ok, head, _effects} <- verify_expression(first, environment, context, core),
         true <- expression.selected_instance == %{trait: trait.name, head: head},
         method_type = Type.substitute(method.type, %{trait.parameter => head}),
         {:ok, result, effects} <-
           verify_applied_arguments(
             expression.arguments,
             method_type,
             environment,
             context,
             core
           ) do
      {:ok, result, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :request} = expression, environment, context, core) do
    effect = Map.fetch!(core.effects, expression.effect)
    operation = Map.fetch!(effect.operations, expression.operation)

    with {:ok, types, effects} <-
           verify_expressions(expression.arguments, environment, context, core),
         true <- types == operation.parameters do
      {:ok, operation.result, combine_effects(effects, [{:effect, effect.name}])}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :handle} = expression, environment, context, core) do
    handler = Map.fetch!(core.handlers, expression.handler)

    with {:ok, type, effects} <-
           verify_expression(expression.expression, environment, context, core),
         true <- type == handler.input,
         {:ok, residual} <- remove_effect(effects, {:effect, handler.effect}) do
      {:ok, handler.output, residual}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :resume} = expression, environment, context, core) do
    with {:ok, resumption} <- Map.fetch(context.resumptions, expression.resumption),
         true <- expression.resumption_type == resumption,
         {:ok, type, effects} <-
           verify_expression(expression.expression, environment, context, core),
         true <- type == resumption.argument do
      {:ok, resumption.result, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :spawn} = expression, environment, context, core) do
    entry = Map.fetch!(core.process_entries, expression.entry)

    with {:ok, types, effects} <-
           verify_expressions(expression.arguments, environment, context, core),
         true <- types == entry.parameters do
      {:ok, {:process, entry.mailbox}, combine_effects(effects, [:process])}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :self}, _environment, %{mailbox: mailbox}, _core)
       when not is_nil(mailbox),
       do: {:ok, {:process, mailbox}, []}

  defp derive_expression(%{tag: :send} = expression, environment, context, core) do
    with {:ok, {:process, mailbox}, target_effects} <-
           verify_expression(expression.left, environment, context, core),
         true <- expression.mailbox == mailbox,
         {:ok, ^mailbox, message_effects} <-
           verify_expression(expression.right, environment, context, core) do
      {:ok, :unit, combine_effects(target_effects, message_effects, [:process])}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :receive} = expression, environment, context, core)
       when not is_nil(context.mailbox) do
    with true <- expression.mailbox == context.mailbox,
         {:ok, result, effects} <-
           verify_clauses(
             expression.clauses,
             context.mailbox,
             environment,
             context,
             core
           ) do
      {:ok, result, combine_effects(effects, [:process])}
    else
      _ -> :error
    end
  end

  defp derive_expression(%{tag: :trap} = expression, environment, context, core) do
    with {:ok, reason_type, effects} <-
           verify_expression(expression.expression, environment, context, core),
         true <- reason_type == expression.reason_type,
         true <- Type.closed?(reason_type) and sendable?(reason_type, core.data) do
      {:ok, expression.type, effects}
    else
      _ -> :error
    end
  end

  defp derive_expression(_expression, _environment, _context, _core), do: :error

  defp verify_let_binding(
         %{binding: %{variables: variables, type: binding_type}, value: value},
         value_type,
         value_effects,
         environment
       )
       when is_list(variables) do
    canonical_variables = variables |> Enum.uniq() |> Enum.sort()

    generalizable? =
      value_effects == [] and (non_expansive?(value) or not effect_control?(value))

    expected_variables =
      if generalizable? do
        binding_type
        |> Type.variables()
        |> MapSet.difference(environment_type_variables(environment))
        |> MapSet.to_list()
        |> Enum.sort()
      else
        []
      end

    if binding_type == value_type and variables == canonical_variables and
         variables == expected_variables do
      if variables == [],
        do: {:ok, {:mono, binding_type}},
        else: {:ok, {:scheme, variables, binding_type}}
    else
      :error
    end
  end

  defp verify_let_binding(_expression, _value_type, _value_effects, _environment), do: :error

  defp environment_type_variables(environment) do
    environment
    |> Map.values()
    |> Enum.reduce(MapSet.new(), fn
      {:mono, type}, variables ->
        MapSet.union(variables, Type.variables(type))

      {:scheme, bound, type}, variables ->
        free = MapSet.difference(Type.variables(type), MapSet.new(bound))
        MapSet.union(variables, free)

      type, variables ->
        MapSet.union(variables, Type.variables(type))
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

  defp verify_expressions(expressions, environment, context, core) do
    Enum.reduce_while(expressions, {:ok, [], []}, fn expression, {:ok, types, effects} ->
      case verify_expression(expression, environment, context, core) do
        {:ok, type, expression_effects} ->
          {:cont, {:ok, [type | types], combine_effects(effects, expression_effects)}}

        :error ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, types, effects} -> {:ok, Enum.reverse(types), effects}
      :error -> :error
    end
  end

  defp verify_applied_arguments(arguments, function_type, environment, context, core) do
    Enum.reduce_while(arguments, {:ok, function_type, []}, fn argument, {:ok, current, effects} ->
      case current do
        {:function, parameter, latent, result} ->
          case verify_expression(argument, environment, context, core) do
            {:ok, ^parameter, argument_effects} ->
              {:cont, {:ok, result, combine_effects(effects, argument_effects, latent)}}

            _ ->
              {:halt, :error}
          end

        _ ->
          {:halt, :error}
      end
    end)
  end

  defp verify_clauses(clauses, scrutinee_type, environment, context, core) do
    Enum.reduce_while(clauses, {:ok, nil, []}, fn clause, {:ok, result_type, effects} ->
      with {:ok, bindings} <- verify_pattern(clause.pattern, scrutinee_type, core),
           branch_environment = Map.merge(environment, bindings),
           {:ok, guard_effects} <-
             verify_guard(clause.guard, branch_environment, context, core),
           {:ok, body_type, body_effects} <-
             verify_expression(clause.body, branch_environment, context, core),
           true <- is_nil(result_type) or result_type == body_type do
        {:cont, {:ok, body_type, combine_effects(effects, guard_effects, body_effects)}}
      else
        _ -> {:halt, :error}
      end
    end)
  end

  defp verify_guard(nil, _environment, _context, _core), do: {:ok, []}

  defp verify_guard(guard, environment, context, core) do
    with true <- portable_condition?(guard),
         {:ok, :boolean, []} <- verify_expression(guard, environment, context, core) do
      {:ok, []}
    else
      _ -> :error
    end
  end

  defp verify_pattern(pattern, expected, core) do
    if pattern.type == expected do
      derive_pattern(pattern, expected, core)
    else
      :error
    end
  end

  defp derive_pattern(%{tag: :wildcard}, _expected, _core), do: {:ok, %{}}

  defp derive_pattern(%{tag: :bind, name: name}, expected, _core),
    do: {:ok, %{name => expected}}

  defp derive_pattern(%{tag: :integer}, :integer, _core), do: {:ok, %{}}
  defp derive_pattern(%{tag: :boolean}, :boolean, _core), do: {:ok, %{}}

  defp derive_pattern(%{tag: :tuple, elements: patterns}, {:tuple, types}, core)
       when length(patterns) == length(types),
       do: verify_patterns(patterns, types, core)

  defp derive_pattern(
         %{tag: :variant, label: label, pattern: pattern},
         {:variant, row},
         core
       ) do
    case Map.fetch(row.fields, label) do
      {:ok, type} -> verify_pattern(pattern, type, core)
      :error -> :error
    end
  end

  defp derive_pattern(%{tag: :constructor} = pattern, {:nominal, name, arguments}, core) do
    constructor = Map.fetch!(core.data.constructors, pattern.constructor)
    substitution = constructor.parameters |> Enum.zip(arguments) |> Map.new()
    fields = Enum.map(constructor.fields, &Type.substitute(&1, substitution))

    if constructor.type_name == name and length(arguments) == length(constructor.parameters) and
         length(pattern.patterns) == length(fields) do
      verify_patterns(pattern.patterns, fields, core)
    else
      :error
    end
  end

  defp derive_pattern(%{tag: :as, pattern: pattern, name: name}, expected, core) do
    with {:ok, bindings} <- verify_pattern(pattern, expected, core),
         false <- Map.has_key?(bindings, name) do
      {:ok, Map.put(bindings, name, expected)}
    else
      _ -> :error
    end
  end

  defp derive_pattern(%{tag: :or, alternatives: alternatives}, expected, core) do
    with {:ok, bindings} <-
           Enum.reduce_while(alternatives, {:ok, nil}, fn alternative, {:ok, first} ->
             case verify_pattern(alternative, expected, core) do
               {:ok, current} when is_nil(first) -> {:cont, {:ok, current}}
               {:ok, current} when current == first -> {:cont, {:ok, first}}
               _ -> {:halt, :error}
             end
           end) do
      {:ok, bindings}
    end
  end

  defp derive_pattern(_pattern, _expected, _core), do: :error

  defp verify_patterns(patterns, types, core) do
    Enum.zip(patterns, types)
    |> Enum.reduce_while({:ok, %{}}, fn {pattern, type}, {:ok, bindings} ->
      case verify_pattern(pattern, type, core) do
        {:ok, next} ->
          if Map.keys(bindings) |> Enum.any?(&Map.has_key?(next, &1)),
            do: {:halt, :error},
            else: {:cont, {:ok, Map.merge(bindings, next)}}

        :error ->
          {:halt, :error}
      end
    end)
  end

  defp selection_evidence?(%{tag: :construct} = expression, core) do
    case Map.fetch(core.data.constructors, expression.constructor) do
      {:ok, constructor} -> selected_constructor?(expression.selected_constructor, constructor)
      :error -> false
    end
  end

  defp selection_evidence?(%{tag: :trait_call} = expression, core) do
    with %{trait: trait, head: head} <- expression.selected_instance,
         {:ok, instance} <- Map.fetch(core.instances, {trait, head}),
         definition when is_binary(definition) <- expression.selected_definition do
      Map.get(instance.methods, expression.method) == definition
    else
      _ -> false
    end
  end

  defp selection_evidence?(%{tag: :handle} = expression, core) do
    case Map.fetch(core.handlers, expression.handler) do
      {:ok, handler} ->
        expression.selected_handler.name == handler.name and
          expression.selected_handler.effect == handler.effect

      :error ->
        false
    end
  end

  defp selection_evidence?(%{tag: :request} = expression, core) do
    with {:ok, effect} <- Map.fetch(core.effects, expression.effect),
         {:ok, operation} <- Map.fetch(effect.operations, expression.operation) do
      length(expression.arguments) == length(operation.parameters)
    else
      _ -> false
    end
  end

  defp selection_evidence?(%{tag: :spawn} = expression, core) do
    case Map.fetch(core.process_entries, expression.entry) do
      {:ok, entry} ->
        expression.selected_entry.identity == entry.identity and
          expression.selected_entry.mailbox == entry.mailbox and
          expression.selected_entry.parameters == entry.parameters and
          expression.selected_entry.spawn_symbol == entry.spawn_symbol

      :error ->
        false
    end
  end

  defp selection_evidence?(%{tag: tag, clauses: clauses}, core)
       when tag in [:match, :receive],
       do: Enum.all?(clauses, &pattern_selection_evidence?(&1.pattern, core))

  defp selection_evidence?(_expression, _core), do: true

  defp pattern_selection_evidence?(%{tag: :constructor} = pattern, core) do
    case Map.fetch(core.data.constructors, pattern.constructor) do
      {:ok, constructor} ->
        selected_constructor?(pattern.selected_constructor, constructor) and
          Enum.all?(pattern.patterns, &pattern_selection_evidence?(&1, core))

      :error ->
        false
    end
  end

  defp pattern_selection_evidence?(%{tag: :tuple, elements: patterns}, core),
    do: Enum.all?(patterns, &pattern_selection_evidence?(&1, core))

  defp pattern_selection_evidence?(%{tag: :variant, pattern: pattern}, core),
    do: pattern_selection_evidence?(pattern, core)

  defp pattern_selection_evidence?(%{tag: :as, pattern: pattern}, core),
    do: pattern_selection_evidence?(pattern, core)

  defp pattern_selection_evidence?(%{tag: :or, alternatives: patterns}, core),
    do: Enum.all?(patterns, &pattern_selection_evidence?(&1, core))

  defp pattern_selection_evidence?(_pattern, _core), do: true

  defp selected_constructor?(selected, constructor) do
    selected.name == constructor.name and selected.type_name == constructor.type_name and
      selected.parameters == constructor.parameters and selected.fields == constructor.fields
  end

  defp exhaustive?(type, clauses, core) do
    unguarded = Enum.filter(clauses, &is_nil(&1.guard))

    if Enum.any?(unguarded, &catch_all_pattern?(&1.pattern)) do
      true
    else
      covered = unguarded |> Enum.flat_map(&covered_heads(&1.pattern)) |> MapSet.new()

      case type do
        :boolean ->
          covered == MapSet.new([true, false])

        {:variant, %{fields: fields, tail: nil}} ->
          covered == MapSet.new(Map.keys(fields))

        {:nominal, name, _arguments} ->
          case Map.fetch(core.data.types, name) do
            {:ok, data} ->
              covered == MapSet.new(Enum.map(data.constructors, & &1.name))

            :error ->
              false
          end

        _ ->
          false
      end
    end
  end

  defp catch_all_pattern?(%{tag: tag}) when tag in [:wildcard, :bind], do: true
  defp catch_all_pattern?(%{tag: :as, pattern: pattern}), do: catch_all_pattern?(pattern)

  defp catch_all_pattern?(%{tag: :or, alternatives: alternatives}),
    do: Enum.any?(alternatives, &catch_all_pattern?/1)

  defp catch_all_pattern?(_pattern), do: false

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

  defp irrefutable_pattern?(%{tag: tag}) when tag in [:wildcard, :bind], do: true

  defp irrefutable_pattern?(%{tag: :tuple, elements: elements}),
    do: Enum.all?(elements, &irrefutable_pattern?/1)

  defp irrefutable_pattern?(%{tag: :as, pattern: pattern}),
    do: irrefutable_pattern?(pattern)

  defp irrefutable_pattern?(%{tag: :or, alternatives: alternatives}),
    do: Enum.any?(alternatives, &irrefutable_pattern?/1)

  defp irrefutable_pattern?(_pattern), do: false

  defp scheme_instance?(scheme, instance, variables) do
    case match_scheme(scheme, instance, MapSet.new(variables), %{}) do
      {:ok, _substitution} -> true
      :error -> false
    end
  end

  defp match_scheme({:variable, name}, instance, variables, substitution) do
    if MapSet.member?(variables, name) do
      case Map.fetch(substitution, name) do
        {:ok, existing} when existing == instance -> {:ok, substitution}
        {:ok, _existing} -> :error
        :error -> {:ok, Map.put(substitution, name, instance)}
      end
    else
      if instance == {:variable, name}, do: {:ok, substitution}, else: :error
    end
  end

  defp match_scheme({:tuple, left}, {:tuple, right}, variables, substitution)
       when length(left) == length(right),
       do: match_scheme_many(left, right, variables, substitution)

  defp match_scheme(
         {:function, left_parameter, left_effects, left_result},
         {:function, right_parameter, right_effects, right_result},
         variables,
         substitution
       )
       when left_effects == right_effects do
    with {:ok, substitution} <-
           match_scheme(left_parameter, right_parameter, variables, substitution) do
      match_scheme(left_result, right_result, variables, substitution)
    end
  end

  defp match_scheme({tag, left}, {tag, right}, variables, substitution)
       when tag in [:record, :variant] and left.tail == right.tail and
              map_size(left.fields) == map_size(right.fields) do
    labels = Map.keys(left.fields)

    if MapSet.new(labels) == MapSet.new(Map.keys(right.fields)) do
      match_scheme_many(
        Enum.map(labels, &Map.fetch!(left.fields, &1)),
        Enum.map(labels, &Map.fetch!(right.fields, &1)),
        variables,
        substitution
      )
    else
      :error
    end
  end

  defp match_scheme({:process, left}, {:process, right}, variables, substitution),
    do: match_scheme(left, right, variables, substitution)

  defp match_scheme(
         {:nominal, name, left},
         {:nominal, name, right},
         variables,
         substitution
       )
       when length(left) == length(right),
       do: match_scheme_many(left, right, variables, substitution)

  defp match_scheme(type, type, _variables, substitution), do: {:ok, substitution}
  defp match_scheme(_scheme, _instance, _variables, _substitution), do: :error

  defp match_scheme_many(left, right, variables, substitution) do
    Enum.zip(left, right)
    |> Enum.reduce_while({:ok, substitution}, fn {left, right}, {:ok, substitution} ->
      case match_scheme(left, right, variables, substitution) do
        {:ok, substitution} -> {:cont, {:ok, substitution}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp binary_signature(operator, left) do
    cond do
      operator in [:and, :or] ->
        {:ok, :boolean, :boolean}

      operator in [:add, :subtract, :multiply] ->
        {:ok, :integer, :integer}

      operator in [:less, :less_equal, :greater, :greater_equal] ->
        {:ok, :integer, :boolean}

      operator in [:equal, :not_equal] and left in [:integer, :boolean] ->
        {:ok, left, :boolean}

      true ->
        :error
    end
  end

  defp known_type?(type, declarations) do
    case type do
      primitive when primitive in [:integer, :boolean, :unit] ->
        true

      {:variable, _name} ->
        true

      {:tuple, elements} ->
        Enum.all?(elements, &known_type?(&1, declarations))

      {:function, parameter, _effects, result} ->
        known_type?(parameter, declarations) and known_type?(result, declarations)

      {tag, %{fields: fields}} when tag in [:record, :variant] ->
        Enum.all?(fields, fn {_label, field} -> known_type?(field, declarations) end)

      {:process, mailbox} ->
        known_type?(mailbox, declarations)

      {:nominal, name, arguments} ->
        case Map.fetch(declarations, name) do
          {:ok, declaration} ->
            length(arguments) == length(declaration.parameters) and
              Enum.all?(arguments, &known_type?(&1, declarations))

          :error ->
            false
        end

      _ ->
        false
    end
  end

  defp sendable?(type, data), do: sendable?(type, data, MapSet.new())

  defp sendable?(type, _data, _seen) when type in [:integer, :boolean, :unit], do: true

  defp sendable?({:tuple, elements}, data, seen),
    do: Enum.all?(elements, &sendable?(&1, data, seen))

  defp sendable?({tag, %{fields: fields, tail: nil}}, data, seen)
       when tag in [:record, :variant],
       do: Enum.all?(fields, fn {_label, field} -> sendable?(field, data, seen) end)

  defp sendable?({:process, mailbox}, data, seen),
    do: Type.closed?(mailbox) and sendable?(mailbox, data, seen)

  defp sendable?({:nominal, name, arguments} = type, data, seen) do
    if MapSet.member?(seen, type) do
      true
    else
      with {:ok, declaration} <- Map.fetch(data.types, name),
           true <- length(arguments) == length(declaration.parameters) do
        substitution = declaration.parameters |> Enum.zip(arguments) |> Map.new()
        seen = MapSet.put(seen, type)

        Enum.all?(declaration.constructors, fn constructor ->
          Enum.all?(constructor.fields, fn field ->
            field |> Type.substitute(substitution) |> sendable?(data, seen)
          end)
        end)
      else
        _ -> false
      end
    end
  end

  defp sendable?(_type, _data, _seen), do: false

  defp canonical_effects(effects) do
    process = if :process in effects, do: [:process], else: []
    ordinary = effects |> Enum.reject(&(&1 == :process)) |> Enum.sort_by(&inspect/1)
    process ++ ordinary
  end

  defp combine_effects(effects), do: effects |> List.flatten() |> canonical_effects()
  defp combine_effects(first, second), do: combine_effects([first, second])
  defp combine_effects(first, second, third), do: combine_effects([first, second, third])

  defp remove_effect(effects, occurrence) do
    case Enum.split_while(effects, &(&1 != occurrence)) do
      {_before, []} -> :error
      {before, [_occurrence | after_effects]} -> {:ok, before ++ after_effects}
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

  defp function_arity({:function, _parameter, _effects, result}), do: 1 + function_arity(result)
  defp function_arity(_type), do: 0
  defp empty_context, do: %{mailbox: nil, resumptions: %{}}
end
