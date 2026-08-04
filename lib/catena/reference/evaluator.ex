defmodule Catena.Reference.Evaluator do
  @moduledoc "Reference evaluator dispatcher for the executable C001-C006 core."

  @budget_key {__MODULE__, :budget}
  @steps_key {__MODULE__, :steps}
  @effect_versions Catena.LanguageVersion.from(:effects_and_handlers)

  @spec run(map(), String.t(), [term()]) :: {:ok, term()} | {:error, term()}
  def run(core, name, arguments \\ [])

  def run(%{frontend_version: version} = core, name, arguments)
      when version in @effect_versions,
      do: Catena.Effect.Reference.run(core, name, arguments)

  def run(core, name, arguments) do
    definitions = Map.new(core.definitions, &{&1.name, &1})

    with {:ok, definition} <- Map.fetch(definitions, name) do
      value = evaluate_definition(definition, definitions)
      {:ok, Enum.reduce(arguments, value, &apply_value(&2, &1))}
    else
      :error -> {:error, {:unknown_definition, name}}
    end
  rescue
    error -> {:error, {error.__struct__, Exception.message(error)}}
  end

  @spec run_bounded(map(), String.t(), [term()], pos_integer()) ::
          {:ok, term(), non_neg_integer()}
          | {:error, term(), non_neg_integer()}
          | {:budget_exhausted, non_neg_integer()}
  def run_bounded(core, name, arguments, budget) when is_integer(budget) and budget > 0 do
    Process.put(@budget_key, budget)
    Process.put(@steps_key, 0)

    try do
      case run_standard(core, name, arguments) do
        {:ok, value} -> {:ok, value, Process.get(@steps_key)}
        {:error, reason} -> {:error, reason, Process.get(@steps_key)}
      end
    catch
      :throw, :catena_specification_budget_exhausted ->
        {:budget_exhausted, Process.get(@steps_key)}
    after
      Process.delete(@budget_key)
      Process.delete(@steps_key)
    end
  end

  defp run_standard(core, name, arguments) do
    definitions = Map.new(core.definitions, &{&1.name, &1})

    with {:ok, definition} <- Map.fetch(definitions, name) do
      value = evaluate_definition(definition, definitions)
      {:ok, Enum.reduce(arguments, value, &apply_value(&2, &1))}
    else
      :error -> {:error, {:unknown_definition, name}}
    end
  rescue
    error -> {:error, {error.__struct__, Exception.message(error)}}
  end

  defp evaluate_definition(%{expression: %{tag: :derived_fold} = fold}, definitions) do
    arity = length(fold.handler_names) + 1
    curry(arity, fn arguments -> evaluate_fold(fold, arguments, definitions) end)
  end

  defp evaluate_definition(
         %{expression: %{tag: :derived_capability} = derived} = definition,
         _definitions
       ) do
    curry(length(definition.parameters), &evaluate_capability(derived, &1))
  end

  defp evaluate_definition(definition, definitions),
    do: evaluate(definition.expression, %{}, definitions)

  defp evaluate(expression, environment, definitions) do
    spend!()
    do_evaluate(expression, environment, definitions)
  end

  defp do_evaluate(%{tag: :integer, value: value}, _environment, _definitions), do: value
  defp do_evaluate(%{tag: :boolean, value: value}, _environment, _definitions), do: value

  defp do_evaluate(%{tag: :unary, operator: :not, operand: operand}, environment, definitions),
    do: not evaluate(operand, environment, definitions)

  defp do_evaluate(%{tag: :unary, operator: :negate, operand: operand}, environment, definitions),
    do: -evaluate(operand, environment, definitions)

  defp do_evaluate(
         %{tag: :binary, operator: :and, left: left, right: right},
         environment,
         definitions
       ) do
    evaluate(left, environment, definitions) and evaluate(right, environment, definitions)
  end

  defp do_evaluate(
         %{tag: :binary, operator: :or, left: left, right: right},
         environment,
         definitions
       ) do
    evaluate(left, environment, definitions) or evaluate(right, environment, definitions)
  end

  defp do_evaluate(
         %{tag: :binary, operator: operator, left: left, right: right},
         environment,
         definitions
       ) do
    left = evaluate(left, environment, definitions)
    right = evaluate(right, environment, definitions)

    case operator do
      :equal -> left === right
      :not_equal -> left !== right
      :less -> left < right
      :less_equal -> left <= right
      :greater -> left > right
      :greater_equal -> left >= right
      :add -> left + right
      :subtract -> left - right
      :multiply -> left * right
    end
  end

  defp do_evaluate(%{tag: :variable, name: name}, environment, definitions) do
    case Map.fetch(environment, name) do
      {:ok, value} -> value
      :error -> definitions |> Map.fetch!(name) |> evaluate_definition(definitions)
    end
  end

  defp do_evaluate(%{tag: :function, parameter: parameter, body: body}, environment, definitions),
    do:
      {:closure,
       fn value -> evaluate(body, Map.put(environment, parameter, value), definitions) end}

  defp do_evaluate(%{tag: :call, callee: callee, arguments: arguments}, environment, definitions) do
    Enum.reduce(arguments, evaluate(callee, environment, definitions), fn argument, function ->
      apply_value(function, evaluate(argument, environment, definitions))
    end)
  end

  defp do_evaluate(%{tag: :let, name: name, value: value, body: body}, environment, definitions) do
    bound = evaluate(value, environment, definitions)
    evaluate(body, Map.put(environment, name, bound), definitions)
  end

  defp do_evaluate(%{tag: :tuple, elements: elements}, environment, definitions),
    do: elements |> Enum.map(&evaluate(&1, environment, definitions)) |> List.to_tuple()

  defp do_evaluate(%{tag: :annotate, expression: expression}, environment, definitions),
    do: evaluate(expression, environment, definitions)

  defp do_evaluate(
         %{tag: :construct, constructor: constructor, arguments: arguments},
         environment,
         definitions
       ) do
    payload =
      arguments
      |> Enum.map(fn argument ->
        expression = Map.get(argument, :expression, argument)
        {argument.field_index, evaluate(expression, environment, definitions)}
      end)
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(&elem(&1, 1))

    {:catena_value, constructor.id, payload}
  end

  defp do_evaluate(
         %{tag: :match, scrutinee: scrutinee, clauses: clauses},
         environment,
         definitions
       ) do
    value = evaluate(scrutinee, environment, definitions)
    evaluate_clauses(value, clauses, environment, definitions)
  end

  defp evaluate_clauses(_value, [], _environment, _definitions),
    do: raise(ArgumentError, "verified exhaustive match reached no clause")

  defp evaluate_clauses(value, [clause | rest], environment, definitions) do
    case match_pattern(clause.pattern, value, %{}) do
      {:ok, bindings} ->
        branch = Map.merge(environment, bindings)

        if is_nil(clause.guard) or evaluate(clause.guard, branch, definitions) == true,
          do: evaluate(clause.body, branch, definitions),
          else: evaluate_clauses(value, rest, environment, definitions)

      :no_match ->
        evaluate_clauses(value, rest, environment, definitions)
    end
  end

  defp match_pattern(%{tag: :wildcard}, _value, bindings), do: {:ok, bindings}

  defp match_pattern(%{tag: :bind, name: name}, value, bindings),
    do: {:ok, Map.put(bindings, name, value)}

  defp match_pattern(%{tag: :integer, value: value}, value, bindings), do: {:ok, bindings}
  defp match_pattern(%{tag: :boolean, value: value}, value, bindings), do: {:ok, bindings}

  defp match_pattern(%{tag: tag}, _value, _bindings) when tag in [:integer, :boolean],
    do: :no_match

  defp match_pattern(%{tag: :tuple, elements: patterns}, value, bindings)
       when is_tuple(value) and tuple_size(value) == length(patterns),
       do: match_patterns(patterns, Tuple.to_list(value), bindings)

  defp match_pattern(
         %{tag: :constructor, constructor: constructor, patterns: patterns},
         {:catena_value, id, values},
         bindings
       )
       when id == constructor.id,
       do: match_patterns(patterns, values, bindings)

  defp match_pattern(%{tag: :as, pattern: pattern, name: name}, value, bindings) do
    with {:ok, bindings} <- match_pattern(pattern, value, bindings),
         do: {:ok, Map.put(bindings, name, value)}
  end

  defp match_pattern(%{tag: :or, alternatives: alternatives}, value, bindings) do
    Enum.find_value(alternatives, :no_match, fn alternative ->
      case match_pattern(alternative, value, bindings) do
        {:ok, _} = result -> result
        :no_match -> false
      end
    end)
  end

  defp match_pattern(_pattern, _value, _bindings), do: :no_match

  defp match_patterns(patterns, values, bindings) do
    Enum.zip(patterns, values)
    |> Enum.reduce_while({:ok, bindings}, fn {pattern, value}, {:ok, current} ->
      case match_pattern(pattern, value, current) do
        {:ok, next} -> {:cont, {:ok, next}}
        :no_match -> {:halt, :no_match}
      end
    end)
  end

  defp evaluate_fold(fold, arguments, _definitions) do
    {handlers, [value]} = Enum.split(arguments, length(fold.handler_names))
    {:catena_value, constructor_id, payload} = value
    constructor = Enum.find(fold.datatype.constructors, &(&1.id == constructor_id))
    handler = Enum.at(handlers, constructor.index)
    Enum.reduce(payload, handler, &apply_value(&2, &1))
  end

  defp evaluate_capability(%{capability: "Equatable"}, [left, right]), do: left === right

  defp evaluate_capability(%{capability: "Orderable"}, [left, right]) do
    cond do
      left < right -> -1
      left === right -> 0
      true -> 1
    end
  end

  defp evaluate_capability(
         %{capability: capability, datatype: datatype, target_indexes: targets},
         arguments
       )
       when capability in ~w(Mapper TwoSlotMapper CollectingMapper) do
    {callbacks, [subject]} = Enum.split(arguments, length(arguments) - 1)
    {:catena_value, constructor_id, payload} = subject
    constructor = Enum.find(datatype.constructors, &(&1.id == constructor_id))

    mapped =
      Enum.zip(constructor.fields, payload)
      |> Enum.map(fn {field, value} ->
        case Enum.find_index(targets, &(&1 == direct_variable(field.type))) do
          nil -> value
          target -> apply_value(Enum.at(callbacks, target), value)
        end
      end)

    {:catena_value, constructor_id, mapped}
  end

  defp evaluate_capability(
         %{capability: "Reducible", datatype: datatype, target_indexes: targets},
         [callback, initial, {:catena_value, constructor_id, payload}]
       ) do
    constructor = Enum.find(datatype.constructors, &(&1.id == constructor_id))

    Enum.zip(constructor.fields, payload)
    |> Enum.reduce(initial, fn {field, value}, accumulator ->
      if direct_variable(field.type) in targets do
        callback |> apply_value(accumulator) |> apply_value(value)
      else
        accumulator
      end
    end)
  end

  defp direct_variable({:var, index}), do: index
  defp direct_variable(_type), do: nil

  defp curry(0, function), do: function.([])
  defp curry(arity, function), do: curry(arity, function, [])

  defp curry(1, function, arguments),
    do: {:closure, fn value -> function.(Enum.reverse([value | arguments])) end}

  defp curry(arity, function, arguments),
    do: {:closure, fn value -> curry(arity - 1, function, [value | arguments]) end}

  defp apply_value({:closure, function}, argument), do: function.(argument)

  defp spend! do
    case Process.get(@budget_key) do
      nil ->
        :ok

      budget ->
        steps = Process.get(@steps_key, 0)

        if steps >= budget do
          throw(:catena_specification_budget_exhausted)
        else
          Process.put(@steps_key, steps + 1)
        end
    end
  end
end
