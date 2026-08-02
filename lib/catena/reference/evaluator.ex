defmodule Catena.Reference.Evaluator do
  @moduledoc "Pure reference evaluator for the executable C001-C003 core."

  @spec run(map(), String.t(), [term()]) :: {:ok, term()} | {:error, term()}
  def run(core, name, arguments \\ []) do
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

  defp evaluate_definition(definition, definitions),
    do: evaluate(definition.expression, %{}, definitions)

  defp evaluate(%{tag: :integer, value: value}, _environment, _definitions), do: value
  defp evaluate(%{tag: :boolean, value: value}, _environment, _definitions), do: value

  defp evaluate(%{tag: :unary, operator: :not, operand: operand}, environment, definitions),
    do: not evaluate(operand, environment, definitions)

  defp evaluate(%{tag: :unary, operator: :negate, operand: operand}, environment, definitions),
    do: -evaluate(operand, environment, definitions)

  defp evaluate(
         %{tag: :binary, operator: :and, left: left, right: right},
         environment,
         definitions
       ) do
    evaluate(left, environment, definitions) and evaluate(right, environment, definitions)
  end

  defp evaluate(
         %{tag: :binary, operator: :or, left: left, right: right},
         environment,
         definitions
       ) do
    evaluate(left, environment, definitions) or evaluate(right, environment, definitions)
  end

  defp evaluate(
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

  defp evaluate(%{tag: :variable, name: name}, environment, definitions) do
    case Map.fetch(environment, name) do
      {:ok, value} -> value
      :error -> definitions |> Map.fetch!(name) |> evaluate_definition(definitions)
    end
  end

  defp evaluate(%{tag: :function, parameter: parameter, body: body}, environment, definitions),
    do:
      {:closure,
       fn value -> evaluate(body, Map.put(environment, parameter, value), definitions) end}

  defp evaluate(%{tag: :call, callee: callee, arguments: arguments}, environment, definitions) do
    Enum.reduce(arguments, evaluate(callee, environment, definitions), fn argument, function ->
      apply_value(function, evaluate(argument, environment, definitions))
    end)
  end

  defp evaluate(%{tag: :let, name: name, value: value, body: body}, environment, definitions) do
    bound = evaluate(value, environment, definitions)
    evaluate(body, Map.put(environment, name, bound), definitions)
  end

  defp evaluate(%{tag: :tuple, elements: elements}, environment, definitions),
    do: elements |> Enum.map(&evaluate(&1, environment, definitions)) |> List.to_tuple()

  defp evaluate(%{tag: :annotate, expression: expression}, environment, definitions),
    do: evaluate(expression, environment, definitions)

  defp evaluate(
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

  defp evaluate(%{tag: :match, scrutinee: scrutinee, clauses: clauses}, environment, definitions) do
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

  defp curry(0, function), do: function.([])
  defp curry(arity, function), do: curry(arity, function, [])

  defp curry(1, function, arguments),
    do: {:closure, fn value -> function.(Enum.reverse([value | arguments])) end}

  defp curry(arity, function, arguments),
    do: {:closure, fn value -> curry(arity - 1, function, [value | arguments]) end}

  defp apply_value({:closure, function}, argument), do: function.(argument)
end
