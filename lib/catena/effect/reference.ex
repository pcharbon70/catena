defmodule Catena.Effect.Reference do
  @moduledoc "Executable free-request reference semantics for Catena 0.5."

  alias Catena.Effect.Runtime

  @type computation ::
          {:returned, term()}
          | {:requested, String.t(), String.t(), atom(), [term()], (term() -> computation())}

  @spec run(map(), String.t(), [term()]) :: {:ok, term()} | {:error, term()}
  def run(core, name, arguments \\ []) do
    definitions = Map.new(core.definitions, &{&1.name, &1})

    with {:ok, definition} <- Map.fetch(definitions, name) do
      computation =
        evaluate(
          definition.expression,
          %{},
          definitions,
          core.effects.handlers,
          &returned/1
        )

      computation =
        Enum.reduce(arguments, computation, fn argument, current ->
          bind(current, fn function -> apply_value(function, argument, &returned/1) end)
        end)

      finish(computation)
    else
      :error -> {:error, {:unknown_definition, name}}
    end
  rescue
    error -> {:error, {error.__struct__, Exception.message(error)}}
  end

  defp evaluate(%{tag: :integer, value: value}, _environment, _definitions, _handlers, k),
    do: k.(value)

  defp evaluate(%{tag: :boolean, value: value}, _environment, _definitions, _handlers, k),
    do: k.(value)

  defp evaluate(%{tag: :variable, name: name}, environment, definitions, handlers, k) do
    case Map.fetch(environment, name) do
      {:ok, value} ->
        k.(value)

      :error ->
        definition = Map.fetch!(definitions, name)
        evaluate(definition.expression, %{}, definitions, handlers, k)
    end
  end

  defp evaluate(
         %{tag: :function, parameter: parameter, body: body},
         environment,
         definitions,
         handlers,
         k
       ) do
    closure =
      {:effect_closure,
       fn value, continuation ->
         evaluate(
           body,
           Map.put(environment, parameter, value),
           definitions,
           handlers,
           continuation
         )
       end}

    k.(closure)
  end

  defp evaluate(
         %{tag: :unary, operator: operator, operand: operand},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(operand, environment, definitions, handlers, fn value ->
      k.(if(operator == :not, do: not value, else: -value))
    end)
  end

  defp evaluate(
         %{tag: :binary, operator: :and, left: left, right: right},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(left, environment, definitions, handlers, fn
      false -> k.(false)
      true -> evaluate(right, environment, definitions, handlers, k)
    end)
  end

  defp evaluate(
         %{tag: :binary, operator: :or, left: left, right: right},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(left, environment, definitions, handlers, fn
      true -> k.(true)
      false -> evaluate(right, environment, definitions, handlers, k)
    end)
  end

  defp evaluate(
         %{tag: :binary, operator: operator, left: left, right: right},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(left, environment, definitions, handlers, fn left_value ->
      evaluate(right, environment, definitions, handlers, fn right_value ->
        k.(binary(operator, left_value, right_value))
      end)
    end)
  end

  defp evaluate(
         %{
           tag: :call,
           callee: %{tag: :variable, name: name},
           arguments: arguments,
           effect_bindings: bindings
         },
         environment,
         definitions,
         handlers,
         k
       ) do
    if Map.has_key?(environment, name) or not Map.has_key?(definitions, name) do
      evaluate_call(name, arguments, environment, definitions, handlers, k)
    else
      evaluate_values(arguments, environment, definitions, handlers, fn values ->
        computation =
          evaluate(
            Map.fetch!(definitions, name).expression,
            %{},
            definitions,
            handlers,
            &returned/1
          )
          |> apply_values(values, &returned/1)
          |> rebind_computation(bindings)

        bind(computation, k)
      end)
    end
  end

  defp evaluate(
         %{tag: :call, callee: callee, arguments: arguments},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(callee, environment, definitions, handlers, fn function ->
      evaluate_values(arguments, environment, definitions, handlers, fn values ->
        apply_values(returned(function), values, k)
      end)
    end)
  end

  defp evaluate(
         %{tag: :let, name: name, value: value, body: body},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(value, environment, definitions, handlers, fn bound ->
      evaluate(body, Map.put(environment, name, bound), definitions, handlers, k)
    end)
  end

  defp evaluate(
         %{tag: :tuple, elements: elements},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate_values(elements, environment, definitions, handlers, fn values ->
      k.(List.to_tuple(values))
    end)
  end

  defp evaluate(
         %{tag: :annotate, expression: expression},
         environment,
         definitions,
         handlers,
         k
       ),
       do: evaluate(expression, environment, definitions, handlers, k)

  defp evaluate(
         %{tag: :construct, constructor: constructor, arguments: arguments},
         environment,
         definitions,
         handlers,
         k
       ) do
    expressions = Enum.map(arguments, &Map.get(&1, :expression, &1))

    evaluate_values(expressions, environment, definitions, handlers, fn values ->
      payload =
        arguments
        |> Enum.zip(values)
        |> Enum.sort_by(fn {argument, _value} -> argument.field_index end)
        |> Enum.map(fn {_argument, value} -> value end)

      k.({:catena_value, constructor.id, payload})
    end)
  end

  defp evaluate(
         %{tag: :match, scrutinee: scrutinee, clauses: clauses},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(scrutinee, environment, definitions, handlers, fn value ->
      evaluate_clauses(value, clauses, environment, definitions, handlers, k)
    end)
  end

  defp evaluate(
         %{tag: :request} = expression,
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate_values(expression.arguments, environment, definitions, handlers, fn values ->
      capability = expression.selected_capability
      operation = String.to_atom(expression.operation)
      Runtime.trace({:request, capability.family, capability.capability, operation})

      {:requested, capability.capability, capability.family, operation, values, k}
    end)
  end

  defp evaluate(
         %{tag: :resume, resumption: resumption, value: value},
         environment,
         definitions,
         handlers,
         k
       ) do
    evaluate(value, environment, definitions, handlers, fn reply ->
      environment
      |> Map.fetch!(resumption)
      |> Runtime.resume(reply)
      |> bind(k)
    end)
  end

  defp evaluate(
         %{tag: :handle} = expression,
         environment,
         definitions,
         handlers,
         k
       ) do
    handler = Map.fetch!(handlers, expression.handler)

    evaluate_values(expression.arguments, environment, definitions, handlers, fn handler_values ->
      parameter_environment =
        Enum.zip(handler.parameters, handler_values)
        |> Enum.reduce(environment, fn {parameter, value}, current ->
          Map.put(current, parameter.name, value)
        end)

      Runtime.trace({:handle, handler.id, expression.selected_capability.capability})

      handled =
        expression.expression
        |> evaluate(environment, definitions, handlers, &returned/1)
        |> handle_computation(
          expression.selected_capability.capability,
          handler,
          parameter_environment,
          definitions,
          handlers,
          Map.get(expression, :handler_effect_bindings, [])
        )

      bind(handled, k)
    end)
  end

  defp handle_computation(
         {:returned, value},
         _capability,
         handler,
         environment,
         definitions,
         handlers,
         bindings
       ) do
    Runtime.trace({:return, handler.id})

    handler.return_clause.body
    |> evaluate(
      Map.put(environment, handler.return_clause.parameter, value),
      definitions,
      handlers,
      &returned/1
    )
    |> rebind_computation(bindings)
  end

  defp handle_computation(
         {:requested, capability, _family, operation, values, continuation},
         capability,
         handler,
         environment,
         definitions,
         handlers,
         bindings
       ) do
    clause = Enum.find(handler.operation_clauses, &(String.to_atom(&1.operation) == operation))
    Runtime.trace({:clause, handler.id, clause.operation})

    resumption =
      Runtime.new_resumption(fn reply ->
        continuation.(reply)
        |> handle_computation(
          capability,
          handler,
          environment,
          definitions,
          handlers,
          bindings
        )
      end)

    clause_environment =
      Enum.zip(clause.parameters, values)
      |> Enum.reduce(environment, fn {name, value}, current -> Map.put(current, name, value) end)
      |> Map.put(clause.resumption, resumption)

    unless contains_resume?(clause.body, clause.resumption) do
      Runtime.trace({:abort, handler.id, clause.operation})
    end

    clause.body
    |> evaluate(clause_environment, definitions, handlers, &returned/1)
    |> rebind_computation(bindings)
  end

  defp handle_computation(
         {:requested, capability, family, operation, values, continuation},
         selected,
         handler,
         environment,
         definitions,
         handlers,
         bindings
       ) do
    {:requested, capability, family, operation, values,
     fn reply ->
       continuation.(reply)
       |> handle_computation(
         selected,
         handler,
         environment,
         definitions,
         handlers,
         bindings
       )
     end}
  end

  defp evaluate_values(expressions, environment, definitions, handlers, k),
    do: do_evaluate_values(expressions, environment, definitions, handlers, [], k)

  defp do_evaluate_values([], _environment, _definitions, _handlers, values, k),
    do: k.(Enum.reverse(values))

  defp do_evaluate_values(
         [expression | rest],
         environment,
         definitions,
         handlers,
         values,
         k
       ) do
    evaluate(expression, environment, definitions, handlers, fn value ->
      do_evaluate_values(rest, environment, definitions, handlers, [value | values], k)
    end)
  end

  defp evaluate_call(name, arguments, environment, definitions, handlers, k) do
    evaluate(
      %{tag: :variable, name: name},
      environment,
      definitions,
      handlers,
      fn function ->
        evaluate_values(arguments, environment, definitions, handlers, fn values ->
          apply_values(returned(function), values, k)
        end)
      end
    )
  end

  defp apply_values(computation, [], k), do: bind(computation, k)

  defp apply_values(computation, [argument | rest], k) do
    bind(computation, fn function ->
      apply_value(function, argument, fn value -> apply_values(returned(value), rest, k) end)
    end)
  end

  defp apply_value({:effect_closure, function}, argument, k), do: function.(argument, k)
  defp apply_value({:closure, function}, argument, k), do: k.(function.(argument))

  defp returned(value), do: {:returned, value}

  defp bind({:returned, value}, k), do: k.(value)

  defp bind({:requested, capability, family, operation, values, continuation}, k) do
    {:requested, capability, family, operation, values,
     fn reply -> continuation.(reply) |> bind(k) end}
  end

  defp finish({:returned, value}), do: {:ok, value}

  defp finish({:requested, capability, family, operation, _values, _continuation}),
    do: {:error, {:unhandled_request, family, operation, capability}}

  defp rebind_computation(computation, []), do: computation

  defp rebind_computation({:returned, _value} = computation, _bindings), do: computation

  defp rebind_computation(
         {:requested, capability, family, operation, values, continuation},
         bindings
       ) do
    replacements = Map.new(bindings, &{&1.declared, &1.selected})
    selected = Map.get(replacements, capability, capability)

    {:requested, selected, family, operation, values,
     fn reply -> continuation.(reply) |> rebind_computation(bindings) end}
  end

  defp evaluate_clauses(_value, [], _environment, _definitions, _handlers, _k),
    do: raise(ArgumentError, "verified exhaustive match reached no clause")

  defp evaluate_clauses(
         value,
         [clause | rest],
         environment,
         definitions,
         handlers,
         k
       ) do
    case match_pattern(clause.pattern, value, %{}) do
      {:ok, bindings} ->
        branch = Map.merge(environment, bindings)

        case clause.guard do
          nil ->
            evaluate(clause.body, branch, definitions, handlers, k)

          guard ->
            evaluate(guard, branch, definitions, handlers, fn
              true -> evaluate(clause.body, branch, definitions, handlers, k)
              false -> evaluate_clauses(value, rest, environment, definitions, handlers, k)
            end)
        end

      :no_match ->
        evaluate_clauses(value, rest, environment, definitions, handlers, k)
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

  defp binary(:equal, left, right), do: left === right
  defp binary(:not_equal, left, right), do: left !== right
  defp binary(:less, left, right), do: left < right
  defp binary(:less_equal, left, right), do: left <= right
  defp binary(:greater, left, right), do: left > right
  defp binary(:greater_equal, left, right), do: left >= right
  defp binary(:add, left, right), do: left + right
  defp binary(:subtract, left, right), do: left - right
  defp binary(:multiply, left, right), do: left * right
end
