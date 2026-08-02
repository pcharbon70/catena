defmodule Catena.TypedCore.Verifier do
  @moduledoc "An inference-independent structural verifier for C001-C003 typed core."

  alias Catena.Condition
  alias Catena.Pattern.Coverage
  alias Catena.Type
  alias Catena.Type.Scheme

  @spec verify(map()) :: :ok | {:error, String.t()}
  def verify(module) do
    initial_globals =
      module.definitions
      |> Enum.filter(&Map.get(&1, :generated?, false))
      |> Map.new(&{&1.name, &1.scheme})
      |> Map.merge(get_in(module, [:conditions, :schemes]) || %{})

    Enum.reduce_while(module.definitions, {:ok, initial_globals}, fn definition, {:ok, globals} ->
      case verify_definition(definition, globals, Map.get(module, :data, empty_data())) do
        :ok -> {:cont, {:ok, Map.put(globals, definition.name, definition.scheme)}}
        {:error, reason} -> {:halt, {:error, "#{definition.name}: #{reason}"}}
      end
    end)
    |> case do
      {:ok, _globals} -> :ok
      error -> error
    end
  rescue
    error in Catena.TypeError ->
      {:error, "coverage invariant failed: #{error.diagnostic.message}"}
  end

  defp verify_definition(
         %{expression: %{tag: :derived_fold} = expression, scheme: scheme},
         _globals,
         _data
       ) do
    type = expression.datatype

    cond do
      expression.provenance != :compiler_derived ->
        {:error, "generated fold lacks compiler provenance"}

      "fold" not in type.derivations ->
        {:error, "generated fold was not requested"}

      Enum.any?(type.constructors, & &1.gadt?) ->
        {:error, "generated fold targets a GADT"}

      scheme.type != expression.type ->
        {:error, "generated fold scheme is inconsistent"}

      length(expression.handler_names) != length(type.constructors) ->
        {:error, "generated fold handler count is inconsistent"}

      true ->
        :ok
    end
  end

  defp verify_definition(definition, globals, data) do
    with :ok <- verify_condition_definition(definition),
         result <- verify_expression(definition.expression, globals, data) do
      case result do
        {:ok, type} ->
          if instance?(type, definition.scheme),
            do: :ok,
            else: {:error, "definition scheme is inconsistent"}

        error ->
          error
      end
    end
  end

  defp verify_condition_definition(%{kind: :condition, condition: evidence}) do
    if Condition.valid_evidence?(evidence, :definition),
      do: :ok,
      else: {:error, "condition definition evidence is invalid"}
  end

  defp verify_condition_definition(_definition), do: :ok

  defp verify_expression(%{tag: :integer, type: :integer}, _environment, _data),
    do: {:ok, :integer}

  defp verify_expression(%{tag: :boolean, type: :boolean}, _environment, _data),
    do: {:ok, :boolean}

  defp verify_expression(
         %{tag: :unary, operator: :not, operand: operand, type: :boolean},
         environment,
         data
       ) do
    with {:ok, :boolean} <- verify_expression(operand, environment, data), do: {:ok, :boolean}
  end

  defp verify_expression(
         %{tag: :unary, operator: :negate, operand: operand, type: :integer},
         environment,
         data
       ) do
    with {:ok, :integer} <- verify_expression(operand, environment, data), do: {:ok, :integer}
  end

  defp verify_expression(
         %{tag: :binary, operator: operator, left: left, right: right, type: result_type},
         environment,
         data
       ) do
    expected =
      cond do
        operator in [:and, :or] -> {:boolean, :boolean}
        operator in [:add, :subtract, :multiply] -> {:integer, :integer}
        operator in [:less, :less_equal, :greater, :greater_equal] -> {:integer, :boolean}
        operator in [:equal, :not_equal] -> {Map.get(left, :type), :boolean}
        true -> :invalid
      end

    case expected do
      {operand_type, ^result_type} when operand_type in [:integer, :boolean] ->
        with {:ok, ^operand_type} <- verify_expression(left, environment, data),
             {:ok, ^operand_type} <- verify_expression(right, environment, data) do
          {:ok, result_type}
        else
          _ -> {:error, "condition operator operand types are inconsistent"}
        end

      _ ->
        {:error, "condition operator metadata is inconsistent"}
    end
  end

  defp verify_expression(%{tag: :variable, name: name, type: type}, environment, _data) do
    case Map.fetch(environment, name) do
      {:ok, %Scheme{variables: [], type: ^type}} ->
        {:ok, type}

      {:ok, %Scheme{} = scheme} ->
        if instance?(type, scheme),
          do: {:ok, type},
          else: {:error, "variable type is not an instance of its scheme"}

      :error ->
        {:error, "unbound core variable #{name}"}
    end
  end

  defp verify_expression(
         %{
           tag: :function,
           parameter: parameter,
           body: body,
           type: {:function, parameter_type, result_type}
         },
         environment,
         data
       ) do
    with {:ok, ^result_type} <-
           verify_expression(
             body,
             Map.put(environment, parameter, Scheme.mono(parameter_type)),
             data
           ) do
      {:ok, {:function, parameter_type, result_type}}
    else
      {:ok, actual} ->
        {:error, "function result #{inspect(actual)} does not match #{inspect(result_type)}"}

      error ->
        error
    end
  end

  defp verify_expression(
         %{tag: :call, callee: callee, arguments: arguments, type: result_type},
         environment,
         data
       ) do
    with {:ok, callee_type} <- verify_expression(callee, environment, data),
         {:ok, final_type} <- verify_arguments(arguments, callee_type, environment, data),
         true <- final_type == result_type do
      {:ok, result_type}
    else
      false -> {:error, "call result annotation is inconsistent"}
      {:error, _} = error -> error
    end
  end

  defp verify_expression(
         %{tag: :let, name: name, value: value, body: body, scheme: scheme, type: result_type},
         environment,
         data
       ) do
    with {:ok, value_type} <- verify_expression(value, environment, data),
         true <- instance?(value_type, scheme),
         {:ok, ^result_type} <- verify_expression(body, Map.put(environment, name, scheme), data) do
      {:ok, result_type}
    else
      false ->
        {:error, "let scheme is inconsistent with its value"}

      {:ok, actual} ->
        {:error, "let body #{inspect(actual)} does not match #{inspect(result_type)}"}

      error ->
        error
    end
  end

  defp verify_expression(
         %{tag: :tuple, elements: elements, type: {:tuple, types}},
         environment,
         data
       )
       when length(elements) == length(types) do
    verify_typed_expressions(elements, types, environment, data)
    |> case do
      :ok -> {:ok, {:tuple, types}}
      error -> error
    end
  end

  defp verify_expression(%{tag: :annotate, expression: expression, type: type}, environment, data) do
    case verify_expression(expression, environment, data) do
      {:ok, ^type} -> {:ok, type}
      {:ok, actual} -> {:error, "annotation #{inspect(type)} does not match #{inspect(actual)}"}
      error -> error
    end
  end

  defp verify_expression(
         %{tag: :construct, constructor: constructor, arguments: arguments, type: type},
         environment,
         data
       ) do
    with %{id: id} <- Map.get(data.constructors, constructor.id),
         true <- id == constructor.id,
         true <- type == constructor.result,
         true <- length(arguments) == length(constructor.fields),
         :ok <- verify_construct_arguments(arguments, constructor.fields, environment, data) do
      {:ok, type}
    else
      nil -> {:error, "construction uses an unknown or hidden constructor"}
      false -> {:error, "construction metadata is inconsistent"}
      {:error, _} = error -> error
    end
  end

  defp verify_expression(
         %{
           tag: :match,
           scrutinee: scrutinee,
           clauses: clauses,
           decision_tree: decision,
           type: result_type
         },
         environment,
         data
       ) do
    with {:ok, scrutinee_type} <- verify_expression(scrutinee, environment, data),
         true <-
           valid_decision?(decision, clauses),
         :ok <- verify_clauses(clauses, scrutinee_type, result_type, environment, data),
         coverage <- Coverage.check!(clauses, scrutinee_type, data),
         true <- coverage.clauses == clauses do
      {:ok, result_type}
    else
      false -> {:error, "match decision tree is missing or inconsistent"}
      {:error, _} = error -> error
      _ -> {:error, "match coverage invariant is inconsistent"}
    end
  end

  defp verify_expression(expression, _environment, _data),
    do: {:error, "malformed typed-core node #{inspect(expression)}"}

  defp valid_decision?(%{tag: :ordered_decision, exhaustive?: true, clauses: clauses}, clauses),
    do: true

  defp valid_decision?(
         %{
           tag: :ordered_guard_tree,
           exhaustive?: true,
           guard_once?: true,
           false_falls_through?: true,
           clauses: clauses
         },
         clauses
       ),
       do: true

  defp valid_decision?(_decision, _clauses), do: false

  defp verify_construct_arguments(arguments, fields, environment, data) do
    ordered = Enum.sort_by(arguments, &Map.get(&1, :field_index, 0))

    expressions =
      Enum.map(ordered, fn
        %{expression: expression} -> expression
        expression -> expression
      end)

    verify_typed_expressions(expressions, Enum.map(fields, & &1.type), environment, data)
  end

  defp verify_clauses(clauses, scrutinee_type, result_type, environment, data) do
    Enum.reduce_while(clauses, :ok, fn clause, :ok ->
      with {:ok, bindings} <- verify_pattern(clause.pattern, scrutinee_type, data, %{}),
           branch_environment <-
             Map.merge(
               environment,
               Map.new(bindings, fn {name, type} -> {name, Scheme.mono(type)} end)
             ),
           :ok <- verify_guard(clause.guard, branch_environment, data),
           {:ok, body_type} <- verify_expression(clause.body, branch_environment, data),
           true <- body_type == Type.refine(result_type, clause.refinements) do
        {:cont, :ok}
      else
        false -> {:halt, {:error, "match branch result is inconsistent"}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp verify_guard(nil, _environment, _data), do: :ok

  defp verify_guard(guard, environment, data) do
    case verify_expression(guard, environment, data) do
      {:ok, :boolean} ->
        case Map.get(guard, :condition_evidence) do
          nil ->
            :ok

          evidence ->
            if Condition.valid_evidence?(evidence, :guard),
              do: :ok,
              else: {:error, "match guard condition evidence is invalid"}
        end

      {:ok, _other} ->
        {:error, "match guard is not Boolean"}

      error ->
        error
    end
  end

  defp verify_pattern(%{tag: :wildcard, type: type}, type, _data, bindings), do: {:ok, bindings}

  defp verify_pattern(%{tag: :bind, name: name, type: type}, type, _data, bindings),
    do: put_pattern_binding(bindings, name, type)

  defp verify_pattern(%{tag: :integer, type: :integer}, :integer, _data, bindings),
    do: {:ok, bindings}

  defp verify_pattern(%{tag: :boolean, type: :boolean}, :boolean, _data, bindings),
    do: {:ok, bindings}

  defp verify_pattern(%{tag: :tuple, elements: patterns}, {:tuple, types}, data, bindings)
       when length(patterns) == length(types),
       do: verify_patterns(patterns, types, data, bindings)

  defp verify_pattern(
         %{tag: :constructor, constructor: constructor, patterns: patterns},
         {:nominal, id, _arguments},
         data,
         bindings
       ) do
    with %{type_id: ^id} <- Map.get(data.constructors, constructor.id),
         true <- length(patterns) == length(constructor.fields) do
      verify_patterns(patterns, Enum.map(constructor.fields, & &1.type), data, bindings)
    else
      _ -> {:error, "constructor pattern metadata is inconsistent"}
    end
  end

  defp verify_pattern(%{tag: :as, pattern: pattern, name: name, type: type}, type, data, bindings) do
    with {:ok, bindings} <- verify_pattern(pattern, type, data, bindings) do
      put_pattern_binding(bindings, name, type)
    end
  end

  defp verify_pattern(%{tag: :or, alternatives: alternatives}, type, data, bindings) do
    alternatives
    |> Enum.map(&verify_pattern(&1, type, data, bindings))
    |> case do
      [] ->
        {:error, "empty or pattern"}

      [{:ok, first} | rest] ->
        if Enum.all?(rest, &(&1 == {:ok, first})),
          do: {:ok, first},
          else: {:error, "or bindings differ"}

      [error | _] ->
        error
    end
  end

  defp verify_pattern(_pattern, _type, _data, _bindings), do: {:error, "malformed typed pattern"}

  defp verify_patterns(patterns, types, data, bindings) do
    Enum.zip(patterns, types)
    |> Enum.reduce_while({:ok, bindings}, fn {pattern, type}, {:ok, current} ->
      case verify_pattern(pattern, type, data, current) do
        {:ok, next} -> {:cont, {:ok, next}}
        error -> {:halt, error}
      end
    end)
  end

  defp put_pattern_binding(bindings, name, type) do
    if Map.has_key?(bindings, name),
      do: {:error, "pattern binds #{name} twice"},
      else: {:ok, Map.put(bindings, name, type)}
  end

  defp verify_typed_expressions(expressions, types, environment, data) do
    Enum.zip(expressions, types)
    |> Enum.reduce_while(:ok, fn {expression, expected}, :ok ->
      case verify_expression(expression, environment, data) do
        {:ok, ^expected} ->
          {:cont, :ok}

        {:ok, actual} ->
          {:halt, {:error, "expression #{inspect(actual)} does not match #{inspect(expected)}"}}

        error ->
          {:halt, error}
      end
    end)
  end

  defp verify_arguments([], type, _environment, _data), do: {:ok, type}

  defp verify_arguments([argument | rest], {:function, parameter, result}, environment, data) do
    with {:ok, ^parameter} <- verify_expression(argument, environment, data) do
      verify_arguments(rest, result, environment, data)
    else
      {:ok, actual} ->
        {:error, "argument #{inspect(actual)} does not match #{inspect(parameter)}"}

      error ->
        error
    end
  end

  defp verify_arguments(_arguments, _type, _environment, _data),
    do: {:error, "non-function used as a callee"}

  defp instance?(actual, %Scheme{variables: variables, type: pattern}) do
    case match_instance(actual, pattern, MapSet.new(variables), %{}) do
      {:ok, _bindings} -> true
      :error -> false
    end
  end

  defp match_instance(actual, {:var, id}, variables, bindings) do
    if MapSet.member?(variables, id) do
      case Map.fetch(bindings, id) do
        {:ok, ^actual} -> {:ok, bindings}
        {:ok, _other} -> :error
        :error -> {:ok, Map.put(bindings, id, actual)}
      end
    else
      if actual == {:var, id}, do: {:ok, bindings}, else: :error
    end
  end

  defp match_instance({:function, ai, ao}, {:function, pi, po}, variables, bindings) do
    with {:ok, bindings} <- match_instance(ai, pi, variables, bindings),
         do: match_instance(ao, po, variables, bindings)
  end

  defp match_instance({:tuple, actual}, {:tuple, pattern}, variables, bindings)
       when length(actual) == length(pattern),
       do: match_lists(actual, pattern, variables, bindings)

  defp match_instance({:nominal, id, actual}, {:nominal, id, pattern}, variables, bindings)
       when length(actual) == length(pattern),
       do: match_lists(actual, pattern, variables, bindings)

  defp match_instance(type, type, _variables, bindings), do: {:ok, bindings}
  defp match_instance(_actual, _pattern, _variables, _bindings), do: :error

  defp match_lists(actual, pattern, variables, bindings) do
    Enum.zip(actual, pattern)
    |> Enum.reduce_while({:ok, bindings}, fn {actual_type, pattern_type}, {:ok, current} ->
      case match_instance(actual_type, pattern_type, variables, current) do
        {:ok, next} -> {:cont, {:ok, next}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp empty_data, do: %{constructors: %{}, types_by_id: %{}}
end
