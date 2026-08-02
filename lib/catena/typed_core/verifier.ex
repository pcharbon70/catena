defmodule Catena.TypedCore.Verifier do
  @moduledoc "An inference-independent structural type verifier for elaborated core."

  alias Catena.Type.Scheme

  @spec verify(map()) :: :ok | {:error, String.t()}
  def verify(module) do
    Enum.reduce_while(module.definitions, {:ok, %{}}, fn definition, {:ok, globals} ->
      case verify_expression(definition.expression, globals) do
        {:ok, _type} -> {:cont, {:ok, Map.put(globals, definition.name, definition.scheme)}}
        {:error, reason} -> {:halt, {:error, "#{definition.name}: #{reason}"}}
      end
    end)
    |> case do
      {:ok, _globals} -> :ok
      error -> error
    end
  end

  defp verify_expression(%{tag: :integer, type: :integer}, _environment), do: {:ok, :integer}
  defp verify_expression(%{tag: :boolean, type: :boolean}, _environment), do: {:ok, :boolean}

  defp verify_expression(%{tag: :variable, name: name, type: type}, environment) do
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
         environment
       ) do
    with {:ok, ^result_type} <-
           verify_expression(body, Map.put(environment, parameter, Scheme.mono(parameter_type))) do
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
         environment
       ) do
    with {:ok, callee_type} <- verify_expression(callee, environment),
         {:ok, final_type} <- verify_arguments(arguments, callee_type, environment),
         true <- final_type == result_type do
      {:ok, result_type}
    else
      false -> {:error, "call result annotation is inconsistent"}
      {:error, _} = error -> error
    end
  end

  defp verify_expression(
         %{tag: :let, name: name, value: value, body: body, scheme: scheme, type: result_type},
         environment
       ) do
    with {:ok, value_type} <- verify_expression(value, environment),
         true <- instance?(value_type, scheme),
         {:ok, ^result_type} <- verify_expression(body, Map.put(environment, name, scheme)) do
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

  defp verify_expression(%{tag: :tuple, elements: elements, type: {:tuple, types}}, environment)
       when length(elements) == length(types) do
    Enum.zip(elements, types)
    |> Enum.reduce_while(:ok, fn {element, expected}, :ok ->
      case verify_expression(element, environment) do
        {:ok, ^expected} ->
          {:cont, :ok}

        {:ok, actual} ->
          {:halt,
           {:error, "tuple element #{inspect(actual)} does not match #{inspect(expected)}"}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      :ok -> {:ok, {:tuple, types}}
      error -> error
    end
  end

  defp verify_expression(%{tag: :annotate, expression: expression, type: type}, environment) do
    case verify_expression(expression, environment) do
      {:ok, ^type} -> {:ok, type}
      {:ok, actual} -> {:error, "annotation #{inspect(type)} does not match #{inspect(actual)}"}
      error -> error
    end
  end

  defp verify_expression(expression, _environment),
    do: {:error, "malformed typed-core node #{inspect(expression)}"}

  defp verify_arguments([], type, _environment), do: {:ok, type}

  defp verify_arguments([argument | rest], {:function, parameter, result}, environment) do
    with {:ok, ^parameter} <- verify_expression(argument, environment) do
      verify_arguments(rest, result, environment)
    else
      {:ok, actual} ->
        {:error, "argument #{inspect(actual)} does not match #{inspect(parameter)}"}

      error ->
        error
    end
  end

  defp verify_arguments(_arguments, _type, _environment),
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

  defp match_instance(
         {:function, actual_input, actual_output},
         {:function, pattern_input, pattern_output},
         variables,
         bindings
       ) do
    with {:ok, bindings} <- match_instance(actual_input, pattern_input, variables, bindings) do
      match_instance(actual_output, pattern_output, variables, bindings)
    end
  end

  defp match_instance({:tuple, actual}, {:tuple, pattern}, variables, bindings)
       when length(actual) == length(pattern) do
    Enum.zip(actual, pattern)
    |> Enum.reduce_while({:ok, bindings}, fn {actual, pattern}, {:ok, current} ->
      case match_instance(actual, pattern, variables, current) do
        {:ok, next} -> {:cont, {:ok, next}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp match_instance(type, type, _variables, bindings), do: {:ok, bindings}
  defp match_instance(_actual, _pattern, _variables, _bindings), do: :error
end
