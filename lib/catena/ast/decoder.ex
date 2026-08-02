defmodule Catena.AST.Decoder do
  @moduledoc "Strict decoder for the temporary, versioned C001 JSON AST."

  alias Catena.Diagnostic

  @version "0.1"
  @module_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @value_name ~r/^[a-z][a-zA-Z0-9_]*$/
  @expression_tags ~w(integer boolean variable function call let tuple annotate)

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(json) when is_binary(json) do
    with {:ok, value} <- JSON.decode(json),
         :ok <- require_map(value, "$"),
         :ok <- version(value),
         {:ok, module_name} <- name(value, "module", @module_name, "$"),
         {:ok, exports} <- exports(value),
         {:ok, definitions} <- definitions(value) do
      {:ok,
       %{
         version: @version,
         module: module_name,
         exports: exports,
         definitions: definitions,
         source: Map.get(value, "source", "<catena-json>")
       }}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      {:error, reason} -> error("invalid JSON: #{inspect(reason)}", "$")
    end
  end

  defp version(%{"version" => @version}), do: :ok

  defp version(%{"version" => version}) do
    error("unsupported AST version #{inspect(version)}; expected #{@version}", "$.version")
  end

  defp version(_), do: error("missing AST version", "$.version")

  defp exports(%{"exports" => exports}) when is_list(exports) do
    if Enum.all?(exports, &(is_binary(&1) and Regex.match?(@value_name, &1))) and
         length(exports) == length(Enum.uniq(exports)) do
      {:ok, exports}
    else
      error("exports must be unique value names", "$.exports")
    end
  end

  defp exports(_), do: error("exports must be a list", "$.exports")

  defp definitions(%{"definitions" => definitions}) when is_list(definitions) do
    with {:ok, decoded} <- map_ok(definitions, &definition/2),
         true <- unique_names?(decoded) do
      {:ok, decoded}
    else
      false -> error("definition names must be unique", "$.definitions")
      {:error, _} = result -> result
    end
  end

  defp definitions(_), do: error("definitions must be a list", "$.definitions")

  defp definition(value, index) do
    path = "$.definitions[#{index}]"

    with :ok <- require_map(value, path),
         {:ok, name} <- name(value, "name", @value_name, path),
         {:ok, parameters} <- parameters(value, path),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok,
       %{
         name: name,
         parameters: parameters,
         signature: Map.get(value, "signature"),
         body: body,
         path: path
       }}
    end
  end

  defp parameters(value, path) do
    parameters = Map.get(value, "parameters", [])

    if is_list(parameters) and
         Enum.all?(parameters, &(is_binary(&1) and Regex.match?(@value_name, &1))) and
         length(parameters) == length(Enum.uniq(parameters)) do
      {:ok, parameters}
    else
      error("parameters must be unique value names", path <> ".parameters")
    end
  end

  defp expression(%{"tag" => tag} = value, path) when tag in @expression_tags do
    case tag do
      "integer" -> literal(value, "value", &is_integer/1, :integer, path)
      "boolean" -> literal(value, "value", &is_boolean/1, :boolean, path)
      "variable" -> variable(value, path)
      "function" -> function(value, path)
      "call" -> call(value, path)
      "let" -> let(value, path)
      "tuple" -> tuple(value, path)
      "annotate" -> annotate(value, path)
    end
  end

  defp expression(%{"tag" => tag}, path),
    do: error("unknown expression tag #{inspect(tag)}", path)

  defp expression(_, path), do: error("expression must contain a tag", path)

  defp literal(value, key, predicate, tag, path) do
    literal = Map.get(value, key)

    if predicate.(literal),
      do: {:ok, %{tag: tag, value: literal, path: path}},
      else: error("invalid #{tag} literal", path)
  end

  defp variable(value, path) do
    with {:ok, name} <- name(value, "name", @value_name, path) do
      {:ok, %{tag: :variable, name: name, path: path}}
    end
  end

  defp function(value, path) do
    with {:ok, parameter} <- name(value, "parameter", @value_name, path),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, %{tag: :function, parameter: parameter, body: body, path: path}}
    end
  end

  defp call(value, path) do
    arguments = Map.get(value, "arguments")

    with {:ok, callee} <- expression(Map.get(value, "callee"), path <> ".callee"),
         true <- is_list(arguments),
         {:ok, decoded} <-
           map_ok(arguments, fn item, index -> expression(item, "#{path}.arguments[#{index}]") end) do
      {:ok, %{tag: :call, callee: callee, arguments: decoded, path: path}}
    else
      false -> error("call arguments must be a list", path <> ".arguments")
      {:error, _} = result -> result
    end
  end

  defp let(value, path) do
    with {:ok, name} <- name(value, "name", @value_name, path),
         {:ok, bound} <- expression(Map.get(value, "value"), path <> ".value"),
         {:ok, body} <- expression(Map.get(value, "body"), path <> ".body") do
      {:ok, %{tag: :let, name: name, value: bound, body: body, path: path}}
    end
  end

  defp tuple(value, path) do
    elements = Map.get(value, "elements")

    if is_list(elements) do
      with {:ok, decoded} <-
             map_ok(elements, fn item, index -> expression(item, "#{path}.elements[#{index}]") end) do
        {:ok, %{tag: :tuple, elements: decoded, path: path}}
      end
    else
      error("tuple elements must be a list", path <> ".elements")
    end
  end

  defp annotate(value, path) do
    with {:ok, expression} <- expression(Map.get(value, "expression"), path <> ".expression"),
         signature when is_map(signature) <- Map.get(value, "signature") do
      {:ok, %{tag: :annotate, expression: expression, signature: signature, path: path}}
    else
      nil -> error("annotation requires a signature", path <> ".signature")
      {:error, _} = result -> result
    end
  end

  defp name(value, key, regex, path) do
    case Map.get(value, key) do
      name when is_binary(name) ->
        if Regex.match?(regex, name),
          do: {:ok, name},
          else: error("invalid #{key} #{inspect(name)}", path <> "." <> key)

      _ ->
        error("missing or invalid #{key}", path <> "." <> key)
    end
  end

  defp require_map(value, _path) when is_map(value), do: :ok
  defp require_map(_, path), do: error("expected an object", path)

  defp unique_names?(definitions),
    do: definitions |> Enum.map(& &1.name) |> then(&(length(&1) == length(Enum.uniq(&1))))

  defp map_ok(items, function) do
    items
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {item, index}, {:ok, acc} ->
      case function.(item, index) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp error(message, path), do: {:error, Diagnostic.new("T012", message, path: path)}
end
