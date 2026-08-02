defmodule Catena.Type.Parser do
  @moduledoc "Decoder for type signatures embedded in the versioned JSON AST."

  alias Catena.{Diagnostic, Type}
  alias Catena.Type.Scheme

  @spec parse_scheme(map(), String.t(), map()) :: Scheme.t()
  def parse_scheme(signature, path, types \\ %{})

  def parse_scheme(signature, path, types) when is_map(signature) do
    variables = Map.get(signature, "forall", [])

    unless is_list(variables) and Enum.all?(variables, &is_binary/1) and
             length(variables) == length(Enum.uniq(variables)) do
      fail("T012", "forall must contain unique variable names", path)
    end

    ids = variables |> Enum.with_index() |> Map.new()
    type = parse(Map.get(signature, "type"), ids, path <> ".type", types)

    %Scheme{
      variables: Enum.to_list(0..length(variables)//1) |> Enum.take(length(variables)),
      type: type
    }
  end

  def parse_scheme(_signature, path, _types),
    do: fail("T012", "signature must be an object", path)

  @spec parse(map(), map(), String.t(), map()) :: Type.t()
  def parse(value, variables, path, types \\ %{})

  def parse(%{"tag" => "integer"}, _variables, _path, _types), do: :integer
  def parse(%{"tag" => "boolean"}, _variables, _path, _types), do: :boolean

  def parse(%{"tag" => "variable", "name" => name}, variables, path, _types) do
    case Map.fetch(variables, name) do
      {:ok, id} -> {:var, id}
      :error -> fail("T012", "unbound type variable #{inspect(name)}", path)
    end
  end

  def parse(
        %{"tag" => "function", "parameter" => parameter, "result" => result} = value,
        variables,
        path,
        types
      ) do
    effect = Map.get(value, "effect", [])

    if effect != [] do
      fail(
        "T010",
        "the executable C001 subset currently accepts only empty effect annotations",
        path <> ".effect"
      )
    end

    {:function, parse(parameter, variables, path <> ".parameter", types),
     parse(result, variables, path <> ".result", types)}
  end

  def parse(%{"tag" => "tuple", "elements" => elements}, variables, path, types)
      when is_list(elements) do
    {:tuple,
     elements
     |> Enum.with_index()
     |> Enum.map(fn {element, index} ->
       parse(element, variables, "#{path}.elements[#{index}]", types)
     end)}
  end

  def parse(%{"tag" => "named", "name" => name} = value, variables, path, types) do
    arguments = Map.get(value, "arguments", [])

    with %{id: id, arity: arity} <- Map.get(types, name),
         true <- is_list(arguments) and length(arguments) == arity do
      {:nominal, id,
       arguments
       |> Enum.with_index()
       |> Enum.map(fn {argument, index} ->
         parse(argument, variables, "#{path}.arguments[#{index}]", types)
       end)}
    else
      nil -> fail("A001", "unknown type #{inspect(name)}", path)
      false -> fail("A001", "type #{name} has the wrong number of arguments", path)
    end
  end

  def parse(_, _variables, path, _types), do: fail("T012", "unsupported or malformed type", path)

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
