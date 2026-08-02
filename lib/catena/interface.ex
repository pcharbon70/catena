defmodule Catena.Interface do
  @moduledoc "Deterministic, layout-free C002/C003 module interfaces."

  alias Catena.{CanonicalJSON, Condition, Diagnostic}
  alias Catena.Type.Scheme

  @versions ~w(0.2 0.3)

  @spec build(map()) :: map()
  def build(core) do
    values =
      core.definitions
      |> Enum.filter(&(&1.name in core.exports))
      |> Enum.map(fn definition ->
        value = %{"name" => definition.name, "scheme" => encode_scheme(definition.scheme)}

        case Map.get(definition, :condition) do
          nil -> value
          evidence -> Map.put(value, "condition", Condition.encode_evidence(evidence))
        end
      end)
      |> Enum.sort_by(& &1["name"])

    types =
      core.data.types
      |> Enum.filter(&(&1.visibility in [:transparent, :abstract]))
      |> Enum.map(&encode_datatype/1)
      |> Enum.sort_by(& &1["id"])

    payload = %{
      "format" => "catena-interface",
      "version" => if(core.frontend_version == "0.3", do: "0.3", else: "0.2"),
      "origin" => core.origin,
      "module" => core.module,
      "types" => types,
      "values" => values
    }

    Map.put(payload, "digest", digest(payload))
  end

  @spec encode(map()) :: binary()
  def encode(interface), do: CanonicalJSON.encode(interface) <> "\n"

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(binary) when is_binary(binary) do
    with {:ok, value} <- JSON.decode(binary),
         true <- is_map(value),
         version when version in @versions <- Map.get(value, "version"),
         "catena-interface" <- Map.get(value, "format"),
         digest when is_binary(digest) <- Map.get(value, "digest"),
         payload = Map.delete(value, "digest"),
         true <- secure_equal?(digest, digest(payload)),
         {:ok, types} <- decode_types(Map.get(value, "types"), value),
         {:ok, values} <- decode_values(Map.get(value, "values"), types) do
      {:ok,
       %{
         version: version,
         origin: Map.fetch!(value, "origin"),
         module: Map.fetch!(value, "module"),
         digest: digest,
         types: types,
         values: values
       }}
    else
      false -> error("interface digest does not match its contents")
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      _ -> error("malformed or unsupported Catena interface")
    end
  rescue
    _error -> error("malformed or unsupported Catena interface")
  end

  defp encode_datatype(type) do
    base = %{
      "id" => type.id,
      "origin" => type.origin,
      "module" => type.module,
      "name" => type.name,
      "arity" => type.arity,
      "kind" => List.duplicate("Type", type.arity) ++ ["Type"],
      "visibility" => Atom.to_string(type.visibility),
      "inhabitation" => Atom.to_string(type.inhabitation),
      "variance" => Enum.map(type.variance, &Atom.to_string/1),
      "positive" => type.positive?,
      "regular" => type.regular?
    }

    if type.visibility == :transparent do
      Map.put(base, "constructors", Enum.map(type.constructors, &encode_constructor/1))
    else
      base
    end
  end

  defp encode_constructor(constructor) do
    %{
      "id" => constructor.id,
      "name" => constructor.name,
      "index" => constructor.index,
      "field_style" => Atom.to_string(constructor.field_style),
      "fields" =>
        Enum.map(constructor.fields, fn field ->
          %{"name" => field.name, "index" => field.index, "type" => encode_type(field.type)}
        end),
      "variables" => constructor.variables,
      "universal_count" => constructor.universal_count,
      "existential_ids" => constructor.existential_ids |> MapSet.to_list() |> Enum.sort(),
      "result" => encode_type(constructor.result),
      "gadt" => constructor.gadt?
    }
  end

  defp encode_scheme(%Scheme{variables: variables, type: type}),
    do: %{"variables" => variables, "type" => encode_type(type)}

  defp encode_type(:integer), do: %{"tag" => "integer"}
  defp encode_type(:boolean), do: %{"tag" => "boolean"}
  defp encode_type({:var, id}), do: %{"tag" => "variable", "id" => id}
  defp encode_type({:skolem, id}), do: %{"tag" => "skolem", "id" => inspect(id)}

  defp encode_type({:function, parameter, result}),
    do: %{
      "tag" => "function",
      "parameter" => encode_type(parameter),
      "result" => encode_type(result)
    }

  defp encode_type({:tuple, elements}),
    do: %{"tag" => "tuple", "elements" => Enum.map(elements, &encode_type/1)}

  defp encode_type({:nominal, id, arguments}),
    do: %{"tag" => "nominal", "id" => id, "arguments" => Enum.map(arguments, &encode_type/1)}

  defp decode_types(types, interface) when is_list(types) do
    decoded =
      Enum.map(types, fn type ->
        visibility = decode_visibility(Map.get(type, "visibility"))

        base = %{
          id: Map.fetch!(type, "id"),
          origin: Map.fetch!(type, "origin"),
          module: Map.fetch!(type, "module"),
          name: Map.fetch!(type, "name"),
          arity: Map.fetch!(type, "arity"),
          visibility: visibility,
          inhabitation: decode_inhabitation(Map.get(type, "inhabitation")),
          variance: Enum.map(Map.get(type, "variance", []), &decode_variance/1),
          positive?: Map.get(type, "positive", false),
          regular?: Map.get(type, "regular", false),
          derivations: [],
          imported?: true,
          path: "interface://#{interface["module"]}/#{type["name"]}"
        }

        constructors =
          Enum.map(Map.get(type, "constructors", []), fn constructor ->
            %{
              id: Map.fetch!(constructor, "id"),
              type_id: Map.fetch!(type, "id"),
              type_name: Map.fetch!(type, "name"),
              name: Map.fetch!(constructor, "name"),
              qualified: "#{type["module"]}.#{type["name"]}.#{constructor["name"]}",
              index: Map.fetch!(constructor, "index"),
              fields:
                Enum.map(Map.fetch!(constructor, "fields"), fn field ->
                  %{
                    name: Map.get(field, "name"),
                    index: Map.fetch!(field, "index"),
                    type: decode_type(Map.fetch!(field, "type"))
                  }
                end),
              field_style: String.to_existing_atom(Map.fetch!(constructor, "field_style")),
              variables: Map.fetch!(constructor, "variables"),
              universal_count: Map.fetch!(constructor, "universal_count"),
              existential_ids: MapSet.new(Map.fetch!(constructor, "existential_ids")),
              result: decode_type(Map.fetch!(constructor, "result")),
              gadt?: Map.fetch!(constructor, "gadt"),
              visibility: visibility,
              path: "interface://#{interface["module"]}/#{constructor["name"]}"
            }
          end)

        Map.put(base, :constructors, constructors)
      end)

    {:ok, decoded}
  end

  defp decode_types(_, _interface), do: error("interface types must be a list")

  defp decode_values(values, _types) when is_list(values) do
    values
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {value, index}, {:ok, decoded} ->
      base = %{
        name: Map.fetch!(value, "name"),
        scheme: decode_scheme(Map.fetch!(value, "scheme"))
      }

      case Map.get(value, "condition") do
        nil ->
          {:cont, {:ok, [Map.put(base, :condition, nil) | decoded]}}

        encoded ->
          case Condition.decode_evidence(encoded, "$.values[#{index}].condition") do
            {:ok, evidence} ->
              if Condition.valid_for_scheme?(evidence, base.scheme) do
                {:cont, {:ok, [Map.put(base, :condition, evidence) | decoded]}}
              else
                {:halt,
                 {:error,
                  Diagnostic.new(
                    "CND005",
                    "condition evidence does not match its exported type scheme",
                    path: "$.values[#{index}].condition"
                  )}}
              end

            {:error, _} = error ->
              {:halt, error}
          end
      end
    end)
    |> case do
      {:ok, decoded} -> {:ok, Enum.reverse(decoded)}
      error -> error
    end
  end

  defp decode_values(_, _types), do: error("interface values must be a list")

  defp decode_scheme(%{"variables" => variables, "type" => type}),
    do: %Scheme{variables: variables, type: decode_type(type)}

  defp decode_type(%{"tag" => "integer"}), do: :integer
  defp decode_type(%{"tag" => "boolean"}), do: :boolean
  defp decode_type(%{"tag" => "variable", "id" => id}), do: {:var, id}
  defp decode_type(%{"tag" => "skolem", "id" => id}), do: {:skolem, id}

  defp decode_type(%{"tag" => "function", "parameter" => parameter, "result" => result}),
    do: {:function, decode_type(parameter), decode_type(result)}

  defp decode_type(%{"tag" => "tuple", "elements" => elements}),
    do: {:tuple, Enum.map(elements, &decode_type/1)}

  defp decode_type(%{"tag" => "nominal", "id" => id, "arguments" => arguments}),
    do: {:nominal, id, Enum.map(arguments, &decode_type/1)}

  defp decode_visibility("transparent"), do: :transparent
  defp decode_visibility("abstract"), do: :abstract
  defp decode_visibility(_), do: raise(ArgumentError, "invalid visibility")

  defp decode_inhabitation("empty"), do: :empty
  defp decode_inhabitation("inhabited"), do: :inhabited
  defp decode_inhabitation("unknown"), do: :unknown
  defp decode_inhabitation(_), do: :unknown

  defp decode_variance("phantom"), do: :phantom
  defp decode_variance("covariant"), do: :covariant
  defp decode_variance("contravariant"), do: :contravariant
  defp decode_variance("invariant"), do: :invariant
  defp decode_variance(_), do: raise(ArgumentError, "invalid variance")

  defp digest(payload),
    do: :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)

  defp secure_equal?(left, right) when byte_size(left) == byte_size(right) do
    left
    |> :binary.bin_to_list()
    |> Enum.zip(:binary.bin_to_list(right))
    |> Enum.reduce(0, fn {a, b}, acc -> Bitwise.bor(acc, Bitwise.bxor(a, b)) end) == 0
  end

  defp secure_equal?(_left, _right), do: false

  defp error(message), do: {:error, Diagnostic.new("A005", message)}
end
