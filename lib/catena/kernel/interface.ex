defmodule Catena.Kernel.Interface do
  @moduledoc "Canonical, digest-bound value and public-process interface for kernel 0.1.8."

  alias Catena.{CanonicalJSON, Diagnostic}
  alias Catena.Kernel.Type

  @module_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @value_name ~r/^[a-z][A-Za-z0-9_]*$/

  @spec build(map()) :: map()
  def build(core) do
    types =
      core.data.types
      |> Map.values()
      |> Enum.filter(&(&1.name in core.exports.types))
      |> Enum.map(fn data ->
        %{
          "name" => data.name,
          "parameters" => data.parameters,
          "constructors" =>
            data.constructors
            |> Enum.map(fn constructor ->
              %{
                "name" => constructor.name,
                "fields" => Enum.map(constructor.fields, &Type.encode/1)
              }
            end)
            |> Enum.sort_by(& &1["name"])
        }
      end)
      |> Enum.sort_by(& &1["name"])

    values =
      core.definitions
      |> Enum.filter(&(&1.name in core.exports.values))
      |> Enum.map(fn definition ->
        %{
          "name" => definition.name,
          "type" => Type.encode(definition.signature),
          "uses" => encode_effects(definition.uses),
          "arity" => definition.arity
        }
      end)
      |> Enum.sort_by(& &1["name"])

    processes =
      core.processes
      |> Enum.filter(&(&1.name in core.exports.processes))
      |> Enum.map(fn process ->
        %{
          "identity" => process.identity,
          "name" => process.name,
          "parameters" => Enum.map(process.parameters, &Type.encode(&1.type)),
          "mailbox" => Type.encode(process.mailbox),
          "arity" => length(process.parameters),
          "spawn_symbol" => process.spawn_symbol
        }
      end)
      |> Enum.sort_by(& &1["name"])

    payload = %{
      "format" => "catena-interface",
      "version" => "0.1.8",
      "edition" => "0.1",
      "language_revision" => "0.1.8",
      "previews" => [],
      "required_previews" => [],
      "origin" => core.origin,
      "module" => core.module,
      "types" => types,
      "values" => values,
      "processes" => processes
    }

    Map.put(payload, "digest", digest(payload))
  end

  @spec encode(map()) :: binary()
  def encode(interface), do: CanonicalJSON.encode(interface) <> "\n"

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(binary) when is_binary(binary) do
    with {:ok, value} <- JSON.decode(binary), do: decode_value(value)
  rescue
    _error -> error("malformed kernel interface")
  end

  @spec decode_value(term()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode_value(value) when is_map(value) do
    digest_value = Map.get(value, "digest")
    payload = Map.delete(value, "digest")

    with "catena-interface" <- Map.get(value, "format"),
         "0.1.8" <- Map.get(value, "version"),
         "0.1" <- Map.get(value, "edition"),
         "0.1.8" <- Map.get(value, "language_revision"),
         [] <- Map.get(value, "previews"),
         [] <- Map.get(value, "required_previews"),
         origin when is_binary(origin) and byte_size(origin) > 0 <- Map.get(value, "origin"),
         module when is_binary(module) <- Map.get(value, "module"),
         true <- Regex.match?(@module_name, module),
         digest when is_binary(digest) <- digest_value,
         true <- secure_equal?(digest, digest(payload)),
         {:ok, types} <- decode_type_entries(Map.get(value, "types")),
         {:ok, values} <- decode_values(Map.get(value, "values"), types),
         {:ok, processes} <- decode_processes(Map.get(value, "processes"), origin, module, types) do
      {:ok,
       %{
         format: :kernel_interface,
         version: "0.1.8",
         edition: "0.1",
         language_revision: "0.1.8",
         previews: [],
         required_previews: [],
         origin: origin,
         module: module,
         digest: digest,
         types: types,
         values: values,
         processes: processes
       }}
    else
      false -> error("kernel interface digest or canonical field order is invalid")
      _ -> error("malformed or unsupported kernel interface")
    end
  rescue
    _error -> error("malformed or unsupported kernel interface")
  end

  def decode_value(_value), do: error("malformed or unsupported kernel interface")

  defp decode_type_entries(types) when is_list(types) do
    with true <- types == Enum.sort_by(types, &Map.get(&1, "name")),
         true <- length(types) == length(Enum.uniq_by(types, &Map.get(&1, "name"))),
         {:ok, decoded} <- decode_each_type(types),
         declarations = Map.new(decoded, &{&1.name, &1}),
         true <- Enum.all?(decoded, &valid_type_declaration?(&1, declarations)),
         constructors =
           Enum.flat_map(decoded, &Enum.map(&1.constructors, fn item -> item.name end)),
         true <- length(constructors) == length(Enum.uniq(constructors)) do
      {:ok, decoded}
    else
      _ -> :error
    end
  end

  defp decode_type_entries(_types), do: :error

  defp decode_each_type(types) do
    Enum.reduce_while(types, {:ok, []}, fn type, {:ok, decoded} ->
      case decode_type_entry(type) do
        {:ok, entry} -> {:cont, {:ok, [entry | decoded]}}
        :error -> {:halt, :error}
      end
    end)
    |> reverse_ok()
  end

  defp decode_type_entry(%{
         "name" => name,
         "parameters" => parameters,
         "constructors" => constructors
       })
       when is_binary(name) and is_list(parameters) and is_list(constructors) do
    with true <- Regex.match?(@module_name, name),
         true <- Enum.all?(parameters, &(is_binary(&1) and Regex.match?(@value_name, &1))),
         true <- length(parameters) == length(Enum.uniq(parameters)),
         true <- constructors != [],
         true <- constructors == Enum.sort_by(constructors, &Map.get(&1, "name")),
         true <- length(constructors) == length(Enum.uniq_by(constructors, &Map.get(&1, "name"))),
         {:ok, constructors} <- decode_constructors(constructors) do
      {:ok, %{name: name, parameters: parameters, constructors: constructors}}
    else
      _ -> :error
    end
  end

  defp decode_type_entry(_type), do: :error

  defp decode_constructors(constructors) do
    Enum.reduce_while(constructors, {:ok, []}, fn
      %{"name" => name, "fields" => fields}, {:ok, decoded}
      when is_binary(name) and is_list(fields) ->
        with true <- Regex.match?(@module_name, name),
             {:ok, fields} <- decode_types(fields) do
          {:cont, {:ok, [%{name: name, fields: fields} | decoded]}}
        else
          _ -> {:halt, :error}
        end

      _constructor, _decoded ->
        {:halt, :error}
    end)
    |> reverse_ok()
  end

  defp decode_values(values, types) when is_list(values) do
    with true <- values == Enum.sort_by(values, &Map.get(&1, "name")),
         true <- length(values) == length(Enum.uniq_by(values, &Map.get(&1, "name"))) do
      Enum.reduce_while(values, {:ok, []}, fn value, {:ok, decoded} ->
        case decode_value_entry(value, types) do
          {:ok, entry} -> {:cont, {:ok, [entry | decoded]}}
          :error -> {:halt, :error}
        end
      end)
      |> reverse_ok()
    else
      _ -> :error
    end
  end

  defp decode_values(_values, _types), do: :error

  defp decode_value_entry_fields(name, encoded_type, encoded_effects, arity, types) do
    with true <- Regex.match?(@value_name, name),
         {:ok, type} <- Type.decode(encoded_type),
         {:ok, effects} <- decode_effects(encoded_effects),
         true <- function_arity(type) == arity,
         true <- known_type?(type, Map.new(types, &{&1.name, &1})) do
      {:ok, %{name: name, type: type, uses: effects, arity: arity}}
    else
      _ -> :error
    end
  end

  defp decode_value_entry(value, types) do
    case value do
      %{"name" => name, "type" => encoded_type, "uses" => encoded_effects, "arity" => arity}
      when is_binary(name) and is_integer(arity) and arity >= 0 ->
        decode_value_entry_fields(name, encoded_type, encoded_effects, arity, types)

      _ ->
        :error
    end
  end

  defp decode_processes(processes, origin, module, types) when is_list(processes) do
    with true <- processes == Enum.sort_by(processes, &Map.get(&1, "name")),
         true <- length(processes) == length(Enum.uniq_by(processes, &Map.get(&1, "name"))) do
      Enum.reduce_while(processes, {:ok, []}, fn process, {:ok, decoded} ->
        case decode_process(process, origin, module, types) do
          {:ok, entry} -> {:cont, {:ok, [entry | decoded]}}
          :error -> {:halt, :error}
        end
      end)
      |> reverse_ok()
    else
      _ -> :error
    end
  end

  defp decode_processes(_processes, _origin, _module, _types), do: :error

  defp decode_process(
         %{
           "identity" => identity,
           "name" => name,
           "parameters" => parameters,
           "mailbox" => mailbox,
           "arity" => arity,
           "spawn_symbol" => spawn_symbol
         },
         origin,
         module,
         types
       )
       when is_binary(identity) and is_binary(name) and is_list(parameters) and is_integer(arity) and
              is_binary(spawn_symbol) do
    with true <- Regex.match?(@module_name, name),
         true <- identity == origin <> "#" <> module <> "." <> name,
         true <- spawn_symbol == "__catena_spawn_#{name}",
         true <- arity == length(parameters),
         {:ok, parameters} <- decode_types(parameters),
         declarations = Map.new(types, &{&1.name, &1}),
         true <- Enum.all?(parameters, &(Type.closed?(&1) and sendable?(&1, declarations))),
         {:ok, mailbox} <- Type.decode(mailbox),
         true <- Type.closed?(mailbox) and sendable?(mailbox, declarations) do
      {:ok,
       %{
         identity: identity,
         name: name,
         parameters: parameters,
         mailbox: mailbox,
         arity: arity,
         spawn_symbol: spawn_symbol
       }}
    else
      _ -> :error
    end
  end

  defp decode_process(_process, _origin, _module, _types), do: :error

  defp decode_types(types) do
    Enum.reduce_while(types, {:ok, []}, fn encoded, {:ok, decoded} ->
      case Type.decode(encoded) do
        {:ok, type} -> {:cont, {:ok, [type | decoded]}}
        :error -> {:halt, :error}
      end
    end)
    |> reverse_ok()
  end

  defp valid_type_declaration?(declaration, declarations) do
    parameters = MapSet.new(declaration.parameters)

    Enum.all?(declaration.constructors, fn constructor ->
      Enum.all?(constructor.fields, fn field ->
        MapSet.subset?(Type.variables(field), parameters) and known_type?(field, declarations)
      end)
    end)
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

  defp sendable?(type, declarations), do: sendable?(type, declarations, MapSet.new())

  defp sendable?(type, _declarations, _seen) when type in [:integer, :boolean, :unit], do: true

  defp sendable?({:tuple, elements}, declarations, seen),
    do: Enum.all?(elements, &sendable?(&1, declarations, seen))

  defp sendable?({tag, %{fields: fields, tail: nil}}, declarations, seen)
       when tag in [:record, :variant],
       do: Enum.all?(fields, fn {_label, field} -> sendable?(field, declarations, seen) end)

  defp sendable?({:process, mailbox}, declarations, seen),
    do: Type.closed?(mailbox) and sendable?(mailbox, declarations, seen)

  defp sendable?({:nominal, name, arguments} = type, declarations, seen) do
    if MapSet.member?(seen, type) do
      true
    else
      with {:ok, declaration} <- Map.fetch(declarations, name),
           true <- length(arguments) == length(declaration.parameters) do
        substitution = declaration.parameters |> Enum.zip(arguments) |> Map.new()
        seen = MapSet.put(seen, type)

        Enum.all?(declaration.constructors, fn constructor ->
          Enum.all?(constructor.fields, fn field ->
            field |> Type.substitute(substitution) |> sendable?(declarations, seen)
          end)
        end)
      else
        _ -> false
      end
    end
  end

  defp sendable?(_type, _declarations, _seen), do: false

  defp encode_effects(effects) do
    Enum.map(effects, fn
      :process -> "Process"
      {:effect, name} -> %{"effect" => name}
    end)
  end

  defp decode_effects(effects) when is_list(effects) do
    Enum.reduce_while(effects, {:ok, []}, fn
      "Process", {:ok, decoded} ->
        {:cont, {:ok, [:process | decoded]}}

      %{"effect" => name}, {:ok, decoded} when is_binary(name) ->
        {:cont, {:ok, [{:effect, name} | decoded]}}

      _effect, _acc ->
        {:halt, :error}
    end)
    |> reverse_ok()
  end

  defp decode_effects(_effects), do: :error

  defp reverse_ok({:ok, values}), do: {:ok, Enum.reverse(values)}
  defp reverse_ok(:error), do: :error

  defp function_arity({:function, _parameter, _effects, result}), do: 1 + function_arity(result)
  defp function_arity(_type), do: 0

  defp digest(payload),
    do: :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)

  defp secure_equal?(left, right) when byte_size(left) == byte_size(right) do
    left
    |> :binary.bin_to_list()
    |> Enum.zip(:binary.bin_to_list(right))
    |> Enum.reduce(0, fn {a, b}, acc -> Bitwise.bor(acc, Bitwise.bxor(a, b)) end) == 0
  end

  defp secure_equal?(_left, _right), do: false
  defp error(message), do: {:error, Diagnostic.new("PRC004", message)}
end
