defmodule Catena.Protocol.Contract do
  @moduledoc "Local protocol schema binding over verified exported kernel mailbox types."
  alias Catena.Kernel.{Interface, Type}
  alias Catena.{CanonicalJSON, LanguageVersion}

  def from_interface(binary, process_name, identity, version, roles)
      when is_binary(binary) and is_binary(process_name) and is_binary(identity) and
             is_binary(version) and is_map(roles) do
    with true <- identity != "" and LanguageVersion.valid_core_semver?(version),
         true <- valid_roles?(roles),
         {:ok, interface} <- Interface.decode(binary),
         %{} = process <- Enum.find(interface.processes, &(&1.name == process_name)),
         {:ok, request, response, reply_mailbox} <- shape(process.mailbox, roles) do
      schema = %{
        "format" => "catena-local-protocol-0.1.55",
        "identity" => identity,
        "version" => version,
        "interface_digest" => interface.digest,
        "process_identity" => process.identity,
        "mailbox" => Type.encode(process.mailbox),
        "roles" => Map.new(roles, fn {key, value} -> {Atom.to_string(key), value} end)
      }

      digest = :crypto.hash(:sha256, CanonicalJSON.encode(schema))

      {:ok,
       %{
         source: binary,
         process_name: process_name,
         identity: identity,
         version: version,
         roles: roles,
         digest: Base.encode16(digest, case: :lower),
         wire_identity: :binary.decode_unsigned(digest),
         request: request,
         response: response,
         reply_mailbox: reply_mailbox,
         types: Map.new(interface.types, &{&1.name, &1})
       }}
    else
      _ -> {:error, :invalid_protocol_contract}
    end
  end

  def from_interface(_, _, _, _, _), do: {:error, :invalid_protocol_contract}

  def validate(contract) when is_map(contract) do
    with {:ok, expected} <-
           from_interface(
             contract[:source],
             contract[:process_name],
             contract[:identity],
             contract[:version],
             contract[:roles]
           ),
         true <- expected == contract do
      :ok
    else
      _ -> {:error, :invalid_protocol_contract}
    end
  end

  def validate(_), do: {:error, :invalid_protocol_contract}

  def wire_roles(contract) do
    {:ok, Map.new(contract.roles, fn {role, label} -> {role, String.to_existing_atom(label)} end)}
  rescue
    ArgumentError -> {:error, :unlinked_protocol_schema}
  end

  def valid_payload?(contract, role, value) when role in [:request, :response],
    do: value?(value, Map.fetch!(contract, role), contract.types)

  defp valid_roles?(roles) do
    labels = Map.values(roles)

    Enum.sort(Map.keys(roles)) == [:negotiate, :ready, :reply, :request] and
      Enum.all?(labels, &(is_binary(&1) and byte_size(&1) > 0 and String.valid?(&1))) and
      length(Enum.uniq(labels)) == 4
  end

  defp shape({:variant, %{tail: nil, fields: fields}}, roles) when map_size(fields) == 2 do
    with {:tuple, [:integer, request, {:process, reply_mailbox}]} <- fields[roles.request],
         {:process, ^reply_mailbox} <- fields[roles.negotiate],
         {:variant, %{tail: nil, fields: replies}} when map_size(replies) == 2 <- reply_mailbox,
         :integer <- replies[roles.ready],
         {:tuple, [:integer, response]} <- replies[roles.reply] do
      {:ok, request, response, reply_mailbox}
    else
      _ -> :error
    end
  end

  defp shape(_, _), do: :error

  defp value?(value, :integer, _), do: is_integer(value)
  defp value?(value, :boolean, _), do: is_boolean(value)
  defp value?(:unit, :unit, _), do: true
  defp value?(value, {:process, _}, _), do: is_pid(value) and node(value) == node()

  defp value?(value, {:tuple, types}, declarations)
       when is_tuple(value) and tuple_size(value) == length(types),
       do: pairwise?(Tuple.to_list(value), types, declarations)

  defp value?(
         {:catena_variant, label, payload},
         {:variant, %{fields: fields, tail: nil}},
         declarations
       ) do
    label = if is_atom(label), do: Atom.to_string(label), else: nil

    case Map.fetch(fields, label) do
      {:ok, type} -> value?(payload, type, declarations)
      :error -> false
    end
  end

  defp value?(value, {:record, %{fields: fields, tail: nil}}, declarations) when is_map(value) do
    Enum.all?(Map.keys(value), &is_atom/1) and map_size(value) == map_size(fields) and
      Enum.all?(value, fn {label, field} ->
        case Map.fetch(fields, Atom.to_string(label)) do
          {:ok, type} -> value?(field, type, declarations)
          :error -> false
        end
      end)
  end

  defp value?({:catena_constructor, name, fields}, {:nominal, type_name, arguments}, declarations)
       when is_atom(name) and is_tuple(fields) do
    with %{} = type <- declarations[type_name],
         %{} = constructor <- Enum.find(type.constructors, &(&1.name == Atom.to_string(name))),
         true <- tuple_size(fields) == length(constructor.fields) do
      substitutions = Map.new(Enum.zip(type.parameters, arguments))

      pairwise?(
        Tuple.to_list(fields),
        Enum.map(constructor.fields, &Type.substitute(&1, substitutions)),
        declarations
      )
    else
      _ -> false
    end
  end

  defp value?(_, _, _), do: false

  defp pairwise?(values, types, declarations),
    do:
      Enum.zip(values, types)
      |> Enum.all?(fn {value, type} -> value?(value, type, declarations) end)
end
