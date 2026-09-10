defmodule Catena.Distribution.Wire do
  @moduledoc "Bounded canonical wire framing for checked local-protocol payloads."

  alias Catena.{CanonicalJSON, Resource.Budget}
  alias Catena.Distribution.Contract, as: DistributionContract
  alias Catena.Protocol.Contract, as: ProtocolContract

  @max_frame_bytes 1_048_576
  @max_sequence 9_007_199_254_740_991

  def profile,
    do: %{
      format: :catena_wire,
      version: 1,
      max_frame_bytes: @max_frame_bytes,
      max_nodes: 16_384,
      max_depth: 64,
      integer_encoding: :canonical_decimal,
      float_encoding: :ieee_754_binary64_hex,
      bytes_encoding: :base64,
      unknown_fields: :reject
    }

  def encode(contract, peer, role, sequence, message_id, payload)
      when role in [:request, :response] do
    with :ok <- DistributionContract.validate(contract),
         true <- Map.has_key?(contract.peers, peer),
         true <- sequence in 0..@max_sequence,
         true <- message_id?(message_id),
         true <- ProtocolContract.valid_payload?(contract.local, role, payload),
         {:ok, encoded} <- encode_value(type(contract, role), payload, contract.local.types, 0) do
      frame = %{
        "format" => "catena-wire",
        "version" => 1,
        "contract" => contract.endpoint_digest,
        "protocol" => contract.local.digest,
        "sender" => contract.node,
        "receiver" => peer,
        "service" => contract.service,
        "package" => contract.package_digest,
        "role" => Atom.to_string(role),
        "sequence" => sequence,
        "message_id" => message_id,
        "payload" => encoded
      }

      bytes = CanonicalJSON.encode(frame)
      if byte_size(bytes) <= @max_frame_bytes, do: {:ok, bytes}, else: {:error, :frame_too_large}
    else
      false -> {:error, :invalid_remote_payload}
      {:error, _} = error -> error
      _ -> {:error, :invalid_remote_payload}
    end
  rescue
    _ -> {:error, :invalid_remote_payload}
  end

  def encode(_, _, _, _, _, _), do: {:error, :invalid_remote_payload}

  def decode(contract, peer, role, bytes)
      when role in [:request, :response] and is_binary(bytes) do
    with :ok <- DistributionContract.validate(contract),
         true <- byte_size(bytes) <= @max_frame_bytes,
         {:ok, frame} <- JSON.decode(bytes),
         true <- CanonicalJSON.encode(frame) == bytes,
         true <- exact_frame?(frame),
         true <- frame["format"] == "catena-wire" and frame["version"] == 1,
         true <-
           frame["contract"] ==
             peer_endpoint_digest(contract, peer, frame["service"], frame["package"]),
         true <- frame["protocol"] == contract.local.digest,
         true <- frame["sender"] == peer and frame["receiver"] == contract.node,
         true <- frame["service"] in contract.peers[peer].services,
         true <- frame["package"] in contract.peers[peer].package_digests,
         true <- frame["role"] == Atom.to_string(role),
         true <- frame["sequence"] in 0..@max_sequence and message_id?(frame["message_id"]),
         true <- nodes(frame) <= profile().max_nodes,
         :ok <- Budget.validate_tree(frame),
         {:ok, payload} <-
           decode_value(type(contract, role), frame["payload"], contract.local.types, 0),
         true <- ProtocolContract.valid_payload?(contract.local, role, payload) do
      {:ok,
       %{
         sequence: frame["sequence"],
         message_id: frame["message_id"],
         payload: payload,
         digest: DistributionContract.hash(bytes)
       }}
    else
      false -> {:error, :invalid_remote_frame}
      {:error, _} = error -> error
      _ -> {:error, :invalid_remote_frame}
    end
  rescue
    _ -> {:error, :invalid_remote_frame}
  end

  def decode(_, _, _, _), do: {:error, :invalid_remote_frame}

  def hello(contract) do
    CanonicalJSON.encode(%{
      "format" => "catena-distribution-hello",
      "version" => 1,
      "node" => contract.node,
      "service" => contract.service,
      "package" => contract.package_digest,
      "protocol" => contract.local.digest,
      "endpoint" => contract.endpoint_digest
    })
  end

  def decode_hello(bytes) when is_binary(bytes) and byte_size(bytes) <= 4096 do
    with {:ok, value} <- JSON.decode(bytes),
         true <- is_map(value),
         true <- CanonicalJSON.encode(value) == bytes,
         true <-
           Map.keys(value) |> Enum.sort() ==
             ~w(endpoint format node package protocol service version),
         true <- value["format"] == "catena-distribution-hello" and value["version"] == 1 do
      {:ok, value}
    else
      _ -> {:error, :invalid_remote_hello}
    end
  end

  def decode_hello(_), do: {:error, :invalid_remote_hello}

  defp peer_endpoint_digest(contract, peer, service, package),
    do:
      DistributionContract.endpoint_digest(
        contract.version,
        peer,
        service,
        package,
        contract.local.digest
      )

  defp exact_frame?(frame) when is_map(frame),
    do:
      Map.keys(frame) |> Enum.sort() ==
        ~w(contract format message_id package payload protocol receiver role sender sequence service version)

  defp exact_frame?(_), do: false
  defp type(contract, :request), do: contract.local.request
  defp type(contract, :response), do: contract.local.response
  defp message_id?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)

  defp encode_value(type, value, declarations, depth),
    do: visit(:encode, type, value, declarations, depth)

  defp decode_value(type, value, declarations, depth),
    do: visit(:decode, type, value, declarations, depth)

  defp visit(_, _, _, _, depth) when depth > 64, do: {:error, :wire_budget_exhausted}

  defp visit(:encode, :integer, value, _, _) when is_integer(value) do
    encoded = Integer.to_string(value)

    if byte_size(String.trim_leading(encoded, "-")) <= 4096,
      do: {:ok, %{"t" => "i", "v" => encoded}},
      else: {:error, :wire_budget_exhausted}
  end

  defp visit(:decode, :integer, %{"t" => "i", "v" => value}, _, _) when is_binary(value) do
    if byte_size(String.trim_leading(value, "-")) <= 4096 and
         Regex.match?(~r/^-?(?:0|[1-9][0-9]*)$/, value) do
      integer = String.to_integer(value)
      if Integer.to_string(integer) == value, do: {:ok, integer}, else: {:error, :invalid_integer}
    else
      {:error, :invalid_integer}
    end
  rescue
    _ -> {:error, :invalid_integer}
  end

  defp visit(:encode, :boolean, value, _, _) when is_boolean(value),
    do: {:ok, %{"t" => "b", "v" => value}}

  defp visit(:decode, :boolean, %{"t" => "b", "v" => value}, _, _) when is_boolean(value),
    do: {:ok, value}

  defp visit(:encode, :unit, :unit, _, _), do: {:ok, %{"t" => "u"}}
  defp visit(:decode, :unit, %{"t" => "u"}, _, _), do: {:ok, :unit}

  defp visit(:encode, :float, value, _, _) when is_float(value) do
    <<bits::unsigned-64>> = <<value::float-64>>
    <<_::1, exponent::11, _::52>> = <<bits::unsigned-64>>

    if exponent < 2047,
      do: {:ok, %{"t" => "f", "v" => Base.encode16(<<bits::64>>, case: :lower)}},
      else: {:error, :invalid_float}
  end

  defp visit(:decode, :float, %{"t" => "f", "v" => hex}, _, _) when is_binary(hex) do
    case Base.decode16(hex, case: :lower) do
      {:ok, <<bits::64>>} ->
        <<_::1, exponent::11, _::52>> = <<bits::64>>
        <<value::float-64>> = <<bits::64>>
        if exponent < 2047, do: {:ok, value}, else: {:error, :invalid_float}

      _ ->
        {:error, :invalid_float}
    end
  end

  defp visit(:encode, :text, value, _, _) when is_binary(value),
    do:
      if(String.valid?(value),
        do: {:ok, %{"t" => "t", "v" => value}},
        else: {:error, :invalid_text}
      )

  defp visit(:decode, :text, %{"t" => "t", "v" => value}, _, _) when is_binary(value),
    do: if(String.valid?(value), do: {:ok, value}, else: {:error, :invalid_text})

  defp visit(:encode, :bytes, value, _, _) when is_binary(value),
    do: {:ok, %{"t" => "y", "v" => Base.encode64(value)}}

  defp visit(:decode, :bytes, %{"t" => "y", "v" => value}, _, _) when is_binary(value) do
    case Base.decode64(value) do
      {:ok, bytes} -> {:ok, bytes}
      :error -> {:error, :invalid_bytes}
    end
  end

  defp visit(:encode, :character, value, _, _)
       when is_integer(value) and value >= 0 and value <= 0x10FFFF and value not in 0xD800..0xDFFF,
       do: {:ok, %{"t" => "c", "v" => value}}

  defp visit(:decode, :character, %{"t" => "c", "v" => value}, _, _)
       when is_integer(value) and value >= 0 and value <= 0x10FFFF and value not in 0xD800..0xDFFF,
       do: {:ok, value}

  defp visit(:decode, {:tuple, types}, %{"t" => "p", "v" => values}, declarations, depth),
    do: visit(:decode, {:tuple, types}, values, declarations, depth)

  defp visit(direction, {:tuple, types}, value, declarations, depth)
       when direction == :encode or is_list(value) do
    values = if direction == :encode and is_tuple(value), do: Tuple.to_list(value), else: value

    with true <- is_list(values) and length(values) == length(types),
         {:ok, converted} <- fields(direction, types, values, declarations, depth + 1) do
      {:ok,
       if(direction == :encode,
         do: %{"t" => "p", "v" => converted},
         else: List.to_tuple(converted)
       )}
    else
      _ -> {:error, :invalid_tuple}
    end
  end

  defp visit(:decode, {:record, _} = type, %{"t" => "r", "v" => value}, declarations, depth),
    do: visit(:decode, type, value, declarations, depth)

  defp visit(direction, {:record, %{fields: types, tail: nil}}, value, declarations, depth)
       when is_map(value) do
    labels = Enum.sort(Map.keys(types))

    input =
      if direction == :encode,
        do: Map.new(value, fn {k, v} -> {Atom.to_string(k), v} end),
        else: value

    with true <- Map.keys(input) |> Enum.sort() == labels,
         {:ok, converted} <-
           fields(
             direction,
             Enum.map(labels, &types[&1]),
             Enum.map(labels, &input[&1]),
             declarations,
             depth + 1
           ) do
      if direction == :encode,
        do: {:ok, %{"t" => "r", "v" => Map.new(Enum.zip(labels, converted))}},
        else: {:ok, Map.new(Enum.zip(Enum.map(labels, &String.to_existing_atom/1), converted))}
    else
      _ -> {:error, :invalid_record}
    end
  rescue
    _ -> {:error, :invalid_record}
  end

  defp visit(direction, {:variant, %{fields: types, tail: nil}}, value, declarations, depth) do
    case {direction, value} do
      {:encode, {:catena_variant, label, payload}} when is_atom(label) ->
        name = Atom.to_string(label)

        with {:ok, type} <- Map.fetch(types, name),
             {:ok, converted} <- visit(:encode, type, payload, declarations, depth + 1),
             do: {:ok, %{"t" => "v", "n" => name, "v" => converted}}

      {:decode, %{"t" => "v", "n" => name, "v" => payload}} when is_binary(name) ->
        with {:ok, type} <- Map.fetch(types, name),
             {:ok, converted} <- visit(:decode, type, payload, declarations, depth + 1),
             do: {:ok, {:catena_variant, String.to_existing_atom(name), converted}}

      _ ->
        {:error, :invalid_variant}
    end
  rescue
    _ -> {:error, :invalid_variant}
  end

  defp visit(
         :decode,
         {:nominal, name, arguments},
         %{"t" => "n", "n" => label, "v" => values},
         declarations,
         depth
       )
       when is_binary(label) and is_list(values) do
    with %{} = declaration <- declarations[name],
         %{} = constructor <- Enum.find(declaration.constructors, &(&1.name == label)),
         true <- length(declaration.parameters) == length(arguments),
         true <- length(constructor.fields) == length(values),
         substitutions <- Map.new(Enum.zip(declaration.parameters, arguments)),
         types <- Enum.map(constructor.fields, &Catena.Kernel.Type.substitute(&1, substitutions)),
         {:ok, converted} <- fields(:decode, types, values, declarations, depth + 1) do
      {:ok, {:catena_constructor, String.to_existing_atom(label), List.to_tuple(converted)}}
    else
      _ -> {:error, :invalid_nominal}
    end
  rescue
    _ -> {:error, :invalid_nominal}
  end

  defp visit(
         :encode,
         {:nominal, name, arguments},
         {:catena_constructor, label, values},
         declarations,
         depth
       )
       when is_atom(label) and is_tuple(values) do
    label = Atom.to_string(label)

    with %{} = declaration <- declarations[name],
         %{} = constructor <- Enum.find(declaration.constructors, &(&1.name == label)),
         true <- length(declaration.parameters) == length(arguments),
         true <- length(constructor.fields) == tuple_size(values),
         substitutions <- Map.new(Enum.zip(declaration.parameters, arguments)),
         types <- Enum.map(constructor.fields, &Catena.Kernel.Type.substitute(&1, substitutions)),
         {:ok, converted} <-
           fields(:encode, types, Tuple.to_list(values), declarations, depth + 1) do
      {:ok, %{"t" => "n", "n" => label, "v" => converted}}
    else
      _ -> {:error, :invalid_nominal}
    end
  end

  defp visit(_, _, _, _, _), do: {:error, :unsupported_wire_value}

  defp fields(direction, types, values, declarations, depth) do
    Enum.zip(types, values)
    |> Enum.reduce_while({:ok, []}, fn {type, value}, {:ok, acc} ->
      case visit(direction, type, value, declarations, depth) do
        {:ok, converted} -> {:cont, {:ok, [converted | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp nodes(value) when is_map(value),
    do: 1 + Enum.sum(Enum.map(value, fn {key, item} -> nodes(key) + nodes(item) end))

  defp nodes(value) when is_list(value), do: 1 + Enum.sum(Enum.map(value, &nodes/1))
  defp nodes(_), do: 1
end
