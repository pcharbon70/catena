defmodule Catena.ValueBoundary.Data do
  @moduledoc "Checked scalar/product/record/sum adapter; no external ABI promise."

  @scalars [:integer, :boolean, :unit, :float, :text, :character, :bytes]

  def decode(type, native, limits), do: convert(type, native, limits, :decode)
  def encode(type, semantic, limits), do: convert(type, semantic, limits, :encode)

  @doc false
  def scalar(type, value, limits) when type in @scalars, do: visit(type, value, limits, :decode)

  def valid_scalar?(type, value) when type in [:float, :text, :character, :bytes] do
    bytes = if is_binary(value), do: byte_size(value), else: 8
    match?({:ok, _}, Catena.ValueBoundary.Data.decode(type, value, %{nodes: 1, bytes: bytes}))
  end

  def lower(type, semantic, limits, annotation) do
    with {:ok, native} <- encode(type, semantic, limits),
         do: {:ok, :erl_parse.abstract(native, annotation)}
  end

  defp convert(type, value, %{nodes: nodes, bytes: bytes} = limits, direction)
       when is_integer(nodes) and nodes > 0 and is_integer(bytes) and bytes >= 0 and
              map_size(limits) == 2 do
    with :ok <- schema(type),
         {:ok, result, _} <- visit(type, value, limits, direction),
         do: {:ok, result}
  rescue
    _ in [ArgumentError, FunctionClauseError, Protocol.UndefinedError] ->
      {:error, :invalid_boundary_type}
  end

  defp convert(_, _, _, _), do: {:error, :invalid_validation_budget}

  defp schema(type) when type in @scalars, do: :ok

  defp schema({:tuple, types}) when is_list(types),
    do: schemas(types)

  defp schema({tag, fields}) when tag in [:record, :variant] and is_map(fields) do
    if Enum.all?(Map.keys(fields), &(is_binary(&1) and byte_size(&1) > 0 and String.valid?(&1))) do
      schemas(Map.values(fields))
    else
      {:error, :invalid_boundary_type}
    end
  end

  defp schema(_), do: {:error, :invalid_boundary_type}

  defp schemas(types) do
    Enum.reduce_while(types, :ok, fn type, :ok ->
      case schema(type) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp visit(_, _, %{nodes: 0}, _), do: {:error, :validation_budget_exhausted}

  defp visit(:integer, value, limits, _) when is_integer(value),
    do: charge_scalar(value, byte_size(:binary.encode_unsigned(abs(value))), limits)

  defp visit(:boolean, value, limits, _) when is_boolean(value),
    do: charge_scalar(value, 1, limits)

  defp visit(:unit, :unit, limits, _), do: charge_scalar(:unit, 0, limits)

  defp visit(:float, value, limits, _) when is_float(value) do
    <<_sign::1, exponent::11, _fraction::52>> = <<value::float-64>>

    if exponent < 2047,
      do: charge_scalar(value, 8, limits),
      else: {:error, :payload_type_mismatch}
  end

  defp visit(type, value, limits, _) when type in [:text, :bytes] and is_binary(value) do
    # Charge before scanning UTF-8: even rejected large input has a bounded scan.
    with {:ok, _, remaining} <- charge_scalar(value, byte_size(value), limits) do
      if type == :bytes or String.valid?(value),
        do: {:ok, value, remaining},
        else: {:error, :payload_type_mismatch}
    end
  end

  defp visit(:character, value, limits, _)
       when is_integer(value) and value >= 0 and value <= 0x10FFFF and
              value not in 0xD800..0xDFFF,
       do: charge_scalar(value, 4, limits)

  defp visit({:tuple, types}, value, limits, direction)
       when is_tuple(value) and tuple_size(value) == length(types) do
    with {:ok, _, rest} <- charge_scalar(nil, 0, limits),
         {:ok, values, rest} <- fields(types, Tuple.to_list(value), rest, direction),
         do: {:ok, List.to_tuple(values), rest}
  end

  defp visit({:record, types}, value, limits, direction) when is_map(value) do
    labels = Enum.sort(Map.keys(types))
    keys = if direction == :encode, do: labels, else: Enum.map(labels, &existing_key/1)

    if map_size(value) == length(keys) and
         (direction == :encode or Enum.all?(Map.keys(value), &is_atom/1)) and
         Enum.all?(keys, &Map.has_key?(value, &1)) do
      with {:ok, _, rest} <- charge_scalar(nil, 0, limits),
           {:ok, values, rest} <-
             fields(Enum.map(labels, &types[&1]), Enum.map(keys, &value[&1]), rest, direction) do
        output_keys =
          if direction == :decode, do: labels, else: Enum.map(labels, &String.to_atom/1)

        {:ok, Map.new(Enum.zip(output_keys, values)), rest}
      end
    else
      {:error, :payload_type_mismatch}
    end
  end

  defp visit({:variant, types}, {:catena_variant, label, payload}, limits, direction) do
    name = if direction == :decode and is_atom(label), do: Atom.to_string(label), else: label

    if is_binary(name) and Map.has_key?(types, name) and
         ((direction == :decode and is_atom(label)) or direction == :encode) do
      with {:ok, _, rest} <- charge_scalar(nil, 0, limits),
           {:ok, converted, rest} <- visit(types[name], payload, rest, direction) do
        output_label = if direction == :decode, do: name, else: String.to_atom(name)
        {:ok, {:catena_variant, output_label, converted}, rest}
      end
    else
      {:error, :payload_type_mismatch}
    end
  end

  defp visit(_, _, _, _), do: {:error, :payload_type_mismatch}

  defp existing_key(label) do
    String.to_existing_atom(label)
  rescue
    ArgumentError -> {:missing_label, label}
  end

  defp charge_scalar(value, bytes, limits),
    do: Catena.ValueBoundary.Budget.scalar(value, bytes, limits)

  defp fields(types, values, limits, direction) do
    Enum.zip(types, values)
    |> Enum.reduce_while({:ok, [], limits}, fn {type, value}, {:ok, acc, remaining} ->
      case visit(type, value, remaining, direction) do
        {:ok, converted, rest} -> {:cont, {:ok, [converted | acc], rest}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values, rest} -> {:ok, Enum.reverse(values), rest}
      error -> error
    end
  end
end
