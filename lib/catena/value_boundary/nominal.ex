defmodule Catena.ValueBoundary.Nominal do
  @moduledoc "Checked ordinary-ADT bridge derived from verified typed-core exports."
  alias Catena.{Type, TypedCore.Verifier}

  def describe(core, name, layout) when layout in [:uniform, :compact] do
    with :ok <- Verifier.verify(core),
         true <- name in core.exports,
         %{scheme: scheme} <- Enum.find(core.definitions, &(&1.name == name)),
         true <- MapSet.size(Type.free(scheme.type)) == 0 do
      {:ok, %{core: core, export: name, type: scheme.type, layout: layout, data: core.data}}
    else
      _ -> {:error, :invalid_boundary_description}
    end
  rescue
    _error in [KeyError, BadMapError, FunctionClauseError, ArgumentError, Protocol.UndefinedError] ->
      {:error, :invalid_boundary_description}
  end

  def describe(core, name, :fixed), do: Catena.ValueBoundary.Fixed.describe(core, name)

  def describe(_, _, _), do: {:error, :invalid_boundary_description}

  def decode(description, value, budget), do: convert(description, value, budget, :decode)
  def encode(description, value, budget), do: convert(description, value, budget, :encode)

  defp convert(description, value, budget, direction) do
    if Catena.ValueBoundary.Budget.valid?(budget) do
      case description do
        %{core: core, export: name, layout: layout} ->
          with {:ok, expected} <- describe(core, name, layout),
               true <- expected == description,
               {:ok, result, _} <- visit(description.type, value, description, budget, direction) do
            {:ok, result}
          else
            false -> {:error, :invalid_boundary_description}
            error -> error
          end

        _ ->
          {:error, :invalid_boundary_description}
      end
    else
      {:error, :invalid_validation_budget}
    end
  end

  defp visit(_, _, _, %{nodes: 0}, _), do: {:error, :validation_budget_exhausted}

  defp visit(type, value, _, budget, _)
       when type in [:integer, :boolean, :unit, :float, :text, :character, :bytes],
       do: Catena.ValueBoundary.Data.scalar(type, value, budget)

  defp visit({:tuple, types}, value, description, budget, direction)
       when is_tuple(value) and tuple_size(value) == length(types) do
    with {:ok, _, rest} <- Catena.ValueBoundary.Budget.scalar(nil, 0, budget),
         {:ok, values, remaining} <-
           fields(types, Tuple.to_list(value), description, rest, direction),
         do: {:ok, List.to_tuple(values), remaining}
  end

  defp visit({:record, types}, value, description, budget, direction) when is_map(value) do
    labels = Enum.sort(Map.keys(types))
    keys = if direction == :encode, do: labels, else: Enum.map(labels, &String.to_atom/1)

    if map_size(value) == length(keys) and Enum.all?(keys, &Map.has_key?(value, &1)) do
      with {:ok, _, rest} <- Catena.ValueBoundary.Budget.scalar(nil, 0, budget),
           {:ok, values, rest} <-
             fields(
               Enum.map(labels, &types[&1]),
               Enum.map(keys, &value[&1]),
               description,
               rest,
               direction
             ) do
        output_keys =
          if direction == :decode, do: labels, else: Enum.map(labels, &String.to_atom/1)

        {:ok, Map.new(Enum.zip(output_keys, values)), rest}
      end
    else
      {:error, :payload_type_mismatch}
    end
  end

  defp visit({:variant, types}, {:catena_variant, label, value}, description, budget, direction) do
    name = if is_atom(label) and direction == :decode, do: Atom.to_string(label), else: label

    if is_binary(name) and Map.has_key?(types, name) and (direction == :encode or is_atom(label)) do
      with {:ok, _, rest} <- Catena.ValueBoundary.Budget.scalar(nil, 0, budget),
           {:ok, value, rest} <- visit(types[name], value, description, rest, direction) do
        label = if direction == :decode, do: name, else: String.to_atom(name)
        {:ok, {:catena_variant, label, value}, rest}
      end
    else
      {:error, :payload_type_mismatch}
    end
  end

  defp visit({:nominal, id, arguments}, value, description, budget, direction) do
    with %{constructors: constructors} <- description.data.types_by_id[id],
         {:ok, constructor, values} <- unpack(value, constructors, description.layout, direction),
         true <- constructor.visibility == :transparent,
         false <- constructor.gadt?,
         true <- MapSet.size(constructor.existential_ids) == 0,
         true <- length(arguments) == constructor.universal_count,
         true <- list_arity?(values, length(constructor.fields)),
         substitution <-
           Map.new(
             Enum.zip(Enum.take(constructor.variables, constructor.universal_count), arguments)
           ),
         types <- Enum.map(constructor.fields, &substitute(&1.type, substitution)),
         {:ok, _, rest} <- Catena.ValueBoundary.Budget.scalar(nil, 0, budget),
         {:ok, converted, remaining} <- fields(types, values, description, rest, direction) do
      {:ok, pack(constructor, converted, description.layout, direction), remaining}
    else
      {:error, _} = error -> error
      _ -> {:error, :unsupported_or_malformed_nominal}
    end
  end

  defp visit(_, _, _, _, _), do: {:error, :payload_type_mismatch}

  defp fields(types, values, description, budget, direction) do
    Enum.zip(types, values)
    |> Enum.reduce_while({:ok, [], budget}, fn {type, value}, {:ok, acc, fuel} ->
      case visit(type, value, description, fuel, direction) do
        {:ok, converted, remaining} -> {:cont, {:ok, [converted | acc], remaining}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, reversed, remaining} -> {:ok, Enum.reverse(reversed), remaining}
      error -> error
    end
  end

  defp unpack({:catena_value, id, values}, constructors, _, :encode)
       when is_binary(id) and is_list(values) do
    case Enum.find(constructors, &(&1.id == id)) do
      nil -> {:error, :wrong_nominal_identity}
      constructor -> {:ok, constructor, values}
    end
  end

  defp unpack({:catena_adt, id, index, values}, constructors, :uniform, :decode)
       when is_atom(id) and is_integer(index) and is_tuple(values) do
    case Enum.find(constructors, &(&1.index == index and &1.type_id == Atom.to_string(id))) do
      nil ->
        {:error, :wrong_nominal_identity}

      constructor ->
        if tuple_size(values) == length(constructor.fields),
          do: {:ok, constructor, Tuple.to_list(values)},
          else: {:error, :malformed_representation}
    end
  end

  defp unpack({:catena_constructor, name, values}, constructors, :fixed, :decode)
       when is_atom(name) and is_tuple(values) do
    case Enum.find(constructors, &(&1.name == Atom.to_string(name))) do
      %{fields: fields} = constructor when length(fields) == tuple_size(values) ->
        {:ok, constructor, Tuple.to_list(values)}

      _ ->
        {:error, :malformed_representation}
    end
  end

  defp unpack(value, constructors, :compact, :decode) when is_atom(value),
    do: compact(value, [], constructors)

  defp unpack(value, constructors, :compact, :decode)
       when is_tuple(value) and tuple_size(value) > 1 do
    tag = elem(value, 0)
    constructor = if is_atom(tag), do: Enum.find(constructors, &(&1.id == Atom.to_string(tag)))

    if constructor && tuple_size(value) == length(constructor.fields) + 1 do
      [_ | fields] = Tuple.to_list(value)
      {:ok, constructor, fields}
    else
      {:error, :malformed_representation}
    end
  end

  defp unpack(_, _, _, _), do: {:error, :malformed_representation}

  defp compact(tag, fields, constructors) do
    case Enum.find(constructors, &(&1.id == Atom.to_string(tag))) do
      nil -> {:error, :wrong_nominal_identity}
      constructor -> {:ok, constructor, fields}
    end
  end

  defp substitute({tag, fields}, substitution) when tag in [:record, :variant],
    do: {tag, Map.new(fields, fn {label, type} -> {label, substitute(type, substitution)} end)}

  defp substitute({:tuple, types}, substitution),
    do: {:tuple, Enum.map(types, &substitute(&1, substitution))}

  defp substitute({:nominal, id, arguments}, substitution),
    do: {:nominal, id, Enum.map(arguments, &substitute(&1, substitution))}

  defp substitute(type, substitution), do: Type.apply(type, substitution)

  defp list_arity?([], 0), do: true
  defp list_arity?([_ | rest], count) when count > 0, do: list_arity?(rest, count - 1)
  defp list_arity?(_, _), do: false

  defp pack(constructor, fields, _, :decode), do: {:catena_value, constructor.id, fields}

  defp pack(constructor, fields, :fixed, :encode),
    do: {:catena_constructor, String.to_atom(constructor.name), List.to_tuple(fields)}

  defp pack(constructor, fields, :uniform, :encode),
    do:
      {:catena_adt, String.to_atom(constructor.type_id), constructor.index, List.to_tuple(fields)}

  defp pack(constructor, [], :compact, :encode), do: String.to_atom(constructor.id)

  defp pack(constructor, fields, :compact, :encode),
    do: List.to_tuple([String.to_atom(constructor.id) | fields])
end
