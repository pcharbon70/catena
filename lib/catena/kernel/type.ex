defmodule Catena.Kernel.Type do
  @moduledoc "Types and closed-boundary predicates for the 0.1.8 semantic kernel."

  @type row :: %{fields: %{String.t() => t()}, tail: String.t() | nil}
  @type effect :: :process | {:effect, String.t()}
  @type t ::
          :integer
          | :boolean
          | :unit
          | :bottom
          | {:variable, String.t()}
          | {:inference, non_neg_integer()}
          | {:tuple, [t()]}
          | {:function, t(), [effect()], t()}
          | {:record, row()}
          | {:variant, row()}
          | {:process, t()}
          | {:nominal, String.t(), [t()]}

  @spec closed?(t()) :: boolean()
  def closed?(:integer), do: true
  def closed?(:boolean), do: true
  def closed?(:unit), do: true
  def closed?(:bottom), do: false
  def closed?({:variable, _name}), do: false
  def closed?({:inference, _id}), do: false
  def closed?({:tuple, elements}), do: Enum.all?(elements, &closed?/1)

  def closed?({:function, parameter, _effects, result}),
    do: closed?(parameter) and closed?(result)

  def closed?({tag, %{fields: fields, tail: nil}}) when tag in [:record, :variant],
    do: Enum.all?(fields, fn {_label, type} -> closed?(type) end)

  def closed?({tag, %{tail: tail}}) when tag in [:record, :variant] and not is_nil(tail),
    do: false

  def closed?({:process, mailbox}), do: closed?(mailbox)
  def closed?({:nominal, _name, arguments}), do: Enum.all?(arguments, &closed?/1)

  @spec sendable?(t()) :: boolean()
  def sendable?(type) when type in [:integer, :boolean, :unit], do: true
  def sendable?({:tuple, elements}), do: Enum.all?(elements, &sendable?/1)

  def sendable?({tag, %{fields: fields, tail: nil}}) when tag in [:record, :variant],
    do: Enum.all?(fields, fn {_label, type} -> sendable?(type) end)

  def sendable?({:process, mailbox}), do: closed?(mailbox) and sendable?(mailbox)
  def sendable?({:nominal, _name, arguments}), do: Enum.all?(arguments, &sendable?/1)
  def sendable?(_type), do: false

  @spec variables(t()) :: MapSet.t(String.t())
  def variables({:variable, name}), do: MapSet.new([name])
  def variables({:inference, _id}), do: MapSet.new()
  def variables({:tuple, elements}), do: union_variables(elements)

  def variables({:function, parameter, _effects, result}),
    do: MapSet.union(variables(parameter), variables(result))

  def variables({tag, %{fields: fields, tail: tail}}) when tag in [:record, :variant] do
    field_variables = fields |> Map.values() |> union_variables()
    if is_nil(tail), do: field_variables, else: MapSet.put(field_variables, tail)
  end

  def variables({:process, mailbox}), do: variables(mailbox)
  def variables({:nominal, _name, arguments}), do: union_variables(arguments)
  def variables(_type), do: MapSet.new()

  @spec substitute(t(), map()) :: t()
  def substitute({:variable, name} = variable, substitution),
    do: Map.get(substitution, name, variable)

  def substitute({:inference, id} = variable, substitution),
    do: Map.get(substitution, id, variable)

  def substitute({:tuple, elements}, substitution),
    do: {:tuple, Enum.map(elements, &substitute(&1, substitution))}

  def substitute({:function, parameter, effects, result}, substitution),
    do:
      {:function, substitute(parameter, substitution), effects, substitute(result, substitution)}

  def substitute({tag, %{fields: fields} = row}, substitution) when tag in [:record, :variant] do
    fields = Map.new(fields, fn {label, type} -> {label, substitute(type, substitution)} end)
    {tag, %{row | fields: fields}}
  end

  def substitute({:process, mailbox}, substitution),
    do: {:process, substitute(mailbox, substitution)}

  def substitute({:nominal, name, arguments}, substitution),
    do: {:nominal, name, Enum.map(arguments, &substitute(&1, substitution))}

  def substitute(type, _substitution), do: type

  @spec encode(t()) :: map()
  def encode(:integer), do: %{"tag" => "integer"}
  def encode(:boolean), do: %{"tag" => "boolean"}
  def encode(:unit), do: %{"tag" => "unit"}
  def encode({:variable, name}), do: %{"tag" => "variable", "name" => name}
  def encode({:inference, id}), do: %{"tag" => "inference", "id" => id}

  def encode({:tuple, elements}),
    do: %{"tag" => "tuple", "elements" => Enum.map(elements, &encode/1)}

  def encode({:function, parameter, effects, result}) do
    %{
      "tag" => "function",
      "parameter" => encode(parameter),
      "effects" => Enum.map(effects, &encode_effect/1),
      "result" => encode(result)
    }
  end

  def encode({tag, %{fields: fields, tail: tail}}) when tag in [:record, :variant] do
    %{
      "tag" => Atom.to_string(tag),
      "fields" =>
        fields
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(fn {label, type} -> %{"label" => label, "type" => encode(type)} end),
      "tail" => tail
    }
  end

  def encode({:process, mailbox}), do: %{"tag" => "process", "mailbox" => encode(mailbox)}

  def encode({:nominal, name, arguments}),
    do: %{"tag" => "nominal", "name" => name, "arguments" => Enum.map(arguments, &encode/1)}

  @spec decode(map()) :: {:ok, t()} | :error
  def decode(%{"tag" => "integer"}), do: {:ok, :integer}
  def decode(%{"tag" => "boolean"}), do: {:ok, :boolean}
  def decode(%{"tag" => "unit"}), do: {:ok, :unit}

  def decode(%{"tag" => "variable", "name" => name}) when is_binary(name),
    do: {:ok, {:variable, name}}

  def decode(%{"tag" => "tuple", "elements" => elements}) when is_list(elements) do
    with {:ok, decoded} <- decode_many(elements), do: {:ok, {:tuple, decoded}}
  end

  def decode(%{
        "tag" => "function",
        "parameter" => parameter,
        "effects" => effects,
        "result" => result
      })
      when is_list(effects) do
    with {:ok, parameter} <- decode(parameter),
         {:ok, effects} <- decode_effects(effects),
         {:ok, result} <- decode(result) do
      {:ok, {:function, parameter, effects, result}}
    end
  end

  def decode(%{"tag" => tag, "fields" => fields, "tail" => tail})
      when tag in ["record", "variant"] and is_list(fields) and
             (is_nil(tail) or is_binary(tail)) do
    with true <-
           Enum.all?(fields, &match?(%{"label" => label, "type" => _} when is_binary(label), &1)),
         true <- fields == Enum.sort_by(fields, & &1["label"]),
         true <- length(fields) == length(Enum.uniq_by(fields, & &1["label"])),
         {:ok, decoded} <- decode_fields(fields) do
      row_tag = if tag == "record", do: :record, else: :variant
      {:ok, {row_tag, %{fields: decoded, tail: tail}}}
    else
      _ -> :error
    end
  end

  def decode(%{"tag" => "process", "mailbox" => mailbox}) do
    with {:ok, mailbox} <- decode(mailbox), do: {:ok, {:process, mailbox}}
  end

  def decode(%{"tag" => "nominal", "name" => name, "arguments" => arguments})
      when is_binary(name) and is_list(arguments) do
    with {:ok, arguments} <- decode_many(arguments), do: {:ok, {:nominal, name, arguments}}
  end

  def decode(_value), do: :error

  defp union_variables(types),
    do: Enum.reduce(types, MapSet.new(), &MapSet.union(variables(&1), &2))

  defp encode_effect(:process), do: "Process"
  defp encode_effect({:effect, name}), do: %{"effect" => name}

  defp decode_effects(effects) do
    Enum.reduce_while(effects, {:ok, []}, fn
      "Process", {:ok, decoded} ->
        {:cont, {:ok, [:process | decoded]}}

      %{"effect" => name}, {:ok, decoded} when is_binary(name) ->
        {:cont, {:ok, [{:effect, name} | decoded]}}

      _effect, _acc ->
        {:halt, :error}
    end)
    |> case do
      {:ok, decoded} -> {:ok, Enum.reverse(decoded)}
      :error -> :error
    end
  end

  defp decode_many(values) do
    Enum.reduce_while(values, {:ok, []}, fn value, {:ok, decoded} ->
      case decode(value) do
        {:ok, type} -> {:cont, {:ok, [type | decoded]}}
        :error -> {:halt, :error}
      end
    end)
    |> case do
      {:ok, decoded} -> {:ok, Enum.reverse(decoded)}
      :error -> :error
    end
  end

  defp decode_fields(fields) do
    Enum.reduce_while(fields, {:ok, %{}}, fn %{"label" => label, "type" => value},
                                             {:ok, decoded} ->
      case decode(value) do
        {:ok, type} -> {:cont, {:ok, Map.put(decoded, label, type)}}
        :error -> {:halt, :error}
      end
    end)
  end
end
