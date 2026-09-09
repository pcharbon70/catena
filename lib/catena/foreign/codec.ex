defmodule Catena.Foreign.Codec do
  @moduledoc "Exact 0.1.60 Erlang codecs with complete carrier preflight and nominal provenance."
  alias Catena.ValueBoundary.{Data, Nominal}
  alias Catena.Foreign.Budget

  def new(schema, options \\ []) do
    with {:ok, selection} <- selection(options),
         :ok <- validate_schema(schema) do
      {:ok, %{format: :foreign_codec, version: "0.1.60", selection: selection, schema: schema}}
    else
      {:error, reason} -> failure(reason, :setup)
    end
  rescue
    _ -> failure(:invalid_codec, :setup)
  end

  def for_export(core, name, layout, options \\ []) do
    with {:ok, description} <- Nominal.describe(core, name, layout),
         do: new({:nominal, description}, options)
  end

  def sequence_for_export(core, name, layout, empty, cons, options \\ []) do
    with {:ok, description} <- Nominal.describe(core, name, layout),
         do: new({:sequence, description, empty, cons}, options)
  end

  def verify(%{format: :foreign_codec, schema: schema, selection: selection} = codec) do
    case new(schema, language_selection: selection) do
      {:ok, ^codec} -> :ok
      _ -> failure(:invalid_codec, :setup)
    end
  end

  def verify(_), do: failure(:invalid_codec, :setup)

  defp selection(options) do
    requested =
      Keyword.get(options, :language_selection, Catena.LanguageVersion.legacy_selection("0.1.60"))

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{language_revision: "0.1.60", previews: []} = selection} -> {:ok, selection}
      _ -> {:error, :invalid_foreign_codec_selection}
    end
  end

  def decode(codec, native, limits), do: convert(codec, native, limits, :decode)
  def encode(codec, semantic, limits), do: convert(codec, semantic, limits, :encode)
  def to_native(codec, semantic, limits), do: convert(codec, semantic, limits, :to_native)
  def from_native(codec, native, limits), do: convert(codec, native, limits, :from_native)

  def lower(codec, semantic, limits, annotation \\ 1) do
    with {:ok, native} <- to_native(codec, semantic, limits),
         do: {:ok, :erl_parse.abstract(native, annotation)}
  end

  defp convert(codec, value, limits, direction) do
    with :ok <- verify(codec),
         :ok <- Budget.check(value, limits),
         {:ok, converted} <- visit(codec.schema, value, limits, direction),
         :ok <- Budget.check(converted, limits) do
      {:ok, converted}
    else
      {:error, %{kind: :conversion_failure}} = error -> error
      {:error, reason} -> failure(reason, direction)
      _ -> failure(:invalid_conversion, direction)
    end
  rescue
    _ -> failure(:malformed_conversion, direction)
  end

  defp validate_schema({:data, schema}), do: Data.validate_schema(schema)
  defp validate_schema({:nominal, description}), do: description(description)

  defp validate_schema({:sequence, description, empty, cons}) do
    with :ok <- description(description), {:ok, _} <- sequence(description, empty, cons), do: :ok
  end

  defp validate_schema(_), do: {:error, :unsupported_codec_schema}

  defp description(description) do
    with {:ok, expected} <-
           Nominal.describe(description.core, description.export, description.layout),
         true <- expected == description do
      :ok
    else
      _ -> {:error, :invalid_boundary_description}
    end
  end

  defp visit({:data, type}, value, limits, direction),
    do: apply(Data, direction(direction), [type, value, data_limits(limits)])

  defp visit({:nominal, description}, value, limits, direction),
    do: apply(Nominal, direction(direction), [description, value, data_limits(limits)])

  defp visit({:sequence, description, _, _}, value, limits, direction)
       when direction in [:to_native, :from_native],
       do: apply(Nominal, direction(direction), [description, value, data_limits(limits)])

  defp visit({:sequence, description, empty, cons}, value, limits, :decode) do
    with {:ok, _} <- sequence(description, empty, cons),
         {:ok, values} <- decode_list(value, description, limits, []) do
      {:ok,
       Enum.reduce(values, {:catena_value, empty, []}, fn head, tail ->
         {:catena_value, cons, [head, tail]}
       end)}
    end
  end

  defp visit({:sequence, description, empty, cons}, value, limits, :encode) do
    with {:ok, _} <- sequence(description, empty, cons),
         do: encode_list(value, description, empty, cons, limits, [])
  end

  defp decode_list([], _, _, acc), do: {:ok, acc}

  defp decode_list([head | tail], description, limits, acc) do
    with {:ok, value} <-
           Nominal.convert_argument(description, 0, head, data_limits(limits), :decode),
         do: decode_list(tail, description, limits, [value | acc])
  end

  defp decode_list(_, _, _, _), do: {:error, :improper_sequence}
  defp encode_list({:catena_value, empty, []}, _, empty, _, _, acc), do: {:ok, Enum.reverse(acc)}

  defp encode_list({:catena_value, cons, [head, tail]}, description, empty, cons, limits, acc) do
    with {:ok, value} <-
           Nominal.convert_argument(description, 0, head, data_limits(limits), :encode),
         do: encode_list(tail, description, empty, cons, limits, [value | acc])
  end

  defp encode_list(_, _, _, _, _, _), do: {:error, :malformed_nominal_sequence}

  defp sequence(%{type: {:nominal, id, [argument]}} = description, empty, cons) do
    with %{constructors: constructors} <- description.data.types_by_id[id],
         true <- length(constructors) == 2,
         %{fields: [], visibility: :transparent, gadt?: false, universal_count: 1} =
           empty_constructor <- Enum.find(constructors, &(&1.id == empty)),
         %{fields: [head, tail], visibility: :transparent, gadt?: false, universal_count: 1} =
           constructor <- Enum.find(constructors, &(&1.id == cons)),
         true <- MapSet.size(constructor.existential_ids) == 0,
         true <- MapSet.size(empty_constructor.existential_ids) == 0,
         substitution <- %{hd(constructor.variables) => argument},
         true <- Catena.Type.apply(head.type, substitution) == argument,
         true <- Catena.Type.apply(tail.type, substitution) == description.type do
      {:ok, argument}
    else
      _ -> {:error, :invalid_sequence_declaration}
    end
  end

  defp sequence(_, _, _), do: {:error, :invalid_sequence_declaration}
  defp data_limits(limits), do: Map.take(limits, [:nodes, :bytes])
  defp direction(direction) when direction in [:decode, :from_native], do: :decode
  defp direction(_), do: :encode

  defp failure(reason, direction),
    do: {:error, %{kind: :conversion_failure, reason: reason, direction: direction}}
end
