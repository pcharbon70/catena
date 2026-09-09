defmodule Catena.Standard.Binary.Pattern do
  @moduledoc "Checked sequential binary segments; explicit internal roles without source syntax."
  alias Catena.Foreign.{Budget, Codec}
  alias Catena.Standard.Outcomes, as: Outcome

  def describe(segments) when is_list(segments) and length(segments) <= 253 do
    with {:ok, captures} <- schemas(segments, []),
         {:ok, codec} <- Codec.new({:data, {:tuple, captures}}) do
      {:ok, %{version: "0.1.66", segments: segments, captures: codec, consumption: :whole}}
    end
  end

  def describe(_), do: {:error, :invalid_binary_pattern}

  def verify(description) do
    case describe(description.segments) do
      {:ok, ^description} -> :ok
      _ -> {:error, :invalid_binary_pattern}
    end
  rescue
    _ -> {:error, :invalid_binary_pattern}
  end

  def match(description, bytes, limits) do
    with :ok <- verify(description),
         true <- is_binary(bytes),
         :ok <- Budget.check(description.segments, limits),
         :ok <- Budget.check(bytes, limits) do
      result =
        case consume(description.segments, bytes, []) do
          {:ok, values} -> Outcome.present(List.to_tuple(values))
          :mismatch -> Outcome.absent()
        end

      with :ok <- Budget.check(result, limits), do: {:ok, result}
    else
      false -> {:error, :invalid_binary_input}
      error -> error
    end
  end

  defp schemas([], acc), do: {:ok, Enum.reverse(acc)}
  defp schemas([:rest_bytes], acc), do: {:ok, Enum.reverse([:bytes | acc])}

  defp schemas([{:integer, bits, signed, endian} | rest], acc)
       when is_integer(bits) and bits in 1..4096 and signed in [:unsigned, :signed] and
              endian in [:big, :little] do
    if endian == :little and rem(bits, 8) != 0,
      do: {:error, :unaligned_little_integer_width},
      else: schemas(rest, [:integer | acc])
  end

  defp schemas([{:bytes, size} | rest], acc) when is_integer(size) and size >= 0,
    do: schemas(rest, [:bytes | acc])

  defp schemas([{:literal, bytes} | rest], acc) when is_binary(bytes), do: schemas(rest, acc)
  defp schemas(_, _), do: {:error, :invalid_binary_pattern}

  defp consume([], <<>>, reversed), do: {:ok, Enum.reverse(reversed)}

  defp consume([:rest_bytes], rest, reversed) when is_binary(rest),
    do: {:ok, Enum.reverse([rest | reversed])}

  defp consume([{:literal, literal} | rest], bytes, reversed) do
    size = byte_size(literal)

    case bytes do
      <<^literal::binary-size(^size), tail::bitstring>> -> consume(rest, tail, reversed)
      _ -> :mismatch
    end
  end

  defp consume([{:bytes, size} | rest], bytes, reversed) when bit_size(bytes) >= size * 8 do
    <<value::binary-size(^size), tail::bitstring>> = bytes
    consume(rest, tail, [value | reversed])
  end

  defp consume([{:integer, bits, signed, endian} | rest], bytes, reversed)
       when bit_size(bytes) >= bits do
    {value, tail} =
      case {signed, endian} do
        {:unsigned, :big} ->
          <<value::unsigned-big-integer-size(^bits), tail::bitstring>> = bytes
          {value, tail}

        {:signed, :big} ->
          <<value::signed-big-integer-size(^bits), tail::bitstring>> = bytes
          {value, tail}

        {:unsigned, :little} ->
          <<value::unsigned-little-integer-size(^bits), tail::bitstring>> = bytes
          {value, tail}

        {:signed, :little} ->
          <<value::signed-little-integer-size(^bits), tail::bitstring>> = bytes
          {value, tail}
      end

    consume(rest, tail, [value | reversed])
  end

  defp consume(_, _, _), do: :mismatch
end
