defmodule Catena.Standard.Text.Encoding do
  @moduledoc false
  @encodings [:utf8, :utf16_be, :utf16_le, :utf32_be, :utf32_le]
  def supported?(encoding), do: encoding in @encodings

  def decode(bytes, encoding) when encoding in @encodings and is_binary(bytes),
    do: decode(bytes, encoding, 0, [])

  def decode(_, _), do: {:error, :unsupported_encoding}

  defp decode(<<>>, _, _, reversed),
    do: {:ok, reversed |> Enum.reverse() |> IO.iodata_to_binary()}

  defp decode(bytes, encoding, offset, reversed) do
    case scalar(bytes, encoding) do
      {:ok, scalar, rest} ->
        decode(rest, encoding, offset + byte_size(bytes) - byte_size(rest), [
          <<scalar::utf8>> | reversed
        ])

      :error ->
        {:error, {:malformed_encoding, offset}}
    end
  end

  defp scalar(<<scalar::utf8, rest::binary>>, :utf8), do: {:ok, scalar, rest}

  defp scalar(bytes, encoding) when encoding in [:utf16_be, :utf16_le] do
    with {:ok, word, rest} <- word(bytes, encoding) do
      cond do
        word in 0xD800..0xDBFF ->
          case word(rest, encoding) do
            {:ok, low, tail} when low in 0xDC00..0xDFFF ->
              {:ok, 0x10000 + (word - 0xD800) * 1024 + low - 0xDC00, tail}

            _ ->
              :error
          end

        word in 0xDC00..0xDFFF ->
          :error

        true ->
          {:ok, word, rest}
      end
    end
  end

  defp scalar(<<scalar::unsigned-big-32, rest::binary>>, :utf32_be), do: scalar32(scalar, rest)
  defp scalar(<<scalar::unsigned-little-32, rest::binary>>, :utf32_le), do: scalar32(scalar, rest)
  defp scalar(_, _), do: :error

  defp scalar32(scalar, rest) when scalar <= 0x10FFFF and scalar not in 0xD800..0xDFFF,
    do: {:ok, scalar, rest}

  defp scalar32(_, _), do: :error
  defp word(<<word::unsigned-big-16, rest::binary>>, :utf16_be), do: {:ok, word, rest}
  defp word(<<word::unsigned-little-16, rest::binary>>, :utf16_le), do: {:ok, word, rest}
  defp word(_, _), do: :error

  def encode(text, encoding) when encoding in @encodings do
    if is_binary(text) and String.valid?(text) do
      {:ok, for(<<scalar::utf8 <- text>>, into: <<>>, do: encode_scalar(scalar, encoding))}
    else
      {:error, :invalid_text}
    end
  end

  def encode(_, _), do: {:error, :unsupported_encoding}
  defp encode_scalar(scalar, :utf8), do: <<scalar::utf8>>
  defp encode_scalar(scalar, :utf16_be), do: <<scalar::utf16-big>>
  defp encode_scalar(scalar, :utf16_le), do: <<scalar::utf16-little>>
  defp encode_scalar(scalar, :utf32_be), do: <<scalar::unsigned-big-32>>
  defp encode_scalar(scalar, :utf32_le), do: <<scalar::unsigned-little-32>>
end
