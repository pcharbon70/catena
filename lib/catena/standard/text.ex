defmodule Catena.Standard.Text do
  @moduledoc "Explicit text/binary contract operations; names are internal role labels."
  alias Catena.Foreign.Budget
  alias Catena.Standard.Outcomes, as: Outcome
  alias Catena.Standard.Text.{Encoding, Graphemes, Normalization, Unicode}
  @units [:byte, :scalar, :grapheme]

  def profile,
    do: %{
      version: "0.1.66",
      unicode: "17.0.0",
      uax29: 47,
      uax15: 57,
      table_digest: Unicode.identity(),
      grapheme: :default_extended,
      normalization: :explicit
    }

  def failure_schema,
    do:
      {:variant,
       %{
         "mixed_index_units" => :unit,
         "split_scalar" => :unit,
         "slice_bounds" => {:tuple, [:integer, :integer, :integer]},
         "malformed_encoding" => :integer
       }}

  def failure(reason) when reason in [:mixed_index_units, :split_scalar],
    do: {:catena_variant, reason, :unit}

  def failure({:slice_bounds, first, last, count}),
    do: {:catena_variant, :slice_bounds, {first, last, count}}

  def failure({:malformed_encoding, offset}), do: {:catena_variant, :malformed_encoding, offset}

  def index(unit, offset) when unit in @units and is_integer(offset),
    do: {:ok, %{version: "0.1.66", unit: unit, offset: offset}}

  def index(_, _), do: {:error, :invalid_text_index}

  def verify_index(value) do
    case index(value.unit, value.offset) do
      {:ok, ^value} -> :ok
      _ -> {:error, :invalid_text_index}
    end
  rescue
    _ -> {:error, :invalid_text_index}
  end

  def measure(text, unit, limits) when unit in @units do
    with :ok <- text(text, limits) do
      result =
        case unit do
          :byte -> byte_size(text)
          :scalar -> length(String.to_charlist(text))
          :grapheme -> length(Graphemes.boundaries(text)) - 1
        end

      bounded(result, limits)
    end
  end

  def measure(_, _, _), do: {:error, :invalid_text_unit}

  def slice(text, first, last, limits) do
    with :ok <- text(text, limits), :ok <- verify_index(first), :ok <- verify_index(last) do
      if first.unit != last.unit do
        outcome(Outcome.failure(failure(:mixed_index_units)), limits)
      else
        points = boundaries(text, first.unit)
        count = if first.unit == :byte, do: byte_size(text), else: length(points) - 1
        a = first.offset
        b = last.offset

        cond do
          a < 0 or b < a or b > count ->
            outcome(Outcome.failure(failure({:slice_bounds, a, b, count})), limits)

          first.unit == :byte ->
            if scalar_boundary?(text, a) and scalar_boundary?(text, b),
              do: outcome(Outcome.success(binary_part(text, a, b - a)), limits),
              else: outcome(Outcome.failure(failure(:split_scalar)), limits)

          true ->
            points = List.to_tuple(points)
            start = elem(points, a)
            finish = elem(points, b)
            outcome(Outcome.success(binary_part(text, start, finish - start)), limits)
        end
      end
    end
  end

  def slice_bytes(bytes, first, last, limits) do
    with true <- is_binary(bytes),
         :ok <- Budget.check(bytes, limits),
         :ok <- Budget.check({first, last}, limits),
         :ok <- verify_index(first),
         :ok <- verify_index(last) do
      a = first.offset
      b = last.offset

      cond do
        first.unit != :byte or last.unit != :byte ->
          outcome(Outcome.failure(failure(:mixed_index_units)), limits)

        a < 0 or b < a or b > byte_size(bytes) ->
          outcome(Outcome.failure(failure({:slice_bounds, a, b, byte_size(bytes)})), limits)

        true ->
          outcome(Outcome.success(binary_part(bytes, a, b - a)), limits)
      end
    else
      false -> {:error, :invalid_bytes}
      error -> error
    end
  end

  def normalize(text, form, limits) when form in [:nfc, :nfd, :nfkc, :nfkd] do
    with :ok <- text(text, limits), do: bounded(Normalization.normalize(text, form), limits)
  end

  def normalize(_, _, _), do: {:error, :unsupported_normalization_form}

  def decode(bytes, encoding, limits) do
    with true <- is_binary(bytes),
         :ok <- Budget.check(bytes, limits),
         true <- Encoding.supported?(encoding) do
      case Encoding.decode(bytes, encoding) do
        {:ok, text} -> outcome(Outcome.success(text), limits)
        {:error, reason} -> outcome(Outcome.failure(failure(reason)), limits)
      end
    else
      false -> {:error, :invalid_encoding_input}
      error -> error
    end
  end

  def encode(text, encoding, limits) do
    with :ok <- text(text, limits),
         {:ok, bytes} <- Encoding.encode(text, encoding),
         do: bounded(bytes, limits)
  end

  def concatenate(parts, limits) do
    with true <- is_list(parts),
         :ok <- Budget.check(parts, limits),
         true <- Enum.all?(parts, &(is_binary(&1) and String.valid?(&1))) do
      bounded(IO.iodata_to_binary(parts), limits)
    else
      false -> {:error, :invalid_text_parts}
      error -> error
    end
  end

  def format(parts, limits) do
    with true <- is_list(parts), :ok <- Budget.check(parts, limits) do
      result =
        Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, acc} ->
          case format_part(part) do
            {:ok, piece} -> {:cont, {:ok, [piece | acc]}}
            error -> {:halt, error}
          end
        end)

      with {:ok, reversed} <- result,
           do: bounded(reversed |> Enum.reverse() |> IO.iodata_to_binary(), limits)
    else
      false -> {:error, :invalid_format_parts}
      error -> error
    end
  end

  defp format_part({:text, text}) when is_binary(text) do
    if String.valid?(text), do: {:ok, text}, else: {:error, :invalid_text}
  end

  defp format_part({:character, scalar})
       when is_integer(scalar) and scalar in 0..0x10FFFF and scalar not in 0xD800..0xDFFF,
       do: {:ok, <<scalar::utf8>>}

  defp format_part({:integer, value}) when is_integer(value), do: {:ok, Integer.to_string(value)}

  defp format_part({:bytes_hex, bytes}) when is_binary(bytes),
    do: {:ok, Base.encode16(bytes, case: :lower)}

  defp format_part(_), do: {:error, :unsupported_format_role}

  defp boundaries(_, :byte), do: nil
  defp boundaries(text, :grapheme), do: Graphemes.boundaries(text)

  defp boundaries(text, :scalar) do
    {offset, reversed} =
      for <<scalar::utf8 <- text>>, reduce: {0, [0]} do
        {offset, reversed} ->
          next = offset + byte_size(<<scalar::utf8>>)
          {next, [next | reversed]}
      end

    _ = offset
    Enum.reverse(reversed)
  end

  defp scalar_boundary?(text, offset) when offset == byte_size(text), do: true
  defp scalar_boundary?(text, offset), do: :binary.at(text, offset) not in 0x80..0xBF

  defp text(text, limits) do
    with true <- is_binary(text),
         :ok <- Budget.check(text, limits),
         true <- String.valid?(text),
         do: :ok,
         else: (
           false -> {:error, :invalid_text}
           error -> error
         )
  end

  defp outcome(value, limits), do: bounded(value, limits)
  defp bounded(value, limits), do: with(:ok <- Budget.check(value, limits), do: {:ok, value})
end
