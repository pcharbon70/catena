defmodule Catena.SourceSpan do
  @moduledoc "A half-open source range with byte and human-readable positions."

  @enforce_keys [
    :byte_start,
    :byte_end,
    :line_start,
    :column_start,
    :line_end,
    :column_end
  ]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          byte_start: non_neg_integer(),
          byte_end: non_neg_integer(),
          line_start: pos_integer(),
          column_start: pos_integer(),
          line_end: pos_integer(),
          column_end: pos_integer()
        }

  @spec new(map(), map()) :: t()
  def new(start, finish) do
    %__MODULE__{
      byte_start: start.offset,
      byte_end: finish.offset,
      line_start: start.line,
      column_start: start.column,
      line_end: finish.line,
      column_end: finish.column
    }
  end

  @spec merge(t(), t()) :: t()
  def merge(left, right) do
    %__MODULE__{
      byte_start: left.byte_start,
      byte_end: right.byte_end,
      line_start: left.line_start,
      column_start: left.column_start,
      line_end: right.line_end,
      column_end: right.column_end
    }
  end

  @spec to_map(t()) :: map()
  def to_map(span) do
    %{
      byte_start: span.byte_start,
      byte_end: span.byte_end,
      line_start: span.line_start,
      column_start: span.column_start,
      line_end: span.line_end,
      column_end: span.column_end
    }
  end

  @doc "Resolve original UTF-8 byte endpoints to scalar columns; reject split codepoints."
  def from_bytes(source, first, last)
      when is_binary(source) and is_integer(first) and is_integer(last) and first >= 0 and
             last >= first and last <= byte_size(source) do
    with {:ok, a} <- position(binary_part(source, 0, first)),
         {:ok, b} <- position(binary_part(source, 0, last)) do
      {:ok, new(Map.put(a, :offset, first), Map.put(b, :offset, last))}
    end
  end

  def from_bytes(_, _, _), do: {:error, :invalid_source_span}

  defp position(prefix) do
    if String.valid?(prefix) do
      normalized = prefix |> String.replace("\r\n", "\n") |> String.replace("\r", "\n")
      lines = String.split(normalized, "\n")
      {:ok, %{line: length(lines), column: length(String.to_charlist(List.last(lines))) + 1}}
    else
      {:error, :split_source_codepoint}
    end
  end

  @doc "Resolve a batch of original byte ranges in one UTF-8 scan."
  def from_ranges(source, ranges) when is_binary(source) and is_map(ranges) do
    targets = ranges |> Map.values() |> Enum.flat_map(fn {a, b} -> [a, b] end) |> MapSet.new()
    positions = collect_positions(source, 0, 1, 1, targets, %{})

    Map.new(ranges, fn {key, {a, b}} ->
      {key, new(Map.fetch!(positions, a), Map.fetch!(positions, b))}
    end)
  end

  defp collect_positions(rest, offset, line, column, targets, positions) do
    positions =
      if MapSet.member?(targets, offset),
        do: Map.put(positions, offset, %{offset: offset, line: line, column: column}),
        else: positions

    case rest do
      <<>> ->
        positions

      <<13, 10, tail::binary>> ->
        positions =
          if MapSet.member?(targets, offset + 1),
            do: Map.put(positions, offset + 1, %{offset: offset + 1, line: line + 1, column: 1}),
            else: positions

        collect_positions(tail, offset + 2, line + 1, 1, targets, positions)

      <<13, tail::binary>> ->
        collect_positions(tail, offset + 1, line + 1, 1, targets, positions)

      <<10, tail::binary>> ->
        collect_positions(tail, offset + 1, line + 1, 1, targets, positions)

      <<scalar::utf8, tail::binary>> ->
        collect_positions(
          tail,
          offset + byte_size(<<scalar::utf8>>),
          line,
          column + 1,
          targets,
          positions
        )
    end
  end
end
