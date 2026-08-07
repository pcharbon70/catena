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
end
