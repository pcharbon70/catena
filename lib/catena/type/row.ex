defmodule Catena.Type.Row do
  @moduledoc "Executable contracts for unique value rows and duplicate effect rows."

  alias Catena.Diagnostic

  @type unique :: %{fields: %{String.t() => term()}, tail: term() | nil}
  @type effect :: %{occurrences: [{String.t(), term()}], tail: term() | nil}

  @spec unique([{String.t(), term()}], term() | nil) :: unique()
  def unique(fields, tail \\ nil) do
    labels = Enum.map(fields, &elem(&1, 0))

    if length(labels) != length(Enum.uniq(labels)) do
      raise Catena.TypeError,
        diagnostic: Diagnostic.new("T005", "unique value rows cannot contain duplicate labels")
    end

    %{fields: Map.new(fields), tail: tail}
  end

  @spec equal_unique?(unique(), unique()) :: boolean()
  def equal_unique?(left, right), do: left == right

  @spec effects([{String.t(), term()}], term() | nil) :: effect()
  def effects(occurrences, tail \\ nil), do: %{occurrences: canonical(occurrences), tail: tail}

  @spec union_effects(effect(), effect()) :: effect()
  def union_effects(left, right) do
    if left.tail && right.tail && left.tail != right.tail do
      raise Catena.TypeError,
        diagnostic: Diagnostic.new("T004", "cannot union effect rows with unrelated open tails")
    end

    effects(left.occurrences ++ right.occurrences, left.tail || right.tail)
  end

  @spec remove_effect(effect(), {String.t(), term()}) :: effect()
  def remove_effect(row, occurrence) do
    case remove_first(row.occurrences, occurrence) do
      {:ok, remaining} ->
        %{row | occurrences: remaining}

      :error ->
        raise Catena.TypeError,
          diagnostic: Diagnostic.new("T002", "handled capability is absent from the effect row")
    end
  end

  defp canonical(occurrences),
    do: Enum.sort_by(occurrences, fn {label, identity} -> {label, inspect(identity)} end)

  defp remove_first([], _occurrence), do: :error
  defp remove_first([occurrence | rest], occurrence), do: {:ok, rest}

  defp remove_first([head | rest], occurrence) do
    case remove_first(rest, occurrence) do
      {:ok, remaining} -> {:ok, [head | remaining]}
      :error -> :error
    end
  end
end
