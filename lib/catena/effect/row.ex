defmodule Catena.Effect.Row do
  @moduledoc "Identity-aware effect rows for the Catena 0.1.5 bootstrap boundary."

  alias Catena.Type

  @enforce_keys [:entries]
  defstruct entries: [], tail: nil

  @type entry :: %{
          required(:family) => String.t(),
          required(:family_name) => String.t(),
          required(:arguments) => [Type.t()],
          required(:capability) => String.t(),
          optional(:name) => String.t() | nil,
          optional(:abstract?) => boolean()
        }
  @type t :: %__MODULE__{entries: [entry()], tail: String.t() | nil}

  @spec empty() :: t()
  def empty, do: %__MODULE__{entries: []}

  @spec new([entry()], String.t() | nil) :: t()
  def new(entries, tail \\ nil) when is_list(entries),
    do: normalize(%__MODULE__{entries: entries, tail: tail})

  @spec union(t(), t()) :: t()
  def union(%__MODULE__{} = left, %__MODULE__{} = right) do
    tail = merge_tail(left.tail, right.tail)
    new(left.entries ++ right.entries, tail)
  end

  @spec union_all([t()]) :: t()
  def union_all(rows), do: Enum.reduce(rows, empty(), &union/2)

  @spec subtract(t(), String.t()) :: t()
  def subtract(%__MODULE__{} = row, capability) do
    %{row | entries: Enum.reject(row.entries, &(&1.capability == capability))}
  end

  @spec member?(t(), String.t()) :: boolean()
  def member?(%__MODULE__{} = row, capability),
    do: Enum.any?(row.entries, &(&1.capability == capability))

  @spec equal?(t(), t()) :: boolean()
  def equal?(%__MODULE__{} = left, %__MODULE__{} = right),
    do: canonical(left) == canonical(right)

  @spec matches_declaration?(t(), t()) :: boolean()
  def matches_declaration?(%__MODULE__{} = inferred, %__MODULE__{} = declared) do
    inferred_entries = canonical(%{inferred | tail: nil})
    declared_entries = canonical(%{declared | tail: nil})

    inferred_entries == declared_entries and
      (inferred.tail == declared.tail or
         (is_nil(inferred.tail) and not is_nil(declared.tail)) or
         (not is_nil(inferred.tail) and not is_nil(declared.tail)))
  end

  @spec subset?(t(), t()) :: boolean()
  def subset?(%__MODULE__{} = actual, %__MODULE__{} = allowed) do
    allowed_capabilities = MapSet.new(allowed.entries, & &1.capability)

    Enum.all?(actual.entries, &MapSet.member?(allowed_capabilities, &1.capability)) and
      (is_nil(actual.tail) or not is_nil(allowed.tail))
  end

  @spec normalize(t()) :: t()
  def normalize(%__MODULE__{} = row) do
    entries =
      row.entries
      |> Enum.uniq_by(& &1.capability)
      |> Enum.sort_by(&{&1.family, &1.capability})

    %{row | entries: entries}
  end

  @spec encode(t()) :: map()
  def encode(%__MODULE__{} = row) do
    %{
      "entries" =>
        Enum.map(row.entries, fn entry ->
          %{
            "family" => entry.family,
            "family_name" => entry.family_name,
            "arguments" => Enum.map(entry.arguments, &Type.normalize/1),
            "capability" => entry.capability,
            "name" => Map.get(entry, :name),
            "abstract" => Map.get(entry, :abstract?, false)
          }
        end),
      "tail" => row.tail
    }
  end

  defp canonical(row) do
    {Enum.map(
       row.entries,
       &{&1.family, Enum.map(&1.arguments, fn type -> Type.normalize(type) end), &1.capability}
     ), row.tail}
  end

  defp merge_tail(nil, tail), do: tail
  defp merge_tail(tail, nil), do: tail
  defp merge_tail(tail, tail), do: tail
  defp merge_tail(left, right), do: Enum.min([left, right])
end
