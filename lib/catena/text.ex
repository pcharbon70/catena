defmodule Catena.Text do
  @moduledoc """
  Text, Character, and Bytes elaboration at 0.1.35: the C018 pattern
  executed for C017's three scanned kinds. A scanned literal becomes
  its typed meaning — the decoded content — deterministically and
  totally; raw-hash counts, provenance, and source units stay scanner
  facts and never reach the value.
  """

  alias Catena.Literal

  defmodule Meaning do
    @moduledoc "The typed meaning of one elaborated text, character, or bytes literal."

    @enforce_keys [:kind, :type, :value]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            kind: :text | :character | :bytes,
            type: :Text | :Character | :Bytes,
            value: binary() | non_neg_integer()
          }
  end

  @kinds ~w(text character bytes)a

  @doc """
  Elaborates one scanned text, character, or bytes literal into its
  typed meaning. Total and deterministic over successfully scanned
  literals; cooked and raw forms of equal decoded content elaborate
  to equal meanings. A scanned literal of any other kind raises
  `ArgumentError` — elaboration covers exactly the three text kinds.
  The character kind's payload is already the decoded code point
  (C017 decodes to one Unicode scalar); text and bytes payloads are
  the decoded content binaries.
  """
  @spec elaborate(Literal.t() | map(), keyword()) :: {:ok, Meaning.t()}
  def elaborate(literal, options \\ [])

  def elaborate(%Literal{kind: kind, payload: payload}, _options) when kind in @kinds do
    case kind do
      :character when is_integer(payload) ->
        {:ok, meaning(:character, :Character, payload)}

      :text ->
        {:ok, meaning(:text, :Text, payload)}

      :bytes ->
        {:ok, meaning(:bytes, :Bytes, payload)}
    end
  end

  def elaborate(%Literal{kind: kind}, _options) do
    raise ArgumentError,
          "text elaboration covers the text, character, and bytes kinds; got #{kind}"
  end

  @doc """
  The content order over two elaborated meanings of one kind:
  lexicographic by Unicode scalar for Text and Character, by byte for
  Bytes. Raises `ArgumentError` for mixed kinds.
  """
  @spec compare(Meaning.t(), Meaning.t()) :: :lt | :eq | :gt
  def compare(%Meaning{kind: :character} = left, %Meaning{kind: :character} = right) do
    cond do
      left.value == right.value -> :eq
      left.value < right.value -> :lt
      true -> :gt
    end
  end

  def compare(%Meaning{kind: kind} = left, %Meaning{kind: kind} = right)
      when kind in [:text, :bytes] do
    compare_binaries(left.value, right.value)
  end

  def compare(left, right),
    do: raise(ArgumentError, "comparison is monomorphic: #{inspect(left)} vs #{inspect(right)}")

  defp compare_binaries(left, right) do
    cond do
      left == right -> :eq
      left < right -> :lt
      true -> :gt
    end
  end

  defp meaning(kind, type, value), do: %Meaning{kind: kind, type: type, value: value}
end
