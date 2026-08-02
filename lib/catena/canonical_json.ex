defmodule Catena.CanonicalJSON do
  @moduledoc "Deterministic JSON encoding with recursively sorted object keys."

  @spec encode(term()) :: binary()
  def encode(value) when is_map(value) do
    value
    |> Enum.map(fn {key, item} -> {to_string(key), item} end)
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map_join(",", fn {key, item} -> JSON.encode!(key) <> ":" <> encode(item) end)
    |> then(&("{" <> &1 <> "}"))
  end

  def encode(value) when is_list(value), do: "[" <> Enum.map_join(value, ",", &encode/1) <> "]"

  def encode(value) when is_atom(value) and value not in [true, false, nil],
    do: JSON.encode!(Atom.to_string(value))

  def encode(value), do: JSON.encode!(value)
end
