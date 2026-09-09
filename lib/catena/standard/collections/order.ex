defmodule Catena.Standard.Collections.Order do
  @moduledoc "Closed, compiler-owned semantic key order. No arbitrary comparator admission."
  @kinds [:integer, :float, :text, :character, :bytes]
  def new(kind) when kind in @kinds,
    do: {:ok, %{version: "0.1.65", kind: kind, equality: :semantic, order: :ascending}}

  def new(_), do: {:error, :unsupported_key_order}

  def verify(evidence) do
    case new(evidence.kind) do
      {:ok, ^evidence} -> :ok
      _ -> {:error, :invalid_key_order}
    end
  rescue
    _ -> {:error, :invalid_key_order}
  end

  def compare(evidence, a, b) do
    with :ok <- verify(evidence),
         true <- valid?(evidence.kind, a),
         true <- valid?(evidence.kind, b) do
      {:ok, Catena.Values.compare(a, b)}
    else
      _ -> {:error, :invalid_ordered_key}
    end
  end

  def key(%{kind: :float}, value), do: <<value::float-64>>
  def key(_, value), do: value
  defp valid?(:integer, value), do: is_integer(value)
  defp valid?(kind, value), do: Catena.ValueBoundary.Data.valid_scalar?(kind, value)
end
