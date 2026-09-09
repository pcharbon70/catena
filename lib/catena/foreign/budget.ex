defmodule Catena.Foreign.Budget do
  @moduledoc "Whole-carrier preflight for typed foreign conversion; no hidden payload traversal."

  def valid?(%{nodes: n, bytes: b, depth: d} = limits),
    do:
      map_size(limits) == 3 and is_integer(n) and n > 0 and is_integer(b) and b >= 0 and
        is_integer(d) and d >= 0

  def valid?(_), do: false

  def check(value, limits) do
    if valid?(limits) do
      case walk(value, limits, 0) do
        {:ok, _} -> :ok
        error -> error
      end
    else
      {:error, :invalid_validation_budget}
    end
  end

  defp walk(_, %{nodes: 0}, _), do: {:error, :node_budget_exhausted}

  defp walk(_, %{depth: maximum}, depth) when depth > maximum,
    do: {:error, :depth_budget_exhausted}

  defp walk(value, fuel, _) when is_binary(value), do: charge(fuel, byte_size(value))

  defp walk(value, fuel, _) when is_integer(value),
    do: charge(fuel, byte_size(:binary.encode_unsigned(abs(value))))

  defp walk(value, fuel, _) when is_float(value), do: charge(fuel, 8)

  defp walk(value, fuel, _) when is_atom(value),
    do: charge(fuel, byte_size(Atom.to_string(value)))

  defp walk([], fuel, _), do: charge(fuel, 0)

  defp walk([head | tail], fuel, depth) do
    with {:ok, fuel} <- charge(fuel, 0),
         {:ok, fuel} <- walk(head, fuel, depth + 1),
         do: walk(tail, fuel, depth)
  end

  defp walk(value, fuel, depth) when is_tuple(value) do
    if tuple_size(value) >= fuel.nodes do
      {:error, :node_budget_exhausted}
    else
      with {:ok, fuel} <- charge(fuel, 0), do: children(Tuple.to_list(value), fuel, depth + 1)
    end
  end

  defp walk(value, fuel, depth) when is_map(value) do
    if map_size(value) * 2 >= fuel.nodes do
      {:error, :node_budget_exhausted}
    else
      with {:ok, fuel} <- charge(fuel, 0),
           do: map_children(:maps.iterator(value), fuel, depth + 1)
    end
  end

  defp walk(_, _, _), do: {:error, :unsupported_carrier}

  defp charge(%{bytes: bytes} = fuel, count) when count <= bytes,
    do: {:ok, %{fuel | nodes: fuel.nodes - 1, bytes: bytes - count}}

  defp charge(_, _), do: {:error, :byte_budget_exhausted}
  defp children([], fuel, _), do: {:ok, fuel}

  defp children([value | rest], fuel, depth) do
    with {:ok, fuel} <- walk(value, fuel, depth), do: children(rest, fuel, depth)
  end

  defp map_children(iterator, fuel, depth) do
    case :maps.next(iterator) do
      :none ->
        {:ok, fuel}

      {key, value, rest} ->
        with {:ok, fuel} <- walk(key, fuel, depth),
             {:ok, fuel} <- walk(value, fuel, depth),
             do: map_children(rest, fuel, depth)
    end
  end
end
