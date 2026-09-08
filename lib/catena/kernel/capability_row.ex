defmodule Catena.Kernel.CapabilityRow do
  @moduledoc """
  Closed, identity-indexed rows for the capability-kernel integration work.

  This component is not selected by the exact 0.1.8 frontend. Entries carry
  resolver-owned slot and family identities, not user-facing names. Type
  arguments are already-resolved kernel types; their formation and kinding
  remain the checker's responsibility. This module checks exact descriptor
  consistency and never silently chooses between conflicting descriptors.
  Open tails and capability instantiation require the later solver boundary.
  """

  alias Catena.Diagnostic

  @type entry :: %{slot: String.t(), family: String.t(), arguments: [term()]}
  @type row :: [entry()]
  @type result :: {:ok, row()} | {:error, Diagnostic.t()}

  @spec normalize(term()) :: result()
  def normalize(entries) when is_list(entries) do
    entries
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, %{}}, fn {entry, index}, {:ok, slots} ->
      case insert(slots, entry, index) do
        {:ok, next} -> {:cont, {:ok, next}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, slots} -> {:ok, slots |> Map.values() |> Enum.sort_by(& &1.slot)}
      error -> error
    end
  end

  def normalize(_entries), do: invalid("expected a closed capability-row list", "$.row")

  @spec union(term(), term()) :: result()
  def union(left, right) do
    with {:ok, left} <- normalize(left),
         {:ok, right} <- normalize(right) do
      normalize(left ++ right)
    end
  end

  @spec subtract(term(), term()) :: result()
  def subtract(entries, slot) when is_binary(slot) and byte_size(slot) > 0 do
    with {:ok, entries} <- normalize(entries) do
      if Enum.any?(entries, &(&1.slot == slot)) do
        {:ok, Enum.reject(entries, &(&1.slot == slot))}
      else
        invalid("handled capability is absent from the row", "$.slot", %{slot: slot})
      end
    end
  end

  def subtract(_entries, _slot), do: invalid("expected a nonempty capability identity", "$.slot")

  defp insert(slots, %{slot: slot, family: family, arguments: arguments} = entry, index)
       when is_binary(slot) and byte_size(slot) > 0 and is_binary(family) and
              byte_size(family) > 0 and is_list(arguments) and map_size(entry) == 3 do
    case Map.fetch(slots, slot) do
      :error ->
        {:ok, Map.put(slots, slot, entry)}

      {:ok, ^entry} ->
        {:ok, slots}

      {:ok, _other} ->
        invalid(
          "one capability identity has conflicting effect descriptors",
          "$.row[#{index}]",
          %{
            slot: slot
          }
        )
    end
  end

  defp insert(_slots, _entry, index),
    do: invalid("malformed capability-row entry", "$.row[#{index}]")

  defp invalid(message, path, details \\ %{}),
    do: {:error, Diagnostic.new("T002", message, path: path, details: details)}
end
