defmodule Catena.Kernel.CapabilityBinding do
  @moduledoc """
  Structural identities and static selection for the capability-kernel work.

  Callers supply a lexical-visible environment after name resolution and
  operation-membership checking. This component checks descriptor and slot
  identity, not visibility, type formation or authority escape. It is not
  selected by the retained 0.1.8 frontend.
  """
  alias Catena.{Diagnostic, Kernel.CapabilityRow}

  @spec identity(term(), term(), term()) :: {:ok, String.t()} | {:error, Diagnostic.t()}
  def identity(origin, owner, path)
      when is_binary(origin) and byte_size(origin) > 0 and is_binary(owner) and
             byte_size(owner) > 0 and is_list(path) and path != [] do
    if String.valid?(origin) and String.valid?(owner) and
         Enum.all?(path, &(is_integer(&1) and &1 >= 0)) do
      {:ok, JSON.encode!([origin, owner, path])}
    else
      invalid("malformed structural capability identity", "$.binding")
    end
  end

  def identity(_, _, _), do: invalid("malformed structural capability identity", "$.binding")

  @spec select(term(), term(), term(), term()) ::
          {:ok, CapabilityRow.entry()} | {:error, Diagnostic.t()}
  def select(visible, family, arguments, qualifier \\ nil)

  def select(visible, family, arguments, qualifier)
      when is_binary(family) and byte_size(family) > 0 and is_list(arguments) and
             (is_nil(qualifier) or (is_binary(qualifier) and byte_size(qualifier) > 0)) do
    with {:ok, entries} <- CapabilityRow.normalize(visible) do
      candidates =
        Enum.filter(entries, fn entry ->
          entry.family == family and entry.arguments == arguments and
            (is_nil(qualifier) or entry.slot == qualifier)
        end)

      case candidates do
        [entry] ->
          {:ok, entry}

        [] ->
          invalid("no compatible visible capability", "$.request")

        candidates ->
          {:error,
           Diagnostic.new("T002", "ambiguous capability selection",
             path: "$.request",
             details: %{candidates: Enum.map(candidates, & &1.slot)}
           )}
      end
    end
  end

  def select(_, _, _, _), do: invalid("malformed capability selection", "$.request")

  defp invalid(message, path), do: {:error, Diagnostic.new("T002", message, path: path)}
end
