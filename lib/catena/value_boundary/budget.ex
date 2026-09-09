defmodule Catena.ValueBoundary.Budget do
  @moduledoc "Explicit visited-value and scalar-payload budgets shared by boundary codecs."

  def valid?(%{nodes: nodes, bytes: bytes} = limits),
    do:
      map_size(limits) == 2 and is_integer(nodes) and nodes > 0 and is_integer(bytes) and
        bytes >= 0

  def valid?(_), do: false

  def scalar(value, bytes, %{nodes: nodes, bytes: available})
      when nodes > 0 and bytes <= available,
      do: {:ok, value, %{nodes: nodes - 1, bytes: available - bytes}}

  def scalar(_, _, _), do: {:error, :validation_budget_exhausted}
end
