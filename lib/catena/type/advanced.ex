defmodule Catena.Type.Advanced do
  @moduledoc "Scope checks shared by GADT branches, rigid existentials, and resumptions."

  alias Catena.Diagnostic

  @spec branch(MapSet.t(term()), MapSet.t(term()), (map() -> term())) :: term()
  def branch(equalities, existentials, checker) do
    checker.(%{equalities: equalities, rigid: existentials, generalize?: false})
  end

  @spec assert_no_escape!(term(), MapSet.t(term())) :: :ok
  def assert_no_escape!(type, rigid) do
    if MapSet.disjoint?(variables(type), rigid) do
      :ok
    else
      raise Catena.TypeError,
        diagnostic:
          Diagnostic.new("T009", "a rigid existential or branch equality escapes its scope")
    end
  end

  defp variables({:var, variable}), do: MapSet.new([variable])
  defp variables({:skolem, variable}), do: MapSet.new([variable])
  defp variables(tuple) when is_tuple(tuple), do: tuple |> Tuple.to_list() |> variables()

  defp variables(list) when is_list(list),
    do: Enum.reduce(list, MapSet.new(), &MapSet.union(variables(&1), &2))

  defp variables(_), do: MapSet.new()
end
