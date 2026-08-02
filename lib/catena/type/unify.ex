defmodule Catena.Type.Unify do
  @moduledoc "Kind-preserving, occurs-checked unification for core value types."

  alias Catena.{Diagnostic, Type}

  @spec unify(Type.t(), Type.t(), map(), String.t() | nil) :: map()
  def unify(left, right, substitution, path \\ nil) do
    left = Type.apply(left, substitution)
    right = Type.apply(right, substitution)
    do_unify(left, right, substitution, path)
  end

  defp do_unify(type, type, substitution, _path), do: substitution

  defp do_unify({:var, id}, type, substitution, path),
    do: bind(id, type, substitution, path)

  defp do_unify(type, {:var, id}, substitution, path),
    do: bind(id, type, substitution, path)

  defp do_unify(
         {:function, left_parameter, left_result},
         {:function, right_parameter, right_result},
         substitution,
         path
       ) do
    substitution = unify(left_parameter, right_parameter, substitution, path)
    unify(left_result, right_result, substitution, path)
  end

  defp do_unify({:tuple, left}, {:tuple, right}, substitution, path)
       when length(left) == length(right) do
    Enum.zip(left, right)
    |> Enum.reduce(substitution, fn {left_type, right_type}, current ->
      unify(left_type, right_type, current, path)
    end)
  end

  defp do_unify(left, right, _substitution, path) do
    raise Catena.TypeError,
      diagnostic:
        Diagnostic.new(
          "T002",
          "cannot unify #{inspect(Type.normalize(left))} with #{inspect(Type.normalize(right))}",
          path: path,
          details: %{left: Type.normalize(left), right: Type.normalize(right)}
        )
  end

  defp bind(id, {:var, id}, substitution, _path), do: substitution

  defp bind(id, type, substitution, path) do
    if MapSet.member?(Type.free(type), id) do
      raise Catena.TypeError,
        diagnostic: Diagnostic.new("T003", "type variable occurs inside its own type", path: path)
    end

    Map.put(substitution, id, type)
  end
end

defmodule Catena.TypeError do
  @moduledoc false
  defexception [:diagnostic]

  @impl true
  def message(%__MODULE__{diagnostic: diagnostic}), do: diagnostic.message
end
