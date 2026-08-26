defmodule Catena.Bindings do
  @moduledoc """
  Unused-binding analysis at 0.1.27: deny-able `BS001` warnings for
  non-`_`-prefixed binders that never occur in their binding's body.

  The walk is deterministic and advisory to validity: an unused
  binding remains valid and its right-hand side still evaluates, with
  effects observable — the kernel rule. `_`-prefixed binders are
  exempt because the normative sequencing idiom
  (`let _ = e1; e2`) uses exactly such a binder.
  """

  alias Catena.Diagnostic

  @diagnostic_id "BS001"

  @spec diagnostic_id() :: String.t()
  def diagnostic_id, do: @diagnostic_id

  @doc """
  Walks one definition's expression tree and returns one `BS001`
  warning per non-`_`-prefixed `let` binder that never occurs in its
  body. The walk is order-independent and total over decodable
  expression forms.
  """
  @spec unused_binding_warnings(map(), String.t()) :: [Diagnostic.t()]
  def unused_binding_warnings(definition, name) do
    expression = Map.get(definition, :expression) || definition

    expression
    |> collect([])
    |> Enum.reverse()
    |> Enum.map(&warning(&1, name))
  end

  defp collect(%{tag: :let} = node, acc) do
    used = uses(node.body, MapSet.new())

    acc =
      if MapSet.member?(used, node.name) or exempt?(node.name),
        do: acc,
        else: [%{name: node.name, path: node.path} | acc]

    collect(node.value, collect(node.body, acc))
  end

  defp collect(%{tag: _} = node, acc),
    do: traverse(node, acc)

  defp collect(_other, acc), do: acc

  defp traverse(node, acc) do
    node
    |> Map.delete(:tag)
    |> Map.delete(:path)
    |> Enum.reduce(acc, fn
      {_key, %{tag: _} = child}, acc ->
        collect(child, acc)

      {_key, children}, acc when is_list(children) ->
        Enum.reduce(children, acc, fn
          %{tag: _} = child, acc -> collect(child, acc)
          _other, acc -> acc
        end)

      _other, acc ->
        acc
    end)
  end

  defp uses(%{tag: :variable, name: name}, acc), do: MapSet.put(acc, name)

  defp uses(%{tag: _} = node, acc) do
    node
    |> Map.delete(:tag)
    |> Map.delete(:path)
    |> Enum.reduce(acc, fn
      {_key, %{tag: _} = child}, acc ->
        uses(child, acc)

      {_key, children}, acc when is_list(children) ->
        Enum.reduce(children, acc, fn
          %{tag: _} = child, acc -> uses(child, acc)
          _other, acc -> acc
        end)

      _other, acc ->
        acc
    end)
  end

  defp uses(_other, acc), do: acc

  defp exempt?(name), do: String.starts_with?(name, "_")

  defp warning(unused, definition_name) do
    Diagnostic.new(
      @diagnostic_id,
      "binding #{unused.name} is never used in #{definition_name}",
      path: unused.path,
      severity: :warning,
      details: %{binding: unused.name, definition: definition_name}
    )
  end
end
