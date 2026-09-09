defmodule Catena.Calling.Lowering do
  @moduledoc false
  # Names belong to this exact artifact, never a stable external ABI.
  def factory(name), do: String.to_atom("__catena_calling_entry_" <> name)

  def origins(functions, name, origin, span, written_arity, kind) do
    Map.new(functions, fn {:function, _, symbol, arity, _} ->
      {{symbol, arity},
       %{
         name: name,
         origin: origin,
         span: span,
         kind: kind,
         hidden_arguments: max(arity - written_arity, 0)
       }}
    end)
  end

  def attach(forms, factories, origins) do
    exports = for {:function, _, name, arity, _} <- factories, do: {name, arity}
    existing = for {:function, _, name, arity, _} <- forms, do: {name, arity}

    if Enum.any?(exports, &(&1 in existing)),
      do: raise(ArgumentError, "calling factory collision")

    Enum.flat_map(forms, fn
      {:attribute, ann, :module, _} = form ->
        [form, {:attribute, ann, :catena_calling_origins, origins}]

      {:attribute, ann, :export, values} ->
        [{:attribute, ann, :export, values ++ exports}]

      form ->
        [form]
    end) ++ factories
  end
end
