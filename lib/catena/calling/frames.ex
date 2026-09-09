defmodule Catena.Calling.Frames do
  @moduledoc "Trace actual generated frames to recorded lowering origins without inventing missing frames."

  def explain(artifact, core, stack, options \\ []) when is_list(stack) do
    with :ok <- Catena.Calling.Artifact.verify(artifact, core, options) do
      {:ok, Enum.map(stack, &frame(artifact, &1))}
    end
  end

  defp frame(artifact, {module, name, arity_or_arguments, location} = frame)
       when is_atom(name) and is_list(location) do
    arity =
      if is_list(arity_or_arguments), do: length(arity_or_arguments), else: arity_or_arguments

    entry =
      if module == artifact.module do
        Enum.find(
          artifact.descriptor.functions,
          &(&1.name == Atom.to_string(name) and &1.arity == arity)
        )
      end

    %{technical: frame, source: entry && entry.source}
  end

  defp frame(_, frame), do: %{technical: frame, source: nil}
end
