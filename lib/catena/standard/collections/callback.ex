defmodule Catena.Standard.Collections.Callback do
  @moduledoc false
  alias Catena.Calling.Scope
  alias Catena.Foreign.{Callback, Codec}

  def run(description, input, output, limits, body) do
    with :ok <- Callback.verify(description, limits),
         true <- description.input == input and description.output == output do
      Scope.run(
        description.artifact,
        description.core,
        Map.take(limits, [:nodes, :bytes]),
        fn scope ->
          with {:ok, entry} <- Scope.entry(scope, description.name),
               {:ok, handle} <- captures(scope, entry, description.captures) do
            function = fn value ->
              with {:ok, _} <- Codec.from_native(input, value, limits),
                   {:ok, result} <- Scope.call(scope, handle, value),
                   {:ok, native} <- Codec.to_native(output, result, limits) do
                native
              else
                {:error, {:execution_failure, :error, {:catena_trap, reason}}} ->
                  :erlang.error({:catena_trap, reason})

                {:error, {:execution_failure, kind, reason}} ->
                  :erlang.raise(kind, reason, [])

                error ->
                  :erlang.error({:catena_trap, {:collection_callback, error}})
              end
            end

            body.(function)
          end
        end,
        max_handles: length(description.captures) + 2,
        reuse_loaded: true
      )
    else
      _ -> {:error, :invalid_pure_collection_callback}
    end
  end

  defp captures(_, handle, []), do: {:ok, handle}

  defp captures(scope, handle, [value | rest]) do
    with {:ok, next} <- Scope.call(scope, handle, value), do: captures(scope, next, rest)
  end
end
