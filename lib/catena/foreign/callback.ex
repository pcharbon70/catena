defmodule Catena.Foreign.Callback do
  @moduledoc "Verified pure unary callback with immutable, typed data captures."
  alias Catena.Calling.{Artifact, Scope}
  alias Catena.Calling.Adapter, as: Calling
  alias Catena.Foreign.Codec

  def new(artifact, core, name, captures, input, output, limits) do
    with true <- is_list(captures),
         :ok <- Catena.Foreign.Budget.check(List.to_tuple(captures), limits),
         :ok <- Artifact.verify(artifact, core),
         %{kind: :value} <- Enum.find(artifact.descriptor.entries, &(&1.name == name)),
         definition when not is_nil(definition) <- Enum.find(core.definitions, &(&1.name == name)),
         true <- Calling.initial_pure?(definition),
         type <- Map.get(definition, :signature) || definition.scheme.type,
         {:ok, type} <- captures(type, captures, limits),
         {:ok, parameter, result} <- stage(type),
         {:ok, parameter} <- Calling.schema(parameter),
         {:ok, result} <- Calling.schema(result),
         {:ok, ^input} <- Codec.new({:data, parameter}),
         {:ok, ^output} <- Codec.new({:data, result}) do
      {:ok,
       %{
         version: "0.1.61",
         artifact: artifact,
         core: core,
         name: name,
         captures: captures,
         input: input,
         output: output
       }}
    else
      _ -> {:error, :invalid_foreign_callback}
    end
  rescue
    _ -> {:error, :invalid_foreign_callback}
  end

  def verify(callback, limits) do
    case new(
           callback.artifact,
           callback.core,
           callback.name,
           callback.captures,
           callback.input,
           callback.output,
           limits
         ) do
      {:ok, ^callback} -> :ok
      _ -> {:error, :invalid_foreign_callback}
    end
  rescue
    _ -> {:error, :invalid_foreign_callback}
  end

  def invoke(callback, native, limits) do
    with :ok <- verify(callback, limits),
         {:ok, semantic} <- Codec.decode(callback.input, native, limits),
         {:ok, argument} <- Codec.to_native(callback.input, semantic, limits) do
      Scope.run(
        callback.artifact,
        callback.core,
        Map.take(limits, [:nodes, :bytes]),
        fn scope ->
          with {:ok, handle} <- Scope.entry(scope, callback.name),
               {:ok, handle} <- apply_captures(scope, handle, callback.captures),
               {:ok, result} <- Scope.call(scope, handle, argument),
               do: Codec.encode(callback.output, result, limits)
        end,
        max_handles: length(callback.captures) + 2,
        reuse_loaded: true
      )
    end
  end

  defp captures(type, [], _), do: {:ok, type}

  defp captures(type, [value | rest], limits) do
    with {:ok, parameter, result} <- stage(type),
         {:ok, schema} <- Calling.schema(parameter),
         {:ok, codec} <- Codec.new({:data, schema}),
         {:ok, _} <- Codec.from_native(codec, value, limits),
         do: captures(result, rest, limits)
  end

  defp apply_captures(_, handle, []), do: {:ok, handle}

  defp apply_captures(scope, handle, [value | rest]) do
    with {:ok, handle} <- Scope.call(scope, handle, value),
         do: apply_captures(scope, handle, rest)
  end

  defp stage({:function, parameter, [], result}), do: {:ok, parameter, result}
  defp stage({:function, parameter, result}), do: {:ok, parameter, result}
  defp stage(_), do: {:error, :effectful_or_nonfunction_callback}
end
