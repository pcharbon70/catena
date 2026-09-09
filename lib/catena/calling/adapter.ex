defmodule Catena.Calling.Adapter do
  @moduledoc "Checked saturated pure-data entry adapter; scoped and lifecycle calls have separate owners."
  alias Catena.Calling.Artifact
  alias Catena.ValueBoundary.Data

  def invoke(artifact, core, name, arguments, limits, options \\ []) do
    with :ok <- Artifact.verify(artifact, core, options),
         %{kind: :value} = entry <- Enum.find(artifact.descriptor.entries, &(&1.name == name)),
         true <- is_list(arguments) and length(arguments) == entry.beam_arity,
         definition <- Enum.find(core.definitions, &(&1.name == name)),
         true <- initial_pure?(definition),
         type <- Map.get(definition, :signature) || definition.scheme.type,
         {:ok, parameters, result} <- consume(type, entry.beam_arity),
         {:ok, parameters} <- schemas(parameters),
         {:ok, result} <- schema(result),
         :ok <- validate_arguments(parameters, arguments, limits),
         {:module, module} <-
           Catena.OTP.Compiler.load(artifact.module, ~c"calling-adapter.beam", artifact.binary) do
      execute(module, entry.symbol, arguments, result, limits)
    else
      false -> {:error, :invalid_arity_or_effect_boundary}
      nil -> {:error, :unknown_call_entry}
      {:error, _} = error -> error
      _ -> {:error, :unsupported_call_entry}
    end
  rescue
    _ -> {:error, :malformed_call_request}
  end

  defp execute(module, name, arguments, result, limits) do
    value = apply(module, String.to_existing_atom(name), arguments)

    case Data.decode(result, value, limits) do
      {:ok, semantic} -> {:ok, semantic}
      {:error, reason} -> {:error, {:result_boundary_failure, reason}}
    end
  catch
    kind, reason -> {:error, {:execution_failure, kind, reason}}
  end

  def initial_pure?(%{uses: []}), do: true
  def initial_pure?(%{uses: %{row: %{entries: [], tail: nil}}}), do: true

  def initial_pure?(definition),
    do: not Map.has_key?(definition, :uses) or definition.uses == nil

  defp consume(type, 0), do: {:ok, [], type}

  defp consume({:function, parameter, result}, count) when count > 0 do
    with {:ok, rest, result} <- consume(result, count - 1), do: {:ok, [parameter | rest], result}
  end

  defp consume({:function, parameter, [], result}, count) when count > 0,
    do: consume({:function, parameter, result}, count)

  defp consume(_, _), do: {:error, :unsupported_application_stage}

  def schema(type) when type in [:integer, :boolean, :unit, :float, :text, :character, :bytes],
    do: {:ok, type}

  def schema({:tuple, elements}) do
    with {:ok, elements} <- schemas(elements), do: {:ok, {:tuple, elements}}
  end

  def schema({tag, %{fields: fields, tail: nil}}) when tag in [:record, :variant] do
    labels = Enum.sort(Map.keys(fields))

    with {:ok, types} <- schemas(Enum.map(labels, &fields[&1])),
         do: {:ok, {tag, Map.new(Enum.zip(labels, types))}}
  end

  def schema(_), do: {:error, :unsupported_data_boundary}

  defp schemas(types) do
    Enum.reduce_while(types, {:ok, []}, fn type, {:ok, acc} ->
      case schema(type) do
        {:ok, type} -> {:cont, {:ok, [type | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, types} -> {:ok, Enum.reverse(types)}
      error -> error
    end
  end

  defp validate_arguments(types, values, limits) do
    # One product validation shares a single budget across the complete argument vector.
    case Data.decode({:tuple, types}, List.to_tuple(values), limits) do
      {:ok, _} -> :ok
      error -> error
    end
  end
end
