defmodule Catena.Artifact.Migration do
  @moduledoc "Deterministic adjacent artifact migration with immutable input retention."
  alias Catena.CanonicalJSON
  @version "0.1.80"
  @supported ~w(0.1.6 0.1.7 0.1.8)

  def profile,
    do: %{
      version: @version,
      supported_formats: @supported,
      migration: :adjacent_deterministic,
      original_retained: true,
      signature_rewriting: false,
      unknown_future_formats: :refused
    }

  def ledger(interpreters, migrations) when is_map(interpreters) and is_map(migrations) do
    if Enum.sort(Map.keys(interpreters)) == @supported and
         Enum.all?(migrations, fn {{from, to}, fun} ->
           adjacent?(from, to) and is_function(fun, 1)
         end),
       do: {:ok, %{interpreters: interpreters, migrations: migrations}},
       else: {:error, :invalid_evolution_ledger}
  end

  def replay(bytes, ledger, context \\ %{}) when is_binary(bytes) do
    with {:ok, document} <- decode(bytes),
         version when version in @supported <- document["version"],
         interpreter when is_function(interpreter, 2) <- ledger.interpreters[version],
         :ok <- archived_context(context) do
      interpreter.(document, context)
    else
      nil -> {:error, :missing_historical_interpreter}
      version when is_binary(version) -> {:error, {:unsupported_artifact_version, version}}
      error -> error
    end
  end

  def migrate(bytes, target, ledger) when target in @supported do
    with {:ok, document} <- decode(bytes),
         source when source in @supported <- document["version"],
         {:ok, migrated, path} <- migrate_steps(document, source, target, ledger, []) do
      {:ok,
       %{
         "format" => "catena-derived-artifact",
         "version" => target,
         "source_version" => source,
         "source_digest" => digest(bytes),
         "source_bytes" => Base.encode64(bytes),
         "migration_path" => path,
         "document" => migrated
       }}
    else
      version when is_binary(version) -> {:error, {:unsupported_artifact_version, version}}
      error -> error
    end
  end

  def migrate(_bytes, target, _ledger), do: {:error, {:unsupported_artifact_version, target}}
  def encode_derived(envelope), do: CanonicalJSON.encode(envelope)

  def verify_derived(envelope) do
    with "catena-derived-artifact" <- envelope["format"],
         {:ok, source} <- Base.decode64(envelope["source_bytes"]),
         true <- digest(source) == envelope["source_digest"],
         {:ok, original} <- decode(source),
         true <- original["version"] == envelope["source_version"],
         do: :ok,
         else: (_ -> {:error, :invalid_derived_artifact})
  end

  defp migrate_steps(document, version, version, _ledger, path),
    do: {:ok, document, Enum.reverse(path)}

  defp migrate_steps(document, from, target, ledger, path) do
    next = next(from)

    case ledger.migrations[{from, next}] do
      nil ->
        {:error, {:missing_adjacent_migration, from, next}}

      migration ->
        case migration.(document) do
          {:ok, %{"version" => ^next} = next_document, []} ->
            migrate_steps(next_document, next, target, ledger, [from <> "->" <> next | path])

          {:ok, _document, losses} when is_list(losses) ->
            {:error, {:semantic_loss, losses}}

          _ ->
            {:error, {:invalid_adjacent_migration, from, next}}
        end
    end
  end

  defp decode(bytes) do
    case JSON.decode(bytes) do
      {:ok, document} when is_map(document) -> {:ok, document}
      _ -> {:error, :invalid_historical_artifact}
    end
  end

  defp archived_context(context) do
    cond do
      Map.get(context, :root_status) == :revoked and not Map.get(context, :historical_root, false) ->
        {:error, :missing_historical_root}

      not Map.get(context, :tool_available, true) ->
        {:error, :missing_archived_tool}

      not Map.get(context, :dependencies_available, true) ->
        {:error, :missing_archived_dependency}

      true ->
        :ok
    end
  end

  defp adjacent?(from, to), do: next(from) == to
  defp next(version), do: @supported |> Enum.drop_while(&(&1 != version)) |> Enum.at(1)
  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
