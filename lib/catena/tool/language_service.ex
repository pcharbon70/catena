defmodule Catena.Tool.LanguageService do
  @moduledoc "Immutable retained-input language-service core for the grammar-independent G123 slice."

  alias Catena.{CanonicalJCS, Diagnostic, Identifier, LanguageVersion}
  alias Catena.Diagnostic.Contract

  @version "0.1.96"
  @maximum_input_bytes 16_777_216
  @maximum_symbols 4_096
  @maximum_cancelled_requests 256
  @maximum_completion_prefix_bytes 256
  @maximum_rename_edits 1_024
  @maximum_result_bytes 16_777_216
  @methods ~w(diagnostics hover completion definition semantic_tokens rename formatting)

  defmodule Snapshot do
    @moduledoc "One immutable retained-JSON document snapshot and its shared compiler analysis."
    @enforce_keys [:uri, :version, :digest, :source, :analysis, :diagnostics, :symbols]
    defstruct @enforce_keys ++ [:document, :previous_digest, cancelled: MapSet.new()]
  end

  def profile do
    %{
      version: @version,
      input: :retained_json_ast,
      public_source_protocol: :held_for_p109,
      transport: :held_for_p109,
      snapshot_identity: :uri_version_and_sha256,
      analysis_authority: :shared_compiler,
      result_freshness: :exact_snapshot,
      cancellation: :request_id_before_dispatch,
      diagnostics: :stable_snapshot_bound_identity,
      coordinates: :retained_json_path,
      completion_visibility: :exports_only,
      rename: :identity_bound_exact_preview,
      formatting: :held_for_p109,
      methods: @methods,
      maximum_input_bytes: @maximum_input_bytes,
      maximum_symbols: @maximum_symbols,
      maximum_cancelled_requests: @maximum_cancelled_requests,
      maximum_completion_prefix_bytes: @maximum_completion_prefix_bytes,
      maximum_rename_edits: @maximum_rename_edits,
      maximum_result_bytes: @maximum_result_bytes
    }
  end

  def open(uri, version, source)
      when is_binary(uri) and uri != "" and is_integer(version) and version >= 0 and
             is_binary(source) do
    if byte_size(source) <= @maximum_input_bytes do
      build_snapshot(uri, version, source, nil)
    else
      {:error, :language_service_limit_exceeded}
    end
  end

  def open(_, _, _), do: {:error, :invalid_language_service_snapshot}

  def change(%Snapshot{} = snapshot, version, source)
      when is_integer(version) and version > snapshot.version and is_binary(source) do
    if byte_size(source) <= @maximum_input_bytes do
      build_snapshot(snapshot.uri, version, source, snapshot.digest)
    else
      {:error, :language_service_limit_exceeded}
    end
  end

  def change(%Snapshot{}, _, _), do: {:error, :stale_language_service_change}
  def change(_, _, _), do: {:error, :invalid_language_service_snapshot}

  def cancel(%Snapshot{} = snapshot, request_id)
      when is_binary(request_id) and request_id != "" do
    cancelled = MapSet.put(snapshot.cancelled, request_id)

    if MapSet.size(cancelled) <= @maximum_cancelled_requests,
      do: {:ok, %{snapshot | cancelled: cancelled}},
      else: {:error, :language_service_limit_exceeded}
  end

  def cancel(_, _), do: {:error, :invalid_language_service_request}

  def request(%Snapshot{} = snapshot, request) when is_map(request) do
    with {:ok, id, method, params} <- request_envelope(snapshot, request) do
      identity = snapshot_identity(snapshot)

      if MapSet.member?(snapshot.cancelled, id) do
        {:ok, %{"id" => id, "snapshot" => identity, "status" => "cancelled"}}
      else
        dispatch(snapshot, id, method, params, identity)
      end
    end
  rescue
    _ -> {:error, :invalid_language_service_request}
  end

  def request(_, _), do: {:error, :invalid_language_service_request}

  def apply_rename(%Snapshot{} = snapshot, plan, next_version)
      when is_map(plan) and is_integer(next_version) and next_version > snapshot.version do
    with :ok <- verify_rename_plan(snapshot, plan),
         {:ok, result} <- Base.decode64(plan["result"]),
         true <- byte_size(result) <= @maximum_result_bytes,
         true <- digest(result) == plan["result_digest"] do
      change(snapshot, next_version, result)
    else
      false -> {:error, :invalid_language_service_edit}
      {:error, _} = error -> error
      _ -> {:error, :invalid_language_service_edit}
    end
  end

  def apply_rename(%Snapshot{}, _, _), do: {:error, :stale_language_service_change}
  def apply_rename(_, _, _), do: {:error, :invalid_language_service_edit}

  defp build_snapshot(uri, version, source, previous_digest) do
    digest_value = digest(source)

    {document, analysis, diagnostics, symbols} =
      case Catena.check_json(source) do
        {:ok, core} ->
          {:ok, document} = JSON.decode(source)
          symbols = symbol_index(core)

          if length(symbols) <= @maximum_symbols do
            {document, {:ok, core}, [], symbols}
          else
            {document, {:error, :language_service_limit_exceeded},
             [limit_diagnostic(uri, version, digest_value)], []}
          end

        {:error, %Diagnostic{} = diagnostic} ->
          document =
            case JSON.decode(source) do
              {:ok, value} -> value
              _ -> nil
            end

          {document, {:error, diagnostic}, [diagnostic], []}
      end

    snapshot = %Snapshot{
      uri: uri,
      version: version,
      digest: digest_value,
      source: source,
      document: document,
      analysis: analysis,
      diagnostics: diagnostics,
      symbols: symbols,
      previous_digest: previous_digest
    }

    {:ok, snapshot}
  rescue
    _ -> {:error, :invalid_language_service_snapshot}
  end

  defp request_envelope(snapshot, request) do
    id = field(request, "id")
    method = field(request, "method")
    params = field(request, "params") || %{}

    cond do
      not (is_binary(id) and id != "" and method in @methods and is_map(params)) ->
        {:error, :invalid_language_service_request}

      field(request, "uri") != snapshot.uri or field(request, "version") != snapshot.version or
          field(request, "digest") != snapshot.digest ->
        {:error, :stale_language_service_request}

      true ->
        {:ok, id, method, params}
    end
  end

  defp dispatch(snapshot, id, "diagnostics", _params, identity) do
    result = Enum.map(snapshot.diagnostics, &diagnostic_record(&1, snapshot))
    response(id, identity, result)
  end

  defp dispatch(_snapshot, id, "formatting", _params, identity) do
    {:ok,
     %{
       "id" => id,
       "snapshot" => identity,
       "status" => "held-for-p109",
       "reason" => "public-source-formatting-held-for-p109"
     }}
  end

  defp dispatch(%Snapshot{analysis: {:error, _}}, _id, _method, _params, _identity),
    do: {:error, :language_service_analysis_unavailable}

  defp dispatch(snapshot, id, "completion", params, identity) do
    prefix = field(params, "prefix") || ""

    if is_binary(prefix) and String.valid?(prefix) and
         byte_size(prefix) <= @maximum_completion_prefix_bytes do
      result =
        snapshot.symbols
        |> Enum.filter(
          &(&1["visibility"] == "public" and String.starts_with?(&1["name"], prefix))
        )
        |> Enum.map(&Map.take(&1, ~w(id name kind visibility)))

      response(id, identity, result)
    else
      {:error, :language_service_limit_exceeded}
    end
  end

  defp dispatch(snapshot, id, "hover", params, identity) do
    with {:ok, symbol} <- find_symbol(snapshot, field(params, "symbol_id")) do
      response(id, identity, Map.take(symbol, ~w(id name kind visibility type uses)))
    end
  end

  defp dispatch(snapshot, id, "definition", params, identity) do
    with {:ok, symbol} <- find_symbol(snapshot, field(params, "symbol_id")) do
      response(id, identity, %{
        "uri" => snapshot.uri,
        "path" => symbol["path"],
        "symbol_id" => symbol["id"]
      })
    end
  end

  defp dispatch(snapshot, id, "semantic_tokens", _params, identity) do
    tokens = Enum.map(snapshot.symbols, &Map.take(&1, ~w(id path kind visibility)))

    response(id, identity, %{
      "coordinate_space" => "retained-json-path",
      "public_source_coordinates" => "held-for-p109",
      "tokens" => tokens
    })
  end

  defp dispatch(snapshot, id, "rename", params, identity) do
    with {:ok, symbol} <- find_symbol(snapshot, field(params, "symbol_id")),
         {:ok, identifier} <- parse_identifier(field(params, "new_name")),
         :ok <- unique_new_name(snapshot, symbol, identifier.canonical),
         :ok <- unreferenced_top_level(snapshot, symbol),
         {:ok, plan} <- rename_plan(snapshot, symbol, identifier.canonical) do
      response(id, identity, plan)
    end
  end

  defp response(id, identity, result),
    do: {:ok, %{"id" => id, "snapshot" => identity, "status" => "ok", "result" => result}}

  defp symbol_index(core) do
    exports = MapSet.new(core.exports)

    core.definitions
    |> Enum.map(fn definition ->
      body = %{
        "module" => to_string(core.module),
        "kind" => to_string(definition.kind),
        "name" => definition.name
      }

      %{
        "id" => CanonicalJCS.digest(body),
        "name" => definition.name,
        "kind" => to_string(definition.kind),
        "visibility" =>
          if(MapSet.member?(exports, definition.name), do: "public", else: "private"),
        "path" => definition.path,
        "type" => stringify(Contract.present_type(definition.scheme.type)),
        "uses" =>
          Enum.sort(definition.verified_uses_row.entries, &effect_sort/2)
          |> Enum.map(& &1.capability)
      }
    end)
    |> Enum.sort_by(&{&1["name"], &1["kind"], &1["id"]})
  end

  defp effect_sort(left, right), do: left.capability <= right.capability

  defp find_symbol(snapshot, id) when is_binary(id) do
    case Enum.find(snapshot.symbols, &(&1["id"] == id)) do
      nil -> {:error, :unknown_language_service_symbol}
      symbol -> {:ok, symbol}
    end
  end

  defp find_symbol(_, _), do: {:error, :unknown_language_service_symbol}

  defp parse_identifier(value) when is_binary(value) do
    Identifier.parse(value,
      language_selection: LanguageVersion.legacy_selection("0.1.10")
    )
  end

  defp parse_identifier(_), do: {:error, :invalid_language_service_rename}

  defp unique_new_name(snapshot, symbol, name) do
    if Enum.any?(snapshot.symbols, &(&1["name"] == name and &1["id"] != symbol["id"])),
      do: {:error, :language_service_rename_conflict},
      else: :ok
  end

  defp unreferenced_top_level(%Snapshot{analysis: {:ok, core}}, symbol) do
    referenced? =
      Enum.any?(core.definitions, fn definition ->
        contains_variable?(definition.expression, symbol["name"])
      end)

    if referenced?,
      do: {:error, :language_service_rename_requires_resolved_occurrences},
      else: :ok
  end

  defp contains_variable?(%{tag: :variable, name: name}, target), do: name == target

  defp contains_variable?(map, target) when is_map(map),
    do: Enum.any?(Map.values(map), &contains_variable?(&1, target))

  defp contains_variable?(list, target) when is_list(list),
    do: Enum.any?(list, &contains_variable?(&1, target))

  defp contains_variable?(_, _), do: false

  defp rename_plan(snapshot, symbol, new_name) do
    definition_index = definition_index(symbol["path"])

    with index when is_integer(index) <- definition_index,
         true <- is_map(snapshot.document),
         definitions when is_list(definitions) <- snapshot.document["definitions"],
         definition when is_map(definition) <- Enum.at(definitions, index),
         true <- definition["name"] == symbol["name"] do
      exports = snapshot.document["exports"] || []

      edits =
        [
          %{
            "kind" => "json-edit",
            "operation" => "replace",
            "path" => "$.definitions[#{index}].name",
            "value" => new_name,
            "applicability" => "machine-applicable"
          }
        ] ++
          (exports
           |> Enum.with_index()
           |> Enum.filter(fn {name, _} -> name == symbol["name"] end)
           |> Enum.map(fn {_, export_index} ->
             %{
               "kind" => "json-edit",
               "operation" => "replace",
               "path" => "$.exports[#{export_index}]",
               "value" => new_name,
               "applicability" => "machine-applicable"
             }
           end))

      if length(edits) <= @maximum_rename_edits do
        result_document =
          snapshot.document
          |> put_in(["definitions", Access.at(index), "name"], new_name)
          |> Map.put(
            "exports",
            Enum.map(exports, &if(&1 == symbol["name"], do: new_name, else: &1))
          )

        result = CanonicalJCS.encode(result_document) <> "\n"

        if byte_size(result) <= @maximum_result_bytes do
          body = %{
            "format" => "catena-language-service-rename",
            "version" => @version,
            "uri" => snapshot.uri,
            "snapshot_version" => snapshot.version,
            "preimage_digest" => snapshot.digest,
            "symbol_id" => symbol["id"],
            "old_name" => symbol["name"],
            "new_name" => new_name,
            "edits" => edits,
            "result" => Base.encode64(result),
            "result_digest" => digest(result)
          }

          {:ok, Map.put(body, "digest", CanonicalJCS.digest(body))}
        else
          {:error, :language_service_limit_exceeded}
        end
      else
        {:error, :language_service_limit_exceeded}
      end
    else
      _ -> {:error, :invalid_language_service_rename}
    end
  end

  defp verify_rename_plan(snapshot, %{"digest" => plan_digest} = plan) do
    with true <- plan["format"] == "catena-language-service-rename",
         true <- plan["version"] == @version,
         true <- plan["uri"] == snapshot.uri,
         true <- plan["snapshot_version"] == snapshot.version,
         true <- plan["preimage_digest"] == snapshot.digest,
         true <- plan_digest == CanonicalJCS.digest(Map.delete(plan, "digest")),
         true <- is_list(plan["edits"]) and length(plan["edits"]) <= @maximum_rename_edits,
         {:ok, symbol} <- find_symbol(snapshot, plan["symbol_id"]),
         {:ok, identifier} <- parse_identifier(plan["new_name"]),
         :ok <- unique_new_name(snapshot, symbol, identifier.canonical),
         :ok <- unreferenced_top_level(snapshot, symbol),
         {:ok, expected} <- rename_plan(snapshot, symbol, identifier.canonical),
         true <- expected == plan do
      :ok
    else
      _ -> {:error, :invalid_language_service_edit}
    end
  end

  defp verify_rename_plan(_, _), do: {:error, :invalid_language_service_edit}

  defp definition_index(path) when is_binary(path) do
    case Regex.run(~r/^\$\.definitions\[([0-9]+)\]$/, path) do
      [_, value] -> String.to_integer(value)
      _ -> nil
    end
  end

  defp diagnostic_record(diagnostic, snapshot) do
    body = %{
      "compiler_id" => diagnostic.id,
      "message" => diagnostic.message,
      "path" => diagnostic.path,
      "severity" => to_string(diagnostic.severity),
      "uri" => snapshot.uri,
      "snapshot_version" => snapshot.version,
      "snapshot_digest" => snapshot.digest
    }

    Map.put(body, "stable_id", CanonicalJCS.digest(body))
  end

  defp limit_diagnostic(_uri, _version, _digest),
    do: Diagnostic.new("LSP001", "language-service symbol limit exceeded", path: "$")

  defp snapshot_identity(snapshot),
    do: %{"uri" => snapshot.uri, "version" => snapshot.version, "digest" => snapshot.digest}

  defp field(map, key), do: Map.get(map, key, Map.get(map, String.to_atom(key)))
  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)

  defp stringify(map) when is_map(map),
    do: Map.new(map, fn {key, value} -> {to_string(key), stringify(value)} end)

  defp stringify(value) when is_atom(value), do: Atom.to_string(value)
  defp stringify(value), do: value
end
