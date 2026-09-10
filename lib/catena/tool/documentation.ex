defmodule Catena.Tool.Documentation do
  @moduledoc "Deterministic interface-linked documentation over retained Catena inputs."

  alias Catena.{CanonicalJCS, Comment, Interface}
  alias Catena.Tool.TestRunner

  @version "0.1.92"
  @kinds ~w(type value trait instance effect handler claim)
  @maximum_nodes 4_096
  @maximum_body_bytes 65_536
  @link ~r/\[\[([^\]]+)\]\]/u
  @raw_html ~r/<\/?[A-Za-z!][^>]*>/u
  @doctest ~r/```catena doctest\n(.*?)\n```/su

  def profile do
    %{
      version: @version,
      graph_source: :verified_interface,
      symbol_kinds: @kinds,
      public_visibility: :interface_only,
      internal_view: :explicit_authorization,
      raw_html: :refused,
      links: :resolved_or_refused,
      doctests: :explicit_retained_inputs,
      environmental_services: :denied,
      public_source_examples: :held_for_p109,
      maximum_nodes: @maximum_nodes,
      maximum_documentation_body_bytes: @maximum_body_bytes
    }
  end

  def build(interface_binary, attachments, options \\ [])

  def build(interface_binary, attachments, options)
      when is_binary(interface_binary) and is_list(attachments) and is_list(options) do
    with {:ok, _verified} <- Interface.decode(interface_binary),
         {:ok, raw} <- decode_raw(interface_binary),
         {:ok, dependencies} <- dependencies(Keyword.get(options, :dependencies, [])),
         {:ok, nodes} <- nodes(raw, Keyword.get(options, :internal_symbols, []), options),
         {:ok, documented} <- attach(nodes, attachments),
         {:ok, linked} <- resolve_links(documented, dependencies),
         {:ok, reports} <- run_doctests(linked, raw["digest"]) do
      graph = %{
        "format" => "catena-documentation-graph",
        "version" => @version,
        "module" => raw["module"],
        "interface_digest" => raw["digest"],
        "nodes" => linked,
        "doctests" => reports,
        "public_source_examples" => "held-for-p109"
      }

      {:ok, Map.put(graph, "digest", CanonicalJCS.digest(graph))}
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_documentation_input}
    end
  rescue
    _ -> {:error, :invalid_documentation_input}
  end

  def build(_, _, _), do: {:error, :invalid_documentation_input}

  def render(%{"format" => "catena-documentation-graph"} = graph) do
    expected = graph |> Map.delete("digest") |> CanonicalJCS.digest()

    if graph["digest"] == expected do
      body =
        graph["nodes"]
        |> Enum.map_join("\n", fn node ->
          details = node["details"] |> CanonicalJCS.encode()

          "## #{node["name"]}\n\n" <>
            "<a id=\"#{node["anchor"]}\"></a>\n\n" <>
            "**#{node["kind"]}** `#{node["id"]}`\n\n" <>
            render_body(node["documentation"], node["links"]) <>
            "\n```json\n#{details}\n```\n"
        end)

      {:ok,
       "# #{graph["module"]}\n\n" <>
         "Interface digest: `#{graph["interface_digest"]}`\n\n" <> body}
    else
      {:error, :forged_documentation_graph}
    end
  rescue
    _ -> {:error, :invalid_documentation_graph}
  end

  def render(_), do: {:error, :invalid_documentation_graph}

  defp decode_raw(binary) do
    case JSON.decode(binary) do
      {:ok, raw} when is_map(raw) -> {:ok, raw}
      _ -> {:error, :invalid_interface}
    end
  end

  defp dependencies(binaries) when is_list(binaries) do
    result =
      Enum.reduce_while(binaries, {:ok, MapSet.new(), %{}}, fn binary, {:ok, modules, known} ->
        with true <- is_binary(binary),
             {:ok, _} <- Interface.decode(binary),
             {:ok, raw} <- decode_raw(binary),
             {:ok, nodes} <- nodes(raw, [], []) do
          additions = Map.new(nodes, &{&1["id"], %{module: raw["module"], anchor: &1["anchor"]}})

          if MapSet.member?(modules, raw["module"]),
            do: {:halt, {:error, :duplicate_dependency_module}},
            else: {:cont, {:ok, MapSet.put(modules, raw["module"]), Map.merge(known, additions)}}
        else
          _ -> {:halt, {:error, :invalid_dependency_interface}}
        end
      end)

    case result do
      {:ok, _modules, known} -> {:ok, known}
      {:error, _} = error -> error
    end
  end

  defp dependencies(_), do: {:error, :invalid_dependency_interface}

  defp nodes(raw, internal, options) do
    with true <- is_binary(raw["module"]),
         {:ok, public} <- public_nodes(raw),
         {:ok, extra} <- internal_nodes(internal, options),
         all = Enum.sort_by(public ++ extra, & &1["id"]),
         true <- length(all) <= @maximum_nodes,
         true <- unique?(Enum.map(all, & &1["id"])),
         true <- unique?(Enum.map(all, & &1["anchor"])) do
      {:ok, all}
    else
      false -> {:error, :duplicate_or_excessive_documentation_nodes}
      {:error, _} = error -> error
      _ -> {:error, :invalid_documentation_nodes}
    end
  end

  defp public_nodes(raw) do
    Enum.reduce_while(@kinds, {:ok, []}, fn kind, {:ok, acc} ->
      key = if kind == "type", do: "types", else: kind <> "s"

      case Map.get(raw, key, []) do
        records when is_list(records) ->
          built = Enum.map(records, &node(raw["module"], kind, &1, "public"))
          {:cont, {:ok, acc ++ built}}

        _ ->
          {:halt, {:error, :invalid_interface_symbols}}
      end
    end)
  end

  defp internal_nodes([], _), do: {:ok, []}

  defp internal_nodes(records, options) when is_list(records) do
    if Keyword.get(options, :authorized_internal, false) do
      if Enum.all?(records, &(is_map(&1) and is_binary(&1["module"]) and &1["kind"] in @kinds)),
        do: {:ok, Enum.map(records, &node(&1["module"], &1["kind"], &1, "internal"))},
        else: {:error, :invalid_internal_symbols}
    else
      {:error, :internal_documentation_not_authorized}
    end
  end

  defp internal_nodes(_, _), do: {:error, :invalid_internal_symbols}

  defp node(module, kind, record, visibility) do
    name = record["name"] || record["id"] || record["trait"] || "anonymous"
    id = "#{module}.#{kind}.#{name}"

    %{
      "id" => id,
      "anchor" => anchor(id),
      "kind" => kind,
      "name" => name,
      "visibility" => visibility,
      "details" => record,
      "documentation" => "",
      "links" => []
    }
  end

  defp attach(nodes, attachments) do
    docs =
      Enum.reduce_while(attachments, {:ok, %{}}, fn attachment, {:ok, acc} ->
        case attachment_fields(attachment) do
          {target, body}
          when is_binary(target) and is_binary(body) and byte_size(body) <= @maximum_body_bytes ->
            cond do
              body == "" -> {:halt, {:error, :empty_documentation_attachment}}
              Regex.match?(@raw_html, body) -> {:halt, {:error, :active_raw_html_refused}}
              Map.has_key?(acc, target) -> {:halt, {:error, :duplicate_documentation_attachment}}
              true -> {:cont, {:ok, Map.put(acc, target, body)}}
            end

          _ ->
            {:halt, {:error, :invalid_documentation_attachment}}
        end
      end)

    with {:ok, docs} <- docs,
         ids = MapSet.new(nodes, & &1["id"]),
         true <- Enum.all?(Map.keys(docs), &MapSet.member?(ids, &1)) do
      {:ok, Enum.map(nodes, &Map.put(&1, "documentation", Map.get(docs, &1["id"], "")))}
    else
      false -> {:error, :hidden_or_unknown_documentation_target}
      {:error, _} = error -> error
    end
  end

  defp attachment_fields(%Comment.Attachment{target_id: target, body: body}),
    do: {to_string(target), body}

  defp attachment_fields(%{target_id: target, body: body}), do: {to_string(target), body}
  defp attachment_fields(%{"target_id" => target, "body" => body}), do: {target, body}
  defp attachment_fields(_), do: :error

  defp resolve_links(nodes, dependencies) do
    local = Map.new(nodes, &{&1["id"], %{module: nil, anchor: &1["anchor"]}})
    known = Map.merge(dependencies, local)

    Enum.reduce_while(nodes, {:ok, []}, fn node, {:ok, acc} ->
      targets =
        Regex.scan(@link, node["documentation"], capture: :all_but_first) |> List.flatten()

      if Enum.all?(targets, &Map.has_key?(known, &1)) do
        links =
          Enum.map(targets, fn target ->
            location = known[target]
            %{"target" => target, "module" => location.module, "anchor" => location.anchor}
          end)

        {:cont, {:ok, [Map.put(node, "links", links) | acc]}}
      else
        {:halt, {:error, :unresolved_documentation_link}}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  defp run_doctests(nodes, subject_digest) do
    with true <- digest?(subject_digest),
         {:ok, cases} <- doctest_cases(nodes, subject_digest) do
      if cases == [] do
        {:ok, []}
      else
        with {:ok, suite} <- TestRunner.define("documentation", subject_digest, cases, seed: 0),
             {:ok, report} <- TestRunner.run(suite, subject_digest),
             "pass" <- report["status"] do
          {:ok, report["results"]}
        else
          {:ok, report} -> {:error, {:doctest_failed, report}}
          {:error, _} = error -> error
          _ -> {:error, :doctest_failed}
        end
      end
    else
      false -> {:error, :invalid_interface_digest}
      {:error, _} = error -> error
    end
  end

  defp doctest_cases(nodes, subject_digest) do
    nodes
    |> Enum.flat_map(fn node ->
      Regex.scan(@doctest, node["documentation"], capture: :all_but_first)
      |> List.flatten()
      |> Enum.with_index(1)
      |> Enum.map(fn {json, index} -> {node["id"], index, json} end)
    end)
    |> Enum.reduce_while({:ok, []}, fn {target, index, json}, {:ok, acc} ->
      with {:ok, envelope} when is_map(envelope) <- JSON.decode(json),
           ^subject_digest <- envelope["subject_digest"],
           [] <- Map.get(envelope, "effects", []),
           true <- envelope["format"] in ["json", "kernel"],
           true <- is_map(envelope["expect"]) do
        test_case = %{
          id: "#{target}:#{index}",
          kind: :unit,
          effects: [],
          execute: fn _context -> execute_example(envelope) end
        }

        {:cont, {:ok, [test_case | acc]}}
      else
        _ -> {:halt, {:error, :invalid_or_stale_doctest}}
      end
    end)
    |> case do
      {:ok, cases} -> {:ok, Enum.reverse(cases)}
      error -> error
    end
  end

  defp execute_example(%{"format" => "json", "source" => source, "expect" => expected})
       when is_map(source),
       do: compare(Catena.check_json(JSON.encode!(source)), expected)

  defp execute_example(%{"format" => "kernel", "source" => source, "expect" => expected})
       when is_binary(source),
       do: compare(Catena.check_kernel(source), expected)

  defp execute_example(_), do: {:fail, %{"reason" => "invalid-example"}}

  defp compare({:ok, _}, %{"status" => "ok"}), do: :pass
  defp compare({:error, %{id: id}}, %{"status" => "error", "id" => id}), do: :pass
  defp compare(actual, _), do: {:fail, %{"actual" => inspect(actual, limit: 20)}}

  defp render_body("", _links), do: "No documentation supplied.\n"

  defp render_body(body, links) do
    index = Map.new(links, &{&1["target"], &1})

    Regex.replace(@link, body, fn _, target ->
      link = index[target]
      prefix = if link["module"], do: "#{link["module"]}.md", else: ""
      "[#{target}](#{prefix}##{link["anchor"]})"
    end) <> "\n"
  end

  defp anchor(id) do
    id
    |> String.downcase()
    |> String.replace(~r/[^a-z0-9]+/u, "-")
    |> String.trim("-")
  end

  defp unique?(items), do: length(items) == length(Enum.uniq(items))
  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)
end
