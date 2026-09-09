defmodule Catena.Trust.Obligations do
  @moduledoc "Exact transitive foreign/native disclosure derived from checked build inputs."
  alias Catena.Calling.Descriptor, as: Identity
  alias Catena.Foreign.{Descriptor, Program}
  alias Catena.Foreign.Native.Package

  def build(root, inputs) when is_binary(root) and is_map(inputs) and map_size(inputs) in 1..64 do
    with true <- byte_size(:erlang.term_to_binary(inputs)) <= 64 * 1024 * 1024,
         true <- Map.has_key?(inputs, root),
         true <- Enum.all?(inputs, fn {name, node} -> valid_node?(name, node, inputs) end),
         true <- Enum.sum(Enum.map(inputs, fn {_, n} -> length(n.dependencies) end)) <= 256,
         {:ok, records} <- visit(root, inputs, %{}, []),
         true <- map_size(records) == map_size(inputs) do
      document = %{
        "format" => "catena-trusted-obligations",
        "version" => "0.1.71",
        "root" => root,
        "nodes" => records,
        "trust_profile" => Catena.Trust.Profile.summary()["digest"]
      }

      document = Map.put(document, "digest", Catena.CanonicalJCS.digest(document))

      if length(records[root]["obligations"]) <= 256 and
           byte_size(Catena.CanonicalJCS.encode(document)) <= 1_048_576 do
        {:ok, %{root: root, inputs: inputs, document: document}}
      else
        {:error, :trusted_graph_limit}
      end
    else
      _ -> {:error, :invalid_trusted_dependency_graph}
    end
  rescue
    _ -> {:error, :invalid_trusted_dependency_graph}
  end

  def build(_, _), do: {:error, :invalid_trusted_dependency_graph}

  def verify(graph) do
    case build(graph.root, graph.inputs) do
      {:ok, ^graph} -> :ok
      _ -> {:error, :changed_trusted_dependency_graph}
    end
  rescue
    _ -> {:error, :invalid_trusted_dependency_graph}
  end

  def decode(binary) when is_binary(binary) and byte_size(binary) <= 1_048_576 do
    with {:ok, doc} <- Catena.CanonicalJCS.decode(binary, canonical: true),
         true <- Enum.sort(Map.keys(doc)) == ~w(digest format nodes root trust_profile version),
         "catena-trusted-obligations" <- doc["format"],
         "0.1.71" <- doc["version"],
         true <- doc["digest"] == Catena.CanonicalJCS.digest(Map.delete(doc, "digest")),
         true <- is_map(doc["nodes"]) and map_size(doc["nodes"]) in 1..64,
         true <- Map.has_key?(doc["nodes"], doc["root"]) do
      {:ok, doc}
    else
      _ -> {:error, :invalid_trusted_obligation_document}
    end
  rescue
    _ -> {:error, :invalid_trusted_obligation_document}
  end

  def decode(_), do: {:error, :invalid_trusted_obligation_document}

  def verify_document(binary, graph) do
    with :ok <- verify(graph),
         {:ok, doc} <- decode(binary),
         true <- doc == graph.document,
         do: :ok,
         else: (_ -> {:error, :unbound_trusted_obligation_document})
  end

  def obligations(graph, name), do: graph.document["nodes"][name]["obligations"]

  defp valid_node?(name, node, inputs) do
    is_binary(name) and byte_size(name) in 1..128 and String.valid?(name) and
      Enum.sort(Map.keys(node)) == [:dependencies, :implementation, :version] and
      is_binary(node.version) and match?({:ok, _}, Version.parse(node.version)) and
      is_list(node.dependencies) and node.dependencies == Enum.sort(Enum.uniq(node.dependencies)) and
      Enum.all?(node.dependencies, &Map.has_key?(inputs, &1))
  end

  defp visit(name, inputs, records, visiting) do
    cond do
      name in visiting ->
        {:error, :trusted_dependency_cycle}

      Map.has_key?(records, name) ->
        {:ok, records}

      true ->
        node = inputs[name]

        with {:ok, records} <-
               Enum.reduce_while(node.dependencies, {:ok, records}, fn child, {:ok, acc} ->
                 case visit(child, inputs, acc, [name | visiting]) do
                   {:ok, result} -> {:cont, {:ok, result}}
                   error -> {:halt, error}
                 end
               end),
             {:ok, local} <- boundary_records(node.implementation) do
          owner = %{
            "package" => name,
            "version" => node.version,
            "implementation" => Identity.digest(node.implementation)
          }

          local =
            Enum.map(local, fn record ->
              record
              |> Map.put("implementation_boundary", record["boundary"])
              |> Map.put(
                "boundary",
                Catena.CanonicalJCS.digest(%{
                  "implementation_boundary" => record["boundary"],
                  "owner" => owner
                })
              )
              |> Map.put("owners", [owner])
            end)

          inherited = Enum.flat_map(node.dependencies, &records[&1]["obligations"])
          obligations = merge(local ++ inherited)

          record = %{
            "owner" => owner,
            "dependencies" => Map.new(node.dependencies, &{&1, records[&1]["digest"]}),
            "obligations" => obligations
          }

          record = Map.put(record, "digest", Catena.CanonicalJCS.digest(record))
          {:ok, Map.put(records, name, record)}
        end
    end
  end

  defp merge(records) do
    records
    |> Enum.group_by(& &1["boundary"])
    |> Enum.map(fn {_, group} ->
      owners =
        group
        |> Enum.flat_map(& &1["owners"])
        |> Enum.uniq()
        |> Enum.sort_by(&Catena.CanonicalJCS.encode/1)

      Map.put(hd(group), "owners", owners)
    end)
    |> Enum.sort_by(& &1["boundary"])
  end

  def boundary_records(%{kind: :pure, core: core, artifact: artifact} = implementation) do
    with true <- Enum.sort(Map.keys(implementation)) == [:artifact, :core, :kind],
         true <- Enum.all?(core.definitions, &Catena.Calling.Adapter.initial_pure?/1),
         :ok <- Catena.Calling.Artifact.verify(artifact, core),
         do: {:ok, []},
         else: (_ -> {:error, :invalid_pure_trust_input})
  end

  def boundary_records(%{kind: :foreign, program: program} = implementation) do
    with true <- Enum.sort(Map.keys(implementation)) == [:kind, :program],
         :ok <- Program.verify(program) do
      records =
        program.description.bindings
        |> Map.values()
        |> Enum.flat_map(&Map.values/1)
        |> Enum.map(fn d ->
          %{
            "boundary" => Descriptor.id(d),
            "kind" => "trusted-beam",
            "obligations" =>
              ~w(cooperative-cancellation host-closure-disclosure host-code-correctness scheduler-correctness),
            "evidence" =>
              "typed-codecs-and-exact-loaded-module; host obligations remain assertions"
          }
        end)

      {:ok, records}
    else
      _ -> {:error, :invalid_foreign_trust_input}
    end
  end

  def boundary_records(%{kind: :native, ready: ready} = implementation) do
    with true <- Enum.sort(Map.keys(implementation)) == [:kind, :ready],
         :ok <- Package.verify_ready(ready) do
      kind = ready.package.description["kind"]

      {:ok,
       [
         %{
           "boundary" => Identity.digest(ready.package),
           "kind" => kind,
           "obligations" =>
             Enum.sort([
               "host-closure-disclosure" | ready.package.description["unsafe_obligations"]
             ]),
           "evidence" => "signed-bytes-platform-and-codecs; native obligations remain assertions"
         }
       ]}
    else
      _ -> {:error, :invalid_native_trust_input}
    end
  end

  def boundary_records(_), do: {:error, :unsupported_trusted_implementation}
end
