defmodule Catena.Package.Deps do
  @moduledoc """
  The source-only Catena 0.1.21 package dependency engine.

  Versions and requirements parse against the SemVer 2.0.0 grammar with
  exact/caret/tilde operators; resolution picks one version per package
  name per build — the highest satisfying every gathered requirement,
  order-independently; `catena.lock` bytes generate and replay
  deterministically; bundle digests identify package content
  registry-neutrally. The engine fetches nothing, caches nothing, builds
  nothing, and runs no registry protocol.
  """

  alias Catena.{CanonicalJCS, Diagnostic}

  @package_name ~r/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/
  @numeric ~r/^(0|[1-9][0-9]*)$/
  @identifier ~r/^[0-9A-Za-z-]+$/

  defmodule Version do
    @moduledoc "One parsed SemVer 2.0.0 version."

    @enforce_keys [:major, :minor, :patch, :pre, :build, :source]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            major: non_neg_integer(),
            minor: non_neg_integer(),
            patch: non_neg_integer(),
            pre: [String.t() | non_neg_integer()],
            build: [String.t()] | nil,
            source: String.t()
          }
  end

  defmodule Requirement do
    @moduledoc "One parsed exact, caret, or tilde requirement."

    @enforce_keys [:op, :version, :source]
    defstruct @enforce_keys

    @type op :: :exact | :caret | :tilde
    @type t :: %__MODULE__{op: op(), version: Version.t(), source: String.t()}
  end

  @spec parse_version(String.t()) :: {:ok, Version.t()} | {:error, Diagnostic.t()}
  def parse_version(source) when is_binary(source) do
    with {:ok, core, rest} <- parse_core(source),
         {:ok, pre, rest2} <- parse_pre(core, rest),
         {:ok, build, ""} <- parse_build(rest2, source) do
      {:ok,
       %Version{
         major: elem(core, 0),
         minor: elem(core, 1),
         patch: elem(core, 2),
         pre: pre,
         build: build,
         source: source
       }}
    else
      _ -> invalid_version(source)
    end
  end

  @spec parse_requirement(String.t()) :: {:ok, Requirement.t()} | {:error, Diagnostic.t()}
  def parse_requirement(">" <> _rest = source), do: invalid_requirement(source)
  def parse_requirement("<" <> _rest = source), do: invalid_requirement(source)
  def parse_requirement("=" <> _rest = source), do: invalid_requirement(source)

  def parse_requirement("^" <> operand = source), do: parse_operator(:caret, operand, source)

  def parse_requirement("~" <> operand = source) do
    if String.starts_with?(operand, ">") do
      invalid_requirement(source)
    else
      parse_operator(:tilde, operand, source)
    end
  end

  def parse_requirement(source) when is_binary(source),
    do: parse_operator(:exact, source, source)

  @spec satisfies?(Version.t() | String.t(), Requirement.t() | String.t()) :: boolean()
  def satisfies?(%Version{} = version, %Requirement{} = requirement) do
    case requirement.op do
      :exact ->
        compare(version, requirement.version) == :eq

      :caret ->
        upper = caret_upper(requirement.version)
        compare(version, requirement.version) != :lt and compare(version, upper) == :lt

      :tilde ->
        upper = tilde_upper(requirement.version)
        compare(version, requirement.version) != :lt and compare(version, upper) == :lt
    end and pre_release_matches?(version, requirement)
  end

  def satisfies?(version, requirement) when is_binary(version) and is_binary(requirement) do
    with {:ok, v} <- parse_version(version),
         {:ok, r} <- parse_requirement(requirement) do
      satisfies?(v, r)
    else
      _ -> false
    end
  end

  @spec compare(Version.t() | String.t(), Version.t() | String.t()) :: :lt | :eq | :gt
  def compare(%Version{} = a, %Version{} = b) do
    triple_a = {a.major, a.minor, a.patch}
    triple_b = {b.major, b.minor, b.patch}

    cond do
      triple_a < triple_b -> :lt
      triple_a > triple_b -> :gt
      a.pre == [] and b.pre != [] -> :gt
      a.pre != [] and b.pre == [] -> :lt
      pre_compare(a.pre, b.pre) == :lt -> :lt
      pre_compare(a.pre, b.pre) == :gt -> :gt
      true -> :eq
    end
  end

  def compare(a, b) when is_binary(a) or is_binary(b) do
    with {:ok, va} <- parse_version(a),
         {:ok, vb} <- parse_version(b) do
      compare(va, vb)
    else
      _ -> raise ArgumentError, "invalid version in compare"
    end
  end

  @doc """
  Resolves a root manifest's dependency graph against a package
  environment.

  The root is a map with optional `dependencies` (name → requirement
  string). The environment is a map name → %{version_string => meta}
  where meta carries `interface_digests`, `component_digests`, and
  optional `dependencies`. Returns the resolved set as a sorted list of
  entries, or one diagnostic.
  """
  @spec resolve(map(), map()) :: {:ok, [map()]} | {:error, Diagnostic.t()}
  def resolve(root, environment) do
    root_deps_raw = Map.get(root, :dependencies) || Map.get(root, "dependencies") || %{}
    prelude_raw = Map.get(root, :prelude) || Map.get(root, "prelude")

    with {:ok, root_deps} <- normalize_dependencies(root_deps_raw),
         {:ok, merged_deps} <- merge_prelude(root_deps, prelude_raw),
         {:ok, gathered} <- gather(merged_deps, environment) do
      resolve_gathered(gathered, environment)
    end
  end

  defp merge_prelude(deps, nil), do: {:ok, deps}

  defp merge_prelude(deps, %{"package" => package, "requirement" => requirement}) do
    case Map.get(deps, package) do
      nil ->
        with {:ok, parsed} <- parse_requirement(requirement) do
          {:ok, Map.put(deps, package, parsed)}
        end

      existing ->
        {:ok, Map.put(deps, package, existing)}
    end
  end

  @doc """
  Generates deterministic canonical `catena.lock` bytes for a resolution.
  """
  @spec generate_lockfile([map()], keyword()) :: binary()
  def generate_lockfile(resolution, options \\ []) do
    root = Keyword.get(options, :root, %{})

    payload = %{
      "format" => "catena.lock",
      "version" => 1,
      "packages" =>
        Enum.map(Enum.sort_by(resolution, & &1.name), fn entry ->
          %{
            "name" => entry.name,
            "version" => entry.version,
            "requirement" => entry.requirement,
            "requirers" => Enum.sort(entry.requirers),
            "bundle_digest" => entry.bundle_digest,
            "interface_digests" => Enum.sort(entry.interface_digests || []),
            "component_digests" => Enum.sort(entry.component_digests || []),
            "selection" => %{
              "edition" => Map.get(entry, :edition) || Map.get(root, :edition, "0.1"),
              "language_revision" =>
                Map.get(entry, :language_revision) || Map.get(root, :language_revision),
              "previews" => Enum.sort(Map.get(entry, :previews) || Map.get(root, :previews, []))
            }
          }
        end)
    }

    CanonicalJCS.encode(payload) <> "\n"
  end

  @doc """
  Replays a lockfile as an exact-pin resolution.

  Accepts the manifest, lockfile bytes, and a digest-check function
  `(name, version, bundle_digest) -> boolean()`; pass a function that
  always returns true to skip content verification. Returns the pinned
  resolution or `PKG005`.
  """
  @spec replay_lockfile(map(), binary(), (String.t(), String.t(), String.t() -> boolean())) ::
          {:ok, [map()]} | {:error, Diagnostic.t()}
  def replay_lockfile(manifest, lockfile_bytes, digest_ok \\ fn _, _, _ -> true end)
      when is_binary(lockfile_bytes) do
    with {:ok, lock} <- decode_lock(lockfile_bytes),
         {:ok, manifest_deps} <-
           normalize_dependencies(
             Map.get(manifest, :dependencies) || Map.get(manifest, "dependencies") || %{}
           ) do
      pinned =
        Enum.map(lock["packages"], fn pkg ->
          %{
            name: pkg["name"],
            version: pkg["version"],
            requirement: pkg["requirement"],
            requirers: pkg["requirers"],
            bundle_digest: pkg["bundle_digest"],
            interface_digests: pkg["interface_digests"],
            component_digests: pkg["component_digests"],
            edition: pkg["selection"]["edition"],
            language_revision: pkg["selection"]["language_revision"],
            previews: pkg["selection"]["previews"]
          }
        end)

      stale? =
        Enum.any?(pinned, fn entry ->
          req = Map.get(manifest_deps, entry.name)

          req != nil and req.source != entry.requirement and
            not (satisfies?(entry.version, req.source) and entry.requirement == req.source)
        end)

      missing =
        Enum.flat_map(manifest_deps, fn {name, req} ->
          if Enum.any?(pinned, &(&1.name == name and satisfies?(&1.version, req.source))) do
            []
          else
            [name]
          end
        end)

      tampered =
        Enum.reject(pinned, fn entry ->
          digest_ok.(entry.name, entry.version, entry.bundle_digest)
        end)

      cond do
        stale? ->
          {:error,
           lock_error("stale_lockfile", "a lockfile requirement no longer matches the manifest")}

        missing != [] ->
          {:error,
           lock_error(
             "stale_lockfile",
             "the manifest declares dependencies absent from the lockfile",
             %{missing: Enum.sort(missing)}
           )}

        tampered != [] ->
          {:error,
           lock_error(
             "tampered_lockfile",
             "a recorded bundle digest does not match present content",
             %{packages: Enum.map(tampered, & &1.name)}
           )}

        true ->
          {:ok, Enum.sort_by(pinned, & &1.name)}
      end
    end
  end

  @doc """
  Computes the registry-neutral SHA-256 bundle digest of a package's
  semantic content.

  Accepts a map with `name`, `version`, `dependencies`, `selection`,
  `modules`, `interfaces`, `roots`, `output`, plus `interface_digests`
  and `component_digests` lists.
  """
  @spec bundle_digest(map()) :: String.t()
  def bundle_digest(package) do
    semantic = %{
      "name" => package[:name] || package["name"],
      "version" => package[:version] || package["version"],
      "dependencies" =>
        package[:dependencies] || package["dependencies"] ||
          %{}
          |> Enum.map(fn {k, v} -> {k, v.source || v} end)
          |> Enum.sort()
          |> Map.new(),
      "selection" => package[:selection] || package["selection"] || %{},
      "modules" => Enum.sort(package[:modules] || package["modules"] || []),
      "interfaces" => Enum.sort(package[:interfaces] || package["interfaces"] || []),
      "roots" => Enum.sort(package[:roots] || package["roots"] || []),
      "output" => package[:output] || package["output"],
      "interface_digests" =>
        Enum.sort(package[:interface_digests] || package["interface_digests"] || []),
      "component_digests" =>
        Enum.sort(package[:component_digests] || package["component_digests"] || [])
    }

    CanonicalJCS.digest(semantic)
  end

  defp normalize_dependencies(deps) when is_map(deps) do
    Enum.reduce_while(deps, {:ok, %{}}, fn {name, requirement}, {:ok, acc} ->
      name = to_string(name)

      with true <- Regex.match?(@package_name, name) || :bad_name,
           req when is_binary(req) <- requirement,
           {:ok, parsed} <- parse_requirement(req) do
        {:cont, {:ok, Map.put(acc, name, parsed)}}
      else
        :bad_name ->
          {:halt, {:error, pkg_error("PKG001", "invalid package name", %{name: name})}}

        _ ->
          {:halt,
           {:error,
            pkg_error("PKG001", "malformed dependency requirement", %{
              name: name,
              requirement: inspect(requirement)
            })}}
      end
    end)
  end

  defp normalize_dependencies(_),
    do: {:error, pkg_error("PKG001", "dependencies must be an object", %{})}

  defp gather(root_deps, environment) do
    initial = Enum.map(root_deps, fn {name, req} -> {"<root>", name, req} end)
    do_gather(initial, environment, %{}, MapSet.new(), MapSet.new())
  end

  defp do_gather([], _environment, gathered, _edges, _expanded), do: {:ok, gathered}

  defp do_gather([{requirer, name, req} | rest], environment, gathered, edges, expanded) do
    case environment do
      %{^name => _versions} ->
        if reaches_back?(name, requirer, edges) do
          {:error,
           pkg_error("PKG002", "the package dependency graph contains a cycle", %{
             cycle: cycle_path(name, requirer, edges)
           })}
        else
          entry = Map.get(gathered, name, %{requirements: [], requirers: []})

          gathered =
            Map.put(gathered, name, %{
              requirements: [req | entry.requirements],
              requirers: [requirer | entry.requirers] |> Enum.uniq()
            })

          edges = MapSet.put(edges, {requirer, name})

          if MapSet.member?(expanded, name) do
            do_gather(rest, environment, gathered, edges, expanded)
          else
            with {:ok, nested} <- nested_deps(environment, name) do
              queued = Enum.map(nested, fn {dep_name, dep_req} -> {name, dep_name, dep_req} end)
              do_gather(queued ++ rest, environment, gathered, edges, MapSet.put(expanded, name))
            end
          end
        end

      _ ->
        {:error,
         pkg_error("PKG004", "a declared dependency name is absent from the environment", %{
           name: name,
           requirer: requirer
         })}
    end
  end

  defp reaches_back?(name, requirer, edges) do
    do_walk([name], requirer, edges, MapSet.new([name]))
  end

  defp do_walk([], _requirer, _edges, _seen), do: false

  defp do_walk([node | nodes], requirer, edges, seen) do
    if node == requirer do
      true
    else
      next =
        edges
        |> Enum.filter(fn {from, _to} -> from == node end)
        |> Enum.map(fn {_from, to} -> to end)
        |> Enum.reject(&MapSet.member?(seen, &1))

      do_walk(nodes ++ next, requirer, edges, MapSet.union(seen, MapSet.new(next)))
    end
  end

  defp cycle_path(origin, sink, edges) do
    path = find_path(origin, sink, edges, [origin], MapSet.new([origin]))

    (path |> Enum.reverse() |> Enum.join(" -> ")) <>
      " -> " <> origin <> " (cycle)"
  end

  defp find_path(node, sink, edges, acc, visited) do
    nexts =
      edges
      |> Enum.filter(fn {from, _to} -> from == node end)
      |> Enum.map(fn {_from, to} -> to end)
      |> Enum.reject(&MapSet.member?(visited, &1))

    Enum.find_value(nexts, fn next ->
      cond do
        next == sink -> [next | acc]
        true -> find_path(next, sink, edges, [next | acc], MapSet.put(visited, next))
      end
    end) || [sink | acc]
  end

  defp nested_deps(environment, name) do
    deps =
      environment
      |> Map.fetch!(name)
      |> Enum.map(fn {_v, meta} ->
        Map.get(meta, :dependencies) || Map.get(meta, "dependencies")
      end)
      |> Enum.reject(&is_nil/1)

    with {:ok, normalized} <-
           Enum.reduce_while(deps, {:ok, %{}}, fn dep_map, {:ok, acc} ->
             case normalize_dependencies(dep_map) do
               {:ok, parsed} -> {:cont, {:ok, Map.merge(acc, parsed)}}
               {:error, _} = error -> {:halt, error}
             end
           end) do
      {:ok, Enum.map(normalized, fn {n, r} -> {n, r} end)}
    end
  end

  defp resolve_gathered(gathered, environment) do
    gathered
    |> Enum.sort_by(fn {name, _} -> name end)
    |> Enum.reduce_while({:ok, []}, fn {name, entry}, {:ok, acc} ->
      versions = Map.fetch!(environment, name)

      case dedupe_build_free(name, versions) do
        {:error, %Diagnostic{}} = error ->
          {:halt, error}

        {:ok, candidates} ->
          chosen =
            candidates
            |> Enum.filter(fn v -> Enum.all?(entry.requirements, &satisfies?(v, &1)) end)
            |> Enum.sort(&(&1.source < &2.source))
            |> Enum.reverse()
            |> List.first()

          case chosen do
            nil ->
              {:halt,
               {:error,
                pkg_error(
                  "PKG003",
                  "no available version satisfies every gathered requirement",
                  %{
                    name: name,
                    requirers:
                      Enum.map(Enum.zip(entry.requirers, entry.requirements), fn {requirer, req} ->
                        %{requirer: requirer, requirement: req.source}
                      end)
                      |> Enum.sort_by(& &1.requirer)
                  }
                )}}

            version ->
              meta = Map.fetch!(versions, version.source)

              {:cont,
               {:ok,
                [
                  %{
                    name: name,
                    version: version.source,
                    requirement: hd(entry.requirements).source,
                    requirers: Enum.uniq(entry.requirers),
                    bundle_digest:
                      Map.get(meta, :bundle_digest) || Map.get(meta, "bundle_digest") ||
                        bundle_digest_from_meta(meta),
                    interface_digests:
                      Map.get(meta, :interface_digests) || Map.get(meta, "interface_digests") ||
                        [],
                    component_digests:
                      Map.get(meta, :component_digests) || Map.get(meta, "component_digests") ||
                        [],
                    selection: Map.get(meta, :selection) || Map.get(meta, "selection") || %{}
                  }
                  | acc
                ]}}
          end
      end
    end)
    |> case do
      {:ok, resolved} -> {:ok, Enum.reverse(resolved)}
      error -> error
    end
  end

  defp bundle_digest_from_meta(meta) do
    bundle_digest(%{
      name: Map.get(meta, :name) || Map.get(meta, "name"),
      version: Map.get(meta, :version) || Map.get(meta, "version"),
      dependencies: Map.get(meta, :dependencies) || Map.get(meta, "dependencies") || %{},
      selection: Map.get(meta, :selection) || Map.get(meta, "selection") || %{},
      modules: [],
      interfaces: [],
      roots: [],
      output: nil,
      interface_digests: Map.get(meta, :interface_digests) || [],
      component_digests: Map.get(meta, :component_digests) || []
    })
  end

  defp dedupe_build_free(name, versions) do
    parsed =
      Enum.flat_map(versions, fn {source, meta} ->
        case parse_version(source) do
          {:ok, v} -> [{v, source, meta}]
          {:error, _} -> []
        end
      end)

    groups =
      Enum.group_by(parsed, fn {v, _s, _m} -> {v.major, v.minor, v.patch, v.pre} end)

    duplicate =
      Enum.find(groups, fn {_key, list} -> length(list) > 1 end)

    case duplicate do
      nil ->
        {:ok, Enum.map(parsed, fn {v, _s, _m} -> v end)}

      {key, list} ->
        {:error,
         pkg_error("PKG005", "the environment duplicates a version up to build metadata", %{
           name: name,
           versions: Enum.map(list, fn {_v, s, _m} -> s end) |> Enum.sort(),
           identity: inspect(key)
         })}
    end
  end

  defp parse_core(full) do
    source = parse_core_split(full)

    case String.split(source, ".", parts: 3) do
      [major, minor, rest] ->
        with true <- Regex.match?(@numeric, major),
             true <- Regex.match?(@numeric, minor),
             {patch, tail} <- split_patch(rest),
             true <- Regex.match?(@numeric, patch) do
          {:ok, {String.to_integer(major), String.to_integer(minor), String.to_integer(patch)},
           tail}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp split_patch(rest) do
    case String.split(rest, "-", parts: 2) do
      [patch] -> {patch, ""}
      [patch, tail] -> {patch, "-" <> tail}
    end
  end

  defp parse_core_split(source) do
    case String.split(source, "+", parts: 2) do
      [core, _build] -> core
      [core] -> core
    end
  end

  defp parse_pre(_core, "-" <> rest) do
    identifiers = String.split(rest, ".")

    if Enum.all?(identifiers, &Regex.match?(@identifier, &1)) and identifiers != [] do
      {:ok, Enum.map(identifiers, &identifier_value/1), ""}
    else
      :error
    end
  end

  defp parse_pre(_core, rest), do: {:ok, [], rest}

  defp parse_build(rest, full) do
    case String.split(full, "+", parts: 2) do
      [_core, build] ->
        identifiers = String.split(build, ".")

        if Enum.all?(identifiers, &Regex.match?(@identifier, &1)) and identifiers != [] and
             rest == "" do
          {:ok, identifiers, ""}
        else
          :error
        end

      [_core] when rest == "" ->
        {:ok, nil, ""}

      _ ->
        :error
    end
  end

  defp identifier_value(identifier) do
    if Regex.match?(@numeric, identifier), do: String.to_integer(identifier), else: identifier
  end

  defp parse_operator(op, operand, source) do
    with {:ok, version} <- parse_version(operand),
         nil <- version.build do
      {:ok, %Requirement{op: op, version: version, source: source}}
    else
      _ -> invalid_requirement(source)
    end
  end

  defp pre_compare([], []), do: :eq
  defp pre_compare([], _), do: :lt
  defp pre_compare(_, []), do: :gt

  defp pre_compare([a | ra], [b | rb]) do
    case identifier_compare(a, b) do
      :eq -> pre_compare(ra, rb)
      result -> result
    end
  end

  defp identifier_compare(a, b) when is_integer(a) and is_integer(b) do
    cond do
      a < b -> :lt
      a > b -> :gt
      true -> :eq
    end
  end

  defp identifier_compare(a, b) when is_integer(a) and is_binary(b), do: :lt
  defp identifier_compare(a, b) when is_binary(a) and is_integer(b), do: :gt

  defp identifier_compare(a, b) when is_binary(a) and is_binary(b) do
    cond do
      a < b -> :lt
      a > b -> :gt
      true -> :eq
    end
  end

  defp pre_release_matches?(version, requirement) do
    case version.pre do
      [] ->
        true

      _ ->
        operand = requirement.version
        operand.pre != [] and same_triple?(version, operand)
    end
  end

  defp same_triple?(a, b),
    do: {a.major, a.minor, a.patch} == {b.major, b.minor, b.patch}

  defp caret_upper(%{major: 0, minor: 0, patch: patch}) do
    %Version{major: 0, minor: 0, patch: patch + 1, pre: [], build: nil, source: ""}
  end

  defp caret_upper(%{major: 0, minor: minor}) do
    %Version{major: 0, minor: minor + 1, patch: 0, pre: [], build: nil, source: ""}
  end

  defp caret_upper(%{major: major}) do
    %Version{major: major + 1, minor: 0, patch: 0, pre: [], build: nil, source: ""}
  end

  defp tilde_upper(%{major: major, minor: minor}) do
    %Version{major: major, minor: minor + 1, patch: 0, pre: [], build: nil, source: ""}
  end

  defp decode_lock(bytes) do
    with {:ok, value} <- JSON.decode(bytes),
         true <- is_map(value),
         "catena.lock" <- Map.get(value, "format"),
         1 <- Map.get(value, "version"),
         packages when is_list(packages) <- Map.get(value, "packages") do
      {:ok, value}
    else
      _ -> {:error, pkg_error("PKG001", "malformed catena.lock", %{})}
    end
  rescue
    _ -> {:error, pkg_error("PKG001", "malformed catena.lock", %{})}
  end

  defp invalid_version(source),
    do: {:error, pkg_error("PKG001", "invalid SemVer version", %{version: source})}

  defp invalid_requirement(source),
    do: {:error, pkg_error("PKG001", "invalid requirement", %{requirement: source})}

  defp lock_error(reason, message, extra \\ %{}) do
    Diagnostic.new("PKG005", message, details: Map.merge(%{reason: reason}, extra))
  end

  defp pkg_error(id, message, details) do
    Diagnostic.new(id, message, details: details)
  end
end
