defmodule Catena.Package.Build do
  @moduledoc "Deterministic workspace planning, acquisition, caching, and offline builds."

  alias Catena.CanonicalJCS

  @version "0.1.81"
  @digest ~r/^[0-9a-f]{64}$/
  @name ~r/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/
  @profiles ~w(development test release)

  def profile do
    %{
      version: @version,
      profiles: @profiles,
      packages: 256,
      dependencies_per_package: 256,
      generators_per_package: 64,
      acquisition_bytes: 67_108_864,
      offline_builds: true,
      transactional_acquisition: true,
      transactional_outputs: true,
      network_during_build: false
    }
  end

  def discover(candidates, explicit \\ nil)

  def discover(candidates, explicit) when is_list(candidates) do
    paths = candidates |> Enum.filter(&path?/1) |> Enum.uniq()

    cond do
      explicit != nil and path?(explicit) and explicit in paths ->
        {:ok, explicit}

      explicit != nil ->
        {:error, :project_not_found}

      "catena.project.json" in paths ->
        {:ok, "catena.project.json"}

      length(Enum.filter(paths, &(Path.basename(&1) == "catena.project.json"))) == 1 ->
        {:ok, Enum.find(paths, &(Path.basename(&1) == "catena.project.json"))}

      true ->
        {:error, :ambiguous_project_discovery}
    end
  end

  def discover(_, _), do: {:error, :ambiguous_project_discovery}

  def plan(packages, selected_profile, toolchain_digest)
      when is_list(packages) and selected_profile in @profiles do
    with true <- digest?(toolchain_digest),
         true <- length(packages) in 1..profile().packages,
         {:ok, indexed} <- index_packages(packages),
         {:ok, order} <- topological_order(indexed) do
      entries =
        Enum.reduce(order, %{}, fn name, entries ->
          package = indexed[name]

          dependencies =
            Enum.map(package["dependencies"], fn
              %{"workspace" => dependency} ->
                %{"workspace" => dependency, "cache_key" => entries[dependency]["cache_key"]}

              external ->
                external
            end)

          key_input = %{
            "format" => "catena-build-cache-key",
            "version" => @version,
            "package" => name,
            "profile" => selected_profile,
            "toolchain" => toolchain_digest,
            "input" => package["input_digest"],
            "capabilities" => package["capability_digest"],
            "dependencies" => dependencies,
            "generators" => package["generators"]
          }

          Map.put(entries, name, Map.put(package, "cache_key", CanonicalJCS.digest(key_input)))
        end)

      payload = %{
        "format" => "catena-build-plan",
        "version" => @version,
        "profile" => selected_profile,
        "toolchain" => toolchain_digest,
        "order" => order,
        "packages" => entries
      }

      {:ok, Map.put(payload, "digest", CanonicalJCS.digest(payload))}
    else
      _ -> {:error, :invalid_build_workspace}
    end
  rescue
    _ -> {:error, :invalid_build_workspace}
  end

  def plan(_, _, _), do: {:error, :invalid_build_workspace}

  def acquire(plan, cache, fetch) when is_map(cache) and is_function(fetch, 1) do
    with :ok <- verify_plan(plan),
         :ok <- verify_cache(cache),
         requirements <- external_requirements(plan),
         {:ok, additions} <- acquire_all(requirements, cache, fetch) do
      {:ok, Map.merge(cache, additions)}
    else
      _ -> {:error, :build_acquisition_failed, cache}
    end
  rescue
    _ -> {:error, :build_acquisition_failed, cache}
  end

  def acquire(_, cache, _), do: {:error, :build_acquisition_failed, cache}

  def build_offline(plan, cache, build) when is_map(cache) and is_function(build, 2) do
    with :ok <- verify_plan(plan),
         :ok <- verify_cache(cache),
         true <- Enum.all?(external_requirements(plan), &Map.has_key?(cache, &1["digest"])),
         {:ok, outputs, next_cache} <- execute(plan, cache, build) do
      {:ok, %{plan: plan["digest"], outputs: outputs, cache: next_cache}}
    else
      _ -> {:error, :offline_build_failed}
    end
  rescue
    _ -> {:error, :offline_build_failed}
  end

  def build_offline(_, _, _), do: {:error, :offline_build_failed}

  def build_retained(plan, cache, root) when is_binary(root) do
    build_offline(plan, cache, fn package, _dependencies ->
      case Catena.Package.Reproducible.build(
             package["reproducible_plan"],
             Path.join(root, package["name"])
           ) do
        {:ok, result} -> {:ok, result.archive}
        error -> error
      end
    end)
  end

  def build_retained(_, _, _), do: {:error, :offline_build_failed}

  def publish(archive, destination) do
    with {:ok, staged} <- Catena.Package.Reproducible.stage(archive, destination),
         :ok <- Catena.Package.Reproducible.commit(staged),
         do: :ok,
         else: (_ -> {:error, :build_output_publish_failed})
  end

  defp index_packages(packages) do
    if Enum.all?(packages, &package?/1) do
      indexed = Map.new(packages, &{&1["name"], &1})

      if map_size(indexed) == length(packages) and
           Enum.all?(packages, fn package ->
             Enum.all?(package["dependencies"], fn
               %{"workspace" => name} -> Map.has_key?(indexed, name) and name != package["name"]
               %{"package" => _, "version" => _, "digest" => _} -> true
             end)
           end) do
        {:ok, indexed}
      else
        {:error, :invalid_package_graph}
      end
    else
      {:error, :invalid_package_graph}
    end
  end

  defp package?(package) do
    is_map(package) and
      Enum.sort(Map.keys(package)) ==
        ~w(capability_digest dependencies generators input_digest name reproducible_plan) and
      Regex.match?(@name, package["name"] || "") and digest?(package["input_digest"]) and
      digest?(package["capability_digest"]) and is_map(package["reproducible_plan"]) and
      valid_dependencies?(package["dependencies"]) and valid_generators?(package["generators"])
  end

  defp valid_dependencies?(dependencies) do
    is_list(dependencies) and length(dependencies) <= profile().dependencies_per_package and
      dependencies == Enum.uniq(dependencies) and
      Enum.all?(dependencies, fn
        dependency when is_map(dependency) and map_size(dependency) == 1 ->
          match?(%{"workspace" => name} when is_binary(name), dependency)

        %{"package" => name, "version" => version, "digest" => digest} = dependency ->
          map_size(dependency) == 3 and Regex.match?(@name, name) and is_binary(version) and
            version != "" and digest?(digest)

        _ ->
          false
      end)
  end

  defp valid_generators?(generators) do
    is_list(generators) and length(generators) <= profile().generators_per_package and
      Enum.all?(generators, fn generator ->
        is_map(generator) and Enum.sort(Map.keys(generator)) == ~w(inputs output) and
          is_list(generator["inputs"]) and generator["inputs"] != [] and
          Enum.all?(generator["inputs"], &path?/1) and path?(generator["output"]) and
          generator["output"] not in generator["inputs"]
      end)
  end

  defp topological_order(indexed), do: visit_all(Enum.sort(Map.keys(indexed)), indexed, %{}, [])
  defp visit_all([], _, _, order), do: {:ok, Enum.reverse(order)}

  defp visit_all([name | rest], indexed, marks, order) do
    with {:ok, marks, order} <- visit(name, indexed, marks, order),
         do: visit_all(rest, indexed, marks, order)
  end

  defp visit(name, indexed, marks, order) do
    case marks[name] do
      :done ->
        {:ok, marks, order}

      :active ->
        {:error, :dependency_cycle}

      nil ->
        dependencies =
          indexed[name]["dependencies"]
          |> Enum.flat_map(fn
            %{"workspace" => dependency} -> [dependency]
            _ -> []
          end)
          |> Enum.sort()

        with {:ok, marks, order} <-
               visit_dependencies(dependencies, indexed, Map.put(marks, name, :active), order) do
          {:ok, Map.put(marks, name, :done), [name | order]}
        end
    end
  end

  defp visit_dependencies([], _, marks, order), do: {:ok, marks, order}

  defp visit_dependencies([name | rest], indexed, marks, order) do
    with {:ok, marks, order} <- visit(name, indexed, marks, order),
         do: visit_dependencies(rest, indexed, marks, order)
  end

  defp verify_plan(%{"digest" => digest} = plan) do
    unsigned = Map.delete(plan, "digest")

    with true <- digest?(digest),
         ^digest <- CanonicalJCS.digest(unsigned),
         %{
           "format" => "catena-build-plan",
           "version" => @version,
           "packages" => packages,
           "order" => order,
           "profile" => profile_name,
           "toolchain" => toolchain
         } <- unsigned,
         true <- profile_name in @profiles and digest?(toolchain),
         true <-
           Enum.all?(packages, fn {_name, package} -> Map.has_key?(package, "cache_key") end),
         base_packages <-
           Map.new(packages, fn {name, package} -> {name, Map.delete(package, "cache_key")} end),
         {:ok, rebuilt} <- plan(Map.values(base_packages), profile_name, toolchain),
         true <- rebuilt == plan and rebuilt["order"] == order,
         do: :ok,
         else: (_ -> {:error, :invalid_build_plan})
  end

  defp verify_plan(_), do: {:error, :invalid_build_plan}

  defp verify_cache(cache) do
    if Enum.all?(cache, fn {digest, bytes} ->
         digest?(digest) and is_binary(bytes) and
           (hash(bytes) == digest or
              match?({:ok, _}, Catena.Package.Reproducible.decode_archive(bytes)))
       end),
       do: :ok,
       else: {:error, :corrupt_build_cache}
  end

  defp external_requirements(plan) do
    plan["packages"]
    |> Map.values()
    |> Enum.flat_map(& &1["dependencies"])
    |> Enum.filter(&Map.has_key?(&1, "package"))
    |> Enum.uniq_by(& &1["digest"])
    |> Enum.sort_by(&{&1["package"], &1["version"], &1["digest"]})
  end

  defp acquire_all(requirements, cache, fetch) do
    Enum.reduce_while(requirements, {:ok, %{}}, fn requirement, {:ok, additions} ->
      digest = requirement["digest"]

      if Map.has_key?(cache, digest) do
        {:cont, {:ok, additions}}
      else
        case fetch.(requirement) do
          {:ok, bytes} when is_binary(bytes) ->
            if byte_size(bytes) <= profile().acquisition_bytes and hash(bytes) == digest,
              do: {:cont, {:ok, Map.put(additions, digest, bytes)}},
              else: {:halt, {:error, :acquired_content_mismatch}}

          _ ->
            {:halt, {:error, :acquisition_interrupted}}
        end
      end
    end)
  end

  defp execute(plan, cache, build) do
    Enum.reduce_while(plan["order"], {:ok, %{}, cache}, fn name, {:ok, outputs, cache} ->
      package = plan["packages"][name]
      key = package["cache_key"]

      case cache[key] do
        archive when is_binary(archive) ->
          if valid_output?(archive, package["input_digest"]),
            do: {:cont, {:ok, Map.put(outputs, name, archive), cache}},
            else: {:halt, {:error, :corrupt_build_output}}

        nil ->
          inputs = dependency_inputs(package, outputs, cache)

          case build.(package, inputs) do
            {:ok, archive} when is_binary(archive) ->
              if valid_output?(archive, package["input_digest"]),
                do: {:cont, {:ok, Map.put(outputs, name, archive), Map.put(cache, key, archive)}},
                else: {:halt, {:error, :invalid_build_output}}

            _ ->
              {:halt, {:error, :package_build_failed}}
          end
      end
    end)
  end

  defp dependency_inputs(package, outputs, cache) do
    Map.new(package["dependencies"], fn
      %{"workspace" => name} ->
        {{:workspace, name}, Map.fetch!(outputs, name)}

      %{"package" => name, "version" => version, "digest" => digest} ->
        {{:package, name, version}, Map.fetch!(cache, digest)}
    end)
  end

  defp valid_output?(archive, input_digest) do
    with {:ok, document} <- CanonicalJCS.decode(archive, canonical: true),
         ^input_digest <- document["input"],
         {:ok, _} <- Catena.Package.Reproducible.decode_archive(archive),
         do: true,
         else: (_ -> false)
  end

  defp path?(path),
    do:
      is_binary(path) and path != "" and byte_size(path) <= 240 and Path.type(path) == :relative and
        Enum.all?(Path.split(path), &(&1 not in [".", "..", ""]))

  defp digest?(value), do: is_binary(value) and Regex.match?(@digest, value)
  defp hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
