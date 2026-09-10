defmodule Catena.Package.Registry do
  @moduledoc "Signed registry metadata, immutable acquisition, and offline lock replay."

  alias Catena.{CanonicalJCS, Governance.Crypto, OTP.Profile, Package.Deps}

  @version "0.1.74"
  @digest ~r/^[0-9a-f]{64}$/
  @name ~r/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/
  @root_keys ~w(format version sequence expires keys roles delegations)
  @role_keys ~w(principals threshold)
  @release_keys ~w(artifact publisher_signatures status status_sequence)
  @artifact_keys ~w(bundle_digest content_digest kind package provenance size version)
  @max_integer 9_007_199_254_740_991

  def profile do
    %{
      version: @version,
      keys: 32,
      delegations: 256,
      releases: 4096,
      signatures: 32,
      mirrors: 8,
      metadata_bytes: 16_777_216,
      artifact_bytes: 67_108_864,
      offline_replay: true,
      yanked_locked_replay: true,
      compromised_replay: false
    }
  end

  def signing_payload(kind, payload) when kind in ~w(root snapshot release),
    do: "catena:registry:#{kind}:1\n" <> CanonicalJCS.encode(payload)

  def decode_root(binary) when is_binary(binary) and byte_size(binary) <= 1_048_576 do
    with {:ok, root} <- CanonicalJCS.decode(binary, canonical: true),
         true <- valid_root?(root) do
      {:ok, Map.put(root, "digest", CanonicalJCS.digest(root))}
    else
      _ -> {:error, :invalid_registry_root}
    end
  rescue
    _ -> {:error, :invalid_registry_root}
  end

  def decode_root(_), do: {:error, :invalid_registry_root}

  def rotate(current, binary, now)
      when is_map(current) and is_binary(binary) and is_integer(now) and now >= 0 do
    with true <- valid_loaded_root?(current),
         {:ok, envelope} <- CanonicalJCS.decode(binary, canonical: true),
         true <-
           exact?(
             envelope,
             ~w(format version mode prior_digest signed old_signatures new_signatures)
           ),
         "catena-registry-root-update" <- envelope["format"],
         "1" <- envelope["version"],
         mode when mode in ~w(normal recovery) <- envelope["mode"],
         true <- envelope["prior_digest"] == current["digest"],
         next when is_map(next) <- envelope["signed"],
         true <- valid_root?(next),
         true <- next["sequence"] == current["sequence"] + 1,
         true <- next["expires"] >= now,
         :ok <- verify_rotation(mode, current, next, envelope) do
      {:ok, Map.put(next, "digest", CanonicalJCS.digest(next))}
    else
      _ -> {:error, :registry_root_rotation_denied}
    end
  rescue
    _ -> {:error, :registry_root_rotation_denied}
  end

  def rotate(_, _, _), do: {:error, :registry_root_rotation_denied}

  def open(root, snapshot_binary, now, prior \\ nil)

  def open(root, snapshot_binary, now, prior)
      when is_map(root) and is_binary(snapshot_binary) and is_integer(now) and now >= 0 do
    with true <- valid_loaded_root?(root),
         true <- root["expires"] >= now,
         true <- byte_size(snapshot_binary) <= profile().metadata_bytes,
         {:ok, envelope} <- CanonicalJCS.decode(snapshot_binary, canonical: true),
         true <- exact?(envelope, ~w(format version signed signatures)),
         "catena-registry-snapshot-envelope" <- envelope["format"],
         "1" <- envelope["version"],
         snapshot when is_map(snapshot) <- envelope["signed"],
         true <- valid_snapshot?(snapshot, root, now),
         :ok <- threshold(root, "snapshot", "snapshot", snapshot, envelope["signatures"]),
         digest <- CanonicalJCS.digest(snapshot),
         :ok <- monotonic(prior, snapshot, digest) do
      {:ok,
       %{
         envelope: snapshot_binary,
         root: root,
         snapshot: snapshot,
         snapshot_digest: digest,
         sequence: snapshot["sequence"],
         verified_at: now
       }}
    else
      _ -> {:error, :registry_snapshot_denied}
    end
  rescue
    _ -> {:error, :registry_snapshot_denied}
  end

  def open(_, _, _, _), do: {:error, :registry_snapshot_denied}

  def acquire(client, package, version, mirrors, options \\ []) do
    mode = Keyword.get(options, :mode, :new)

    with true <-
           Keyword.keys(options) --
             [:mode, :lock, :observed_at, :platform, :toolchain, :acknowledgements] == [],
         true <- mode in [:new, :locked],
         :ok <- verify_client(client),
         :ok <- freshness(client, mode, Keyword.get(options, :observed_at)),
         {:ok, release} <- find_release(client, package, version),
         :ok <- status(release, mode, Keyword.get(options, :lock), client.snapshot_digest),
         {:ok, bytes, mirror} <- fetch(release["artifact"], mirrors),
         :ok <- native_admission(release["artifact"], options) do
      {:ok,
       %{
         package: package,
         version: version,
         bytes: bytes,
         mirror: mirror,
         bundle_digest: release["artifact"]["bundle_digest"],
         content_digest: release["artifact"]["content_digest"],
         snapshot_digest: client.snapshot_digest,
         status: release["status"],
         kind: release["artifact"]["kind"],
         provenance: release["artifact"]["provenance"]
       }}
    else
      _ -> {:error, :registry_acquisition_denied}
    end
  rescue
    _ -> {:error, :registry_acquisition_denied}
  end

  def acquire_lock(client, manifest, lockfile, mirrors, options \\ []) do
    with :ok <- verify_client(client),
         lookup <- release_index(client.snapshot["releases"]),
         {:ok, resolution} <-
           Deps.replay_lockfile(manifest, lockfile, fn name, version, digest ->
             case lookup[{name, version}] do
               %{"artifact" => %{"bundle_digest" => ^digest}} -> true
               _ -> false
             end
           end) do
      Enum.reduce_while(resolution, {:ok, %{}}, fn entry, {:ok, files} ->
        lock = %{
          "package" => entry.name,
          "version" => entry.version,
          "bundle_digest" => entry.bundle_digest,
          "snapshot_digest" => client.snapshot_digest
        }

        case acquire(
               client,
               entry.name,
               entry.version,
               mirrors,
               Keyword.merge(options, mode: :locked, lock: lock)
             ) do
          {:ok, acquired} ->
            path = "registry/#{entry.name}-#{entry.version}.bundle"
            {:cont, {:ok, Map.put(files, path, acquired.bytes)}}

          _ ->
            {:halt, {:error, :registry_lock_replay_denied}}
        end
      end)
    else
      _ -> {:error, :registry_lock_replay_denied}
    end
  end

  def replay_locked(client, lock, mirrors, options \\ []) when is_map(lock) do
    acquire(
      client,
      lock["package"],
      lock["version"],
      mirrors,
      Keyword.merge(options, mode: :locked, lock: lock)
    )
  end

  def dependency_environment(client) do
    with :ok <- verify_client(client) do
      environment =
        client.snapshot["releases"]
        |> Enum.filter(&(&1["status"] == "active" and &1["artifact"]["kind"] == "source"))
        |> Enum.reduce(%{}, fn release, environment ->
          artifact = release["artifact"]
          p = artifact["provenance"]

          metadata = %{
            dependencies: p["dependencies"],
            selection: p["selection"],
            interface_digests: p["interface_digests"],
            component_digests: p["component_digests"],
            modules: p["modules"],
            interfaces: p["interfaces"],
            roots: p["roots"],
            output: p["output"],
            bundle_digest: artifact["bundle_digest"]
          }

          put_in(
            environment,
            [Access.key(artifact["package"], %{}), artifact["version"]],
            metadata
          )
        end)

      {:ok, environment}
    end
  end

  defp valid_root?(root) do
    exact?(root, @root_keys) and root["format"] == "catena-registry-root" and
      root["version"] == "1" and is_integer(root["sequence"]) and root["sequence"] > 0 and
      root["sequence"] <= @max_integer and is_integer(root["expires"]) and
      root["expires"] in 0..@max_integer and valid_keys?(root["keys"]) and
      valid_roles?(root["roles"], root["keys"]) and
      valid_delegations?(root["delegations"], root["keys"])
  end

  defp valid_loaded_root?(root) do
    is_map(root) and exact?(root, ["digest" | @root_keys]) and digest?(root["digest"]) and
      root["digest"] == CanonicalJCS.digest(Map.delete(root, "digest")) and
      valid_root?(Map.delete(root, "digest"))
  end

  defp valid_keys?(keys) do
    is_map(keys) and map_size(keys) in 1..32 and
      Enum.all?(keys, fn {id, public} ->
        digest?(id) and is_binary(public) and byte_size(public) == 64 and
          id == hash(public)
      end)
  end

  defp valid_roles?(roles, keys) do
    is_map(roles) and Enum.sort(Map.keys(roles)) == ~w(recovery root snapshot) and
      Enum.all?(roles, fn {_, role} ->
        exact?(role, @role_keys) and is_list(role["principals"]) and
          role["principals"] == Enum.sort(Enum.uniq(role["principals"])) and
          Enum.all?(role["principals"], &Map.has_key?(keys, &1)) and
          is_integer(role["threshold"]) and role["threshold"] in 1..length(role["principals"])
      end)
  end

  defp valid_delegations?(delegations, keys) do
    is_list(delegations) and length(delegations) <= 256 and
      delegations == Enum.sort_by(delegations, & &1["package"]) and
      length(delegations) == length(Enum.uniq_by(delegations, & &1["package"])) and
      Enum.all?(delegations, fn d ->
        exact?(d, ~w(package principals threshold)) and is_binary(d["package"]) and
          Regex.match?(@name, d["package"]) and is_list(d["principals"]) and
          d["principals"] == Enum.sort(Enum.uniq(d["principals"])) and
          Enum.all?(d["principals"], &Map.has_key?(keys, &1)) and
          is_integer(d["threshold"]) and d["threshold"] in 1..length(d["principals"])
      end)
  end

  defp valid_snapshot?(snapshot, root, now) do
    exact?(snapshot, ~w(format version sequence root_sequence expires releases)) and
      snapshot["format"] == "catena-registry-snapshot" and snapshot["version"] == "1" and
      is_integer(snapshot["sequence"]) and snapshot["sequence"] > 0 and
      snapshot["sequence"] <= @max_integer and
      snapshot["root_sequence"] == root["sequence"] and is_integer(snapshot["expires"]) and
      snapshot["expires"] in now..@max_integer and is_list(snapshot["releases"]) and
      length(snapshot["releases"]) <= 4096 and
      snapshot["releases"] ==
        Enum.sort_by(
          snapshot["releases"],
          &{&1["artifact"]["package"], &1["artifact"]["version"]}
        ) and
      length(snapshot["releases"]) ==
        length(
          Enum.uniq_by(
            snapshot["releases"],
            &{&1["artifact"]["package"], &1["artifact"]["version"]}
          )
        ) and
      Enum.all?(snapshot["releases"], &valid_release?(&1, root)) and
      Enum.all?(snapshot["releases"], &(&1["status_sequence"] <= snapshot["sequence"]))
  end

  defp valid_release?(release, root) do
    with true <- exact?(release, @release_keys),
         artifact when is_map(artifact) <- release["artifact"],
         true <- valid_artifact?(artifact),
         delegation when is_map(delegation) <-
           Enum.find(root["delegations"], &(&1["package"] == artifact["package"])),
         true <- release["status"] in ~w(active yanked compromised),
         true <- is_integer(release["status_sequence"]) and release["status_sequence"] > 0,
         :ok <- delegated_threshold(root, delegation, artifact, release["publisher_signatures"]) do
      true
    else
      _ -> false
    end
  end

  defp valid_artifact?(a) do
    exact?(a, @artifact_keys) and is_binary(a["package"]) and Regex.match?(@name, a["package"]) and
      match?({:ok, _}, Deps.parse_version(a["version"])) and digest?(a["bundle_digest"]) and
      digest?(a["content_digest"]) and is_integer(a["size"]) and
      a["size"] in 0..profile().artifact_bytes and a["kind"] in ~w(source native) and
      valid_provenance?(a)
  end

  defp valid_provenance?(%{"kind" => "source", "provenance" => p} = a) do
    exact?(
      p,
      ~w(component_digests dependencies interface_digests interfaces modules output reproducible_input roots selection)
    ) and
      digest?(p["reproducible_input"]) and valid_dependencies?(p["dependencies"]) and
      exact?(p["selection"], ~w(edition language_revision previews)) and
      p["selection"]["edition"] == "0.1" and is_binary(p["selection"]["language_revision"]) and
      is_list(p["selection"]["previews"]) and
      p["selection"]["previews"] == Enum.sort(Enum.uniq(p["selection"]["previews"])) and
      string_list?(p["interface_digests"], true) and string_list?(p["component_digests"], true) and
      string_list?(p["modules"], false) and string_list?(p["interfaces"], false) and
      string_list?(p["roots"], false) and is_binary(p["output"]) and
      a["bundle_digest"] ==
        Deps.bundle_digest(%{
          name: a["package"],
          version: a["version"],
          dependencies: p["dependencies"],
          selection: p["selection"],
          modules: p["modules"],
          interfaces: p["interfaces"],
          roots: p["roots"],
          output: p["output"],
          interface_digests: p["interface_digests"],
          component_digests: p["component_digests"]
        })
  end

  defp valid_provenance?(%{"kind" => "native", "provenance" => p} = a) do
    exact?(p, ~w(native_package_digest platform reproducible_input toolchain unsafe_obligations)) and
      digest?(p["native_package_digest"]) and p["native_package_digest"] == a["content_digest"] and
      digest?(p["reproducible_input"]) and digest?(p["toolchain"]) and
      is_binary(p["platform"]) and byte_size(p["platform"]) in 1..128 and
      string_list?(p["unsafe_obligations"], false)
  end

  defp valid_provenance?(_), do: false

  defp valid_dependencies?(dependencies) do
    is_map(dependencies) and
      Enum.all?(dependencies, fn {name, requirement} ->
        is_binary(name) and Regex.match?(@name, name) and is_binary(requirement) and
          match?({:ok, _}, Deps.parse_requirement(requirement))
      end)
  end

  defp verify_rotation("normal", old, next, envelope) do
    payload = %{"mode" => "normal", "prior_digest" => old["digest"], "signed" => next}

    with :ok <- threshold(old, "root", "root", payload, envelope["old_signatures"]),
         :ok <- threshold(next, "root", "root", payload, envelope["new_signatures"]),
         do: :ok
  end

  defp verify_rotation("recovery", old, next, envelope) do
    payload = %{"mode" => "recovery", "prior_digest" => old["digest"], "signed" => next}
    threshold(old, "recovery", "root", payload, envelope["old_signatures"])
  end

  defp threshold(root, role, kind, payload, signatures) do
    role = root["roles"][role]

    if is_map(role) and valid_signatures?(signatures) do
      valid =
        signatures
        |> Enum.uniq_by(& &1["principal"])
        |> Enum.count(fn signature ->
          principal = signature["principal"]

          principal in role["principals"] and
            Crypto.verify(
              signing_payload(kind, payload),
              root["keys"][principal],
              signature["signature"]
            )
        end)

      if valid >= role["threshold"], do: :ok, else: {:error, :threshold_not_met}
    else
      {:error, :invalid_signatures}
    end
  end

  defp delegated_threshold(root, delegation, artifact, signatures) do
    if valid_signatures?(signatures) do
      valid =
        signatures
        |> Enum.uniq_by(& &1["principal"])
        |> Enum.count(fn signature ->
          principal = signature["principal"]

          principal in delegation["principals"] and
            Crypto.verify(
              signing_payload("release", artifact),
              root["keys"][principal],
              signature["signature"]
            )
        end)

      if valid >= delegation["threshold"], do: :ok, else: {:error, :publisher_threshold_not_met}
    else
      {:error, :invalid_signatures}
    end
  end

  defp valid_signatures?(signatures),
    do:
      is_list(signatures) and length(signatures) <= 32 and
        Enum.all?(
          signatures,
          &(exact?(&1, ~w(principal signature)) and is_binary(&1["principal"]) and
              is_binary(&1["signature"]))
        )

  defp monotonic(nil, _, _), do: :ok

  defp monotonic(
         %{sequence: sequence, snapshot_digest: digest},
         %{"sequence" => sequence},
         digest
       ),
       do: :ok

  defp monotonic(%{sequence: prior, snapshot: old}, %{"sequence" => sequence} = next, _)
       when sequence > prior do
    old_index = release_index(old["releases"])
    next_index = release_index(next["releases"])

    if Enum.all?(old_index, fn {identity, previous} ->
         case next_index[identity] do
           nil ->
             false

           current ->
             previous["artifact"] == current["artifact"] and
               current["status_sequence"] >= previous["status_sequence"] and
               (current["status"] == previous["status"] or
                  current["status_sequence"] > previous["status_sequence"]) and
               (previous["status"] != "compromised" or current["status"] == "compromised")
         end
       end),
       do: :ok,
       else: {:error, :invalid_registry_history}
  end

  defp monotonic(_, _, _), do: {:error, :rollback_or_equivocation}

  defp verify_client(client) do
    with true <-
           exact?(client, [
             :envelope,
             :root,
             :sequence,
             :snapshot,
             :snapshot_digest,
             :verified_at
           ]),
         {:ok, rebuilt} <- open(client.root, client.envelope, client.verified_at),
         true <- rebuilt == client,
         do: :ok,
         else: (_ -> {:error, :invalid_registry_client})
  rescue
    _ -> {:error, :invalid_registry_client}
  end

  defp freshness(client, :new, observed_at)
       when is_integer(observed_at) and observed_at >= client.verified_at and
              observed_at <= @max_integer do
    if client.root["expires"] >= observed_at and client.snapshot["expires"] >= observed_at,
      do: :ok,
      else: {:error, :registry_metadata_expired}
  end

  defp freshness(_, :locked, _), do: :ok
  defp freshness(_, _, _), do: {:error, :registry_freshness_required}

  defp find_release(client, package, version) do
    case release_index(client.snapshot["releases"])[{package, version}] do
      nil -> {:error, :unknown_release}
      release -> {:ok, release}
    end
  end

  defp release_index(releases),
    do: Map.new(releases, &{{&1["artifact"]["package"], &1["artifact"]["version"]}, &1})

  defp status(%{"status" => "active"}, :new, _, _), do: :ok

  defp status(%{"status" => status, "artifact" => artifact}, :locked, lock, snapshot_digest)
       when status in ~w(active yanked) do
    if is_map(lock) and exact?(lock, ~w(bundle_digest package snapshot_digest version)) and
         lock["package"] == artifact["package"] and lock["version"] == artifact["version"] and
         lock["bundle_digest"] == artifact["bundle_digest"] and
         lock["snapshot_digest"] == snapshot_digest,
       do: :ok,
       else: {:error, :invalid_registry_lock}
  end

  defp status(_, _, _, _), do: {:error, :release_not_admitted}

  defp fetch(artifact, mirrors) do
    if is_list(mirrors) and length(mirrors) in 1..8 do
      mirrors
      |> Enum.with_index()
      |> Enum.find_value({:error, :artifact_unavailable}, fn {mirror, index} ->
        bytes = if is_map(mirror), do: Map.get(mirror, artifact["content_digest"])

        if is_binary(bytes) and byte_size(bytes) == artifact["size"] and
             hash(bytes) == artifact["content_digest"],
           do: {:ok, bytes, index},
           else: nil
      end)
    else
      {:error, :invalid_mirrors}
    end
  end

  defp native_admission(%{"kind" => "source"}, options) do
    if Keyword.keys(options) -- [:mode, :lock, :observed_at] == [],
      do: :ok,
      else: {:error, :source_options_denied}
  end

  defp native_admission(%{"kind" => "native", "provenance" => p}, options) do
    with {:ok, observed} <- Profile.require_supported(),
         platform <- Keyword.get(options, :platform, observed["architecture"]),
         toolchain <- Keyword.get(options, :toolchain, Profile.digest(observed)),
         acknowledgements <- Keyword.get(options, :acknowledgements, []),
         true <- platform == p["platform"] and toolchain == p["toolchain"],
         true <- is_list(acknowledgements) and p["unsafe_obligations"] -- acknowledgements == [],
         do: :ok,
         else: (_ -> {:error, :native_registry_admission_denied})
  end

  defp exact?(map, keys), do: is_map(map) and Enum.sort(Map.keys(map)) == Enum.sort(keys)
  defp digest?(value), do: is_binary(value) and Regex.match?(@digest, value)

  defp string_list?(values, digests),
    do:
      is_list(values) and values == Enum.sort(Enum.uniq(values)) and
        Enum.all?(values, &(is_binary(&1) and (not digests or digest?(&1))))

  defp hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
