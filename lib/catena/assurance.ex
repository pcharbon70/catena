defmodule Catena.Assurance do
  @moduledoc "Build and independently inspect Catena 0.6 artifact-bound assurance manifests."

  alias Catena.{CanonicalJCS, Diagnostic, Governance}
  alias Catena.Governance.Crypto

  @spec build(map(), [map()], [map()], map() | nil, [map()]) :: map()
  def build(package, artifacts, cores, governance_result, signatures \\ []) do
    claims =
      (Enum.flat_map(cores, &get_in(&1, [:specifications, :claims]))
       |> Enum.map(&claim_record/1)) ++ Map.get(package, :claims, [])

    claims = claims |> Enum.uniq_by(& &1["id"]) |> Enum.sort_by(& &1["id"])
    artifact_records = Enum.map(artifacts, &artifact_record/1) |> Enum.sort_by(& &1["path"])

    evidence =
      case governance_result do
        nil ->
          artifact_digests =
            Enum.map(artifact_records, & &1["sha256"]) ++
              Map.get(package, :dependency_digests, [])

          cores
          |> Enum.flat_map(&Governance.compiler_evidence(&1.specifications))
          |> Enum.map(&Map.put(&1, "artifact_digests", Enum.sort(artifact_digests)))

        result ->
          result.evidence
      end

    signed = %{
      "package" => package.package,
      "profile" => package.profile,
      "action" => package.action,
      "modules" => cores |> Enum.map(& &1.module) |> Enum.sort(),
      "dependency_digests" => Map.get(package, :dependency_digests, []) |> Enum.sort(),
      "compiler" => Application.spec(:catena, :vsn) |> to_string(),
      "frontend" => "json-ast-0.6",
      "specification" => "0.6",
      "otp" => :erlang.system_info(:otp_release) |> to_string(),
      "canonicalization" => "RFC8785/catena-safe-integer",
      "artifacts" => artifact_records,
      "claims" => claims,
      "evidence" => evidence |> Enum.sort_by(& &1["id"]),
      "assumptions" =>
        governance_result
        |> governance_evidence()
        |> Enum.filter(&(&1["kind"] == "assumption"))
        |> Enum.sort_by(& &1["id"]),
      "governance" => governance_record(governance_result),
      "erasure" => erasure_record(cores)
    }

    document = %{
      "format" => "catena-assurance-manifest",
      "version" => "0.6",
      "signed" => signed,
      "signatures" => signatures
    }

    payload = CanonicalJCS.payload("manifest", signed)

    %{
      document: document,
      binary: CanonicalJCS.encode(document),
      payload: payload,
      payload_digest: :crypto.hash(:sha256, payload) |> Base.encode16(case: :lower),
      digest: CanonicalJCS.digest(document)
    }
  end

  @spec verify(binary(), Path.t(), map() | nil) :: {:ok, map()} | {:error, Diagnostic.t()}
  def verify(binary, directory, root) do
    with {:ok, document} <- CanonicalJCS.decode(binary, canonical: true),
         "catena-assurance-manifest" <- Map.get(document, "format"),
         "0.6" <- Map.get(document, "version"),
         signed when is_map(signed) <- Map.get(document, "signed"),
         signatures when is_list(signatures) <- Map.get(document, "signatures"),
         :ok <- verify_manifest_shape(signed),
         :ok <- verify_manifest_signature(signed, signatures, root),
         :ok <- verify_artifacts(Map.get(signed, "artifacts"), directory),
         :ok <- verify_erasure(Map.get(signed, "erasure")),
         :ok <- verify_ungoverned_evidence(signed),
         :ok <- verify_governance(signed, root) do
      {:ok,
       %{
         package: signed["package"],
         action: signed["action"],
         state: get_in(signed, ["governance", "state"]),
         digest: CanonicalJCS.digest(document),
         payload_digest:
           :crypto.hash(:sha256, CanonicalJCS.payload("manifest", signed))
           |> Base.encode16(case: :lower)
       }}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      _ -> error("malformed catena-assurance-manifest 0.6 document")
    end
  end

  defp artifact_record(artifact) do
    %{
      "path" => artifact.path,
      "kind" => artifact.kind,
      "size" => byte_size(artifact.binary),
      "sha256" => :crypto.hash(:sha256, artifact.binary) |> Base.encode16(case: :lower)
    }
  end

  defp claim_record(claim) do
    %{
      "id" => claim.id,
      "semantic_digest" => claim.semantic_digest,
      "kind" => claim.kind,
      "subject" => claim.subject,
      "checker_type" => claim.checker_type,
      "examples" => claim.examples
    }
  end

  defp verify_manifest_shape(signed) do
    claims = signed["claims"]
    evidence = signed["evidence"]
    assumptions = signed["assumptions"]
    dependencies = signed["dependency_digests"]
    modules = signed["modules"]

    valid? =
      is_binary(signed["package"]) and byte_size(signed["package"]) > 0 and
        is_binary(signed["profile"]) and signed["action"] in ~w(build publish activate) and
        is_binary(signed["compiler"]) and signed["frontend"] == "json-ast-0.6" and
        signed["specification"] == "0.6" and is_binary(signed["otp"]) and
        signed["canonicalization"] == "RFC8785/catena-safe-integer" and
        string_list?(modules) and digest_list?(dependencies) and is_list(claims) and
        ordered_unique_ids?(claims) and Enum.all?(claims, &valid_claim_record?/1) and
        is_list(evidence) and ordered_unique_ids?(evidence) and is_list(assumptions) and
        ordered_unique_ids?(assumptions) and
        assumptions ==
          Enum.filter(evidence, &(&1["kind"] == "assumption")) |> Enum.sort_by(& &1["id"])

    if valid?, do: :ok, else: error("assurance manifest signed fields are malformed")
  end

  defp verify_ungoverned_evidence(%{"governance" => nil} = signed) do
    claims = signed["claims"]
    artifacts = signed["artifacts"]
    expected_artifacts = Enum.map(artifacts, & &1["sha256"]) ++ signed["dependency_digests"]

    valid? =
      Enum.all?(signed["evidence"], fn record ->
        claim = Enum.find(claims, &(&1["id"] == record["claim_id"]))
        kind = record["kind"]

        common? =
          kind in ~w(conformance example) and is_map(claim) and
            record["claim_digest"] == claim["semantic_digest"] and
            record["subject"] == claim["subject"] and record["producer"] == "catena-compiler" and
            is_binary(record["tool"]) and
            Enum.sort(record["artifact_digests"]) == Enum.sort(expected_artifacts)

        common? and valid_compiler_evidence_result?(kind, record, claim)
      end)

    if valid?, do: :ok, else: error("ungoverned compiler evidence is wrongly bound")
  end

  defp verify_ungoverned_evidence(_signed), do: :ok

  defp valid_compiler_evidence_result?("conformance", record, claim),
    do:
      record["id"] == "compiler:" <> claim["semantic_digest"] and
        record["result"] == "typed_and_pure"

  defp valid_compiler_evidence_result?("example", record, claim) do
    example = record["example"]

    is_map(example) and example in claim["examples"] and example["outcome"] == "supported" and
      record["result"] == "supported" and
      record["id"] ==
        "example:" <> CanonicalJCS.digest(%{"claim" => record["claim_id"], "example" => example})
  end

  defp valid_compiler_evidence_result?(_kind, _record, _claim), do: false

  defp valid_claim_record?(%{
         "id" => id,
         "semantic_digest" => digest,
         "kind" => "rule",
         "subject" => subject,
         "checker_type" => _checker_type,
         "examples" => examples
       }),
       do: is_binary(id) and digest?(digest) and is_map(subject) and is_list(examples)

  defp valid_claim_record?(_claim), do: false

  defp ordered_unique_ids?(records) do
    if Enum.all?(records, &is_map/1) do
      ids = Enum.map(records, &Map.get(&1, "id"))
      Enum.all?(ids, &is_binary/1) and ids == Enum.sort(Enum.uniq(ids))
    else
      false
    end
  end

  defp string_list?(values),
    do:
      is_list(values) and Enum.all?(values, &is_binary/1) and
        values == values |> Enum.uniq() |> Enum.sort()

  defp digest_list?(values), do: string_list?(values) and Enum.all?(values, &digest?/1)
  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)

  defp governance_record(nil), do: nil

  defp governance_record(result) do
    %{
      "bundle_digest" => result.bundle_digest,
      "policy_digest" => result.policy_digest,
      "state" => result.state,
      "sequence" => result.sequence,
      "decision" => result.decision,
      "explanations" => result.explanations,
      "approval_payload_digest" => result.approval_payload_digest,
      "trust_root_digest" => result.trust_root_digest,
      "proposal_digest" => result.proposal_digest,
      "transition_digest" => result.transition_digest,
      "bundle" => result.bundle
    }
  end

  defp governance_evidence(nil), do: []
  defp governance_evidence(result), do: result.evidence

  defp erasure_record(cores) do
    removed =
      cores
      |> Enum.flat_map(fn core ->
        core.definitions
        |> Enum.filter(&Map.get(&1, :verification_only?, false))
        |> Enum.map(&(core.module <> "." <> &1.name))
      end)
      |> Enum.sort()

    retained =
      cores
      |> Enum.flat_map(fn core ->
        core.definitions
        |> Enum.reject(&Map.get(&1, :verification_only?, false))
        |> Enum.map(&(core.module <> "." <> &1.name))
      end)
      |> Enum.sort()

    %{
      "verification_definitions_removed" => removed,
      "runtime_definitions_retained" => retained,
      "runtime_monitors" => [],
      "assurance_metadata_in_beam" => false
    }
  end

  defp verify_manifest_signature(signed, signatures, root) do
    action = signed["action"]

    cond do
      action == "build" and signatures == [] ->
        :ok

      is_nil(root) ->
        error("signed publish or activate manifest requires a trust root")

      true ->
        sequence = get_in(signed, ["governance", "sequence"]) || root.sequence

        case Crypto.verify_threshold(root, "normal", "manifest", signed, signatures, sequence) do
          {:ok, _signers} -> :ok
          {:error, reason} -> error("assurance manifest signature rejected: #{reason}")
        end
    end
  end

  defp verify_artifacts(artifacts, directory) when is_list(artifacts) do
    paths = if Enum.all?(artifacts, &is_map/1), do: Enum.map(artifacts, & &1["path"]), else: nil

    if is_list(paths) and paths == paths |> Enum.uniq() |> Enum.sort() do
      Enum.reduce_while(artifacts, :ok, fn artifact, :ok ->
        with path when is_binary(path) <- artifact["path"],
             size when is_integer(size) and size >= 0 <- artifact["size"],
             digest when is_binary(digest) <- artifact["sha256"],
             true <- digest?(digest),
             {:ok, resolved} <- safe_path(directory, path),
             {:ok, binary} <- File.read(resolved),
             true <- byte_size(binary) == size,
             observed <- :crypto.hash(:sha256, binary) |> Base.encode16(case: :lower),
             true <- observed == digest do
          {:cont, :ok}
        else
          _ ->
            {:halt, error("artifact #{inspect(artifact["path"])} does not match its manifest")}
        end
      end)
    else
      error("manifest artifact paths must be unique and sorted")
    end
  end

  defp verify_artifacts(_artifacts, _directory), do: error("manifest artifacts must be a list")

  defp verify_erasure(%{
         "runtime_monitors" => [],
         "assurance_metadata_in_beam" => false,
         "verification_definitions_removed" => removed,
         "runtime_definitions_retained" => retained
       })
       when is_list(removed) and is_list(retained),
       do: :ok

  defp verify_erasure(_value), do: error("manifest erasure report is malformed")

  defp verify_governance(%{"action" => "build", "governance" => nil}, _root), do: :ok

  defp verify_governance(%{"governance" => nil}, _root),
    do: error("publish and activate manifests require embedded governance")

  defp verify_governance(%{"governance" => record} = signed, supplied_root)
       when is_map(record) do
    with {:ok, verification_root} <- verification_root(record, supplied_root),
         bundle_payload when is_map(bundle_payload) <- record["bundle"],
         bundle_document <- Map.put(bundle_payload, "manifest_signatures", []),
         {:ok, bundle} <-
           bundle_document |> CanonicalJCS.encode() |> Governance.decode_bundle(),
         {:ok, result} <-
           Governance.evaluate(bundle, verification_root, governance_context(signed)),
         true <- Enum.sort_by(result.evidence, & &1["id"]) == signed["evidence"],
         true <- governance_record(result) == record do
      :ok
    else
      {:error, %Diagnostic{} = diagnostic} ->
        error(
          "embedded governance replay failed: #{diagnostic.id}: #{diagnostic.message}: #{JSON.encode!(diagnostic.details)}"
        )

      _ ->
        error("embedded governance record does not reproduce its recorded decision")
    end
  end

  defp verify_governance(_signed, _root), do: error("manifest governance record is malformed")

  defp verification_root(%{"trust_root_digest" => nil}, _supplied_root), do: {:ok, nil}

  defp verification_root(%{"trust_root_digest" => digest}, %{digest: digest} = root)
       when is_binary(digest),
       do: {:ok, root}

  defp verification_root(%{"trust_root_digest" => digest}, _root) when is_binary(digest),
    do: error("assurance manifest names a different or missing trust root")

  defp verification_root(_record, _root), do: error("governance trust-root binding is malformed")

  defp governance_context(signed) do
    claims = Map.get(signed, "claims", [])
    artifacts = Map.get(signed, "artifacts", [])
    evidence = Map.get(signed, "evidence", [])

    artifact_subjects =
      artifacts
      |> Enum.flat_map(fn
        %{"kind" => "interface", "path" => path} -> [%{"kind" => "interface", "name" => path}]
        %{"kind" => "companion_beam", "path" => path} -> [%{"kind" => "output", "name" => path}]
        _ -> []
      end)

    claim_subjects =
      Enum.flat_map(claims, fn
        %{"subject" => subject} when is_map(subject) -> [subject]
        _ -> []
      end)

    %{
      action: signed["action"],
      package: signed["package"],
      profile: signed["profile"],
      modules: Map.get(signed, "modules", []),
      subjects: Enum.uniq(artifact_subjects ++ claim_subjects),
      compiler_evidence: Enum.filter(evidence, &(&1["kind"] in ~w(conformance example))),
      claims: claims,
      claim_digests: Enum.map(claims, & &1["semantic_digest"]),
      artifact_digests:
        Enum.map(artifacts, & &1["sha256"]) ++ Map.get(signed, "dependency_digests", [])
    }
  end

  defp safe_path(directory, path) do
    if Path.type(path) == :absolute or ".." in Path.split(path) do
      {:error, :unsafe}
    else
      root = Path.expand(directory)
      expanded = Path.expand(path, root)

      with true <- String.starts_with?(expanded, root <> "/"),
           {:ok, root_real} <- real_existing_path(root),
           {:ok, artifact_real} <- real_existing_path(expanded),
           true <- String.starts_with?(artifact_real, root_real <> "/") do
        {:ok, artifact_real}
      else
        _ -> {:error, :unsafe}
      end
    end
  end

  defp real_existing_path(path), do: real_existing_path(Path.expand(path), MapSet.new())

  defp real_existing_path(path, visited) do
    if MapSet.member?(visited, path) do
      {:error, :symlink_cycle}
    else
      case File.read_link(path) do
        {:ok, target} ->
          target =
            if Path.type(target) == :absolute,
              do: target,
              else: Path.join(Path.dirname(path), target)

          real_existing_path(Path.expand(target), MapSet.put(visited, path))

        {:error, :einval} ->
          real_directory_components(path, visited)

        {:error, :enoent} ->
          {:error, :missing}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp real_directory_components(path, visited) do
    parent = Path.dirname(path)

    if parent == path do
      {:ok, path}
    else
      with {:ok, real_parent} <- real_existing_path(parent, visited) do
        candidate = Path.join(real_parent, Path.basename(path))

        case File.read_link(candidate) do
          {:ok, target} ->
            target =
              if Path.type(target) == :absolute,
                do: target,
                else: Path.join(real_parent, target)

            real_existing_path(Path.expand(target), MapSet.put(visited, candidate))

          {:error, :einval} ->
            {:ok, candidate}

          {:error, reason} ->
            {:error, reason}
        end
      end
    end
  end

  defp error(message), do: {:error, Diagnostic.new("ART001", message, path: "$")}
end
