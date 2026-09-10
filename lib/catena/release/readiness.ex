defmodule Catena.Release.Readiness do
  @moduledoc "Digest-bound release-readiness manifests with explicit evidence gates."

  alias Catena.CanonicalJCS
  alias Catena.OTP.Profile, as: OTPProfile

  @version "0.1.90"
  @format "catena-release-readiness"
  @classes ~w(experimental complete stable)
  @required_gates ~w(P096 P107 P109 P117 G118 P119 G120 G123 P125 G137)
  @required_evidence ~w(compatibility performance source_tooling usability)
  @required_audits ~w(abstraction-invariant-boundary package-identifier-underscore)
  @proof_source_digest "107a74da9b8e8a963a282f23bfb485f240259d00ef493b66e4d6aad098211024"
  @proof_image_digest "sha256:33926fb3757b2b560c3844157bd64173a26b6d338a256be1ae5eec8df2019025"
  @bounded_theorems ~w(bounded_core_composition component_interaction context_compatibility preservation progress substitution_preserves_typing)

  def profile do
    %{
      version: @version,
      classes: @classes,
      complete_gates: @required_gates,
      evidence_domains: @required_evidence,
      contradiction_audits: @required_audits,
      manifest: :canonical_jcs_digest_bound,
      proof_policy: :kernel_checked_exact_registry,
      platform_policy: :exact_tested_fingerprint,
      publication: :explicit_external_action,
      automatic_class_upgrade: false,
      bounded_proof: proof_workbench(),
      integrated_catena_proof: :required_for_complete_and_stable
    }
  end

  def proof_workbench do
    %{
      "checker" => "The Rocq Prover 9.2",
      "container_digest" => @proof_image_digest,
      "scope" => "bounded-core-not-integrated-catena",
      "source_digest" => @proof_source_digest,
      "status" => "verified",
      "theorems" => @bounded_theorems
    }
  end

  def build(fields) when is_map(fields) do
    payload =
      fields
      |> stringify_keys()
      |> Map.delete("digest")
      |> Map.merge(%{
        "format" => @format,
        "manifest_version" => 1,
        "language_revision" => @version
      })
      |> normalize()

    with :ok <- validate_payload(payload) do
      {:ok, Map.put(payload, "digest", CanonicalJCS.digest(payload))}
    end
  rescue
    _ -> {:error, :invalid_release_manifest}
  end

  def build(_), do: {:error, :invalid_release_manifest}

  def verify(%{"digest" => digest} = manifest) when is_binary(digest) do
    payload = Map.delete(manifest, "digest")

    with :ok <- validate_payload(payload),
         true <- CanonicalJCS.digest(payload) == digest do
      :ok
    else
      false -> {:error, :release_manifest_digest_mismatch}
      {:error, _} = error -> error
    end
  rescue
    _ -> {:error, :invalid_release_manifest}
  end

  def verify(_), do: {:error, :invalid_release_manifest}

  def assess(manifest) do
    with :ok <- verify(manifest) do
      blockers = blockers(manifest)
      release_class = manifest["release_class"]

      {:ok,
       %{
         status: if(blockers == [], do: :ready, else: :blocked),
         release_class: String.to_existing_atom(release_class),
         blockers: blockers,
         claims: claims(release_class, blockers),
         publication_performed: false,
         automatic_upgrade_performed: false
       }}
    end
  end

  defp validate_payload(payload) do
    obligations = payload["obligations"]

    valid =
      payload["format"] == @format and payload["manifest_version"] == 1 and
        payload["language_revision"] == @version and payload["release_class"] in @classes and
        chapter_rows_valid?(payload["normative_chapters"]) and
        obligations_valid?(obligations) and
        nonempty_rows?(payload["platforms"], ~w(fingerprint digest status)) and
        evidence_valid?(payload["evidence"]) and string_list?(payload["open_items"]) and
        nonempty_rows?(payload["limitations"], ~w(id status disposition)) and
        nonempty_rows?(
          payload["proofs"],
          ~w(checker container_digest scope source_digest status theorems)
        ) and
        audit_rows_valid?(payload["contradictions"])

    if valid, do: :ok, else: {:error, :invalid_release_manifest}
  end

  defp blockers(manifest) do
    class = manifest["release_class"]

    []
    |> add_unless(
      Enum.all?(manifest["normative_chapters"], &(&1["status"] == "normative")),
      "normative-chapter-status"
    )
    |> add_unless(platforms_supported?(manifest["platforms"]), "unsupported-platform")
    |> add_unless(bounded_proof_present?(manifest["proofs"]), "bounded-proof")
    |> add_unless(audits_resolved?(manifest["contradictions"]), "contradiction-audit")
    |> add_class_blockers(class, manifest)
    |> Enum.reverse()
  end

  defp add_class_blockers(blockers, "experimental", _manifest), do: blockers

  defp add_class_blockers(blockers, class, manifest) when class in ~w(complete stable) do
    evidence_passes =
      Enum.all?(@required_evidence, &(manifest["evidence"][&1]["status"] == "pass"))

    gates_closed = Enum.all?(@required_gates, &(&1 not in manifest["open_items"]))

    blockers
    |> add_unless(obligations_complete?(manifest["obligations"]), "obligation-coverage")
    |> add_unless(evidence_passes, "required-evidence")
    |> add_unless(gates_closed, "completion-gates")
    |> add_unless(integrated_proof_present?(manifest["proofs"]), "integrated-catena-proof")
    |> add_unless(class != "stable" or stable_policy?(manifest["stability"]), "stability-policy")
  end

  defp claims("experimental", []), do: ["experimental-evidence-bundle"]
  defp claims("complete", []), do: ["language-definition-complete", "integrated-safety-kernel"]

  defp claims("stable", []),
    do: ["language-definition-complete", "integrated-safety-kernel", "stable-release"]

  defp claims(_, _), do: []

  defp obligations_valid?(%{
         "total" => total,
         "traced" => traced,
         "partial" => partial,
         "untraced" => untraced
       }) do
    Enum.all?([total, traced, partial, untraced], &(is_integer(&1) and &1 >= 0)) and
      total == traced + partial + untraced
  end

  defp obligations_valid?(_), do: false

  defp obligations_complete?(%{
         "total" => total,
         "traced" => total,
         "partial" => 0,
         "untraced" => 0
       }),
       do: total > 0

  defp obligations_complete?(_), do: false

  defp evidence_valid?(evidence) when is_map(evidence),
    do:
      Enum.all?(@required_evidence, fn key ->
        case evidence[key] do
          %{"digest" => digest, "status" => status} ->
            digest?(digest) and status in ~w(pass blocked)

          _ ->
            false
        end
      end)

  defp evidence_valid?(_), do: false

  defp audit_rows_valid?(rows) do
    nonempty_rows?(rows, ~w(id status disposition)) and
      Enum.sort(Enum.map(rows, & &1["id"])) == @required_audits
  end

  defp chapter_rows_valid?(rows) do
    nonempty_rows?(rows, ~w(id version digest status)) and
      Enum.all?(rows, fn row ->
        is_binary(row["id"]) and is_binary(row["version"]) and digest?(row["digest"])
      end)
  end

  defp audits_resolved?(rows), do: Enum.all?(rows, &(&1["status"] == "resolved"))

  defp platforms_supported?(rows) do
    Enum.all?(rows, fn row ->
      row["status"] == "supported" and row["fingerprint"] in OTPProfile.supported() and
        row["digest"] == OTPProfile.digest(row["fingerprint"])
    end)
  end

  defp bounded_proof_present?(proofs), do: proof_workbench() in proofs

  # No integrated Catena theorem is admitted in this revision. A self-asserted
  # manifest row cannot extend the verifier's proof registry.
  defp integrated_proof_present?(_proofs), do: false

  defp stable_policy?(%{"compatibility_window" => window, "support_policy_digest" => digest}) do
    is_integer(window) and window > 0 and is_binary(digest) and byte_size(digest) == 64
  end

  defp stable_policy?(_), do: false

  defp nonempty_rows?(rows, keys) when is_list(rows) and rows != [] do
    Enum.all?(rows, fn row ->
      is_map(row) and Enum.all?(keys, &Map.has_key?(row, &1))
    end)
  end

  defp nonempty_rows?(_, _), do: false

  defp string_list?(items),
    do: is_list(items) and Enum.all?(items, &is_binary/1) and items == Enum.sort(Enum.uniq(items))

  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)

  defp add_unless(blockers, true, _blocker), do: blockers
  defp add_unless(blockers, false, blocker), do: [blocker | blockers]

  defp stringify_keys(map) when is_map(map) do
    Map.new(map, fn {key, value} -> {to_string(key), stringify_keys(value)} end)
  end

  defp stringify_keys(list) when is_list(list), do: Enum.map(list, &stringify_keys/1)
  defp stringify_keys(value), do: value

  defp normalize(map) when is_map(map),
    do: Map.new(map, fn {key, value} -> {key, normalize(value)} end)

  defp normalize(list) when is_list(list) do
    list
    |> Enum.map(&normalize/1)
    |> Enum.sort_by(&CanonicalJCS.encode/1)
  end

  defp normalize(value), do: value
end
