defmodule Catena.Governance do
  @moduledoc "Versioned governance bundle validation, evidence admission, and package gate."

  alias Catena.{CanonicalJCS, Diagnostic, ImplementationLimits, LanguageVersion}
  alias Catena.Governance.{Crypto, Lifecycle, Policy}

  @actions ~w(build publish activate)
  @scope_kinds ~w(package module subject action output interface profile)
  @legacy_version LanguageVersion.introduced(:specifications_and_governance)
  @edition_version LanguageVersion.introduced(:editions_and_feature_lifecycle)
  @versions LanguageVersion.signed_format_versions()

  @spec decode_bundle(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode_bundle(binary) when is_binary(binary) do
    with {:ok, value} <- CanonicalJCS.decode(binary, canonical: true),
         "catena-governance-bundle" <- Map.get(value, "format"),
         version when version in @versions <- Map.get(value, "version"),
         package when is_binary(package) and byte_size(package) > 0 <- Map.get(value, "package"),
         profile when is_binary(profile) and byte_size(profile) > 0 <-
           Map.get(value, "profile", "static"),
         policies when is_list(policies) <- Map.get(value, "policies"),
         true <- policies != [] and unique_records?(policies),
         true <- Enum.all?(policies, &valid_policy?/1),
         evidence when is_list(evidence) <- Map.get(value, "evidence", []),
         true <- unique_records?(evidence) and Enum.all?(evidence, &valid_evidence_shape?/1),
         approvals when is_list(approvals) <- Map.get(value, "approvals", []),
         true <- unique_records?(approvals) and Enum.all?(approvals, &valid_signed_record?/1),
         transitions when is_list(transitions) <- Map.get(value, "transitions", []),
         manifest_signatures when is_list(manifest_signatures) <-
           Map.get(value, "manifest_signatures", []),
         true <- Enum.all?(manifest_signatures, &valid_signature_shape?/1) do
      {:ok,
       %{
         version: version,
         package: package,
         profile: profile,
         policies: policies,
         evidence: evidence,
         approvals: approvals,
         transitions: transitions,
         manifest_signatures: manifest_signatures,
         digest: CanonicalJCS.digest(Map.delete(value, "manifest_signatures")),
         canonical: value
       }}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      _ -> error("GOV001", "malformed or unsupported catena-governance-bundle document", "$")
    end
  end

  @spec evaluate(map(), map() | nil, map()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def evaluate(bundle, root, context) do
    context = Map.put(context, :format_version, bundle.version)
    action = Map.get(context, :action)

    with true <- action in @actions,
         true <- bundle.package == context.package,
         :ok <- namespace_matches(root, bundle.package),
         :ok <- format_matches(root, bundle.version),
         {:ok, lifecycle} <- Lifecycle.replay(bundle.transitions, root),
         :ok <- valid_action_state(action, lifecycle),
         sequence <- current_sequence(root, lifecycle),
         {:ok, evidence} <-
           admit_evidence(
             bundle.evidence ++ Map.get(context, :compiler_evidence, []),
             root,
             sequence,
             context
           ),
         policies <- Enum.filter(bundle.policies, &scope_matches?(&1["scope"], context, bundle)),
         true <- policies != [],
         policy_digest <- CanonicalJCS.digest(policies),
         approval_payload <-
           approval_payload(context, lifecycle, evidence, policy_digest, sequence),
         policy_context <- %{
           format_version: bundle.version,
           action: action,
           subject: Map.get(context, :subject, %{"kind" => "package", "name" => context.package}),
           state: lifecycle.state,
           profile: bundle.profile,
           edition: Map.get(context, :edition),
           language_revision: Map.get(context, :language_revision),
           previews: Map.get(context, :previews, []),
           diagnostics: Map.get(context, :diagnostics, []),
           sequence: sequence,
           root: root,
           evidence: evidence,
           approvals: bundle.approvals,
           approval_payload: approval_payload
         },
         :ok <- validate_assumptions(evidence, policies),
         {:ok, explanations, steps} <- evaluate_policies(policies, policy_context),
         :ok <-
           validate_action_transition(
             action,
             lifecycle,
             context,
             evidence,
             policy_digest,
             explanations,
             bundle.approvals,
             sequence
           ) do
      {:ok,
       %{
         decision: "allow",
         action: action,
         package: bundle.package,
         profile: bundle.profile,
         edition: Map.get(context, :edition),
         language_revision: Map.get(context, :language_revision),
         previews: Map.get(context, :previews, []),
         diagnostics: Map.get(context, :diagnostics, []),
         state: lifecycle.state,
         sequence: sequence,
         policy_digest: policy_digest,
         bundle_digest: bundle.digest,
         approval_payload: approval_payload,
         approval_payload_digest: CanonicalJCS.digest(approval_payload),
         evidence: evidence,
         explanations: explanations,
         steps: steps,
         lifecycle: lifecycle,
         transition_digest: lifecycle.digest,
         proposal_digest:
           case List.last(lifecycle.events) do
             nil -> nil
             event -> event["proposal_digest"]
           end,
         bundle: Map.delete(bundle.canonical, "manifest_signatures"),
         trust_root_digest: if(root, do: root.digest, else: nil)
       }}
    else
      false -> error("GOV001", "governed action has no applicable allowing policy", "$.policies")
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      {:error, reason} when is_binary(reason) -> error("GOV001", reason, "$.policies")
      _ -> error("GOV001", "governed action is malformed or denied", "$")
    end
  end

  @spec compiler_evidence(map()) :: [map()]
  def compiler_evidence(%{claims: claims}) do
    Enum.flat_map(claims, fn claim ->
      typed = %{
        "id" => "compiler:" <> claim.semantic_digest,
        "kind" => "conformance",
        "claim_id" => claim.id,
        "claim_digest" => claim.semantic_digest,
        "subject" => claim.subject,
        "producer" => "catena-compiler",
        "tool" => "catena-" <> to_string(Application.spec(:catena, :vsn)),
        "artifact_digests" => [],
        "result" => "typed_and_pure"
      }

      examples =
        Enum.map(claim.examples, fn example ->
          %{
            "id" =>
              "example:" <> CanonicalJCS.digest(%{"claim" => claim.id, "example" => example}),
            "kind" => "example",
            "claim_id" => claim.id,
            "claim_digest" => claim.semantic_digest,
            "subject" => claim.subject,
            "producer" => "catena-compiler",
            "tool" => "catena-" <> to_string(Application.spec(:catena, :vsn)),
            "artifact_digests" => [],
            "example" => example,
            "result" => example["outcome"]
          }
        end)

      [typed | examples]
    end)
  end

  def compiler_evidence(_), do: []

  defp evaluate_policies(policies, context) do
    budget = ImplementationLimits.configured(:governance_policy_steps)

    Enum.reduce_while(policies, {:ok, [], 0, budget}, fn policy,
                                                         {:ok, explanations, steps, remaining} ->
      case Policy.evaluate(policy["requirement"], context, remaining) do
        {:ok, true, explanation, spent} ->
          value = %{"policy" => policy["id"], "decision" => true, "requirement" => explanation}
          {:cont, {:ok, [value | explanations], steps + spent, remaining - spent}}

        {:ok, false, explanation, _spent} ->
          {:halt,
           {:error,
            Diagnostic.new(
              "GOV001",
              "policy #{policy["id"]} denied the #{context.action} action: #{JSON.encode!(explanation)}",
              path: "$.policies",
              details: %{
                action: context.action,
                subject: context.subject,
                policy: policy["id"],
                requirement: explanation
              }
            )}}

        {:error, _} = result ->
          {:halt, result}
      end
    end)
    |> case do
      {:ok, explanations, steps, _remaining} -> {:ok, Enum.reverse(explanations), steps}
      result -> result
    end
  end

  defp admit_evidence(records, root, sequence, context) do
    if not unique_records?(records) do
      error("EVD001", "evidence identifiers must be unique", "$.evidence")
    else
      Enum.reduce_while(records, {:ok, []}, fn record, {:ok, admitted} ->
        case admit_evidence_record(record, root, sequence, context) do
          {:ok, value} -> {:cont, {:ok, [value | admitted]}}
          {:error, _} = result -> {:halt, result}
        end
      end)
      |> case do
        {:ok, values} -> {:ok, Enum.reverse(values)}
        result -> result
      end
    end
  end

  defp admit_evidence_record(%{"kind" => kind} = record, _root, _sequence, context)
       when kind in ~w(conformance example) do
    claim = Enum.find(Map.get(context, :claims, []), &(&1["id"] == record["claim_id"]))

    checks = %{
      claim: is_map(claim),
      claim_digest: is_map(claim) and record["claim_digest"] == claim["semantic_digest"],
      subject: is_map(claim) and record["subject"] == claim["subject"],
      producer: record["producer"] == "catena-compiler",
      tool: is_binary(record["tool"]),
      artifact_digests:
        is_list(record["artifact_digests"]) and
          Enum.sort(record["artifact_digests"]) ==
            Enum.sort(Map.get(context, :artifact_digests, [])),
      result: is_map(claim) and valid_compiler_result?(kind, record, claim)
    }

    if Enum.all?(checks, fn {_name, valid?} -> valid? end) do
      {:ok, record}
    else
      {:error,
       Diagnostic.new("EVD001", "compiler evidence is malformed or wrongly bound",
         path: "$.evidence",
         details: %{
           evidence: record["id"],
           evidence_claim: record["claim_id"],
           available_claims: Enum.map(Map.get(context, :claims, []), & &1["id"]),
           checks: checks
         }
       )}
    end
  end

  defp admit_evidence_record(%{"kind" => "attestation"} = record, root, sequence, context)
       when is_map(root) do
    payload = record["payload"]
    window = payload["sequence"]
    role = payload["producer_role"]
    claim = Enum.find(Map.get(context, :claims, []), &(&1["id"] == payload["claim_id"]))

    with true <- payload["id"] == record["id"],
         true <- payload["kind"] == "attestation",
         true <- record["producer_role"] == role,
         true <- record["sequence"] == window,
         true <- is_map(claim),
         true <- payload["claim_digest"] == claim["semantic_digest"],
         true <- payload["subject"] == claim["subject"],
         true <- is_list(payload["artifact_digests"]),
         true <-
           Enum.sort(payload["artifact_digests"]) ==
             Enum.sort(Map.get(context, :artifact_digests, [])),
         true <- payload["result"] in ~w(accepted supported),
         true <- is_binary(payload["tool"]) and byte_size(payload["tool"]) > 0,
         true <- valid_window?(window, sequence),
         true <- record["id"] not in root.revocations.evidence,
         {:ok, signers} <-
           Crypto.verify_threshold(
             root,
             role,
             "evidence",
             payload,
             record["signatures"],
             sequence,
             %{
               action: context.action,
               subject: get_in(payload, ["subject", "name"]),
               profile: context.profile
             }
           ) do
      {:ok,
       payload
       |> Map.put("id", record["id"])
       |> Map.put("kind", "attestation")
       |> Map.put("producer_role", role)
       |> Map.put("signers", Enum.sort(signers))}
    else
      _ ->
        error(
          "EVD001",
          "external attestation is stale, revoked, or incorrectly signed",
          "$.evidence"
        )
    end
  end

  defp admit_evidence_record(%{"kind" => "assumption"} = record, _root, sequence, context) do
    claim = Enum.find(Map.get(context, :claims, []), &(&1["id"] == record["claim_id"]))

    if valid_window?(record["sequence"], sequence) and is_map(claim) and
         record["claim_digest"] == claim["semantic_digest"] and
         record["subject"] == claim["subject"] and record["result"] == "assumed" and
         is_binary(record["reason"]) and byte_size(record["reason"]) > 0 do
      {:ok, record}
    else
      error("EVD001", "assumption is malformed or outside its sequence window", "$.evidence")
    end
  end

  defp admit_evidence_record(_record, _root, _sequence, _context),
    do: error("EVD001", "unknown evidence kind or missing trust root", "$.evidence")

  defp valid_compiler_result?("conformance", record, claim),
    do:
      record["id"] == "compiler:" <> claim["semantic_digest"] and
        record["result"] == "typed_and_pure"

  defp valid_compiler_result?("example", record, claim) do
    example = record["example"]

    is_map(example) and example in claim["examples"] and example["outcome"] == "supported" and
      record["result"] == "supported" and
      record["id"] ==
        "example:" <> CanonicalJCS.digest(%{"claim" => record["claim_id"], "example" => example})
  end

  defp validate_assumptions(evidence, policies) do
    assumptions = Enum.filter(evidence, &(&1["kind"] == "assumption"))

    if Enum.all?(assumptions, fn assumption ->
         Enum.all?(policies, fn policy ->
           explicitly_authorizes_assumption?(policy["requirement"], assumption["claim_id"])
         end)
       end) do
      :ok
    else
      error(
        "EVD001",
        "an assumption may count only when every active policy names it and requires an authorized role",
        "$.evidence"
      )
    end
  end

  defp explicitly_accepts_assumption?(%{"op" => "evidence"} = node, claim_id),
    do:
      node["kind"] == "assumption" and (is_nil(node["claim_id"]) or node["claim_id"] == claim_id)

  defp explicitly_accepts_assumption?(%{"requirements" => children}, claim_id)
       when is_list(children),
       do: Enum.any?(children, &explicitly_accepts_assumption?(&1, claim_id))

  defp explicitly_accepts_assumption?(_node, _claim_id), do: false

  defp contains_role?(%{"op" => "role"}), do: true

  defp contains_role?(%{"requirements" => children}) when is_list(children),
    do: Enum.any?(children, &contains_role?/1)

  defp contains_role?(_node), do: false

  defp explicitly_authorizes_assumption?(%{"op" => "all", "requirements" => children}, claim_id)
       when is_list(children) do
    (Enum.any?(children, &explicitly_accepts_assumption?(&1, claim_id)) and
       Enum.any?(children, &contains_role?/1)) or
      Enum.any?(children, &explicitly_authorizes_assumption?(&1, claim_id))
  end

  defp explicitly_authorizes_assumption?(%{"requirements" => children}, claim_id)
       when is_list(children),
       do: Enum.any?(children, &explicitly_authorizes_assumption?(&1, claim_id))

  defp explicitly_authorizes_assumption?(_node, _claim_id), do: false

  defp approval_payload(context, lifecycle, evidence, policy_digest, sequence) do
    {from, to, prior_transition_digest} = approval_states(context.action, lifecycle)

    payload = %{
      "action" => context.action,
      "package" => context.package,
      "profile" => context.profile,
      "subject" => Map.get(context, :subject, %{"kind" => "package", "name" => context.package}),
      "from" => from,
      "to" => to,
      "sequence" => sequence,
      "prior_transition_digest" => prior_transition_digest,
      "policy_digest" => policy_digest,
      "claim_digests" => Enum.sort(Map.get(context, :claim_digests, [])),
      "artifact_digests" => Enum.sort(Map.get(context, :artifact_digests, [])),
      "evidence" =>
        evidence
        |> Enum.map(&%{"id" => &1["id"], "digest" => CanonicalJCS.digest(&1)})
        |> Enum.sort_by(& &1["id"])
    }

    selection_approval_payload(payload, context)
  end

  defp approval_states("activate", %{events: events}) do
    case List.last(events) do
      %{"from" => from, "to" => to, "prior_digest" => prior} -> {from, to, prior}
      _ -> {"Draft", "Draft", String.duplicate("0", 64)}
    end
  end

  defp approval_states(_action, lifecycle),
    do: {lifecycle.state, lifecycle.state, lifecycle.digest}

  defp scope_matches?(%{"kind" => "package", "name" => name}, context, _bundle),
    do: name == context.package

  defp scope_matches?(%{"kind" => "action", "name" => name}, context, _bundle),
    do: name == context.action

  defp scope_matches?(%{"kind" => "profile", "name" => name}, _context, bundle),
    do: name == bundle.profile

  defp scope_matches?(%{"kind" => "module", "name" => name}, context, _bundle),
    do: name in Map.get(context, :modules, [])

  defp scope_matches?(%{"kind" => "subject", "name" => name}, context, _bundle),
    do: Enum.any?(Map.get(context, :subjects, []), &(&1["name"] == name))

  defp scope_matches?(%{"kind" => kind, "name" => name}, context, _bundle)
       when kind in ~w(output interface),
       do: %{"kind" => kind, "name" => name} in Map.get(context, :subjects, [])

  defp scope_matches?(_scope, _context, _bundle), do: false

  defp namespace_matches(nil, _package), do: :ok
  defp namespace_matches(%{namespace: package}, package), do: :ok

  defp namespace_matches(_root, _package),
    do: {:error, "trust-root namespace does not match package"}

  defp format_matches(nil, _version), do: :ok
  defp format_matches(%{version: version}, version), do: :ok

  defp format_matches(_root, _version),
    do: {:error, "governance bundle and trust root use different format versions"}

  defp selection_approval_payload(payload, %{format_version: @edition_version} = context) do
    Map.merge(payload, %{
      "edition" => Map.get(context, :edition),
      "language_revision" => Map.get(context, :language_revision),
      "previews" => Map.get(context, :previews, []),
      "diagnostics" => Map.get(context, :diagnostics, [])
    })
  end

  defp selection_approval_payload(payload, %{format_version: @legacy_version}), do: payload

  defp valid_action_state("activate", %{state: "Active", events: events}) do
    case List.last(events) do
      %{"action" => "activate", "to" => "Active"} -> :ok
      _ -> {:error, "activate requires a signed transition into Active"}
    end
  end

  defp valid_action_state("activate", _lifecycle),
    do: {:error, "activate requires the replayed lifecycle state to be Active"}

  defp valid_action_state(_action, _lifecycle), do: :ok

  defp validate_action_transition(
         "activate",
         lifecycle,
         context,
         evidence,
         policy_digest,
         explanations,
         approvals,
         sequence
       ) do
    event = List.last(lifecycle.events)

    expected_evidence =
      evidence
      |> Enum.map(&%{"id" => &1["id"], "digest" => CanonicalJCS.digest(&1)})
      |> Enum.sort_by(& &1["id"])

    expected_subject =
      Map.get(context, :subject, %{"kind" => "package", "name" => context.package})

    valid? =
      is_map(event) and event["sequence"] == sequence and event["action"] == "activate" and
        event["from"] == "Accepted" and event["to"] == "Active" and
        event["subject"] == expected_subject and
        Enum.sort(event["claim_digests"]) == Enum.sort(Map.get(context, :claim_digests, [])) and
        Enum.sort(event["artifact_digests"]) ==
          Enum.sort(Map.get(context, :artifact_digests, [])) and
        event["policy_digest"] == policy_digest and event["evidence"] == expected_evidence and
        Enum.sort(event["approvals"]) ==
          approvals |> Enum.map(& &1["id"]) |> Enum.sort() and
        event["decision"] == "allow" and event["explanation"] == %{"policies" => explanations}

    if valid?, do: :ok, else: {:error, "activate transition does not bind the exact decision"}
  end

  defp validate_action_transition(
         _action,
         _lifecycle,
         _context,
         _evidence,
         _policy_digest,
         _explanations,
         _approvals,
         _sequence
       ),
       do: :ok

  defp current_sequence(nil, lifecycle), do: max(lifecycle.sequence, 1)
  defp current_sequence(root, lifecycle), do: max(root.sequence, max(lifecycle.sequence, 1))

  defp valid_policy?(%{
         "id" => id,
         "scope" => %{"kind" => kind, "name" => name},
         "requirement" => requirement
       }) do
    is_binary(id) and byte_size(id) > 0 and kind in @scope_kinds and is_binary(name) and
      byte_size(name) > 0 and is_map(requirement)
  end

  defp valid_policy?(_value), do: false

  defp valid_evidence_shape?(%{"id" => id, "kind" => kind})
       when is_binary(id) and kind in ~w(attestation assumption),
       do: true

  defp valid_evidence_shape?(_value), do: false

  defp valid_signed_record?(%{"id" => id, "payload" => payload, "signatures" => signatures}),
    do:
      is_binary(id) and is_map(payload) and payload["id"] == id and is_list(signatures) and
        Enum.all?(signatures, &valid_signature_shape?/1)

  defp valid_signed_record?(_value), do: false

  defp valid_signature_shape?(%{"principal" => principal, "signature" => signature}),
    do:
      is_binary(principal) and byte_size(principal) > 0 and is_binary(signature) and
        Regex.match?(~r/^[0-9a-f]{128}$/, signature)

  defp valid_signature_shape?(_value), do: false

  defp unique_records?(records) when is_list(records),
    do:
      Enum.all?(records, &(is_map(&1) and is_binary(Map.get(&1, "id")))) and
        length(records) == length(Enum.uniq_by(records, & &1["id"]))

  defp valid_window?(%{"from" => first, "to" => last}, sequence),
    do: is_integer(first) and is_integer(last) and first <= sequence and sequence <= last

  defp valid_window?(_window, _sequence), do: false

  defp error(id, message, path), do: {:error, Diagnostic.new(id, message, path: path)}
end
