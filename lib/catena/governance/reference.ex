defmodule Catena.Governance.Reference do
  @moduledoc "Separately structured oracle for Catena 0.6 policy decisions."

  alias Catena.CanonicalJCS
  alias Catena.Governance.{Crypto, Lifecycle}

  @limit 20_000

  @spec evaluate(map(), map() | nil, map()) :: {:ok, map()} | {:error, atom()}
  def evaluate(bundle, root, context) do
    with true <- context.action in ~w(build publish activate),
         true <- bundle.package == context.package,
         true <- is_nil(root) or root.namespace == context.package,
         {:ok, lifecycle} <- Lifecycle.replay(bundle.transitions, root),
         true <- context.action != "activate" or lifecycle.state == "Active",
         sequence <-
           if(root,
             do: max(root.sequence, max(lifecycle.sequence, 1)),
             else: max(lifecycle.sequence, 1)
           ),
         {:ok, evidence} <-
           admit(bundle.evidence ++ context.compiler_evidence, root, sequence, context),
         policies <- Enum.filter(bundle.policies, &scope?(&1["scope"], context, bundle)),
         true <- policies != [],
         policy_digest <- CanonicalJCS.digest(policies),
         approval <- approval_payload(context, lifecycle, evidence, policy_digest, sequence),
         true <- assumptions_authorized?(evidence, policies),
         policy_context <- %{
           action: context.action,
           state: lifecycle.state,
           profile: bundle.profile,
           sequence: sequence,
           root: root,
           evidence: evidence,
           approvals: bundle.approvals,
           approval_payload: approval
         },
         {:ok, true, explanations} <- decide(policies, policy_context),
         true <-
           transition_exact?(
             context.action,
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
         state: lifecycle.state,
         sequence: sequence,
         policy_digest: policy_digest,
         evidence: evidence,
         explanations: explanations,
         approval_payload: approval
       }}
    else
      _ -> {:error, :denied_or_malformed}
    end
  end

  @spec decide([map()], map()) :: {:ok, boolean(), [map()]} | {:error, atom()}
  def decide(policies, context) when is_list(policies) do
    policies
    |> Enum.reduce_while({:ok, [], @limit}, fn policy, {:ok, traces, fuel} ->
      case check(policy["requirement"], context, fuel) do
        {:ok, allowed, trace, left} ->
          entry = %{"policy" => policy["id"], "decision" => allowed, "requirement" => trace}
          {:cont, {:ok, [entry | traces], left}}

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
    |> case do
      {:ok, traces, _fuel} ->
        traces = Enum.reverse(traces)
        {:ok, Enum.all?(traces, & &1["decision"]), traces}

      result ->
        result
    end
  end

  defp admit(records, root, sequence, context) do
    if length(records) == length(Enum.uniq_by(records, & &1["id"])) do
      Enum.reduce_while(records, {:ok, []}, fn record, {:ok, accepted} ->
        case admit_one(record, root, sequence, context) do
          {:ok, value} -> {:cont, {:ok, [value | accepted]}}
          :error -> {:halt, {:error, :invalid_evidence}}
        end
      end)
      |> case do
        {:ok, values} -> {:ok, Enum.reverse(values)}
        result -> result
      end
    else
      {:error, :duplicate_evidence}
    end
  end

  defp admit_one(%{"kind" => kind} = record, _root, _sequence, context)
       when kind in ~w(conformance example) do
    claim = Enum.find(context.claims, &(&1["id"] == record["claim_id"]))

    common? =
      is_map(claim) and record["claim_digest"] == claim["semantic_digest"] and
        record["subject"] == claim["subject"] and record["producer"] == "catena-compiler" and
        is_binary(record["tool"]) and
        Enum.sort(record["artifact_digests"]) == Enum.sort(context.artifact_digests)

    result? =
      case {kind, claim} do
        {"conformance", claim} when is_map(claim) ->
          record["id"] == "compiler:" <> claim["semantic_digest"] and
            record["result"] == "typed_and_pure"

        {"example", claim} when is_map(claim) ->
          example = record["example"]

          is_map(example) and example in claim["examples"] and example["outcome"] == "supported" and
            record["result"] == "supported" and
            record["id"] ==
              "example:" <>
                CanonicalJCS.digest(%{"claim" => record["claim_id"], "example" => example})

        _ ->
          false
      end

    if common? and result?, do: {:ok, record}, else: :error
  end

  defp admit_one(%{"kind" => "attestation"} = record, root, sequence, context)
       when is_map(root) do
    payload = record["payload"]
    claim = Enum.find(context.claims, &(&1["id"] == payload["claim_id"]))
    window = payload["sequence"]
    role = payload["producer_role"]

    valid? =
      payload["id"] == record["id"] and payload["kind"] == "attestation" and
        record["producer_role"] == role and record["sequence"] == window and is_map(claim) and
        payload["claim_digest"] == claim["semantic_digest"] and
        payload["subject"] == claim["subject"] and
        Enum.sort(payload["artifact_digests"]) == Enum.sort(context.artifact_digests) and
        payload["result"] in ~w(accepted supported) and is_binary(payload["tool"]) and
        byte_size(payload["tool"]) > 0 and valid_window?(window, sequence) and
        record["id"] not in root.revocations.evidence

    signatures =
      if valid? do
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
        )
      else
        {:error, "invalid"}
      end

    case signatures do
      {:ok, signers} ->
        {:ok,
         payload
         |> Map.put("id", record["id"])
         |> Map.put("kind", "attestation")
         |> Map.put("producer_role", role)
         |> Map.put("signers", Enum.sort(signers))}

      _ ->
        :error
    end
  end

  defp admit_one(%{"kind" => "assumption"} = record, _root, sequence, context) do
    claim = Enum.find(context.claims, &(&1["id"] == record["claim_id"]))

    if is_map(claim) and valid_window?(record["sequence"], sequence) and
         record["claim_digest"] == claim["semantic_digest"] and
         record["subject"] == claim["subject"] and record["result"] == "assumed" and
         is_binary(record["reason"]) and byte_size(record["reason"]) > 0,
       do: {:ok, record},
       else: :error
  end

  defp admit_one(_record, _root, _sequence, _context), do: :error

  defp scope?(%{"kind" => "package", "name" => name}, context, _bundle),
    do: name == context.package

  defp scope?(%{"kind" => "action", "name" => name}, context, _bundle),
    do: name == context.action

  defp scope?(%{"kind" => "profile", "name" => name}, _context, bundle),
    do: name == bundle.profile

  defp scope?(%{"kind" => "module", "name" => name}, context, _bundle),
    do: name in context.modules

  defp scope?(%{"kind" => "subject", "name" => name}, context, _bundle),
    do: Enum.any?(context.subjects, &(&1["name"] == name))

  defp scope?(%{"kind" => kind, "name" => name}, context, _bundle)
       when kind in ~w(output interface),
       do: %{"kind" => kind, "name" => name} in context.subjects

  defp scope?(_scope, _context, _bundle), do: false

  defp approval_payload(context, lifecycle, evidence, policy_digest, sequence) do
    {from, to, prior} =
      if context.action == "activate" do
        event = List.last(lifecycle.events)
        {event["from"], event["to"], event["prior_digest"]}
      else
        {lifecycle.state, lifecycle.state, lifecycle.digest}
      end

    %{
      "action" => context.action,
      "package" => context.package,
      "profile" => context.profile,
      "subject" => Map.get(context, :subject, %{"kind" => "package", "name" => context.package}),
      "from" => from,
      "to" => to,
      "sequence" => sequence,
      "prior_transition_digest" => prior,
      "policy_digest" => policy_digest,
      "claim_digests" => Enum.sort(context.claim_digests),
      "artifact_digests" => Enum.sort(context.artifact_digests),
      "evidence" =>
        evidence
        |> Enum.map(&%{"id" => &1["id"], "digest" => CanonicalJCS.digest(&1)})
        |> Enum.sort_by(& &1["id"])
    }
  end

  defp assumptions_authorized?(evidence, policies) do
    evidence
    |> Enum.filter(&(&1["kind"] == "assumption"))
    |> Enum.all?(fn assumption ->
      Enum.all?(policies, &authorizes_assumption?(&1["requirement"], assumption["claim_id"]))
    end)
  end

  defp authorizes_assumption?(%{"op" => "all", "requirements" => children}, claim_id) do
    (Enum.any?(children, &assumption_evidence?(&1, claim_id)) and
       Enum.any?(children, &role_node?/1)) or
      Enum.any?(children, &authorizes_assumption?(&1, claim_id))
  end

  defp authorizes_assumption?(%{"requirements" => children}, claim_id) when is_list(children),
    do: Enum.any?(children, &authorizes_assumption?(&1, claim_id))

  defp authorizes_assumption?(_node, _claim_id), do: false

  defp assumption_evidence?(%{"op" => "evidence"} = node, claim_id),
    do:
      node["kind"] == "assumption" and (is_nil(node["claim_id"]) or node["claim_id"] == claim_id)

  defp assumption_evidence?(%{"requirements" => children}, claim_id) when is_list(children),
    do: Enum.any?(children, &assumption_evidence?(&1, claim_id))

  defp assumption_evidence?(_node, _claim_id), do: false

  defp role_node?(%{"op" => "role"}), do: true

  defp role_node?(%{"requirements" => children}) when is_list(children),
    do: Enum.any?(children, &role_node?/1)

  defp role_node?(_node), do: false

  defp transition_exact?(
         "activate",
         lifecycle,
         context,
         evidence,
         digest,
         explanations,
         approvals,
         sequence
       ) do
    event = List.last(lifecycle.events)

    expected_evidence =
      evidence
      |> Enum.map(&%{"id" => &1["id"], "digest" => CanonicalJCS.digest(&1)})
      |> Enum.sort_by(& &1["id"])

    event["sequence"] == sequence and event["from"] == "Accepted" and event["to"] == "Active" and
      event["action"] == "activate" and event["policy_digest"] == digest and
      event["subject"] ==
        Map.get(context, :subject, %{"kind" => "package", "name" => context.package}) and
      Enum.sort(event["claim_digests"]) == Enum.sort(context.claim_digests) and
      Enum.sort(event["artifact_digests"]) == Enum.sort(context.artifact_digests) and
      event["evidence"] == expected_evidence and
      Enum.sort(event["approvals"]) == approvals |> Enum.map(& &1["id"]) |> Enum.sort() and
      event["decision"] == "allow" and event["explanation"] == %{"policies" => explanations}
  end

  defp transition_exact?(
         _action,
         _lifecycle,
         _context,
         _evidence,
         _digest,
         _explanations,
         _approvals,
         _sequence
       ),
       do: true

  defp valid_window?(%{"from" => first, "to" => last}, sequence),
    do: is_integer(first) and is_integer(last) and first <= sequence and sequence <= last

  defp valid_window?(_window, _sequence), do: false

  defp check(_node, _context, fuel) when fuel <= 0, do: {:error, :budget_exhausted}

  defp check(%{"op" => connective, "requirements" => children}, context, fuel)
       when connective in ~w(all any) and is_list(children) do
    with {:ok, traces, left} <- check_children(children, context, fuel - 1) do
      decision =
        if connective == "all",
          do: Enum.all?(traces, & &1["decision"]),
          else: Enum.any?(traces, & &1["decision"])

      {:ok, decision, trace(connective, decision, %{"children" => traces}), left}
    end
  end

  defp check(
         %{"op" => "threshold", "minimum" => minimum, "requirements" => children},
         context,
         fuel
       )
       when is_integer(minimum) and minimum > 0 and is_list(children) and
              minimum <= length(children) do
    with {:ok, traces, left} <- check_children(children, context, fuel - 1) do
      valid = Enum.count(traces, & &1["decision"])
      decision = valid >= minimum

      {:ok, decision,
       trace("threshold", decision, %{
         "minimum" => minimum,
         "valid" => valid,
         "children" => traces
       }), left}
    end
  end

  defp check(%{"op" => "role", "role" => role, "minimum" => minimum}, context, fuel)
       when is_binary(role) and is_integer(minimum) and minimum > 0 do
    audits =
      context.approvals
      |> Enum.filter(&(get_in(&1, ["payload", "decision"]) == context.approval_payload))
      |> Enum.map(fn approval ->
        case Crypto.signer_audit(
               context.root,
               role,
               "approval",
               approval["payload"],
               approval["signatures"],
               context.sequence,
               %{
                 action: context.action,
                 subject: get_in(approval, ["payload", "decision", "subject", "name"]),
                 profile: context.profile
               }
             ) do
          {:ok, audit} -> audit
          {:error, _reason} -> %{valid: [], invalid: 0, revoked: 0, duplicate: 0}
        end
      end)

    combined = Enum.flat_map(audits, & &1.valid)
    signers = combined |> Enum.uniq() |> Enum.sort()
    duplicate = Enum.sum(Enum.map(audits, & &1.duplicate)) + length(combined) - length(signers)
    invalid = Enum.sum(Enum.map(audits, & &1.invalid))
    revoked = Enum.sum(Enum.map(audits, & &1.revoked))

    root_threshold = get_in(context.root, [:roles, role, :threshold]) || minimum
    effective_minimum = max(minimum, root_threshold)
    decision = length(signers) >= effective_minimum

    {:ok, decision,
     trace("role", decision, %{
       "role" => role,
       "minimum" => minimum,
       "root_threshold" => root_threshold,
       "effective_minimum" => effective_minimum,
       "valid" => length(signers),
       "invalid" => invalid,
       "revoked" => revoked,
       "duplicate" => duplicate,
       "valid_principals" => signers
     }), fuel - 1}
  end

  defp check(%{"op" => "evidence"} = node, context, fuel) do
    minimum = Map.get(node, "minimum", 1)

    matching =
      context.evidence
      |> Enum.filter(fn evidence ->
        evidence["result"] in ~w(supported typed_and_pure accepted assumed) and
          (is_nil(node["claim_id"]) or evidence["claim_id"] == node["claim_id"]) and
          (is_nil(node["kind"]) or evidence["kind"] == node["kind"])
      end)
      |> Enum.uniq_by(& &1["id"])

    decision = is_integer(minimum) and minimum > 0 and length(matching) >= minimum

    {:ok, decision,
     trace("evidence", decision, %{
       "minimum" => minimum,
       "valid_evidence" => matching |> Enum.map(& &1["id"]) |> Enum.sort(),
       "claim_id" => Map.get(node, "claim_id"),
       "kind" => Map.get(node, "kind")
     }), fuel - 1}
  end

  defp check(%{"op" => "action", "allowed" => allowed}, context, fuel)
       when is_list(allowed),
       do: leaf("action", context.action in allowed, %{"allowed" => allowed}, fuel)

  defp check(%{"op" => "state", "allowed" => allowed}, context, fuel)
       when is_list(allowed),
       do: leaf("state", context.state in allowed, %{"allowed" => allowed}, fuel)

  defp check(%{"op" => "profile", "name" => name}, context, fuel) when is_binary(name),
    do: leaf("profile", context.profile == name, %{"name" => name}, fuel)

  defp check(%{"op" => "sequence", "from" => first, "to" => last}, context, fuel)
       when is_integer(first) and is_integer(last) and first <= last,
       do:
         leaf(
           "sequence",
           context.sequence >= first and context.sequence <= last,
           %{"from" => first, "to" => last},
           fuel
         )

  defp check(%{"op" => "deny", "reason" => reason}, _context, fuel) when is_binary(reason),
    do: leaf("deny", false, %{"reason" => reason}, fuel)

  defp check(_node, _context, _fuel), do: {:error, :malformed_policy}

  defp check_children(children, context, fuel) do
    Enum.reduce_while(children, {:ok, [], fuel}, fn child, {:ok, traces, left} ->
      case check(child, context, left) do
        {:ok, _decision, result, next} -> {:cont, {:ok, [result | traces], next}}
        {:error, _} = result -> {:halt, result}
      end
    end)
    |> case do
      {:ok, traces, left} -> {:ok, Enum.reverse(traces), left}
      result -> result
    end
  end

  defp leaf(op, decision, details, fuel),
    do: {:ok, decision, trace(op, decision, details), fuel - 1}

  defp trace(op, decision, details), do: Map.merge(%{"op" => op, "decision" => decision}, details)
end
