defmodule Catena.Governance.Policy do
  @moduledoc "Closed, bounded, explanation-producing versioned policy algebra."

  alias Catena.{Diagnostic, ImplementationLimits, LanguageLifecycle, LanguageVersion}
  alias Catena.Governance.Crypto

  @budget ImplementationLimits.configured(:governance_policy_steps)
  @edition_version LanguageVersion.introduced(:editions_and_feature_lifecycle)

  @spec evaluate(map(), map(), pos_integer()) ::
          {:ok, boolean(), map(), non_neg_integer()} | {:error, Diagnostic.t()}
  def evaluate(requirement, context, budget \\ @budget) do
    case evaluate_node(requirement, context, budget) do
      {:ok, decision, explanation, remaining} ->
        {:ok, decision, explanation, budget - remaining}

      {:error, _} = result ->
        result
    end
  end

  defp evaluate_node(_requirement, _context, remaining) when remaining <= 0,
    do:
      error(
        "policy evaluation exhausted its #{@budget}-step budget",
        ImplementationLimits.details(:governance_policy_steps, @budget + 1)
      )

  defp evaluate_node(%{"op" => "all", "requirements" => requirements}, context, remaining)
       when is_list(requirements) do
    with {:ok, children, left} <- evaluate_many(requirements, context, remaining - 1) do
      decision = Enum.all?(children, & &1["decision"])
      {:ok, decision, explanation("all", decision, %{"children" => children}), left}
    end
  end

  defp evaluate_node(%{"op" => "any", "requirements" => requirements}, context, remaining)
       when is_list(requirements) do
    with {:ok, children, left} <- evaluate_many(requirements, context, remaining - 1) do
      decision = Enum.any?(children, & &1["decision"])
      {:ok, decision, explanation("any", decision, %{"children" => children}), left}
    end
  end

  defp evaluate_node(
         %{"op" => "threshold", "minimum" => minimum, "requirements" => requirements},
         context,
         remaining
       )
       when is_integer(minimum) and minimum > 0 and is_list(requirements) and
              minimum <= length(requirements) do
    with {:ok, children, left} <- evaluate_many(requirements, context, remaining - 1) do
      valid = Enum.count(children, & &1["decision"])
      decision = valid >= minimum

      {:ok, decision,
       explanation("threshold", decision, %{
         "minimum" => minimum,
         "valid" => valid,
         "children" => children
       }), left}
    end
  end

  defp evaluate_node(
         %{"op" => "role", "role" => role, "minimum" => minimum},
         context,
         remaining
       )
       when is_binary(role) and is_integer(minimum) and minimum > 0 do
    with root when is_map(root) <- Map.get(context, :root),
         {:ok, audit} <- approval_audit(context, root, role) do
      distinct = audit.valid
      root_threshold = get_in(root, [:roles, role, :threshold]) || minimum
      effective_minimum = max(minimum, root_threshold)
      decision = length(distinct) >= effective_minimum

      {:ok, decision,
       explanation("role", decision, %{
         "role" => role,
         "minimum" => minimum,
         "root_threshold" => root_threshold,
         "effective_minimum" => effective_minimum,
         "valid" => length(distinct),
         "invalid" => audit.invalid,
         "revoked" => audit.revoked,
         "duplicate" => audit.duplicate,
         "valid_principals" => Enum.sort(distinct)
       }), remaining - 1}
    else
      nil ->
        {:ok, false,
         explanation("role", false, %{
           "role" => role,
           "minimum" => minimum,
           "reason" => "trust root is missing"
         }), remaining - 1}

      {:error, reason} ->
        {:ok, false,
         explanation("role", false, %{
           "role" => role,
           "minimum" => minimum,
           "reason" => reason
         }), remaining - 1}
    end
  end

  defp evaluate_node(%{"op" => "evidence"} = requirement, context, remaining) do
    minimum = Map.get(requirement, "minimum", 1)

    if is_integer(minimum) and minimum > 0 do
      matching =
        context
        |> Map.get(:evidence, [])
        |> Enum.filter(&evidence_matches?(&1, requirement))
        |> Enum.uniq_by(& &1["id"])

      decision = length(matching) >= minimum

      {:ok, decision,
       explanation("evidence", decision, %{
         "minimum" => minimum,
         "valid_evidence" => matching |> Enum.map(& &1["id"]) |> Enum.sort(),
         "claim_id" => Map.get(requirement, "claim_id"),
         "kind" => Map.get(requirement, "kind")
       }), remaining - 1}
    else
      error("evidence minimum must be positive")
    end
  end

  defp evaluate_node(%{"op" => "action", "allowed" => allowed}, context, remaining)
       when is_list(allowed) do
    decision = context.action in allowed
    {:ok, decision, explanation("action", decision, %{"allowed" => allowed}), remaining - 1}
  end

  defp evaluate_node(%{"op" => "state", "allowed" => allowed}, context, remaining)
       when is_list(allowed) do
    decision = context.state in allowed
    {:ok, decision, explanation("state", decision, %{"allowed" => allowed}), remaining - 1}
  end

  defp evaluate_node(%{"op" => "profile", "name" => name}, context, remaining)
       when is_binary(name) do
    decision = context.profile == name
    {:ok, decision, explanation("profile", decision, %{"name" => name}), remaining - 1}
  end

  defp evaluate_node(
         %{"op" => "sequence", "from" => first, "to" => last},
         context,
         remaining
       )
       when is_integer(first) and is_integer(last) and first <= last do
    decision = context.sequence >= first and context.sequence <= last

    {:ok, decision, explanation("sequence", decision, %{"from" => first, "to" => last}),
     remaining - 1}
  end

  defp evaluate_node(%{"op" => "edition", "allowed" => allowed} = node, context, remaining) do
    known = Enum.map(LanguageVersion.editions(), & &1["id"])

    if context.format_version == @edition_version and exact_fields?(node, ~w(op allowed)) and
         known_list?(allowed, known) do
      selected = context.edition
      decision = selected in allowed

      {:ok, decision,
       explanation("edition", decision, %{"selected" => selected, "allowed" => allowed}),
       remaining - 1}
    else
      error("unknown or malformed edition policy requirement")
    end
  end

  defp evaluate_node(
         %{"op" => "language_revision", "from" => first, "to" => last} = node,
         context,
         remaining
       ) do
    revisions = LanguageVersion.all()

    if context.format_version == @edition_version and exact_fields?(node, ~w(op from to)) and
         first in revisions and last in revisions and LanguageVersion.at_or_after?(last, first) do
      selected = context.language_revision
      decision = selected in revisions and LanguageVersion.between?(selected, first, last)

      {:ok, decision,
       explanation("language_revision", decision, %{
         "selected" => selected,
         "from" => first,
         "to" => last
       }), remaining - 1}
    else
      error("unknown or malformed language revision policy requirement")
    end
  end

  defp evaluate_node(%{"op" => "previews", "allowed" => allowed} = node, context, remaining) do
    known = LanguageLifecycle.preview_ids(context.language_revision)

    if context.format_version == @edition_version and exact_fields?(node, ~w(op allowed)) and
         known_list?(allowed, known) do
      selected = context.previews
      decision = is_list(selected) and Enum.all?(selected, &(&1 in allowed))

      {:ok, decision,
       explanation("previews", decision, %{"selected" => selected, "allowed" => allowed}),
       remaining - 1}
    else
      error("unknown or malformed previews policy requirement")
    end
  end

  defp evaluate_node(%{"op" => "diagnostics", "absent" => absent} = node, context, remaining) do
    if context.format_version == @edition_version and exact_fields?(node, ~w(op absent)) and
         known_list?(absent, LanguageLifecycle.warning_ids()) do
      present = context.diagnostics
      decision = is_list(present) and Enum.all?(absent, &(&1 not in present))

      {:ok, decision,
       explanation("diagnostics", decision, %{"present" => present, "absent" => absent}),
       remaining - 1}
    else
      error("unknown or malformed diagnostics policy requirement")
    end
  end

  defp evaluate_node(%{"op" => "deny", "reason" => reason}, _context, remaining)
       when is_binary(reason) and byte_size(reason) > 0,
       do: {:ok, false, explanation("deny", false, %{"reason" => reason}), remaining - 1}

  defp evaluate_node(_requirement, _context, _remaining),
    do: error("unknown or malformed policy requirement")

  defp evaluate_many(requirements, context, remaining) do
    Enum.reduce_while(requirements, {:ok, [], remaining}, fn requirement, {:ok, values, left} ->
      case evaluate_node(requirement, context, left) do
        {:ok, _decision, explanation, next} ->
          {:cont, {:ok, [explanation | values], next}}

        {:error, _} = result ->
          {:halt, result}
      end
    end)
    |> case do
      {:ok, values, left} -> {:ok, Enum.reverse(values), left}
      result -> result
    end
  end

  defp approval_audit(context, root, role) do
    context
    |> Map.get(:approvals, [])
    |> Enum.reduce_while({:ok, %{valid: [], invalid: 0, revoked: 0, duplicate: 0}}, fn approval,
                                                                                       {:ok,
                                                                                        aggregate} ->
      if get_in(approval, ["payload", "decision"]) == context.approval_payload do
        case Crypto.signer_audit(
               root,
               role,
               "approval",
               approval["payload"],
               Map.get(approval, "signatures", []),
               context.sequence,
               %{
                 action: context.action,
                 subject: get_in(approval, ["payload", "decision", "subject", "name"]),
                 profile: context.profile
               }
             ) do
          {:ok, audit} ->
            combined = aggregate.valid ++ audit.valid

            next = %{
              valid: Enum.uniq(combined),
              invalid: aggregate.invalid + audit.invalid,
              revoked: aggregate.revoked + audit.revoked,
              duplicate:
                aggregate.duplicate + audit.duplicate +
                  (length(combined) - length(Enum.uniq(combined)))
            }

            {:cont, {:ok, next}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      else
        {:cont, {:ok, aggregate}}
      end
    end)
  end

  defp evidence_matches?(evidence, requirement) do
    result_ok? = evidence["result"] in ~w(supported typed_and_pure accepted assumed)
    claim_ok? = is_nil(requirement["claim_id"]) or evidence["claim_id"] == requirement["claim_id"]
    kind_ok? = is_nil(requirement["kind"]) or evidence["kind"] == requirement["kind"]
    result_ok? and claim_ok? and kind_ok?
  end

  defp explanation(op, decision, details),
    do: Map.merge(%{"op" => op, "decision" => decision}, details)

  defp known_list?(values, known),
    do:
      is_list(values) and Enum.all?(values, &is_binary/1) and
        values == values |> Enum.uniq() |> Enum.sort() and Enum.all?(values, &(&1 in known))

  defp exact_fields?(value, fields),
    do: MapSet.new(Map.keys(value)) == MapSet.new(fields)

  defp error(message, details \\ %{}),
    do: {:error, Diagnostic.new("GOV002", message, path: "$.policies", details: details)}
end
