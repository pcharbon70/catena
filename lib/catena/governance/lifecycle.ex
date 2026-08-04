defmodule Catena.Governance.Lifecycle do
  @moduledoc "Immutable Catena 0.1.6 lifecycle replay with signed hash-chain validation."

  alias Catena.{CanonicalJCS, Diagnostic}
  alias Catena.Governance.{Crypto, TrustRoot}

  @initial_digest String.duplicate("0", 64)

  @edges %{
    "Draft" => ~w(Proposed),
    "Proposed" => ~w(Accepted Rejected Withdrawn),
    "Accepted" => ~w(Active Withdrawn),
    "Active" => ~w(Deprecated),
    "Deprecated" => ~w(Superseded),
    "Rejected" => [],
    "Withdrawn" => [],
    "Superseded" => []
  }

  @spec replay([map()], map() | nil) :: {:ok, map()} | {:error, Diagnostic.t()}
  def replay(events, root) when is_list(events) do
    initial = %{state: "Draft", sequence: 0, digest: @initial_digest, events: []}

    Enum.reduce_while(events, {:ok, initial}, fn event, {:ok, prior} ->
      case apply_event(event, prior, root) do
        {:ok, next} -> {:cont, {:ok, next}}
        {:error, _} = result -> {:halt, result}
      end
    end)
  end

  def replay(_events, _root), do: error("transition history must be a list")

  @spec valid_edge?(String.t(), String.t()) :: boolean()
  def valid_edge?(from, to), do: to in Map.get(@edges, from, [])

  defp apply_event(event, prior, root) when is_map(event) do
    payload = Map.drop(event, ["digest", "signatures"])
    signatures = Map.get(event, "signatures", [])
    sequence = Map.get(event, "sequence")
    from = Map.get(event, "from")
    to = Map.get(event, "to")
    digest = Map.get(event, "digest")

    with true <- sequence == prior.sequence + 1,
         true <- Map.get(event, "prior_digest") == prior.digest,
         true <- from == prior.state and valid_edge?(from, to),
         true <- required_binding_fields?(event),
         true <- is_binary(digest) and digest == CanonicalJCS.digest(payload),
         :ok <- verify_event(root, payload, signatures, sequence) do
      {:ok, %{state: to, sequence: sequence, digest: digest, events: prior.events ++ [event]}}
    else
      {:error, reason} ->
        error(reason)

      _ ->
        error(
          "invalid lifecycle transition #{inspect(from)} -> #{inspect(to)} at sequence #{inspect(sequence)}"
        )
    end
  end

  defp apply_event(_event, _prior, _root), do: error("transition must be an object")

  defp verify_event(nil, _payload, _signatures, _sequence),
    do: {:error, "lifecycle transition requires an explicit trust root"}

  defp verify_event(root, payload, signatures, sequence) do
    case TrustRoot.at_sequence(root, sequence) do
      nil ->
        {:error, "no trust-root state exists for transition sequence #{sequence}"}

      historical_root ->
        case Crypto.verify_threshold(
               historical_root,
               "normal",
               "transition",
               payload,
               signatures,
               sequence
             ) do
          {:ok, _signers} -> :ok
          {:error, reason} -> {:error, "transition signature rejected: #{reason}"}
        end
    end
  end

  defp required_binding_fields?(event) do
    Enum.all?(
      ~w(action subject proposal_digest claim_digests artifact_digests policy_digest evidence approvals decision explanation),
      &Map.has_key?(event, &1)
    ) and is_list(event["claim_digests"]) and is_list(event["artifact_digests"]) and
      is_list(event["evidence"]) and is_list(event["approvals"]) and
      event["decision"] in ~w(allow deny) and is_map(event["explanation"]) and
      digest?(event["proposal_digest"]) and digest?(event["policy_digest"]) and
      Enum.all?(event["claim_digests"], &digest?/1) and
      Enum.all?(event["artifact_digests"], &digest?/1)
  end

  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)

  defp error(message), do: {:error, Diagnostic.new("GOV004", message, path: "$.transitions")}
end
