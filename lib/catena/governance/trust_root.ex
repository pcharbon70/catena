defmodule Catena.Governance.TrustRoot do
  @moduledoc "Canonical offline trust-root decoding and hash-chained versioned rotation."

  alias Catena.{CanonicalJCS, Diagnostic, LanguageVersion}
  alias Catena.Governance.Crypto

  @hex_key ~r/^[0-9a-f]{64}$/
  @hex_digest ~r/^[0-9a-f]{64}$/
  @legacy_version LanguageVersion.introduced(:specifications_and_governance)
  @edition_version LanguageVersion.introduced(:editions_and_feature_lifecycle)
  @versions LanguageVersion.from(:specifications_and_governance)

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(binary) when is_binary(binary) do
    with {:ok, value} <- CanonicalJCS.decode(binary, canonical: true),
         "catena-trust-root" <- Map.get(value, "format"),
         version when version in @versions <- Map.get(value, "version"),
         namespace when is_binary(namespace) and byte_size(namespace) > 0 <-
           Map.get(value, "namespace"),
         initial when is_map(initial) <- Map.get(value, "initial"),
         history when is_list(history) <- Map.get(value, "history", []),
         {:ok, root} <- decode_state(initial, namespace, version),
         root <- Map.put(root, :states, %{root.sequence => snapshot(root)}),
         {:ok, current} <- replay(history, root) do
      {:ok, Map.put(current, :history, history)}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      _ -> error("malformed or unsupported catena-trust-root document")
    end
  end

  @spec replay([map()], map()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def replay(history, initial) do
    Enum.reduce_while(history, {:ok, initial}, fn event, {:ok, prior} ->
      case apply_event(event, prior) do
        {:ok, next} -> {:cont, {:ok, next}}
        {:error, _} = result -> {:halt, result}
      end
    end)
  end

  @spec state_payload(map()) :: map()
  def state_payload(root) do
    payload = %{
      "sequence" => root.sequence,
      "principals" =>
        root.principals
        |> Enum.map(fn {id, principal} ->
          %{"id" => id, "public_key" => principal.public_key}
        end)
        |> Enum.sort_by(& &1["id"]),
      "roles" =>
        Map.new(root.roles, fn {name, role} ->
          {name, %{"principals" => Enum.sort(role.principals), "threshold" => role.threshold}}
        end),
      "delegations" =>
        root.delegations
        |> Enum.map(fn delegation ->
          %{
            "id" => delegation.id,
            "principal" => delegation.principal,
            "role" => delegation.role,
            "from" => delegation.from,
            "to" => delegation.to,
            "actions" => delegation.actions,
            "subjects" => delegation.subjects,
            "profiles" => delegation.profiles
          }
        end)
        |> Enum.sort_by(& &1["id"]),
      "revocations" => %{
        "principals" => Enum.sort(root.revocations.principals),
        "delegations" => Enum.sort(root.revocations.delegations),
        "evidence" => Enum.sort(root.revocations.evidence)
      }
    }

    case Map.get(root, :version, @legacy_version) do
      @edition_version -> Map.put(payload, "format_version", @edition_version)
      @legacy_version -> payload
    end
  end

  @spec at_sequence(map(), pos_integer()) :: map() | nil
  def at_sequence(root, sequence) do
    states = Map.get(root, :states, %{root.sequence => root})

    states
    |> Map.keys()
    |> Enum.filter(&(&1 <= sequence))
    |> Enum.max(fn -> nil end)
    |> case do
      nil -> nil
      selected -> Map.fetch!(states, selected)
    end
  end

  defp apply_event(event, prior) when is_map(event) do
    payload = Map.drop(event, ["signatures", "new_signatures", "digest"])
    sequence = Map.get(event, "sequence")
    mode = Map.get(event, "mode")
    next_value = Map.get(event, "root")
    signatures = Map.get(event, "signatures", [])
    new_signatures = Map.get(event, "new_signatures", [])

    with true <- mode in ~w(normal recovery),
         true <- is_integer(sequence) and sequence == prior.sequence + 1,
         true <- Map.get(event, "prior_digest") == prior.digest,
         digest when is_binary(digest) <- Map.get(event, "digest"),
         true <- Regex.match?(@hex_digest, digest) and digest == CanonicalJCS.digest(payload),
         true <- is_map(next_value),
         {:ok, next} <- decode_state(next_value, prior.namespace, prior.version),
         true <- next.sequence == sequence,
         :ok <- verify_rotation(mode, prior, next, payload, signatures, new_signatures, sequence) do
      states = Map.put(Map.get(prior, :states, %{}), next.sequence, snapshot(next))
      {:ok, next |> Map.put(:history_digest, digest) |> Map.put(:states, states)}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      {:error, reason} -> error(reason)
      _ -> error("invalid trust-root transition at logical sequence #{inspect(sequence)}")
    end
  end

  defp apply_event(_event, _prior), do: error("trust-root history event must be an object")

  defp verify_rotation("normal", prior, next, payload, old_signatures, new_signatures, sequence) do
    with {:ok, _old} <-
           Crypto.verify_threshold(prior, "normal", "root", payload, old_signatures, sequence),
         {:ok, _new} <-
           Crypto.verify_threshold(next, "normal", "root", payload, new_signatures, sequence) do
      :ok
    else
      {:error, reason} -> {:error, "normal root rotation rejected: #{reason}"}
    end
  end

  defp verify_rotation("recovery", prior, _next, payload, signatures, _new, sequence) do
    case Crypto.verify_threshold(prior, "recovery", "root", payload, signatures, sequence) do
      {:ok, _signers} -> :ok
      {:error, reason} -> {:error, "root recovery rejected: #{reason}"}
    end
  end

  defp decode_state(value, namespace, version) do
    principals = Map.get(value, "principals")
    roles = Map.get(value, "roles")
    delegations = Map.get(value, "delegations", [])
    revocations = Map.get(value, "revocations", %{})
    sequence = Map.get(value, "sequence")

    with true <- is_integer(sequence) and sequence > 0,
         {:ok, principals} <- decode_principals(principals),
         {:ok, roles} <- decode_roles(roles, principals),
         true <- Map.has_key?(roles, "normal") and Map.has_key?(roles, "recovery"),
         {:ok, delegations} <- decode_delegations(delegations, principals, roles),
         {:ok, revocations} <- decode_revocations(revocations, principals, delegations) do
      root = %{
        version: version,
        namespace: namespace,
        sequence: sequence,
        principals: principals,
        roles: roles,
        delegations: delegations,
        revocations: revocations,
        history_digest: nil
      }

      {:ok, Map.put(root, :digest, CanonicalJCS.digest(state_payload(root)))}
    else
      {:error, reason} -> error(reason)
      _ -> error("malformed trust-root state")
    end
  end

  defp decode_principals(values) when is_list(values) and values != [] do
    if Enum.all?(values, fn value ->
         is_map(value) and is_binary(value["id"]) and is_binary(value["public_key"]) and
           Regex.match?(@hex_key, value["public_key"])
       end) and length(values) == length(Enum.uniq_by(values, & &1["id"])) do
      {:ok,
       Map.new(values, fn value ->
         {value["id"], %{id: value["id"], public_key: value["public_key"]}}
       end)}
    else
      {:error, "principals require unique IDs and lowercase Ed25519 public keys"}
    end
  end

  defp decode_principals(_values), do: {:error, "trust root requires principals"}

  defp decode_roles(values, principals) when is_map(values) and map_size(values) > 0 do
    result =
      Enum.reduce_while(values, {:ok, %{}}, fn {name, value}, {:ok, decoded} ->
        members = if is_map(value), do: Map.get(value, "principals"), else: nil
        threshold = if is_map(value), do: Map.get(value, "threshold"), else: nil

        if is_binary(name) and is_list(members) and members != [] and
             Enum.all?(members, &Map.has_key?(principals, &1)) and members == Enum.uniq(members) and
             is_integer(threshold) and threshold > 0 and threshold <= length(members) do
          {:cont, {:ok, Map.put(decoded, name, %{principals: members, threshold: threshold})}}
        else
          {:halt, {:error, "role #{inspect(name)} has invalid membership or threshold"}}
        end
      end)

    result
  end

  defp decode_roles(_values, _principals), do: {:error, "trust root requires roles"}

  defp decode_delegations(values, principals, roles) when is_list(values) do
    valid? = fn value ->
      is_map(value) and is_binary(value["id"]) and Map.has_key?(principals, value["principal"]) and
        Map.has_key?(roles, value["role"]) and is_integer(value["from"]) and
        is_integer(value["to"]) and value["from"] > 0 and value["from"] <= value["to"] and
        string_list?(Map.get(value, "actions", [])) and
        string_list?(Map.get(value, "subjects", [])) and
        string_list?(Map.get(value, "profiles", []))
    end

    if Enum.all?(values, valid?) and length(values) == length(Enum.uniq_by(values, & &1["id"])) do
      {:ok,
       Enum.map(values, fn value ->
         %{
           id: value["id"],
           principal: value["principal"],
           role: value["role"],
           from: value["from"],
           to: value["to"],
           actions: Map.get(value, "actions", []),
           subjects: Map.get(value, "subjects", []),
           profiles: Map.get(value, "profiles", [])
         }
       end)}
    else
      {:error, "delegations must be unique, scoped, and sequence-bounded"}
    end
  end

  defp decode_delegations(_values, _principals, _roles),
    do: {:error, "delegations must be a list"}

  defp decode_revocations(value, principals, delegations) when is_map(value) do
    principal_ids = Map.get(value, "principals", [])
    delegation_ids = Map.get(value, "delegations", [])
    evidence_ids = Map.get(value, "evidence", [])
    known_delegations = MapSet.new(delegations, & &1.id)

    if string_list?(principal_ids) and Enum.all?(principal_ids, &Map.has_key?(principals, &1)) and
         string_list?(delegation_ids) and
         Enum.all?(delegation_ids, &MapSet.member?(known_delegations, &1)) and
         string_list?(evidence_ids) do
      {:ok, %{principals: principal_ids, delegations: delegation_ids, evidence: evidence_ids}}
    else
      {:error, "revocations must name known unique principals and delegations"}
    end
  end

  defp decode_revocations(_value, _principals, _delegations),
    do: {:error, "revocations must be an object"}

  defp string_list?(values),
    do: is_list(values) and Enum.all?(values, &is_binary/1) and values == Enum.uniq(values)

  defp snapshot(root), do: Map.drop(root, [:states, :history])

  defp error(message), do: {:error, Diagnostic.new("GOV005", message, path: "$")}
end
