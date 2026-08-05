defmodule Catena.Governance.Crypto do
  @moduledoc "Offline Ed25519 verification for versioned Catena governance records."

  alias Catena.{CanonicalJCS, LanguageVersion}

  @legacy_version LanguageVersion.introduced(:specifications_and_governance)

  @hex ~r/^[0-9a-f]+$/

  @spec verify(binary(), String.t(), String.t()) :: boolean()
  def verify(message, public_key, signature)
      when is_binary(message) and is_binary(public_key) and is_binary(signature) do
    with true <- byte_size(public_key) == 64 and Regex.match?(@hex, public_key),
         true <- byte_size(signature) == 128 and Regex.match?(@hex, signature),
         {:ok, key} <- Base.decode16(public_key, case: :lower),
         {:ok, signed} <- Base.decode16(signature, case: :lower) do
      :crypto.verify(:eddsa, :none, message, signed, [key, :ed25519])
    else
      _ -> false
    end
  rescue
    _error -> false
  end

  @spec verify_threshold(map(), String.t(), String.t(), term(), [map()], pos_integer(), map()) ::
          {:ok, [String.t()]} | {:error, String.t()}
  def verify_threshold(root, role, kind, payload, signatures, sequence, scope \\ %{}) do
    with %{threshold: threshold} <- get_in(root, [:roles, role]),
         {:ok, signers} <-
           valid_signers_for_payload(root, role, kind, payload, signatures, sequence, scope),
         true <- length(signers) >= threshold do
      {:ok, signers}
    else
      nil -> {:error, "unknown role #{role}"}
      false -> {:error, "role #{role} does not meet its distinct-signature threshold"}
      {:error, _} = result -> result
    end
  end

  @spec valid_signers(map(), String.t(), String.t(), term(), [map()], pos_integer(), map()) ::
          {:ok, [String.t()]} | {:error, String.t()}
  def valid_signers(root, role, kind, payload, signatures, sequence, scope \\ %{}) do
    if is_nil(get_in(root, [:roles, role])) do
      {:error, "unknown role #{role}"}
    else
      with {:ok, audit} <- signer_audit(root, role, kind, payload, signatures, sequence, scope) do
        {:ok, audit.valid}
      end
    end
  end

  @spec signer_audit(map(), String.t(), String.t(), term(), [map()], pos_integer(), map()) ::
          {:ok, map()} | {:error, String.t()}
  def signer_audit(root, role, kind, payload, signatures, sequence, scope \\ %{}) do
    if is_nil(get_in(root, [:roles, role])) or not is_list(signatures) do
      {:error, "unknown role #{role} or malformed signatures"}
    else
      {:ok, audit_signatures(root, role, kind, payload, signatures, sequence, scope)}
    end
  end

  defp valid_signers_for_payload(root, role, kind, payload, signatures, sequence, scope) do
    with {:ok, audit} <- signer_audit(root, role, kind, payload, signatures, sequence, scope) do
      {:ok, audit.valid}
    end
  end

  defp audit_signatures(root, role, kind, payload, signatures, sequence, scope) do
    revoked = MapSet.new(root.revocations.principals)
    message = CanonicalJCS.payload(kind, format_version(root), payload)

    groups = Enum.group_by(signatures, &Map.get(&1, "principal"))

    audit =
      Enum.reduce(groups, %{valid: [], invalid: 0, revoked: 0}, fn {principal, records}, audit ->
        key = get_in(root, [:principals, principal, :public_key])

        cond do
          is_binary(principal) and MapSet.member?(revoked, principal) ->
            %{audit | revoked: audit.revoked + 1}

          not is_binary(principal) or not is_binary(key) or
              not delegated_at?(root, principal, role, sequence, scope) ->
            %{audit | invalid: audit.invalid + 1}

          Enum.any?(records, &verify(message, key, Map.get(&1, "signature"))) ->
            %{audit | valid: [principal | audit.valid]}

          true ->
            %{audit | invalid: audit.invalid + 1}
        end
      end)

    %{
      valid: Enum.sort(audit.valid),
      invalid: audit.invalid,
      revoked: audit.revoked,
      duplicate: length(signatures) - map_size(groups)
    }
  end

  defp delegated_at?(root, principal, role, sequence, scope) do
    direct? = principal in get_in(root, [:roles, role, :principals])

    direct? or
      Enum.any?(root.delegations, fn delegation ->
        delegation.principal == principal and delegation.role == role and
          sequence >= delegation.from and sequence <= delegation.to and
          delegation.id not in root.revocations.delegations and
          scope_match?(delegation.actions, Map.get(scope, :action)) and
          scope_match?(delegation.subjects, Map.get(scope, :subject)) and
          scope_match?(delegation.profiles, Map.get(scope, :profile))
      end)
  end

  defp scope_match?([], _value), do: true
  defp scope_match?(_allowed, nil), do: false
  defp scope_match?(allowed, value), do: value in allowed

  defp format_version(root), do: Map.get(root, :version, @legacy_version)
end
