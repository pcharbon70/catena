defmodule Catena.Distribution.Contract do
  @moduledoc "Authenticated remote-service identity bound to one checked local protocol."

  alias Catena.{CanonicalJSON, Protocol.Contract}

  @version "0.1.76"
  @max_peers 64
  @max_identity_bytes 128

  def profile do
    %{
      version: @version,
      transport: :tls_1_3,
      peer_verification: :certificate_sha256,
      application_framing: :canonical_json_packet_4,
      max_peers: @max_peers,
      max_identity_bytes: @max_identity_bytes,
      automatic_retry: false,
      exactly_once: false
    }
  end

  def new(local, options) when is_list(options) do
    node = Keyword.get(options, :node)
    service = Keyword.get(options, :service)
    package_digest = Keyword.get(options, :package_digest)
    peers = Keyword.get(options, :peers)

    with [] <- Keyword.keys(options) -- [:node, :service, :package_digest, :peers],
         :ok <- Contract.validate(local),
         true <- identity?(node) and identity?(service),
         true <- digest?(package_digest),
         {:ok, peers} <- peers(peers) do
      public = %{
        "format" => "catena-distribution-contract",
        "version" => @version,
        "node" => node,
        "service" => service,
        "package_digest" => package_digest,
        "protocol_identity" => local.identity,
        "protocol_version" => local.version,
        "protocol_digest" => local.digest,
        "peers" =>
          Map.new(peers, fn {id, peer} ->
            {id,
             %{
               "certificate_sha256" => peer.certificate_sha256,
               "services" => peer.services,
               "package_digests" => peer.package_digests
             }}
          end)
      }

      endpoint = %{
        "format" => "catena-distribution-endpoint",
        "version" => @version,
        "node" => node,
        "service" => service,
        "package_digest" => package_digest,
        "protocol_digest" => local.digest
      }

      {:ok,
       %{
         format: :catena_distribution_contract,
         version: @version,
         local: local,
         node: node,
         service: service,
         package_digest: package_digest,
         peers: peers,
         endpoint_digest: hash(CanonicalJSON.encode(endpoint)),
         digest: hash(CanonicalJSON.encode(public))
       }}
    else
      _ -> {:error, :invalid_distribution_contract}
    end
  rescue
    _ -> {:error, :invalid_distribution_contract}
  end

  def new(_, _), do: {:error, :invalid_distribution_contract}

  def validate(%{format: :catena_distribution_contract} = contract) do
    options = [
      node: contract.node,
      service: contract.service,
      package_digest: contract.package_digest,
      peers: contract.peers
    ]

    case new(contract.local, options) do
      {:ok, ^contract} -> :ok
      _ -> {:error, :invalid_distribution_contract}
    end
  end

  def validate(_), do: {:error, :invalid_distribution_contract}

  def authorize(contract, peer_node, peer_service, package_digest, certificate) do
    with :ok <- validate(contract),
         %{certificate_sha256: expected, services: services, package_digests: packages} <-
           contract.peers[peer_node],
         true <- peer_service in services,
         true <- package_digest in packages,
         true <- hash(certificate) == expected do
      :ok
    else
      _ -> {:error, :unauthorized_peer}
    end
  end

  def endpoint_digest(version, node, service, package_digest, protocol_digest) do
    endpoint = %{
      "format" => "catena-distribution-endpoint",
      "version" => version,
      "node" => node,
      "service" => service,
      "package_digest" => package_digest,
      "protocol_digest" => protocol_digest
    }

    hash(CanonicalJSON.encode(endpoint))
  end

  defp peers(peers) when is_map(peers) and map_size(peers) in 1..@max_peers do
    if Enum.all?(peers, fn {id, peer} -> identity?(id) and peer?(peer) end) do
      {:ok,
       Map.new(peers, fn {id, peer} ->
         {id,
          %{
            certificate_sha256: peer[:certificate_sha256],
            services: Enum.sort(peer[:services]),
            package_digests: Enum.sort(peer[:package_digests])
          }}
       end)}
    else
      :error
    end
  end

  defp peers(_), do: :error

  defp peer?(peer) when is_map(peer) do
    Map.keys(peer) |> Enum.sort() == [:certificate_sha256, :package_digests, :services] and
      digest?(peer[:certificate_sha256]) and identities?(peer[:services]) and
      digests?(peer[:package_digests])
  end

  defp peer?(_), do: false

  defp identities?(values),
    do:
      is_list(values) and values != [] and values == Enum.uniq(values) and
        Enum.all?(values, &identity?/1)

  defp digests?(values),
    do:
      is_list(values) and values != [] and values == Enum.uniq(values) and
        Enum.all?(values, &digest?/1)

  defp identity?(value),
    do:
      is_binary(value) and byte_size(value) in 1..@max_identity_bytes and String.valid?(value) and
        Regex.match?(~r/^[a-z][a-z0-9]*(?:[._-][a-z0-9]+)*$/, value)

  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)
  def hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
