defmodule Catena.Runtime.Environment do
  @moduledoc "Explicit scoped environmental authority; no ambient service resolution."
  alias Catena.Runtime.Environment.{Session, Authority}
  alias Catena.Resource.Runtime

  def deliver_secret(scope, reference, recipient),
    do: Catena.Runtime.Secret.deliver(scope, reference, recipient)

  def profile,
    do: %{
      version: "0.1.68",
      services: Catena.Runtime.Environment.Schema.services(),
      max_grants: 8,
      max_authorities: 64,
      max_service_bytes: 1_048_576,
      max_ttl_ms: 1_000_000,
      default_requests: 1024,
      max_requests: 10000,
      default_inflight: 8,
      max_inflight: 64,
      default_timeout_ms: 1000,
      max_timeout_ms: 1_000_000,
      cooperative_grace_ms: 1500,
      scope_release_ms: 5000,
      helper: :posix_python,
      executable_snapshot_bytes: 16_777_216,
      helper_digest: Catena.Runtime.Environment.Adapter.helper_digest()
    }

  def run(grants, limits, body, options \\ []) when is_function(body, 1) do
    owner = self()
    release = make_ref()

    with {:ok, manager} <- Session.start(owner, grants, limits, release, options),
         {:ok, bundle} <- GenServer.call(manager, {:bundle, owner}) do
      try do
        Runtime.run(
          {manager, release},
          fn {manager, release} ->
            case GenServer.call(manager, {:close, release}, 5000) do
              :ok -> :unit
              error -> :erlang.error({:catena_trap, {:environment_cleanup_failed, error}})
            end
          end,
          fn _, _ -> body.(bundle) end,
          5_000_000_000
        )
      after
        if Process.alive?(manager), do: Process.exit(manager, :kill)
      end
    end
  end

  def start(authority, operation, argument), do: request(authority, {:start, operation, argument})

  def call(authority, operation, argument) do
    case start(authority, operation, argument) do
      {:ok, handle} -> await(handle)
      {:answer, result} -> {:ok, result}
      error -> error
    end
  end

  def attenuate(authority, policy), do: request(authority, {:attenuate, policy})
  def revoke(authority), do: request(authority, :revoke)
  def validate(authority, service), do: request(authority, {:validate, service})

  def events(%{format: :environment_bundle, version: "0.1.68", manager: manager}),
    do: manager_call(manager, :events, 5000)

  def await({__MODULE__, owner, manager, reference}) when owner == self(),
    do: manager_call(manager, {:await, reference}, 1_005_000)

  def await(_), do: {:error, :invalid_environment_request_owner}

  def cancel({__MODULE__, owner, manager, reference}) when owner == self(),
    do: manager_call(manager, {:cancel, reference}, 5000)

  def cancel(_), do: {:error, :invalid_environment_request_owner}

  defp manager_call(manager, command, timeout) do
    GenServer.call(manager, command, timeout)
  catch
    :exit, _ -> {:error, :expired_environment_scope}
  end

  defp request(authority, command) do
    with {:ok, manager, _, _} <- Authority.decode(authority),
         do: GenServer.call(manager, {:authority, authority, command}, 5000)
  catch
    :exit, _ -> {:error, :expired_environment_authority}
  end
end

defmodule Catena.Runtime.Environment.Authority do
  @moduledoc false
  @identity :"catena://environment/0.1.68::Authority"
  def issue(service, manager, owner, nonce) do
    payload = :erlang.term_to_binary({manager, owner, nonce}, [:deterministic])
    {:catena_adt, @identity, 0, {Atom.to_string(service), payload}}
  end

  def decode({:catena_adt, @identity, 0, {service, <<131, tag, _::binary>> = payload}})
      when is_binary(service) and byte_size(payload) <= 1024 and tag != 80 do
    case :erlang.binary_to_term(payload, [:safe]) do
      {manager, owner, nonce}
      when is_pid(manager) and is_pid(owner) and is_binary(nonce) and byte_size(nonce) == 32 ->
        {:ok, manager, owner, nonce}

      _ ->
        {:error, :invalid_environment_authority}
    end
  rescue
    _ -> {:error, :invalid_environment_authority}
  end

  def decode(_), do: {:error, :invalid_environment_authority}
end
