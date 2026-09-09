defmodule Catena.Runtime.Secret.Input do
  @moduledoc "Sensitive host provisioning input; never a public artifact field."
  @enforce_keys [:value]
  defstruct [:value]
end

defmodule Catena.Runtime.Secret.Ref do
  @moduledoc "Opaque live vault reference; contains no credential bytes."
  @enforce_keys [:owner, :manager, :token]
  defstruct [:owner, :manager, :token]
end

defimpl Inspect, for: [Catena.Runtime.Secret.Input, Catena.Runtime.Secret.Ref] do
  def inspect(_, _), do: "#CatenaSecret<redacted>"
end

defmodule Catena.Runtime.Secret do
  @moduledoc "Explicit credential capabilities with sealed transformations and recipient results."
  alias Catena.Runtime.Secret.{Input, Ref, Session}

  def profile,
    do: %{
      version: "0.1.72",
      max_setup_bytes: 1_048_576,
      max_providers: 64,
      max_recipients: 64,
      max_input_bytes: 65_536,
      max_value_bytes: 131_072,
      max_storage_bytes: 1_048_576,
      max_objects: 256,
      max_scopes: 64,
      max_jobs: 1024,
      max_inflight: 8,
      max_expression_depth: 16,
      max_name_bytes: 128,
      max_ttl_ms: 1_000_000,
      service_timeout_ms: 1000,
      worker_deadline_ms: 7000,
      release_ms: 8000,
      network: :explicit_loopback_broker,
      secure_erasure: false,
      hostile_host_secrecy: false
    }

  def conformance_profile do
    Map.new(profile(), fn {key, value} ->
      {Atom.to_string(key),
       if(is_atom(value) and value not in [true, false, nil],
         do: Atom.to_string(value),
         else: value
       )}
    end)
  end

  def input(bytes) when is_binary(bytes) and byte_size(bytes) <= 65_536,
    do: {:ok, %Input{value: bytes}}

  def input(_), do: {:error, :secret_input_limit}

  @context :__catena_secret_scope_context__
  def context?, do: Process.get(@context, false) == true

  def run(providers, recipients, body, options \\ []) when is_function(body, 1) do
    previous = Process.flag(:sensitive, true)
    previous_context = Process.get(@context, :absent)
    Process.put(@context, true)

    try do
      release = make_ref()

      case Session.start(self(), providers, recipients, release, options) do
        {:ok, pid} ->
          try do
            with {:ok, scope} <- call(pid, :scope, 5000) do
              Catena.Resource.Runtime.run(
                pid,
                fn pid ->
                  case call(pid, {:close, release}, 8000) do
                    :ok -> :unit
                    _ -> :erlang.error({:catena_trap, :secret_cleanup_unconfirmed})
                  end
                end,
                fn _, _ -> body.(scope) end,
                8_000_000_000
              )
            end
          after
            if Process.alive?(pid), do: Process.exit(pid, :kill)
          end

        _ ->
          {:error, :invalid_secret_setup}
      end
    catch
      :error, {:catena_trap, {:mandatory_release_failed, _}} ->
        :erlang.error({:catena_trap, {:mandatory_release_failed, :secret_cleanup_unconfirmed}})

      :throw, {:catena_resource_cancelled, _} ->
        throw({:catena_resource_cancelled, :redacted})

      :exit, _ ->
        exit(:secret_scope_failed)

      _, _ ->
        :erlang.error({:catena_trap, :secret_scope_failed})
    after
      if previous_context == :absent,
        do: Process.delete(@context),
        else: Process.put(@context, previous_context)

      Process.flag(:sensitive, previous)
    end
  end

  def fetch(scope, name) do
    case request(scope, {:fetch, name}) do
      {:ok, %Ref{} = value} -> {:ok, value}
      {:ok, job} -> await(job)
      error -> error
    end
  end

  def derive(scope, expression), do: request(scope, {:derive, expression})
  def start(scope, secret, recipient), do: request(scope, {:deliver, secret, recipient})

  def deliver(scope, secret, recipient) do
    with {:ok, job} <- start(scope, secret, recipient), do: await(job)
  end

  def attenuate(scope, providers, recipients, ttl_ms),
    do: request(scope, {:attenuate, providers, recipients, ttl_ms})

  def revoke(scope), do: request(scope, :revoke)
  def audit(scope), do: request(scope, :audit)

  def await({:catena_secret_job, owner, manager, token}) when owner == self(),
    do: call(manager, {:await, token}, 9000)

  def await(_), do: {:error, :invalid_secret_owner}

  def cancel({:catena_secret_job, owner, manager, token}) when owner == self(),
    do: call(manager, {:cancel, token}, 5000)

  def cancel(_), do: {:error, :invalid_secret_owner}

  defp request({:catena_secret_scope, owner, manager, _} = scope, command) when owner == self(),
    do: call(manager, {scope, command}, 5000)

  defp request(_, _), do: {:error, :invalid_secret_owner}

  defp call(manager, command, timeout) do
    GenServer.call(manager, command, timeout)
  catch
    :exit, _ -> {:error, :expired_secret_scope}
  end

  def redact_diagnostic(diagnostic) do
    if context?() do
      %{
        diagnostic
        | id: "SEC001",
          message: "Sensitive context diagnostic",
          path: nil,
          span: nil,
          severity: :error,
          details: %{},
          fixes: []
      }
    else
      redact(diagnostic)
    end
  end

  def redact(%{__struct__: type}) when type in [Input, Ref], do: "[secret]"

  def redact(value) when is_map(value),
    do: Map.new(Map.to_list(value), fn {k, v} -> {redact(k), redact(v)} end)

  def redact(value) when is_list(value), do: Enum.map(value, &redact/1)

  def redact(value) when is_tuple(value) do
    if tuple_size(value) > 0 and elem(value, 0) in [:catena_secret_scope, :catena_secret_job],
      do: "[secret-capability]",
      else: value |> Tuple.to_list() |> Enum.map(&redact/1) |> List.to_tuple()
  end

  def redact(value), do: value

  def artifact_input(value) do
    if sensitive?(value), do: {:error, :sensitive_artifact_input}, else: {:ok, value}
  end

  def sensitive?(%{__struct__: type}) when type in [Input, Ref], do: true

  def sensitive?(value) when is_map(value),
    do: Enum.any?(Map.to_list(value), fn {k, v} -> sensitive?(k) or sensitive?(v) end)

  def sensitive?(value) when is_list(value), do: Enum.any?(value, &sensitive?/1)

  def sensitive?(value) when is_tuple(value) do
    (tuple_size(value) > 0 and elem(value, 0) in [:catena_secret_scope, :catena_secret_job]) or
      Enum.any?(Tuple.to_list(value), &sensitive?/1)
  end

  def sensitive?(_), do: false
end
