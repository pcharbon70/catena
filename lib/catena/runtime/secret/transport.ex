defmodule Catena.Runtime.Secret.Transport do
  @moduledoc false
  alias Catena.Runtime.Environment, as: E
  alias Catena.Runtime.Environment.Policy
  alias Catena.Foreign.{Descriptor, Codec, Adapter}
  @limits %{nodes: 10000, bytes: 131_072, depth: 100}

  def provider?(%Catena.Runtime.Secret.Input{value: value}),
    do: is_binary(value) and byte_size(value) <= 65_536

  def provider?(%{kind: :environment, grant: grant, name: name} = p),
    do:
      map_size(p) == 3 and Policy.verify(grant) == :ok and grant.service == :environment and
        Policy.authorize(grant, :get, name) == :ok

  def provider?(_), do: false

  def recipient?(%{kind: :environment, grant: grant, target: target, response_bytes: count} = r) do
    map_size(r) == 4 and Policy.verify(grant) == :ok and grant.service in [:process, :network] and
      is_integer(count) and count in 0..65_536 and
      case grant.service do
        :process ->
          Policy.authorize(grant, :run, {target, <<>>}) == :ok

        :network ->
          Policy.authorize(grant, :exchange, {target, <<>>, count}) == :ok and
            loopback?(elem(grant.policy.endpoints[target], 0))
      end
  end

  def recipient?(%{kind: :foreign, declaration: d} = r) do
    {:ok, bytes} = Codec.new({:data, :bytes})
    map_size(r) == 2 and Descriptor.verify(d) == :ok and d.arguments == [bytes]
  end

  def recipient?(_), do: false

  defp loopback?({127, _, _, _}), do: true
  defp loopback?({0, 0, 0, 0, 0, 0, 0, 1}), do: true
  defp loopback?(_), do: false

  def fetch(%{kind: :environment, grant: grant, name: name}) do
    case environment(grant, :get, name) do
      {:ok, {:catena_variant, :ok, {:catena_variant, :present, bytes}}} when is_binary(bytes) ->
        {:ok, bytes}

      _ ->
        {:error, :secret_provider_failed}
    end
  end

  def deliver(%{kind: :environment, grant: grant, target: target, response_bytes: count}, bytes)
      when is_binary(bytes) do
    {operation, input} =
      if grant.service == :process,
        do: {:run, {target, bytes}},
        else: {:exchange, {target, bytes, count}}

    environment(grant, operation, input)
  end

  def deliver(%{kind: :foreign, declaration: d}, bytes) when is_binary(bytes) do
    Adapter.run(
      [d],
      @limits,
      fn scope ->
        case Adapter.start(scope, d, [bytes]) do
          {:ok, handle} ->
            foreign_wait(scope, handle, System.monotonic_time(:millisecond) + 1000, false)

          _ ->
            {:error, :secret_delivery_failed}
        end
      end,
      sensitive: true
    )
  end

  def deliver(_, _), do: {:error, :secret_delivery_type}

  defp foreign_wait(scope, handle, deadline, cancelled) do
    cancelled =
      receive do
        :secret_cancel ->
          Adapter.cancel(scope, handle, :secret_cancelled)
          true
      after
        0 -> cancelled
      end

    case Adapter.await(scope, handle, 0) do
      {:ok, :pending} ->
        if System.monotonic_time(:millisecond) >= deadline do
          Adapter.cancel(scope, handle, :secret_timeout)
          {:error, :secret_delivery_timeout}
        else
          Process.sleep(5)
          foreign_wait(scope, handle, deadline, cancelled)
        end

      {:ok, {:completed, value}} when not cancelled ->
        {:ok, value}

      _ ->
        {:error, :secret_delivery_failed}
    end
  end

  defp environment(grant, operation, argument) do
    E.run(
      [grant],
      @limits,
      fn bundle ->
        case E.start(bundle.authorities[grant.service], operation, argument) do
          {:ok, {E, _, manager, token} = handle} ->
            request = :gen_server.send_request(manager, {:await, token})
            environment_wait(request, handle)

          {:answer, result} ->
            {:ok, result}

          _ ->
            {:error, :secret_delivery_failed}
        end
      end,
      timeout_ms: 1000,
      sensitive: true
    )
  end

  defp environment_wait(request, handle) do
    receive do
      :secret_cancel ->
        E.cancel(handle)
        environment_wait(request, handle)

      message ->
        case :gen_server.check_response(message, request) do
          {:reply, result} -> result
          {:error, _} -> {:error, :secret_delivery_failed}
          :no_reply -> environment_wait(request, handle)
        end
    after
      6500 ->
        E.cancel(handle)
        {:error, :secret_delivery_timeout}
    end
  end
end
