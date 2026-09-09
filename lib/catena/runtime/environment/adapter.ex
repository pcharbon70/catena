defmodule Catena.Runtime.Environment.Adapter do
  @moduledoc "Real explicitly granted services; owned callers provide cancellation and deadlines."
  alias Catena.Runtime.Environment.Schema
  @external_resource "priv/runtime/environment_helper.py"
  @helper File.read!(@external_resource)
  def helper_digest, do: :crypto.hash(:sha256, @helper) |> Base.encode16(case: :lower)

  def perform(:filesystem, operation, {path, arg}, policy, timeout) do
    request = %{
      service: "filesystem",
      operation: Atom.to_string(operation),
      root: policy.root,
      path: path
    }

    request =
      if operation == :read,
        do: Map.put(request, :count, arg),
        else: Map.put(request, :data, Base.encode64(arg))

    helper(request, timeout)
  end

  def perform(:process, :run, {name, data}, policy, timeout) do
    helper(
      %{
        service: "process",
        command: policy.commands[name],
        data: Base.encode64(data),
        timeout_ms: timeout,
        max_bytes: policy.max_bytes
      },
      timeout
    )
  end

  def perform(:io, :read, count, policy, _) do
    case IO.binread(policy.device, count) do
      :eof -> Schema.success(<<>>)
      bytes when is_binary(bytes) -> Schema.success(bytes)
      _ -> Schema.failure(:io_failure)
    end
  end

  def perform(:io, :write, bytes, policy, _) do
    case IO.binwrite(policy.device, bytes) do
      :ok -> Schema.success(:unit)
      _ -> Schema.failure(:io_failure)
    end
  end

  def perform(:logging, :emit, {level, message}, policy, _) do
    payload = JSON.encode!(%{"level" => level, "message" => message}) <> "\n"

    if byte_size(payload) > policy.max_bytes do
      Schema.failure(:limit)
    else
      case IO.binwrite(policy.device, payload) do
        :ok -> Schema.success(:unit)
        _ -> Schema.failure(:io_failure)
      end
    end
  end

  def perform(:environment, :get, name, policy, _) do
    case System.get_env(name) do
      nil ->
        Schema.success({:catena_variant, :absent, :unit})

      value ->
        if byte_size(value) <= policy.max_bytes,
          do: Schema.success({:catena_variant, :present, value}),
          else: Schema.failure(:limit)
    end
  end

  def perform(:random, :bytes, count, _, _), do: Schema.success(:crypto.strong_rand_bytes(count))

  def perform(:time, :monotonic, :unit, _, _),
    do: Schema.success(System.monotonic_time(:nanosecond))

  def perform(:time, :wall, :unit, _, _), do: Schema.success(System.system_time(:nanosecond))

  def perform(:time, :sleep, milliseconds, _, _) do
    receive do
      :environment_cancel -> Schema.failure(:cancelled)
    after
      milliseconds -> Schema.success(:unit)
    end
  end

  def perform(:network, :exchange, {name, bytes, count}, policy, timeout) do
    {ip, port} = policy.endpoints[name]
    options = [:binary, active: false, packet: 0, send_timeout: timeout, send_timeout_close: true]
    options = if tuple_size(ip) == 8, do: [:inet6 | options], else: options

    case :gen_tcp.connect(ip, port, options, timeout) do
      {:ok, socket} ->
        try do
          with :ok <- :gen_tcp.send(socket, bytes) do
            if count == 0 do
              Schema.success(<<>>)
            else
              case :gen_tcp.recv(socket, count, timeout) do
                {:ok, data} -> Schema.success(data)
                {:error, :timeout} -> Schema.failure(:timeout)
                _ -> Schema.failure(:network_failure)
              end
            end
          else
            _ -> Schema.failure(:network_failure)
          end
        after
          :gen_tcp.close(socket)
        end

      {:error, :timeout} ->
        Schema.failure(:timeout)

      _ ->
        Schema.failure(:network_failure)
    end
  end

  defp helper(request, timeout) do
    python = System.find_executable("python3")

    if is_nil(python) do
      Schema.failure(:unavailable)
    else
      port =
        Port.open({:spawn_executable, python}, [
          :binary,
          :exit_status,
          {:packet, 4},
          {:args, ["-I", "-u", "-c", @helper]}
        ])

      try do
        true = Port.command(port, JSON.encode!(request))
        helper_receive(port, timeout + 1000, false, request.service)
      after
        if Port.info(port) != nil, do: Port.close(port)
      end
    end
  end

  defp helper_receive(port, timeout, cancelled, service) do
    receive do
      {^port, {:data, data}} ->
        if cancelled, do: Schema.failure(:cancelled), else: decode_helper(data, service)

      {^port, {:exit_status, _}} ->
        throw(:unconfirmed_environment_cleanup)

      :environment_cancel ->
        Port.command(port, "{}")
        helper_receive(port, 1000, true, service)
    after
      timeout -> throw(:unconfirmed_environment_cleanup)
    end
  end

  defp decode_helper(data, service) do
    case JSON.decode(data) do
      {:ok, %{"cleanup_unconfirmed" => true}} ->
        throw(:unconfirmed_environment_cleanup)

      {:ok, %{"status" => status, "bytes" => bytes}} ->
        Schema.success({status, Base.decode64!(bytes)})

      {:ok, %{"bytes" => bytes}} ->
        Schema.success(Base.decode64!(bytes))

      {:ok, %{"unit" => true}} ->
        Schema.success(:unit)

      {:ok, %{"error" => reason}} ->
        errors = %{
          "denied" => :denied,
          "not_found" => :not_found,
          "timeout" => :timeout,
          "cancelled" => :cancelled,
          "limit" => :limit,
          "unavailable" => :unavailable,
          "invalid_request" => :invalid_request,
          "io_failure" => :io_failure
        }

        reason = Map.get(errors, reason, :io_failure)

        reason =
          if service == "process" and reason == :io_failure, do: :process_failure, else: reason

        Schema.failure(reason)

      _ ->
        Schema.failure(:io_failure)
    end
  end
end
