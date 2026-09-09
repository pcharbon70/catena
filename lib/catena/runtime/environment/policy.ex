defmodule Catena.Runtime.Environment.Policy do
  @moduledoc "Explicit host grants and structural attenuation. Declarations never grant authority."
  alias Catena.Runtime.Environment.Schema
  @common [:operations, :max_bytes, :ttl_ms]
  def new(service, policy) when is_map(policy) do
    with true <- service in Schema.services(),
         true <- exact_fields?(service, policy),
         true <- is_list(policy.operations) and policy.operations == Enum.uniq(policy.operations),
         true <- Enum.all?(policy.operations, &Map.has_key?(Schema.operations(service), &1)),
         true <- is_integer(policy.max_bytes) and policy.max_bytes in 0..1_048_576,
         true <- is_integer(policy.ttl_ms) and policy.ttl_ms in 1..1_000_000,
         true <- valid_resources?(service, policy) do
      {:ok, %{version: "0.1.68", service: service, policy: policy}}
    else
      _ -> {:error, :invalid_environment_grant}
    end
  rescue
    _ -> {:error, :invalid_environment_grant}
  end

  def new(_, _), do: {:error, :invalid_environment_grant}

  def verify(grant) do
    case new(grant.service, grant.policy) do
      {:ok, ^grant} -> :ok
      _ -> {:error, :invalid_environment_grant}
    end
  rescue
    _ -> {:error, :invalid_environment_grant}
  end

  def attenuate(grant, requested) do
    with :ok <- verify(grant),
         {:ok, narrowed} <- new(grant.service, requested),
         true <- subset?(requested.operations, grant.policy.operations),
         true <-
           requested.max_bytes <= grant.policy.max_bytes and
             requested.ttl_ms <= grant.policy.ttl_ms,
         true <- resource_subset?(grant.service, requested, grant.policy) do
      {:ok, narrowed}
    else
      _ -> {:error, :authority_escalation}
    end
  end

  def authorize(grant, operation, value) do
    p = grant.policy

    if operation in p.operations and permitted?(grant.service, operation, value, p),
      do: :ok,
      else: {:error, :denied}
  rescue
    _ -> {:error, :denied}
  end

  def result_valid?(service, operation, argument, {:catena_variant, :ok, payload}, policy) do
    case {service, operation} do
      {:random, :bytes} ->
        byte_size(payload) == argument

      {:network, :exchange} ->
        byte_size(payload) == elem(argument, 2)

      {:io, :read} ->
        byte_size(payload) <= argument

      {:filesystem, :read} ->
        byte_size(payload) <= elem(argument, 1)

      {:environment, :get} ->
        case payload do
          {:catena_variant, :absent, :unit} -> true
          {:catena_variant, :present, bytes} -> byte_size(bytes) <= policy.max_bytes
        end

      {:process, :run} ->
        byte_size(elem(payload, 1)) <= policy.max_bytes

      _ ->
        true
    end
  end

  def result_valid?(_, _, _, {:catena_variant, :error, _}, _), do: true

  defp exact_fields?(service, p) do
    extra =
      case service do
        :io -> [:device]
        :filesystem -> [:root, :paths]
        :network -> [:endpoints]
        :time -> []
        :random -> []
        :environment -> [:names]
        :logging -> [:device, :levels]
        :process -> [:commands]
      end

    Enum.sort(Map.keys(p)) == Enum.sort(@common ++ extra)
  end

  defp valid_resources?(:io, p), do: is_pid(p.device)

  defp valid_resources?(:filesystem, p),
    do: absolute?(p.root) and strings?(p.paths) and Enum.all?(p.paths, &relative?/1)

  defp valid_resources?(:network, p),
    do:
      is_map(p.endpoints) and map_size(p.endpoints) <= 64 and
        Enum.all?(p.endpoints, fn {name, endpoint} -> text?(name) and endpoint?(endpoint) end)

  defp valid_resources?(service, _) when service in [:time, :random], do: true

  defp valid_resources?(:environment, p),
    do: strings?(p.names) and Enum.all?(p.names, &(not String.contains?(&1, "=")))

  defp valid_resources?(:logging, p), do: is_pid(p.device) and strings?(p.levels)

  defp valid_resources?(:process, p),
    do:
      is_map(p.commands) and map_size(p.commands) <= 32 and
        Enum.all?(p.commands, fn {name, command} -> text?(name) and command?(command) end)

  defp command?(%{executable: exe, arguments: args, environment: env, cwd: cwd} = command),
    do:
      map_size(command) == 4 and absolute?(exe) and absolute?(cwd) and arguments?(args) and
        is_map(env) and map_size(env) <= 256 and
        Enum.all?(env, fn {k, v} ->
          text?(k) and not String.contains?(k, "=") and is_binary(v) and byte_size(v) <= 4096 and
            String.valid?(v) and
            not String.contains?(v, <<0>>)
        end)

  defp command?(_), do: false

  defp endpoint?({ip, port}) when is_tuple(ip) and is_integer(port) and port in 1..65535 do
    size = tuple_size(ip)

    size in [4, 8] and
      Enum.all?(
        Tuple.to_list(ip),
        &(is_integer(&1) and &1 >= 0 and &1 <= if(size == 4, do: 255, else: 65535))
      )
  end

  defp endpoint?(_), do: false

  defp text?(v),
    do:
      is_binary(v) and byte_size(v) in 1..4096 and String.valid?(v) and
        not String.contains?(v, <<0>>)

  defp arguments?(values),
    do:
      is_list(values) and length(values) <= 256 and
        Enum.all?(values, fn value ->
          is_binary(value) and byte_size(value) <= 4096 and String.valid?(value) and
            not String.contains?(value, <<0>>)
        end)

  defp strings?(v), do: is_list(v) and length(v) <= 256 and Enum.all?(v, &text?/1)
  defp absolute?(v), do: text?(v) and Path.type(v) == :absolute

  def relative?(v),
    do:
      text?(v) and Path.type(v) == :relative and
        Enum.all?(String.split(v, "/"), &(&1 not in ["", ".", ".."])) and
        not String.contains?(v, "\\")

  defp subset?(a, b), do: MapSet.subset?(MapSet.new(a), MapSet.new(b))
  defp submap?(a, b), do: Enum.all?(a, fn {k, v} -> Map.fetch(b, k) == {:ok, v} end)
  defp resource_subset?(:io, a, b), do: a.device == b.device
  defp resource_subset?(:filesystem, a, b), do: a.root == b.root and subset?(a.paths, b.paths)
  defp resource_subset?(:network, a, b), do: submap?(a.endpoints, b.endpoints)
  defp resource_subset?(:environment, a, b), do: subset?(a.names, b.names)
  defp resource_subset?(:logging, a, b), do: a.device == b.device and subset?(a.levels, b.levels)
  defp resource_subset?(:process, a, b), do: submap?(a.commands, b.commands)
  defp resource_subset?(_, _, _), do: true

  defp permitted?(:filesystem, :read, {path, count}, p),
    do: path in p.paths and is_integer(count) and count >= 0 and count <= p.max_bytes

  defp permitted?(:filesystem, :write, {path, bytes}, p),
    do: path in p.paths and is_binary(bytes) and byte_size(bytes) <= p.max_bytes

  defp permitted?(:network, :exchange, {name, bytes, count}, p),
    do:
      Map.has_key?(p.endpoints, name) and is_binary(bytes) and byte_size(bytes) <= p.max_bytes and
        is_integer(count) and count >= 0 and count <= p.max_bytes

  defp permitted?(:environment, :get, name, p), do: name in p.names

  defp permitted?(:logging, :emit, {level, message}, p),
    do: level in p.levels and is_binary(message) and byte_size(message) <= p.max_bytes

  defp permitted?(:process, :run, {name, bytes}, p),
    do: Map.has_key?(p.commands, name) and is_binary(bytes) and byte_size(bytes) <= p.max_bytes

  defp permitted?(:io, :read, count, p),
    do: is_integer(count) and count >= 0 and count <= p.max_bytes

  defp permitted?(:io, :write, bytes, p), do: is_binary(bytes) and byte_size(bytes) <= p.max_bytes

  defp permitted?(:random, :bytes, count, p),
    do: is_integer(count) and count >= 0 and count <= p.max_bytes

  defp permitted?(:time, :sleep, count, p),
    do: is_integer(count) and count >= 0 and count <= p.ttl_ms

  defp permitted?(:time, operation, :unit, _) when operation in [:monotonic, :wall], do: true
  defp permitted?(_, _, _, _), do: false
end
