defmodule Catena.Runtime.Capacity do
  @moduledoc "Explicit bounded runtime queue above raw local send semantics."

  @version "0.1.75"
  @max_messages 65_536
  @max_bytes 67_108_864

  def profile,
    do: %{
      version: @version,
      max_messages: @max_messages,
      max_bytes: @max_bytes,
      overload_policies: [:reject, :terminate],
      byte_accounting: :erlang_external_size,
      host_fatal_recovery: false,
      raw_send_changed: false,
      silent_loss: false
    }

  def start(options \\ []) do
    owner = self()
    token = :crypto.strong_rand_bytes(32)
    messages = Keyword.get(options, :messages)
    bytes = Keyword.get(options, :bytes)
    overload = Keyword.get(options, :overload, :reject)

    if Keyword.keys(options) -- [:messages, :bytes, :overload] == [] and
         is_integer(messages) and messages in 1..@max_messages and is_integer(bytes) and
         bytes in 1..@max_bytes and overload in [:reject, :terminate] do
      pid = spawn(fn -> init(owner, token, messages, bytes, overload) end)
      {:ok, {__MODULE__, pid, token}}
    else
      {:error, :invalid_capacity_policy}
    end
  end

  def offer({__MODULE__, pid, token}, payload) when is_pid(pid),
    do: call(pid, token, {:offer, payload})

  def offer(_, _), do: {:error, :invalid_capacity_handle}

  def take({__MODULE__, pid, token}) when is_pid(pid), do: call(pid, token, :take)
  def take(_), do: {:error, :invalid_capacity_handle}

  def stats({__MODULE__, pid, token}) when is_pid(pid), do: call(pid, token, :stats)
  def stats(_), do: {:error, :invalid_capacity_handle}

  def close({__MODULE__, pid, token}) when is_pid(pid), do: call(pid, token, :close)
  def close(_), do: {:error, :invalid_capacity_handle}

  defp init(owner, token, max_messages, max_bytes, overload) do
    monitor = Process.monitor(owner)

    loop(%{
      owner: owner,
      monitor: monitor,
      token: token,
      queue: :queue.new(),
      messages: 0,
      bytes: 0,
      max_messages: max_messages,
      max_bytes: max_bytes,
      overload: overload,
      rejected: 0
    })
  end

  defp loop(state) do
    receive do
      {:capacity_call, caller, reply, token, operation} when token == state.token ->
        case operation(state, caller, operation) do
          {:continue, result, next} ->
            send(caller, {reply, result})
            loop(next)

          {:stop, result, reason, next} ->
            send(caller, {reply, result})

            if reason != :normal,
              do:
                send(
                  next.owner,
                  {:catena_capacity_exit, {__MODULE__, self(), next.token}, reason}
                )

            exit(reason)
        end

      {:capacity_call, caller, reply, _token, _operation} ->
        send(caller, {reply, {:error, :invalid_capacity_handle}})
        loop(state)

      {:DOWN, monitor, :process, owner, _}
      when monitor == state.monitor and owner == state.owner ->
        :ok

      _ ->
        loop(state)
    end
  end

  defp operation(state, _caller, {:offer, payload}) do
    bytes = :erlang.external_size(payload)

    if state.messages + 1 <= state.max_messages and state.bytes + bytes <= state.max_bytes do
      next = %{
        state
        | queue: :queue.in({payload, bytes}, state.queue),
          messages: state.messages + 1,
          bytes: state.bytes + bytes
      }

      {:continue, :ok, next}
    else
      next = %{state | rejected: state.rejected + 1}

      case state.overload do
        :reject -> {:continue, {:error, :overloaded}, next}
        :terminate -> {:stop, {:error, :capacity_exhausted}, :capacity_exhausted, next}
      end
    end
  rescue
    _ -> {:continue, {:error, :invalid_capacity_payload}, state}
  end

  defp operation(state, caller, :take) when caller == state.owner do
    case :queue.out(state.queue) do
      {{:value, {payload, bytes}}, queue} ->
        {:continue, {:ok, payload},
         %{state | queue: queue, messages: state.messages - 1, bytes: state.bytes - bytes}}

      {:empty, _} ->
        {:continue, :empty, state}
    end
  end

  defp operation(state, caller, :stats) when caller == state.owner do
    {:continue,
     {:ok,
      Map.take(state, [
        :messages,
        :bytes,
        :max_messages,
        :max_bytes,
        :overload,
        :rejected
      ])}, state}
  end

  defp operation(state, caller, :close) when caller == state.owner,
    do: {:stop, {:ok, %{discarded: state.messages, bytes: state.bytes}}, :normal, state}

  defp operation(state, _, _), do: {:continue, {:error, :capacity_owner_required}, state}

  defp call(pid, token, operation) do
    reply = :erlang.alias()
    monitor = Process.monitor(pid)
    send(pid, {:capacity_call, self(), reply, token, operation})

    try do
      receive do
        {^reply, result} -> result
        {:DOWN, ^monitor, :process, ^pid, reason} -> {:error, {:capacity_unavailable, reason}}
      after
        5_000 -> {:error, :capacity_unavailable}
      end
    after
      :erlang.unalias(reply)
      Process.demonitor(monitor, [:flush])
    end
  end
end
