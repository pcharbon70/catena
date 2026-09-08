defmodule Catena.Task.Managed do
  @moduledoc "Managed actor broker for exact 0.1.52, with private lifecycle and user-message channels."
  alias Catena.Task.Monitor

  def spawn_actor(body, grace_ns)
      when is_function(body, 0) and is_integer(grace_ns) and grace_ns >= 0 do
    ready = :erlang.alias()
    {broker, monitor} = spawn_monitor(fn -> broker_init(body, grace_ns, ready) end)

    try do
      receive do
        {^ready, :ready} ->
          {__MODULE__, broker}

        {:DOWN, ^monitor, :process, ^broker, reason} ->
          :erlang.error({:catena_trap, {:managed_start_failed, reason}})
      end
    after
      :erlang.unalias(ready)
      Process.demonitor(monitor, [:flush])

      receive do
        {^ready, _} -> :ok
      after
        0 -> :ok
      end
    end
  end

  def context, do: Process.get({__MODULE__, :context})

  def self_actor do
    case context() do
      {broker, _} -> {__MODULE__, broker}
      _ -> :erlang.error({:catena_trap, :missing_managed_context})
    end
  end

  def send_message({__MODULE__, broker}, payload) when is_pid(broker),
    do:
      (
        send(broker, {:user_payload, payload})
        :unit
      )

  def pid({__MODULE__, broker}) when is_pid(broker), do: broker

  def checkpoint do
    case context() do
      {_, token} ->
        receive do
          {:managed_control, ^token, :stop, reason} -> exit({:catena_resource_exit, reason})
        after
          0 -> :ok
        end

      _ ->
        :ok
    end
  end

  def receive_message do
    {_, token} = context()

    receive do
      {:managed_message, ^token, payload} -> payload
      {:managed_control, ^token, :stop, reason} -> exit({:catena_resource_exit, reason})
    end
  end

  def link({__MODULE__, peer}), do: call({:link, peer})
  def unlink(handle), do: call({:unlink, handle})

  def observe(handle, labels) do
    {:role, role, payload} = call({:observe, handle})
    {:catena_variant, Map.fetch!(labels, role), payload}
  end

  def trapping(body) when is_function(body, 0) do
    old = call({:trapping, true})

    try do
      body.()
    after
      call({:trapping, old})
    end
  end

  defp call(operation) do
    {broker, token} = context() || :erlang.error({:catena_trap, :missing_managed_context})
    reply = :erlang.alias()
    monitor = Process.monitor(broker)
    send(broker, {:control_call, token, reply, operation})

    try do
      receive do
        {^reply, {:ok, value}} ->
          value

        {^reply, {:error, reason}} ->
          :erlang.error({:catena_trap, reason})

        {:managed_control, ^token, :stop, reason} ->
          exit({:catena_resource_exit, reason})

        {:DOWN, ^monitor, :process, ^broker, reason} ->
          :erlang.error({:catena_trap, {:managed_broker_lost, reason}})
      end
    after
      :erlang.unalias(reply)
      Process.demonitor(monitor, [:flush])

      receive do
        {^reply, _} -> :ok
      after
        0 -> :ok
      end
    end
  end

  defp broker_init(body, grace, ready) do
    Process.flag(:trap_exit, true)
    broker = self()
    token = make_ref()

    {worker, monitor} =
      :erlang.spawn_opt(
        fn ->
          receive do
            {^token, :start} ->
              Process.put({__MODULE__, :context}, {broker, token})

              outcome =
                try do
                  {:ok, body.()}
                catch
                  :error, {:catena_trap, reason} -> {:trap, reason}
                  :exit, {:catena_resource_exit, reason} -> {:exit, reason}
                  :throw, {:catena_resource_cancelled, reason} -> {:cancelled, reason}
                  kind, reason -> {:trap, {:foreign_managed_failure, kind, reason}}
                end

              send(broker, {:worker_result, token, outcome})
          end
        end,
        [:link, :monitor]
      )

    send(ready, {ready, :ready})
    send(worker, {token, :start})

    broker_loop(%{
      worker: worker,
      monitor: monitor,
      token: token,
      grace: grace,
      trapping: false,
      links: %{},
      next: 0,
      outcome: nil,
      deadline: nil,
      forced: false
    })
  end

  defp broker_loop(s) do
    receive do
      {:user_payload, payload} ->
        if is_nil(s.outcome), do: send(s.worker, {:managed_message, s.token, payload})
        broker_loop(s)

      {:control_call, token, reply, operation} when token == s.token ->
        broker_loop(operation(s, reply, operation))

      {:worker_result, token, outcome} when token == s.token ->
        selected =
          cond do
            s.forced -> s.outcome
            match?({:trap, _}, outcome) -> outcome
            is_nil(s.outcome) -> outcome
            true -> s.outcome
          end

        broker_loop(%{s | outcome: selected})

      {:DOWN, monitor, :process, worker, reason}
      when monitor == s.monitor and worker == s.worker ->
        publish(s.outcome || {:exit, {:external_loss, reason}})

      {:EXIT, worker, _} when worker == s.worker ->
        broker_loop(s)

      {:EXIT, peer, reason} ->
        broker_loop(link_exit(s, peer, reason))
    after
      wait(s.deadline) ->
        if now() < s.deadline do
          broker_loop(s)
        else
          Process.unlink(s.worker)
          Process.exit(s.worker, :kill)

          broker_loop(%{
            s
            | deadline: nil,
              forced: true,
              outcome: {:exit, :shutdown_deadline_exhausted}
          })
        end
    end
  end

  defp operation(s, reply, {:trapping, enabled}) when is_boolean(enabled) do
    send(reply, {reply, {:ok, s.trapping}})
    %{s | trapping: enabled}
  end

  defp operation(s, reply, {:link, peer}) when is_pid(peer) and peer != self() do
    {:links, native_links} = Process.info(self(), :links)
    existing = if peer in native_links, do: s.links[peer], else: nil

    {relation, s} =
      case existing do
        %{active: true} = old ->
          {old, s}

        _ ->
          drain_exit(peer)
          Process.link(peer)

          relation = %{
            generation: s.next,
            active: true,
            outcome: nil,
            waiter: nil,
            observed: false
          }

          {relation, %{s | links: Map.put(s.links, peer, relation), next: s.next + 1}}
      end

    send(reply, {reply, {:ok, {__MODULE__, self(), peer, relation.generation}}})
    s
  end

  defp operation(s, reply, {op, {__MODULE__, broker, peer, generation}}) when broker == self() do
    case s.links[peer] do
      %{generation: ^generation} = relation ->
        case op do
          :unlink ->
            Process.unlink(peer)
            drain_exit(peer)
            send(reply, {reply, {:ok, :unit}})
            %{s | links: Map.delete(s.links, peer)}

          :observe
          when s.trapping and not relation.observed and
                 (relation.active or relation.outcome != nil) ->
            relation = %{relation | observed: true}

            if relation.outcome == nil do
              put_in(s.links[peer], %{relation | waiter: reply})
            else
              send(reply, {reply, {:ok, relation.outcome}})
              put_in(s.links[peer], relation)
            end

          _ ->
            send(reply, {reply, {:error, :invalid_link_observation}})
            s
        end

      _ ->
        send(reply, {reply, {:error, :stale_managed_link}})
        s
    end
  end

  defp operation(s, reply, _) do
    send(reply, {reply, {:error, :invalid_managed_relationship}})
    s
  end

  defp link_exit(s, peer, reason) do
    case s.links[peer] do
      %{active: true} = relation ->
        outcome = Monitor.classify_exit(reason)

        s =
          put_in(s.links[peer], %{
            relation
            | active: false,
              outcome: if(s.trapping, do: outcome, else: nil)
          })

        cond do
          s.trapping ->
            if relation.waiter != nil,
              do: send(relation.waiter, {relation.waiter, {:ok, outcome}})

            s

          reason == :normal ->
            s

          s.outcome != nil ->
            s

          true ->
            cause = {:linked, peer, reason}
            send(s.worker, {:managed_control, s.token, :stop, cause})

            %{
              s
              | outcome: {:exit, cause},
                deadline: now() + div(s.grace + 999_999, 1_000_000) * 1_000_000
            }
        end

      nil ->
        # A native link is symmetric even when only the peer requested a handle.
        relation = %{generation: s.next, active: true, outcome: nil, waiter: nil, observed: false}
        link_exit(%{s | links: Map.put(s.links, peer, relation), next: s.next + 1}, peer, reason)

      _ ->
        s
    end
  end

  defp drain_exit(peer) do
    receive do
      {:EXIT, ^peer, _} -> drain_exit(peer)
    after
      0 -> :ok
    end
  end

  defp publish({:ok, _}), do: :ok
  defp publish({:trap, reason}), do: exit({:catena_managed_trap, reason})
  defp publish({:exit, reason}), do: exit({:catena_managed_exit, reason})
  defp publish({:cancelled, reason}), do: exit({:catena_managed_cancelled, reason})
  defp now, do: System.monotonic_time(:nanosecond)
  defp wait(nil), do: :infinity
  defp wait(deadline), do: min(div(max(deadline - now(), 0) + 999_999, 1_000_000), 4_294_967_295)
end
