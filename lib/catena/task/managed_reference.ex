defmodule Catena.Task.ManagedReference do
  @moduledoc "Experimental managed actor transitions, separate from user mailbox delivery."

  def evaluate(c, p, %{tag: :managed_spawn} = e, env) do
    case e.arguments do
      [] -> spawn_actor(c, p, e, [])
      [first | rest] -> push(c, p, first, env, {:managed_arguments, e, [], rest, env})
    end
  end

  def evaluate(c, p, %{tag: :managed_self}, _),
    do: value(c, p, {:catena_managed_process, p.id})

  def evaluate(c, p, %{tag: :managed_send} = e, env),
    do: push(c, p, e.left, env, {:managed_send_target, e.right, env})

  def evaluate(c, p, %{tag: :managed_link} = e, env),
    do: push(c, p, e.target, env, {:managed_link_target, e.labels})

  def evaluate(c, p, %{tag: tag} = e, env) when tag in [:managed_unlink, :managed_observe],
    do: push(c, p, e.link, env, {tag})

  def evaluate(c, p, %{tag: :managed_trapping} = e, env),
    do:
      push(
        c,
        Map.put(p, :managed_trapping, true),
        e.body,
        env,
        {:managed_restore, Map.get(p, :managed_trapping, false)}
      )

  def returned(c, p, {:managed_arguments, e, completed, [], _}, argument),
    do: spawn_actor(c, p, e, Enum.reverse([argument | completed]))

  def returned(c, p, {:managed_arguments, e, completed, [next | rest], env}, argument),
    do: push(c, p, next, env, {:managed_arguments, e, [argument | completed], rest, env})

  def returned(c, p, {:managed_restore, old}, result),
    do: value(c, Map.put(p, :managed_trapping, old), result)

  def returned(c, p, {:managed_send_target, message, env}, target),
    do: push(c, p, message, env, {:managed_send_message, target})

  def returned(c, p, {:managed_send_message, {:catena_managed_process, target}}, message) do
    c = put(c, p)

    c =
      case c.processes[target] do
        nil ->
          c

        peer ->
          if terminal?(peer),
            do: c,
            else: put(c, %{peer | mailbox: peer.mailbox ++ [{p.id, message}]})
      end

    value(c, c.processes[p.id], :unit)
  end

  def returned(c, p, {:managed_link_target, _}, {:catena_managed_process, peer})
      when peer == p.id,
      do: invalid(c, p)

  def returned(c, p, {:managed_link_target, labels}, {:catena_managed_process, peer}) do
    c =
      Map.update(c, :managed_retired, %{}, fn retired ->
        Map.reject(retired, fn {{owner, target, _}, _} -> owner == p.id and target == peer end)
      end)

    pair = pair(p.id, peer)
    links = Map.get(c, :managed_links, %{})
    generation = Map.get(c, :next_managed_link, 0)

    old = Map.get(links, pair)
    fresh = is_nil(old) or not old.active

    relationship =
      if fresh,
        do: %{
          generation: generation,
          labels: %{},
          results: %{},
          observed: MapSet.new(),
          active: true
        },
        else: old

    relationship = put_in(relationship.labels[p.id], labels)

    c =
      c
      |> Map.put(:managed_links, Map.put(links, pair, relationship))
      |> Map.put(:next_managed_link, generation + 1)

    c =
      if fresh and
           (is_nil(c.processes[peer]) or terminal?(c.processes[peer])),
         do: enqueue(c, {p.id, peer, relationship.generation, {:absent, :unit}}),
         else: c

    value(c, p, {:catena_managed_link, p.id, peer, relationship.generation})
  end

  def returned(c, p, {:managed_unlink}, {:catena_managed_link, owner, peer, generation})
      when owner == p.id do
    case relationship(c, owner, peer, generation) do
      nil ->
        invalid(c, p)

      link ->
        retired = Map.get(c, :managed_retired, %{}) |> Map.delete({owner, peer, generation})

        retired =
          if Map.has_key?(link.labels, peer),
            do: Map.put(retired, {peer, owner, generation}, link),
            else: retired

        c =
          c
          |> Map.put(:managed_retired, retired)
          |> Map.update(:managed_links, %{}, &Map.delete(&1, pair(owner, peer)))

        value(c, p, :unit)
    end
  end

  def returned(c, p, {:managed_observe}, {:catena_managed_link, owner, peer, generation})
      when owner == p.id do
    case relationship(c, owner, peer, generation) do
      %{generation: ^generation} = link ->
        if Map.get(p, :managed_trapping, false) and not MapSet.member?(link.observed, owner) and
             (link.active or Map.has_key?(link.results, owner)) do
          p =
            p
            |> Map.put(:status, :managed_observing)
            |> Map.put(:managed_observation, {peer, generation})
            |> Map.put(:control, nil)

          put(c, p)
        else
          invalid(c, p)
        end

      _ ->
        invalid(c, p)
    end
  end

  def returned(c, p, _, _), do: invalid(c, p)

  def signals?(c, p),
    do:
      Enum.any?(Map.get(c, :managed_signals, []), fn {recipient, _, _, _} -> recipient == p.id end)

  def ready?(c, p) do
    {peer, generation} = p.managed_observation
    link = relationship(c, p.id, peer, generation)

    Map.get(p, :task_pending) != nil or signals?(c, p) or
      is_nil(link) or link.generation != generation or Map.has_key?(link.results, p.id)
  end

  def before(c, p) do
    signals = Map.get(c, :managed_signals, [])
    index = Enum.find_index(signals, fn {recipient, _, _, _} -> recipient == p.id end)

    cond do
      index != nil ->
        {signal, remaining} = List.pop_at(signals, index)
        {:handled, deliver(Map.put(c, :managed_signals, remaining), p, signal)}

      p.status == :managed_observing and is_nil(Map.get(p, :task_pending)) and ready?(c, p) ->
        {peer, generation} = p.managed_observation

        case relationship(c, p.id, peer, generation) do
          %{generation: ^generation} = link ->
            {role, payload} = Map.fetch!(link.results, p.id)

            observed = %{link | observed: MapSet.put(link.observed, p.id)}

            c =
              if match?(
                   %{generation: ^generation},
                   get_in(c, [:managed_links, pair(p.id, peer)])
                 ),
                 do: put_in(c.managed_links[pair(p.id, peer)], observed),
                 else: put_in(c.managed_retired[{p.id, peer, generation}], observed)

            {:handled,
             value(
               c,
               %{p | status: :running},
               {:catena_variant, Map.fetch!(link.labels[p.id], role), payload}
             )}

          _ ->
            {:handled, invalid(c, p)}
        end

      true ->
        :ordinary
    end
  end

  def after_step(c) do
    Enum.reduce(Map.keys(c.processes), c, fn id, c ->
      p = c.processes[id]

      if Map.get(p, :managed, false) and terminal?(p) and not Map.get(p, :managed_notified, false) do
        c = put(c, Map.put(p, :managed_notified, true))

        Enum.reduce(Map.get(c, :managed_links, %{}), c, fn {{a, b}, link}, c ->
          cond do
            a == id -> enqueue(c, {b, id, link.generation, outcome(p)})
            b == id -> enqueue(c, {a, id, link.generation, outcome(p)})
            true -> c
          end
        end)
      else
        c
      end
    end)
  end

  defp deliver(c, p, {_, peer, generation, outcome}) do
    case get_in(c, [:managed_links, pair(p.id, peer)]) do
      %{generation: ^generation} ->
        c = put_in(c.managed_links[pair(p.id, peer)].active, false)

        cond do
          terminal?(p) ->
            c

          Map.get(p, :managed_trapping, false) ->
            put_in(c.managed_links[pair(p.id, peer)].results[p.id], outcome)

          outcome == {:completed, :unit} ->
            c

          is_nil(Map.get(p, :task_pending)) ->
            p =
              p
              |> Map.put(:task_pending, {:exit, {:linked, peer, outcome}})
              |> Map.put(:task_shutdown_deadline, Map.get(c, :task_clock, 0) + p.managed_grace)

            put(c, p)

          true ->
            c
        end

      _ ->
        c
    end
  end

  defp spawn_actor(c, p, e, arguments) do
    entry = Enum.find(c.core.processes, &(&1.name == e.selected_entry.name))
    env = entry.parameters |> Enum.map(& &1.name) |> Enum.zip(arguments) |> Map.new()

    child = %{
      id: c.next,
      name: entry.name,
      status: :running,
      control: {:expr, entry.body, env},
      stack: [],
      mailbox: [],
      mailbox_type: entry.mailbox,
      result: nil,
      trap: nil,
      managed: true,
      managed_grace: e.grace_ns
    }

    %{c | next: c.next + 1} |> put(child) |> value(p, {:catena_managed_process, child.id})
  end

  defp relationship(c, owner, peer, generation) do
    case get_in(c, [:managed_links, pair(owner, peer)]) do
      %{generation: ^generation} = link -> link
      _ -> get_in(c, [:managed_retired, {owner, peer, generation}])
    end
  end

  defp outcome(%{status: :terminated}), do: {:completed, :unit}
  defp outcome(%{status: :trapped, trap: n}) when is_integer(n), do: {:trapped, n}
  defp outcome(%{status: :cancelled, result: n}) when is_integer(n), do: {:cancelled, n}

  defp outcome(%{status: :exited, result: :shutdown_deadline_exhausted}),
    do: {:external_loss, :unit}

  defp outcome(%{status: :exited}), do: {:exited, :unit}
  defp outcome(_), do: {:runtime_failure, :unit}
  defp terminal?(p), do: p.status in [:terminated, :trapped, :cancelled, :exited]
  defp pair(a, b), do: if(a <= b, do: {a, b}, else: {b, a})
  defp enqueue(c, signal), do: Map.update(c, :managed_signals, [signal], &(&1 ++ [signal]))
  defp put(c, p), do: %{c | processes: Map.put(c.processes, p.id, p)}
  defp value(c, p, value), do: put(c, %{p | control: {:value, value}})

  defp push(c, p, e, env, frame),
    do: put(c, %{p | control: {:expr, e, env}, stack: [frame | p.stack]})

  defp invalid(c, p), do: Catena.Task.Reference.stop(c, p, {:trap, :invalid_managed_relationship})
end
