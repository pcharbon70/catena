defmodule Catena.Task.Reference do
  @moduledoc "Owned-task CEK bridge for exact 0.1.52 and the separate experimental time target."

  def evaluate(c, p, %{tag: tag} = e, env)
      when tag in [
             :managed_spawn,
             :managed_send,
             :managed_self,
             :managed_link,
             :managed_unlink,
             :managed_observe,
             :managed_trapping
           ],
      do: Catena.Task.ManagedReference.evaluate(c, p, e, env)

  def evaluate(c, p, %{tag: :task_scope} = e, env) do
    id = Map.get(c, :next_task_scope, 0)

    scope = %{
      phase: :open,
      children: [],
      outcome: nil,
      after: nil,
      grace_ns: e.grace_ns,
      lifetime_order: Map.get(c, :next_lifetime, 0)
    }

    p = Map.put(p, :task_scopes, Map.put(Map.get(p, :task_scopes, %{}), id, scope))

    c =
      c
      |> Map.put(:next_task_scope, id + 1)
      |> Map.put(:next_lifetime, Map.get(c, :next_lifetime, 0) + 1)

    env = Map.put(env, e.binder, {:catena_task_scope, p.id, id})
    put(c, %{p | stack: [{:task_end, id} | p.stack], control: {:expr, e.body, env}})
  end

  def evaluate(c, p, %{tag: :task_monitor} = e, env),
    do: push(c, p, e.scope, env, {:task_monitor_scope, e.target, e.labels, env})

  def evaluate(c, p, %{tag: tag} = e, env) when tag in [:task_observe, :task_demonitor],
    do: push(c, p, e.monitor, env, {tag})

  def evaluate(c, p, %{tag: :task_sleep} = e, env),
    do: push(c, p, e.scope, env, {:task_sleep_scope, e.duration, env})

  def evaluate(c, p, %{tag: :task_start} = e, env),
    do: push(c, p, e.scope, env, {:task_start_scope, e.body, env})

  def evaluate(c, p, %{tag: :task_cancel} = e, env),
    do: push(c, p, e.task, env, {:task_cancel_handle, e.reason, env})

  def advance(c, now) when is_integer(now) and now >= 0 do
    if now >= Map.get(c, :task_clock, 0),
      do: {:ok, c |> Map.put(:task_clock, now) |> Map.put(:resource_clock, now)},
      else: {:error, :clock_reversed}
  end

  def advance(_, _), do: {:error, :invalid_clock}

  def expire_shutdown(c, child) do
    case c.processes[child] do
      %{task_shutdown_deadline: deadline} = p ->
        if not terminal?(p) and Map.get(c, :task_clock, 0) >= deadline,
          do: {:ok, force_stop(c, child) |> after_step()},
          else: {:error, :shutdown_deadline_not_due}

      _ ->
        {:error, :shutdown_deadline_not_due}
    end
  end

  defp force_stop(c, child) do
    p = c.processes[child]
    descendants = Map.get(p, :task_scopes, %{}) |> Map.values() |> Enum.flat_map(& &1.children)

    c =
      Enum.reduce(descendants, c, fn id, c ->
        if terminal?(c.processes[id]), do: c, else: force_stop(c, id)
      end)

    p = %{
      p
      | status: :exited,
        result: :shutdown_deadline_exhausted,
        control: nil,
        stack: [],
        mailbox: []
    }

    p =
      Map.put(
        p,
        :resources,
        Map.new(Map.get(p, :resources, %{}), fn {id, entry} -> {id, %{entry | active: false}} end)
      )

    put(c, p)
  end

  def sleeping_ready?(c, p),
    do: Map.get(p, :task_pending) != nil or p.task_wake <= Map.get(c, :task_clock, 0)

  def returned(c, p, frame, value)
      when is_tuple(frame) and
             elem(frame, 0) in [
               :managed_arguments,
               :managed_send_target,
               :managed_send_message,
               :managed_link_target,
               :managed_unlink,
               :managed_observe,
               :managed_restore
             ],
      do: Catena.Task.ManagedReference.returned(c, p, frame, value)

  def returned(
        c,
        p,
        {:task_monitor_scope, target, labels, env},
        {:catena_task_scope, owner, scope}
      )
      when owner == p.id,
      do: push(c, p, target, env, {:task_monitor_target, scope, labels})

  def returned(c, p, {:task_monitor_target, scope, labels}, {kind, target})
      when kind in [:catena_process, :catena_managed_process] do
    if get_in(p, [:task_scopes, scope, :phase]) == :open do
      id = Map.get(c, :next_task_monitor, 0)

      result =
        if Map.has_key?(c.processes, target) and not terminal?(c.processes[target]),
          do: nil,
          else: {:absent, :unit}

      m = %{
        owner: p.id,
        scope: scope,
        target: target,
        labels: labels,
        result: result,
        active: true,
        observed: false
      }

      c =
        c
        |> Map.put(:next_task_monitor, id + 1)
        |> Map.put(:task_monitors, Map.put(Map.get(c, :task_monitors, %{}), id, m))

      put(c, %{p | control: {:value, {:catena_task_monitor, p.id, id}}})
    else
      stop(c, p, {:trap, :invalid_task_scope_owner})
    end
  end

  def returned(c, p, {:task_demonitor}, {:catena_task_monitor, owner, id}) when owner == p.id do
    c = put_in(c.task_monitors[id].active, false)
    put(c, %{p | control: {:value, :unit}})
  end

  def returned(c, p, {:task_observe}, {:catena_task_monitor, owner, id}) when owner == p.id do
    m = c.task_monitors[id]

    if m.active and not m.observed do
      p = p |> Map.put(:status, :task_observing) |> Map.put(:task_observation, id)
      put(c, %{p | control: nil})
    else
      stop(c, p, {:trap, :inactive_task_monitor})
    end
  end

  def returned(c, p, {:task_sleep_scope, duration, env}, {:catena_task_scope, owner, id})
      when owner == p.id,
      do: push(c, p, duration, env, {:task_sleep_duration, id})

  def returned(c, p, {:task_sleep_duration, id}, duration) do
    cond do
      get_in(p, [:task_scopes, id, :phase]) != :open ->
        stop(c, p, {:trap, :invalid_task_scope_owner})

      not is_integer(duration) or duration < 0 ->
        stop(c, p, {:trap, :invalid_duration})

      true ->
        p =
          p
          |> Map.put(:task_wake, Map.get(c, :task_clock, 0) + duration)
          |> Map.put(:status, :task_sleeping)

        put(c, %{p | control: nil})
    end
  end

  def returned(c, p, {:task_start_scope, body, env}, {:catena_task_scope, owner, id})
      when owner == p.id,
      do: push(c, p, body, env, {:task_start_body, id})

  def returned(c, p, {:task_start_body, id}, closure) do
    case get_in(p, [:task_scopes, id, :phase]) do
      :open ->
        child = %{
          id: c.next,
          name: "$owned",
          status: :running,
          control: {:value, closure},
          stack: [{:apply_value, :unit}],
          mailbox: [],
          mailbox_type: :unit,
          result: nil,
          trap: nil,
          task_owner: {p.id, id},
          task_notified: false
        }

        p = update_in(p.task_scopes[id].children, &(&1 ++ [child.id]))
        p = %{p | control: {:value, {:catena_owned_task, p.id, id, child.id}}}
        %{c | next: c.next + 1} |> put(child) |> put(p)

      _ ->
        Catena.Kernel.Stepper.task_stop(c, p, {:trap, :invalid_task_scope_owner})
    end
  end

  def returned(c, p, {:task_cancel_handle, reason, env}, {:catena_owned_task, owner, id, child})
      when owner == p.id,
      do: push(c, p, reason, env, {:task_cancel_reason, id, child})

  def returned(c, p, {:task_cancel_reason, id, child}, reason) do
    if child in Map.get(p.task_scopes[id], :children, []) do
      c = request(c, child, {:cancelled, reason})
      put(c, %{p | control: {:value, :unit}})
    else
      Catena.Kernel.Stepper.task_stop(c, p, {:trap, :invalid_task_owner})
    end
  end

  def returned(c, p, {:task_end, id}, value),
    do: close(c, p, id, {:ok, value}, {:value, value, p.stack})

  def returned(c, p, {:task_operation_exit, resumption, lifetimes}, value) do
    if MapSet.member?(c.resumptions, resumption),
      do: put(c, %{p | control: {:value, value}}),
      else: actions(c, p, lifetimes, {:abort, value}, {:value, value, p.stack})
  end

  def returned(c, p, _, _),
    do: Catena.Kernel.Stepper.task_stop(c, p, {:trap, :invalid_task_owner})

  def active?(p),
    do: Enum.any?(Map.get(p, :task_scopes, %{}), fn {_, scope} -> scope.phase != :closed end)

  def stop(c, p, outcome) do
    p = %{p | status: :running}

    tasks =
      for {id, scope} <- Map.get(p, :task_scopes, %{}),
          scope.phase != :closed,
          do: {scope.lifetime_order, {:task_end, id}}

    resources =
      for {id, resource} <- Map.get(p, :resources, %{}),
          resource.active,
          do: {Map.get(resource, :lifetime_order, id), {:resource_end, id}}

    actions = (tasks ++ resources) |> Enum.sort(:desc) |> Enum.map(&elem(&1, 1))
    actions(c, p, actions, outcome, :terminal)
  end

  def actions(c, p, [], outcome, :terminal), do: Catena.Kernel.Stepper.task_stop(c, p, outcome)

  def actions(c, p, [], outcome, {:value, value, stack}) do
    if normal?(outcome) or match?({:abort, _}, outcome),
      do: put(c, %{p | status: :running, control: {:value, value}, stack: stack}),
      else: stop(c, p, outcome)
  end

  def actions(c, p, [{:managed_restore, old} | rest], outcome, after_action),
    do: actions(c, Map.put(p, :managed_trapping, old), rest, outcome, after_action)

  def actions(c, p, [{:resource_end, id} | rest], outcome, after_action),
    do: Catena.Kernel.Stepper.task_release(c, p, id, outcome, rest, after_action)

  def actions(c, p, [{:task_end, id} | rest], outcome, after_action),
    do: close(c, p, id, outcome, {:actions, rest, after_action})

  def observing_ready?(c, p),
    do: Map.get(p, :task_pending) != nil or c.task_monitors[p.task_observation].result != nil

  defp observed(c, p) do
    id = p.task_observation
    m = c.task_monitors[id]

    if m.active and not m.observed do
      {role, payload} = m.result
      c = put_in(c.task_monitors[id].observed, true)

      put(c, %{
        p
        | status: :running,
          control: {:value, {:catena_variant, Map.fetch!(m.labels, role), payload}}
      })
    else
      stop(c, p, {:trap, :inactive_task_monitor})
    end
  end

  def before(c, p) do
    case Catena.Task.ManagedReference.before(c, p) do
      :ordinary -> before_task(c, p)
      handled -> handled
    end
  end

  defp before_task(c, p) do
    pending = Map.get(p, :task_pending)

    point =
      p.status in [:waiting, :task_joining, :task_sleeping, :task_observing, :managed_observing] or
        case p.control do
          {:expr, %{tag: tag}, _} ->
            tag in [:call, :receive, :timed_receive, :task_sleep]

          {:value, _} ->
            match?([{:apply_value, _} | _], p.stack)

          _ ->
            p.status in [
              :waiting,
              :task_joining,
              :task_sleeping,
              :task_observing,
              :managed_observing
            ]
        end

    if pending != nil and point and is_nil(Map.get(p, :release_deadline)) do
      p = Map.put(p, :task_pending, nil)

      {:handled, stop(c, p, pending)}
    else
      cond do
        p.status == :task_sleeping and sleeping_ready?(c, p) ->
          {:handled, put(c, %{p | status: :running, control: {:value, :unit}})}

        p.status == :task_observing and observing_ready?(c, p) ->
          {:handled, observed(c, p)}

        true ->
          :ordinary
      end
    end
  end

  def after_step(%{core: %{version: version}} = c)
      when version in ["0.1.52", :owned_task_experiment] do
    c = Catena.Task.ManagedReference.after_step(c)

    c =
      Enum.reduce(Map.get(c, :task_monitors, %{}), c, fn {id, m}, acc ->
        if m.active and is_nil(m.result) and terminal?(acc.processes[m.target]),
          do: put_in(acc.task_monitors[id].result, monitor_outcome(acc.processes[m.target])),
          else: acc
      end)

    Enum.reduce(Map.keys(c.processes), c, fn child, c ->
      p = c.processes[child]

      if Map.has_key?(p, :task_owner) and not p.task_notified and terminal?(p) do
        {owner, scope} = p.task_owner
        c = put(c, %{p | task_notified: true})
        parent = c.processes[owner]
        outcome = outcome(p)

        c = %{
          c
          | trace:
              c.trace ++
                [%{label: :task_child_completed, pid: owner, child: child, outcome: outcome}]
        }

        current = parent.task_scopes[scope]

        c =
          if not normal?(outcome) and (is_nil(current.outcome) or normal?(current.outcome)) do
            parent = put_in(parent.task_scopes[scope].outcome, {:exit, {:child, child, outcome}})

            parent =
              if current.phase == :open,
                do: Map.put(parent, :task_pending, {:exit, {:child, child, outcome}}),
                else: parent

            c = put(c, parent)

            Enum.reduce(
              current.children,
              c,
              &request(&2, &1, {:cancelled, {:sibling_failed, child, outcome}})
            )
          else
            c
          end

        maybe_finish(c, c.processes[owner], scope)
      else
        c
      end
    end)
  end

  def after_step(c), do: c

  defp close(c, p, id, outcome, after_action) do
    old = p.task_scopes[id]
    outcome = if is_nil(old.outcome) or normal?(old.outcome), do: outcome, else: old.outcome
    scope = %{old | phase: :joining, outcome: outcome, after: after_action}
    p = put_in(p.task_scopes[id], scope)
    p = %{p | status: :task_joining, control: nil}
    c = put(c, p)

    c =
      Enum.reduce(Map.get(c, :task_monitors, %{}), c, fn {monitor, m}, acc ->
        if m.owner == p.id and m.scope == id,
          do: put_in(acc.task_monitors[monitor].active, false),
          else: acc
      end)

    c =
      if normal?(outcome),
        do: c,
        else:
          Enum.reduce(scope.children, c, &request(&2, &1, {:cancelled, {:owner_ended, outcome}}))

    maybe_finish(c, c.processes[p.id], id)
  end

  defp maybe_finish(c, p, id) do
    scope = p.task_scopes[id]

    if not terminal?(p) and scope.phase == :joining and
         Enum.all?(scope.children, &terminal?(c.processes[&1])) do
      p = put_in(p.task_scopes[id].phase, :closed)
      p = %{p | status: :running}

      case scope.after do
        {:actions, rest, after_action} ->
          actions(c, p, rest, scope.outcome, after_action)

        {:value, value, stack} ->
          if normal?(scope.outcome) or match?({:abort, _}, scope.outcome),
            do: put(c, %{p | control: {:value, value}, stack: stack}),
            else: stop(c, p, scope.outcome)
      end
    else
      c
    end
  end

  defp request(c, child, reason) do
    p = c.processes[child]

    if not terminal?(p) and is_nil(Map.get(p, :task_pending)) do
      {owner, scope} = p.task_owner
      grace = c.processes[owner].task_scopes[scope].grace_ns

      p =
        p
        |> Map.put(:task_pending, reason)
        |> Map.put(:task_shutdown_deadline, Map.get(c, :task_clock, 0) + grace)

      put(c, p)
    else
      c
    end
  end

  defp monitor_outcome(%{status: :terminated}), do: {:completed, :unit}

  defp monitor_outcome(%{status: :trapped, trap: reason}) when is_integer(reason),
    do: {:trapped, reason}

  defp monitor_outcome(%{status: :cancelled, result: reason}) when is_integer(reason),
    do: {:cancelled, reason}

  defp monitor_outcome(%{status: :exited, result: :shutdown_deadline_exhausted}),
    do: {:external_loss, :unit}

  defp monitor_outcome(%{status: :exited}), do: {:exited, :unit}
  defp monitor_outcome(_), do: {:runtime_failure, :unit}

  defp normal?({:ok, _}), do: true
  defp normal?(_), do: false
  defp terminal?(p), do: p.status in [:terminated, :trapped, :cancelled, :exited]
  defp outcome(%{status: :terminated, result: value}), do: {:ok, value}
  defp outcome(%{status: :trapped, trap: reason}), do: {:trap, reason}
  defp outcome(%{status: :cancelled, result: reason}), do: {:cancelled, reason}
  defp outcome(%{status: :exited, result: reason}), do: {:exit, reason}
  defp put(c, p), do: %{c | processes: Map.put(c.processes, p.id, p)}

  defp push(c, p, e, env, frame),
    do: put(c, %{p | control: {:expr, e, env}, stack: [frame | p.stack]})
end
