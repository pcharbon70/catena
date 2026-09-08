defmodule Catena.Task.Lifecycle do
  @moduledoc "Experimental local relationship and owned-task transition model; not a selected language revision."

  def new do
    %{tasks: %{0 => task(nil)}, monitors: %{}, links: %{}, next: 1, signals: [], trace: []}
  end

  def step(state, {:spawn, parent, id, mode}) when mode in [:raw, :owned] do
    with %{phase: :running} <- state.tasks[parent],
         false <- Map.has_key?(state.tasks, id),
         true <- is_integer(id) and id > 0 do
      owner = if mode == :owned, do: parent, else: nil
      {:ok, %{state | tasks: Map.put(state.tasks, id, task(owner))}}
    else
      _ -> {:error, :invalid_spawn}
    end
  end

  def step(state, {:monitor, watcher, target, token}) do
    with true <- live?(state, watcher), false <- Map.has_key?(state.monitors, token) do
      relation = %{watcher: watcher, target: target, active: true, notified: false}
      state = %{state | monitors: Map.put(state.monitors, token, relation)}

      state =
        if live?(state, target),
          do: state,
          else: enqueue(state, {:down, token, watcher, target, :absent})

      {:ok, state}
    else
      _ -> {:error, :invalid_monitor}
    end
  end

  def step(state, {:demonitor, watcher, token, flush}) when is_boolean(flush) do
    case state.monitors[token] do
      %{watcher: ^watcher} = relation ->
        state = %{state | monitors: Map.put(state.monitors, token, %{relation | active: false})}

        state =
          if flush do
            update_in(
              state.tasks[watcher].observations,
              &Enum.reject(&1, fn
                {:down, ^token, _, _} -> true
                _ -> false
              end)
            )
          else
            state
          end

        {:ok, state}

      _ ->
        {:error, :invalid_monitor_owner}
    end
  end

  def step(state, {:link, left, right}) when left != right do
    key = pair(left, right)

    cond do
      not live?(state, left) ->
        {:error, :invalid_link_owner}

      Map.has_key?(state.links, key) ->
        {:ok, state}

      true ->
        generation = state.next
        state = %{state | links: Map.put(state.links, key, generation), next: generation + 1}

        state =
          if live?(state, right),
            do: state,
            else: enqueue(state, {:exit, key, generation, right, left, :absent})

        {:ok, state}
    end
  end

  def step(state, {:unlink, left, right}) do
    if live?(state, left),
      do: {:ok, %{state | links: Map.delete(state.links, pair(left, right))}},
      else: {:error, :invalid_link_owner}
  end

  def step(state, {:trap_exits, id, enabled}) when is_boolean(enabled) do
    if live?(state, id),
      do: {:ok, put_in(state.tasks[id].trap_exits, enabled)},
      else: {:error, :invalid_task}
  end

  def step(state, {:cancel, owner, child, reason}) do
    case state.tasks[child] do
      %{owner: ^owner} when owner != nil ->
        if live?(state, owner),
          do: {:ok, request_cancel(state, child, reason)},
          else: {:error, :invalid_owner}

      _ ->
        {:error, :invalid_owner}
    end
  end

  def step(state, {:safe_point, id}) do
    case state.tasks[id] do
      %{phase: :running, cancellation: nil} ->
        {:ok, state}

      %{phase: :running, cancellation: {:reason, reason}} ->
        {:ok, close(state, id, {:cancelled, reason})}

      _ ->
        {:error, :invalid_safe_point}
    end
  end

  def step(state, {:finish, id, outcome}) do
    if match?(%{phase: :running}, state.tasks[id]) and outcome?(outcome),
      do: {:ok, close(state, id, outcome)},
      else: {:error, :invalid_completion}
  end

  def step(state, {:cleanup_done, id, result}) do
    case state.tasks[id] do
      %{phase: :closing, cleaned: false, outcome: primary} ->
        outcome =
          case {primary, result} do
            {_, :ok} -> primary
            {{:trap, _}, {:error, _}} -> primary
            {_, {:error, reason}} -> {:trap, {:mandatory_release_failed, reason}}
            _ -> :invalid
          end

        if outcome == :invalid do
          {:error, :invalid_cleanup_result}
        else
          state =
            state
            |> put_in([:tasks, id, :cleaned], true)
            |> put_in([:tasks, id, :outcome], outcome)

          state = if normal?(outcome), do: state, else: close(state, id, outcome)
          {:ok, settle(state, id)}
        end

      _ ->
        {:error, :invalid_cleanup_completion}
    end
  end

  def step(state, {:deliver, index}) when is_integer(index) and index >= 0 do
    case Enum.fetch(state.signals, index) do
      {:ok, signal} ->
        state = %{state | signals: List.delete_at(state.signals, index)}
        {:ok, deliver(state, signal)}

      :error ->
        {:error, :invalid_signal}
    end
  end

  def step(_, _), do: {:error, :invalid_transition}

  defp task(owner),
    do: %{
      owner: owner,
      phase: :running,
      cancellation: nil,
      cleaned: false,
      outcome: nil,
      trap_exits: false,
      observations: []
    }

  defp pair(a, b), do: Enum.sort([a, b]) |> List.to_tuple()
  defp live?(state, id), do: match?(%{phase: phase} when phase != :terminal, state.tasks[id])
  defp children(state, id), do: for({child, %{owner: ^id}} <- state.tasks, do: child)
  defp outcome?({kind, _}), do: kind in [:ok, :trap, :cancelled, :exit]
  defp outcome?(_), do: false
  defp normal?({:ok, _}), do: true
  defp normal?(_), do: false
  defp enqueue(state, signal), do: %{state | signals: state.signals ++ [signal]}
  defp record(state, event), do: %{state | trace: state.trace ++ [event]}

  defp request_cancel(state, id, reason) do
    case state.tasks[id] do
      %{phase: :running, cancellation: nil} ->
        put_in(state.tasks[id].cancellation, {:reason, reason})

      _ ->
        state
    end
  end

  defp close(state, id, outcome) do
    state =
      state
      |> put_in([:tasks, id, :phase], :closing)
      |> put_in([:tasks, id, :outcome], outcome)

    if normal?(outcome),
      do: state,
      else:
        Enum.reduce(children(state, id), state, &request_cancel(&2, &1, {:owner_ended, outcome}))
  end

  defp settle(state, id) do
    task = state.tasks[id]

    if task.phase == :closing and task.cleaned and
         Enum.all?(children(state, id), fn child ->
           state.tasks[child].phase == :terminal and
             Enum.any?(task.observations, &match?({:owned_down, ^child, _}, &1))
         end) do
      state = put_in(state.tasks[id].phase, :terminal) |> record({:terminal, id, task.outcome})

      state =
        Enum.reduce(state.monitors, state, fn {token, relation}, acc ->
          if relation.active and relation.target == id,
            do: enqueue(acc, {:down, token, relation.watcher, id, task.outcome}),
            else: acc
        end)

      state =
        Enum.reduce(state.links, state, fn {{left, right} = key, generation}, acc ->
          cond do
            left == id -> enqueue(acc, {:exit, key, generation, id, right, task.outcome})
            right == id -> enqueue(acc, {:exit, key, generation, id, left, task.outcome})
            true -> acc
          end
        end)

      if is_nil(task.owner),
        do: state,
        else: enqueue(state, {:owned_down, task.owner, id, task.outcome})
    else
      state
    end
  end

  defp deliver(state, {:down, token, watcher, target, outcome}) do
    case state.monitors[token] do
      %{active: true, notified: false} ->
        state = put_in(state.monitors[token].notified, true)
        observe(state, watcher, {:down, token, target, outcome})

      _ ->
        state
    end
  end

  defp deliver(state, {:exit, key, generation, from, target, outcome}) do
    if state.links[key] == generation and live?(state, target) do
      state = %{state | links: Map.delete(state.links, key)}

      cond do
        state.tasks[target].trap_exits -> observe(state, target, {:exit, from, outcome})
        normal?(outcome) -> state
        state.tasks[target].phase == :running -> close(state, target, {:exit, {from, outcome}})
        true -> state
      end
    else
      state
    end
  end

  defp deliver(state, {:owned_down, owner, child, outcome}) do
    state = observe(state, owner, {:owned_down, child, outcome})

    state =
      if not normal?(outcome) and
           (state.tasks[owner].phase == :running or
              (state.tasks[owner].phase == :closing and normal?(state.tasks[owner].outcome))),
         do: close(state, owner, {:exit, {:child, child, outcome}}),
         else: state

    settle(state, owner)
  end

  defp observe(state, id, event) do
    if live?(state, id),
      do: update_in(state.tasks[id].observations, &(&1 ++ [event])),
      else: state
  end
end
