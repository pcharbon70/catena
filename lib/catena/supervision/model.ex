defmodule Catena.Supervision.Model do
  @moduledoc "Independent policy model for the supported static supervision contract."

  def new(children, strategy, intensity, period)
      when strategy in [:one_for_one, :one_for_all, :rest_for_one] and
             is_integer(intensity) and intensity > 0 and is_integer(period) and period > 0 do
    valid =
      is_list(children) and children != [] and
        Enum.all?(children, fn child ->
          is_map(child) and Map.keys(child) |> Enum.sort() == [:id, :restart] and
            is_binary(child.id) and child.id != "" and
            child.restart in [:permanent, :transient, :temporary]
        end)

    if valid and length(Enum.uniq_by(children, & &1.id)) == length(children) do
      {:ok,
       %{
         children: Enum.map(children, &Map.merge(&1, %{generation: 0, running: true})),
         strategy: strategy,
         intensity: intensity,
         period: period,
         history: [],
         now: nil,
         phase: :running
       }}
    else
      {:error, :invalid_description}
    end
  end

  def new(_, _, _, _), do: {:error, :invalid_description}

  def exit_child(%{phase: :running} = state, id, generation, reason, now)
      when is_integer(now) do
    child = Enum.find(state.children, &(&1.id == id))

    cond do
      state.now != nil and now < state.now ->
        {:error, :backward_clock}

      child == nil or not child.running or child.generation != generation ->
        {:error, :stale_child}

      true ->
        decide(%{state | now: now}, child, reason)
    end
  end

  def exit_child(_, _, _, _, _), do: {:error, :closed_tree}

  def stop(%{phase: :running} = state) do
    commands = stopping(state.children)

    {:ok, %{state | phase: :closed, children: Enum.map(state.children, &%{&1 | running: false})},
     commands}
  end

  def stop(_), do: {:error, :closed_tree}

  defp decide(state, child, reason) do
    stopped =
      Enum.map(state.children, fn c -> if c.id == child.id, do: %{c | running: false}, else: c end)

    restart = child.restart == :permanent or (child.restart == :transient and not normal?(reason))

    if restart do
      history = Enum.filter(state.history, &(&1 >= state.now - state.period))

      if length(history) >= state.intensity do
        {:ok,
         %{
           state
           | phase: :closed,
             children: Enum.map(stopped, &%{&1 | running: false}),
             history: history
         }, stopping(stopped) ++ [{:tree_stopped, :restart_intensity}]}
      else
        affected =
          case state.strategy do
            :one_for_one -> [child.id]
            :one_for_all -> Enum.map(stopped, & &1.id)
            :rest_for_one -> stopped |> Enum.drop_while(&(&1.id != child.id)) |> Enum.map(& &1.id)
          end

        selected = Enum.filter(stopped, &(&1.id in affected))
        restarting = selected |> Enum.reject(&(&1.restart == :temporary))
        commands = stopping(selected) ++ Enum.map(restarting, &{:start, &1.id, &1.generation + 1})

        next =
          stopped
          |> Enum.reject(&(&1.id in affected and &1.restart == :temporary))
          |> Enum.map(fn c ->
            if c.id in affected, do: %{c | generation: c.generation + 1, running: true}, else: c
          end)

        {:ok, %{state | children: next, history: [state.now | history]}, commands}
      end
    else
      next =
        if child.restart == :temporary,
          do: Enum.reject(stopped, &(&1.id == child.id)),
          else: stopped

      {:ok, %{state | children: next}, []}
    end
  end

  defp stopping(children),
    do:
      children
      |> Enum.reverse()
      |> Enum.filter(& &1.running)
      |> Enum.map(&{:stop, &1.id, &1.generation})

  defp normal?(reason), do: reason in [:normal, :shutdown] or match?({:shutdown, _}, reason)
end
