defmodule Catena.Task.Monitor do
  @moduledoc "Experimental scoped monitor adapter with bounded typed outcome roles."
  alias Catena.Task.Runtime

  @roles %{
    completed: :unit,
    trapped: :integer,
    cancelled: :integer,
    exited: :unit,
    absent: :unit,
    runtime_failure: :unit,
    external_loss: :unit
  }

  def valid_labels?(labels) when is_map(labels) do
    Enum.sort(Map.keys(labels)) == Enum.sort(Map.keys(@roles)) and
      Enum.all?(Map.values(labels), &(is_binary(&1) and &1 != "" and String.valid?(&1))) and
      length(Enum.uniq(Map.values(labels))) == map_size(labels)
  end

  def valid_labels?(_), do: false

  def type(labels),
    do:
      {:variant,
       %{
         fields: Map.new(@roles, fn {role, type} -> {Map.fetch!(labels, role), type} end),
         tail: nil
       }}

  def start(scope, {Catena.Task.Managed, _} = target),
    do: start(scope, Catena.Task.Managed.pid(target))

  def start(scope, target) do
    unless is_pid(target), do: :erlang.error({:catena_trap, :invalid_task_monitor_target})

    handle =
      Runtime.start_observer(scope, fn ->
        monitor = Process.monitor(target)
        Runtime.observer_ready()
        token = Process.get({Runtime, :worker})

        try do
          receive do
            {:DOWN, ^monitor, :process, ^target, reason} -> classify_exit(reason)
            {^token, :cancel, _} -> :abandoned
          end
        after
          Process.demonitor(monitor, [:flush])
        end
      end)

    :unit = Runtime.scope_call(scope, {:ready, handle})
    handle
  end

  def observe({Runtime, owner, manager, token, _} = monitor, labels),
    do: observe({Runtime, owner, manager, token}, monitor, labels)

  def demonitor({Runtime, owner, manager, token, _} = monitor),
    do: demonitor({Runtime, owner, manager, token}, monitor)

  def observe(scope, monitor, labels) do
    case Runtime.scope_call(scope, {:observe, monitor}) do
      {:role, role, payload} -> {:catena_variant, Map.fetch!(labels, role), payload}
      _ -> :erlang.error({:catena_trap, :inactive_task_monitor})
    end
  end

  def demonitor(scope, monitor), do: Runtime.scope_call(scope, {:demonitor, monitor})

  def classify_exit({:catena_managed_trap, reason}) when is_integer(reason),
    do: {:role, :trapped, reason}

  def classify_exit({:catena_managed_cancelled, reason}) when is_integer(reason),
    do: {:role, :cancelled, reason}

  def classify_exit({:catena_managed_exit, :shutdown_deadline_exhausted}),
    do: {:role, :external_loss, :unit}

  def classify_exit({:catena_managed_exit, _}), do: {:role, :exited, :unit}
  def classify_exit(reason), do: classify(reason)

  defp classify(:normal), do: {:role, :completed, :unit}
  defp classify(:noproc), do: {:role, :absent, :unit}
  defp classify(:killed), do: {:role, :external_loss, :unit}

  defp classify({{:catena_trap, reason}, _stack}) when is_integer(reason),
    do: {:role, :trapped, reason}

  defp classify({{:nocatch, {:catena_resource_cancelled, reason}}, _stack})
       when is_integer(reason),
       do: {:role, :cancelled, reason}

  defp classify({:catena_resource_exit, _}), do: {:role, :exited, :unit}
  defp classify(_), do: {:role, :runtime_failure, :unit}
end
