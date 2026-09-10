defmodule Catena.Scheduler do
  @moduledoc "Explicit scheduler policy and bounded foreign-work classification."

  alias Catena.Foreign.Descriptor

  @version "0.1.78"
  @max_foreign_workers 1_024

  def profile do
    %{
      version: @version,
      deterministic_scheduling: false,
      fairness_guarantee: false,
      reduction_preemption: :runtime_defined,
      priorities: [:low, :normal, :high],
      priority_semantics: :deployment_policy,
      work_classes: [:preemptible, :scheduled_blocking, :unsafe_unbounded],
      unsafe_unbounded_admitted: false,
      max_foreign_workers: @max_foreign_workers,
      reduction_count_observable: false
    }
  end

  def new(options \\ []) do
    priority = Keyword.get(options, :priority, :normal)
    capacity = Keyword.get(options, :foreign_workers)

    if Keyword.keys(options) -- [:priority, :foreign_workers] == [] and
         priority in profile().priorities and is_integer(capacity) and
         capacity in 1..@max_foreign_workers do
      {:ok, %{priority: priority, capacity: capacity, workers: %{}}}
    else
      {:error, :invalid_scheduler_policy}
    end
  end

  def classify(:catena), do: {:ok, :preemptible}

  def classify({:foreign, descriptor}) do
    with :ok <- Descriptor.verify(descriptor),
         true <- descriptor.scheduler == :owned_process do
      {:ok, :scheduled_blocking}
    else
      _ -> {:error, :unclassified_foreign_work}
    end
  end

  def classify({:native, :port}), do: {:ok, :scheduled_blocking}
  def classify({:native, :nif}), do: {:ok, :scheduled_blocking}
  def classify({:unsafe, _}), do: {:ok, :unsafe_unbounded}
  def classify(_), do: {:error, :unclassified_foreign_work}

  def reserve(policy, work) do
    with {:ok, class} <- classify(work) do
      case class do
        :preemptible -> {:ok, :runtime, policy}
        :unsafe_unbounded -> {:error, :unsafe_unbounded_work, policy}
        :scheduled_blocking -> reserve_worker(policy)
      end
    else
      {:error, reason} -> {:error, reason, policy}
    end
  end

  def release(%{workers: workers} = policy, token) do
    if Map.has_key?(workers, token),
      do: {:ok, %{policy | workers: Map.delete(workers, token)}},
      else: {:error, :invalid_scheduler_token, policy}
  end

  def validate_profile(profile),
    do: if(profile == profile(), do: :ok, else: {:error, :invalid_scheduler_profile})

  defp reserve_worker(%{capacity: capacity, workers: workers} = policy) do
    if map_size(workers) < capacity do
      token = make_ref()
      {:ok, token, %{policy | workers: Map.put(workers, token, :scheduled_blocking)}}
    else
      {:error, :foreign_worker_capacity_exhausted, policy}
    end
  end
end
