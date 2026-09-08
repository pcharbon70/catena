defmodule Catena.Protocol.Model do
  @moduledoc "Independent transition model for the explicit local protocol contract."

  def new(schema, capacity) when is_binary(schema) and is_integer(capacity) and capacity > 0 do
    %{
      schema: schema,
      capacity: capacity,
      phase: :unnegotiated,
      next: 0,
      now: 0,
      pending: %{},
      completed: %{}
    }
  end

  def step(%{phase: :unnegotiated} = state, {:negotiate, peer_schema}) do
    if peer_schema == state.schema do
      {:ok, %{state | phase: :ready}, []}
    else
      {:ok, %{state | phase: :closed}, [{:refused, :schema_mismatch}]}
    end
  end

  def step(%{phase: :ready} = state, {:request, reply_to, payload, duration})
      when is_integer(duration) and duration >= 0 do
    if map_size(state.pending) + map_size(state.completed) >= state.capacity do
      {:ok, state, [{:refused, reply_to, :overloaded}]}
    else
      id = state.next
      request = %{reply_to: reply_to, deadline: state.now + duration}

      {:ok, %{state | next: id + 1, pending: Map.put(state.pending, id, request)},
       [{:transmit, id, payload}]}
    end
  end

  def step(state, {:advance, now}) when is_integer(now) and now >= state.now,
    do: {:ok, %{state | now: now}, []}

  def step(%{phase: :ready} = state, {:reply, id, payload}),
    do: finish(state, id, {:replied, payload})

  def step(%{phase: :ready} = state, {:invalid_response, id}),
    do: finish(state, id, :invalid_response)

  def step(%{phase: :closed} = state, :dispose),
    do: {:ok, %{state | completed: %{}}, []}

  def step(%{phase: :ready} = state, {:cancel, id}),
    do: finish(state, id, :cancelled)

  def step(%{phase: :ready} = state, {:expire, id}) do
    case state.pending[id] do
      %{deadline: deadline} when deadline <= state.now -> finish(state, id, :timed_out)
      %{} -> {:error, :not_due}
      nil -> missing(state, id)
    end
  end

  def step(state, {:observe, id}) do
    case Map.pop(state.completed, id) do
      {nil, _} -> {:error, :not_completed}
      {outcome, completed} -> {:ok, %{state | completed: completed}, [{:observed, id, outcome}]}
    end
  end

  def step(%{phase: :ready} = state, :peer_lost), do: close(state, :peer_lost)
  def step(%{phase: :ready} = state, :close), do: close(state, :cancelled)
  def step(_state, _event), do: {:error, :invalid_transition}

  defp finish(state, id, outcome) do
    case Map.pop(state.pending, id) do
      {nil, _} ->
        missing(state, id)

      {%{reply_to: reply_to}, pending} ->
        {:ok, %{state | pending: pending, completed: Map.put(state.completed, id, outcome)},
         [{:complete, reply_to, id, outcome}]}
    end
  end

  defp missing(state, id) when is_integer(id) and id >= 0 and id < state.next,
    do: {:error, :already_terminal}

  defp missing(_, _), do: {:error, :unknown_correlation}

  defp close(state, outcome) do
    commands =
      state.pending
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map(fn {id, request} ->
        {:complete, request.reply_to, id, outcome}
      end)

    completed =
      Enum.reduce(state.pending, state.completed, fn {id, _}, acc -> Map.put(acc, id, outcome) end)

    {:ok, %{state | phase: :closed, pending: %{}, completed: completed}, commands}
  end
end
