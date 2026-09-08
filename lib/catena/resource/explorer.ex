defmodule Catena.Resource.Explorer do
  @moduledoc "Bounded event-schedule exploration for the bounded lifecycle model."
  alias Catena.Resource.Lifecycle

  def explore(state, events, options \\ []) do
    limit = Keyword.get(options, :transition_limit, 1000)

    if is_integer(limit) and limit > 0 and is_list(events) do
      walk([{state, events}], MapSet.new(), [], 0, limit)
    else
      {:error, :invalid_exploration_bounds}
    end
  end

  defp walk([], _, terminals, count, _),
    do: {:ok, %{terminals: Enum.reverse(terminals), transitions: count}}

  defp walk(queue, _, terminals, count, limit) when count >= limit,
    do:
      {:exhausted,
       %{terminals: Enum.reverse(terminals), transitions: count, pending: length(queue)}}

  defp walk([{state, pending} | queue], seen, terminals, count, limit) do
    key = {Map.drop(state, [:trace]), Enum.sort(pending)}

    if MapSet.member?(seen, key) do
      walk(queue, seen, terminals, count, limit)
    else
      seen = MapSet.put(seen, key)

      enabled =
        pending
        |> Enum.with_index()
        |> Enum.flat_map(fn {event, index} ->
          case Lifecycle.step(state, event) do
            {:ok, next} -> [{next, List.delete_at(pending, index)}]
            {:error, _} -> []
          end
        end)

      cond do
        enabled == [] ->
          walk(queue, seen, [state | terminals], count, limit)

        count + length(enabled) > limit ->
          {:exhausted,
           %{terminals: Enum.reverse(terminals), transitions: count, pending: length(queue) + 1}}

        true ->
          walk(enabled ++ queue, seen, terminals, count + length(enabled), limit)
      end
    end
  end
end
