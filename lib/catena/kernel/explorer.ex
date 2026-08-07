defmodule Catena.Kernel.Explorer do
  @moduledoc "Bounded exhaustive scheduler exploration for kernel actor configurations."

  alias Catena.Kernel.Stepper

  @default_transition_limit 20_000
  @default_configuration_limit 20_000

  @spec explore(map(), String.t(), [term()], keyword()) ::
          {:ok, map()} | {:exhausted, map()} | {:error, term()}
  def explore(core, name, arguments \\ [], options \\ []) do
    with {:ok, initial} <- Stepper.initial(core, name, arguments) do
      transition_limit = Keyword.get(options, :transition_limit, @default_transition_limit)

      configuration_limit =
        Keyword.get(options, :configuration_limit, @default_configuration_limit)

      hash = state_hash(initial)

      search(
        :queue.in(initial, :queue.new()),
        MapSet.new([hash]),
        MapSet.new(),
        0,
        transition_limit,
        configuration_limit,
        false
      )
    end
  end

  defp search(
         queue,
         seen,
         outcomes,
         transitions,
         transition_limit,
         configuration_limit,
         exhausted
       ) do
    cond do
      :queue.is_empty(queue) ->
        result = result(seen, outcomes, transitions)
        if exhausted, do: {:exhausted, result}, else: {:ok, result}

      transitions >= transition_limit or MapSet.size(seen) >= configuration_limit ->
        result = result(seen, outcomes, transitions)
        {:exhausted, result}

      true ->
        {{:value, configuration}, queue} = :queue.out(queue)

        case Stepper.runnable_pids(configuration) do
          [] ->
            outcome = configuration |> Stepper.outcome() |> normalize_outcome()

            search(
              queue,
              seen,
              MapSet.put(outcomes, outcome),
              transitions,
              transition_limit,
              configuration_limit,
              exhausted
            )

          pids ->
            {queue, seen, transitions, exhausted} =
              Enum.reduce(pids, {queue, seen, transitions, exhausted}, fn pid,
                                                                          {queue, seen,
                                                                           transitions, exhausted} ->
                if transitions >= transition_limit or MapSet.size(seen) >= configuration_limit do
                  {queue, seen, transitions, true}
                else
                  {:ok, next} = Stepper.step(configuration, pid)
                  hash = state_hash(next)

                  if MapSet.member?(seen, hash) do
                    {queue, seen, transitions + 1, exhausted}
                  else
                    {:queue.in(next, queue), MapSet.put(seen, hash), transitions + 1, exhausted}
                  end
                end
              end)

            search(
              queue,
              seen,
              outcomes,
              transitions,
              transition_limit,
              configuration_limit,
              exhausted
            )
        end
    end
  end

  defp result(seen, outcomes, transitions) do
    %{
      configurations: MapSet.size(seen),
      transitions: transitions,
      outcomes: outcomes |> MapSet.to_list() |> Enum.sort()
    }
  end

  defp normalize_outcome(outcome) do
    Map.take(outcome, [:root_status, :root_result, :root_trap, :processes, :trace])
  end

  defp state_hash(configuration) do
    configuration
    |> Map.drop([:core, :definitions, :steps])
    |> :erlang.term_to_binary([:deterministic])
    |> then(&:crypto.hash(:sha256, &1))
  end
end
