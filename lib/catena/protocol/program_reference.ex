defmodule Catena.Protocol.ProgramReference do
  @moduledoc "Independent explicit-event evaluator for checked protocol library applications."
  alias Catena.Protocol.{Contract, Model, Program}

  def run(program, events \\ %{}, options \\ []) do
    with :ok <- Program.verify(program) do
      schema = Keyword.get(options, :peer_schema, program.contract.digest)

      case Model.step(Model.new(program.contract.digest, program.capacity), {:negotiate, schema}) do
        {:ok, %{phase: :ready} = model, []} ->
          run_operations(
            program.operations,
            program,
            events,
            %{model: model, slots: %{}, outputs: []},
            0
          )

        {:ok, _, _} ->
          {:ok, variant("unavailable", variant("schema_mismatch", :unit))}
      end
    end
  end

  defp run_operations([], _program, _events, state, _index) do
    closed =
      case state.model.phase do
        :closed ->
          state.model

        :ready ->
          {:ok, closed, _} = Model.step(state.model, :close)
          closed
      end

    {:ok, disposed, []} = Model.step(closed, :dispose)
    true = disposed.pending == %{} and disposed.completed == %{}
    value = if state.outputs == [], do: :unit, else: List.to_tuple(state.outputs)
    {:ok, variant("completed", value)}
  end

  defp run_operations([operation | rest], program, events, state, index) do
    with {:ok, state} <- operate(operation, program, state),
         {:ok, model} <- deliver(state.model, Map.get(events, index, []), program.contract) do
      run_operations(rest, program, events, %{state | model: model}, index + 1)
    else
      {:waiting, _} -> {:waiting, %{index: index, state: state}}
      error -> error
    end
  end

  defp operate({:submit, key, producer, duration}, program, state) do
    with {:ok, payload, _evidence} <- Catena.Kernel.Stepper.run(program.core, producer) do
      case Model.step(state.model, {:request, key, payload, duration}) do
        {:ok, model, [{:transmit, id, _}]} ->
          {:ok, %{state | model: model, slots: Map.put(state.slots, key, {:pending, id})}}

        {:ok, model, [{:refused, _, reason}]} ->
          {:ok, %{state | model: model, slots: Map.put(state.slots, key, {:refused, reason})}}

        {:error, :invalid_transition} ->
          {:ok, %{state | slots: Map.put(state.slots, key, {:refused, :session_lost})}}
      end
    else
      error -> {:error, {:producer_evaluation, error}}
    end
  end

  defp operate({:await, key}, _program, state) do
    case state.slots[key] do
      {:refused, reason} ->
        {:ok, output(state, variant(Atom.to_string(reason), :unit))}

      {:observed, _} ->
        {:ok, output(state, variant("invalid_request_handle", :unit))}

      {:pending, id} ->
        case Model.step(state.model, {:observe, id}) do
          {:ok, model, [{:observed, ^id, result}]} ->
            next = %{state | model: model, slots: Map.put(state.slots, key, {:observed, id})}
            {:ok, output(next, outcome(result))}

          {:error, :not_completed} ->
            {:waiting, id}
        end
    end
  end

  defp operate({:cancel, key}, _program, state) do
    case state.slots[key] do
      {:refused, reason} ->
        {:ok, output(state, variant(Atom.to_string(reason), :unit))}

      {:observed, _} ->
        {:ok, output(state, variant("invalid_request_handle", :unit))}

      {:pending, id} ->
        case Model.step(state.model, {:cancel, id}) do
          {:ok, model, _} ->
            {:ok, output(%{state | model: model}, variant("cancel_selected", :unit))}

          {:error, :already_terminal} ->
            {:ok, output(state, variant("already_terminal", :unit))}

          {:error, :invalid_transition} ->
            {:ok, output(state, variant("session_lost", :unit))}
        end
    end
  end

  defp deliver(model, events, contract) do
    Enum.reduce_while(events, {:ok, model}, fn event, {:ok, current} ->
      event =
        case event do
          {:reply, id, payload} ->
            if Contract.valid_payload?(contract, :response, payload),
              do: event,
              else: {:invalid_response, id}

          event ->
            event
        end

      case Model.step(current, event) do
        {:ok, next, _} ->
          {:cont, {:ok, next}}

        {:error, reason} when reason in [:already_terminal, :unknown_correlation] ->
          {:cont, {:ok, current}}

        {:error, :invalid_transition} when current.phase == :closed ->
          {:cont, {:ok, current}}

        error ->
          {:halt, error}
      end
    end)
  end

  defp outcome({:replied, value}), do: variant("reply", canonical(value))
  defp outcome(reason), do: variant(Atom.to_string(reason), :unit)
  defp output(state, value), do: %{state | outputs: state.outputs ++ [value]}
  defp variant(label, value), do: {:catena_variant, label, value}

  def canonical({:catena_variant, label, value}) when is_atom(label),
    do: variant(Atom.to_string(label), canonical(value))

  def canonical(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&canonical/1) |> List.to_tuple()

  def canonical(list) when is_list(list), do: Enum.map(list, &canonical/1)

  def canonical(value) when is_map(value),
    do: Map.new(value, fn {key, item} -> {key, canonical(item)} end)

  def canonical(value), do: value
end
