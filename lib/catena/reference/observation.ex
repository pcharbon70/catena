defmodule Catena.Reference.Observation do
  @moduledoc "Common bounded observation contract over Catena's independent reference machines."

  alias Catena.Effect.Runtime
  alias Catena.Kernel.{Explorer, Stepper}
  alias Catena.Reference.Evaluator
  alias Catena.Resource

  @version "0.1.83"
  @engines ~w(expression effect kernel schedules resource foreign external source)a
  @statuses ~w(completed trapped exited cancelled quiescent budget_exhausted host_timeout rejected unsupported)a
  @max_fuel 10_000_000
  @max_host_timeout_ms 5_000
  @max_external_responses 4_096

  def profile do
    %{
      version: @version,
      engines: @engines,
      statuses: @statuses,
      semantic_fuel: @max_fuel,
      host_timeout_ms: @max_host_timeout_ms,
      external_responses: @max_external_responses,
      public_source: :held_for_p109,
      agreement_is_proof: false
    }
  end

  def observe(engine, subject, options \\ [])

  def observe(:expression, %{core: core, entry: entry} = subject, options) do
    arguments = Map.get(subject, :arguments, [])

    with {:ok, fuel} <- bound(options, :fuel, @max_fuel),
         true <- is_binary(entry) and is_list(arguments) do
      {result, events} =
        Runtime.capture_trace(fn -> Evaluator.run_bounded(core, entry, arguments, fuel) end)

      {:ok, expression_result(result, events, fuel)}
    else
      _ -> {:error, :invalid_reference_request}
    end
  end

  def observe(:effect, %{core: core, entry: entry} = subject, options) do
    arguments = Map.get(subject, :arguments, [])

    with {:ok, timeout} <- bound(options, :host_timeout_ms, @max_host_timeout_ms),
         true <- is_binary(entry) and is_list(arguments) do
      task =
        Task.async(fn ->
          Runtime.capture_trace(fn -> Evaluator.run(core, entry, arguments) end)
        end)

      case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
        {:ok, {result, events}} ->
          {:ok, effect_result(result, events, timeout)}

        nil ->
          {:ok,
           record(:effect, :host_timeout,
             reason: :host_deadline,
             bounds: %{host_timeout_ms: timeout}
           )}
      end
    else
      _ -> {:error, :invalid_reference_request}
    end
  end

  def observe(:kernel, %{core: core, entry: entry} = subject, options) do
    arguments = Map.get(subject, :arguments, [])

    with {:ok, fuel} <- bound(options, :fuel, @max_fuel),
         true <- is_binary(entry) and is_list(arguments) do
      {:ok,
       kernel_result(safely(fn -> Stepper.run(core, entry, arguments, budget: fuel) end), fuel)}
    else
      _ -> {:error, :invalid_reference_request}
    end
  end

  def observe(:schedules, %{core: core, entry: entry} = subject, options) do
    arguments = Map.get(subject, :arguments, [])

    with {:ok, transitions} <- bound(options, :transition_limit, @max_fuel),
         {:ok, configurations} <- bound(options, :configuration_limit, @max_fuel),
         true <- is_binary(entry) and is_list(arguments) do
      result =
        safely(fn ->
          Explorer.explore(core, entry, arguments,
            transition_limit: transitions,
            configuration_limit: configurations
          )
        end)

      {:ok, schedule_result(result, transitions, configurations)}
    else
      _ -> {:error, :invalid_reference_request}
    end
  end

  def observe(:resource, %{owner: owner, events: events}, options)
      when is_binary(owner) and owner != "" and is_list(events) do
    with true <- length(events) <= @max_external_responses,
         {:ok, transitions} <- bound(options, :transition_limit, @max_fuel) do
      result =
        Resource.Explorer.explore(Resource.Lifecycle.new(owner), events,
          transition_limit: transitions
        )

      {:ok, resource_result(result, transitions)}
    else
      _ -> {:error, :invalid_reference_request}
    end
  end

  def observe(:foreign, %{codec: codec, native: native, limits: limits}, _options) do
    case Catena.Foreign.Codec.from_native(codec, native, limits) do
      {:ok, semantic} ->
        {:ok, record(:foreign, :completed, value: semantic, bounds: limits)}

      {:error, reason} ->
        {:ok, record(:foreign, :rejected, reason: reason, bounds: limits)}
    end
  end

  def observe(:external, %{request: request, responses: responses}, _options)
      when is_list(responses) and length(responses) <= @max_external_responses do
    case Enum.find(responses, &(Map.get(&1, :request) == request)) do
      %{result: {:ok, value}} -> {:ok, record(:external, :completed, value: value)}
      %{result: {:trap, reason}} -> {:ok, record(:external, :trapped, reason: reason)}
      %{result: {:exit, reason}} -> {:ok, record(:external, :exited, reason: reason)}
      %{result: :host_timeout} -> {:ok, record(:external, :host_timeout, reason: :host_deadline)}
      nil -> {:ok, record(:external, :unsupported, reason: :unmodelled_external_response)}
      _ -> {:ok, record(:external, :rejected, reason: :invalid_external_response)}
    end
  end

  def observe(:source, %{format: :kernel, text: text, entry: entry} = subject, options)
      when is_binary(text) and is_binary(entry) do
    case Catena.check_kernel(text) do
      {:ok, core} ->
        observe(
          :kernel,
          %{core: core, entry: entry, arguments: Map.get(subject, :arguments, [])},
          options
        )

      {:error, reason} ->
        {:ok, record(:source, :rejected, reason: reason)}
    end
  end

  def observe(:source, %{format: :public}, _options),
    do: {:ok, record(:source, :unsupported, reason: :public_source_grammar_held_for_p109)}

  def observe(engine, _subject, _options) when engine in @engines,
    do: {:error, :invalid_reference_request}

  def observe(_engine, _subject, _options),
    do: {:ok, record(:unknown, :unsupported, reason: :unknown_reference_engine)}

  defp expression_result({:ok, value, steps}, events, fuel),
    do:
      record(:expression, :completed,
        value: value,
        events: events,
        bounds: %{fuel: fuel, steps: steps}
      )

  defp expression_result({:error, reason, steps}, events, fuel),
    do:
      record(:expression, :rejected,
        reason: reason,
        events: events,
        bounds: %{fuel: fuel, steps: steps}
      )

  defp expression_result({:budget_exhausted, steps}, events, fuel),
    do:
      record(:expression, :budget_exhausted, events: events, bounds: %{fuel: fuel, steps: steps})

  defp effect_result({:ok, value}, events, timeout),
    do:
      record(:effect, :completed,
        value: value,
        events: events,
        bounds: %{host_timeout_ms: timeout}
      )

  defp effect_result({:error, reason}, events, timeout),
    do:
      record(:effect, :rejected,
        reason: reason,
        events: events,
        bounds: %{host_timeout_ms: timeout}
      )

  defp effect_result(other, events, timeout),
    do:
      record(:effect, :rejected,
        reason: {:invalid_result, other},
        events: events,
        bounds: %{host_timeout_ms: timeout}
      )

  defp kernel_result({:ok, value, outcome}, fuel),
    do:
      record(:kernel, :completed,
        value: value,
        events: outcome.trace,
        lifetime: lifetime(outcome),
        bounds: %{fuel: fuel, steps: outcome.steps}
      )

  defp kernel_result({:trap, reason, outcome}, fuel),
    do:
      record(:kernel, :trapped,
        reason: reason,
        events: outcome.trace,
        lifetime: lifetime(outcome),
        bounds: %{fuel: fuel, steps: outcome.steps}
      )

  defp kernel_result({:exited, reason, outcome}, fuel),
    do:
      record(:kernel, :exited,
        reason: reason,
        events: outcome.trace,
        lifetime: lifetime(outcome),
        bounds: %{fuel: fuel, steps: outcome.steps}
      )

  defp kernel_result({:cancelled, reason, outcome}, fuel),
    do:
      record(:kernel, :cancelled,
        reason: reason,
        events: outcome.trace,
        lifetime: lifetime(outcome),
        bounds: %{fuel: fuel, steps: outcome.steps}
      )

  defp kernel_result({:quiescent, outcome}, fuel),
    do:
      record(:kernel, :quiescent,
        events: outcome.trace,
        lifetime: lifetime(outcome),
        bounds: %{fuel: fuel, steps: outcome.steps}
      )

  defp kernel_result({:budget_exhausted, outcome}, fuel),
    do:
      record(:kernel, :budget_exhausted,
        events: outcome.trace,
        lifetime: lifetime(outcome),
        bounds: %{fuel: fuel, steps: outcome.steps}
      )

  defp kernel_result({:error, reason}, fuel),
    do: record(:kernel, :rejected, reason: reason, bounds: %{fuel: fuel})

  defp kernel_result({:raised, reason}, fuel),
    do: record(:kernel, :rejected, reason: reason, bounds: %{fuel: fuel})

  defp schedule_result({kind, result}, transitions, configurations)
       when kind in [:ok, :exhausted] do
    status = if kind == :ok, do: :completed, else: :budget_exhausted

    record(:schedules, status,
      value: %{outcomes: result.outcomes, configurations: result.configurations},
      bounds: %{
        transition_limit: transitions,
        configuration_limit: configurations,
        transitions: result.transitions
      }
    )
  end

  defp schedule_result({:error, reason}, transitions, configurations),
    do:
      record(:schedules, :rejected,
        reason: reason,
        bounds: %{transition_limit: transitions, configuration_limit: configurations}
      )

  defp schedule_result({:raised, reason}, transitions, configurations),
    do:
      record(:schedules, :rejected,
        reason: reason,
        bounds: %{transition_limit: transitions, configuration_limit: configurations}
      )

  defp resource_result({kind, result}, transitions) when kind in [:ok, :exhausted] do
    status = if kind == :ok, do: :completed, else: :budget_exhausted
    events = Enum.map(result.terminals, & &1.trace)
    lifetime = Enum.map(result.terminals, &resource_lifetime/1)

    record(:resource, status,
      events: events,
      lifetime: lifetime,
      bounds: %{transition_limit: transitions, transitions: result.transitions}
    )
  end

  defp resource_result({:error, reason}, transitions),
    do: record(:resource, :rejected, reason: reason, bounds: %{transition_limit: transitions})

  defp lifetime(outcome),
    do: Enum.map(outcome.processes, &Map.take(&1, [:pid, :status, :result, :trap]))

  defp resource_lifetime(state) do
    %{
      owner_lost: state.lost,
      scopes: Map.new(state.scopes, fn {id, scope} -> {id, scope.phase} end),
      resources: Map.new(state.resources, fn {id, resource} -> {id, resource.status} end),
      completed: state.completed
    }
  end

  defp record(engine, status, fields) do
    %{
      format: :catena_reference_observation,
      version: @version,
      engine: engine,
      status: status,
      value: Keyword.get(fields, :value),
      reason: Keyword.get(fields, :reason),
      events: Keyword.get(fields, :events, []),
      lifetime: Keyword.get(fields, :lifetime, []),
      bounds: Keyword.get(fields, :bounds, %{})
    }
  end

  defp bound(options, key, maximum) do
    value = Keyword.get(options, key, maximum)
    if is_integer(value) and value > 0 and value <= maximum, do: {:ok, value}, else: :error
  end

  defp safely(function) do
    function.()
  rescue
    error -> {:raised, {error.__struct__, Exception.message(error)}}
  catch
    kind, reason -> {:raised, {kind, reason}}
  end
end
