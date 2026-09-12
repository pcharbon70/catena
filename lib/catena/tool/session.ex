defmodule Catena.Tool.Session do
  @moduledoc "Owned retained-input session engine for the grammar-independent G120 slice."
  use GenServer

  alias Catena.{Effect.Row, Reference.Evaluator}

  @version "0.1.95"
  @maximum_capabilities 16
  @maximum_generations 32
  @maximum_history 256
  @default_budget 100_000
  @maximum_budget 10_000_000
  @cleanup_confirmation_timeout_ms 1_000

  def profile do
    %{
      version: @version,
      inputs: [:retained_json_ast, :retained_semantic_kernel],
      public_repl: :held_for_p109,
      session_ownership: :creating_process,
      capabilities: :explicit_closed_set,
      generation_replacement: :immutable_successive,
      evaluation: :reference_model_bounded,
      child_lifetime: :owned_until_result_interrupt_or_close,
      history: :bounded_metadata_redacted_by_default,
      governance: :external_admission_required,
      maximum_capabilities: @maximum_capabilities,
      maximum_generations_per_module: @maximum_generations,
      maximum_history_entries: @maximum_history,
      default_evaluation_steps: @default_budget,
      maximum_evaluation_steps: @maximum_budget,
      cleanup_confirmation_timeout_ms: @cleanup_confirmation_timeout_ms
    }
  end

  def open(capabilities \\ [], options \\ []) when is_list(capabilities) and is_list(options) do
    GenServer.start(__MODULE__, {self(), capabilities, options})
  end

  def load(session, source, options \\ []) when is_binary(source) and is_list(options),
    do: owner_call(session, {:load, source, options})

  def evaluate(session, module, name, arguments \\ [], options \\ [])
      when is_binary(name) and is_list(arguments) and is_list(options),
      do: owner_call(session, {:evaluate, module, name, arguments, options}, :infinity)

  def start_evaluation(session, module, name, arguments \\ [], options \\ [])
      when is_binary(name) and is_list(arguments) and is_list(options),
      do: owner_call(session, {:start, module, name, arguments, options})

  def await(handle, timeout \\ 5_000)

  def await({__MODULE__, owner, session, reference}, timeout)
      when owner == self() and is_integer(timeout) and timeout > 0,
      do: owner_call(session, {:await, reference}, timeout)

  def await(_, _), do: {:error, :invalid_session_job_owner}

  def interrupt({__MODULE__, owner, session, reference}) when owner == self(),
    do: owner_call(session, {:interrupt, reference})

  def interrupt(_), do: {:error, :invalid_session_job_owner}

  def history(session), do: owner_call(session, :history)
  def close(session), do: owner_call(session, :close)

  @impl true
  def init({owner, capabilities, options}) do
    Process.flag(:trap_exit, true)

    with :ok <- validate_options(options),
         {:ok, capabilities} <- validate_capabilities(capabilities),
         {:ok, budget} <- budget(Keyword.get(options, :evaluation_steps, @default_budget)),
         capture when is_boolean(capture) <- Keyword.get(options, :capture_values, false) do
      {:ok,
       %{
         owner: owner,
         owner_monitor: Process.monitor(owner),
         capabilities: capabilities,
         budget: budget,
         capture_values: capture,
         active: %{},
         generations: %{},
         next_generation: %{},
         jobs: %{},
         workers: %{},
         history: [],
         dropped_history: 0,
         sequence: 0
       }}
    else
      _ -> {:stop, :invalid_interactive_session}
    end
  end

  @impl true
  def handle_call({:await, reference}, {owner, _} = from, state) when owner == state.owner do
    case state.jobs[reference] do
      %{status: :done, result: result} ->
        {:reply, result, state}

      %{status: :pending, waiter: nil} = job ->
        {:noreply, put_in(state.jobs[reference], %{job | waiter: from})}

      %{status: :pending} ->
        {:reply, {:error, :session_job_already_waited}, state}

      _ ->
        {:reply, {:error, :invalid_session_job}, state}
    end
  end

  def handle_call(command, {owner, _}, state) when owner == state.owner,
    do: handle_owner_call(command, state)

  def handle_call(_, _, state), do: {:reply, {:error, :invalid_session_owner}, state}

  defp handle_owner_call({:load, source, options}, state) do
    with :ok <- validate_load_options(options),
         :ok <- governance_boundary(options),
         {:ok, core} <- load_core(source, Keyword.get(options, :format, :json_ast)),
         {:ok, module} <- module_name(core),
         :ok <- replacement_boundary(state, module, options) do
      number = Map.get(state.next_generation, module, 0)
      digest = digest(source)
      generation = %{module: module, number: number, digest: digest, core: core}
      key = {module, number}

      state =
        state
        |> put_in([:generations, key], generation)
        |> put_in([:active, module], key)
        |> put_in([:next_generation, module], number + 1)
        |> retain_generations(module)
        |> record(:load, %{module: module, generation: number, source_digest: digest}, :ok, false)

      {:reply, {:ok, generation_handle(state, generation)}, state}
    else
      {:error, _} = error -> {:reply, error, record(state, :load, %{}, error, false)}
      _ -> {:reply, {:error, :invalid_session_input}, state}
    end
  end

  defp handle_owner_call({:evaluate, module, name, arguments, options}, state) do
    with {:ok, generation} <- select_generation(state, module, options),
         {:ok, budget} <- budget(Keyword.get(options, :evaluation_steps, state.budget)),
         :ok <- evaluation_options(options),
         :ok <- admit_effects(generation.core, name, state.capabilities) do
      result = Evaluator.run_bounded(generation.core, name, arguments, budget)
      sensitive = Keyword.get(options, :sensitive, false)
      details = %{module: generation.module, generation: generation.number, definition: name}
      state = record(state, :evaluate, details, result, sensitive)
      {:reply, result, state}
    else
      {:error, _} = error ->
        {:reply, error, record(state, :evaluate, %{definition: name}, error, false)}
    end
  end

  defp handle_owner_call({:start, module, name, arguments, options}, state) do
    with {:ok, generation} <- select_generation(state, module, options),
         {:ok, budget} <- budget(Keyword.get(options, :evaluation_steps, state.budget)),
         :ok <- evaluation_options(options),
         :ok <- admit_effects(generation.core, name, state.capabilities) do
      reference = make_ref()
      manager = self()

      {worker, monitor} =
        spawn_monitor(fn ->
          result = Evaluator.run_bounded(generation.core, name, arguments, budget)
          send(manager, {:session_result, reference, result})
        end)

      job = %{
        worker: worker,
        monitor: monitor,
        status: :pending,
        result: nil,
        waiter: nil,
        sensitive: Keyword.get(options, :sensitive, false),
        details: %{module: generation.module, generation: generation.number, definition: name}
      }

      state =
        state
        |> put_in([:jobs, reference], job)
        |> put_in([:workers, monitor], reference)
        |> record(:start, job.details, :pending, job.sensitive)

      {:reply, {:ok, {__MODULE__, state.owner, self(), reference}}, state}
    else
      {:error, _} = error ->
        {:reply, error, record(state, :start, %{definition: name}, error, false)}
    end
  end

  defp handle_owner_call({:interrupt, reference}, state) do
    case state.jobs[reference] do
      %{status: :pending} = job ->
        case terminate_job(job) do
          :ok ->
            result = {:cancelled, :session_interrupt}
            state = finish_job(state, reference, job, result)
            {:reply, :ok, state}

          {:error, _} = error ->
            {:reply, error, state}
        end

      %{status: :done} ->
        {:reply, :already_completed, state}

      _ ->
        {:reply, {:error, :invalid_session_job}, state}
    end
  end

  defp handle_owner_call(:history, state) do
    {:reply, {:ok, %{entries: Enum.reverse(state.history), dropped: state.dropped_history}},
     state}
  end

  defp handle_owner_call(:close, state) do
    pending = for {reference, %{status: :pending} = job} <- state.jobs, do: {reference, job}

    {state, failed?} =
      Enum.reduce(pending, {state, false}, fn {reference, job}, {acc, failed?} ->
        case terminate_job(job) do
          :ok ->
            {finish_job(acc, reference, job, {:cancelled, :session_close}), failed?}

          {:error, _} ->
            {acc, true}
        end
      end)

    if failed? do
      {:reply, {:error, :session_cleanup_timeout}, state}
    else
      report = %{
        status: :closed,
        cleanup: :confirmed,
        cancelled_jobs: length(pending),
        generations: map_size(state.generations),
        history_entries: length(state.history),
        dropped_history: state.dropped_history
      }

      {:stop, :normal, {:ok, report}, state}
    end
  end

  @impl true
  def handle_info({:session_result, reference, result}, state) do
    case state.jobs[reference] do
      %{status: :pending} = job -> {:noreply, finish_job(state, reference, job, result)}
      _ -> {:noreply, state}
    end
  end

  def handle_info(
        {:DOWN, monitor, :process, owner, _},
        %{owner_monitor: monitor, owner: owner} = state
      ) do
    Enum.each(state.jobs, fn
      {_, %{status: :pending} = job} -> Process.exit(job.worker, :kill)
      _ -> :ok
    end)

    {:stop, :normal, state}
  end

  def handle_info({:DOWN, monitor, :process, _worker, reason}, state) do
    case Map.pop(state.workers, monitor) do
      {nil, _} ->
        {:noreply, state}

      {reference, workers} ->
        state = %{state | workers: workers}

        case state.jobs[reference] do
          %{status: :pending} = job ->
            {:noreply,
             finish_job(state, reference, job, {:error, {:session_worker_exit, reason}})}

          _ ->
            {:noreply, state}
        end
    end
  end

  def handle_info(_, state), do: {:noreply, state}

  defp finish_job(state, reference, job, result) do
    Process.demonitor(job.monitor, [:flush])
    if job.waiter, do: GenServer.reply(job.waiter, result)

    state
    |> Map.put(:workers, Map.delete(state.workers, job.monitor))
    |> put_in([:jobs, reference], %{job | status: :done, result: result, waiter: nil})
    |> record(:result, job.details, result, job.sensitive)
  end

  defp select_generation(state, module, options) do
    module = normalize_module(module)
    requested = Keyword.get(options, :generation)
    key = if is_nil(requested), do: state.active[module], else: {module, requested}

    case state.generations[key] do
      nil -> {:error, :stale_or_unknown_session_generation}
      generation -> {:ok, generation}
    end
  end

  defp admit_effects(core, name, capabilities) do
    case Enum.find(core.definitions, &(&1.name == name)) do
      nil ->
        {:error, {:unknown_definition, name}}

      definition ->
        row =
          Map.get(definition, :verified_uses_row) || Map.get(definition, :effect_row) ||
            Row.empty()

        cond do
          not is_nil(row.tail) -> {:error, :open_effect_row_not_interactive}
          Enum.all?(row.entries, &MapSet.member?(capabilities, &1.capability)) -> :ok
          true -> {:error, :interactive_capability_denied}
        end
    end
  end

  defp retain_generations(state, module) do
    keys =
      state.generations
      |> Map.keys()
      |> Enum.filter(&(elem(&1, 0) == module))
      |> Enum.sort_by(&elem(&1, 1), :desc)

    Enum.drop(keys, @maximum_generations)
    |> Enum.reduce(state, fn key, acc -> update_in(acc.generations, &Map.delete(&1, key)) end)
  end

  defp generation_handle(state, generation),
    do: {__MODULE__, state.owner, self(), generation.module, generation.number, generation.digest}

  defp replacement_boundary(state, module, options) do
    if Map.has_key?(state.active, module) and not Keyword.get(options, :replace, false),
      do: {:error, :session_replacement_requires_explicit_selection},
      else: :ok
  end

  defp governance_boundary(options) do
    if Keyword.get(options, :governed, false),
      do: {:error, :governance_action_requires_external_admission},
      else: :ok
  end

  defp record(state, action, details, result, sensitive) do
    capture = state.capture_values and not sensitive

    entry = %{
      sequence: state.sequence,
      action: action,
      details: details,
      outcome: classify(result),
      value: if(capture, do: captured_value(result), else: :redacted)
    }

    history = [entry | state.history]

    {history, dropped} =
      if length(history) > @maximum_history,
        do: {Enum.take(history, @maximum_history), state.dropped_history + 1},
        else: {history, state.dropped_history}

    %{state | history: history, dropped_history: dropped, sequence: state.sequence + 1}
  end

  defp classify(:ok), do: :ok
  defp classify(:pending), do: :pending
  defp classify({:ok, _, _}), do: :completed
  defp classify({:budget_exhausted, _}), do: :budget_exhausted
  defp classify({:cancelled, _}), do: :cancelled
  defp classify({:error, _}), do: :error
  defp classify(_), do: :other

  defp captured_value({:ok, value, _steps}), do: value
  defp captured_value(result), do: result

  defp validate_capabilities(capabilities) when length(capabilities) <= @maximum_capabilities do
    if Enum.all?(capabilities, &(is_binary(&1) and &1 != "")) and
         length(capabilities) == length(Enum.uniq(capabilities)),
       do: {:ok, MapSet.new(capabilities)},
       else: {:error, :invalid_session_capabilities}
  end

  defp validate_capabilities(_), do: {:error, :interactive_session_limit_exceeded}

  defp validate_options(options) do
    keys = if Keyword.keyword?(options), do: Keyword.keys(options), else: []

    if Keyword.keyword?(options) and length(keys) == length(Enum.uniq(keys)) and
         Enum.all?(keys, &(&1 in [:evaluation_steps, :capture_values])),
       do: :ok,
       else: {:error, :invalid_session_options}
  end

  defp validate_load_options(options) do
    keys = if Keyword.keyword?(options), do: Keyword.keys(options), else: []

    if Keyword.keyword?(options) and length(keys) == length(Enum.uniq(keys)) and
         Enum.all?(keys, &(&1 in [:replace, :governed, :format])) and
         is_boolean(Keyword.get(options, :replace, false)) and
         is_boolean(Keyword.get(options, :governed, false)) and
         Keyword.get(options, :format, :json_ast) in [:json_ast, :kernel],
       do: :ok,
       else: {:error, :invalid_session_options}
  end

  defp evaluation_options(options) do
    keys = if Keyword.keyword?(options), do: Keyword.keys(options), else: []

    if Keyword.keyword?(options) and length(keys) == length(Enum.uniq(keys)) and
         Enum.all?(keys, &(&1 in [:generation, :evaluation_steps, :sensitive])) and
         is_boolean(Keyword.get(options, :sensitive, false)),
       do: :ok,
       else: {:error, :invalid_session_options}
  end

  defp budget(value) when is_integer(value) and value > 0 and value <= @maximum_budget,
    do: {:ok, value}

  defp budget(_), do: {:error, :interactive_session_limit_exceeded}

  defp module_name(%{module: module}) when is_atom(module), do: {:ok, Atom.to_string(module)}
  defp module_name(%{module: module}) when is_binary(module) and module != "", do: {:ok, module}
  defp module_name(_), do: {:error, :invalid_session_module}
  defp normalize_module(module) when is_atom(module), do: Atom.to_string(module)
  defp normalize_module(module) when is_binary(module), do: module
  defp normalize_module(_), do: nil

  defp load_core(source, :json_ast), do: Catena.check_json(source)
  defp load_core(source, :kernel), do: Catena.check_kernel(source)

  defp owner_call(session, command, timeout \\ 5_000)

  defp owner_call(session, command, timeout) when is_pid(session) do
    GenServer.call(session, command, timeout)
  catch
    :exit, _ -> {:error, :closed_or_unavailable_session}
  end

  defp owner_call(_, _, _), do: {:error, :invalid_session}

  defp terminate_job(job) do
    Process.exit(job.worker, :kill)

    receive do
      {:DOWN, monitor, :process, worker, _}
      when monitor == job.monitor and worker == job.worker ->
        :ok
    after
      @cleanup_confirmation_timeout_ms -> {:error, :session_cleanup_timeout}
    end
  end

  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
