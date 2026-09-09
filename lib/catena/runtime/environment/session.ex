defmodule Catena.Runtime.Environment.Session do
  @moduledoc false
  use GenServer
  alias Catena.Runtime.Environment.{Authority, Policy, Schema, Adapter}
  alias Catena.Foreign.{Budget, Codec}

  def start(owner, grants, limits, release, options) do
    GenServer.start(__MODULE__, {owner, grants, limits, release, options})
  end

  def init({owner, grants, limits, release, options}) do
    Process.flag(:trap_exit, true)
    timeout = Keyword.get(options, :timeout_ms, 1000)
    maximum = Keyword.get(options, :max_requests, 1024)
    parallel = Keyword.get(options, :max_inflight, 8)
    fake = Keyword.get(options, :fake, nil)

    valid =
      is_list(grants) and length(grants) <= 8 and Enum.all?(grants, &(Policy.verify(&1) == :ok)) and
        length(Enum.uniq_by(grants, & &1.service)) == length(grants) and Budget.valid?(limits) and
        is_integer(timeout) and timeout in 1..1_000_000 and is_integer(maximum) and
        maximum in 1..10000 and
        is_integer(parallel) and parallel in 1..64 and (is_nil(fake) or is_list(fake)) and
        length(Keyword.keys(options)) == length(Enum.uniq(Keyword.keys(options))) and
        (is_nil(fake) or length(fake) <= maximum) and
        Enum.all?(
          Keyword.keys(options),
          &(&1 in [:timeout_ms, :max_requests, :max_inflight, :fake])
        )

    if valid do
      ledger =
        Map.new(grants, fn grant ->
          nonce = :crypto.strong_rand_bytes(32)
          authority = Authority.issue(grant.service, self(), owner, nonce)

          {nonce,
           %{
             grant: grant,
             authority: authority,
             expires: now() + grant.policy.ttl_ms,
             ancestors: [],
             revoked: false
           }}
        end)

      bundle = %{
        format: :environment_bundle,
        version: "0.1.68",
        manager: self(),
        authorities: Map.new(ledger, fn {_, entry} -> {entry.grant.service, entry.authority} end)
      }

      {:ok,
       %{
         owner: owner,
         owner_monitor: Process.monitor(owner),
         release: release,
         limits: limits,
         timeout: timeout,
         maximum: maximum,
         parallel: parallel,
         fake: fake,
         ledger: ledger,
         bundle: bundle,
         jobs: %{},
         workers: %{},
         next: 0,
         events: [],
         closing: false,
         cleanup_failure: false,
         closers: []
       }}
    else
      {:stop, :invalid_environment_scope}
    end
  end

  def handle_call({:bundle, owner}, {owner, _}, %{owner: owner} = state),
    do: {:reply, {:ok, state.bundle}, state}

  def handle_call(:events, {owner, _}, %{owner: owner} = state),
    do: {:reply, {:ok, Enum.reverse(state.events)}, state}

  def handle_call({:close, release}, from, %{release: release} = state) do
    state = %{state | closing: true, closers: [from | state.closers]}
    state = Enum.reduce(Map.keys(state.workers), state, &interrupt(&2, &1, :cancelled))
    close_if_ready(state)
  end

  def handle_call({:authority, authority, command}, {owner, _}, %{owner: owner} = state) do
    case authority_entry(authority, state) do
      {:ok, nonce, entry} -> authority_command(command, nonce, entry, state)
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:await, reference}, from, %{owner: owner} = state)
      when elem(from, 0) == owner do
    case state.jobs[reference] do
      %{status: :done, result: result} ->
        {:reply, result, state}

      %{status: :pending, waiters: [_ | _]} ->
        {:reply, {:error, :environment_request_already_waited}, state}

      %{status: :pending} = job ->
        {:noreply, put_in(state.jobs[reference], %{job | waiters: [from | job.waiters]})}

      _ ->
        {:reply, {:error, :invalid_environment_request}, state}
    end
  end

  def handle_call({:cancel, reference}, {owner, _}, %{owner: owner} = state) do
    case state.jobs[reference] do
      %{status: :pending} -> {:reply, :ok, interrupt(state, reference, :cancelled)}
      %{status: :done} -> {:reply, :already_completed, state}
      _ -> {:reply, {:error, :invalid_environment_request}, state}
    end
  end

  def handle_call(_, _, state), do: {:reply, {:error, :invalid_environment_owner}, state}

  defp authority_entry(authority, state) do
    with false <- state.closing,
         {:ok, manager, owner, nonce} <- Authority.decode(authority),
         true <- manager == self() and owner == state.owner,
         %{authority: ^authority} = entry <- state.ledger[nonce] do
      {:ok, nonce, entry}
    else
      _ -> {:error, :invalid_environment_authority}
    end
  end

  defp status(%{revoked: true}), do: {:error, :revoked}
  defp status(entry), do: if(now() >= entry.expires, do: {:error, :expired}, else: :ok)

  defp authority_command({:validate, service}, _, entry, state) do
    result = if entry.grant.service == service, do: status(entry), else: {:error, :denied}
    {:reply, result, state}
  end

  defp authority_command({:attenuate, requested}, nonce, entry, state) do
    with :ok <- status(entry),
         true <- map_size(state.ledger) < 64,
         {:ok, grant} <- Policy.attenuate(entry.grant, requested) do
      child = :crypto.strong_rand_bytes(32)
      authority = Authority.issue(grant.service, self(), state.owner, child)

      narrowed = %{
        grant: grant,
        authority: authority,
        expires: min(entry.expires, now() + grant.policy.ttl_ms),
        ancestors: [nonce | entry.ancestors],
        revoked: false
      }

      {:reply, {:ok, authority}, put_in(state.ledger[child], narrowed)}
    else
      false -> {:reply, {:error, :environment_authority_limit}, state}
      error -> {:reply, error, state}
    end
  end

  defp authority_command(:revoke, nonce, _, state) do
    descendants =
      for {key, entry} <- state.ledger, key == nonce or nonce in entry.ancestors, do: key

    state = %{
      state
      | ledger:
          Map.new(state.ledger, fn {key, entry} ->
            {key, if(key in descendants, do: %{entry | revoked: true}, else: entry)}
          end)
    }

    state =
      Enum.reduce(state.workers, state, fn {reference, _}, acc ->
        if acc.jobs[reference].authority in descendants,
          do: interrupt(acc, reference, :revoked),
          else: acc
      end)

    {:reply, :ok, state}
  end

  defp authority_command({:start, operation, argument}, nonce, entry, state) do
    with :ok <- status(entry),
         {:ok, description} <- Schema.operation(entry.grant.service, operation),
         {:ok, _} <- Codec.from_native(description.input, argument, state.limits),
         :ok <- Policy.authorize(entry.grant, operation, argument),
         true <- state.next < state.maximum and map_size(state.workers) < state.parallel do
      start_job(nonce, entry, description, argument, state)
    else
      false ->
        failure_reply(state, :limit)

      {:error, reason} when reason in [:expired, :revoked, :denied] ->
        failure_reply(state, reason)

      {:error, reason} ->
        {:reply, {:error, reason}, state}

      _ ->
        failure_reply(state, :invalid_request)
    end
  end

  defp authority_command(_, _, _, state),
    do: {:reply, {:error, :invalid_environment_command}, state}

  defp failure_reply(state, reason) do
    answer = Schema.failure(reason)

    case Budget.check(answer, state.limits) do
      :ok -> {:reply, {:answer, answer}, state}
      error -> {:reply, error, state}
    end
  end

  defp start_job(nonce, entry, description, argument, state) do
    reference = make_ref()
    manager = self()
    completion_secret = make_ref()
    timeout = min(state.timeout, max(1, entry.expires - now()))
    reason = if entry.expires - now() <= state.timeout, do: :expired, else: :timeout

    {fake, rest} =
      case state.fake do
        nil -> {:real, nil}
        [head | tail] -> {head, tail}
        [] -> {:exhausted, []}
      end

    {worker, monitor} =
      :erlang.spawn_opt(
        fn ->
          result =
            try do
              case fake do
                :real ->
                  Adapter.perform(
                    description.service,
                    description.operation,
                    argument,
                    entry.grant.policy,
                    timeout
                  )

                {service, operation, ^argument, result, delay}
                when service == description.service and operation == description.operation and
                       is_integer(delay) and delay >= 0 and delay <= 1_000_000 ->
                  Process.sleep(delay)
                  result

                _ ->
                  Schema.failure(:invalid_request)
              end
            catch
              :throw, :unconfirmed_environment_cleanup -> :unconfirmed_environment_cleanup
              _, _ -> Schema.failure(service_error(description.service))
            end

          send(manager, {:finished, reference, completion_secret, result})
        end,
        [:link, :monitor]
      )

    timer = Process.send_after(self(), {:deadline, reference, reason}, timeout)

    job = %{
      status: :pending,
      completion_secret: completion_secret,
      authority: nonce,
      argument: argument,
      description: description,
      waiters: [],
      result: nil,
      cancellation: nil,
      deadline: now() + timeout,
      deadline_reason: reason,
      timer: timer,
      force: nil,
      worker: worker,
      monitor: monitor,
      cooperative: fake == :real and description.service in [:process, :filesystem, :time]
    }

    state = %{
      state
      | next: state.next + 1,
        fake: rest,
        jobs: Map.put(state.jobs, reference, job),
        workers: Map.put(state.workers, reference, worker),
        events: [
          {:started, state.next, description.service, description.operation, argument}
          | state.events
        ]
    }

    handle = {Catena.Runtime.Environment, state.owner, self(), reference}
    {:reply, {:ok, handle}, state}
  end

  def handle_info({:finished, reference, secret, result}, state) do
    case state.jobs[reference] do
      %{status: :pending, completion_secret: ^secret} = job ->
        state =
          if result == :unconfirmed_environment_cleanup,
            do: %{state | cleanup_failure: true},
            else: state

        result =
          if result == :unconfirmed_environment_cleanup,
            do: Schema.failure(service_error(job.description.service)),
            else: result

        {:noreply, put_in(state.jobs[reference], %{job | result: result})}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:deadline, reference, reason}, state),
    do: {:noreply, interrupt(state, reference, reason)}

  def handle_info({:force, reference}, state) do
    state =
      if worker = state.workers[reference] do
        Process.exit(worker, :kill)
        if state.jobs[reference].cooperative, do: %{state | cleanup_failure: true}, else: state
      else
        state
      end

    {:noreply, state}
  end

  def handle_info({:DOWN, monitor, :process, _, _}, %{owner_monitor: monitor} = state) do
    state = %{state | closing: true}
    state = Enum.reduce(Map.keys(state.workers), state, &interrupt(&2, &1, :cancelled))
    close_if_ready(state)
  end

  def handle_info({:DOWN, monitor, :process, _, _}, state) do
    case Enum.find(state.jobs, fn {_, job} ->
           job.monitor == monitor and job.status == :pending
         end) do
      {reference, job} ->
        state =
          if job.cooperative and job.result == nil,
            do: %{state | cleanup_failure: true},
            else: state

        Process.cancel_timer(job.timer)
        if job.force, do: Process.cancel_timer(job.force)

        result =
          cond do
            job.cancellation != nil -> Schema.failure(job.cancellation)
            now() >= job.deadline -> Schema.failure(job.deadline_reason)
            job.result != nil -> job.result
            true -> Schema.failure(service_error(job.description.service))
          end

        answer =
          case Codec.from_native(job.description.result, result, state.limits) do
            {:ok, _} ->
              if Policy.result_valid?(
                   job.description.service,
                   job.description.operation,
                   job.argument,
                   result,
                   state.ledger[job.authority].grant.policy
                 ), do: {:ok, result}, else: {:error, :invalid_environment_result}

            error ->
              error
          end

        Enum.each(job.waiters, &GenServer.reply(&1, answer))
        job = %{job | status: :done, result: answer, waiters: []}

        state = %{
          state
          | jobs: Map.put(state.jobs, reference, job),
            workers: Map.delete(state.workers, reference),
            events: [
              {:completed, job.description.service, job.description.operation, answer}
              | state.events
            ]
        }

        close_if_ready(state)

      nil ->
        {:noreply, state}
    end
  end

  def handle_info({:EXIT, _, _}, state), do: {:noreply, state}
  def handle_info(_, state), do: {:noreply, state}

  defp interrupt(state, reference, reason) do
    case state.jobs[reference] do
      %{status: :pending, cancellation: nil} = job ->
        if job.cooperative,
          do: send(job.worker, :environment_cancel),
          else: Process.exit(job.worker, :kill)

        force = Process.send_after(self(), {:force, reference}, 1500)
        put_in(state.jobs[reference], %{job | cancellation: reason, force: force})

      _ ->
        state
    end
  end

  defp close_if_ready(%{closing: true, workers: workers} = state) when map_size(workers) == 0 do
    answer = if state.cleanup_failure, do: {:error, :unconfirmed_environment_cleanup}, else: :ok
    Enum.each(state.closers, &GenServer.reply(&1, answer))
    {:stop, :normal, state}
  end

  defp close_if_ready(state), do: {:noreply, state}
  defp now, do: System.monotonic_time(:millisecond)
  defp service_error(:network), do: :network_failure
  defp service_error(:process), do: :process_failure
  defp service_error(service) when service in [:time, :random, :environment], do: :unavailable
  defp service_error(_), do: :io_failure
end
