defmodule Catena.Runtime.Secret.Session do
  @moduledoc false
  use GenServer
  alias Catena.Runtime.Secret.{Input, Ref, Transport}

  def start(owner, providers, recipients, release, options),
    do: GenServer.start(__MODULE__, {owner, providers, recipients, release, options})

  def init({owner, providers, recipients, release, options}) do
    Process.flag(:sensitive, true)
    Process.flag(:trap_exit, true)
    ttl = Keyword.get(options, :ttl_ms, 10000)

    with true <- Keyword.keys(options) in [[], [:ttl_ms]],
         true <- is_integer(ttl) and ttl in 1..1_000_000,
         true <- named?(providers) and named?(recipients),
         true <- byte_size(:erlang.term_to_binary({providers, recipients})) <= 1_048_576,
         true <- Enum.all?(providers, fn {_, p} -> Transport.provider?(p) end),
         true <- Enum.all?(recipients, fn {_, r} -> Transport.recipient?(r) end) do
      scope = {:catena_secret_scope, owner, self(), make_ref()}

      entry = %{
        providers: Map.keys(providers),
        recipients: Map.keys(recipients),
        parents: [],
        expires: now() + ttl,
        revoked: false
      }

      {:ok,
       %{
         owner: owner,
         monitor: Process.monitor(owner),
         providers: providers,
         recipients: recipients,
         release: release,
         scope: scope,
         scopes: %{scope => entry},
         objects: %{},
         bytes: 0,
         jobs: %{},
         next: 0,
         events: [],
         closing: false,
         closers: [],
         cleanup_failed: false
       }}
    else
      _ -> {:stop, :invalid_secret_setup}
    end
  rescue
    _ -> {:stop, :invalid_secret_setup}
  end

  def format_status(status), do: Map.new(status, fn {key, _} -> {key, :redacted} end)

  defp named?(map),
    do:
      is_map(map) and map_size(map) <= 64 and
        Enum.all?(
          Map.keys(map),
          &(is_binary(&1) and byte_size(&1) in 1..128 and String.valid?(&1))
        )

  defp now, do: System.monotonic_time(:millisecond)

  def handle_call(:scope, {owner, _}, %{owner: owner} = s), do: {:reply, {:ok, s.scope}, s}

  def handle_call({:close, release}, from, %{release: release} = s),
    do: closing(%{s | closers: [from | s.closers]})

  def handle_call({:await, token}, from = {owner, _}, %{owner: owner} = s) do
    case s.jobs[token] do
      %{status: :done, result: result} -> {:reply, result, s}
      %{waiter: nil} -> {:noreply, put_in(s.jobs[token].waiter, from)}
      _ -> {:reply, {:error, :invalid_secret_job}, s}
    end
  end

  def handle_call({:cancel, token}, {owner, _}, %{owner: owner} = s) do
    case s.jobs[token] do
      %{status: :pending} -> {:reply, :ok, cancel(s, token, :cancelled)}
      %{status: :done} -> {:reply, :already_completed, s}
      _ -> {:reply, {:error, :invalid_secret_job}, s}
    end
  end

  def handle_call({scope, command}, {owner, _}, %{owner: owner} = s) do
    if live?(s, scope) and not s.closing,
      do: command(command, scope, s),
      else: {:reply, {:error, :expired_secret_authority}, s}
  end

  def handle_call(_, _, s), do: {:reply, {:error, :invalid_secret_owner}, s}

  defp live?(s, scope) do
    case s.scopes[scope] do
      %{revoked: false, expires: expires, parents: parents} ->
        expires > now() and
          Enum.all?(parents, fn p -> not s.scopes[p].revoked and s.scopes[p].expires > now() end)

      _ ->
        false
    end
  end

  defp command({:fetch, name}, scope, s) do
    if name in s.scopes[scope].providers do
      case s.providers[name] do
        %Input{value: bytes} ->
          case seal(s, scope, [name], bytes) do
            {:ok, ref, s} -> {:reply, {:ok, ref}, event(s, :retrieved)}
            error -> {:reply, error, s}
          end

        provider ->
          start_job(s, scope, [name], {:fetch, provider})
      end
    else
      {:reply, {:error, :secret_provider_denied}, s}
    end
  end

  defp command({:derive, expression}, scope, s) do
    with {:ok, value, labels} <- evaluate(expression, scope, s, 0),
         {:ok, ref, s} <- seal(s, scope, labels, value, origins(expression, s)) do
      {:reply, {:ok, ref}, event(s, :derived)}
    else
      _ -> {:reply, {:error, :secret_derivation_denied}, s}
    end
  end

  defp command({:deliver, ref, name}, scope, s) do
    with true <- name in s.scopes[scope].recipients,
         {:ok, value, labels} <- resolve(ref, scope, s),
         true <- is_binary(value) do
      start_job(s, scope, labels, {:deliver, s.recipients[name], value}, origins(ref, s))
    else
      _ -> {:reply, {:error, :secret_delivery_denied}, s}
    end
  end

  defp command({:attenuate, providers, recipients, ttl}, scope, s) do
    entry = s.scopes[scope]

    if map_size(s.scopes) < 64 and is_integer(ttl) and ttl in 1..1_000_000 and
         subset?(providers, entry.providers) and subset?(recipients, entry.recipients) do
      child = {:catena_secret_scope, s.owner, self(), make_ref()}

      entry = %{
        providers: providers,
        recipients: recipients,
        parents: [scope | entry.parents],
        expires: min(entry.expires, now() + ttl),
        revoked: false
      }

      {:reply, {:ok, child}, put_in(s.scopes[child], entry)}
    else
      {:reply, {:error, :secret_attenuation_denied}, s}
    end
  end

  defp command(:revoke, scope, s) do
    s = put_in(s.scopes[scope].revoked, true)

    s =
      Enum.reduce(s.jobs, s, fn {token, job}, acc ->
        if job.status == :pending and
             (not live?(acc, job.scope) or not Enum.all?(job.issuers, &live?(acc, &1))),
           do: cancel(acc, token, :revoked),
           else: acc
      end)

    {:reply, :ok, event(s, :revoked)}
  end

  defp command(:audit, _, s), do: {:reply, {:ok, Enum.reverse(s.events)}, s}
  defp command(_, _, s), do: {:reply, {:error, :invalid_secret_request}, s}

  defp subset?(xs, ys),
    do: is_list(xs) and length(xs) <= 64 and xs == Enum.uniq(xs) and xs -- ys == []

  defp resolve(%Ref{owner: owner, manager: manager, token: token}, scope, s)
       when owner == s.owner and manager == self() do
    case s.objects[token] do
      %{issuers: issuers, labels: labels, value: value} ->
        if Enum.all?(issuers, &live?(s, &1)) and live?(s, scope) and
             labels -- s.scopes[scope].providers == [],
           do: {:ok, value, labels},
           else: {:error, :secret_reference_denied}

      _ ->
        {:error, :invalid_secret_reference}
    end
  end

  defp resolve(_, _, _), do: {:error, :invalid_secret_reference}

  defp seal(s, scope, labels, value, issuers \\ []) do
    size = byte_size(:erlang.term_to_binary(value))

    if map_size(s.objects) < 256 and size <= 131_072 and s.bytes + size <= 1_048_576 do
      token = make_ref()

      object = %{
        issuers: Enum.uniq([scope | issuers]),
        labels: Enum.sort(Enum.uniq(labels)),
        value: value
      }

      ref = %Ref{owner: s.owner, manager: self(), token: token}
      {:ok, ref, %{s | objects: Map.put(s.objects, token, object), bytes: s.bytes + size}}
    else
      {:error, :secret_storage_limit}
    end
  end

  defp origins(%Ref{token: token}, s), do: Map.get(s.objects[token] || %{}, :issuers, [])

  defp origins(value, s) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.flat_map(&origins(&1, s)) |> Enum.uniq()

  defp origins(_, _), do: []

  defp evaluate(_, _, _, depth) when depth > 16, do: {:error, :secret_expression_limit}
  defp evaluate(%Ref{} = ref, scope, s, _), do: resolve(ref, scope, s)

  defp evaluate({:base64, child}, scope, s, depth) do
    with {:ok, value, labels} when is_binary(value) <- evaluate(child, scope, s, depth + 1),
         true <- div(byte_size(value) + 2, 3) * 4 <= 65_536,
         do: {:ok, Base.encode64(value), labels},
         else: (_ -> {:error, :secret_expression_limit})
  end

  defp evaluate({:hex, child}, scope, s, depth) do
    with {:ok, value, labels} when is_binary(value) <- evaluate(child, scope, s, depth + 1),
         true <- byte_size(value) * 2 <= 65_536,
         do: {:ok, Base.encode16(value, case: :lower), labels},
         else: (_ -> {:error, :secret_expression_limit})
  end

  defp evaluate({:concat, a, b}, scope, s, depth) do
    with {:ok, a, la} when is_binary(a) <- evaluate(a, scope, s, depth + 1),
         {:ok, b, lb} when is_binary(b) <- evaluate(b, scope, s, depth + 1),
         true <- byte_size(a) + byte_size(b) <= 65_536,
         do: {:ok, a <> b, Enum.uniq(la ++ lb)},
         else: (_ -> {:error, :secret_expression_limit})
  end

  defp evaluate({:public, bytes}, _, _, _) when is_binary(bytes) and byte_size(bytes) <= 65_536,
    do: {:ok, bytes, []}

  defp evaluate({:element, child, index}, scope, s, depth)
       when is_integer(index) and index >= 0 do
    with {:ok, tuple, labels} when is_tuple(tuple) <- evaluate(child, scope, s, depth + 1),
         true <- index < tuple_size(tuple),
         do: {:ok, elem(tuple, index), labels},
         else: (_ -> {:error, :secret_projection_denied})
  end

  defp evaluate(_, _, _, _), do: {:error, :invalid_secret_expression}

  defp start_job(s, scope, labels, work, issuers \\ []) do
    if s.next < 1024 and Enum.count(s.jobs, fn {_, j} -> j.status == :pending end) < 8 do
      token = make_ref()
      secret = make_ref()
      manager = self()

      {pid, monitor} =
        :erlang.spawn_opt(
          fn ->
            Process.flag(:sensitive, true)

            result =
              try do
                case work do
                  {:fetch, provider} -> Transport.fetch(provider)
                  {:deliver, recipient, value} -> Transport.deliver(recipient, value)
                end
              catch
                :error, {:catena_trap, _} -> {:error, :secret_cleanup_unconfirmed}
                _, _ -> {:error, :secret_worker_failed}
              end

            send(manager, {:secret_result, token, secret, result})
          end,
          [:link, :monitor]
        )

      timer = Process.send_after(self(), {:secret_deadline, token}, 7000)

      expiry = Enum.map(Enum.uniq([scope | issuers]), &s.scopes[&1].expires) |> Enum.min()
      expiry_timer = Process.send_after(self(), {:secret_expired, token}, max(0, expiry - now()))

      job = %{
        expiry_timer: expiry_timer,
        status: :pending,
        issuers: Enum.uniq([scope | issuers]),
        scope: scope,
        labels: labels,
        worker: pid,
        monitor: monitor,
        secret: secret,
        candidate: nil,
        result: nil,
        waiter: nil,
        cancelled: nil,
        timer: timer
      }

      s = %{s | jobs: Map.put(s.jobs, token, job), next: s.next + 1}
      {:reply, {:ok, {:catena_secret_job, s.owner, self(), token}}, event(s, :requested)}
    else
      {:reply, {:error, :secret_job_limit}, s}
    end
  end

  def handle_info({:secret_result, token, secret, result}, s) do
    case s.jobs[token] do
      %{status: :pending, secret: ^secret} -> {:noreply, put_in(s.jobs[token].candidate, result)}
      _ -> {:noreply, s}
    end
  end

  def handle_info({:secret_expired, token}, s), do: {:noreply, cancel(s, token, :expired)}

  def handle_info({:secret_deadline, token}, s) do
    case s.jobs[token] do
      %{status: :pending, worker: worker} ->
        Process.exit(worker, :kill)
        {:noreply, %{cancel(s, token, :timeout) | cleanup_failed: true}}

      _ ->
        {:noreply, s}
    end
  end

  def handle_info({:DOWN, monitor, :process, _, _}, %{monitor: monitor} = s), do: closing(s)

  def handle_info({:DOWN, monitor, :process, _, reason}, s) do
    case Enum.find(s.jobs, fn {_, j} -> j.monitor == monitor and j.status == :pending end) do
      {token, job} ->
        Process.cancel_timer(job.timer)
        Process.cancel_timer(job.expiry_timer)
        failed = reason != :normal or job.candidate == {:error, :secret_cleanup_unconfirmed}

        {result, s} =
          cond do
            failed ->
              {{:error, :secret_cleanup_unconfirmed}, %{s | cleanup_failed: true}}

            job.cancelled != nil ->
              {{:error, job.cancelled}, s}

            not live?(s, job.scope) or not Enum.all?(job.issuers, &live?(s, &1)) ->
              {{:error, :expired_secret_authority}, s}

            true ->
              case job.candidate do
                {:ok, value} ->
                  case seal(s, job.scope, job.labels, value, job.issuers) do
                    {:ok, ref, s} -> {{:ok, ref}, s}
                    error -> {error, s}
                  end

                _ ->
                  {{:error, :secret_delivery_failed}, s}
              end
          end

        if job.waiter, do: GenServer.reply(job.waiter, result)
        job = %{job | status: :done, result: result, candidate: nil, waiter: nil}
        s = %{s | jobs: Map.put(s.jobs, token, job)} |> event(:completed)
        finish_close(s)

      nil ->
        {:noreply, s}
    end
  end

  def handle_info(_, s), do: {:noreply, s}

  defp cancel(s, token, reason) do
    case s.jobs[token] do
      %{status: :pending, cancelled: nil, worker: worker} ->
        send(worker, :secret_cancel)
        put_in(s.jobs[token].cancelled, reason)

      _ ->
        s
    end
  end

  defp closing(s) do
    s = %{s | closing: true}
    s = Enum.reduce(s.jobs, s, fn {token, _}, acc -> cancel(acc, token, :cancelled) end)
    finish_close(s)
  end

  defp finish_close(%{closing: true} = s) do
    if Enum.all?(s.jobs, fn {_, j} -> j.status == :done end) do
      result = if s.cleanup_failed, do: {:error, :secret_cleanup_unconfirmed}, else: :ok
      Enum.each(s.closers, &GenServer.reply(&1, result))
      {:stop, :normal, s}
    else
      {:noreply, s}
    end
  end

  defp finish_close(s), do: {:noreply, s}
  defp event(s, kind), do: %{s | events: [kind | s.events]}
end
