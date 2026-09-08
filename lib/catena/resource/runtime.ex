defmodule Catena.Resource.Runtime do
  @moduledoc "CPS resource execution for the explicit 0.1.51 local scope boundary."

  # The caller owns this token. A release helper receives only the closed
  # callback and its immutable payload, never the token or caller dictionary.
  def run(payload, release, body, grace_ns)
      when is_function(release, 1) and is_function(body, 2) and is_integer(grace_ns) and
             grace_ns >= 0 do
    if Process.get({__MODULE__, :releasing}, false),
      do: :erlang.error({:catena_trap, :resource_cleanup_reentry})

    token = make_ref()
    key = {__MODULE__, token}
    Process.put(key, {:active, payload})

    finish = fn outcome ->
      case Process.get(key) do
        {:active, _} ->
          Catena.Effect.Runtime.trace({:resource_primary, outcome})
          Process.put(key, :released)

          Catena.Effect.Runtime.trace({:resource_release_started, payload})
          result = bounded_release(release, payload, grace_ns)
          Catena.Effect.Runtime.trace({:resource_release_finished, payload, result})

          case result do
            :ok -> :ok
            {:error, reason} -> :erlang.error({:catena_trap, {:mandatory_release_failed, reason}})
          end

        :released ->
          :ok

        _ ->
          :erlang.error({:catena_trap, :invalid_resource_owner})
      end
    end

    try do
      result = body.(finish, {__MODULE__, self(), token})
      finish.({:abort, result})
      result
    catch
      kind, reason ->
        stack = __STACKTRACE__
        # Preserve a primary terminal failure even if mandatory cleanup also
        # fails. Secondary failures are visible only to conformance tracing.
        try do
          primary =
            case {kind, reason} do
              {:error, {:catena_trap, trap}} -> {:trap, trap}
              {:throw, {:catena_resource_cancelled, cancelled}} -> {:cancelled, cancelled}
              {:exit, {:catena_resource_exit, exited}} -> {:exited, exited}
              _ -> {:foreign_failure, kind, reason}
            end

          finish.(primary)
        catch
          cleanup_kind, cleanup_reason ->
            Catena.Effect.Runtime.trace({:resource_secondary, cleanup_kind, cleanup_reason})

            unless kind == :error and match?({:catena_trap, _}, reason) do
              :erlang.raise(cleanup_kind, cleanup_reason, __STACKTRACE__)
            end
        end

        :erlang.raise(kind, reason, stack)
    after
      Process.delete(key)
    end
  end

  def exit_scope(resource, reason) do
    read(resource)
    exit({:catena_resource_exit, reason})
  end

  def cancel(resource, reason) do
    read(resource)
    throw({:catena_resource_cancelled, reason})
  end

  def read({__MODULE__, owner, token}) when owner == self() do
    case Process.get({__MODULE__, token}) do
      {:active, payload} -> payload
      _ -> :erlang.error({:catena_trap, :invalid_resource_owner})
    end
  end

  def read(_), do: :erlang.error({:catena_trap, :invalid_resource_owner})

  defp bounded_release(release, payload, grace_ns) do
    owner = self()
    tag = make_ref()
    milliseconds = div(grace_ns + 999_999, 1_000_000)
    deadline = System.monotonic_time(:nanosecond) + milliseconds * 1_000_000

    {worker, monitor} =
      :erlang.spawn_opt(
        fn ->
          Process.put({__MODULE__, :releasing}, true)

          result =
            try do
              release.(payload)
              :ok
            catch
              :error, {:catena_trap, reason} -> {:error, reason}
              kind, reason -> {:error, {:foreign_release_failure, kind, reason}}
            end

          send(owner, {tag, result})
        end,
        [:link, :monitor]
      )

    # receive's finite timeout range is bounded. Larger durations are waited
    # in monotonic chunks rather than overflowing a host timeout argument.
    await_release(worker, monitor, tag, deadline)
  end

  defp await_release(worker, monitor, tag, deadline) do
    remaining_ns = max(deadline - System.monotonic_time(:nanosecond), 0)
    remaining = div(remaining_ns + 999_999, 1_000_000)

    receive do
      {^tag, result} ->
        receive do
          {:DOWN, ^monitor, :process, ^worker, _} -> result
        end

      {:DOWN, ^monitor, :process, ^worker, reason} ->
        {:error, {:release_worker_exit, reason}}
    after
      min(remaining, 4_294_967_295) ->
        if System.monotonic_time(:nanosecond) < deadline do
          await_release(worker, monitor, tag, deadline)
        else
          Process.unlink(worker)
          Process.exit(worker, :kill)

          receive do
            {:DOWN, ^monitor, :process, ^worker, _} -> :ok
          end

          # A completion may have raced with timeout and termination. Drain
          # only this private reply; the terminal selection is already fixed.
          receive do
            {^tag, _} -> :ok
          after
            0 -> :ok
          end

          {:error, :deadline_exhausted}
        end
    end
  end
end
