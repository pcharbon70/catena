defmodule Catena.Foreign.Adapter do
  @moduledoc "Scoped capability-bound foreign calls; no ambient module/function lookup."
  alias Catena.Foreign.Session

  def deliver_secret(scope, reference, recipient),
    do: Catena.Runtime.Secret.deliver(scope, reference, recipient)

  def run(grants, limits, body, options \\ []) when is_function(body, 1) do
    grace = Keyword.get(options, :release_grace_ns, 1_000_000_000)
    release_token = make_ref()
    evidence_tag = make_ref()
    owner = self()

    with true <- is_integer(grace) and grace >= 0,
         {:ok, manager} <-
           Session.start(
             owner,
             grants,
             limits,
             Keyword.put(options, :release_token, release_token)
           ) do
      scope = {__MODULE__, owner, manager, make_ref()}
      :ok = GenServer.call(manager, {:bind, scope})

      try do
        Catena.Resource.Runtime.run(
          {manager, release_token},
          fn {manager, token} ->
            case GenServer.call(manager, {:release, token}, :infinity) do
              {:ok, evidence} -> send(owner, {evidence_tag, evidence})
              error -> :erlang.error({:catena_trap, {:foreign_cleanup_failed, error}})
            end
          end,
          fn _finish, _resource -> body.(scope) end,
          grace
        )
      after
        # Bounded release can expire while the manager is stalled. Its linked
        # workers still lose their owner; this is termination, never rollback.
        Process.exit(manager, :kill)

        receive do
          {^evidence_tag, evidence} ->
            Catena.Effect.Runtime.trace({:foreign_scope_closed, evidence})
        after
          0 -> :ok
        end
      end
    else
      false -> {:error, :invalid_foreign_release_grace}
      error -> error
    end
  end

  def start(scope, declaration, arguments), do: request(scope, {:start, declaration, arguments})
  def cancel(scope, handle, reason), do: request(scope, {:cancel, handle, reason})
  def poll(scope, handle), do: request(scope, {:poll, handle})
  def callback(scope, description), do: request(scope, {:callback, description})
  def revoke(scope, handle), do: request(scope, {:revoke, handle})
  def events(scope), do: request(scope, :events)

  def await(scope, handle, milliseconds)
      when is_integer(milliseconds) and milliseconds >= 0 and milliseconds <= 4_294_967_295 do
    request(scope, {:await, handle, milliseconds})
  end

  def await(_, _, _), do: {:error, :invalid_foreign_wait}

  def call(scope, declaration, arguments, milliseconds) do
    with {:ok, handle} <- start(scope, declaration, arguments),
         do: await(scope, handle, milliseconds)
  end

  def request({__MODULE__, owner, manager, _} = scope, operation) when owner == self() do
    GenServer.call(manager, {scope, operation}, :infinity)
  catch
    :exit, _ -> {:error, :expired_foreign_scope}
  end

  def request(_, _), do: {:error, :invalid_foreign_scope_owner}
end
