defmodule Catena.Foreign.Native do
  @moduledoc "Explicit signed native service scopes; Float is the initial closed ABI."
  alias Catena.Foreign.Native.{Package, Session}

  def invoke_admitted(scope, package, value),
    do: Catena.Trust.Policy.invoke(scope, package, "call", [value], %{})

  def run(package, policy, body, options \\ []) when is_function(body, 1) do
    release_token = make_ref()
    grace = Keyword.get(options, :release_grace_ns, 2_000_000_000)

    with true <- is_integer(grace) and grace >= 0,
         {:ok, ready} <- Package.verify(package, policy),
         {:ok, manager} <- Session.start(self(), ready, release_token) do
      token = make_ref()
      scope = {__MODULE__, self(), manager, token}
      :ok = GenServer.call(manager, {:bind, scope})

      try do
        Catena.Resource.Runtime.run(
          manager,
          fn pid ->
            case GenServer.call(pid, {:release, release_token}, :infinity) do
              :ok -> :ok
              error -> :erlang.error({:catena_trap, {:native_cleanup_failed, error}})
            end
          end,
          fn _, _ -> body.(scope) end,
          grace
        )
      after
        try do
          GenServer.stop(manager, :normal, 3000)
        catch
          :exit, _ -> Process.exit(manager, :kill)
        end
      end
    else
      false -> {:error, :invalid_native_release_grace}
      error -> error
    end
  end

  def call(scope, value), do: request(scope, {:call, value})
  def close(scope), do: request(scope, :close)

  defp request({__MODULE__, owner, manager, _} = scope, operation) when owner == self() do
    GenServer.call(manager, {scope, operation}, :infinity)
  catch
    :exit, _ -> {:error, :expired_native_scope}
  end

  defp request(_, _), do: {:error, :invalid_native_owner}
end
