defmodule Catena.Foreign.Native.Session do
  @moduledoc false
  use GenServer
  alias Catena.Foreign.Native.{Package, Transport}

  def start(owner, ready, release_token),
    do: GenServer.start(__MODULE__, {owner, ready, release_token})

  def init({owner, ready, release_token}) do
    Process.flag(:trap_exit, true)
    monitor = Process.monitor(owner)

    with :ok <- Package.verify_ready(ready),
         {:ok, transport} <- Transport.open(ready) do
      {:ok,
       %{
         release_token: release_token,
         owner: owner,
         monitor: monitor,
         scope: nil,
         transport: transport,
         closed: false
       }}
    else
      error -> {:stop, error}
    end
  end

  def handle_call({:bind, scope}, {owner, _}, %{owner: owner, scope: nil} = state),
    do: {:reply, :ok, %{state | scope: scope}}

  def handle_call({scope, operation}, {owner, _}, %{scope: scope, owner: owner} = state) do
    case {operation, state.closed} do
      {:close, _} ->
        release(state)

      {{:call, _}, true} ->
        {:reply, {:error, :closed_native_resource}, state}

      {{:call, value}, false} ->
        {result, transport} = Transport.call(state.transport, value)
        {:reply, result, %{state | transport: transport}}

      _ ->
        {:reply, {:error, :invalid_native_operation}, state}
    end
  end

  def handle_call({:release, token}, _, %{release_token: token} = state), do: release(state)
  def handle_call(_, _, state), do: {:reply, {:error, :invalid_native_authority}, state}

  def handle_info(
        {:DOWN, monitor, :process, owner, _},
        %{monitor: monitor, owner: owner} = state
      ),
      do: {:stop, :normal, state}

  def handle_info({port, {:exit_status, _}}, %{transport: %{kind: "port", handle: port}} = state),
    do: {:noreply, put_in(state.transport.ended, true)}

  def handle_info(_, state), do: {:noreply, state}
  def terminate(_, state), do: Transport.dispose(state.transport)
  defp release(%{closed: true} = state), do: {:reply, :ok, state}

  defp release(state) do
    {result, transport} = Transport.close(state.transport)
    {:reply, result, %{state | transport: transport, closed: result == :ok}}
  end
end
