defmodule Catena.Distribution.Model do
  @moduledoc "Bounded transport-state reference model with explicit delivery uncertainty."

  alias Catena.Distribution.{Contract, Wire}

  def new(contract, peer, capacity)
      when is_integer(capacity) and capacity > 0 and capacity <= 65_536 do
    with :ok <- Contract.validate(contract), true <- Map.has_key?(contract.peers, peer) do
      {:ok,
       %{
         contract: contract,
         peer: peer,
         capacity: capacity,
         connection: :disconnected,
         next_sequence: 0,
         pending: %{},
         received: %{}
       }}
    else
      _ -> {:error, :invalid_distribution_model}
    end
  end

  def new(_, _, _), do: {:error, :invalid_distribution_model}

  def connect(state, hello, certificate) do
    with {:ok, peer} <- Wire.decode_hello(hello),
         true <- peer["node"] == state.peer,
         true <- peer["protocol"] == state.contract.local.digest,
         true <-
           peer["endpoint"] ==
             Contract.endpoint_digest(
               state.contract.version,
               peer["node"],
               peer["service"],
               peer["package"],
               peer["protocol"]
             ),
         :ok <-
           Contract.authorize(
             state.contract,
             peer["node"],
             peer["service"],
             peer["package"],
             certificate
           ) do
      {:ok, %{state | connection: :connected}}
    else
      _ -> {:error, :remote_handshake_refused, state}
    end
  end

  def prepare(%{connection: :disconnected} = state, _, _, _),
    do: {:error, :not_enqueued, state}

  def prepare(state, role, message_id, payload) do
    if map_size(state.pending) >= state.capacity do
      {:error, :overloaded, state}
    else
      sequence = state.next_sequence

      case Wire.encode(state.contract, state.peer, role, sequence, message_id, payload) do
        {:ok, bytes} ->
          token = {sequence, message_id}
          pending = Map.put(state.pending, token, %{status: :prepared, bytes: bytes})
          {:ok, token, bytes, %{state | next_sequence: sequence + 1, pending: pending}}

        {:error, _} = error ->
          {kind, reason} = error
          {kind, reason, state}
      end
    end
  end

  def transmitted(state, token) do
    case state.pending[token] do
      %{status: :prepared} = entry ->
        {:ok, put_in(state, [:pending, token], %{entry | status: :in_flight})}

      _ ->
        {:error, :invalid_delivery_token, state}
    end
  end

  def acknowledged(state, token) do
    case state.pending[token] do
      %{status: :in_flight} ->
        {:ok, :admitted, %{state | pending: Map.delete(state.pending, token)}}

      _ ->
        {:error, :invalid_delivery_token, state}
    end
  end

  def disconnect(state) do
    outcomes =
      state.pending
      |> Enum.map(fn {token, entry} ->
        {token, if(entry.status == :prepared, do: :not_enqueued, else: :delivery_unknown)}
      end)
      |> Enum.sort()

    {:ok, outcomes, %{state | connection: :disconnected, pending: %{}}}
  end

  def receive_frame(state, role, bytes) do
    with :connected <- state.connection,
         {:ok, frame} <- Wire.decode(state.contract, state.peer, role, bytes) do
      case state.received[frame.message_id] do
        nil ->
          received = Map.put(state.received, frame.message_id, frame.digest)
          {:ok, :deliver, frame.payload, %{state | received: received}}

        digest when digest == frame.digest ->
          {:ok, :duplicate, nil, state}

        _ ->
          {:error, :conflicting_duplicate, state}
      end
    else
      :disconnected -> {:error, :connection_lost, state}
      {:error, reason} -> {:error, reason, state}
    end
  end
end
