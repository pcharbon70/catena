defmodule Catena.Message do
  @moduledoc "Checked local message snapshots and bounded admission over retained send semantics."

  alias Catena.Foreign.Codec
  alias Catena.Runtime.Capacity

  @version "0.1.77"

  def profile do
    %{
      version: @version,
      raw_local_result: :unit,
      dead_target_result: :unit,
      physical_copy_required: false,
      physical_sharing_observable: false,
      checked_capacity_admission: true,
      native_send_authority: :scope_checked,
      remote_contract: "0.1.76"
    }
  end

  def snapshot(codec, value, limits) do
    with :ok <- Codec.verify(codec),
         {:ok, native} <- Codec.encode(codec, value, limits),
         {:ok, snapshot} <- Codec.decode(codec, native, limits) do
      {:ok, snapshot}
    else
      {:error, _} = error -> error
    end
  rescue
    _ -> {:error, %{kind: :conversion_failure, reason: :invalid_message, direction: :encode}}
  end

  def checked_send(pid, codec, value, limits) when is_pid(pid) and node(pid) == node() do
    with {:ok, snapshot} <- snapshot(codec, value, limits) do
      Kernel.send(pid, snapshot)
      {:ok, :unit}
    end
  end

  def checked_send(_, _, _, _), do: {:error, :invalid_local_message_target}

  def admit(queue, codec, value, limits) do
    with {:ok, snapshot} <- snapshot(codec, value, limits),
         :ok <- Capacity.offer(queue, snapshot) do
      {:ok, :admitted}
    end
  end
end
