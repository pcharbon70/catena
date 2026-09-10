defmodule Catena.MessageSemanticsTest do
  use ExUnit.Case, async: false

  alias Catena.{Foreign.Codec, Message, Runtime.Capacity}

  @limits %{nodes: 64, bytes: 4096, depth: 16}

  defp codec(type), do: elem(Codec.new({:data, type}), 1)

  test "checked local send preserves Unit, dead-target discard and validation before send" do
    integer = codec(:integer)
    assert {:ok, :unit} = Message.checked_send(self(), integer, 41, @limits)
    assert_receive 41

    assert {:error, %{kind: :conversion_failure}} =
             Message.checked_send(self(), integer, false, @limits)

    refute_received false

    dead = spawn(fn -> :ok end)
    monitor = Process.monitor(dead)
    assert_receive {:DOWN, ^monitor, :process, ^dead, :normal}
    assert {:ok, :unit} = Message.checked_send(dead, integer, 42, @limits)
  end

  test "immutable snapshots preserve observations without exposing physical sharing" do
    bytes = :binary.copy(<<0, 255>>, 1024)

    assert {:ok, snapshot} =
             Message.snapshot(codec(:bytes), bytes, %{nodes: 4, bytes: 2048, depth: 4})

    assert snapshot == bytes
    assert Message.profile().physical_copy_required == false
    assert Message.profile().physical_sharing_observable == false
  end

  test "checked bounded admission refuses invalid payloads before capacity accounting" do
    assert {:ok, queue} = Capacity.start(messages: 1, bytes: 4096)
    integer = codec(:integer)

    assert {:error, %{kind: :conversion_failure}} =
             Message.admit(queue, integer, false, @limits)

    assert {:ok, %{messages: 0, rejected: 0}} = Capacity.stats(queue)
    assert {:ok, :admitted} = Message.admit(queue, integer, 7, @limits)
    assert {:error, :overloaded} = Message.admit(queue, integer, 8, @limits)
    assert {:ok, 7} = Capacity.take(queue)
    assert {:ok, %{discarded: 0}} = Capacity.close(queue)
  end

  test "several checked senders preserve each sender's order and allow interleaving" do
    assert {:ok, queue} = Capacity.start(messages: 4, bytes: 4096)
    pair = codec({:tuple, [:integer, :integer]})
    parent = self()

    for sender <- [1, 2] do
      spawn(fn ->
        assert {:ok, :admitted} = Message.admit(queue, pair, {sender, 1}, @limits)
        assert {:ok, :admitted} = Message.admit(queue, pair, {sender, 2}, @limits)
        Kernel.send(parent, {:done, sender})
      end)
    end

    assert_receive {:done, 1}
    assert_receive {:done, 2}
    received = for _ <- 1..4, do: elem(Capacity.take(queue), 1)
    assert Enum.filter(received, &(elem(&1, 0) == 1)) == [{1, 1}, {1, 2}]
    assert Enum.filter(received, &(elem(&1, 0) == 2)) == [{2, 1}, {2, 2}]
    assert {:ok, %{discarded: 0}} = Capacity.close(queue)
  end

  test "conformance profile composes local, native, capacity and remote boundaries" do
    profile = Catena.ConformanceInfo.document()["message_semantics"]
    assert profile["version"] == "0.1.77"
    assert profile["raw_local_result"] == "unit"
    assert profile["dead_target_result"] == "unit"
    assert profile["checked_capacity_admission"]
    assert profile["native_send_authority"] == "scope_checked"
    assert profile["remote_contract"] == "0.1.76"
    assert Catena.LanguageVersion.introduced(:message_semantics) == "0.1.77"
  end
end
