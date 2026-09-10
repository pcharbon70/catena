defmodule Catena.ResourceExhaustionTest do
  use ExUnit.Case, async: false

  alias Catena.{ImplementationLimits, Resource.Budget, Runtime.Capacity}

  test "aggregate compiler budgets accept exact thresholds and refuse the next unit" do
    files = ImplementationLimits.configured(:aggregate_source_files)
    bytes = ImplementationLimits.configured(:aggregate_source_bytes)
    nodes = ImplementationLimits.configured(:aggregate_syntax_nodes)
    outputs = ImplementationLimits.configured(:aggregate_output_bytes)

    assert :ok = Budget.validate_file_sizes(List.duplicate(0, files))

    assert {:error, %{id: "LIM006", details: %{observed: observed}}} =
             Budget.validate_file_sizes(List.duplicate(0, files + 1))

    assert observed == files + 1
    assert :ok = Budget.validate_sources(List.duplicate("x", files))
    assert :ok = Budget.validate_file_sizes([bytes])

    assert {:error, %{id: "LIM007", details: %{observed: observed}}} =
             Budget.validate_file_sizes([bytes + 1])

    assert observed == bytes + 1
    assert :ok = Budget.validate_tree(List.duplicate(:leaf, nodes - 1))

    assert {:error, %{id: "LIM008", details: %{observed: observed}}} =
             Budget.validate_tree(List.duplicate(:leaf, nodes))

    assert observed == nodes + 1
    assert :ok = Budget.validate_output_sizes([outputs])

    assert {:error, %{id: "LIM009", details: %{observed: observed}}} =
             Budget.validate_output_sizes([outputs, 1])

    assert observed == outputs + 1
  end

  test "public compiler entry points apply aggregate source and syntax accounting" do
    assert {:ok, core} = Catena.check_kernel(valid_kernel())
    assert :ok = Budget.validate_tree(core)

    too_large = :binary.copy(" ", ImplementationLimits.configured(:aggregate_source_bytes) + 1)

    assert {:error, %{id: "LIM007", details: %{unit: "bytes"}}} =
             Catena.check_kernel(too_large)
  end

  test "bounded rejection preserves FIFO payloads and reports pressure without silent loss" do
    size = :erlang.external_size({:payload, 1})
    assert {:ok, queue} = Capacity.start(messages: 2, bytes: size * 2, overload: :reject)
    assert :ok = Capacity.offer(queue, {:payload, 1})
    assert :ok = Capacity.offer(queue, {:payload, 2})
    assert {:error, :overloaded} = Capacity.offer(queue, {:payload, 3})
    assert {:ok, %{messages: 2, rejected: 1}} = Capacity.stats(queue)
    assert {:ok, {:payload, 1}} = Capacity.take(queue)
    assert {:ok, {:payload, 2}} = Capacity.take(queue)
    assert :empty = Capacity.take(queue)
    assert {:ok, %{discarded: 0, bytes: 0}} = Capacity.close(queue)

    assert {:ok, pressured} = Capacity.start(messages: 2, bytes: size * 2, overload: :reject)
    assert :ok = Capacity.offer(pressured, {:payload, 1})
    assert :ok = Capacity.offer(pressured, {:payload, 2})
    assert {:error, :overloaded} = Capacity.offer(pressured, {:payload, 3})
    assert {:ok, %{discarded: 2, bytes: discarded_bytes}} = Capacity.close(pressured)
    assert discarded_bytes == size * 2
  end

  test "shared send authority admits several producers while only the owner consumes" do
    assert {:ok, queue} = Capacity.start(messages: 4, bytes: 4096)
    parent = self()

    sender =
      spawn(fn ->
        send(parent, {:offered, Capacity.offer(queue, :from_sender)})
        send(parent, {:take, Capacity.take(queue)})
      end)

    assert_receive {:offered, :ok}
    assert_receive {:take, {:error, :capacity_owner_required}}
    assert is_pid(sender)
    assert {:ok, :from_sender} = Capacity.take(queue)
    assert {:ok, _} = Capacity.close(queue)
  end

  test "termination overload is explicit and owner-observable" do
    assert {:ok, {Capacity, pid, _} = queue} =
             Capacity.start(messages: 1, bytes: 4096, overload: :terminate)

    monitor = Process.monitor(pid)
    assert :ok = Capacity.offer(queue, :first)
    assert {:error, :capacity_exhausted} = Capacity.offer(queue, :second)
    assert_receive {:catena_capacity_exit, ^queue, :capacity_exhausted}
    assert_receive {:DOWN, ^monitor, :process, ^pid, :capacity_exhausted}
  end

  test "owner death cancels the queue and declared profiles expose exact outcomes" do
    parent = self()

    owner =
      spawn(fn ->
        {:ok, {Capacity, pid, _} = queue} = Capacity.start(messages: 1, bytes: 128)
        send(parent, {:queue, queue, pid})
        receive do: (:release_owner -> :ok)
      end)

    owner_monitor = Process.monitor(owner)
    assert_receive {:queue, _queue, pid}
    queue_monitor = Process.monitor(pid)
    send(owner, :release_owner)
    assert_receive {:DOWN, ^owner_monitor, :process, ^owner, :normal}
    assert_receive {:DOWN, ^queue_monitor, :process, ^pid, :normal}

    profile = Catena.ConformanceInfo.document()["resource_exhaustion"]
    assert profile["compiler"]["transactional_outputs"]
    assert profile["runtime"]["overload_policies"] == ["reject", "terminate"]
    assert profile["runtime"]["byte_accounting"] == "erlang_external_size"
    assert profile["runtime"]["host_fatal_recovery"] == false
    assert profile["runtime"]["raw_send_changed"] == false
    assert profile["runtime"]["silent_loss"] == false
  end

  defp valid_kernel do
    """
    (module ResourceBudget
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://resource-budget")
      (export value main)
      (def main (signature Int (uses)) 75))
    """
  end
end
