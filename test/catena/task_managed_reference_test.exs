defmodule Catena.TaskManagedReferenceTest do
  use ExUnit.Case, async: true
  alias Catena.Task.ManagedReference, as: R

  @labels %{
    completed: "done",
    trapped: "fault",
    cancelled: "stopped",
    exited: "exit",
    absent: "missing",
    runtime_failure: "runtime",
    external_loss: "lost"
  }

  @tag obligations: ~w(OT-OBL-009)
  test "remote unlink removes propagation without fabricating a terminal observation" do
    c = %{processes: %{0 => process(0), 1 => process(1)}}

    c =
      R.returned(c, c.processes[0], {:managed_link_target, @labels}, {:catena_managed_process, 1})

    {:value, a} = c.processes[0].control

    c =
      R.returned(c, c.processes[1], {:managed_link_target, @labels}, {:catena_managed_process, 0})

    {:value, b} = c.processes[1].control
    c = R.returned(c, c.processes[0], {:managed_unlink}, a)
    c = R.returned(c, c.processes[1], {:managed_observe}, b)
    refute R.ready?(c, c.processes[1])
    c = put_in(c.processes[0].status, :terminated) |> R.after_step()
    refute R.ready?(c, c.processes[1])
    assert c.processes[1].mailbox == [{99, :unit}]
  end

  @tag obligations: ~w(OT-OBL-009)
  test "an old queued link signal cannot complete a newly registered relationship" do
    c = %{processes: %{0 => process(0), 1 => process(1)}}

    c =
      R.returned(c, c.processes[0], {:managed_link_target, @labels}, {:catena_managed_process, 1})

    {:value, old} = c.processes[0].control
    generation = elem(old, 3)
    c = Map.put(c, :managed_signals, [{0, 1, generation, {:trapped, 99}}])
    c = R.returned(c, c.processes[0], {:managed_unlink}, old)

    c =
      R.returned(c, c.processes[0], {:managed_link_target, @labels}, {:catena_managed_process, 1})

    {:value, fresh} = c.processes[0].control
    refute fresh == old
    c = R.returned(c, c.processes[0], {:managed_observe}, fresh)
    assert {:handled, c} = R.before(c, c.processes[0])
    refute R.ready?(c, c.processes[0])
    assert c.processes[0].mailbox == [{99, :unit}]
  end

  @tag obligations: ~w(OT-OBL-010)
  test "entering trapping later cannot recover a previously ignored normal exit" do
    c = %{processes: %{0 => %{process(0) | managed_trapping: false}, 1 => process(1)}, trace: []}

    c =
      R.returned(c, c.processes[0], {:managed_link_target, @labels}, {:catena_managed_process, 1})

    {:value, link} = c.processes[0].control
    c = put_in(c.processes[1].status, :terminated) |> R.after_step()
    assert {:handled, c} = R.before(c, c.processes[0])
    c = put_in(c.processes[0].managed_trapping, true)
    c = R.returned(c, c.processes[0], {:managed_observe}, link)
    assert c.processes[0].status == :trapped
    assert c.processes[0].trap == :invalid_managed_relationship
  end

  @tag obligations: ~w(OT-OBL-008)
  test "a delivered managed failure cannot be replaced by a later normal worker return" do
    c = %{processes: %{0 => %{process(0) | managed_trapping: false}, 1 => process(1)}, trace: []}

    c =
      R.returned(c, c.processes[0], {:managed_link_target, @labels}, {:catena_managed_process, 1})

    c =
      put_in(c.processes[1].status, :trapped)
      |> put_in([:processes, 1, :trap], 99)
      |> R.after_step()

    assert {:handled, selected} = R.before(c, c.processes[0])

    completed =
      put_in(selected.processes[0].status, :terminated)
      |> put_in([:processes, 0, :result], :unit)
      |> R.after_step()

    assert completed.processes[0].status == :exited
    assert completed.processes[0].result == {:linked, 1, {:trapped, 99}}
  end

  defp process(id),
    do: %{
      id: id,
      name: "test",
      managed: true,
      managed_grace: 10,
      managed_trapping: true,
      status: :running,
      control: {:value, :unit},
      stack: [],
      mailbox: [{99, :unit}],
      mailbox_type: :unit,
      result: nil,
      trap: nil
    }
end
