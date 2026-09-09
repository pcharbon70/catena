defmodule Catena.CallingLifecycleTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(CV-OBL-009)
  alias Catena.Calling.Lifecycle

  test "process and OTP callback shapes bind actual exports and execute under the existing managed owner" do
    source = """
    (module CallingLifecycle (edition 0.1) (revision 0.1.8) (origin "test://calling-lifecycle")
      (process Worker (mailbox Int) (params) (receive (case (bind reason) (trap (var reason)))))
      (def main (signature Unit (uses)) (unit)))
    """

    {:ok, parsed} = Catena.Kernel.Parser.parse(source)
    {:ok, core} = Catena.Task.Kernel.check_selected(parsed, %{})

    child = %{
      id: "worker",
      process: "Worker",
      restart: :temporary,
      grace_ns: 100_000_000,
      shutdown_ms: 1_000,
      provisioning: :fresh_empty
    }

    {:ok, description} =
      Catena.Supervision.Description.check(core, [child], %{
        strategy: :one_for_one,
        intensity: 2,
        period: 5
      })

    assert {:ok, artifact} = Lifecycle.build(description)
    assert :ok = Lifecycle.verify(artifact, description)

    assert [
             %{kind: :process_entry, beam_arity: 0},
             %{kind: :foreign_call, beam_arity: 2},
             %{kind: :callback, beam_arity: 1}
           ] = artifact.descriptor.entries

    assert {:error, :invalid_lifecycle_calling_artifact} =
             Lifecycle.start(
               %{artifact | descriptor: %{artifact.descriptor | compiler_digest: "forged"}},
               description
             )

    assert false == :code.is_loaded(artifact.module)
    owner = self()

    actor =
      Catena.Task.Managed.spawn_actor(
        fn ->
          {:ok, link} = Lifecycle.start(artifact, description)
          send(owner, {:started, link})
          :finish = Catena.Task.Managed.receive_message()
          :unit
        end,
        1_000_000_000
      )

    pid = Catena.Task.Managed.pid(actor)
    monitor = Process.monitor(pid)
    assert_receive {:started, {Catena.Task.Managed, ^pid, supervisor, _}}, 2_000
    [{"worker", worker, _, _}] = :supervisor.which_children(supervisor)
    assert Process.alive?(worker)
    Catena.Task.Managed.send_message(actor, :finish)
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 2_000
    refute Process.alive?(supervisor)
    refute Process.alive?(worker)
    :code.purge(artifact.module)
    :code.delete(artifact.module)
  end
end
