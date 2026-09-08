defmodule Catena.ResourceProcessTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Stepper, Backend}

  @tag obligations: ~w(RS-OBL-003 RS-OBL-009)
  test "a raw local actor releases its scoped resource before normal termination" do
    source = """
    (module ResourceActor (edition 0.1) (revision 0.1.8) (origin "test://resource/actor")
      (export value main) (export value release_seen)
      (def acquire (signature Int (uses)) 7)
      (def release (signature (Fn Int (effects) Unit) (uses)) (fn (payload Int) (call (var release_seen) (var payload))))
      (def release_seen (signature (Fn Int (effects) Unit) (uses)) (fn (payload Int) (unit)))
      (process Worker (mailbox Int) (params) (receive (case (bind start) (unit))))
      (def main (signature (Process Int) (uses Process)) (spawn Worker)))
    """

    {:ok, parsed} = Parser.parse(source)
    [acquire, release, observer, main] = parsed.definitions
    [worker] = parsed.processes
    [clause] = worker.body.clauses

    scope = %{
      tag: :resource_scope,
      acquire: acquire.expression,
      release: release.expression,
      body: clause.body,
      grace_ns: 1_000_000_000,
      span: clause.body.span
    }

    worker = %{worker | body: %{worker.body | clauses: [%{clause | body: scope}]}}

    assert {:ok, core} =
             Catena.Resource.Kernel.check(
               %{parsed | definitions: [observer, main], processes: [worker]},
               %{}
             )

    assert {:ok, configuration} = Stepper.initial(core, "main")
    # The retained reference mailbox is explicit; raw send semantics remain unchanged.
    configuration =
      Enum.reduce(1..2, configuration, fn _, config ->
        {:ok, next} = Stepper.step(config, 0)
        next
      end)

    child = configuration.processes[1]
    configuration = put_in(configuration, [:processes, 1], %{child | mailbox: [{0, 1}]})
    assert {:ok, {:catena_process, 1}, outcome} = Stepper.run_configuration(configuration)
    assert [%{payload: 7}] = Enum.filter(outcome.trace, &(&1.label == :resource_release_started))
    assert Enum.find(outcome.processes, &(&1.pid == 1)).status == :terminated

    assert {:ok, module, binary, _} =
             Backend.compile(core)

    assert {:module, ^module} = :code.load_binary(module, ~c"resource-actor.beam", binary)
    pid = apply(module, :main, [])
    monitor = Process.monitor(pid)
    session = :trace.session_create(:resource_actor_test, self(), [])

    try do
      :trace.function(session, {module, :release_seen, 1}, true, [:local])
      :trace.process(session, pid, true, [:call, :set_on_spawn])
      send(pid, 1)
      assert_receive {:trace, _release_worker, :call, {^module, :release_seen, [7]}}, 1000
      assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 1000
    after
      :trace.session_destroy(session)
      if Process.alive?(pid), do: Process.exit(pid, :kill)
      :code.delete(module)
      :code.purge(module)
    end
  end
end
