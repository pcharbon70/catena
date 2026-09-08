defmodule Catena.SupervisionDescriptionTest do
  use ExUnit.Case, async: false
  alias Catena.Supervision.Description

  defp fixture do
    source = """
    (module SupervisionDescriptionProbe (edition 0.1) (revision 0.1.8) (origin "test://supervision")
      (process Worker (mailbox Int) (params) (receive (case (bind reason) (trap (var reason)))))
      (process Wrong (mailbox Unit) (params (argument Int)) (unit))
      (def main (signature Unit (uses)) (unit)))
    """

    {:ok, parsed} = Catena.Kernel.Parser.parse(source)
    {:ok, core} = Catena.Task.Kernel.check_selected(parsed, %{})

    child = %{
      id: "worker",
      process: "Worker",
      restart: :permanent,
      grace_ns: 100_000_000,
      shutdown_ms: 1_000,
      provisioning: :fresh_empty
    }

    {core, child, %{strategy: :one_for_one, intensity: 2, period: 5}}
  end

  test "descriptions compile deterministic native child specs with checked actor bodies" do
    {core, child, flags} = fixture()
    assert {:ok, description} = Description.check(core, [child], flags)
    assert {:ok, module, binary, metadata} = Description.compile(description)
    assert {:ok, ^module, ^binary, ^metadata} = Description.compile(description)
    assert {:module, ^module} = :code.load_binary(module, ~c"supervision-experiment.beam", binary)
    old = Process.flag(:trap_exit, true)
    {:ok, sup} = Catena.Supervision.Runtime.start_link(metadata.flags, metadata.children)
    [{"worker", first, _, _}] = :supervisor.which_children(sup)
    monitor = Process.monitor(first)
    Catena.Task.Managed.send_message({Catena.Task.Managed, first}, 42)
    assert_receive {:DOWN, ^monitor, :process, ^first, {:catena_managed_trap, 42}}
    # Calls to the supervisor serialize after its selected failure handling.
    assert eventually_new(sup, first, 100)
    :gen_server.stop(sup, :shutdown, 5_000)
    assert_receive {:EXIT, ^sup, :shutdown}
    Process.flag(:trap_exit, old)
    :code.purge(module)
    :code.delete(module)
  end

  test "unknown and parameterized children, inherited capabilities and invalid budgets are rejected" do
    {core, child, flags} = fixture()

    for invalid <- [
          %{child | process: "missing"},
          %{child | process: "Wrong"},
          %{child | provisioning: {:inherit, make_ref()}},
          %{child | grace_ns: -1},
          %{child | grace_ns: 1_000_000_000},
          %{child | shutdown_ms: 0}
        ] do
      assert {:error, %{id: "SUP001"}} = Description.check(core, [invalid], flags)
    end

    assert {:error, _} = Description.check(core, [child, child], flags)
    assert {:error, _} = Description.check(core, [child], %{flags | intensity: 0})
    {:ok, description} = Description.check(core, [child], flags)
    assert {:error, _} = Description.verify(Map.put(description, :unchecked, true))
  end

  test "artifact admission binds the checked origin and rejects altered bytes or manifests" do
    {core, child, flags} = fixture()
    {:ok, description} = Description.check(core, [child], flags)
    {:ok, module, binary, metadata} = Description.compile(description)
    assert :ok = Description.verify_artifact(description, module, binary, metadata.manifest)

    assert {:error, _} =
             Description.verify_artifact(description, module, binary <> <<0>>, metadata.manifest)

    assert {:error, _} =
             Description.verify_artifact(
               description,
               module,
               binary,
               Map.put(metadata.manifest, "origin", "other")
             )

    assert {:error, _} =
             Description.start_artifact(description, module, binary <> <<0>>, metadata.manifest)

    assert metadata.manifest["origin"] == core.origin

    assert metadata.manifest["artifact_digest"] ==
             Base.encode16(:crypto.hash(:sha256, binary), case: :lower)
  end

  test "verified artifact startup is owned by the calling managed lifecycle" do
    {core, child, flags} = fixture()
    {:ok, description} = Description.check(core, [child], flags)
    {:ok, module, binary, metadata} = Description.compile(description)
    owner = self()

    actor =
      Catena.Task.Managed.spawn_actor(
        fn ->
          {:ok, link} = Description.start_artifact(description, module, binary, metadata.manifest)
          send(owner, {:started_tree, link})
          :finish = Catena.Task.Managed.receive_message()
          :unit
        end,
        1_000_000_000
      )

    actor_pid = Catena.Task.Managed.pid(actor)
    monitor = Process.monitor(actor_pid)
    assert_receive {:started_tree, {Catena.Task.Managed, ^actor_pid, supervisor, _}}
    [{"worker", worker, _, _}] = :supervisor.which_children(supervisor)
    Catena.Task.Managed.send_message(actor, :finish)
    assert_receive {:DOWN, ^monitor, :process, ^actor_pid, :normal}
    refute Process.alive?(supervisor)
    refute Process.alive?(worker)
    :code.purge(module)
    :code.delete(module)
  end

  test "supervision selection and emitted artifact revision are exact" do
    {core, child, flags} = fixture()

    for revision <- Catena.LanguageVersion.before(:typed_supervision) do
      assert {:error, _} =
               Description.check(core, [child], flags,
                 selection: Catena.LanguageVersion.legacy_selection(revision)
               )
    end

    {:ok, description} = Description.check(core, [child], flags)

    assert {:error, _} =
             Description.verify(%{
               description
               | selection: Catena.LanguageVersion.legacy_selection("0.1.52")
             })

    {:ok, module, binary, _} = Description.compile(description)
    {:ok, {^module, [compile_info: info]}} = :beam_lib.chunks(binary, [:compile_info])
    assert info[:catena_language_revision] == ~c"0.1.56"
    assert info[:catena_frontend] == ~c"typed-supervision-0.1.56"
    refute "0.1.56" in Catena.LanguageVersion.interface_versions()
    refute "0.1.56" in Catena.LanguageVersion.signed_format_versions()
  end

  defp eventually_new(_, _, 0), do: false

  defp eventually_new(sup, old, attempts) do
    case :supervisor.which_children(sup) do
      [{"worker", pid, _, _}] when is_pid(pid) and pid != old ->
        true

      _ ->
        Process.sleep(1)
        eventually_new(sup, old, attempts - 1)
    end
  end
end
