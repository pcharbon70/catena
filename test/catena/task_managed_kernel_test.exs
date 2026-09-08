defmodule Catena.TaskManagedKernelTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Verifier}
  alias Catena.Task.{Kernel, Instrument, Managed}

  @tag obligations: ~w(OT-OBL-001 OT-OBL-012)
  test "checked managed spawn isolates protocol envelopes from the actor receive" do
    parsed = fixture()
    assert {:ok, core} = Kernel.check_selected(parsed, %{})
    assert :ok = Verifier.verify(core)
    {module, binary} = compiled(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"managed-core-experiment", binary)

    try do
      actor = apply(module, :main, [])
      pid = Managed.pid(actor)
      monitor = Process.monitor(pid)
      assert :unit = Managed.send_message(actor, :unit)
      assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 1000
    after
      :code.purge(module)
      :code.delete(module)
    end

    [main] = core.definitions
    entry = %{main.expression.selected_entry | name: "not_the_checked_worker"}

    forged = %{
      core
      | definitions: [%{main | expression: %{main.expression | selected_entry: entry}}]
    }

    assert {:error, _} = Verifier.verify(forged)
  end

  @tag obligations: ~w(OT-OBL-012)
  test "raw spawn cannot enter a body requiring managed identity" do
    parsed = fixture()
    [worker] = parsed.processes
    [main] = parsed.definitions

    body = %{
      tag: :sequence,
      first: %{tag: :managed_self, span: worker.span},
      second: worker.body,
      span: worker.span
    }

    parsed = %{parsed | processes: [%{worker | body: body}]}
    assert {:ok, _} = Kernel.check(parsed, %{})
    raw = %{main | signature: {:process, :unit}, expression: %{main.expression | tag: :spawn}}
    assert {:error, %{id: "PRC003"}} = Kernel.check(%{parsed | definitions: [raw]}, %{})
  end

  @tag obligations: ~w(OT-OBL-001 OT-OBL-002)
  test "managed result types cannot be relabelled as an old selected core" do
    assert {:ok, core} = Kernel.check(fixture(), %{})

    for version <- ["0.1.8", "0.1.50", "0.1.51"] do
      forged =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          core,
          &Map.put(&2, &1, version)
        )

      assert {:error, _} = Verifier.verify(forged)
    end
  end

  defp fixture do
    source = """
    (module ManagedCoreProbe (edition 0.1) (revision 0.1.8) (origin "test://managed/core")
      (export value main)
      (process Worker (mailbox Unit) (params) (receive (case (bind start) (unit))))
      (def main (signature (Process Unit) (uses Process)) (spawn Worker)))
    """

    {:ok, parsed} = Parser.parse(source)
    [main] = parsed.definitions

    %{
      parsed
      | definitions: [
          %{
            main
            | signature: {:managed_process, :unit},
              expression:
                Map.merge(main.expression, %{tag: :managed_spawn, grace_ns: 1_000_000_000})
          }
        ]
    }
  end

  defp compiled(%{version: "0.1.52"} = core) do
    assert {:ok, module, binary, metadata} = Catena.Kernel.Backend.compile(core)
    assert metadata.artifact_version == "0.1.52"
    assert {:ok, ^module, ^binary, _} = Catena.Kernel.Backend.compile(core)
    {module, binary}
  end

  defp compiled(core) do
    assert {:ok, forms} = Instrument.lower(core)
    assert {:ok, module, binary} = :compile.forms(forms, [:binary, :return_errors])
    {module, binary}
  end
end
