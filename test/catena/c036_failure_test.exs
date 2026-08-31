defmodule Catena.C036FailureTest do
  use ExUnit.Case, async: false

  alias Catena.Effect.Runtime
  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37)

  @divergent """
  (module C036Diverge
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c036-diverge")
    (export value main)
    (def spin
      (signature (Fn Int (effects) Int) (uses))
      (fn (n Int) (call (var spin) (var n))))
    (def main
      (signature Int (uses))
      (call (var spin) 1)))
  """

  @process_witness """
  (module C036Process
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c036-process")
    (export value main)
    (process Boom
      (mailbox Int)
      (params)
      (receive
        (case (bind message)
          (trap (var message)))))
    (def main
      (signature Int (uses Process))
      (let target
        (spawn Boom)
        (sequence
          (send (var target) 42)
          7))))
  """

  describe "revision registration" do
    @tag obligations: ~w(FT-OBL-001 FT-OBL-006)
    test "0.1.32 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.37"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.32" in LanguageVersion.compilable_revisions()
      refute "0.1.32" in LanguageVersion.artifact_versions()
      refute "0.1.32" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("runtime-failure-taxonomy", "0.1.32")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-32-runtime-failure-taxonomy")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "runtime-failure-taxonomy/the-single-outcome.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.37"}}} = Catena.decode_source_text("")
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Catena, :exit_signal, 2)
      refute function_exported?(Catena, :assert_form, 1)
      refute function_exported?(Catena, :raise_foreign, 1)
    end
  end

  describe "the single outcome and reason identity" do
    @tag obligations: ~w(FT-OBL-002 FT-OBL-004)
    test "trap terminals agree on the reason value across stepper and BEAM" do
      assert {100, nil} = stepper_trap(trap_kernel("C036TrapA", 100))
      assert {100, nil} = beam_trap("C036TrapA", trap_kernel("C036TrapA", 100))

      assert {200, nil} = stepper_trap(trap_kernel("C036TrapB", 200))
      assert {200, nil} = beam_trap("C036TrapB", trap_kernel("C036TrapB", 200))

      assert stepper_trap(trap_kernel("C036TrapA", 100)) ==
               stepper_trap(trap_kernel("C036TrapA", 100))
    end

    @tag obligations: ~w(FT-OBL-002)
    test "trap shapes are non-values; the partition holds at the classifier" do
      refute Catena.Values.value?({:trap, :boom, %{}})
      assert :trap = Catena.Values.classify({:trap, :boom, %{}})
      assert Catena.Values.value?({:catena_constructor, :Some, {}})

      assert {:value, 5} =
               Catena.Values.terminal_witness({:ok, 5, %{root_status: :terminated}})
    end

    @tag obligations: ~w(FT-OBL-003)
    test "the process-context witness: a trapping child discards its mailbox and spares its spawner" do
      assert {:ok, core} = Catena.check_kernel(@process_witness)

      assert {:ok, 7, outcome} = Catena.Kernel.Stepper.run(core, "main")
      assert outcome.root_status == :terminated

      processes = outcome.processes
      assert is_list(processes)

      root = Enum.find(processes, &(&1.name == "$root"))
      assert root.status == :terminated
      assert root.result == 7
      assert root.trap == nil

      child = Enum.find(processes, &(&1.name == "Boom"))
      assert child.status == :trapped
      assert child.trap == 42
      assert child.mailbox == []
    end

    @tag obligations: ~w(FT-OBL-003 FT-OBL-008)
    test "the stepper's trap outcome records the reason and repeats deterministically" do
      assert {:ok, core} = Catena.check_kernel(trap_kernel("C036Reason", 33))

      assert {:trap, 33, first} = Catena.Kernel.Stepper.run(core, "main")
      assert {:trap, 33, second} = Catena.Kernel.Stepper.run(core, "main")
      assert first.root_status == second.root_status
    end
  end

  describe "the mapping and its exclusions" do
    @tag obligations: ~w(FT-OBL-005 FT-OBL-007)
    test "typed failure classifies as values; the reserved kinds have no producers" do
      none = {:catena_constructor, :None, {}}
      some = {:catena_constructor, :Some, {1}}

      assert Catena.Values.value?(none) and Catena.Values.value?(some)
      assert Catena.Values.comparable?(some)

      refute function_exported?(Catena, :faulting_operator, 0)
      refute function_exported?(Catena, :divide, 2)

      refute :trap == :budget_exhausted
    end

    @tag obligations: ~w(FT-OBL-002 FT-OBL-008)
    test "divergence remains budget exhaustion, never a trap — the C034 regression" do
      assert {:ok, core} = Catena.check_kernel(@divergent)

      assert {:budget_exhausted, _} = Catena.Kernel.Stepper.run(core, "main", [], budget: 400)
      assert {:value, _} = Catena.Values.terminal_witness({:ok, 1, %{}})
    end
  end

  defp trap_kernel(module, reason) do
    """
    (module #{module}
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c036/#{module}")
      (export value main)
      (def main
        (signature Int (uses))
        (trap #{reason})))
    """
  end

  defp trap_program(module, reason) do
    trap_kernel(module, reason)
  end

  defp stepper_trap(source) do
    assert {:ok, core} = Catena.check_kernel(source)

    case Catena.Kernel.Stepper.run(core, "main") do
      {:trap, reason, _outcome} -> {reason, nil}
      {:ok, value, _outcome} -> {:value, value}
    end
  end

  defp beam_trap(module, source) do
    assert {:ok, core} = Catena.check_kernel(source)
    module_atom = String.to_atom(module)

    assert {:ok, ^module_atom, binary, _metadata} = Catena.compile_kernel(source)

    assert {:module, ^module_atom} =
             :code.load_binary(module_atom, ~c"c036-#{module}.beam", binary)

    result =
      try do
        {:ok, apply(module_atom, :main, [])}
      rescue
        e in ErlangError -> {:trap, e.original}
      end

    on_exit(fn ->
      :code.purge(module_atom)
      :code.delete(module_atom)
    end)

    case result do
      {:trap, {:catena_trap, reason}} -> {reason, nil}
      {:ok, value} -> {:value, value}
    end
  end
end
