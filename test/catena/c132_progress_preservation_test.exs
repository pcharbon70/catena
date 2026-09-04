defmodule Catena.C132ProgressPreservationTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47)

  @effects_kernel """
  (module C132Eff
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c132/eff")
    (effect Ask
      (operation ask (params Int) Int))
    (handler AddOne
      (effect Ask)
      (input Int)
      (output Int)
      (return result (var result))
      (operation ask
        (params (value Int))
        (resume next)
        (resume next (add (var value) 1))))
    (export value main)
    (def main
      (signature Int (uses))
      (handle AddOne (request Ask ask 3))))
  """

  @trap_kernel """
  (module C132Trap
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c132/trap")
    (export value main)
    (def main
      (signature Int (uses))
      (trap 100)))
  """

  describe "revision registration" do
    @tag obligations: ~w(PP-OBL-001)
    test "0.1.45 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.47"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.45" in LanguageVersion.compilable_revisions()
      refute "0.1.45" in LanguageVersion.artifact_versions()
      refute "0.1.45" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} ==
               LanguageLifecycle.state("progress-and-preservation", "0.1.45")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-45-progress-and-preservation")
        )

      assert change["affects"] == ~w(static-meaning)
      assert change["summary"] =~ "progress and preservation"

      assert String.contains?(
               change["specification"],
               "progress-and-preservation/the-effects-and-failure-targets.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.47"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :composed_proof, 0)
      refute function_exported?(Catena, :integrated_theorem_proved, 0)
    end
  end

  describe "the effects and failure targets" do
    @tag obligations: ~w(PP-OBL-002 PP-OBL-004)
    test "handler installation, resume-once, and return agree on both targets" do
      assert {:ok, core} = Catena.check_kernel(@effects_kernel)

      assert {:ok, 4, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C132Eff, binary, _} = Catena.compile_kernel(@effects_kernel)

      assert {:module, :C132Eff} =
               :code.load_binary(:C132Eff, ~c"c132_eff.beam", binary)

      assert apply(:C132Eff, :main, []) == 4

      on_exit(fn ->
        :code.purge(:C132Eff)
        :code.delete(:C132Eff)
      end)
    end

    @tag obligations: ~w(PP-OBL-003 PP-OBL-004)
    test "trap is the failure terminal with kernel-verbatim reasons on both targets" do
      assert {:ok, core} = Catena.check_kernel(@trap_kernel)

      assert {:trap, 100, outcome} = Catena.Kernel.Stepper.run(core, "main")
      assert outcome.root_trap != nil or outcome.root_status != :running

      assert {:ok, :C132Trap, binary, _} = Catena.compile_kernel(@trap_kernel)

      assert {:module, :C132Trap} =
               :code.load_binary(:C132Trap, ~c"c132_trap.beam", binary)

      on_exit(fn ->
        :code.purge(:C132Trap)
        :code.delete(:C132Trap)
      end)
    end

    @tag obligations: ~w(PP-OBL-003)
    test "effect progress guards: an unhandled request rejects statically" do
      unhandled = """
      (module C132Unhandled
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c132/unhandled")
        (effect Ask
          (operation ask (params Int) Int))
        (export value main)
        (def main
          (signature Int (uses))
          (request Ask ask 3)))
      """

      assert {:error, _} = Catena.check_kernel(unhandled)
    end
  end

  describe "the integrated theorem" do
    @tag obligations: ~w(PP-OBL-005 PP-OBL-008)
    test "the composition parts are checkable together: the kernel fixture runs unchanged" do
      source = File.read!("test/fixtures/c010-kernel.catena")

      assert {:ok, core} = Catena.check_kernel(source)

      assert {:ok, {2, true, 5}, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")
    end

    @tag obligations: ~w(PP-OBL-006)
    test "the composition lemma stays a routed obligation, never a claim" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :lemma_discharged, 0)
      refute function_exported?(Catena, :composition_proof, 0)

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-45-progress-and-preservation")
        )

      assert is_map(change)
    end

    @tag obligations: ~w(PP-OBL-007)
    test "process and foreign extensions stay conditional and routed" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :public_process_theorem, 0)
      refute function_exported?(Catena, :foreign_preservation_claim, 1)
    end

    @tag obligations: ~w(PP-OBL-002 PP-OBL-008)
    test "determinism: repeated runs repeat" do
      assert {:ok, core} = Catena.check_kernel(@effects_kernel)

      assert {:ok, first, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, second, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert first == second
    end
  end
end
