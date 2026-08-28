defmodule Catena.C034RecursionTest do
  use ExUnit.Case, async: false

  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31)

  describe "revision registration" do
    @tag obligations: ~w(RT-OBL-001 RT-OBL-004)
    test "0.1.31 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.31"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.31" in LanguageVersion.compilable_revisions()
      refute "0.1.31" in LanguageVersion.artifact_versions()
      refute "0.1.31" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("recursion-and-termination", "0.1.31")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-31-recursion-and-termination")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "recursion-and-termination/program-recursion-is-unrestricted.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.31"}}} = Catena.decode_source_text("")
      assert true = Catena.Values.value?(1.5)
      assert :eq = Catena.Values.compare(1, 1)

      refute function_exported?(Catena, :terminates, 1)
      refute function_exported?(Catena, :termination_check, 1)
      refute function_exported?(Catena, :require_total, 1)
    end
  end

  describe "unrestricted recursion" do
    @non_tail """
    (module C034NonTail
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c034-nontail")
      (export value main)
      (def sum_to
        (signature (Fn Int (effects) Int) (uses))
        (fn (n Int)
          (match (var n)
            (case 0 0)
            (case _ (add (var n) (call (var sum_to) (subtract (var n) 1)))))))
      (def main
        (signature Int (uses))
        (call (var sum_to) 10000)))
    """

    @tail """
    (module C034Tail
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c034-tail")
      (export value main)
      (def count
        (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
        (fn (n Int)
          (fn (acc Int)
            (match (var n)
              (case 0 (var acc))
              (case _ (call (call (var count) (subtract (var n) 1)) (add (var acc) 1)))))))
      (def main
        (signature Int (uses))
        (call (call (var count) 400) 0)))
    """

    @divergent """
    (module C034Diverge
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c034-diverge")
      (export value main)
      (def spin
        (signature (Fn Int (effects) Int) (uses))
        (fn (n Int) (call (var spin) (var n))))
      (def main
        (signature Int (uses))
        (call (var spin) 1)))
    """

    @tag obligations: ~w(RT-OBL-002)
    test "non-tail recursion completes on compiled BEAM at 10,000 depth" do
      assert {:ok, :C034NonTail, binary, _metadata} = Catena.compile_kernel(@non_tail)

      assert {:module, :C034NonTail} =
               :code.load_binary(:C034NonTail, ~c"c034_nontail.beam", binary)

      assert apply(:C034NonTail, :main, []) == 50_005_000

      on_exit(fn ->
        :code.purge(:C034NonTail)
        :code.delete(:C034NonTail)
      end)
    end

    @tag obligations: ~w(RT-OBL-002 RT-OBL-008)
    test "tail recursion terminates within the stepper budget — the C032 shape" do
      assert {:ok, core} = Catena.check_kernel(@tail)
      assert {:ok, 400, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")
    end

    @tag obligations: ~w(RT-OBL-003)
    test "divergence is budget exhaustion on the stepper, never a trap" do
      assert {:ok, core} = Catena.check_kernel(@divergent)

      assert {:budget_exhausted, _configuration} =
               Catena.Kernel.Stepper.run(core, "main", [], budget: 500)

      assert {:budget_exhausted, _again} =
               Catena.Kernel.Stepper.run(core, "main", [], budget: 500)
    end
  end

  describe "the separation table" do
    @tag obligations: ~w(RT-OBL-007)
    test "recursive condition dependencies reject as CND004, unchanged" do
      json =
        JSON.encode!(%{
          "version" => "0.1.3",
          "origin" => "test://c034-cond",
          "module" => "RecursiveCondition",
          "exports" => [],
          "type_exports" => [],
          "type_groups" => [],
          "imports" => [],
          "effects" => [],
          "handlers" => [],
          "definitions" => [
            %{
              "name" => "self",
              "kind" => "condition",
              "parameters" => ["x"],
              "signature" => %{
                "forall" => [],
                "type" => %{
                  "tag" => "function",
                  "parameter" => %{"tag" => "integer"},
                  "result" => %{"tag" => "boolean"},
                  "effect" => []
                }
              },
              "body" => %{
                "tag" => "call",
                "callee" => %{"tag" => "variable", "name" => "self"},
                "arguments" => [%{"tag" => "variable", "name" => "x"}]
              }
            }
          ]
        })

      assert {:error, %{id: id}} = Catena.check_json(json)
      assert id in ["CND002", "CND004"]
    end

    @tag obligations: ~w(RT-OBL-005 RT-OBL-006)
    test "the meta-level regimes hold their bounds and no unbounded evaluator exists" do
      refute function_exported?(Catena, :compile_time_eval, 2)
      refute function_exported?(Catena, :unbounded_eval, 1)
      refute function_exported?(Catena.Specification, :run_unbounded, 1)

      limit = Catena.ImplementationLimits.configured(:specification_example_steps)
      assert is_integer(limit) and limit > 0
    end
  end

  describe "determinism and exclusions" do
    @tag obligations: ~w(RT-OBL-008)
    test "recursion outcomes repeat and no new family appears" do
      assert {:ok, core} = Catena.check_kernel(@tail)

      assert {:ok, 400, first} = Catena.Kernel.Stepper.run(core, "main")
      assert {:ok, 400, second} = Catena.Kernel.Stepper.run(core, "main")
      assert first.root_status == second.root_status

      refute function_exported?(Catena.Diagnostic, :rt_family, 0)
    end
  end
end
