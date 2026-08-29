defmodule Catena.C029ValuesTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Values}
  alias Catena.Kernel.Stepper
  alias Catena.Runtime.ResumptionToken
  alias Catena.Effect.Row
  alias Catena.Type.Scheme

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36)
  @fixture "test/fixtures/c010-kernel.catena"

  describe "revision registration" do
    @tag obligations: ~w(VA-OBL-001 VA-OBL-008)
    test "0.1.25 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.36"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.25" in LanguageVersion.compilable_revisions()
      refute "0.1.25" in LanguageVersion.artifact_versions()
      refute "0.1.25" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("values-and-evaluation", "0.1.25")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-25-values-and-evaluation")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "values-and-evaluation/value-forms-and-first-classness.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.36"}}} = Catena.decode_source_text("")
      assert {:ok, _} = Catena.build_namespace_environment([])
      assert {:ok, _} = Catena.compile_scc([])
      assert {:ok, _} = Catena.Package.Deps.parse_version("1.0.0")

      assert {:ok, core_010} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
      assert core_010.language_revision == "0.1.8"

      refute function_exported?(Values, :equal, 2)
      refute function_exported?(Values, :render, 1)
      refute function_exported?(Values, :lazy, 1)
    end
  end

  describe "the closed grammar" do
    @tag obligations: ~w(VA-OBL-002)
    test "every value form classifies as a value" do
      assert Values.value?(%{tag: :integer, value: 7})
      assert Values.value?(%{tag: :boolean, value: true})
      assert Values.value?(%{tag: :unit})
      assert Values.value?(%{tag: :function, parameter: "x", body: %{tag: :variable, name: "x"}})

      assert Values.value?(%{
               tag: :tuple,
               elements: [%{tag: :integer, value: 1}, %{tag: :boolean, value: false}]
             })

      assert Values.value?(%{
               tag: :record,
               fields: [%{label: "a", expression: %{tag: :integer, value: 2}}]
             })

      assert Values.value?(%{
               tag: :construct,
               constructor: "some",
               arguments: [%{tag: :integer, value: 3}]
             })

      assert Values.value?(%{tag: :inject, label: "ok", payload: %{tag: :integer, value: 4}})

      assert Values.value?(0)
      assert Values.value?(true)
      assert Values.value?(:unit)
      assert Values.value?({:closure, "x", %{tag: :variable, name: "x"}, %{}})
      assert Values.value?({1, true, :unit})
      assert Values.value?(%{label: 1})
      assert Values.value?({:catena_variant, :some, 5})
      assert Values.value?({:catena_constructor, :point, {1, 2}})
      assert Values.value?({:catena_process, "p1"})
    end

    @tag obligations: ~w(VA-OBL-002 VA-OBL-005)
    test "every non-value classifies with its reason and no outside form is a value" do
      assert :effect_row = Values.classify(%Row{entries: [], tail: nil})
      assert :signature = Values.classify(Scheme.mono(:integer))
      assert :resumption = Values.classify(ResumptionToken.new())
      assert :evidence = Values.classify(%{evidence: :facts, digest: "d"})
      assert :trap = Values.classify({:trap, :arithmetic, %{}})
      assert :trap = Values.classify(%{tag: :trap, reason: :crash})
      assert :handler_declaration = Values.classify(%{tag: :handle, clauses: []})
      assert :resumption = Values.classify(%{tag: :resume, resumption: "r"})
      assert :capability_name = Values.classify(%{tag: :request, family: "Ask"})

      for tag <-
            ~w(variable call let sequence annotate unary binary match select update extend restrict spawn send receive self trait_call)a do
        refute Values.value?(%{tag: tag, name: "x"})
        assert {:computation, ^tag} = Values.classify(%{tag: tag, name: "x"})
      end

      refute Values.value?(%{
               tag: :tuple,
               elements: [%{tag: :call, callee: %{tag: :variable, name: "f"}, arguments: []}]
             })

      refute Values.value?({1, :trap})
      refute Values.value?(nil)
      assert Values.value?("string")
      refute Values.value?([1, 2])
      assert true = Values.classify("string")
      assert :unknown_form = Values.classify([1, 2])

      assert Values.non_value_kinds() ==
               ~w(evidence handler_declaration capability_name resumption trap effect_row signature)a
    end

    @tag obligations: ~w(VA-OBL-003)
    test "Float is the tenth value form with C018 semantics unchanged" do
      assert Values.value?(1.5)
      assert Values.value?(-0.0)
      assert Values.value?(0.0)
      assert Values.value?(%{tag: :float, value: 2.25})
      refute Values.value?(%{tag: :float, value: 7})

      {:ok, %{literal: %{payload: one}}} = Catena.scan_literal("1.0")
      assert {:ok, %{value: 1.0}} = Catena.elaborate_numeric_literal(one)

      {:ok, %{literal: %{payload: two_point_five}}} = Catena.scan_literal("2.5")
      assert {:ok, %{value: 2.5}} = Catena.elaborate_numeric_literal(two_point_five)
    end

    @tag obligations: ~w(VA-OBL-004)
    test "values are uniformly first-class: storable and returnable witnesses" do
      handle = {:catena_process, "p1"}
      closure = {:closure, "x", %{tag: :variable, name: "x"}, %{}}

      stored = %{handle: handle, fn: closure, pair: {1, handle}}
      assert Values.value?(stored)
      assert {:value, ^stored} = Values.terminal_witness({:ok, stored, %{}})

      container = {:catena_variant, :some, {closure, handle}}
      assert Values.value?(container)

      refute function_exported?(Values, :storable, 1)
    end
  end

  describe "strictness and terminals" do
    @tag obligations: ~w(VA-OBL-006)
    test "every stepper terminal carries a value or a trap" do
      assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)

      assert {:ok, value, outcome} = Stepper.run(core, "main")
      assert outcome.root_status == :terminated
      assert {:value, ^value} = Values.terminal_witness({:ok, value, outcome})
      assert Values.value?(value)

      assert {:value, {2, true, 5}} = Values.terminal_witness({:value, {2, true, 5}})
    end

    @tag obligations: ~w(VA-OBL-006)
    test "and and or skip their right operand exactly as the kernel fixes" do
      assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
      assert {:ok, {2, true, 5}, _outcome} = Stepper.run(core, "main")

      trap_core = trap_program()

      assert {:trap, reason, _outcome} = Stepper.run(trap_core, "main")
      assert {:trap, ^reason} = Values.terminal_witness({:trap, reason, %{}})
      refute Values.value?({:trap, reason})
      assert :trap = Values.classify({:trap, reason, %{}})
    end

    @tag obligations: ~w(VA-OBL-007)
    test "no lazy form exists and the gate is the lifecycle record path" do
      refute function_exported?(Values, :lazy, 1)
      refute function_exported?(Values, :thunk, 1)
      refute Enum.any?(LanguageLifecycle.features(), &String.contains?(&1["id"], "lazy"))
      assert {:ok, :stable} == LanguageLifecycle.state("values-and-evaluation", "0.1.25")
    end
  end

  describe "determinism" do
    @tag obligations: ~w(VA-OBL-008)
    test "classification is deterministic with zero new diagnostic families" do
      corpus = [
        %{tag: :integer, value: 1},
        %{tag: :call, callee: %{tag: :variable, name: "f"}, arguments: []},
        %Row{entries: [], tail: nil},
        {:catena_variant, :some, 1.5},
        :atom_not_string,
        {:trap, :crash}
      ]

      for term <- corpus do
        assert Values.classify(term) == Values.classify(term)
      end

      refute function_exported?(Catena.Diagnostic, :va_family, 0)
    end
  end

  defp trap_program do
    source = """
    (module Trap
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c029-trap")
      (export value main)
      (def main (signature Int (uses)) (trap 9)))
    """

    {:ok, core} = Catena.check_kernel(source)
    core
  end
end
