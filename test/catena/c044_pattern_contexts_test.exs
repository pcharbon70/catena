defmodule Catena.C044PatternContextsTest do
  use ExUnit.Case, async: false

  alias Catena.{Effect.Runtime, LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43)

  @match_kernel """
  (module C044Match
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c044-match")
    (export value main)
    (def main
      (signature Int (uses))
      (match 2
        (case 0 10)
        (case 1 11)
        (case _ 12))))
  """

  describe "revision registration" do
    @tag obligations: ~w(PC-OBL-001)
    test "0.1.38 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.43"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.38" in LanguageVersion.compilable_revisions()
      refute "0.1.38" in LanguageVersion.artifact_versions()
      refute "0.1.38" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("pattern-contexts", "0.1.38")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-38-pattern-contexts")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "pattern-contexts/the-three-context-classes.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.43"}}} = Catena.decode_source_text("")
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Catena, :pattern_let, 3)
      refute function_exported?(Catena, :pattern_parameter, 2)
      refute function_exported?(Catena, :view_pattern, 1)
      refute function_exported?(Catena, :exception_clause, 0)
    end
  end

  describe "the exhaustive context is unchanged" do
    @tag obligations: ~w(PC-OBL-002 PC-OBL-003)
    test "match keeps C045 authority, agreeing on both targets" do
      assert {:ok, core} = Catena.check_kernel(@match_kernel)

      assert {:ok, 12, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C044Match, binary, _} = Catena.compile_kernel(@match_kernel)

      assert {:module, :C044Match} =
               :code.load_binary(:C044Match, ~c"c044_match.beam", binary)

      assert apply(:C044Match, :main, []) == 12

      on_exit(fn ->
        :code.purge(:C044Match)
        :code.delete(:C044Match)
      end)
    end

    @tag obligations: ~w(PC-OBL-003)
    test "coverage and redundancy diagnostics keep their identities" do
      non_exhaustive = String.replace(@match_kernel, "(case _ 12)", "")

      assert {:error, %{id: "M001"}} = Catena.check_kernel(non_exhaustive)

      redundant =
        JSON.encode!(%{
          "version" => "0.1.7",
          "edition" => "0.1",
          "language_revision" => "0.1.7",
          "previews" => [],
          "origin" => "test://c044/redundant",
          "module" => "C044Redundant",
          "source" => "c044.catena.json",
          "exports" => ["main"],
          "type_exports" => [],
          "type_groups" => [],
          "types" => [],
          "traits" => [],
          "instances" => [],
          "templates" => [],
          "imports" => [],
          "definitions" => [
            %{
              "name" => "main",
              "parameters" => [],
              "signature" => %{
                "forall" => [],
                "type" => %{"tag" => "integer"},
                "uses" => []
              },
              "body" => %{
                "tag" => "match",
                "scrutinee" => %{"tag" => "boolean", "value" => true},
                "clauses" => [
                  %{
                    "pattern" => %{"tag" => "wildcard"},
                    "body" => %{"tag" => "integer", "value" => 1}
                  },
                  %{
                    "pattern" => %{"tag" => "boolean", "value" => true},
                    "body" => %{"tag" => "integer", "value" => 2}
                  }
                ]
              }
            }
          ],
          "effects" => [],
          "handlers" => []
        })

      assert {:error, %{id: "M002"}} = Catena.check_json(redundant)
    end
  end

  describe "binding positions stay plain-named" do
    @tag obligations: ~w(PC-OBL-004)
    test "a pattern-position let binder rejects at the kernel boundary" do
      pattern_let = """
      (module C044PatternLet
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c044-pattern-let")
        (export value main)
        (def main
          (signature Int (uses))
          (let (constructor Pair (bind a) (bind b))
            (construct Pair 1 2)
            (var a))))
      """

      assert {:error, %{}} = Catena.check_kernel(pattern_let)
    end

    @tag obligations: ~w(PC-OBL-004)
    test "the JSON-AST let keeps its plain name binder" do
      program =
        JSON.encode!(%{
          "version" => "0.1.7",
          "edition" => "0.1",
          "language_revision" => "0.1.7",
          "previews" => [],
          "origin" => "test://c044/json-let",
          "module" => "C044JsonLet",
          "source" => "c044.catena.json",
          "exports" => ["main"],
          "type_exports" => [],
          "type_groups" => [],
          "types" => [],
          "traits" => [],
          "instances" => [],
          "templates" => [],
          "imports" => [],
          "definitions" => [
            %{
              "name" => "main",
              "parameters" => [],
              "signature" => %{
                "forall" => [],
                "type" => %{"tag" => "integer"},
                "uses" => []
              },
              "body" => %{
                "tag" => "let",
                "pattern" => [
                  %{"tag" => "bind", "name" => "value"}
                ],
                "value" => %{"tag" => "integer", "value" => 7},
                "body" => %{"tag" => "variable", "name" => "value"}
              }
            }
          ],
          "effects" => [],
          "handlers" => []
        })

      assert {:error, _} = Catena.check_json(program)
    end
  end

  describe "reserved and excluded contexts" do
    @tag obligations: ~w(PC-OBL-005 PC-OBL-006 PC-OBL-008 PC-OBL-009)
    test "no reserved or excluded context has an entry point" do
      refute function_exported?(Catena, :comprehension, 0)
      refute function_exported?(Catena, :generator_qualifier, 0)
      refute function_exported?(Catena, :public_receive, 0)
      refute function_exported?(Catena, :exception_clause, 0)
      refute function_exported?(Catena, :pattern_synonym, 1)
      refute function_exported?(Catena, :active_pattern, 1)
    end

    @tag obligations: ~w(PC-OBL-007)
    test "handler clauses keep plain parameters and the resumption binder" do
      fixture = File.read!("test/fixtures/c010-kernel.catena")

      assert {:ok, core} = Catena.check_kernel(fixture)

      handler = Catena.Kernel.Stepper.run(core, "main")
      assert {:ok, {_, _, _}, %{root_status: :terminated}} = handler
    end

    @tag obligations: ~w(PC-OBL-003)
    test "determinism: repeated runs repeat" do
      assert {:ok, core} = Catena.check_kernel(@match_kernel)

      assert {:ok, first, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, second, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert first == second

      {result, _trace} =
        Runtime.capture_trace(fn -> Catena.Kernel.Stepper.run(core, "main") end)

      assert {:ok, 12, %{root_status: :terminated}} = result
    end
  end
end
