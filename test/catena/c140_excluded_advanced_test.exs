defmodule Catena.C140ExcludedAdvancedTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44)

  describe "revision registration" do
    @tag obligations: ~w(EA-OBL-001)
    test "0.1.44 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.44"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.44" in LanguageVersion.compilable_revisions()
      refute "0.1.44" in LanguageVersion.artifact_versions()
      refute "0.1.44" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} ==
               LanguageLifecycle.state("excluded-advanced-type-features", "0.1.44")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-44-excluded-advanced-type-features")
        )

      assert change["affects"] == ~w(static-meaning)
      assert change["summary"] =~ "excluded advanced type features"

      assert String.contains?(
               change["specification"],
               "excluded-advanced-type-features/the-exclusion-table-and-gate.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.44"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :impredicative_instantiate, 2)
      refute function_exported?(Catena, :linear_check, 1)
      refute function_exported?(Catena, :type_family, 1)
    end
  end

  describe "the exclusion table" do
    @tag obligations: ~w(EA-OBL-002 EA-OBL-004)
    test "excluded-form attempts reject at the profile boundary" do
      quantifier_in_parameter =
        program_with_parameter_type(%{
          "tag" => "forall",
          "variables" => [0],
          "type" => %{"tag" => "variable", "index" => 0}
        })

      assert {:error, %{id: "T012"}} = Catena.check_json(quantifier_in_parameter)

      for tag <- ["impredicative", "linear", "dependent", "type_family", "higher_kinded"] do
        assert {:error, _} = Catena.check_json(program_with_parameter_type(%{"tag" => tag}))
      end

      gadt_without_signature = gadt_program(false)
      assert {:error, %{id: "T010"}} = Catena.check_json(gadt_without_signature)
    end

    @tag obligations: ~w(EA-OBL-005)
    test "the checked profile still checks: signature-directed GADTs unchanged" do
      gadt_with_signature = gadt_program(true)
      assert {:ok, core} = Catena.check_json(gadt_with_signature)

      assert {:ok, 41} = Catena.Reference.Evaluator.run(core, "main")
    end

    @tag obligations: ~w(EA-OBL-002 EA-OBL-007)
    test "no excluded spelling is accepted on any frontend" do
      for tag <- ["exists", "forall_type", "dependent", "typelevel", "linear_arrow"] do
        assert {:error, _} = Catena.check_json(program_with_parameter_type(%{"tag" => tag}))
      end

      kernel_attempt = """
      (module C140Kernel
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c140/kernel")
        (export value main)
        (def main
          (signature (Fn (forall a a) (effects) Int) (uses))
          (fn (f (forall a a))
            0)))
      """

      assert {:error, _} = Catena.check_kernel(kernel_attempt)
    end
  end

  describe "the gate and determinism" do
    @tag obligations: ~w(EA-OBL-003 EA-OBL-006)
    test "the seven-point gate is the only amendment route" do
      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-44-excluded-advanced-type-features")
        )

      assert is_map(change)
      assert change["classification"] in ["stable", "compatible-addition"]

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :advanced_feature_bundle, 0)
      refute function_exported?(Catena, :admit_excluded_form, 1)
    end

    @tag obligations: ~w(EA-OBL-007)
    test "determinism: repeated checks repeat" do
      source = gadt_program(true)

      assert {:ok, first} = Catena.check_json(source)
      assert {:ok, second} = Catena.check_json(source)

      assert {:ok, value_a} = Catena.Reference.Evaluator.run(first, "main")
      assert {:ok, value_b} = Catena.Reference.Evaluator.run(second, "main")
      assert value_a == value_b
    end
  end

  defp program_with_parameter_type(parameter_type) do
    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c140/param",
      "module" => "C140Param",
      "source" => "c140.catena.json",
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
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}, "uses" => []},
          "body" => %{"tag" => "integer", "value" => 0}
        },
        %{
          "name" => "accept",
          "parameters" => ["f"],
          "signature" => %{
            "forall" => [],
            "type" => %{
              "tag" => "function",
              "parameter" => parameter_type,
              "result" => %{"tag" => "integer"},
              "effect" => []
            },
            "uses" => []
          },
          "body" => %{"tag" => "integer", "value" => 0}
        }
      ],
      "effects" => [],
      "handlers" => []
    })
  end

  defp gadt_program(with_signature) do
    expr_group = %{
      "declarations" => [
        %{
          "name" => "Expr",
          "parameters" => [%{"name" => "a", "kind" => "Type"}],
          "constructors" => [
            %{
              "name" => "IntLit",
              "fields" => [%{"name" => "value", "type" => %{"tag" => "integer"}}],
              "existentials" => [],
              "result" => %{
                "tag" => "named",
                "name" => "Expr",
                "arguments" => [%{"tag" => "integer"}]
              }
            },
            %{
              "name" => "BoolLit",
              "fields" => [%{"name" => "value", "type" => %{"tag" => "boolean"}}],
              "existentials" => [],
              "result" => %{
                "tag" => "named",
                "name" => "Expr",
                "arguments" => [%{"tag" => "boolean"}]
              }
            }
          ],
          "derivations" => []
        }
      ]
    }

    definition =
      if with_signature do
        %{
          "name" => "evaluate",
          "parameters" => ["expression"],
          "signature" => %{
            "forall" => ["a"],
            "type" => %{
              "tag" => "function",
              "parameter" => %{
                "tag" => "named",
                "name" => "Expr",
                "arguments" => [%{"tag" => "variable", "name" => "a"}]
              },
              "result" => %{"tag" => "variable", "name" => "a"},
              "effect" => []
            }
          },
          "body" => %{
            "tag" => "match",
            "scrutinee" => %{"tag" => "variable", "name" => "expression"},
            "clauses" => [
              %{
                "pattern" => %{
                  "tag" => "constructor",
                  "constructor" => "Expr.IntLit",
                  "fields" => [
                    %{"name" => "value", "pattern" => %{"tag" => "bind", "name" => "value"}}
                  ]
                },
                "body" => %{"tag" => "variable", "name" => "value"}
              },
              %{
                "pattern" => %{
                  "tag" => "constructor",
                  "constructor" => "Expr.BoolLit",
                  "fields" => [
                    %{"name" => "value", "pattern" => %{"tag" => "bind", "name" => "value"}}
                  ]
                },
                "body" => %{"tag" => "variable", "name" => "value"}
              }
            ]
          }
        }
      else
        %{
          "name" => "evaluate",
          "parameters" => ["expression"],
          "signature" => nil,
          "body" => %{
            "tag" => "match",
            "scrutinee" => %{"tag" => "variable", "name" => "expression"},
            "clauses" => [
              %{
                "pattern" => %{
                  "tag" => "constructor",
                  "constructor" => "Expr.IntLit",
                  "arguments" => [%{"tag" => "bind", "name" => "value"}]
                },
                "body" => %{"tag" => "variable", "name" => "value"}
              },
              %{
                "pattern" => %{
                  "tag" => "constructor",
                  "constructor" => "Expr.BoolLit",
                  "arguments" => [%{"tag" => "bind", "name" => "value"}]
                },
                "body" => %{"tag" => "boolean", "value" => false}
              }
            ]
          }
        }
      end

    definition =
      if with_signature do
        definition
      else
        Map.delete(definition, "signature")
      end

    JSON.encode!(%{
      "version" => "0.1.2",
      "origin" => "test://c140/gadt",
      "module" => "C140Gadt",
      "type_groups" => [expr_group],
      "type_exports" => [%{"name" => "Expr", "visibility" => "transparent"}],
      "imports" => [],
      "exports" => ["main"],
      "definitions" => [
        definition,
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{
            "tag" => "call",
            "callee" => %{"tag" => "variable", "name" => "evaluate"},
            "arguments" => [
              %{
                "tag" => "construct",
                "constructor" => "Expr.IntLit",
                "fields" => [
                  %{"name" => "value", "value" => %{"tag" => "integer", "value" => 41}}
                ]
              }
            ]
          }
        }
      ]
    })
  end
end
