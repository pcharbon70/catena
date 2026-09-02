defmodule Catena.C038CompileTimeTest do
  use ExUnit.Case, async: false

  alias Catena.{ImplementationLimits, LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45)

  describe "revision registration" do
    @tag obligations: ~w(CE-OBL-001 CE-OBL-003)
    test "0.1.34 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.45"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.34" in LanguageVersion.compilable_revisions()
      refute "0.1.34" in LanguageVersion.artifact_versions()
      refute "0.1.34" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("compile-time-evaluation", "0.1.34")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-34-compile-time-evaluation")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "compile-time-evaluation/the-compile-time-stance.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.45"}}} = Catena.decode_source_text("")
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Catena, :const_eval, 1)
      refute function_exported?(Catena, :expand_macro, 2)
      refute function_exported?(Catena, :eval_attribute, 2)
    end
  end

  describe "generated derivations" do
    @tag obligations: ~w(CE-OBL-004 CE-OBL-006)
    test "derived folds carry compiler provenance and recompile byte-identically" do
      source = option_program("C038DerivedFold", ["fold"]) |> JSON.encode!()

      assert {:ok, :C038DerivedFold, first, metadata} = Catena.compile_json(source)

      fold = Enum.find(metadata.core.definitions, &(&1.name == "Option.fold"))
      assert fold != nil
      assert fold.generated? == true

      derived = fold.expression
      assert derived.tag == :derived_fold
      assert derived.provenance == :compiler_derived

      assert {:ok, :C038DerivedFold, second, _} = Catena.compile_json(source)
      assert first == second

      assert {:module, :C038DerivedFold} =
               :code.load_binary(:C038DerivedFold, ~c"c038_fold.beam", first)

      value = apply(:C038DerivedFold, :make, [])
      assert apply(:C038DerivedFold, :"Option.fold", [0, fn item -> item end, value]) == 7

      on_exit(fn ->
        :code.purge(:C038DerivedFold)
        :code.delete(:C038DerivedFold)
      end)
    end
  end

  describe "the restriction table" do
    @tag obligations: ~w(CE-OBL-005 CE-OBL-007)
    test "the three meta-evaluator budgets are configured and unchanged" do
      condition_budget = ImplementationLimits.configured(:condition_normalization_nodes)
      specification_budget = ImplementationLimits.configured(:specification_example_steps)
      kernel_budget = ImplementationLimits.configured(:kernel_reference_steps)

      assert is_integer(condition_budget) and condition_budget > 0
      assert is_integer(specification_budget) and specification_budget > 0
      assert is_integer(kernel_budget) and kernel_budget > 0
    end

    @tag obligations: ~w(CE-OBL-007)
    test "condition evaluation rejects under its own budget family" do
      recursive =
        JSON.encode!(%{
          "version" => "0.1.3",
          "origin" => "test://c038-cond",
          "module" => "BudgetCondition",
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

      assert {:error, %{id: id}} = Catena.check_json(recursive)
      assert id in ["CND002", "CND004"]
    end
  end

  describe "absence and determinism" do
    @tag obligations: ~w(CE-OBL-002 CE-OBL-008)
    test "no compile-time execution surface exists and compilation repeats deterministically" do
      refute function_exported?(Catena, :const_eval, 1)
      refute function_exported?(Catena, :expand_macro, 2)
      refute function_exported?(Catena, :eval_attribute, 2)
      refute function_exported?(Catena.Derive, :evaluate, 2)

      source = option_program("C038Determinism", ["fold"]) |> JSON.encode!()

      assert {:ok, :C038Determinism, first, first_metadata} = Catena.compile_json(source)
      assert {:ok, :C038Determinism, second, second_metadata} = Catena.compile_json(source)

      assert first == second
      assert first_metadata.interface_binary == second_metadata.interface_binary
      assert length(first_metadata.core.definitions) == length(second_metadata.core.definitions)
    end
  end

  defp option_program(module, derivations) do
    option =
      type_decl(
        "Option",
        [parameter("a")],
        [constructor("None", []), constructor("Some", [variable_type("a")])],
        derivations
      )

    make =
      definition(
        "make",
        [],
        forall([], named_type("Option", [integer_type()])),
        construct("Option.Some", [integer(7)])
      )

    main =
      definition(
        "main",
        [],
        forall([], integer_type()),
        match_expr(construct("Option.Some", [integer(7)]), [
          clause(constructor_pattern("Option.None", []), integer(0)),
          clause(constructor_pattern("Option.Some", [bind("value")]), variable("value"))
        ])
      )

    %{
      "version" => "0.1.2",
      "origin" => "test://c038/#{module}",
      "module" => module,
      "type_groups" => [type_group([option])],
      "type_exports" => [%{"name" => "Option", "visibility" => "transparent"}],
      "imports" => [],
      "exports" => ["make", "main"],
      "definitions" => [make, main],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "effects" => [],
      "handlers" => []
    }
  end

  defp type_group(declarations), do: %{"declarations" => declarations}

  defp type_decl(name, parameters, constructors, derivations) do
    %{
      "name" => name,
      "parameters" => parameters,
      "constructors" => constructors,
      "derivations" => derivations
    }
  end

  defp parameter(name), do: %{"name" => name, "kind" => "Type"}

  defp constructor(name, fields),
    do: %{"name" => name, "fields" => fields, "existentials" => []}

  defp definition(name, parameters, signature, body),
    do: %{"name" => name, "parameters" => parameters, "signature" => signature, "body" => body}

  defp forall(variables, type), do: %{"forall" => variables, "type" => type}

  defp integer_type, do: %{"tag" => "integer"}
  defp variable_type(name), do: %{"tag" => "variable", "name" => name}

  defp named_type(name, arguments),
    do: %{"tag" => "named", "name" => name, "arguments" => arguments}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp construct(constructor, arguments),
    do: %{"tag" => "construct", "constructor" => constructor, "arguments" => arguments}

  defp match_expr(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp clause(pattern, body), do: %{"pattern" => pattern, "body" => body}

  defp constructor_pattern(name, arguments),
    do: %{"tag" => "constructor", "constructor" => name, "arguments" => arguments}

  defp bind(name), do: %{"tag" => "bind", "name" => name}
end
