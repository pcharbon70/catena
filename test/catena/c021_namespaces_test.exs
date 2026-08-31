defmodule Catena.C021NamespacesTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}
  alias Catena.Namespace.Resolution

  @tag obligations: ~w(NS-OBL-001 NS-OBL-014)
  test "0.1.17 is an exact deterministic revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.39"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39)

    refute "0.1.17" in LanguageVersion.compilable_revisions()
    refute "0.1.17" in LanguageVersion.interface_versions()
    refute "0.1.17" in LanguageVersion.artifact_versions()
    refute "0.1.17" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("namespaces-and-shadowing", "0.1.17")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-17-namespaces-and-shadowing")
      )

    assert change["affects"] == ~w(static-meaning diagnostics)

    assert String.contains?(
             change["specification"],
             "namespaces-and-shadowing/namespace-inventory-and-spelling.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "namespaces-and-shadowing", required: "0.1.22"}
            }} =
             Catena.build_namespace_environment([], language_selection: selection("0.1.16"))

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, %{selection: %{language_revision: "0.1.15"}}} = Catena.tokenize_source("1")

    assert {:ok, %{selection: %{language_revision: "0.1.16"}}} =
             Catena.resolve_file_unit("", "A.cat", [])

    refute function_exported?(Catena, :parse_imports, 1)
    refute function_exported?(Catena, :check_visibility, 1)
    refute function_exported?(Catena, :compile_environment, 1)
  end

  @tag obligations: ~w(NS-OBL-002)
  test "categories are disjoint and one spelling coexists across them" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :values, spelling: "vec"},
        %{event: :declare, category: :types, spelling: "Vec"},
        %{event: :declare, category: :constructors, spelling: "Vec"}
      ])

    assert {:ok, %Resolution{category: :values, spelling: "vec"}} =
             Catena.resolve_name(env, %{category: :values, spelling: "vec"})

    assert {:ok, %Resolution{category: :types}} =
             Catena.resolve_name(env, %{category: :types, spelling: "Vec"})

    assert {:ok, %Resolution{category: :constructors}} =
             Catena.resolve_name(env, %{category: :constructors, spelling: "Vec"})

    for category <- [
          :values,
          :types,
          :constructors,
          :traits,
          :effects,
          :handlers,
          :entries,
          :modules
        ] do
      assert {:error, %{id: "NSP003"}} =
               Catena.resolve_name(env, %{category: category, spelling: "Missing"})
    end
  end

  @tag obligations: ~w(NS-OBL-003)
  test "the hard spelling-class partition fails both directions as NSP002" do
    for {category, spelling} <- [
          {:types, "vec"},
          {:constructors, "nil"},
          {:traits, "functor"},
          {:effects, "log"},
          {:handlers, "catch"},
          {:entries, "serve"},
          {:modules, "json"}
        ] do
      assert {:error, %{id: "NSP002", details: %{reason: "spelling_class_violation"}}} =
               Catena.build_namespace_environment([
                 %{event: :declare, category: category, spelling: spelling}
               ])
    end

    for {category, spelling} <- [
          {:values, "Vec"},
          {:fields, "Field"},
          {:operations, "Log"},
          {:typevars, "A"}
        ] do
      assert {:error, %{id: "NSP002", details: %{reason: "spelling_class_violation"}}} =
               Catena.build_namespace_environment([
                 %{event: :declare, category: category, spelling: spelling}
               ])
    end

    assert {:ok, _} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :typevars, spelling: "a"},
               %{event: :declare, category: :fields, spelling: "name"}
             ])
  end

  @tag obligations: ~w(NS-OBL-004)
  test "same-scope duplicates fail per uniqueness domain as NSP001" do
    for category <- [:values, :types, :constructors, :traits, :effects] do
      spelling = if category in [:values], do: "dup", else: "Dup"

      assert {:error, %{id: "NSP001", details: %{reason: "duplicate_declaration"}}} =
               Catena.build_namespace_environment([
                 %{event: :declare, category: category, spelling: spelling},
                 %{event: :declare, category: category, spelling: spelling}
               ])
    end

    assert {:error, %{id: "NSP001", details: %{reason: "duplicate_declaration"}}} =
             Catena.build_namespace_environment([
               :open_scope,
               %{event: :declare, category: :typevars, spelling: "a"},
               %{event: :declare, category: :typevars, spelling: "a"}
             ])

    assert {:error, %{id: "NSP001"}} =
             Catena.build_namespace_environment([:close_scope])

    assert {:ok, env} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :values, spelling: "x"},
               :open_scope,
               %{event: :declare, category: :values, spelling: "x"}
             ])

    assert {:ok, _} = Catena.resolve_name(env, %{category: :values, spelling: "x"})
  end

  @tag obligations: ~w(NS-OBL-005)
  test "governed identities never participate in program resolution" do
    assert {:error, %{id: "NSP001", details: %{reason: "governed_separation"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :governed, spelling: "Claim"}
             ])

    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :values, spelling: "claim"}
      ])

    assert {:error, %{id: "NSP001", details: %{reason: "unknown_category"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :claims, spelling: "c1"}
             ])

    assert {:ok, _} = Catena.resolve_name(env, %{category: :values, spelling: "claim"})
  end

  @tag obligations: ~w(NS-OBL-006)
  test "qualification is exactly two segments with NSP005 beyond" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :modules, spelling: "Json"},
        %{event: :import_set, origin: "Json", category: :constructors, names: ["Null"]}
      ])

    assert {:ok, %Resolution{spelling: "Null", origin: "Json", scope_depth: 0}} =
             Catena.resolve_name(env, %{
               category: :constructors,
               spelling: "Json.Null",
               qualified: true
             })

    for deep <- ["A.B.C", "Json.Null.Extra"] do
      assert {:error, %{id: "NSP005", details: %{reason: "invalid_qualification_depth"}}} =
               Catena.resolve_name(env, %{
                 category: :constructors,
                 spelling: deep,
                 qualified: true
               })
    end

    assert {:error, %{id: "NSP003"}} =
             Catena.resolve_name(env, %{
               category: :constructors,
               spelling: "Other.Null",
               qualified: true
             })
  end

  @tag obligations: ~w(NS-OBL-007)
  test "innermost bindings win with silent cross-category-safe shadowing" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :values, spelling: "x"},
        %{event: :declare, category: :types, spelling: "X"},
        :open_scope,
        %{event: :declare, category: :values, spelling: "x"},
        :open_scope,
        %{event: :declare, category: :values, spelling: "x"}
      ])

    assert {:ok, %Resolution{scope_depth: 2}} =
             Catena.resolve_name(env, %{category: :values, spelling: "x"})

    assert {:ok, %Resolution{category: :types, scope_depth: 0}} =
             Catena.resolve_name(env, %{category: :types, spelling: "X"})

    {:ok, env_outer} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :values, spelling: "x"},
        :open_scope,
        :close_scope
      ])

    assert {:ok, %Resolution{scope_depth: 0}} =
             Catena.resolve_name(env_outer, %{category: :values, spelling: "x"})
  end

  @tag obligations: ~w(NS-OBL-008)
  test "type variables scope per quantifier with type shadowing and value separation" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :types, spelling: "List"},
        :open_scope,
        %{event: :declare, category: :typevars, spelling: "list"},
        %{event: :declare, category: :typevars, spelling: "other"}
      ])

    assert {:ok, %Resolution{category: :typevars, spelling: "list", scope_depth: 1}} =
             Catena.resolve_name(env, %{category: :typevars, spelling: "list"})

    assert {:ok, %Resolution{category: :types, spelling: "List", scope_depth: 0}} =
             Catena.resolve_name(env, %{category: :types, spelling: "List"})

    assert {:error, %{id: "NSP003"}} =
             Catena.resolve_name(env, %{category: :values, spelling: "list"})

    {:ok, env2} =
      Catena.build_namespace_environment([
        :open_scope,
        %{event: :declare, category: :typevars, spelling: "a"},
        :close_scope
      ])

    assert {:error, %{id: "NSP003"}} =
             Catena.resolve_name(env2, %{category: :typevars, spelling: "a"})
  end

  @tag obligations: ~w(NS-OBL-009)
  test "local beats imported and ambiguity is order-independent" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :constructors, spelling: "Null"},
        %{event: :import_set, origin: "Json", category: :constructors, names: ["Null"]}
      ])

    assert {:ok, %Resolution{origin: nil, scope_depth: 0}} =
             Catena.resolve_name(env, %{category: :constructors, spelling: "Null"})

    for order <- [
          ["Json", "Option"],
          ["Option", "Json"]
        ] do
      events =
        Enum.map(order, fn origin ->
          %{event: :import_set, origin: origin, category: :constructors, names: ["Null"]}
        end)

      {:ok, env} = Catena.build_namespace_environment(events)

      assert {:error,
              %{id: "NSP004", details: %{reason: "ambiguous_import", origins: ["Json", "Option"]}}} =
               Catena.resolve_name(env, %{category: :constructors, spelling: "Null"})
    end

    {:ok, env3} =
      Catena.build_namespace_environment([
        %{event: :import_set, origin: "Json", category: :constructors, names: ["Null", "Bool"]},
        %{event: :import_set, origin: "Option", category: :constructors, names: ["None"]}
      ])

    assert {:ok, %Resolution{origin: "Json"}} =
             Catena.resolve_name(env3, %{category: :constructors, spelling: "Null"})

    assert {:ok, %Resolution{origin: "Option"}} =
             Catena.resolve_name(env3, %{category: :constructors, spelling: "None"})
  end

  @tag obligations: ~w(NS-OBL-010)
  test "unbound references fail as NSP003 in every category" do
    {:ok, env} = Catena.build_namespace_environment([])

    for category <- [:values, :types, :constructors, :traits, :effects, :handlers, :entries] do
      spelling = if category == :values, do: "missing", else: "Missing"

      assert {:error, %{id: "NSP003", details: %{reason: "unbound_reference"}}} =
               Catena.resolve_name(env, %{category: category, spelling: spelling})
    end
  end

  @tag obligations: ~w(NS-OBL-011)
  test "stable diagnostics carry spelling, category, and all colliding origins" do
    assert {:error, %{id: "NSP002", details: %{category: :types, spelling: "vec"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :types, spelling: "vec"}
             ])

    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :import_set, origin: "A", category: :values, names: ["v"]},
        %{event: :import_set, origin: "B", category: :values, names: ["v"]},
        %{event: :import_set, origin: "C", category: :values, names: ["v"]}
      ])

    assert {:error,
            %{
              id: "NSP004",
              details: %{origins: ["A", "B", "C"], spelling: "v", category: :values}
            }} =
             Catena.resolve_name(env, %{category: :values, spelling: "v"})
  end

  @tag obligations: ~w(NS-OBL-012 NS-OBL-013)
  test "boundaries are deterministic, tree-or-diagnostic, and source-only" do
    events = [
      %{event: :declare, category: :values, spelling: "x"},
      :open_scope,
      %{event: :declare, category: :values, spelling: "y"},
      %{event: :import_set, origin: "Json", category: :constructors, names: ["Null"]}
    ]

    first = Catena.build_namespace_environment(events)
    assert first == Catena.build_namespace_environment(events)

    {:ok, env} = first
    assert {:ok, r} = Catena.resolve_name(env, %{category: :values, spelling: "y"})
    assert Catena.resolve_name(env, %{category: :values, spelling: "y"}) == {:ok, r}

    assert {:error, %{id: "NSP001", details: %{reason: "invalid_event"}}} =
             Catena.build_namespace_environment([%{event: :explode}])

    refute function_exported?(Catena.Namespace, :parse_source, 1)
    refute function_exported?(Catena.Namespace, :tokenize, 1)
    refute function_exported?(Catena.Namespace, :check_types, 1)
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}
end
