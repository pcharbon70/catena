defmodule Catena.C022ImportExportsTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Namespace}
  alias Catena.Namespace.{Environment, ImportWarning, Resolution}

  @json %{
    event: :provide_module,
    module: "Json",
    digest: "d1",
    exports: [
      %{category: :constructors, spelling: "Null"},
      %{category: :types, spelling: "Doc", transparency: :abstract},
      %{category: :values, spelling: "parse"}
    ]
  }

  @option %{
    event: :provide_module,
    module: "Option",
    digest: "d2",
    exports: [%{category: :constructors, spelling: "Null"}]
  }

  @tag obligations: ~w(IM-OBL-001 IM-OBL-013)
  test "0.1.18 is an exact deterministic revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.32"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32)

    refute "0.1.18" in LanguageVersion.compilable_revisions()
    refute "0.1.18" in LanguageVersion.interface_versions()
    refute "0.1.18" in LanguageVersion.artifact_versions()
    refute "0.1.18" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("imports-and-exports", "0.1.18")

    change =
      Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-18-imports-and-exports"))

    assert change["affects"] == ~w(static-meaning diagnostics)

    assert String.contains?(
             change["specification"],
             "imports-and-exports/export-declarations-and-visibility.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "namespaces-and-shadowing", required: "0.1.22"}
            }} =
             Catena.build_namespace_environment([@json], language_selection: selection("0.1.17"))

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, %{selection: %{language_revision: "0.1.15"}}} = Catena.tokenize_source("1")

    assert {:ok, %{selection: %{language_revision: "0.1.16"}}} =
             Catena.resolve_file_unit("", "A.cat", [])

    assert "IMP001" in LanguageLifecycle.warning_ids()

    refute function_exported?(Catena, :parse_import_declarations, 1)
    refute function_exported?(Catena, :check_visibility_modes, 1)
    refute function_exported?(Catena, :assemble_package, 1)
  end

  @tag obligations: ~w(IM-OBL-002 IM-OBL-003 IM-OBL-004)
  test "exports are explicit, validated, and carry transparency modes" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :values, spelling: "x"},
        %{event: :export, category: :values, spelling: "x"},
        %{event: :declare, category: :types, spelling: "Doc"},
        %{event: :export, category: :types, spelling: "Doc", transparency: :abstract},
        %{event: :declare, category: :constructors, spelling: "Nil"}
      ])

    assert Enum.any?(env.exports, &match?({:values, "x", nil}, &1))
    assert Enum.any?(env.exports, &match?({:types, "Doc", :abstract}, &1))
    refute Enum.any?(env.exports, &match?({:constructors, "Nil", _}, &1))

    assert {:error, %{id: "EXP001", details: %{reason: "undeclared_export"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :values, spelling: "x"},
               %{event: :export, category: :values, spelling: "missing"}
             ])

    assert {:error, %{id: "EXP001", details: %{reason: "transparency_only_on_types"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :values, spelling: "x"},
               %{event: :export, category: :values, spelling: "x", transparency: :transparent}
             ])

    assert {:error, %{id: "EXP001", details: %{reason: "invalid_transparency_mode"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :types, spelling: "Doc"},
               %{event: :export, category: :types, spelling: "Doc", transparency: :opaque}
             ])

    assert {:error, %{id: "NSP001", details: %{reason: "duplicate_declaration"}}} =
             Catena.build_namespace_environment([
               %{event: :declare, category: :values, spelling: "x"},
               %{event: :export, category: :values, spelling: "x"},
               %{event: :export, category: :values, spelling: "x"}
             ])
  end

  @tag obligations: ~w(IM-OBL-005)
  test "admission is qualification plus explicit list with the empty qualified-only form" do
    {:ok, env} =
      Catena.build_namespace_environment([
        @json,
        @option,
        %{event: :import_module, module: "Json", digest: "d1", names: [constructors: "Null"]},
        %{event: :import_module, module: "Option", digest: "d2", names: []}
      ])

    assert {:ok, %Resolution{origin: "Json"}} =
             Catena.resolve_name(env, %{category: :constructors, spelling: "Null"})

    assert {:ok, %Resolution{origin: "Json", spelling: "Null"}} =
             Catena.resolve_name(env, %{
               category: :constructors,
               spelling: "Json.Null",
               qualified: true
             })

    assert {:ok, %Resolution{origin: "Option", spelling: "Null"}} =
             Catena.resolve_name(env, %{
               category: :constructors,
               spelling: "Option.Null",
               qualified: true
             })

    assert {:ok, %Resolution{origin: "Json", spelling: "parse"}} =
             Catena.resolve_name(env, %{
               category: :values,
               spelling: "Json.parse",
               qualified: true
             })

    assert {:error, %{id: "NSP003"}} =
             Catena.resolve_name(env, %{category: :values, spelling: "parse"})
  end

  @tag obligations: ~w(IM-OBL-002 IM-OBL-006)
  test "private names never resolve elsewhere and validation rejects bad imports" do
    {:ok, env} =
      Catena.build_namespace_environment([
        @json,
        %{event: :import_module, module: "Json", digest: "d1", names: []}
      ])

    assert {:error, %{id: "NSP003"}} =
             Catena.resolve_name(env, %{
               category: :constructors,
               spelling: "Json.Private",
               qualified: true
             })

    assert {:error, %{id: "IMP002", details: %{reason: "unexported_import", module: "Json"}}} =
             Catena.build_namespace_environment([
               @json,
               %{event: :import_module, module: "Json", digest: "d1", names: [values: "missing"]}
             ])

    assert {:error, %{id: "IMP002"}} =
             Catena.build_namespace_environment([
               @json,
               %{
                 event: :import_module,
                 module: "Json",
                 digest: "d1",
                 names: [constructors: "Doc"]
               }
             ])

    assert {:error, %{id: "IMP003", details: %{reason: "unknown_module", module: "Nope"}}} =
             Catena.build_namespace_environment([
               %{event: :import_module, module: "Nope", digest: "d", names: []}
             ])

    assert {:error, %{id: "NSP001", details: %{reason: "duplicate_declaration"}}} =
             Catena.build_namespace_environment([
               @json,
               %{
                 event: :import_module,
                 module: "Json",
                 digest: "d1",
                 names: [constructors: "Null"]
               },
               %{
                 event: :import_module,
                 module: "Json",
                 digest: "d1",
                 names: [constructors: "Null"]
               }
             ])
  end

  @tag obligations: ~w(IM-OBL-007)
  test "no wildcard, hiding, renaming, alias, or re-export form is admitted" do
    for bad <- [
          %{event: :import_module, module: "Json", digest: "d1", names: :all},
          %{event: :import_module, module: "Json", digest: "d1", names: [], as: "J"},
          %{event: :import_module, module: "Json", digest: "d1", names: [], hiding: [:values]},
          %{event: :reexport, module: "Json", category: :values, spelling: "parse"}
        ] do
      assert {:error, %{id: "NSP001", details: %{reason: "invalid_event"}}} =
               Catena.build_namespace_environment([@json, bad])
    end

    assert {:ok, _} =
             Catena.build_namespace_environment([
               @json,
               %{event: :import_module, module: "Json", digest: "d1", names: []}
             ])
  end

  @tag obligations: ~w(IM-OBL-008)
  test "imports feed C021 precedence and NSP004 unchanged" do
    {:ok, env} =
      Catena.build_namespace_environment([
        @json,
        @option,
        %{event: :declare, category: :constructors, spelling: "Null"},
        %{event: :import_module, module: "Json", digest: "d1", names: [constructors: "Null"]},
        %{event: :import_module, module: "Option", digest: "d2", names: [constructors: "Null"]}
      ])

    assert {:ok, %Resolution{origin: nil, scope_depth: 0}} =
             Catena.resolve_name(env, %{category: :constructors, spelling: "Null"})

    {:ok, env2} =
      Catena.build_namespace_environment([
        @json,
        @option,
        %{event: :import_module, module: "Json", digest: "d1", names: [constructors: "Null"]},
        %{event: :import_module, module: "Option", digest: "d2", names: [constructors: "Null"]}
      ])

    assert {:error,
            %{
              id: "NSP004",
              details: %{reason: "ambiguous_import", origins: ["Json", "Option"]}
            }} =
             Catena.resolve_name(env2, %{category: :constructors, spelling: "Null"})

    {:ok, env3} =
      Catena.build_namespace_environment([
        @option,
        @json,
        %{event: :import_module, module: "Option", digest: "d2", names: [constructors: "Null"]},
        %{event: :import_module, module: "Json", digest: "d1", names: [constructors: "Null"]}
      ])

    assert {:error, %{id: "NSP004", details: %{origins: ["Json", "Option"]}}} =
             Catena.resolve_name(env3, %{category: :constructors, spelling: "Null"})
  end

  @tag obligations: ~w(IM-OBL-009 IM-OBL-010 IM-OBL-011)
  test "unused imports report deny-able warnings only, in stable order" do
    {:ok, env} =
      Catena.build_namespace_environment([
        @json,
        @option,
        %{
          event: :import_module,
          module: "Json",
          digest: "d1",
          names: [constructors: "Null", values: "parse"]
        },
        %{event: :import_module, module: "Option", digest: "d2", names: [constructors: "Null"]}
      ])

    assert {:ok, warnings} =
             Catena.Namespace.check_unused_imports(env, [
               %{category: :constructors, spelling: "Null"},
               %{category: :constructors, spelling: "Null"}
             ])

    assert [
             %ImportWarning{
               module: "Json",
               kind: :unused_name,
               category: :values,
               spelling: "parse"
             }
           ] = warnings

    assert {:ok, warnings2} =
             Catena.Namespace.check_unused_imports(env, [
               %{category: :constructors, spelling: "Null", qualified: "Json"},
               %{category: :values, spelling: "parse"}
             ])

    assert [
             %ImportWarning{
               module: "Json",
               kind: :unused_name,
               category: :constructors,
               spelling: "Null"
             },
             %ImportWarning{module: "Option", kind: :unused_name, spelling: "Null"},
             %ImportWarning{module: "Option", kind: :unused_module}
           ] = warnings2

    assert {:ok, []} =
             Catena.Namespace.check_unused_imports(env, [
               %{category: :constructors, spelling: "Null"},
               %{category: :values, spelling: "parse"},
               %{category: :constructors, spelling: "Null", qualified: "Option"}
             ])

    assert {:ok, _} = Catena.Namespace.check_unused_imports(env, [])
    assert {:ok, _} = Catena.check_unused_imports(env, [])
  end

  @tag obligations: ~w(IM-OBL-009 IM-OBL-012)
  test "diagnostics carry spelling, category, and module deterministically" do
    assert {:error,
            %{id: "IMP002", details: %{module: "Json", category: :values, spelling: "missing"}}} =
             Catena.build_namespace_environment([
               @json,
               %{event: :import_module, module: "Json", digest: "d1", names: [values: "missing"]}
             ])

    assert {:error, %{id: "IMP003", details: %{module: "Ghost"}}} =
             Catena.build_namespace_environment([
               %{event: :import_module, module: "Ghost", digest: "d", names: []}
             ])

    events = [
      @json,
      %{event: :import_module, module: "Json", digest: "d1", names: []},
      %{event: :declare, category: :values, spelling: "x"},
      %{event: :export, category: :values, spelling: "x"}
    ]

    first = Catena.build_namespace_environment(events)
    assert first == Catena.build_namespace_environment(events)

    {:ok, env} = first

    assert {:ok, r} =
             Catena.resolve_name(env, %{
               category: :values,
               spelling: "Json.parse",
               qualified: true
             })

    assert Catena.resolve_name(env, %{category: :values, spelling: "Json.parse", qualified: true}) ==
             {:ok, r}

    assert Catena.Namespace.check_unused_imports(env, []) ==
             Catena.Namespace.check_unused_imports(env, [])
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}
end
