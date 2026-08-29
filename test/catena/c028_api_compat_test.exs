defmodule Catena.C028ApiCompatTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Package.Compat, Package.Deps}
  alias Catena.Type.Scheme

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36)

  @import_events [
    %{event: :import_module, module: "Other", digest: "dx", names: [values: "shared"]}
  ]

  describe "revision registration" do
    @tag obligations: ~w(CP-OBL-001 CP-OBL-003)
    test "0.1.24 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.36"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.24" in LanguageVersion.compilable_revisions()
      refute "0.1.24" in LanguageVersion.artifact_versions()
      refute "0.1.24" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("api-and-abi-compatibility", "0.1.24")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-24-api-and-abi-compatibility")
        )

      assert change["affects"] == ~w(static-meaning diagnostics)

      assert String.contains?(
               change["specification"],
               "api-and-abi-compatibility/compatibility-layers-and-versions.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.36"}}} = Catena.decode_source_text("")
      assert {:ok, _} = Catena.build_namespace_environment([])
      assert {:ok, _} = Catena.compile_scc([])

      assert {:error, %{id: "EDN001", details: %{required: "0.1.22"}}} =
               Catena.build_namespace_environment(@import_events,
                 language_selection: %{
                   edition: "0.1",
                   language_revision: "0.1.17",
                   previews: []
                 }
               )

      refute function_exported?(Catena.Package.Compat, :diff_source, 2)
      refute function_exported?(Catena.Package.Compat, :abi_check, 2)
      refute function_exported?(Catena, :migrate_source, 1)
    end
  end

  describe "matrix classification" do
    @tag obligations: ~w(CP-OBL-004)
    test "classifies export removals, renames, scheme changes, and additions" do
      old = interface(values: [value("main", :integer), value("helper", :integer)])
      renamed = interface(values: [value("main", :integer), value("renamed", :integer)])
      assert {:ok, %{class: :breaking, changes: changes}} = Compat.diff(old, renamed)
      assert {1, :breaking} in Enum.map(changes, &{&1.row, &1.kind})
      assert {6, :minor} in Enum.map(changes, &{&1.row, &1.kind})

      retyped =
        interface(
          values: [value("main", {:function, :integer, :integer}), value("helper", :integer)]
        )

      assert {:ok, %{class: :breaking, changes: [change]}} = Compat.diff(old, retyped)
      assert change.row == 3

      added =
        interface(
          values: [value("main", :integer), value("helper", :integer), value("more", :boolean)]
        )

      assert {:ok, %{class: :minor, changes: [change]}} = Compat.diff(old, added)
      assert change.row == 6

      assert {:ok, %{class: :identical, changes: []}} = Compat.diff(old, old)
    end

    @tag obligations: ~w(CP-OBL-004)
    test "widened effect rows break and narrowed rows are minor" do
      closed = fn name -> value(name, :integer, []) end
      wide = fn name -> value(name, :integer, [entry("Ask", "ask")]) end

      assert {:ok, %{class: :breaking, changes: [change]}} =
               Compat.diff(
                 interface(values: [closed.("main")]),
                 interface(values: [wide.("main")])
               )

      assert {change.row, change.kind} == {4, :breaking}

      assert {:ok, %{class: :minor, changes: [change]}} =
               Compat.diff(
                 interface(values: [wide.("main")]),
                 interface(values: [closed.("main")])
               )

      assert {change.row, change.kind} == {5, :minor}
    end

    @tag obligations: ~w(CP-OBL-004)
    test "classifies datatype, trait, instance, and handler changes" do
      old =
        interface(
          types: [type("test://p::M::Box")],
          traits: [%{"id" => "test://p::M::Eq"}],
          instances: [%{"id" => "test://p::M::Eq-Int"}],
          handlers: [handler("test://p::M::H")]
        )

      removed_type =
        interface(types: [], traits: [%{"id" => "test://p::M::Eq"}], instances: [], handlers: [])

      assert {:ok, %{class: :breaking, changes: changes}} = Compat.diff(old, removed_type)
      assert {7, :breaking} in Enum.map(changes, &{&1.row, &1.kind})

      added_trait =
        interface(
          types: [type("test://p::M::Box")],
          traits: [%{"id" => "test://p::M::Eq"}, %{"id" => "test://p::M::Ord"}],
          instances: [%{"id" => "test://p::M::Eq-Int"}],
          handlers: [handler("test://p::M::H")]
        )

      assert {:ok, %{class: :minor, changes: changes}} = Compat.diff(old, added_trait)
      assert {11, :minor} in Enum.map(changes, &{&1.row, &1.kind})

      changed_trait =
        interface(
          types: [type("test://p::M::Box")],
          traits: [%{"id" => "test://p::M::Eq", "operations" => []}],
          instances: [%{"id" => "test://p::M::Eq-Int"}],
          handlers: [handler("test://p::M::H")]
        )

      assert {:ok, %{class: :breaking}} = Compat.diff(old, changed_trait)
    end

    @tag obligations: ~w(CP-OBL-007)
    test "entry additions are minor and removals or result changes break" do
      entries = fn list -> Enum.map(list, &%{name: &1, result: "integer", launch: false}) end

      assert {:ok, %{class: :minor}} =
               Compat.diff_entries(entries.(["main"]), entries.(["main", "self_check"]))

      assert {:ok, %{class: :breaking}} =
               Compat.diff_entries(entries.(["main", "self_check"]), entries.(["main"]))

      retyped = [%{name: "main", result: "boolean", launch: false}]
      assert {:ok, %{class: :breaking}} = Compat.diff_entries(entries.(["main"]), retyped)

      moved_a = [
        %{name: "main", result: "integer", launch: true},
        %{name: "other", result: "integer", launch: false}
      ]

      moved_b = [
        %{name: "main", result: "integer", launch: false},
        %{name: "other", result: "integer", launch: true}
      ]

      assert {:ok, %{class: :minor, changes: [change]}} = Compat.diff_entries(moved_a, moved_b)
      assert change.row == 15
    end
  end

  describe "claim validation" do
    @tag obligations: ~w(CP-OBL-005)
    test "enforces the 1.0+ major rule and the 0.x Cargo rule" do
      old = interface(values: [value("main", :integer)])
      removed = interface(values: [])
      added = interface(values: [value("main", :integer), value("more", :boolean)])

      assert {:ok, %{claim: :breaking}} =
               Compat.validate_claim(removed, old, {"2.0.0", "3.0.0"})

      assert {:error, %{id: "CMP001", details: %{required: :breaking}}} =
               Compat.validate_claim(old, removed, {"2.0.0", "2.1.0"})

      assert {:ok, %{claim: :minor}} =
               Compat.validate_claim(old, added, {"2.0.0", "2.1.0"})

      assert {:error, %{id: "CMP001", details: %{required: :minor}}} =
               Compat.validate_claim(old, added, {"2.0.0", "2.0.1"})

      assert {:ok, %{claim: :breaking}} =
               Compat.validate_claim(old, removed, {"0.1.0", "0.2.0"})

      assert {:error, %{id: "CMP001", details: %{required: :breaking}}} =
               Compat.validate_claim(old, removed, {"0.1.0", "0.1.1"})

      assert {:ok, %{claim: :minor}} =
               Compat.validate_claim(old, added, {"0.1.0", "0.1.1"})

      assert {:ok, _} = Compat.validate_claim(removed, old, {"0.1.0", "1.0.0"})
    end

    @tag obligations: ~w(CP-OBL-005 CP-OBL-007)
    test "entry removals under-claim with the interface diff" do
      entries = fn list -> Enum.map(list, &%{name: &1, result: "integer", launch: false}) end
      same = interface(values: [value("main", :integer)])

      assert {:error, %{id: "CMP001", details: %{required: :breaking}}} =
               Compat.validate_claim(same, same, {"0.1.0", "0.1.1"},
                 entries: {entries.(["main", "self_check"]), entries.(["main"])}
               )
    end

    @tag obligations: ~w(CP-OBL-006)
    test "malformed input is CMP002 and unclassifiable drift is CMP003" do
      assert {:error, %{id: "CMP002"}} = Compat.diff("not json", interface(values: []))
      assert {:error, %{id: "CMP002"}} = Compat.diff(interface(values: []), 42)
      assert {:error, %{id: "CMP002"}} = Compat.diff(%{}, interface(values: []))

      assert {:error, %{id: "CMP002"}} =
               Compat.validate_claim(
                 interface(values: []),
                 interface(values: []),
                 {"oops", "1.0.0"}
               )

      assert {:error, %{id: "CMP002"}} =
               Compat.validate_claim(
                 interface(values: []),
                 interface(values: []),
                 {"1.0.0", "1.0.0"}
               )

      assert {:error, %{id: "CMP002"}} =
               Compat.validate_claim(
                 interface(values: []),
                 interface(values: []),
                 {"1.1.0", "1.0.0"}
               )

      moved = interface(origin: "pkg://tests/Elsewhere", module: "Elsewhere")

      assert {:error, %{id: "CMP003", details: details}} =
               Compat.diff(interface(values: []), moved)

      assert details.old != details.new
    end
  end

  describe "absences and determinism" do
    @tag obligations: ~w(CP-OBL-002 CP-OBL-008 CP-OBL-010)
    test "representation and metadata changes never break and results are deterministic" do
      base = interface(values: [value("main", :integer)])

      assert {:ok, %{class: :identical}} =
               Compat.diff(base, interface(values: [value("main", :integer)], digest: "other"))

      assert {:ok, first} =
               Compat.diff(
                 base,
                 interface(values: [value("main", :integer), value("more", :boolean)])
               )

      assert {:ok, ^first} =
               Compat.diff(
                 base,
                 interface(values: [value("main", :integer), value("more", :boolean)])
               )

      assert {:ok, %{class: :identical, changes: []}} =
               Compat.diff(
                 interface(types: [type("test://p::M::Box")]),
                 interface(types: [type("test://p::M::Box")])
               )
    end

    @tag obligations: ~w(CP-OBL-009)
    test "diagnostics stay stable with reused family identities unchanged" do
      assert {:error, %{id: "PKG001"}} = Deps.parse_version("not-a-version")
      assert {:ok, _} = Deps.parse_version("1.2.3")

      assert {:error, %{id: "CMP001"}} =
               Compat.validate_claim(
                 interface(values: [value("main", :integer)]),
                 interface(values: []),
                 {"0.1.0", "0.1.1"}
               )
    end

    @tag obligations: ~w(CP-OBL-001 CP-OBL-004 CP-OBL-010)
    test "real compiled interfaces round-trip; behavior changes stay unclassified" do
      v1 = compiled_interface("C028Lib", 8)
      v2 = compiled_interface("C028Lib", 9)

      assert {:ok, %{class: :identical, changes: []}} = Compat.diff(v1, v2)

      assert {:ok, %{class: :identical}} =
               Compat.validate_claim(v1, v2, {"0.1.0", "0.1.1"})

      assert {:error, %{id: "CMP003"}} = Compat.diff(v1, compiled_interface("C028Other", 8))
    end
  end

  defp value(name, type, row_entries \\ []) do
    uses = %Catena.Effect.Row{entries: row_entries, tail: nil}

    %{
      name: name,
      scheme: Scheme.mono(type),
      uses: uses,
      condition: nil
    }
  end

  defp entry(family, name) do
    %{
      family: "test://p::M::#{family}",
      family_name: family,
      capability: :once,
      name: name,
      arguments: [],
      abstract?: false
    }
  end

  defp type(id) do
    %{
      id: id,
      origin: "pkg://tests/M",
      module: "M",
      name: String.split(id, "::") |> List.last(),
      arity: 0,
      visibility: :transparent,
      inhabitation: :nonempty,
      variance: [],
      positive?: false,
      regular?: true,
      constructors: []
    }
  end

  defp handler(id) do
    %{
      id: id,
      origin: "pkg://tests/M",
      module: "M",
      name: "H",
      family: id,
      family_name: "H",
      arguments: [],
      variables: [],
      uses_row: %Catena.Effect.Row{entries: [], tail: nil}
    }
  end

  defp interface(options) do
    %{
      version: "0.1.8",
      origin: Keyword.get(options, :origin, "pkg://tests/M"),
      module: Keyword.get(options, :module, "M"),
      digest: Keyword.get(options, :digest, "d"),
      values: Keyword.get(options, :values, []),
      types: Keyword.get(options, :types, []),
      traits: Keyword.get(options, :traits, []),
      instances: Keyword.get(options, :instances, []),
      templates: Keyword.get(options, :templates, []),
      standard_digest: "sd",
      effects: Keyword.get(options, :effects, []),
      handlers: Keyword.get(options, :handlers, []),
      claims: [],
      specification_digest: nil
    }
  end

  defp compiled_interface(module, value) do
    json =
      JSON.encode!(%{
        "version" => "0.1.1",
        "module" => module,
        "source" => "c028.json",
        "exports" => ["main"],
        "definitions" => [
          %{
            "name" => "main",
            "parameters" => [],
            "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
            "body" => %{"tag" => "integer", "value" => value}
          }
        ]
      })

    {:ok, _module, _binary, metadata} = Catena.compile_json(json)
    {:ok, decoded} = Catena.Interface.decode(metadata.interface_binary)
    decoded
  end
end
