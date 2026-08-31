defmodule Catena.C027EntryPointsTest do
  use ExUnit.Case, async: false

  alias Catena.{Entry, LanguageLifecycle, LanguageVersion, Package.Manifest}
  alias Catena.Effect.Row
  alias Catena.Package.Linker
  alias Catena.Type.Scheme

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38)

  @import_events [
    %{event: :import_module, module: "Other", digest: "dx", names: [values: "shared"]}
  ]

  @tag obligations: ~w(EN-OBL-001 EN-OBL-010)
  test "0.1.23 is an exact registered revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.38"
    assert LanguageVersion.source_text_frontend_versions() == @frontends
    refute "0.1.23" in LanguageVersion.compilable_revisions()
    refute "0.1.23" in LanguageVersion.artifact_versions()
    refute "0.1.23" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("entry-points", "0.1.23")

    change = Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-23-entry-points"))

    assert change["affects"] == ~w(static-meaning diagnostics)

    assert String.contains?(
             change["specification"],
             "entry-points/entry-declarations.md#"
           )

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, %{selection: %{language_revision: "0.1.38"}}} = Catena.decode_source_text("")
    assert {:ok, _} = Catena.build_namespace_environment([])
    assert {:ok, _} = Catena.compile_scc([])
    assert {:ok, _} = Catena.Package.Deps.parse_version("1.0.0")

    assert {:error, %{id: "EDN001", details: %{required: "0.1.22"}}} =
             Catena.build_namespace_environment(@import_events,
               language_selection: %{
                 edition: "0.1",
                 language_revision: "0.1.17",
                 previews: []
               }
             )

    refute function_exported?(Catena, :default_entry, 0)
    refute function_exported?(Catena, :run_package, 1)
    refute function_exported?(Catena.Entry, :spawn_entry, 2)
  end

  @tag obligations: ~w(EN-OBL-002 EN-OBL-004)
  test "the entries field decodes with absent/null/empty library equivalence" do
    manifest =
      package_manifest("C027App")
      |> Map.merge(selection("0.1.7"))
      |> Map.put("entries", [
        %{"name" => "main", "result" => "integer", "launch" => true},
        %{"name" => "other", "result" => "boolean"}
      ])

    assert {:ok, decoded} = Manifest.decode(JSON.encode!(manifest))

    assert decoded.entries == [
             %{name: "main", result: "integer", launch: true},
             %{name: "other", result: "boolean", launch: false}
           ]

    assert Entry.library?(decoded.entries) == false

    for stripped <- [
          Map.delete(manifest, "entries"),
          Map.put(manifest, "entries", nil),
          Map.put(manifest, "entries", [])
        ] do
      assert {:ok, library} = Manifest.decode(JSON.encode!(stripped))
      assert library.entries == []
      assert Entry.library?(library.entries)
    end
  end

  @tag obligations: ~w(EN-OBL-003)
  test "every malformed entry declaration rejects as ENT001 with the offending shape" do
    for bad <- ["oops", 42, %{}] do
      assert {:error, %{id: "ENT001"}} =
               Manifest.decode(
                 JSON.encode!(package_manifest("C027App") |> Map.put("entries", bad))
               )
    end

    for bad <- [
          %{"result" => "integer"},
          %{"name" => "main"},
          %{"name" => "Main", "result" => "integer"},
          %{"name" => "main", "result" => 7},
          %{"name" => "main", "result" => "integer", "launch" => false},
          %{"name" => "main", "result" => "integer", "launch" => "yes"}
        ] do
      assert {:error, %{id: "ENT001", details: %{reason: reason}}} =
               Manifest.decode(
                 JSON.encode!(package_manifest("C027App") |> Map.put("entries", [bad]))
               )

      assert reason in [
               "each entry must be a name and result object",
               "entry names must be value names and launch must be true"
             ]
    end

    duplicate = [
      %{"name" => "main", "result" => "integer"},
      %{"name" => "main", "result" => "boolean"}
    ]

    assert {:error, %{id: "ENT001", details: %{reason: "entry names must be unique"}}} =
             Manifest.decode(
               JSON.encode!(package_manifest("C027App") |> Map.put("entries", duplicate))
             )

    markers = [
      %{"name" => "main", "result" => "integer", "launch" => true},
      %{"name" => "other", "result" => "boolean", "launch" => true}
    ]

    assert {:error,
            %{id: "ENT001", details: %{reason: "at most one entry may carry the launch marker"}}} =
             Manifest.decode(
               JSON.encode!(package_manifest("C027App") |> Map.put("entries", markers))
             )

    closed = fn name, type, parameters ->
      %{
        name: name,
        parameters: parameters,
        scheme: Scheme.mono(type),
        effect_row: %Row{entries: [], tail: nil},
        verified_uses_row: %Row{entries: [], tail: nil}
      }
    end

    open_row = %Row{entries: [%{family: "Ask"}], tail: nil}

    for {entries, modules, reason} <- [
          {[%{name: "ghost", result: "integer", launch: false}],
           [%{beam_module: :C027One, core: core(["main"], [closed.("main", :integer, [])])}],
           "unknown_export"},
          {[%{name: "main", result: "integer", launch: false}],
           [
             %{beam_module: :C027One, core: core(["main"], [closed.("main", :integer, [])])},
             %{beam_module: :C027Two, core: core(["main"], [closed.("main", :integer, [])])}
           ], "ambiguous_export"},
          {[%{name: "add", result: "integer", launch: false}],
           [
             %{
               beam_module: :C027One,
               core:
                 core(["add"], [
                   closed.("add", {:function, :integer, :integer}, ["left", "right"])
                 ])
             }
           ], "non_zero_arity"},
          {[%{name: "ask", result: "integer", launch: false}],
           [
             %{
               beam_module: :C027One,
               core: core(["ask"], [%{closed.("ask", :integer, []) | effect_row: open_row}])
             }
           ], "not_effect_closed"},
          {[%{name: "main", result: "boolean", launch: false}],
           [%{beam_module: :C027One, core: core(["main"], [closed.("main", :integer, [])])}],
           "result_mismatch"}
        ] do
      assert {:error, %{id: "ENT001", details: %{reason: ^reason}}} =
               Entry.validate(entries, modules)
    end
  end

  @tag obligations: ~w(EN-OBL-005)
  test "at most one launch marker and any declared entry launches by name" do
    directory = temporary_directory!("multi")

    write_package(directory, "C027Multi",
      entries: [
        %{"name" => "main", "result" => "integer"},
        %{"name" => "other", "result" => "boolean"}
      ]
    )

    assert {:ok, package} = Linker.compile_manifest(Path.join(directory, "package.json"))

    assert package.entries == [
             %{name: "main", result: "integer", launch: false, module: :C027Multi},
             %{name: "other", result: "boolean", launch: false, module: :C027Multi}
           ]

    assert {:ok, %{status: :completed, value: 8}} = Entry.launch(package, "main")
    assert {:ok, %{status: :completed, value: true}} = Entry.launch(package, "other")
  end

  @tag obligations: ~w(EN-OBL-006 EN-OBL-007)
  test "launch invokes the entry to completion and the value is the shutdown result" do
    directory = temporary_directory!("launch")

    write_package(directory, "C027App",
      entries: [%{"name" => "main", "result" => "integer", "launch" => true}]
    )

    assert {:ok, package} = Linker.compile_manifest(Path.join(directory, "package.json"))
    assert [%{name: "main", launch: true, module: :C027App}] = package.entries

    assert {:ok, %{status: :completed, value: 8}} = Entry.launch(package, "main")
    assert {:ok, %{status: :completed, value: 8}} = Entry.launch(package, "main")

    {boom_module, boom_binary} = boom_binary()

    package = %{
      entries: [%{name: "boom", result: "integer", launch: false, module: boom_module}],
      entry_modules: %{"boom" => %{module: boom_module, binary: boom_binary}}
    }

    assert {:error, %{id: "ENT003", details: %{entry: "boom", trap: "ArgumentError"}}} =
             Entry.launch(package, "boom")
  end

  @tag obligations: ~w(EN-OBL-008)
  test "a launch naming an undeclared entry rejects as ENT002" do
    directory = temporary_directory!("unknown")
    write_package(directory, "C027Unknown", entries: [%{"name" => "main", "result" => "integer"}])

    assert {:ok, package} = Linker.compile_manifest(Path.join(directory, "package.json"))

    assert {:error, %{id: "ENT002", details: %{requested: "ghost", declared: ["main"]}}} =
             Entry.launch(package, "ghost")

    assert {:error, %{id: "ENT002"}} = Entry.launch(package, "")
  end

  @tag obligations: ~w(EN-OBL-009)
  test "diagnostics stay stable with reused family identities unchanged" do
    assert {:error, %{id: "PKG001"}} =
             Manifest.decode(
               JSON.encode!(package_manifest("C027App") |> Map.put("dependencies", "oops"))
             )

    assert {:error, %{id: "EDN001"}} =
             Manifest.decode(
               JSON.encode!(
                 package_manifest("C027App")
                 |> Map.merge(selection("0.1.7"))
                 |> Map.put("language_revision", "9.9.9")
               )
             )

    directory = temporary_directory!("unknown-export")

    write_package(directory, "C027Ghost", entries: [%{"name" => "ghost", "result" => "integer"}])

    assert {:error, %{id: "ENT001", details: %{reason: "unknown_export"}}} =
             Linker.compile_manifest(Path.join(directory, "package.json"))
  end

  @tag obligations: ~w(EN-OBL-010)
  test "entries wiring stays deterministic with compilation roots unchanged" do
    directory = temporary_directory!("determinism")

    write_package(directory, "C027Stable", entries: [])
    assert {:ok, library} = Linker.compile_manifest(Path.join(directory, "package.json"))
    library_beam = File.read!(library.output)

    write_package(directory, "C027Stable",
      entries: [%{"name" => "main", "result" => "integer", "launch" => true}]
    )

    assert {:ok, executable} = Linker.compile_manifest(Path.join(directory, "package.json"))
    executable_again = File.read!(executable.output)

    assert library_beam == executable_again
    assert executable_again == File.read!(executable.output)
    assert library.entries == []
    assert library.entry_modules == %{}
    assert Map.keys(executable.entry_modules) == ["main"]
    assert {:ok, %{status: :completed, value: 8}} = Entry.launch(executable, "main")
  end

  defp selection(revision) do
    %{"edition" => "0.1", "language_revision" => revision, "previews" => []}
  end

  defp core(exports, definitions), do: %{exports: exports, definitions: definitions}

  defp package_manifest(module) do
    %{
      "format" => "catena-package-manifest",
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "package" => "c027",
      "profile" => "static",
      "companion_module" => "C027Companion",
      "modules" => [
        %{
          "source" => "module.json",
          "beam" => module <> ".beam",
          "interface" => "module.cati.json"
        }
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "C027Companion.beam",
      "assurance" => "assurance.json"
    }
  end

  defp module_document(name) do
    %{
      "version" => "0.1.7",
      "origin" => "pkg://tests/#{name}",
      "module" => name,
      "source" => "module.json",
      "exports" => ["main", "other"],
      "type_exports" => [],
      "type_groups" => [],
      "imports" => [],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "integer", "value" => 8}
        },
        %{
          "name" => "other",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "boolean"}},
          "body" => %{"tag" => "boolean", "value" => true}
        }
      ],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "effects" => [],
      "handlers" => [],
      "specifications" => []
    }
  end

  defp write_package(directory, module, entries: entries) do
    File.write!(Path.join(directory, "module.json"), JSON.encode!(module_document(module)))

    File.write!(
      Path.join(directory, "package.json"),
      JSON.encode!(package_manifest(module) |> Map.put("entries", entries))
    )
  end

  defp boom_binary do
    Code.compile_string(
      ~s{defmodule C027Boom do\n  def boom, do: raise(ArgumentError, "entry trap")\nend},
      "c027_boom.exs"
    )
    |> List.first()
  end

  defp temporary_directory!(suffix) do
    path =
      Path.join(System.tmp_dir!(), "catena-c027-#{suffix}-#{System.unique_integer([:positive])}")

    File.mkdir_p!(path)
    on_exit(fn -> File.rm_rf!(path) end)
    path
  end
end
