defmodule Catena.C082TopLevelTest do
  use ExUnit.Case, async: false

  alias Catena.{Entry, LanguageLifecycle, LanguageVersion, Package.Manifest, Type.Scheme}
  alias Catena.Effect.Row
  alias Catena.Package.Linker

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47 0.1.48)

  describe "revision registration" do
    @tag obligations: ~w(TL-OBL-001)
    test "0.1.48 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.48"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.48" in LanguageVersion.compilable_revisions()
      refute "0.1.48" in LanguageVersion.artifact_versions()
      refute "0.1.48" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("top-level-effects", "0.1.48")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-48-top-level-effects")
        )

      assert change["affects"] == ~w(static-meaning)
      assert change["summary"] =~ "top level effects"

      assert String.contains?(
               change["specification"],
               "top-level-effects/the-top-level-boundary.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.48"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :host_handler, 0)
      refute function_exported?(Catena, :ambient_interpreter, 0)
      refute function_exported?(Entry, :launch_with_capabilities, 2)
    end
  end

  describe "the top-level boundary" do
    @tag obligations: ~w(TL-OBL-002 TL-OBL-003 TL-OBL-007)
    test "an effect-closed entry launches to completion, repeatedly" do
      directory = temporary_directory!("c082-launch")

      write_package(directory, "C082App",
        entries: [%{"name" => "main", "result" => "integer", "launch" => true}]
      )

      assert {:ok, package} = Linker.compile_manifest(Path.join(directory, "package.json"))
      assert [%{name: "main", launch: true, module: :C082App}] = package.entries

      assert {:ok, %{status: :completed, value: 8}} = Entry.launch(package, "main")
      assert {:ok, %{status: :completed, value: 8}} = Entry.launch(package, "main")
    end

    @tag obligations: ~w(TL-OBL-002)
    test "a non-effect-closed export rejects as ENT001: nothing is left unhandled" do
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

      entries = [%{name: "ask", result: "integer", launch: false}]

      modules = [
        %{
          beam_module: :C082Open,
          core: core(["ask"], [%{closed.("ask", :integer, []) | effect_row: open_row}])
        }
      ]

      assert {:error, %{id: "ENT001", details: %{reason: "not_effect_closed"}}} =
               Entry.validate(entries, modules)
    end

    @tag obligations: ~w(TL-OBL-004 TL-OBL-005)
    test "no ambient channel exists: capabilities are explicit or absent" do
      assert {:module, _} = Code.ensure_loaded(Catena.Entry)
      refute function_exported?(Entry, :with_host, 1)
      refute function_exported?(Entry, :inject, 2)
      refute function_exported?(Catena, :host_capability, 0)
      refute function_exported?(Catena, :interpret_top_level, 1)
    end

    @tag obligations: ~w(TL-OBL-006)
    test "the door is the lifecycle record: widening amends C027 explicitly" do
      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-48-top-level-effects")
        )

      assert is_map(change)
      assert change["affects"] == ~w(static-meaning)
    end

    @tag obligations: ~w(TL-OBL-002 TL-OBL-007)
    test "determinism: manifest decoding and validation repeat" do
      manifest =
        package_manifest("C082Repeat")
        |> Map.put("entries", [%{"name" => "main", "result" => "integer", "launch" => true}])

      assert {:ok, first} = Manifest.decode(JSON.encode!(manifest))
      assert {:ok, second} = Manifest.decode(JSON.encode!(manifest))
      assert first.entries == second.entries
    end
  end

  defp core(exports, definitions), do: %{exports: exports, definitions: definitions}

  defp write_package(directory, module, entries: entries) do
    File.write!(Path.join(directory, "module.json"), JSON.encode!(module_document(module)))

    File.write!(
      Path.join(directory, "package.json"),
      JSON.encode!(package_manifest(module) |> Map.put("entries", entries))
    )
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
      "handlers" => []
    }
  end

  defp package_manifest(module) do
    %{
      "format" => "catena-package-manifest",
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "package" => "c082",
      "profile" => "static",
      "companion_module" => "C082Companion",
      "modules" => [
        %{
          "source" => "module.json",
          "beam" => module <> ".beam",
          "interface" => "module.cati.json"
        }
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "C082Companion.beam",
      "assurance" => "assurance.json"
    }
  end

  defp temporary_directory!(suffix) do
    path =
      Path.join(System.tmp_dir!(), "catena-c082-#{suffix}-#{System.unique_integer([:positive])}")

    File.mkdir_p!(path)
    path
  end
end
