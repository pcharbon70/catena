defmodule Catena.C008EditionsLifecycleTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Catena.Governance.{Crypto, Policy, Reference, TrustRoot}
  alias Catena.Package.{Linker, Manifest}

  alias Catena.{
    Assurance,
    CanonicalJCS,
    Interface,
    LanguageInfo,
    LanguageLifecycle,
    LanguageSelection,
    LanguageVersion
  }

  @tag obligations: ~w(ED-OBL-009 ED-OBL-011 ED-OBL-012 ED-OBL-029)
  test "the language registry exposes exact retained selections and a closed lifecycle" do
    info = LanguageInfo.document()

    assert info["format"] == "catena-language-info"
    assert info["version"] == "0.1.7"

    assert info["current"] == %{
             "edition" => "0.1",
             "language_revision" => "0.1.36",
             "previews" => []
           }

    assert [%{"id" => "0.1", "revisions" => revisions}] = info["editions"]
    assert revisions == LanguageVersion.all()
    assert LanguageLifecycle.valid_registry?(info["features"])
    assert LanguageLifecycle.valid_changes?(info["changes"])

    assert Enum.map(info["features"], & &1["id"]) ==
             Enum.uniq(Enum.map(info["features"], & &1["id"]))

    assert LanguageLifecycle.valid_transition?("preview", "stable")
    assert LanguageLifecycle.valid_transition?("preview", "withdrawn")
    assert LanguageLifecycle.valid_transition?("stable", "deprecated")
    assert LanguageLifecycle.valid_transition?("deprecated", "removed")
    refute LanguageLifecycle.valid_transition?("stable", "removed")
    refute LanguageLifecycle.valid_transition?("withdrawn", "preview")

    assert :unavailable == LanguageLifecycle.state("effects-and-handlers", "0.1.4")
    assert {:ok, :stable} == LanguageLifecycle.state("effects-and-handlers", "0.1.5")
    assert :unknown == LanguageLifecycle.state("never-reuse-this", "0.1.7")
    assert LanguageLifecycle.preview_ids() == []
    assert Enum.all?(info["features"], &is_binary(&1["change"]))

    assert Enum.all?(info["changes"], &is_binary(&1["from"]))
    assert hd(info["changes"])["from"] == "0.1.0"

    edition_change = Enum.find(info["changes"], &(&1["to"] == "0.1.7"))

    assert edition_change["fixes"] |> Enum.map(& &1["path"]) ==
             ["$.version", "$.edition", "$.language_revision", "$.previews"]

    assert List.last(info["changes"])["to"] == "0.1.36"
    assert List.last(info["changes"])["fixes"] == []

    assert Enum.all?(info["changes"], fn change ->
             String.starts_with?(change["id"], "change-") and
               String.contains?(change["specification"], "#")
           end)

    [first | rest] = info["features"]
    refute LanguageLifecycle.valid_registry?([first, first | rest])

    invalid_history =
      first
      |> Map.put("state", "removed")
      |> Map.put("removed", "0.1.7")
      |> Map.put("history", [
        %{"revision" => "0.1.1", "state" => "stable"},
        %{"revision" => "0.1.7", "state" => "removed"}
      ])

    refute LanguageLifecycle.valid_registry?([invalid_history | rest])

    emergency_boundary = %{
      "revision" => "0.1.7",
      "state" => "removed",
      "emergency" => %{
        "basis" => "soundness",
        "affected_rules" => [first["specification"]],
        "reason" => "modelled unsoundness",
        "exposure" => "all uses",
        "replacement_or_containment" => "disable the feature",
        "migration" => "select an earlier retained revision"
      }
    }

    assert LanguageLifecycle.valid_emergency_transition?(hd(first["history"]), emergency_boundary)

    emergency_entry =
      first
      |> Map.put("state", "removed")
      |> Map.put("removed", "0.1.7")
      |> Map.put("history", [hd(first["history"]), emergency_boundary])

    assert LanguageLifecycle.valid_registry?([emergency_entry | rest])
  end

  @tag obligations: ~w(ED-OBL-001 ED-OBL-003 ED-OBL-008 ED-OBL-013 ED-OBL-014)
  test "selection validation rejects aliases, mismatches, duplicate previews, and unknown pins" do
    assert {:ok, %LanguageSelection{}} =
             LanguageVersion.resolve_selection(selection("0.1.7"))

    for value <- ["0.1", "0.1.7-preview", "0.1.07", "latest", "0.1.37"] do
      assert {:error, %{id: "EDN001", path: "$.language_revision"}} =
               LanguageVersion.resolve_selection(selection(value))
    end

    assert {:error, %{id: "EDN001", path: "$.edition"}} =
             LanguageVersion.resolve_selection(%{
               "edition" => "0.2",
               "language_revision" => "0.1.7",
               "previews" => []
             })

    assert {:error, %{id: "PRV001"}} =
             LanguageVersion.resolve_selection(%{
               "edition" => "0.1",
               "language_revision" => "0.1.7",
               "previews" => ["future-feature", "future-feature"]
             })

    assert {:error, %{id: "PRV001"}} =
             LanguageVersion.resolve_selection(%{
               "edition" => "0.1",
               "language_revision" => "0.1.7",
               "previews" => ["future-feature"]
             })
  end

  @tag obligations: ~w(ED-OBL-009 ED-OBL-021 ED-OBL-029)
  test "every compilation-capable revision compiles through the 0.1.7 artifact schema" do
    for revision <- LanguageVersion.compilable_revisions() do
      assert {:ok, _module, _beam, metadata} =
               module_document("C008Retained#{String.replace(revision, ".", "")}", "0.1.7")
               |> JSON.encode!()
               |> Catena.compile_json(language_selection: selection(revision))

      assert metadata.selection.language_revision == revision
      assert metadata.artifact_version == "0.1.7"
      assert {:ok, interface} = Interface.decode(metadata.interface_binary)
      assert interface.version == "0.1.7"
      assert interface.language_revision == revision
    end
  end

  @tag obligations: ~w(ED-OBL-005)
  test "a module-level selection cannot contradict its package selection" do
    source =
      module_document("C008SelectionMismatch", "0.1.7")
      |> Map.merge(selection("0.1.6"))

    assert {:error, %{id: "EDN001", details: %{module: module, selected: selected}}} =
             source
             |> JSON.encode!()
             |> Catena.check_json(language_selection: selection("0.1.7"))

    assert module["language_revision"] == "0.1.6"
    assert selected["language_revision"] == "0.1.7"
  end

  @tag obligations: ~w(ED-OBL-006 ED-OBL-007 ED-OBL-027)
  test "standalone compilation reports current selection and legacy inference without byte changes" do
    current = module_document("C008Standalone", "0.1.7")
    assert {:ok, current_core} = current |> JSON.encode!() |> Catena.check_json()
    assert current_core.edition == "0.1"
    assert current_core.language_revision == "0.1.7"
    assert current_core.previews == []
    assert current_core.diagnostics == []

    legacy = module_document("C008Legacy", "0.1.6")
    assert {:ok, legacy_core} = legacy |> JSON.encode!() |> Catena.check_json()
    assert legacy_core.language_revision == "0.1.6"
    assert [%{id: "EDN002", severity: :warning, fixes: fixes}] = legacy_core.diagnostics
    assert Enum.map(fixes, & &1["path"]) == ["$.edition", "$.language_revision", "$.previews"]

    explicit =
      Map.merge(legacy, %{
        "edition" => "0.1",
        "language_revision" => "0.1.6",
        "previews" => []
      })

    assert {:ok, explicit_core} = explicit |> JSON.encode!() |> Catena.check_json()
    assert explicit_core.diagnostics == []

    assert {:ok, :C008Legacy, inferred_beam, inferred_metadata} =
             legacy |> JSON.encode!() |> Catena.compile_json()

    assert {:ok, :C008Legacy, explicit_beam, explicit_metadata} =
             explicit |> JSON.encode!() |> Catena.compile_json()

    assert inferred_beam == explicit_beam
    assert inferred_metadata.interface_binary == explicit_metadata.interface_binary

    assert {:error, %{id: "EDN002", severity: :error}} =
             legacy
             |> JSON.encode!()
             |> Catena.check_json(denied_diagnostics: ["EDN002"])

    assert {:error, %{id: "EDN001", path: "$.diagnostics.deny"}} =
             current
             |> JSON.encode!()
             |> Catena.check_json(denied_diagnostics: ["TYPO"])
  end

  @tag obligations: ~w(ED-OBL-004 ED-OBL-009 ED-OBL-010)
  test "an explicit older pin rejects newer constructs but accepts neutral newer transport" do
    neutral = module_document("C008Pinned", "0.1.7")

    assert {:ok, core} =
             neutral
             |> JSON.encode!()
             |> Catena.check_json(language_selection: selection("0.1.1"))

    assert core.language_revision == "0.1.1"

    newer_construct =
      neutral
      |> Map.put("effects", [%{"name" => "Audit", "parameters" => [], "operations" => []}])

    assert {:error, %{id: "EDN001", details: %{introduced: "0.1.5", selected: "0.1.4"}}} =
             newer_construct
             |> JSON.encode!()
             |> Catena.check_json(language_selection: selection("0.1.4"))
  end

  @tag obligations: ~w(ED-OBL-004 ED-OBL-008)
  test "0.1.2 matching is not mistaken for 0.1.3 clause conditions" do
    fixture =
      Path.expand("../fixtures/c002-option.catena.json", __DIR__)
      |> File.read!()
      |> JSON.decode!()
      |> Map.put("version", "0.1.7")

    assert {:ok, _core} =
             fixture
             |> JSON.encode!()
             |> Catena.check_json(language_selection: selection("0.1.2"))

    guarded =
      put_in(fixture, ["definitions", Access.at(1), "body", "clauses", Access.at(0), "guard"], %{
        "tag" => "boolean",
        "value" => true
      })

    assert {:error, %{id: "EDN001", details: %{introduced: "0.1.3"}}} =
             guarded
             |> JSON.encode!()
             |> Catena.check_json(language_selection: selection("0.1.2"))
  end

  @tag obligations: ~w(ED-OBL-023 ED-OBL-035)
  test "0.1.7 retains 0.1.6 verification-only definitions" do
    document =
      module_document("C008CumulativeSpecification", "0.1.7")
      |> update_in(["definitions"], fn definitions ->
        definitions ++
          [
            %{
              "name" => "compile_check",
              "parameters" => [],
              "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
              "body" => %{"tag" => "integer", "value" => 1},
              "verification_only" => true
            }
          ]
      end)

    assert {:ok, core} = document |> JSON.encode!() |> Catena.check_json()
    assert Enum.any?(core.definitions, &(&1.name == "compile_check" and &1.verification_only?))
  end

  @tag obligations: ~w(ED-OBL-002 ED-OBL-003 ED-OBL-007 ED-OBL-026)
  test "0.1.7 package manifests require exact selection and legacy manifests report safe additions" do
    manifest = package_manifest("C008Manifest", "0.1.7")
    assert {:ok, decoded} = manifest |> JSON.encode!() |> Manifest.decode()
    assert decoded.selection == LanguageVersion.legacy_selection("0.1.7")

    assert {:error, %{id: "EDN001"}} =
             manifest
             |> Map.delete("language_revision")
             |> JSON.encode!()
             |> Manifest.decode()

    assert {:error, %{id: "EDN001"}} =
             manifest
             |> put_in(["diagnostics", "deny"], ["UNKNOWN"])
             |> JSON.encode!()
             |> Manifest.decode()

    legacy = legacy_manifest("C008LegacyManifest")
    assert {:ok, legacy_decoded} = legacy |> JSON.encode!() |> Manifest.decode()
    assert legacy_decoded.selection == LanguageVersion.legacy_selection("0.1.6")
    assert [%{id: "EDN002", fixes: fixes}] = legacy_decoded.advisories
    assert length(fixes) == 3

    explicit_legacy = Map.merge(legacy, selection("0.1.6"))
    assert {:ok, explicit_decoded} = explicit_legacy |> JSON.encode!() |> Manifest.decode()
    assert explicit_decoded.selection == legacy_decoded.selection
    assert explicit_decoded.advisories == []

    assert {:error, %{id: "EDN001"}} =
             legacy
             |> Map.put("edition", "0.1")
             |> JSON.encode!()
             |> Manifest.decode()

    assert {:error, %{id: "EDN001", path: "$.language_revision"}} =
             explicit_legacy
             |> Map.put("language_revision", "0.1.7")
             |> JSON.encode!()
             |> Manifest.decode()

    migrated = legacy |> Map.put("version", "0.1.7") |> Map.merge(selection("0.1.7"))
    assert {:ok, migrated_decoded} = migrated |> JSON.encode!() |> Manifest.decode()
    assert migrated_decoded.artifact_version == "0.1.7"
    assert migrated_decoded.selection == LanguageVersion.legacy_selection("0.1.7")
  end

  @tag obligations: ~w(ED-OBL-007 ED-OBL-020 ED-OBL-027)
  test "making a legacy manifest selection explicit preserves all output bytes" do
    directory = temporary_directory!("legacy-manifest")
    source_path = Path.join(directory, "module.json")
    manifest_path = Path.join(directory, "package.json")
    File.write!(source_path, module_document("C008LegacyBytes", "0.1.6") |> JSON.encode!())

    implicit_manifest = legacy_manifest("C008LegacyBytes")
    File.write!(manifest_path, JSON.encode!(implicit_manifest))

    assert {:ok, implicit} = Linker.compile_manifest(manifest_path)
    assert [%{id: "EDN002"}] = implicit.diagnostics

    implicit_bytes = %{
      module: File.read!(Path.join(directory, "C008LegacyBytes.beam")),
      interface: File.read!(Path.join(directory, "module.cati.json")),
      companion: File.read!(implicit.output),
      assurance: File.read!(implicit.assurance)
    }

    File.write!(
      manifest_path,
      implicit_manifest |> Map.merge(selection("0.1.6")) |> JSON.encode!()
    )

    assert {:ok, explicit} = Linker.compile_manifest(manifest_path)
    assert explicit.diagnostics == []

    assert implicit_bytes == %{
             module: File.read!(Path.join(directory, "C008LegacyBytes.beam")),
             interface: File.read!(Path.join(directory, "module.cati.json")),
             companion: File.read!(explicit.output),
             assurance: File.read!(explicit.assurance)
           }
  end

  @tag obligations: ~w(ED-OBL-017 ED-OBL-018 ED-OBL-031 ED-OBL-036)
  test "interfaces bind enabled and publicly required previews and consumers fail closed" do
    assert {:ok, _module, _beam, metadata} =
             module_document("C008Interface", "0.1.7")
             |> JSON.encode!()
             |> Catena.compile_json()

    assert {:ok, interface} = Interface.decode(metadata.interface_binary)
    assert interface.edition == "0.1"
    assert interface.language_revision == "0.1.7"
    assert interface.previews == []
    assert interface.required_previews == []

    tampered = put_in(metadata.interface, ["language_revision"], "0.1.6")
    assert {:error, %{id: "A005"}} = tampered |> Interface.encode() |> Interface.decode()

    consumer = LanguageVersion.current_selection()

    assert {:error, %{id: "PRV002", details: %{preview: "modelled-preview"}}} =
             LanguageLifecycle.validate_interfaces(consumer, [
               %{module: "ModelledDependency", required_previews: ["modelled-preview"]}
             ])
  end

  @tag obligations: ~w(ED-OBL-019 ED-OBL-021 ED-OBL-024 ED-OBL-032 ED-OBL-035)
  test "0.1.7 artifacts and assurance bind the package selection without runtime dispatch" do
    directory = temporary_directory!("package")
    source_path = Path.join(directory, "module.json")
    manifest_path = Path.join(directory, "package.json")
    File.write!(source_path, module_document("C008Package", "0.1.7") |> JSON.encode!())
    File.write!(manifest_path, package_manifest("C008Package", "0.1.7") |> JSON.encode!())

    assert {:ok, first} = Linker.compile_manifest(manifest_path)
    assert first.selection == LanguageVersion.legacy_selection("0.1.7")
    assert first.artifact_version == "0.1.7"
    assert first.diagnostics == []
    assert File.exists?(first.output)
    assert File.exists?(first.assurance)

    first_beam = File.read!(Path.join(directory, "C008Package.beam"))
    first_interface = File.read!(Path.join(directory, "module.cati.json"))
    first_assurance = File.read!(first.assurance)

    assert {:ok, verified} = Assurance.verify(first_assurance, directory, nil)
    assert verified.package == "demo"

    assert {:ok, assurance_document} = CanonicalJCS.decode(first_assurance, canonical: true)
    assert get_in(assurance_document, ["signed", "edition"]) == "0.1"
    assert get_in(assurance_document, ["signed", "language_revision"]) == "0.1.7"
    assert get_in(assurance_document, ["signed", "previews"]) == []

    assert {:ok, {:C008Package, [compile_info: compile_info]}} =
             :beam_lib.chunks(first_beam, [:compile_info])

    assert compile_info[:catena_edition] == ~c"0.1"
    assert compile_info[:catena_language_revision] == ~c"0.1.7"
    assert compile_info[:catena_previews] == []

    downgraded =
      assurance_document
      |> put_in(["signed", "language_revision"], "0.1.6")
      |> put_in(["signed", "specification"], "0.1.6")

    assert {:error, %{id: "ART001"}} =
             downgraded |> CanonicalJCS.encode() |> Assurance.verify(directory, nil)

    File.write!(manifest_path, package_manifest("C008Package", "0.1.6") |> JSON.encode!())
    assert {:ok, second} = Linker.compile_manifest(manifest_path)
    second_beam = File.read!(Path.join(directory, "C008Package.beam"))
    second_interface = File.read!(Path.join(directory, "module.cati.json"))
    second_assurance = File.read!(second.assurance)

    refute first_beam == second_beam
    refute first_interface == second_interface
    refute first_assurance == second_assurance

    assert {:ok, ast} =
             module_document("C008NoDispatch", "0.1.7")
             |> JSON.encode!()
             |> Catena.AST.Decoder.decode()

    assert {:ok, _module, _beam, metadata} = Catena.Compiler.compile(ast)
    refute inspect(metadata.forms) =~ "catena_edition"
    refute inspect(metadata.forms) =~ "catena_previews"
  end

  @tag obligations: ~w(ED-OBL-021 ED-OBL-032)
  test "specialization identities change with exact selection" do
    assert {:ok, _module, _beam, metadata} =
             template_module()
             |> JSON.encode!()
             |> Catena.compile_json(
               language_selection: selection("0.1.7"),
               artifact_version: "0.1.7"
             )

    assert {:ok, interface} = Interface.decode(metadata.interface_binary)

    manifest = %{
      version: "0.1.7",
      artifact_version: "0.1.7",
      selection: LanguageVersion.current_selection(),
      companion_module: "C008Specialized",
      roots: [specialization_root()]
    }

    assert {:ok, _module, first_beam, first} = Linker.link(manifest, [interface])

    older = %{manifest | selection: LanguageVersion.legacy_selection("0.1.6")}
    assert {:ok, _module, second_beam, second} = Linker.link(older, [interface])

    refute first.specialization_keys == second.specialization_keys
    refute first_beam == second_beam
  end

  @tag obligations: ~w(ED-OBL-012 ED-OBL-016 ED-OBL-030)
  test "the 0.1.7 policy algebra constrains selection and agrees with its reference oracle" do
    requirement = %{
      "op" => "all",
      "requirements" => [
        %{"op" => "edition", "allowed" => ["0.1"]},
        %{"op" => "language_revision", "from" => "0.1.6", "to" => "0.1.7"},
        %{"op" => "previews", "allowed" => []},
        %{"op" => "diagnostics", "absent" => ["DEP001"]}
      ]
    }

    policy_context = %{
      format_version: "0.1.7",
      action: "build",
      state: "Draft",
      profile: "static",
      edition: "0.1",
      language_revision: "0.1.7",
      previews: [],
      diagnostics: [],
      sequence: 1,
      root: nil,
      approvals: [],
      approval_payload: %{},
      evidence: []
    }

    assert {:ok, true, production, _steps} = Policy.evaluate(requirement, policy_context)

    assert {:ok, true, [%{"requirement" => oracle}]} =
             Reference.decide(
               [%{"id" => "selection", "requirement" => requirement}],
               policy_context
             )

    assert production == oracle

    value = governance_bundle("0.1.7", requirement)
    assert {:ok, bundle} = value |> CanonicalJCS.encode() |> Catena.Governance.decode_bundle()
    context = governance_context("0.1.7")

    assert {:ok, result} = Catena.Governance.evaluate(bundle, nil, context)
    assert result.approval_payload["edition"] == "0.1"
    assert result.approval_payload["language_revision"] == "0.1.7"
    assert result.approval_payload["previews"] == []
    assert result.approval_payload["diagnostics"] == []
    assert {:ok, reference} = Reference.evaluate(bundle, nil, context)
    assert reference.explanations == result.explanations

    older_context = %{context | language_revision: "0.1.6"}
    assert {:ok, older_result} = Catena.Governance.evaluate(bundle, nil, older_context)
    refute older_result.approval_payload == result.approval_payload

    legacy = governance_bundle("0.1.6", %{"op" => "edition", "allowed" => ["0.1"]})

    assert {:ok, legacy_bundle} =
             legacy |> CanonicalJCS.encode() |> Catena.Governance.decode_bundle()

    assert {:error, %{id: "GOV002"}} =
             Catena.Governance.evaluate(legacy_bundle, nil, governance_context("0.1.6"))

    malformed = %{"op" => "previews", "allowed" => ["invented"]}
    assert {:error, %{id: "GOV002"}} = Policy.evaluate(malformed, policy_context)
  end

  @tag obligations: ~w(ED-OBL-022 ED-OBL-023 ED-OBL-025 ED-OBL-033)
  test "trust roots and signatures use one declared version domain without fallback" do
    signer = keypair("signer")
    recovery = keypair("recovery")
    state = root_state(1, [signer, recovery], [signer.id], [recovery.id])
    new_root = decoded_root("0.1.7", state)
    old_root = decoded_root("0.1.6", state)

    assert new_root.version == "0.1.7"
    assert old_root.version == "0.1.6"
    refute new_root.digest == old_root.digest
    assert old_root.digest == CanonicalJCS.digest(state)

    payload = %{"release" => "candidate"}
    new_signature = signature(signer, "manifest", "0.1.7", payload)
    old_signature = signature(signer, "manifest", "0.1.6", payload)

    assert {:ok, ["signer"]} =
             Crypto.verify_threshold(new_root, "normal", "manifest", payload, [new_signature], 1)

    assert {:error, _reason} =
             Crypto.verify_threshold(new_root, "normal", "manifest", payload, [old_signature], 1)

    requirement = %{"op" => "edition", "allowed" => ["0.1"]}

    assert {:ok, bundle} =
             governance_bundle("0.1.7", requirement)
             |> CanonicalJCS.encode()
             |> Catena.Governance.decode_bundle()

    assert {:error, %{id: "GOV001"}} =
             Catena.Governance.evaluate(bundle, old_root, governance_context("0.1.7"))
  end

  @tag obligations: ~w(ED-OBL-028)
  test "language-info is available as mutation-free JSON from the CLI" do
    output = capture_io(fn -> Catena.CLI.main(["language-info"]) end)
    assert {:ok, document} = JSON.decode(String.trim(output))
    assert document == LanguageInfo.document()
  end

  defp selection(revision) do
    %{"edition" => "0.1", "language_revision" => revision, "previews" => []}
  end

  defp module_document(name, version) do
    %{
      "version" => version,
      "origin" => "pkg://tests/#{name}",
      "module" => name,
      "source" => "module.json",
      "exports" => ["main"],
      "type_exports" => [],
      "type_groups" => [],
      "imports" => [],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{
            "forall" => [],
            "type" => %{"tag" => "integer"}
          },
          "body" => %{"tag" => "integer", "value" => 8}
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

  defp package_manifest(module, revision) do
    %{
      "format" => "catena-package-manifest",
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => revision,
      "previews" => [],
      "diagnostics" => %{"deny" => []},
      "package" => "demo",
      "profile" => "static",
      "companion_module" => "C008Companion",
      "modules" => [
        %{
          "source" => "module.json",
          "beam" => module <> ".beam",
          "interface" => "module.cati.json"
        }
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "C008Companion.beam",
      "assurance" => "assurance.json"
    }
  end

  defp legacy_manifest(module) do
    %{
      "format" => "catena-package-manifest",
      "version" => "0.1.6",
      "package" => "demo",
      "profile" => "static",
      "companion_module" => "C008LegacyCompanion",
      "modules" => [
        %{
          "source" => "module.json",
          "beam" => module <> ".beam",
          "interface" => "module.cati.json"
        }
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "C008LegacyCompanion.beam",
      "assurance" => "assurance.json"
    }
  end

  defp governance_bundle(version, requirement) do
    %{
      "format" => "catena-governance-bundle",
      "version" => version,
      "package" => "demo",
      "profile" => "static",
      "policies" => [
        %{
          "id" => "selection",
          "scope" => %{"kind" => "package", "name" => "demo"},
          "requirement" => requirement
        }
      ],
      "evidence" => [],
      "approvals" => [],
      "transitions" => [],
      "manifest_signatures" => []
    }
  end

  defp governance_context(revision) do
    %{
      action: "build",
      package: "demo",
      profile: "static",
      edition: "0.1",
      language_revision: revision,
      previews: [],
      diagnostics: [],
      modules: [],
      subjects: [],
      compiler_evidence: [],
      claims: [],
      claim_digests: [],
      artifact_digests: []
    }
  end

  defp template_module do
    type = constructor("Int", "Type", "pkg://template")

    %{
      "version" => "0.1.7",
      "origin" => "pkg://template",
      "module" => "C008TemplateSource",
      "exports" => [],
      "type_exports" => [],
      "type_groups" => [],
      "imports" => [],
      "definitions" => [],
      "traits" => [],
      "instances" => [
        %{
          "trait" => "Equatable",
          "arguments" => [type],
          "owner" => "pkg://template",
          "methods" => %{"equals" => "erlang.=:="},
          "law_status" => "tested"
        }
      ],
      "templates" => [
        %{
          "id" => "equals_specialized",
          "parameters" => ["left", "right"],
          "helpers" => [],
          "body" => %{
            "tag" => "trait_call",
            "trait" => "Equatable",
            "arguments" => [
              %{"tag" => "variable", "name" => "$type0", "kind" => "Type"}
            ],
            "method" => "equals",
            "values" => [
              %{"tag" => "argument", "name" => "left"},
              %{"tag" => "argument", "name" => "right"}
            ]
          }
        }
      ],
      "effects" => [],
      "handlers" => [],
      "specifications" => []
    }
  end

  defp specialization_root do
    type = constructor("Int", "Type", "pkg://template")

    %{
      "template" => "equals_specialized",
      "export" => "equals_int",
      "types" => [type],
      "instances" => [%{"trait" => "Equatable", "arguments" => [type]}]
    }
  end

  defp constructor(id, kind, owner),
    do: %{"tag" => "constructor", "id" => id, "kind" => kind, "owner" => owner}

  defp keypair(id) do
    {public, private} = :crypto.generate_key(:eddsa, :ed25519)

    %{
      id: id,
      public: Base.encode16(public, case: :lower),
      private: private
    }
  end

  defp signature(key, kind, version, payload) do
    signed =
      :crypto.sign(:eddsa, :none, CanonicalJCS.payload(kind, version, payload), [
        key.private,
        :ed25519
      ])

    %{"principal" => key.id, "signature" => Base.encode16(signed, case: :lower)}
  end

  defp root_state(sequence, keys, normal, recovery) do
    %{
      "sequence" => sequence,
      "principals" =>
        Enum.map(keys, &%{"id" => &1.id, "public_key" => &1.public}) |> Enum.sort_by(& &1["id"]),
      "roles" => %{
        "normal" => %{"principals" => normal, "threshold" => length(normal)},
        "recovery" => %{"principals" => recovery, "threshold" => length(recovery)}
      },
      "delegations" => [],
      "revocations" => %{"principals" => [], "delegations" => [], "evidence" => []}
    }
  end

  defp decoded_root(version, state) do
    document = %{
      "format" => "catena-trust-root",
      "version" => version,
      "namespace" => "demo",
      "initial" => state,
      "history" => []
    }

    {:ok, root} = document |> CanonicalJCS.encode() |> TrustRoot.decode()
    root
  end

  defp temporary_directory!(suffix) do
    path =
      Path.join(System.tmp_dir!(), "catena-c008-#{suffix}-#{System.unique_integer([:positive])}")

    File.mkdir_p!(path)
    on_exit(fn -> File.rm_rf!(path) end)
    path
  end
end
