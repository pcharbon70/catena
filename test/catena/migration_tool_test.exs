defmodule Catena.MigrationToolTest do
  use ExUnit.Case, async: false

  alias Catena.Tool.Migration

  @moduletag :tmp_dir

  test "C008 JSON edits preview and commit as a checked transaction with retained backup", %{
    tmp_dir: root
  } do
    {path, original, fixes} = legacy_module(root, "module.json")
    request = request("module.json", "module", fixes)

    assert {:ok, plan} = Migration.plan(root, [request])
    assert {:ok, preview} = Migration.preview(plan)
    assert preview["plan"] == plan["digest"]
    assert [%{"before" => before, "after" => after_bytes}] = preview["files"]
    assert Base.decode64!(before) == original
    refute Base.decode64!(after_bytes) == original

    assert {:error, :migration_not_authorized} = Migration.apply(plan, root)
    assert File.read!(path) == original

    assert {:ok, audit} = Migration.apply(plan, root, authorized: true)
    assert audit["status"] == "committed"
    refute audit["governance_approval_inherited"]
    assert audit["digest"] == Catena.CanonicalJCS.digest(Map.delete(audit, "digest"))

    assert {:ok, migrated} = JSON.decode(File.read!(path))
    assert migrated["edition"] == "0.1"
    assert migrated["language_revision"] == migrated["version"]
    assert migrated["previews"] == []
    assert {:ok, %{diagnostics: []}} = Catena.check_json(File.read!(path))

    [file] = audit["files"]
    assert File.read!(Path.join(root, file["backup"])) == original
    assert file["verification"]["kind"] == "module"
    assert is_binary(file["verification"]["interface_digest"])
  end

  test "legacy manifests use the same edit protocol and are rechecked", %{tmp_dir: root} do
    path = Path.join(root, "catena.package.json")
    original = JSON.encode!(legacy_manifest())
    File.write!(path, original)
    assert {:ok, %{advisories: [%{fixes: fixes}]}} = Catena.Package.Manifest.decode(original)

    assert {:ok, plan} =
             Migration.plan(root, [request("catena.package.json", "manifest", fixes)])

    assert {:ok, audit} = Migration.apply(plan, root, authorized: true)
    assert [%{"verification" => verification}] = audit["files"]
    assert verification["kind"] == "manifest"
    assert verification["language_selection"]["language_revision"] == "0.1.6"
    assert {:ok, %{advisories: []}} = Catena.Package.Manifest.decode(File.read!(path))
  end

  test "stale, ambiguous, unsupported, and semantically invalid edits change no input", %{
    tmp_dir: root
  } do
    {path, original, fixes} = legacy_module(root, "module.json")

    overlapping = [
      edit("add", "$.metadata", %{}),
      edit("add", "$.metadata.name", "demo")
    ]

    assert {:error, :ambiguous_or_overlapping_migration_edits} =
             Migration.plan(root, [request("module.json", "module", overlapping)])

    maybe = [Map.put(hd(fixes), "applicability", "maybe-incorrect")]

    assert {:error, :unsupported_migration_edit} =
             Migration.plan(root, [request("module.json", "module", maybe)])

    assert {:ok, valid_plan} = Migration.plan(root, [request("module.json", "module", fixes)])

    tampered =
      update_in(valid_plan, ["files", Access.at(0), "result_digest"], fn _ ->
        String.duplicate("0", 64)
      end)

    assert {:error, :invalid_migration_plan} = Migration.preview(tampered)

    invalid = [edit("replace", "$.module", "lowercase")]
    assert {:ok, invalid_plan} = Migration.plan(root, [request("module.json", "module", invalid)])

    assert {:error, :migrated_document_rejected} =
             Migration.apply(invalid_plan, root, authorized: true)

    assert File.read!(path) == original

    changed_interface = [edit("replace", "$.exports", [])]

    assert {:ok, interface_plan} =
             Migration.plan(root, [request("module.json", "module", changed_interface)])

    assert {:error, :behavioral_change_requires_review} =
             Migration.apply(interface_plan, root, authorized: true)

    assert File.read!(path) == original

    assert {:ok, stale_plan} = Migration.plan(root, [request("module.json", "module", fixes)])
    changed = original <> "\n"
    File.write!(path, changed)

    assert {:error, :stale_migration_preimage} =
             Migration.apply(stale_plan, root, authorized: true)

    assert File.read!(path) == changed
  end

  test "path escape and symlink traversal are refused", %{tmp_dir: root} do
    {_path, _original, fixes} = legacy_module(root, "module.json")

    assert {:error, :unsafe_migration_path} =
             Migration.plan(root, [request("../outside.json", "module", fixes)])

    File.ln_s!(Path.join(root, "module.json"), Path.join(root, "linked.json"))

    assert {:error, :unsafe_migration_path} =
             Migration.plan(root, [request("linked.json", "module", fixes)])
  end

  test "a symlinked backup base cannot redirect retained preimages", %{tmp_dir: root} do
    {_path, _original, fixes} = legacy_module(root, "module.json")
    assert {:ok, plan} = Migration.plan(root, [request("module.json", "module", fixes)])
    outside = Path.join(root, "outside")
    File.mkdir!(outside)
    File.ln_s!(outside, Path.join(root, ".catena-migration-backups"))

    assert {:error, :unsafe_or_existing_migration_backup} =
             Migration.apply(plan, root, authorized: true)
  end

  test "an interrupted multi-file commit restores every preimage", %{tmp_dir: root} do
    {first, first_bytes, first_fixes} = legacy_module(root, "first.json")
    {second, second_bytes, second_fixes} = legacy_module(root, "second.json")

    requests = [
      request("first.json", "module", first_fixes),
      request("second.json", "module", second_fixes)
    ]

    assert {:ok, plan} = Migration.plan(root, requests)

    assert {:error, :migration_interrupted} =
             Migration.apply(plan, root,
               authorized: true,
               failure_injection: %{interrupt_after: 1}
             )

    assert File.read!(first) == first_bytes
    assert File.read!(second) == second_bytes
  end

  test "failure after moving an original restores its exact preimage", %{tmp_dir: root} do
    {path, original, fixes} = legacy_module(root, "module.json")
    assert {:ok, plan} = Migration.plan(root, [request("module.json", "module", fixes)])

    assert {:error, :migration_commit_failed} =
             Migration.apply(plan, root,
               authorized: true,
               failure_injection: %{move_failure_at: 1}
             )

    assert File.read!(path) == original
  end

  test "rollback failure is a distinct outcome", %{tmp_dir: root} do
    {path, original, fixes} = legacy_module(root, "module.json")
    assert {:ok, plan} = Migration.plan(root, [request("module.json", "module", fixes)])

    assert {:error, :migration_rollback_failed, [%{"path" => "module.json"}]} =
             Migration.apply(plan, root,
               authorized: true,
               failure_injection: %{interrupt_after: 1, rollback_failure_at: 1}
             )

    refute File.read!(path) == original
  end

  test "the retained migration slice is versioned and reports its source hold" do
    assert Catena.LanguageVersion.introduced(:migration_tool) == "0.1.93"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("migration-tool", "0.1.93")
    profile = Catena.ConformanceInfo.document()["migration_tool"]
    assert profile["public_source_rewrites"] == "held_for_p109"
    assert profile["governance_approval_inheritance"] == false
  end

  defp legacy_module(root, name) do
    original = File.read!(Path.expand("../fixtures/c002-option.catena.json", __DIR__))
    path = Path.join(root, name)
    File.write!(path, original)
    assert {:ok, %{diagnostics: [%{fixes: fixes}]}} = Catena.check_json(original)
    {path, original, fixes}
  end

  defp request(path, kind, edits),
    do: %{"path" => path, "document_kind" => kind, "edits" => edits}

  defp edit(operation, path, value) do
    %{
      "kind" => "json-edit",
      "operation" => operation,
      "path" => path,
      "value" => value,
      "applicability" => "machine-applicable"
    }
  end

  defp legacy_manifest do
    %{
      "format" => "catena-package-manifest",
      "version" => "0.1.6",
      "package" => "demo",
      "profile" => "static",
      "companion_module" => "MigrationCompanion",
      "modules" => [
        %{
          "source" => "module.json",
          "beam" => "Module.beam",
          "interface" => "module.cati.json"
        }
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "MigrationCompanion.beam",
      "assurance" => "assurance.json"
    }
  end
end
