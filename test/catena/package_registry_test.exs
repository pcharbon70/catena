defmodule Catena.PackageRegistryTest do
  use ExUnit.Case, async: true

  alias Catena.{Assurance, CanonicalJCS, Package.Deps, Package.Registry}
  alias Catena.Package.Reproducible, as: Reproducible

  defp key do
    {public, private} = :crypto.generate_key(:eddsa, :ed25519)
    public = Base.encode16(public, case: :lower)
    id = :crypto.hash(:sha256, public) |> Base.encode16(case: :lower)
    %{id: id, public: public, private: private}
  end

  defp signature(kind, payload, key) do
    signed =
      :crypto.sign(:eddsa, :none, Registry.signing_payload(kind, payload), [key.private, :ed25519])

    %{"principal" => key.id, "signature" => Base.encode16(signed, case: :lower)}
  end

  defp root(keys, sequence \\ 1, expires \\ 10_000) do
    [root, snapshot, recovery, publisher | _] = keys

    value = %{
      "format" => "catena-registry-root",
      "version" => "1",
      "sequence" => sequence,
      "expires" => expires,
      "keys" => Map.new(keys, &{&1.id, &1.public}),
      "roles" => %{
        "root" => %{"principals" => [root.id], "threshold" => 1},
        "snapshot" => %{"principals" => [snapshot.id], "threshold" => 1},
        "recovery" => %{"principals" => [recovery.id], "threshold" => 1}
      },
      "delegations" => [
        %{"package" => "demo", "principals" => [publisher.id], "threshold" => 1},
        %{"package" => "native-demo", "principals" => [publisher.id], "threshold" => 1}
      ]
    }

    {:ok, decoded} = value |> CanonicalJCS.encode() |> Registry.decode_root()
    {value, decoded}
  end

  defp provenance(reproducible_input) do
    %{
      "dependencies" => %{},
      "selection" => %{"edition" => "0.1", "language_revision" => "0.1.73", "previews" => []},
      "modules" => ["src/main.json"],
      "interfaces" => ["out/main.cati.json"],
      "roots" => [],
      "output" => "out/main.beam",
      "interface_digests" => [],
      "component_digests" => [],
      "reproducible_input" => reproducible_input
    }
  end

  defp source_artifact(bytes, reproducible_input) do
    p = provenance(reproducible_input)

    bundle =
      Deps.bundle_digest(%{
        name: "demo",
        version: "1.0.0",
        dependencies: p["dependencies"],
        selection: p["selection"],
        modules: p["modules"],
        interfaces: p["interfaces"],
        roots: p["roots"],
        output: p["output"],
        interface_digests: [],
        component_digests: []
      })

    %{
      "package" => "demo",
      "version" => "1.0.0",
      "content_digest" => hash(bytes),
      "bundle_digest" => bundle,
      "size" => byte_size(bytes),
      "kind" => "source",
      "provenance" => p
    }
  end

  defp release(artifact, publisher, status \\ "active", status_sequence \\ 1) do
    %{
      "artifact" => artifact,
      "publisher_signatures" => [signature("release", artifact, publisher)],
      "status" => status,
      "status_sequence" => status_sequence
    }
  end

  defp snapshot(root, releases, signer, sequence \\ 1, expires \\ 1000) do
    signed = %{
      "format" => "catena-registry-snapshot",
      "version" => "1",
      "sequence" => sequence,
      "root_sequence" => root["sequence"],
      "expires" => expires,
      "releases" =>
        Enum.sort_by(releases, &{&1["artifact"]["package"], &1["artifact"]["version"]})
    }

    envelope = %{
      "format" => "catena-registry-snapshot-envelope",
      "version" => "1",
      "signed" => signed,
      "signatures" => [signature("snapshot", signed, signer)]
    }

    CanonicalJCS.encode(envelope)
  end

  defp fixture(status \\ "active") do
    keys = Enum.map(1..5, fn _ -> key() end)
    {raw, root} = root(keys)
    files = source_files()
    {:ok, plan} = Reproducible.plan(files, "package.json")
    bytes = files["src/main.json"]
    artifact = source_artifact(bytes, plan["digest"])
    release = release(artifact, Enum.at(keys, 3), status)
    snapshot = snapshot(raw, [release], Enum.at(keys, 1))

    %{
      keys: keys,
      raw: raw,
      root: root,
      bytes: bytes,
      files: files,
      plan: plan,
      artifact: artifact,
      release: release,
      snapshot: snapshot
    }
  end

  test "authorized source release resolves, acquires, and replays an exact lock offline" do
    f = fixture()
    assert {:ok, client} = Registry.open(f.root, f.snapshot, 100)

    assert {:ok, acquired} =
             Registry.acquire(
               client,
               "demo",
               "1.0.0",
               [%{}, %{f.artifact["content_digest"] => f.bytes}],
               observed_at: 100
             )

    assert acquired.bytes == f.bytes
    assert acquired.mirror == 1

    {:ok, environment} = Registry.dependency_environment(client)
    manifest = %{dependencies: %{"demo" => "^1.0.0"}}
    assert {:ok, resolution} = Deps.resolve(manifest, environment)
    lockfile = Deps.generate_lockfile(resolution)

    assert {:ok, files} =
             Assurance.verify_registry_lock(client, manifest, lockfile, [
               %{f.artifact["content_digest"] => f.bytes}
             ])

    assert files == %{"registry/demo-1.0.0.bundle" => f.bytes}

    lock = %{
      "package" => "demo",
      "version" => "1.0.0",
      "bundle_digest" => f.artifact["bundle_digest"],
      "snapshot_digest" => client.snapshot_digest
    }

    assert {:ok, %{bytes: bytes}} =
             Registry.replay_locked(client, lock, [%{f.artifact["content_digest"] => f.bytes}])

    assert bytes == f.bytes

    expected_plan = f.plan

    assert {:ok, ^expected_plan} =
             Reproducible.plan(Map.put(f.files, "src/main.json", acquired.bytes), "package.json")

    assert {:ok, built} = Reproducible.build(f.plan, build_root())
    assert Map.has_key?(built.outputs, "out/main.beam")
  end

  test "forged publisher metadata, replaced content, and mirror mismatch fail closed" do
    f = fixture()

    forged =
      Map.put(
        f.release,
        "artifact",
        Map.put(f.release["artifact"], "size", byte_size(f.bytes) + 1)
      )

    bad_snapshot = snapshot(f.raw, [forged], Enum.at(f.keys, 1))
    assert {:error, _} = Registry.open(f.root, bad_snapshot, 100)
    {:ok, client} = Registry.open(f.root, f.snapshot, 100)

    assert {:error, _} =
             Registry.acquire(
               client,
               "demo",
               "1.0.0",
               [%{f.artifact["content_digest"] => "replacement"}],
               observed_at: 100
             )

    assert {:error, _} = Registry.acquire(client, "demo", "1.0.0", [], observed_at: 100)

    forged_client = %{client | snapshot_digest: String.duplicate("0", 64)}

    assert {:error, _} =
             Registry.acquire(forged_client, "demo", "1.0.0", [], observed_at: 100)
  end

  test "freshness, rollback, and same-sequence equivocation are independently rejected" do
    f = fixture()
    assert {:error, _} = Registry.open(f.root, f.snapshot, 1001)
    {:ok, first} = Registry.open(f.root, f.snapshot, 100)

    assert {:error, _} =
             Registry.acquire(first, "demo", "1.0.0", [], observed_at: 1001)

    assert {:ok, same} = Registry.open(f.root, f.snapshot, 100, first)
    assert same.snapshot_digest == first.snapshot_digest
    changed = Map.put(f.release, "status_sequence", 2)
    equivocation = snapshot(f.raw, [changed], Enum.at(f.keys, 1), 1)
    assert {:error, _} = Registry.open(f.root, equivocation, 100, first)
    older = snapshot(f.raw, [f.release], Enum.at(f.keys, 1), 1)
    newer = snapshot(f.raw, [f.release], Enum.at(f.keys, 1), 2)
    {:ok, second} = Registry.open(f.root, newer, 100, first)
    assert {:error, _} = Registry.open(f.root, older, 100, second)

    compromised = release(f.artifact, Enum.at(f.keys, 3), "compromised", 2)
    compromised_snapshot = snapshot(f.raw, [compromised], Enum.at(f.keys, 1), 3)
    {:ok, terminal} = Registry.open(f.root, compromised_snapshot, 100, second)
    restored = release(f.artifact, Enum.at(f.keys, 3), "active", 3)

    assert {:error, _} =
             Registry.open(
               f.root,
               snapshot(f.raw, [restored], Enum.at(f.keys, 1), 4),
               100,
               terminal
             )
  end

  test "yanks preserve exact locked history while compromise denies every acquisition" do
    f = fixture("yanked")
    {:ok, client} = Registry.open(f.root, f.snapshot, 100)
    mirrors = [%{f.artifact["content_digest"] => f.bytes}]
    assert {:error, _} = Registry.acquire(client, "demo", "1.0.0", mirrors, observed_at: 100)

    lock = %{
      "package" => "demo",
      "version" => "1.0.0",
      "bundle_digest" => f.artifact["bundle_digest"],
      "snapshot_digest" => client.snapshot_digest
    }

    assert {:ok, _} = Registry.replay_locked(client, lock, mirrors)

    assert {:error, _} =
             Registry.replay_locked(
               client,
               %{lock | "snapshot_digest" => String.duplicate("0", 64)},
               mirrors
             )

    compromised = release(f.artifact, Enum.at(f.keys, 3), "compromised", 2)
    snapshot = snapshot(f.raw, [compromised], Enum.at(f.keys, 1), 2)
    {:ok, denied} = Registry.open(f.root, snapshot, 100, client)

    assert {:error, _} =
             Registry.replay_locked(
               denied,
               %{lock | "snapshot_digest" => denied.snapshot_digest},
               mirrors
             )
  end

  test "normal root rotation requires old and new authority while recovery replaces compromise" do
    f = fixture()
    new_keys = Enum.map(1..5, fn _ -> key() end)
    {next, _} = root(new_keys, 2)
    payload = %{"mode" => "normal", "prior_digest" => f.root["digest"], "signed" => next}

    envelope = %{
      "format" => "catena-registry-root-update",
      "version" => "1",
      "mode" => "normal",
      "prior_digest" => f.root["digest"],
      "signed" => next,
      "old_signatures" => [signature("root", payload, hd(f.keys))],
      "new_signatures" => [signature("root", payload, hd(new_keys))]
    }

    assert {:ok, rotated} = Registry.rotate(f.root, CanonicalJCS.encode(envelope), 100)
    assert rotated["sequence"] == 2

    assert {:error, _} =
             Registry.rotate(
               f.root,
               CanonicalJCS.encode(%{envelope | "new_signatures" => []}),
               100
             )

    recovery_payload = %{
      "mode" => "recovery",
      "prior_digest" => f.root["digest"],
      "signed" => next
    }

    recovery = %{
      envelope
      | "mode" => "recovery",
        "old_signatures" => [signature("root", recovery_payload, Enum.at(f.keys, 2))],
        "new_signatures" => []
    }

    assert {:ok, _} = Registry.rotate(f.root, CanonicalJCS.encode(recovery), 100)
  end

  test "native release binds platform, toolchain, package digest, provenance, and obligations" do
    f = fixture()
    {:ok, observed} = Catena.OTP.Profile.require_supported()
    bytes = "native package envelope bytes"

    provenance = %{
      "native_package_digest" => hash(bytes),
      "platform" => observed["architecture"],
      "toolchain" => Catena.OTP.Profile.digest(observed),
      "reproducible_input" => String.duplicate("b", 64),
      "unsafe_obligations" => ["vm-crash-possible"]
    }

    artifact = %{
      "package" => "native-demo",
      "version" => "1.0.0",
      "content_digest" => hash(bytes),
      "bundle_digest" => String.duplicate("c", 64),
      "size" => byte_size(bytes),
      "kind" => "native",
      "provenance" => provenance
    }

    release = release(artifact, Enum.at(f.keys, 3))
    snapshot = snapshot(f.raw, [release], Enum.at(f.keys, 1))
    {:ok, client} = Registry.open(f.root, snapshot, 100)
    mirrors = [%{artifact["content_digest"] => bytes}]

    assert {:error, _} =
             Registry.acquire(client, "native-demo", "1.0.0", mirrors, observed_at: 100)

    assert {:error, _} =
             Registry.acquire(client, "native-demo", "1.0.0", mirrors,
               acknowledgements: ["vm-crash-possible"],
               observed_at: 100,
               platform: "wrong-platform"
             )

    assert {:ok, %{bytes: ^bytes}} =
             Registry.acquire(client, "native-demo", "1.0.0", mirrors,
               acknowledgements: ["vm-crash-possible"],
               observed_at: 100
             )
  end

  test "metadata limits, canonical shapes, and machine profile are explicit" do
    f = fixture()

    assert {:error, _} =
             Registry.decode_root(CanonicalJCS.encode(Map.put(f.raw, "unknown", true)))

    assert {:error, _} = Registry.open(f.root, f.snapshot <> " ", 100)

    assert {:error, _} =
             Registry.open(
               %{f.root | "digest" => String.duplicate("0", 64)},
               f.snapshot,
               100
             )

    assert Registry.profile().releases == 4096
    assert Registry.profile().compromised_replay == false
  end

  defp hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)

  defp build_root,
    do: Path.join(System.tmp_dir!(), "catena-registry-#{System.unique_integer([:positive])}")

  defp source_files do
    source = %{
      "version" => "0.1.6",
      "origin" => "pkg://registry/demo",
      "module" => "RegistryDemo",
      "exports" => ["main"],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "integer", "value" => 74}
        }
      ]
    }

    manifest = %{
      "format" => "catena-package-manifest",
      "version" => "0.1.6",
      "package" => "demo",
      "profile" => "static",
      "companion_module" => "RegistryCompanion",
      "modules" => [
        %{
          "source" => "src/main.json",
          "beam" => "out/main.beam",
          "interface" => "out/main.cati.json"
        }
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "out/companion.beam",
      "assurance" => "out/assurance.json"
    }

    %{"package.json" => JSON.encode!(manifest), "src/main.json" => JSON.encode!(source)}
  end
end
