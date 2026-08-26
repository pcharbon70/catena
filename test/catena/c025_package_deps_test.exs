defmodule Catena.C025PackageDepsTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Package.Deps}

  @env %{
    "a" => %{
      "1.0.0" => %{dependencies: %{"c" => "^1.0.0"}},
      "1.1.0" => %{dependencies: %{"c" => "^1.0.0"}}
    },
    "b" => %{"0.5.0" => %{dependencies: %{"c" => "~1.0.0"}}},
    "c" => %{"1.0.0" => %{}, "1.0.5" => %{}, "1.1.0" => %{}, "2.0.0" => %{}}
  }
  @root %{dependencies: %{"a" => "^1.0.0", "b" => "^0.5.0"}}

  @tag obligations: ~w(PK-OBL-001 PK-OBL-012)
  test "0.1.21 is an exact registered revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.28"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28)

    refute "0.1.21" in LanguageVersion.compilable_revisions()
    refute "0.1.21" in LanguageVersion.artifact_versions()
    refute "0.1.21" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} ==
             LanguageLifecycle.state("package-identity-and-dependencies", "0.1.21")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-21-package-identity-and-dependencies")
      )

    assert String.contains?(
             change["specification"],
             "package-identity-and-dependencies/manifest-dependencies-and-versions.md#"
           )

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, _} = Catena.build_namespace_environment([])
    assert {:ok, _} = Catena.compile_scc([])

    refute function_exported?(Catena.Package.Deps, :fetch, 2)
    refute function_exported?(Catena.Package.Deps, :publish, 1)
    refute function_exported?(Catena.Package.Deps, :sign, 2)
  end

  @tag obligations: ~w(PK-OBL-002 PK-OBL-004)
  test "the dependencies field and requirement grammar validate strictly" do
    for good <- ["1.2.3", "^1.2.3", "~0.1.0", "^0.0.1", "~2.10.0", "^1.2.3-rc.1"] do
      assert {:ok, _} = Deps.parse_requirement(good)
    end

    for bad <- [
          ">= 1.0.0",
          "< 1.0.0",
          "== 1.0.0",
          "~> 2.0",
          "^1.2.3+b",
          "1.2",
          "^01.2.3",
          ">= 1.0.0 and < 2.0.0",
          "",
          "^1.2.3-"
        ] do
      assert {:error, %{id: "PKG001", details: %{requirement: ^bad}}} =
               Deps.parse_requirement(bad)
    end

    assert {:ok, _} = Deps.resolve(%{dependencies: %{"a" => "^1.0.0"}}, @env)
    assert {:ok, _} = Deps.resolve(%{}, @env)

    assert {:error, %{id: "PKG001", details: %{name: "Bad_Name"}}} =
             Deps.resolve(%{dependencies: %{"Bad_Name" => "^1.0.0"}}, @env)

    assert {:error, %{id: "PKG001", details: %{name: "a"}}} =
             Deps.resolve(%{dependencies: %{"a" => ">= 1.0.0"}}, @env)
  end

  @tag obligations: ~w(PK-OBL-003)
  test "the SemVer grammar and precedence are exact including pre-releases" do
    assert {:ok, %{major: 1, minor: 2, patch: 3, pre: [], build: nil}} =
             Deps.parse_version("1.2.3")

    assert {:ok, %{pre: ["alpha", 1]}} = Deps.parse_version("1.0.0-alpha.1")
    assert {:ok, %{build: ["meta", "x"]}} = Deps.parse_version("1.2.3+meta.x")

    for bad <- ["1.2", "01.2.3", "1.2.3-", "1.2.3+", "", "1.2.3-alpha..1", "v1.2.3"] do
      assert {:error, %{id: "PKG001", details: %{version: ^bad}}} = Deps.parse_version(bad)
    end

    assert Deps.compare("1.0.0-alpha", "1.0.0") == :lt
    assert Deps.compare("1.0.0", "1.0.0-alpha") == :gt
    assert Deps.compare("1.0.0-alpha", "1.0.0-alpha.1") == :lt
    assert Deps.compare("1.0.0-alpha.beta", "1.0.0-alpha") == :gt
    assert Deps.compare("1.0.0-2", "1.0.0-10") == :lt
    assert Deps.compare("1.0.0-alpha", "1.0.0-beta") == :lt
    assert Deps.compare("1.0.0+b1", "1.0.0+b2") == :eq
    assert Deps.compare("1.0.0+b", "1.0.0") == :eq
    assert Deps.compare("2.0.0", "1.9.9") == :gt
  end

  @tag obligations: ~w(PK-OBL-005)
  test "exact/caret/tilde satisfaction follows the Cargo 0.x rule and pre-release restriction" do
    assert Deps.satisfies?("1.5.0", "^1.2.3")
    refute Deps.satisfies?("2.0.0", "^1.2.3")
    assert Deps.satisfies?("0.1.9", "^0.1.2")
    refute Deps.satisfies?("0.2.0", "^0.1.2")
    assert Deps.satisfies?("0.0.3", "^0.0.3")
    refute Deps.satisfies?("0.0.4", "^0.0.3")
    assert Deps.satisfies?("1.2.9", "~1.2.3")
    refute Deps.satisfies?("1.3.0", "~1.2.3")
    assert Deps.satisfies?("1.2.3", "1.2.3")
    refute Deps.satisfies?("1.2.4", "1.2.3")
    refute Deps.satisfies?("1.3.0-rc.1", "^1.2.3")
    refute Deps.satisfies?("1.2.5-beta", "^1.2.4-beta.1")
    assert Deps.satisfies?("1.2.4-beta.2", "^1.2.4-beta.1")
    {:ok, vx} = Deps.parse_version("1.2.3+x")
    {:ok, ry} = Deps.parse_requirement("1.2.3")
    assert Deps.satisfies?(vx, ry)
  end

  @tag obligations: ~w(PK-OBL-007 PK-OBL-009)
  test "cycles, conflicts, and unknown names reject with precise details" do
    pair_env = %{
      "a" => %{"1.0.0" => %{dependencies: %{"b" => "^1.0.0"}}},
      "b" => %{"1.0.0" => %{dependencies: %{"a" => "^1.0.0"}}}
    }

    assert {:error, %{id: "PKG002", details: %{cycle: cycle}}} =
             Deps.resolve(%{dependencies: %{"a" => "^1.0.0"}}, pair_env)

    assert cycle =~ "(cycle)"

    deep_env = %{
      "a" => %{"1.0.0" => %{dependencies: %{"b" => "^1.0.0"}}},
      "b" => %{"1.0.0" => %{dependencies: %{"c" => "^1.0.0"}}},
      "c" => %{"1.0.0" => %{dependencies: %{"a" => "^1.0.0"}}}
    }

    assert {:error, %{id: "PKG002"}} = Deps.resolve(%{dependencies: %{"a" => "^1.0.0"}}, deep_env)

    conflict_env = %{
      "a" => %{"1.0.0" => %{dependencies: %{"c" => "^1.0.0"}}},
      "b" => %{"0.5.0" => %{dependencies: %{"c" => "^2.0.0"}}},
      "c" => %{"1.0.0" => %{}, "2.0.0" => %{}}
    }

    assert {:error, %{id: "PKG003", details: %{name: "c", requirers: requirers}}} =
             Deps.resolve(@root, conflict_env)

    assert Enum.sort(requirers) == [
             %{requirement: "^1.0.0", requirer: "a"},
             %{requirement: "^2.0.0", requirer: "b"}
           ]

    assert {:error, %{id: "PKG004", details: %{name: "ghost", requirer: "<root>"}}} =
             Deps.resolve(%{dependencies: %{"ghost" => "^1.0.0"}}, @env)

    dup_env = %{"a" => %{"1.0.0" => %{}, "1.0.0+b" => %{}}}

    assert {:error, %{id: "PKG005", details: %{name: "a"}}} =
             Deps.resolve(%{dependencies: %{"a" => "^1.0.0"}}, dup_env)
  end

  @tag obligations: ~w(PK-OBL-008)
  test "resolution picks one highest-satisfying version per name, order-independently" do
    assert {:ok, resolved} = Deps.resolve(@root, @env)

    assert Enum.map(resolved, &{&1.name, &1.version}) == [
             {"a", "1.1.0"},
             {"b", "0.5.0"},
             {"c", "1.0.5"}
           ]

    permuted_env = %{"c" => @env["c"], "a" => @env["a"], "b" => @env["b"]}
    permuted_root = %{dependencies: %{"b" => "^0.5.0", "a" => "^1.0.0"}}

    assert {:ok, ^resolved} = Deps.resolve(permuted_root, permuted_env)

    wide_env = put_in(@env["c"], %{"1.0.0" => %{}, "1.0.5" => %{}, "1.1.0" => %{}})
    assert {:ok, wide} = Deps.resolve(%{dependencies: %{"c" => "^1.0.0"}}, wide_env)
    assert hd(wide).version == "1.1.0"

    assert {:ok, again} = Deps.resolve(@root, @env)
    assert resolved == again
  end

  @tag obligations: ~w(PK-OBL-006 PK-OBL-010)
  test "bundle digests are registry-neutral and lockfiles byte-deterministic" do
    pkg = %{
      name: "web",
      version: "1.0.0",
      dependencies: %{},
      selection: %{edition: "0.1"},
      modules: ["M"],
      interfaces: ["M.cati.json"],
      roots: ["m"],
      output: "priv",
      interface_digests: ["aaa", "bbb"],
      component_digests: ["ccc"]
    }

    reordered = %{
      component_digests: ["ccc"],
      interface_digests: ["bbb", "aaa"],
      output: "priv",
      roots: ["m"],
      interfaces: ["M.cati.json"],
      modules: ["M"],
      dependencies: %{},
      selection: %{edition: "0.1"},
      version: "1.0.0",
      name: "web"
    }

    assert Deps.bundle_digest(pkg) == Deps.bundle_digest(reordered)
    assert byte_size(Deps.bundle_digest(pkg)) == 64

    assert {:ok, resolved} = Deps.resolve(@root, @env)

    root_sel = %{edition: "0.1", language_revision: "0.1.20", previews: []}
    lock1 = Deps.generate_lockfile(resolved, root: root_sel)

    lock2 =
      Deps.generate_lockfile(Enum.reverse(resolved),
        root: %{previews: [], language_revision: "0.1.20", edition: "0.1"}
      )

    assert lock1 == lock2
    assert lock1 == Deps.generate_lockfile(resolved, root: root_sel)

    {:ok, parsed} = JSON.decode(lock1)
    assert parsed["format"] == "catena.lock"
    assert length(parsed["packages"]) == 3
    c = Enum.find(parsed["packages"], &(&1["name"] == "c"))
    assert c["interface_digests"] == Enum.sort(c["interface_digests"] || [])
  end

  @tag obligations: ~w(PK-OBL-011)
  test "lockfiles replay as exact pins and reject stale and tampered states" do
    assert {:ok, resolved} = Deps.resolve(@root, @env)
    root_sel = %{edition: "0.1", language_revision: "0.1.20", previews: []}
    lock = Deps.generate_lockfile(resolved, root: root_sel)

    assert {:ok, pinned} = Deps.replay_lockfile(@root, lock)
    assert Enum.map(pinned, & &1.name) == ["a", "b", "c"]
    assert {:ok, ^pinned} = Deps.replay_lockfile(@root, lock)

    stale = %{dependencies: Map.put(@root.dependencies, "a", "^2.0.0")}

    assert {:error, %{id: "PKG005", details: %{reason: "stale_lockfile"}}} =
             Deps.replay_lockfile(stale, lock)

    added = %{dependencies: Map.put(@root.dependencies, "ghost", "^1.0.0")}

    assert {:error, %{id: "PKG005", details: %{reason: "stale_lockfile", missing: ["ghost"]}}} =
             Deps.replay_lockfile(added, lock)

    assert {:error,
            %{id: "PKG005", details: %{reason: "tampered_lockfile", packages: ["a", "b", "c"]}}} =
             Deps.replay_lockfile(@root, lock, fn _, _, _ -> false end)

    assert {:error, %{id: "PKG001"}} = Deps.replay_lockfile(@root, "{\"format\": \"nope\"}")
  end

  @tag obligations: ~w(PK-OBL-002 PK-OBL-006 PK-OBL-012)
  test "the manifest decoder carries dependencies and joint digests flow into locks" do
    manifest = %{
      "format" => "catena-package-manifest",
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "package" => "web",
      "companion_module" => "Web",
      "modules" => [
        %{"source" => "web.json", "beam" => "Web.beam", "interface" => "Web.cati.json"}
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "Web.beam",
      "assurance" => "assurance.json",
      "dependencies" => %{"json" => "^1.0.0"}
    }

    assert {:ok, decoded} = Catena.Package.Manifest.decode(JSON.encode!(manifest))
    assert decoded.dependencies == %{"json" => "^1.0.0"}

    plain = Map.delete(manifest, "dependencies")
    assert {:ok, _} = Catena.Package.Manifest.decode(JSON.encode!(plain))

    {:ok, resolved} = Deps.resolve(@root, @env)
    enriched = put_in(@env["c"]["1.0.5"], %{component_digests: ["joint-abc"]})
    {:ok, with_scc} = Deps.resolve(@root, enriched)
    lock = Deps.generate_lockfile(with_scc)
    {:ok, parsed} = JSON.decode(lock)
    c = Enum.find(parsed["packages"], &(&1["name"] == "c"))
    assert c["component_digests"] == ["joint-abc"]
    assert resolved != with_scc || c["bundle_digest"] != nil
  end
end
