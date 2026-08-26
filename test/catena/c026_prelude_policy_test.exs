defmodule Catena.C026PreludePolicyTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Package.Deps}

  @events [
    %{
      event: :provide_module,
      module: "Core",
      digest: "dc",
      exports: [
        %{category: :values, spelling: "answer"},
        %{category: :values, spelling: "shared"}
      ]
    },
    %{
      event: :provide_module,
      module: "Other",
      digest: "dx",
      exports: [%{category: :values, spelling: "shared"}]
    }
  ]

  @tag obligations: ~w(PL-OBL-001 PL-OBL-010)
  test "0.1.22 is an exact registered revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.29"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29)

    refute "0.1.22" in LanguageVersion.compilable_revisions()
    refute "0.1.22" in LanguageVersion.artifact_versions()
    refute "0.1.22" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("prelude-policy", "0.1.22")

    change = Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-22-prelude-policy"))

    assert String.contains?(
             change["specification"],
             "prelude-policy/prelude-selection-and-admission.md#"
           )

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, _} = Catena.build_namespace_environment([])
    assert {:ok, _} = Catena.compile_scc([])

    refute function_exported?(Catena, :default_prelude, 0)
    refute function_exported?(Catena.Package.Deps, :fetch_prelude, 1)
  end

  @tag obligations: ~w(PL-OBL-002 PL-OBL-007 PL-OBL-008)
  test "absent/null opt-out guarantees zero implicit names" do
    assert {:ok, env} = Catena.build_namespace_environment(@events, current_module: "App")

    assert {:error, %{id: "NSP003"}} =
             Catena.resolve_name(env, %{category: :values, spelling: "answer"})

    assert {:ok, _} =
             Catena.build_namespace_environment(@events, current_module: "App", prelude: nil)

    for missing_module <- ["Ghost", ""] do
      assert {:error, %{id: "PKG004"}} =
               Catena.build_namespace_environment(@events,
                 prelude: %{package: missing_module, requirement: "^1.0.0"}
               )
    end
  end

  @tag obligations: ~w(PL-OBL-003)
  test "malformed selections reject as PRE001 with the offending shape" do
    for bad <- ["oops", 42, %{}, %{"package" => "core"}, %{"requirement" => "^1.0.0"}] do
      assert {:error, %{id: "PRE001", details: %{reason: reason}}} =
               Catena.build_namespace_environment(@events, prelude: bad)

      assert reason in ["malformed_prelude", "malformed_prelude_requirement"]
    end

    assert {:error, %{id: "PRE001", details: %{reason: "malformed_prelude_requirement"}}} =
             Catena.build_namespace_environment(@events,
               prelude: %{package: "Core", requirement: ">= 1.0.0"}
             )
  end

  @tag obligations: ~w(PL-OBL-004 PL-OBL-006)
  test "the prelude origin admits names at ordinary import precedence" do
    assert {:ok, env} =
             Catena.build_namespace_environment(@events,
               current_module: "App",
               prelude: %{package: "Core", requirement: "^1.0.0"}
             )

    assert {:ok, resolution} = Catena.resolve_name(env, %{category: :values, spelling: "answer"})
    assert resolution.origin == "Core"

    local_events = [%{event: :declare, category: :values, spelling: "answer"} | @events]

    assert {:ok, local_env} =
             Catena.build_namespace_environment(local_events,
               current_module: "App",
               prelude: %{package: "Core", requirement: "^1.0.0"}
             )

    assert {:ok, local_res} =
             Catena.resolve_name(local_env, %{category: :values, spelling: "answer"})

    assert local_res.origin == nil

    collision_events =
      @events ++
        [%{event: :import_module, module: "Other", digest: "dx", names: [values: "shared"]}]

    assert {:ok, coll_env} =
             Catena.build_namespace_environment(collision_events,
               current_module: "App",
               prelude: %{package: "Core", requirement: "^1.0.0"}
             )

    assert {:error, %{id: "NSP004", details: %{origins: origins}}} =
             Catena.resolve_name(coll_env, %{category: :values, spelling: "shared"})

    assert Enum.sort(origins) == ["Core", "Other"]

    qualified =
      Catena.resolve_name(coll_env, %{
        category: :values,
        spelling: "Other.shared",
        qualified: true
      })

    assert {:ok, _} = qualified
  end

  @tag obligations: ~w(PL-OBL-005)
  test "the prelude selection resolves, locks, and replays as a dependency" do
    env_map = %{"core" => %{"1.0.0" => %{}, "1.2.0" => %{}}, "other" => %{"0.5.0" => %{}}}

    root = %{
      dependencies: %{"other" => "^0.5.0"},
      prelude: %{"package" => "core", "requirement" => "^1.0.0"}
    }

    assert {:ok, resolved} = Deps.resolve(root, env_map)
    assert Enum.map(resolved, &{&1.name, &1.version}) == [{"core", "1.2.0"}, {"other", "0.5.0"}]

    lock = Deps.generate_lockfile(resolved)
    assert lock == Deps.generate_lockfile(Enum.reverse(resolved))
    assert {:ok, _pinned} = Deps.replay_lockfile(root, lock)

    assert {:error, %{id: "PKG001"}} =
             Deps.resolve(
               %{dependencies: %{}, prelude: %{"package" => "core", "requirement" => ">= 1"}},
               env_map
             )

    unknown_env = %{"other" => %{"0.5.0" => %{}}}

    assert {:error, %{id: "PKG004"}} =
             Deps.resolve(root, unknown_env)
  end

  @tag obligations: ~w(PL-OBL-002 PL-OBL-009)
  test "the manifest decoder validates the prelude field" do
    base = %{
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
      "assurance" => "assurance.json"
    }

    with_prelude =
      Map.put(base, "prelude", %{"package" => "core", "requirement" => "^1.0.0"})

    assert {:ok, decoded} = Catena.Package.Manifest.decode(JSON.encode!(with_prelude))
    assert decoded.prelude == %{"package" => "core", "requirement" => "^1.0.0"}

    assert {:ok, plain} = Catena.Package.Manifest.decode(JSON.encode!(base))
    assert plain.prelude == nil

    for bad <- ["oops", %{"package" => "core"}] do
      assert {:error, %{id: "PRE001"}} =
               Catena.Package.Manifest.decode(JSON.encode!(Map.put(base, "prelude", bad)))
    end
  end

  @tag obligations: ~w(PL-OBL-001 PL-OBL-010)
  test "the wiring is deterministic and adds no phase beyond existing boundaries" do
    opts = [current_module: "App", prelude: %{package: "Core", requirement: "^1.0.0"}]

    first = Catena.build_namespace_environment(@events, opts)
    assert first == Catena.build_namespace_environment(@events, opts)

    {:ok, env} = first
    assert {:ok, r} = Catena.resolve_name(env, %{category: :values, spelling: "answer"})
    assert Catena.resolve_name(env, %{category: :values, spelling: "answer"}) == {:ok, r}

    refute function_exported?(Catena.Namespace, :parse_source, 1)
    refute function_exported?(Catena.Package.Deps, :fetch, 2)
    refute function_exported?(Catena.Package.Deps, :scaffold, 1)
  end
end
