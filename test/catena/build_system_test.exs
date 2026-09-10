defmodule Catena.BuildSystemTest do
  use ExUnit.Case, async: true

  alias Catena.Package.{Build, Reproducible}

  @digest String.duplicate("1", 64)
  @capabilities String.duplicate("2", 64)
  @toolchain String.duplicate("3", 64)

  test "project discovery has explicit deterministic precedence" do
    assert {:ok, "catena.project.json"} =
             Build.discover(["apps/a/catena.project.json", "catena.project.json"])

    assert {:ok, "apps/a/catena.project.json"} =
             Build.discover(["apps/a/catena.project.json"], "apps/a/catena.project.json")

    assert {:error, :ambiguous_project_discovery} =
             Build.discover(["apps/a/catena.project.json", "apps/b/catena.project.json"])
  end

  test "diamond workspaces build dependencies before dependents exactly once" do
    packages = [
      package("app", [workspace("left"), workspace("right")]),
      package("core"),
      package("left", [workspace("core")]),
      package("right", [workspace("core")])
    ]

    assert {:ok, plan} = Build.plan(packages, "release", @toolchain)
    assert plan["order"] == ~w(core left right app)

    parent = self()

    builder = fn package, inputs ->
      send(parent, {:built, package["name"], Map.keys(inputs)})
      archive(package["name"], package["input_digest"])
    end

    assert {:ok, first} = Build.build_offline(plan, %{}, builder)
    assert_receive {:built, "core", []}
    assert_receive {:built, "left", [{:workspace, "core"}]}
    assert_receive {:built, "right", [{:workspace, "core"}]}
    assert_receive {:built, "app", [{:workspace, "left"}, {:workspace, "right"}]}

    assert {:ok, cached} =
             Build.build_offline(plan, first.cache, fn _, _ -> flunk("cache miss") end)

    assert cached.outputs == first.outputs
  end

  test "verified acquisition is atomic and offline builds never fetch" do
    alpha = "alpha bundle"
    beta = "beta bundle"
    requirements = [external("alpha", alpha), external("beta", beta)]
    {:ok, plan} = Build.plan([package("app", requirements)], "development", @toolchain)

    interrupted = fn
      %{"package" => "alpha"} -> {:ok, alpha}
      %{"package" => "beta"} -> {:error, :interrupted}
    end

    assert {:error, :build_acquisition_failed, %{}} = Build.acquire(plan, %{}, interrupted)
    assert {:error, :offline_build_failed} = Build.build_offline(plan, %{}, fn _, _ -> :never end)

    assert {:ok, cache} =
             Build.acquire(plan, %{}, fn requirement ->
               {:ok, if(requirement["package"] == "alpha", do: alpha, else: beta)}
             end)

    assert {:ok, result} =
             Build.build_offline(plan, cache, fn package, inputs ->
               assert map_size(inputs) == 2
               archive(package["name"], package["input_digest"])
             end)

    assert Map.has_key?(result.outputs, "app")
  end

  test "corrupt cache, cycles, generator escapes, and profile switches are explicit" do
    assert {:error, :invalid_build_workspace} =
             Build.plan(
               [package("a", [workspace("b")]), package("b", [workspace("a")])],
               "test",
               @toolchain
             )

    invalid =
      package("app") |> Map.put("generators", [%{"inputs" => ["src/a"], "output" => "../hidden"}])

    assert {:error, :invalid_build_workspace} = Build.plan([invalid], "test", @toolchain)
    assert {:ok, development} = Build.plan([package("app")], "development", @toolchain)
    assert {:ok, release} = Build.plan([package("app")], "release", @toolchain)
    refute development["packages"]["app"]["cache_key"] == release["packages"]["app"]["cache_key"]

    assert {:error, :offline_build_failed} =
             Build.build_offline(development, %{@digest => "wrong"}, fn _, _ -> :never end)
  end

  test "declared generators and capabilities participate in cache identity" do
    generator = %{"inputs" => ["schema/model"], "output" => "generated/model"}
    one = package("app") |> Map.put("generators", [generator])
    two = Map.put(one, "capability_digest", String.duplicate("4", 64))
    assert {:ok, one_plan} = Build.plan([one], "test", @toolchain)
    assert {:ok, two_plan} = Build.plan([two], "test", @toolchain)
    refute one_plan["packages"]["app"]["cache_key"] == two_plan["packages"]["app"]["cache_key"]
  end

  test "workspace dependency identity invalidates every dependent cache key" do
    graph = [package("app", [workspace("core")]), package("core")]
    changed = List.update_at(graph, 1, &Map.put(&1, "input_digest", String.duplicate("5", 64)))
    assert {:ok, first} = Build.plan(graph, "test", @toolchain)
    assert {:ok, second} = Build.plan(changed, "test", @toolchain)
    refute first["packages"]["core"]["cache_key"] == second["packages"]["core"]["cache_key"]
    refute first["packages"]["app"]["cache_key"] == second["packages"]["app"]["cache_key"]
  end

  test "the retained compiler path produces identical clean and cached package output" do
    source = %{
      "version" => "0.1.6",
      "origin" => "pkg://build/main",
      "module" => "BuildMain",
      "exports" => ["main"],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "integer", "value" => 42}
        }
      ]
    }

    manifest = %{
      "format" => "catena-package-manifest",
      "version" => "0.1.6",
      "package" => "build",
      "profile" => "static",
      "companion_module" => "BuildCompanion",
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

    {:ok, reproducible} =
      Reproducible.plan(
        %{"package.json" => JSON.encode!(manifest), "src/main.json" => JSON.encode!(source)},
        "package.json"
      )

    package =
      package("build")
      |> Map.merge(%{
        "input_digest" => reproducible["digest"],
        "reproducible_plan" => reproducible
      })

    {:ok, plan} = Build.plan([package], "release", @toolchain)
    root = Path.join(System.tmp_dir!(), "catena-workspace-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf(root) end)

    assert {:ok, clean} = Build.build_retained(plan, %{}, root)
    assert {:ok, cached} = Build.build_retained(plan, clean.cache, root)
    assert clean.outputs == cached.outputs
  end

  test "published output is verified and a failed replacement preserves the destination" do
    {:ok, good} = archive("app", @digest)

    destination =
      Path.join(System.tmp_dir!(), "catena-build-#{System.unique_integer([:positive])}.archive")

    on_exit(fn -> File.rm(destination) end)
    assert :ok = Build.publish(good, destination)
    assert File.read!(destination) == good
    assert {:error, :build_output_publish_failed} = Build.publish("corrupt", destination)
    assert File.read!(destination) == good
  end

  test "conformance exposes the complete retained-input build profile" do
    assert Catena.LanguageVersion.latest() == "0.1.92"
    info = Catena.ConformanceInfo.document()
    assert info["build_system"]["version"] == "0.1.81"
    assert info["build_system"]["network_during_build"] == false
  end

  defp package(name, dependencies \\ []) do
    %{
      "name" => name,
      "dependencies" => dependencies,
      "input_digest" => @digest,
      "capability_digest" => @capabilities,
      "generators" => [],
      "reproducible_plan" => %{"digest" => @digest}
    }
  end

  defp workspace(name), do: %{"workspace" => name}

  defp external(name, bytes),
    do: %{"package" => name, "version" => "1.0.0", "digest" => hash(bytes)}

  defp archive(name, input),
    do: Reproducible.archive(%{"artifacts/#{name}.beam" => "compiled #{name}"}, input)

  defp hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
