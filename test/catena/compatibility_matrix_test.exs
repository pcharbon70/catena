defmodule Catena.CompatibilityMatrixTest do
  use ExUnit.Case, async: true

  alias Catena.Package.{CompatibilityMatrix, Deps}

  @old String.duplicate("a", 64)
  @new String.duplicate("b", 64)

  test "layered results keep pass, fail, and unsupported distinct" do
    cases = [
      case_("source-retained", "source", "compatible", %{"revision" => "0.1.8"}),
      case_("future-edition", "source", "incompatible", %{"revision" => "0.2.0"}),
      case_("unmeasured-host", "toolchain", "compatible", %{"fingerprint" => "future"})
    ]

    {:ok, matrix} = CompatibilityMatrix.define("bounded bootstrap fixtures", cases)

    adapters = %{
      "source" => fn %{"revision" => revision} ->
        if revision in Catena.LanguageVersion.all(),
          do: {:compatible, %{"accepted" => revision}},
          else: {:incompatible, %{"diagnostic" => "EDN001"}}
      end,
      "toolchain" => fn _ -> {:unsupported, "host fingerprint is outside the tested matrix"} end
    }

    assert {:ok, report} = CompatibilityMatrix.run(matrix, adapters)
    assert report["counts"] == %{"pass" => 2, "fail" => 0, "unsupported" => 1}
    assert Enum.map(report["results"], & &1["outcome"]) == ~w(pass pass unsupported)
    assert report == elem(CompatibilityMatrix.run(matrix, adapters), 1)
  end

  test "expectation mismatches fail without being relabelled unsupported" do
    {:ok, matrix} =
      CompatibilityMatrix.define("negative oracle", [
        case_("breaking-interface", "interface", "compatible", %{"change" => "remove"})
      ])

    assert {:ok, report} =
             CompatibilityMatrix.run(matrix, %{
               "interface" => fn _ -> {:incompatible, %{"class" => "breaking"}} end
             })

    assert report["counts"] == %{"pass" => 0, "fail" => 1, "unsupported" => 0}
  end

  test "missing and malformed adapters have distinct explicit outcomes" do
    {:ok, matrix} =
      CompatibilityMatrix.define("adapter protocol", [
        case_("malformed-adapter", "interface", "compatible", %{}),
        case_("missing-adapter", "data", "compatible", %{})
      ])

    assert {:ok, report} =
             CompatibilityMatrix.run(matrix, %{"interface" => fn _ -> :invalid_result end})

    assert Enum.map(report["results"], &{&1["id"], &1["outcome"], &1["observed"]}) == [
             {"malformed-adapter", "fail", "invalid-adapter-result"},
             {"missing-adapter", "unsupported", "unsupported"}
           ]

    assert report["counts"] == %{"pass" => 0, "fail" => 1, "unsupported" => 1}
  end

  test "wide and deep dependency graphs use the retained resolver and exact lock replay" do
    wide_names = for n <- 1..64, do: "p#{n}"
    wide_env = Map.new(wide_names, &{&1, %{"1.0.0" => %{dependencies: %{}}}})
    wide_root = %{dependencies: Map.new(wide_names, &{&1, "1.0.0"})}
    assert {:ok, wide} = Deps.resolve(wide_root, wide_env)
    assert length(wide) == 64

    deep_names = for n <- 1..48, do: "d#{n}"

    deep_env =
      deep_names
      |> Enum.with_index()
      |> Map.new(fn {name, index} ->
        dependencies =
          if index + 1 < length(deep_names),
            do: %{Enum.at(deep_names, index + 1) => "1.0.0"},
            else: %{}

        {name, %{"1.0.0" => %{dependencies: dependencies}}}
      end)

    assert {:ok, deep} = Deps.resolve(%{dependencies: %{"d1" => "1.0.0"}}, deep_env)
    lock = Deps.generate_lockfile(deep)

    assert {:ok, replayed} =
             Deps.replay_lockfile(%{dependencies: %{"d1" => "1.0.0"}}, lock, fn _, _, _ ->
               true
             end)

    assert Enum.map(replayed, & &1.name) == Enum.map(deep, & &1.name)
  end

  test "one bounded matrix exercises every compatibility layer through retained authorities" do
    message = "retained release"
    {public, private} = :crypto.generate_key(:eddsa, :ed25519)
    signature = :crypto.sign(:eddsa, :none, message, [private, :ed25519])

    cases = [
      case_("data-v1", "data", "compatible", %{"version" => "0.1.6"}),
      case_("dependency-lock", "dependency", "compatible", %{"root" => "app"}),
      case_("historical-signature", "historical-signature", "compatible", %{
        "message" => message,
        "public" => Base.encode16(public, case: :lower),
        "signature" => Base.encode16(signature, case: :lower)
      }),
      case_("interface-addition", "interface", "compatible", %{"change" => "add"}),
      case_("runtime-upgrade", "runtime-upgrade", "compatible", %{"generation" => 1}),
      case_("source-oldest", "source", "compatible", %{"revision" => "0.1.1"}),
      case_("source-newest", "source", "compatible", %{"revision" => "0.1.96"}),
      case_("toolchain-newest", "toolchain", "compatible", %{"edge" => "newest"}),
      case_("toolchain-oldest", "toolchain", "compatible", %{"edge" => "oldest"})
    ]

    {:ok, matrix} = CompatibilityMatrix.define("retained releases and generated graphs", cases)
    adapters = integration_adapters()

    assert {:ok, report} = CompatibilityMatrix.run(matrix, adapters)
    assert report["counts"] == %{"pass" => 9, "fail" => 0, "unsupported" => 0}
    assert Enum.all?(report["results"], &(&1["outcome"] == "pass"))
  end

  test "matrix validation rejects duplicate, malformed, oversized, and tampered cases" do
    valid = case_("one", "data", "compatible", %{})

    assert {:error, :invalid_compatibility_matrix} =
             CompatibilityMatrix.define("scope", [valid, valid])

    assert {:error, :invalid_compatibility_matrix} =
             CompatibilityMatrix.define("scope", [Map.put(valid, "layer", "combined")])

    assert {:error, :invalid_compatibility_matrix} =
             CompatibilityMatrix.define("scope", [
               Map.put(valid, "input", %{"x" => String.duplicate("x", 1_048_577)})
             ])

    {:ok, matrix} = CompatibilityMatrix.define("scope", [valid])

    assert {:error, :invalid_compatibility_matrix} =
             CompatibilityMatrix.verify(put_in(matrix["scope"], "changed"))
  end

  test "profile and lifecycle expose bounded matrix scope" do
    assert Catena.LanguageVersion.latest() == "0.1.96"
    assert CompatibilityMatrix.profile().ecosystem_wide_claim == false
    assert CompatibilityMatrix.profile().unsupported_is_failure == false
    assert Catena.ConformanceInfo.document()["compatibility_suite"]["version"] == "0.1.82"
  end

  defp case_(id, layer, expectation, input),
    do: %{"id" => id, "layer" => layer, "expectation" => expectation, "input" => input}

  defp integration_adapters do
    %{
      "source" => fn %{"revision" => revision} ->
        case Catena.LanguageVersion.resolve_selection(%{
               edition: "0.1",
               language_revision: revision,
               previews: []
             }) do
          {:ok, selection} -> {:compatible, %{"revision" => selection.language_revision}}
          {:error, diagnostic} -> {:incompatible, %{"diagnostic" => diagnostic.id}}
        end
      end,
      "interface" => fn _ ->
        {:ok, diff} =
          Catena.Package.Compat.diff_entries(
            [%{name: "main", launch: true, result: "Outcome"}],
            [
              %{name: "health", launch: false, result: "Outcome"},
              %{name: "main", launch: true, result: "Outcome"}
            ]
          )

        {:compatible, %{"class" => Atom.to_string(diff.class)}}
      end,
      "dependency" => fn _ ->
        env = %{"library" => %{"1.0.0" => %{dependencies: %{}}}}
        root = %{dependencies: %{"library" => "1.0.0"}}
        {:ok, resolved} = Deps.resolve(root, env)
        lock = Deps.generate_lockfile(resolved)
        {:ok, replayed} = Deps.replay_lockfile(root, lock, fn _, _, _ -> true end)
        {:compatible, %{"packages" => length(replayed)}}
      end,
      "data" => fn %{"version" => version} ->
        bytes = Catena.CanonicalJSON.encode(%{"version" => version, "decision" => "allow"})
        {:ok, {^version, "allow"}} = Catena.Artifact.Migration.replay(bytes, migration_ledger())
        {:compatible, %{"replayed" => version}}
      end,
      "toolchain" => fn %{"edge" => edge} ->
        profiles = Catena.OTP.Profile.supported()
        profile = if edge == "oldest", do: hd(profiles), else: List.last(profiles)
        observed = Catena.OTP.Profile.observe()
        :ok = Catena.OTP.Profile.validate(%{observed | fingerprint: profile})
        {:compatible, %{"fingerprint" => Catena.OTP.Profile.digest(profile)}}
      end,
      "historical-signature" => fn input ->
        if Catena.Governance.Crypto.verify(input["message"], input["public"], input["signature"]),
          do: {:compatible, %{"verified" => true}},
          else: {:incompatible, %{"verified" => false}}
      end,
      "runtime-upgrade" => fn _ ->
        {:ok, system} = Catena.Upgrade.new(@old, interface(), schema("s1", 1), %{generation: 1})
        {:ok, _validated} = Catena.Upgrade.preflight(system, upgrade_descriptor())
        {:compatible, %{"preflight" => "accepted"}}
      end
    }
  end

  defp migration_ledger do
    interpreters =
      Map.new(
        ~w(0.1.6 0.1.7 0.1.8),
        &{&1, fn doc, _ -> {:ok, {doc["version"], doc["decision"]}} end}
      )

    migrations = %{
      {"0.1.6", "0.1.7"} => fn doc -> {:ok, Map.put(doc, "version", "0.1.7"), []} end,
      {"0.1.7", "0.1.8"} => fn doc -> {:ok, Map.put(doc, "version", "0.1.8"), []} end
    }

    {:ok, ledger} = Catena.Artifact.Migration.ledger(interpreters, migrations)
    ledger
  end

  defp interface,
    do: %{origin: "test://compatibility", module: "Counter", values: [], types: []}

  defp schema(id, generation),
    do: %{id: id, validate: &match?(%{generation: ^generation}, &1)}

  defp upgrade_descriptor do
    %{
      old_artifact: @old,
      new_artifact: @new,
      new_interface: interface(),
      old_schema: schema("s1", 1),
      new_schema: schema("s2", 2),
      migrate: fn _ -> {:ok, %{generation: 2}} end,
      reverse: fn _ -> {:ok, %{generation: 1}} end,
      evidence_digest: String.duplicate("d", 64),
      nodes: [local: @old],
      max_state_bytes: 1024,
      max_migration_ms: 100
    }
  end
end
