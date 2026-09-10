defmodule Catena.Release.ReadinessTest do
  use ExUnit.Case, async: true

  alias Catena.Release.Readiness
  alias Catena.OTP.Profile

  defp fields(overrides \\ %{}) do
    fingerprint = hd(Profile.supported())

    Map.merge(
      %{
        "release_class" => "experimental",
        "normative_chapters" => [
          %{
            "id" => "C139",
            "version" => "0.1.90",
            "digest" => String.duplicate("1", 64),
            "status" => "normative"
          }
        ],
        "obligations" => %{"total" => 4, "traced" => 2, "partial" => 1, "untraced" => 1},
        "platforms" => [
          %{
            "fingerprint" => fingerprint,
            "digest" => Profile.digest(fingerprint),
            "status" => "supported"
          }
        ],
        "evidence" => %{
          "compatibility" => %{"digest" => String.duplicate("2", 64), "status" => "pass"},
          "performance" => %{"digest" => String.duplicate("3", 64), "status" => "pass"},
          "source_tooling" => %{"digest" => String.duplicate("4", 64), "status" => "blocked"},
          "usability" => %{"digest" => String.duplicate("5", 64), "status" => "blocked"}
        },
        "open_items" => ~w(G118 G120 G123 G137 P096 P107 P109 P117 P119 P125),
        "limitations" => [
          %{"id" => "public-vocabulary-held", "status" => "open", "disposition" => "P107/P109"}
        ],
        "proofs" => [Readiness.proof_workbench()],
        "contradictions" => [
          %{
            "id" => "abstraction-invariant-boundary",
            "status" => "resolved",
            "disposition" => "typed-dynamic-validation"
          },
          %{
            "id" => "package-identifier-underscore",
            "status" => "resolved",
            "disposition" => "hyphen-only-package-name"
          }
        ]
      },
      overrides
    )
  end

  test "a disclosed experimental evidence bundle is ready only for the experimental claim" do
    assert {:ok, manifest} = Readiness.build(fields())
    assert :ok = Readiness.verify(manifest)

    assert {:ok,
            %{
              status: :ready,
              release_class: :experimental,
              blockers: [],
              claims: ["experimental-evidence-bundle"],
              publication_performed: false,
              automatic_upgrade_performed: false
            }} = Readiness.assess(manifest)
  end

  test "the readiness contract is versioned and published in conformance metadata" do
    assert Catena.LanguageVersion.introduced(:release_readiness) == "0.1.90"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("release-readiness", "0.1.90")

    profile = Catena.ConformanceInfo.document()["release_readiness"]
    assert profile["version"] == "0.1.90"
    assert profile["automatic_class_upgrade"] == false
    assert profile["publication"] == "explicit_external_action"
  end

  test "complete and stable claims stay blocked until every gate and integrated proof exists" do
    assert {:ok, manifest} = Readiness.build(fields(%{"release_class" => "complete"}))
    assert {:ok, result} = Readiness.assess(manifest)

    assert result.status == :blocked
    assert result.claims == []
    assert "completion-gates" in result.blockers
    assert "obligation-coverage" in result.blockers
    assert "required-evidence" in result.blockers
    assert "integrated-catena-proof" in result.blockers
  end

  test "forged proof records, unsupported hosts, and unresolved contradictions block readiness" do
    forged = Map.put(Readiness.proof_workbench(), "source_digest", String.duplicate("0", 64))

    unsupported = %{
      "fingerprint" => %{"otp" => "unknown"},
      "digest" => "unknown",
      "status" => "supported"
    }

    unresolved = [
      %{"id" => "abstraction-invariant-boundary", "status" => "open", "disposition" => "pending"},
      %{
        "id" => "package-identifier-underscore",
        "status" => "resolved",
        "disposition" => "hyphen-only-package-name"
      }
    ]

    assert {:ok, manifest} =
             Readiness.build(
               fields(%{
                 "proofs" => [forged],
                 "platforms" => [unsupported],
                 "contradictions" => unresolved
               })
             )

    assert {:ok, result} = Readiness.assess(manifest)

    assert Enum.sort(result.blockers) ==
             ~w(bounded-proof contradiction-audit unsupported-platform)
  end

  test "a modified manifest fails digest verification" do
    assert {:ok, manifest} = Readiness.build(fields())
    tampered = Map.put(manifest, "open_items", [])
    assert {:error, :release_manifest_digest_mismatch} = Readiness.verify(tampered)
  end

  test "a manifest cannot self-admit an integrated proof" do
    asserted = %{
      "checker" => "The Rocq Prover 9.2",
      "container_digest" => String.duplicate("a", 64),
      "scope" => "integrated-catena",
      "source_digest" => String.duplicate("b", 64),
      "status" => "verified",
      "theorems" => ["claimed_integrated_theorem"],
      "registry_status" => "admitted"
    }

    complete =
      fields(%{
        "release_class" => "complete",
        "open_items" => [],
        "obligations" => %{"total" => 4, "traced" => 4, "partial" => 0, "untraced" => 0},
        "proofs" => [Readiness.proof_workbench(), asserted],
        "evidence" =>
          Map.new(fields()["evidence"], fn {key, evidence} ->
            {key, Map.put(evidence, "status", "pass")}
          end)
      })

    assert {:ok, manifest} = Readiness.build(complete)

    assert {:ok, %{status: :blocked, blockers: ["integrated-catena-proof"]}} =
             Readiness.assess(manifest)
  end

  test "malformed obligation totals and incomplete audit inventories are rejected" do
    assert {:error, :invalid_release_manifest} =
             Readiness.build(
               fields(%{
                 "obligations" => %{"total" => 4, "traced" => 3, "partial" => 0, "untraced" => 0}
               })
             )

    assert {:error, :invalid_release_manifest} =
             Readiness.build(fields(%{"contradictions" => []}))
  end

  test "duplicate inventory identities and noncanonical manifests are rejected" do
    [limitation] = fields()["limitations"]

    assert {:error, :invalid_release_manifest} =
             Readiness.build(fields(%{"limitations" => [limitation, limitation]}))

    assert {:ok, manifest} = Readiness.build(fields())
    noncanonical = Map.update!(manifest, "open_items", &Enum.reverse/1)
    payload = Map.delete(noncanonical, "digest")
    forged_digest = Catena.CanonicalJCS.digest(payload)

    assert {:error, :invalid_release_manifest} =
             Readiness.verify(Map.put(noncanonical, "digest", forged_digest))
  end

  test "any declared open item blocks a complete claim" do
    complete =
      fields(%{
        "release_class" => "complete",
        "open_items" => ["LOCAL-REVIEW"],
        "obligations" => %{"total" => 4, "traced" => 4, "partial" => 0, "untraced" => 0},
        "evidence" =>
          Map.new(fields()["evidence"], fn {key, evidence} ->
            {key, Map.put(evidence, "status", "pass")}
          end)
      })

    assert {:ok, manifest} = Readiness.build(complete)
    assert {:ok, result} = Readiness.assess(manifest)
    assert "completion-gates" in result.blockers
  end
end
