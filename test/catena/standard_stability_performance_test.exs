defmodule Catena.StandardStabilityPerformanceTest do
  use ExUnit.Case, async: true

  alias Catena.{LanguageLifecycle, LanguageVersion}
  alias Catena.Standard.Contract

  @tag obligations: ~w(SP-OBL-001 SP-OBL-002 SP-OBL-003 SP-OBL-004)
  test "inventory is complete, deterministic, and representation neutral" do
    inventory = Contract.inventory()
    assert :ok = Contract.verify_inventory(inventory)
    assert inventory == Contract.inventory()
    assert length(inventory["operations"]) == Contract.profile().operation_count
    assert Enum.all?(inventory["operations"], &(&1["representation"] == "none"))
    assert Enum.all?(inventory["operations"], &(&1["stack"] == "constant"))

    tampered = put_in(inventory["operations"], Enum.reverse(inventory["operations"]))
    assert {:error, :invalid_standard_contract_inventory} = Contract.verify_inventory(tampered)
  end

  @tag obligations: ~w(SP-OBL-005 SP-OBL-006 SP-OBL-007 SP-OBL-008)
  test "compatible replacements preserve or improve every published bound" do
    old = operation("collections.lookup")
    assert {:compatible, []} = Contract.compare(old, old)

    improved =
      old
      |> Map.put("time", "logarithmic")
      |> Map.put("space", "constant")

    assert {:compatible, []} = Contract.compare(old, improved)

    relaxed = Map.put(old, "requirements", [])
    assert {:compatible, []} = Contract.compare(old, relaxed)
  end

  @tag obligations: ~w(SP-OBL-009 SP-OBL-010 SP-OBL-011 SP-OBL-012)
  test "semantic drift and performance regressions are breaking" do
    old = operation("collections.map-values")

    mutations = [
      {"callbacks", "once-per-collision", :callback_multiplicity_changed},
      {"order", "none", :order_changed},
      {"failures", "host-exception", :failure_contract_changed},
      {"time", "quadratic", :time_complexity_regressed},
      {"requirements", ["nonempty"], :input_requirements_strengthened}
    ]

    for {field, value, reason} <- mutations do
      assert {:breaking, reasons} = Contract.compare(old, Map.put(old, field, value))
      assert reason in reasons
    end
  end

  @tag obligations: ~w(SP-OBL-013 SP-OBL-014 SP-OBL-015)
  test "observations bind operation, contract, toolchain, and work units" do
    toolchain = %{
      "architecture" => "x86_64",
      "compiler" => "catena-test",
      "os" => "linux",
      "otp" => "29"
    }

    samples = [
      %{"size" => 0, "work_units" => 0, "elapsed_ns" => 100},
      %{"size" => 1_000, "work_units" => 1_000, "elapsed_ns" => 2_000}
    ]

    assert {:ok, observation} =
             Contract.observation("collections.map-values", toolchain, samples)

    assert observation["portable_claim"] == false
    assert observation["contract"] == Contract.inventory()["digest"]
    assert :ok = Contract.verify_observation(observation)

    assert {:error, :invalid_performance_observation} =
             Contract.observation("collections.map-values", toolchain, Enum.reverse(samples))

    assert {:error, :invalid_performance_observation} =
             Contract.verify_observation(put_in(observation["samples"], [hd(samples)]))
  end

  @tag obligations: ~w(SP-OBL-016 SP-OBL-017 SP-OBL-018)
  test "revision and conformance profile disclose the policy boundary" do
    assert LanguageVersion.latest() == "0.1.95"
    assert LanguageVersion.introduced(:standard_stability_and_performance) == "0.1.87"

    assert {:ok, :stable} =
             LanguageLifecycle.state("standard-stability-and-performance", "0.1.87")

    profile = Catena.ConformanceInfo.document()["standard_stability_and_performance"]
    assert profile["version"] == "0.1.87"
    assert profile["stable_beam_abi"] == false
    assert profile["portable_wall_clock_bounds"] == false
    assert profile["benchmark_results_are_normative"] == false
    assert profile["public_source"] == "held_for_p109"
  end

  defp operation(id),
    do: Enum.find(Contract.inventory()["operations"], &(&1["id"] == id))
end
