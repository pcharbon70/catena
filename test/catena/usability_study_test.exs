defmodule Catena.UsabilityStudyTest do
  use ExUnit.Case, async: true

  alias Catena.Tool.UsabilityStudy

  @package Path.expand("../usability/study-package.json", __DIR__)
  @materials Path.dirname(@package)

  test "the prepared package validates but cannot claim human evidence" do
    package = package()
    assert :ok = UsabilityStudy.validate(package)

    assert {:ok,
            %{
              gate: "G137",
              status: :blocked,
              reason: :observed_human_evidence_absent,
              results_admitted: false,
              package_digest: digest
            }} = UsabilityStudy.evidence(package, @materials)

    assert byte_size(digest) == 64
  end

  test "every study material is bound by exact SHA-256 identity" do
    package = package()
    assert :ok = UsabilityStudy.validate_materials(package, @materials)

    altered = put_in(package, ["materials", "analysis-plan.md"], String.duplicate("0", 64))

    assert {:error, :invalid_usability_study_materials} =
             UsabilityStudy.validate_materials(altered, @materials)
  end

  test "invented results and participant data invalidate the package" do
    assert {:error, :invalid_usability_study_package} =
             package()
             |> Map.put("results", [%{"prediction_accuracy" => 1.0}])
             |> UsabilityStudy.validate()

    assert {:error, :invalid_usability_study_package} =
             package()
             |> Map.put("participant_data", [%{"name" => "invented"}])
             |> UsabilityStudy.validate()
  end

  test "outreach and public-language conditions remain held" do
    assert {:error, :invalid_usability_study_package} =
             package() |> Map.put("outreach_authorized", true) |> UsabilityStudy.validate()

    assert {:error, :invalid_usability_study_package} =
             package() |> Map.put("public_source", "selected") |> UsabilityStudy.validate()
  end

  test "pilot and main schedules counterbalance both programmer strata" do
    for {phase, quota} <- [{"pilot", 3}, {"main", 12}], stratum <- ~w(functional general) do
      orders =
        for ordinal <- 1..quota do
          assert {:ok, assignment} = UsabilityStudy.assignment(package(), phase, stratum, ordinal)
          assert assignment.execution_status == :held_for_p107_and_p109
          assignment.condition_order
        end

      counts = Enum.frequencies(orders)
      assert Enum.max(Map.values(counts)) - Enum.min(Map.values(counts)) <= 1
    end
  end

  test "assignment rejects out-of-cohort positions and unknown strata" do
    assert {:error, :invalid_usability_assignment} =
             UsabilityStudy.assignment(package(), "pilot", "general", 4)

    assert {:error, :invalid_usability_assignment} =
             UsabilityStudy.assignment(package(), "main", "unknown", 1)
  end

  test "the conformance profile publishes the honest preparation boundary" do
    profile = Catena.ConformanceInfo.document()["usability_study"]
    assert profile["gate"] == "G137"
    assert profile["status"] == "prepared_not_observed"
    assert profile["evidence_status"] == "blocked_until_observed_human_study"
    assert profile["public_vocabulary"] == "held_for_p107"
    assert profile["public_source"] == "held_for_p109"
    assert profile["phases"] == %{"main" => 24, "pilot" => 6}
  end

  defp package do
    @package |> File.read!() |> JSON.decode!()
  end
end
