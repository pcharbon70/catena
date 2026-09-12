defmodule Catena.Tool.UsabilityStudy do
  @moduledoc "Validated G137 pre-study packages without participant outreach or invented evidence."

  alias Catena.CanonicalJCS

  @format "catena-usability-prestudy"
  @package_version 1
  @gate "G137"
  @status "prepared-not-observed"
  @version "0.1.97"
  @task_families ~w(comprehension dependent-sequencing diagnostic-repair effect-handling guard-selection independent-combination single-context-mapping traversal)
  @metrics ~w(prediction-accuracy repair-success task-completion transfer-success)
  @strata ~w(functional general)
  @phases %{"pilot" => 6, "main" => 24}
  @materials ~w(README.md analysis-plan.md consent-and-data.md facilitator-guide.md semantic-task-book.md)

  def profile do
    %{
      gate: @gate,
      version: @version,
      package_format: @format,
      package_version: @package_version,
      status: :prepared_not_observed,
      evidence_status: :blocked_until_observed_human_study,
      outreach: :requires_separate_authorization,
      participant_data: :prohibited_in_package,
      public_vocabulary: :held_for_p107,
      public_source: :held_for_p109,
      phases: @phases,
      strata: @strata,
      task_families: @task_families,
      outcome_metrics: @metrics,
      material_identity: :sha256,
      threshold_policy: :pilot_then_preregister_before_main
    }
  end

  def validate(package) when is_map(package) do
    with true <- package["format"] == @format,
         true <- package["package_version"] == @package_version,
         true <- package["gate"] == @gate,
         true <- package["status"] == @status,
         true <- package["outreach_authorized"] == false,
         true <- package["participant_data"] == [],
         true <- package["results"] == [],
         true <- package["public_vocabulary"] == "held-for-p107",
         true <- package["public_source"] == "held-for-p109",
         true <- valid_phases?(package["phases"]),
         true <- exact_strings?(package["strata"], @strata),
         true <- exact_strings?(package["task_families"], @task_families),
         true <- exact_strings?(package["outcome_metrics"], @metrics),
         true <- valid_material_manifest?(package["materials"]),
         true <- valid_thresholds?(package["thresholds"]),
         true <- valid_data_policy?(package["data_policy"]),
         true <- valid_exclusions?(package["exclusions"]) do
      :ok
    else
      _ -> {:error, :invalid_usability_study_package}
    end
  rescue
    _ -> {:error, :invalid_usability_study_package}
  end

  def validate(_), do: {:error, :invalid_usability_study_package}

  def validate_materials(package, root) when is_binary(root) do
    with :ok <- validate(package),
         true <-
           Enum.all?(@materials, fn name ->
             case File.read(Path.join(root, name)) do
               {:ok, bytes} -> digest(bytes) == package["materials"][name]
               _ -> false
             end
           end) do
      :ok
    else
      _ -> {:error, :invalid_usability_study_materials}
    end
  end

  def validate_materials(_, _), do: {:error, :invalid_usability_study_materials}

  def evidence(package, root) do
    with :ok <- validate_materials(package, root) do
      {:ok,
       %{
         gate: @gate,
         status: :blocked,
         reason: :observed_human_evidence_absent,
         package_digest: CanonicalJCS.digest(package),
         results_admitted: false
       }}
    end
  end

  def assignment(package, phase, stratum, ordinal)
      when is_binary(phase) and is_binary(stratum) and is_integer(ordinal) do
    with :ok <- validate(package),
         size when is_integer(size) <- @phases[phase],
         true <- stratum in @strata,
         quota <- div(size, length(@strata)),
         true <- ordinal >= 1 and ordinal <= quota do
      offset = Enum.find_index(@strata, &(&1 == stratum))

      order =
        if rem(ordinal + offset, 2) == 1,
          do: ["semantic-foundation", "public-language"],
          else: ["public-language", "semantic-foundation"]

      {:ok,
       %{
         phase: phase,
         stratum: stratum,
         ordinal: ordinal,
         condition_order: order,
         execution_status: :held_for_p107_and_p109
       }}
    else
      _ -> {:error, :invalid_usability_assignment}
    end
  end

  def assignment(_, _, _, _), do: {:error, :invalid_usability_assignment}

  defp valid_phases?(phases) when is_list(phases) do
    normalized =
      Enum.map(phases, fn row ->
        {row["id"], row["participants"], row["purpose"]}
      end)

    normalized == [
      {"main", 24, "preregistered-confirmatory"},
      {"pilot", 6, "calibrate-materials-and-thresholds"}
    ]
  end

  defp valid_phases?(_), do: false

  defp valid_thresholds?(%{
         "status" => "calibrate-after-pilot-preregister-before-main",
         "prediction_accuracy" => nil,
         "repair_success" => nil,
         "task_completion" => nil,
         "transfer_success" => nil
       }),
       do: true

  defp valid_thresholds?(_), do: false

  defp valid_data_policy?(%{
         "collect" => collect,
         "exclude" => exclude,
         "raw_retention" => "none-after-verified-aggregation",
         "reporting" => "anonymous-aggregate-with-negative-results"
       }) do
    exact_strings?(collect, ~w(condition duration-band outcome-code stratum task-id)) and
      exact_strings?(exclude, ~w(contact-details free-text names source-recordings))
  end

  defp valid_data_policy?(_), do: false

  defp valid_exclusions?(rows) when is_list(rows) do
    ids = Enum.map(rows, & &1["id"])

    ids == ~w(consent-withdrawn duplicate-participation facilitator-protocol-failure) and
      Enum.all?(rows, fn row ->
        row["timing"] == "defined-before-main-study" and is_binary(row["rule"])
      end)
  end

  defp valid_exclusions?(_), do: false

  defp valid_material_manifest?(materials) when is_map(materials) do
    Enum.sort(Map.keys(materials)) == @materials and
      Enum.all?(materials, fn {_name, value} ->
        is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)
      end)
  end

  defp valid_material_manifest?(_), do: false

  defp exact_strings?(actual, expected),
    do: is_list(actual) and actual == Enum.sort(Enum.uniq(actual)) and actual == expected

  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
