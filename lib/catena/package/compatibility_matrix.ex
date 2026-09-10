defmodule Catena.Package.CompatibilityMatrix do
  @moduledoc "Bounded layered compatibility matrices with explicit unsupported outcomes."

  alias Catena.CanonicalJCS

  @version "0.1.82"
  @layers ~w(source interface dependency data toolchain historical-signature runtime-upgrade)
  @expectations ~w(compatible incompatible)
  @max_cases 4096
  @max_case_bytes 1_048_576

  def profile do
    %{
      version: @version,
      layers: @layers,
      outcomes: ~w(pass fail unsupported),
      cases: @max_cases,
      case_bytes: @max_case_bytes,
      ecosystem_wide_claim: false,
      unsupported_is_failure: false
    }
  end

  def define(scope, cases) when is_binary(scope) and is_list(cases) do
    with true <- scope != "" and byte_size(scope) <= 512,
         true <- length(cases) in 1..@max_cases,
         true <- Enum.all?(cases, &case?/1),
         ids <- Enum.map(cases, & &1["id"]),
         true <- ids == Enum.uniq(ids) do
      payload = %{
        "format" => "catena-compatibility-matrix",
        "version" => @version,
        "scope" => scope,
        "cases" => Enum.sort_by(cases, & &1["id"])
      }

      {:ok, Map.put(payload, "digest", CanonicalJCS.digest(payload))}
    else
      _ -> {:error, :invalid_compatibility_matrix}
    end
  rescue
    _ -> {:error, :invalid_compatibility_matrix}
  end

  def define(_, _), do: {:error, :invalid_compatibility_matrix}

  def run(matrix, adapters) when is_map(adapters) do
    with :ok <- verify(matrix) do
      results = Enum.map(matrix["cases"], &evaluate(&1, adapters))
      counts = Enum.frequencies_by(results, & &1["outcome"])

      payload = %{
        "format" => "catena-compatibility-report",
        "version" => @version,
        "matrix" => matrix["digest"],
        "scope" => matrix["scope"],
        "counts" => Map.merge(%{"pass" => 0, "fail" => 0, "unsupported" => 0}, counts),
        "results" => results
      }

      {:ok, Map.put(payload, "digest", CanonicalJCS.digest(payload))}
    end
  rescue
    _ -> {:error, :compatibility_matrix_failed}
  end

  def run(_, _), do: {:error, :compatibility_matrix_failed}

  def verify(%{"digest" => digest} = matrix) when is_binary(digest) do
    unsigned = Map.delete(matrix, "digest")

    with %{
           "format" => "catena-compatibility-matrix",
           "version" => @version,
           "scope" => scope,
           "cases" => cases
         } <- unsigned,
         {:ok, rebuilt} <- define(scope, cases),
         true <- rebuilt == matrix,
         do: :ok,
         else: (_ -> {:error, :invalid_compatibility_matrix})
  end

  def verify(_), do: {:error, :invalid_compatibility_matrix}

  defp evaluate(test_case, adapters) do
    layer = test_case["layer"]

    result =
      case Map.get(adapters, layer) do
        adapter when is_function(adapter, 1) -> adapter.(test_case["input"])
        _ -> {:unsupported, "adapter unavailable"}
      end

    {outcome, observed, evidence} =
      case result do
        {:compatible, evidence} ->
          {match_outcome(test_case["expectation"], "compatible"), "compatible", evidence}

        {:incompatible, evidence} ->
          {match_outcome(test_case["expectation"], "incompatible"), "incompatible", evidence}

        {:unsupported, reason} ->
          {"unsupported", "unsupported", reason}

        _ ->
          {"fail", "invalid-adapter-result", "adapter violated compatibility protocol"}
      end

    %{
      "id" => test_case["id"],
      "layer" => layer,
      "expectation" => test_case["expectation"],
      "observed" => observed,
      "outcome" => outcome,
      "evidence" => evidence,
      "case_digest" => CanonicalJCS.digest(test_case)
    }
  end

  defp match_outcome(expected, observed), do: if(expected == observed, do: "pass", else: "fail")

  defp case?(test_case) do
    is_map(test_case) and Enum.sort(Map.keys(test_case)) == ~w(expectation id input layer) and
      id?(test_case["id"]) and test_case["layer"] in @layers and
      test_case["expectation"] in @expectations and is_map(test_case["input"]) and
      byte_size(CanonicalJCS.encode(test_case["input"])) <= @max_case_bytes
  end

  defp id?(id),
    do: is_binary(id) and byte_size(id) in 1..128 and Regex.match?(~r/^[a-z0-9][a-z0-9-]*$/, id)
end
