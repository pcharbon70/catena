defmodule Catena.PerformanceEnvelopeTest do
  use ExUnit.Case, async: false

  alias Catena.{CanonicalJCS, LanguageLifecycle, LanguageVersion}
  alias Catena.Performance.Envelope

  @tag obligations: ~w(PE-OBL-001 PE-OBL-002 PE-OBL-003 PE-OBL-004)
  test "all workload families produce a supported-host semantic envelope" do
    workloads = workloads(fn size -> Enum.sum(0..size) end)
    assert {:ok, report} = Envelope.run(workloads, repetitions: 2, warmup: 0, seed: 138)
    assert :ok = Envelope.verify(report)
    assert report["all_semantic_gates_pass"]
    assert report["portable_claim"] == false

    assert Enum.sort(Enum.map(report["workloads"], & &1["family"])) ==
             Enum.sort(Envelope.profile().families)
  end

  @tag obligations: ~w(PE-OBL-005 PE-OBL-006 PE-OBL-007 PE-OBL-008)
  test "semantic mismatches and timeouts are retained and excluded from speed claims" do
    mismatched = workloads(fn _ -> :different end)
    assert {:ok, mismatch_report} = Envelope.run(mismatched, repetitions: 1, warmup: 0)
    refute mismatch_report["all_semantic_gates_pass"]
    assert Enum.all?(mismatch_report["workloads"], &(&1["outcome"] == "semantic-mismatch"))

    slow = fn _ ->
      Process.sleep(20)
      :ok
    end

    baseline = fn _ -> :ok end

    workloads =
      for family <- Envelope.profile().families,
          do: Envelope.workload(family, family, [1], slow, baseline)

    assert {:ok, timeout_report} =
             Envelope.run(workloads, repetitions: 1, warmup: 0, timeout_ms: 1)

    assert Enum.all?(timeout_report["workloads"], &(&1["outcome"] == "timeout"))
  end

  @tag obligations: ~w(PE-OBL-009 PE-OBL-010 PE-OBL-011 PE-OBL-012)
  test "relative comparison detects artificial regressions and excludes invalid evidence" do
    assert {:ok, old} =
             Envelope.run(workloads(fn size -> Enum.sum(0..size) end), repetitions: 1, warmup: 0)

    [first | rest] = old["workloads"]
    slower = Map.put(first, "median_ns", first["median_ns"] * 2 + 1)
    unsigned = old |> Map.put("workloads", [slower | rest]) |> Map.delete("digest")
    new = Map.put(unsigned, "digest", CanonicalJCS.digest(unsigned))

    assert {:ok, comparison} = Envelope.compare(old, new, regression_ratio: 1.25)
    assert [%{class: :regression}] = Enum.filter(comparison.comparisons, &(&1.id == first["id"]))

    altered =
      new
      |> put_in(["workloads"], [Map.put(slower, "outcome", "semantic-mismatch") | rest])
      |> Map.put("all_semantic_gates_pass", false)

    unsigned = Map.delete(altered, "digest")
    altered = Map.put(unsigned, "digest", CanonicalJCS.digest(unsigned))
    assert {:ok, excluded} = Envelope.compare(old, altered)
    assert Enum.any?(excluded.exclusions, &(&1.id == first["id"]))
  end

  @tag obligations: ~w(PE-OBL-013 PE-OBL-014 PE-OBL-015 PE-OBL-016)
  test "invalid suites, tampering, lifecycle, and profile boundaries are explicit" do
    assert {:error, :invalid_performance_workloads} = Envelope.run([], repetitions: 1)
    assert {:ok, report} = Envelope.run(workloads(& &1), repetitions: 1, warmup: 0)
    assert {:error, :invalid_performance_envelope} = Envelope.verify(Map.put(report, "seed", -1))

    assert LanguageVersion.latest() == "0.1.88"
    assert LanguageVersion.introduced(:performance_envelope) == "0.1.88"
    assert {:ok, :stable} = LanguageLifecycle.state("performance-envelope", "0.1.88")
    profile = Catena.ConformanceInfo.document()["performance_envelope"]
    assert profile["semantic_gate_required"]
    assert profile["absolute_timing_portable"] == false
    assert profile["negative_results_retained"]
    assert profile["public_source"] == "held_for_p109"
  end

  defp workloads(candidate) do
    for family <- Envelope.profile().families do
      Envelope.workload(family, family, [1], candidate, fn size -> Enum.sum(0..size) end)
    end
  end
end
