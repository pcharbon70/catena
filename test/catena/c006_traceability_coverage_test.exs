defmodule Catena.C006TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The specifications-and-governance 0.1.6 obligation set (SG-OBL-001..044).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (SG registry).
  @expected_obligations ~w(
    SG-OBL-001 SG-OBL-002 SG-OBL-003 SG-OBL-004 SG-OBL-005 SG-OBL-006 SG-OBL-007
    SG-OBL-008 SG-OBL-009 SG-OBL-010 SG-OBL-011 SG-OBL-012 SG-OBL-013 SG-OBL-014
    SG-OBL-015 SG-OBL-016 SG-OBL-017 SG-OBL-018 SG-OBL-019 SG-OBL-020 SG-OBL-021
    SG-OBL-022 SG-OBL-023 SG-OBL-024 SG-OBL-025 SG-OBL-026 SG-OBL-027 SG-OBL-028
    SG-OBL-029 SG-OBL-030 SG-OBL-031 SG-OBL-032 SG-OBL-033 SG-OBL-034 SG-OBL-035
    SG-OBL-036 SG-OBL-037 SG-OBL-038 SG-OBL-039 SG-OBL-040 SG-OBL-041 SG-OBL-042
    SG-OBL-043 SG-OBL-044
  )

  # Obligations not yet covered by a focused c006 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails.
  @allowed_uncovered %{
    "SG-OBL-005" =>
      "no ignore/force switch reports a governed action as ungoverned; architectural",
    "SG-OBL-030" =>
      "the immutable conformance revision passes the whole required corpus; satisfied by the full suite, not one test"
  }

  describe "specifications-and-governance 0.1.6 traceability coverage" do
    test "every tagged obligation is a known governance obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the SG-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
    end

    test "every obligation is covered by a tagged test or explicitly allow-listed" do
      covered = MapSet.new(covered_obligations())
      expected = MapSet.new(@expected_obligations)
      allowed = MapSet.new(Map.keys(@allowed_uncovered))

      uncovered = MapSet.difference(expected, covered)
      # Allow-listed entries that are now covered must be removed.
      stale_allowed = MapSet.intersection(allowed, covered)
      # Uncovered obligations that are not allow-listed must be covered or listed.
      unlisted = MapSet.difference(uncovered, allowed)

      assert Enum.empty?(stale_allowed),
             "remove these allow-listed obligations; they are now covered: " <>
               inspect(MapSet.to_list(stale_allowed))

      assert Enum.empty?(unlisted),
             "governance obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every SG-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c006 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c006_specification_governance_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/SG-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
