defmodule Catena.C003TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The clause-conditions 0.1.3 obligation set (CC-OBL-001..049).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (pilot registry).
  # This list is the cross-repo copy kept in sync by the coordinated research/compiler PRs.
  @expected_obligations ~w(
    CC-OBL-001 CC-OBL-002 CC-OBL-003 CC-OBL-004 CC-OBL-005 CC-OBL-006 CC-OBL-007 CC-OBL-008 CC-OBL-009 CC-OBL-010
    CC-OBL-011 CC-OBL-012 CC-OBL-013 CC-OBL-014 CC-OBL-015 CC-OBL-016 CC-OBL-017 CC-OBL-018 CC-OBL-019 CC-OBL-020
    CC-OBL-021 CC-OBL-022 CC-OBL-023 CC-OBL-024 CC-OBL-025 CC-OBL-026 CC-OBL-027 CC-OBL-028 CC-OBL-029 CC-OBL-030
    CC-OBL-031 CC-OBL-032 CC-OBL-033 CC-OBL-034 CC-OBL-035 CC-OBL-036 CC-OBL-037 CC-OBL-038 CC-OBL-039 CC-OBL-040
    CC-OBL-041 CC-OBL-042 CC-OBL-043 CC-OBL-044 CC-OBL-045 CC-OBL-046 CC-OBL-047 CC-OBL-048 CC-OBL-049
  )

  # Obligations not yet covered by a focused c003 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails.
  @allowed_uncovered %{
    "CC-OBL-010" => "or-pattern lowering with one shared condition continuation",
    "CC-OBL-011" => "0.1.2 interface consumed without condition evidence",
    "CC-OBL-016" => "nonempty condition effect rejected",
    "CC-OBL-032" =>
      "OTP Abstract Format sole BEAM boundary is architectural; no focused c003 unit",
    "CC-OBL-033" => "typed core/effects/source attribution preserved through lowering (partial)",
    "CC-OBL-034" =>
      "ordinary-match exhaustiveness is exercised in c002_data_test; c003-specific test pending",
    "CC-OBL-038" => "body failure or divergence does not resume clause selection",
    "CC-OBL-039" => "independent typed-core verifier rejects duplicated condition evaluation",
    "CC-OBL-040" => "or-pattern alternatives bind the same names",
    "CC-OBL-048" => "implementation limit reported as limit or unknown, not a semantic proof"
  }

  describe "clause-conditions 0.1.3 traceability coverage" do
    test "every tagged obligation is a known clause-conditions obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the CC-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "clause-conditions obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every CC-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c003 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c003_clause_condition_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/CC-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
