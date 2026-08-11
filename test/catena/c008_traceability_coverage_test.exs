defmodule Catena.C008TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The editions-and-feature-lifecycle 0.1.7 obligation set (ED-OBL-001..036).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (ED registry).
  @expected_obligations ~w(
    ED-OBL-001 ED-OBL-002 ED-OBL-003 ED-OBL-004 ED-OBL-005 ED-OBL-006 ED-OBL-007
    ED-OBL-008 ED-OBL-009 ED-OBL-010 ED-OBL-011 ED-OBL-012 ED-OBL-013 ED-OBL-014
    ED-OBL-015 ED-OBL-016 ED-OBL-017 ED-OBL-018 ED-OBL-019 ED-OBL-020 ED-OBL-021
    ED-OBL-022 ED-OBL-023 ED-OBL-024 ED-OBL-025 ED-OBL-026 ED-OBL-027 ED-OBL-028
    ED-OBL-029 ED-OBL-030 ED-OBL-031 ED-OBL-032 ED-OBL-033 ED-OBL-034 ED-OBL-035
    ED-OBL-036
  )

  # Obligations not yet covered by a focused c008 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails.
  @allowed_uncovered %{
    "ED-OBL-015" => "implementations must not add vendor preview names; no focused c008 test",
    "ED-OBL-034" =>
      "default deprecation warnings and project/governance promotion to failure; no focused c008 test"
  }

  describe "editions-and-feature-lifecycle 0.1.7 traceability coverage" do
    test "every tagged obligation is a known editions obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the ED-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "editions obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every ED-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c008 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c008_editions_lifecycle_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/ED-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
