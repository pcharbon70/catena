defmodule Catena.C010TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The formal-semantic-kernel 0.1.8 obligation set (FK-OBL-001..015).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (FK registry).
  # This list is the cross-repo copy kept in sync by the coordinated research/compiler PRs.
  @expected_obligations ~w(
    FK-OBL-001 FK-OBL-002 FK-OBL-003 FK-OBL-004 FK-OBL-005 FK-OBL-006 FK-OBL-007
    FK-OBL-008 FK-OBL-009 FK-OBL-010 FK-OBL-011 FK-OBL-012 FK-OBL-013 FK-OBL-014
    FK-OBL-015
  )

  # Obligations not yet covered by a focused c010 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails. Currently empty:
  # every FK obligation has at least one tagged c010 test.
  @allowed_uncovered %{}

  describe "formal-semantic-kernel 0.1.8 traceability coverage" do
    test "every tagged obligation is a known formal-semantic-kernel obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the FK-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "formal-semantic-kernel obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every FK-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c010 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c010_formal_semantic_kernel_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/FK-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
