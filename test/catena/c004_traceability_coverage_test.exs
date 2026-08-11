defmodule Catena.C004TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The traits-and-categorical-operations 0.1.4 obligation set (TR-OBL-001..032).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (TR registry).
  @expected_obligations ~w(
    TR-OBL-001 TR-OBL-002 TR-OBL-003 TR-OBL-004 TR-OBL-005 TR-OBL-006 TR-OBL-007
    TR-OBL-008 TR-OBL-009 TR-OBL-010 TR-OBL-011 TR-OBL-012 TR-OBL-013 TR-OBL-014
    TR-OBL-015 TR-OBL-016 TR-OBL-017 TR-OBL-018 TR-OBL-019 TR-OBL-020 TR-OBL-021
    TR-OBL-022 TR-OBL-023 TR-OBL-024 TR-OBL-025 TR-OBL-026 TR-OBL-027 TR-OBL-028
    TR-OBL-029 TR-OBL-030 TR-OBL-031 TR-OBL-032
  )

  # Obligations not yet covered by a focused c004 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails. TR has the
  # thinnest test-to-obligation ratio of the traced areas.
  @allowed_uncovered %{
    "TR-OBL-002" => "global instance non-overlap and no local preference; no focused c004 test",
    "TR-OBL-003" => "functional-dependency output-position unification; no focused c004 test",
    "TR-OBL-004" => "structurally decreasing instance contexts; no focused c004 test",
    "TR-OBL-013" => "derived operation must disclose or reject stack-unsafe recursion",
    "TR-OBL-018" =>
      "must not relabel older valid or invalid programs; compatibility, no focused c004 test"
  }

  describe "traits-and-categorical-operations 0.1.4 traceability coverage" do
    test "every tagged obligation is a known traits obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the TR-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "traits obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every TR-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c004 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c004_categorical_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/TR-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
