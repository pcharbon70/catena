defmodule Catena.C005TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The effects-and-handlers 0.1.5 obligation set (EF-OBL-001..027).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (EF registry).
  @expected_obligations ~w(
    EF-OBL-001 EF-OBL-002 EF-OBL-003 EF-OBL-004 EF-OBL-005 EF-OBL-006 EF-OBL-007
    EF-OBL-008 EF-OBL-009 EF-OBL-010 EF-OBL-011 EF-OBL-012 EF-OBL-013 EF-OBL-014
    EF-OBL-015 EF-OBL-016 EF-OBL-017 EF-OBL-018 EF-OBL-019 EF-OBL-020 EF-OBL-021
    EF-OBL-022 EF-OBL-023 EF-OBL-024 EF-OBL-025 EF-OBL-026 EF-OBL-027
  )

  # Obligations not yet covered by a focused c005 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails.
  @allowed_uncovered %{
    "EF-OBL-005" => "public definitions must write their uses entries; only indirectly exercised",
    "EF-OBL-013" =>
      "backend language boundary (no Rust/Python/Core Erlang/BEAM assembly/other VM) is architectural"
  }

  describe "effects-and-handlers 0.1.5 traceability coverage" do
    test "every tagged obligation is a known effects obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the EF-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "effects obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every EF-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c005 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c005_effects_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/EF-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
