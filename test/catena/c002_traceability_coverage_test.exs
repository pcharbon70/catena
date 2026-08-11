defmodule Catena.C002TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The data-and-patterns 0.1.2 obligation set (DP-OBL-001..071).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (DP registry).
  # This list is the cross-repo copy kept in sync by the coordinated research/compiler PRs.
  @expected_obligations ~w(
    DP-OBL-001 DP-OBL-002 DP-OBL-003 DP-OBL-004 DP-OBL-005 DP-OBL-006 DP-OBL-007 DP-OBL-008 DP-OBL-009 DP-OBL-010
    DP-OBL-011 DP-OBL-012 DP-OBL-013 DP-OBL-014 DP-OBL-015 DP-OBL-016 DP-OBL-017 DP-OBL-018 DP-OBL-019 DP-OBL-020
    DP-OBL-021 DP-OBL-022 DP-OBL-023 DP-OBL-024 DP-OBL-025 DP-OBL-026 DP-OBL-027 DP-OBL-028 DP-OBL-029 DP-OBL-030
    DP-OBL-031 DP-OBL-032 DP-OBL-033 DP-OBL-034 DP-OBL-035 DP-OBL-036 DP-OBL-037 DP-OBL-038 DP-OBL-039 DP-OBL-040
    DP-OBL-041 DP-OBL-042 DP-OBL-043 DP-OBL-044 DP-OBL-045 DP-OBL-046 DP-OBL-047 DP-OBL-048 DP-OBL-049 DP-OBL-050
    DP-OBL-051 DP-OBL-052 DP-OBL-053 DP-OBL-054 DP-OBL-055 DP-OBL-056 DP-OBL-057 DP-OBL-058 DP-OBL-059 DP-OBL-060
    DP-OBL-061 DP-OBL-062 DP-OBL-063 DP-OBL-064 DP-OBL-065 DP-OBL-066 DP-OBL-067 DP-OBL-068 DP-OBL-069 DP-OBL-070
    DP-OBL-071
  )

  # Obligations not yet covered by a focused c002 test, each with its reason.
  # This set must shrink toward empty: covering a gap requires both adding a
  # tagged test and removing its entry here, or this gate fails.
  @allowed_uncovered %{
    "DP-OBL-003" => "alias is a future declaration form; no 0.1.2 executable surface",
    "DP-OBL-026" =>
      "future refutability context (function parameters or let bindings); P044 partial, no 0.1.2 executable context",
    "DP-OBL-048" => "coverage must not justify an unsound branch type; no focused c002 unit",
    "DP-OBL-056" =>
      "layout selection after typed-core verification and no spelling/arity reconstruction; architectural",
    "DP-OBL-057" => "L001 inconsistent-layout implementation-failure path; architectural",
    "DP-OBL-058" => "sole OTP 29 compile:noenv_forms BEAM boundary; architectural",
    "DP-OBL-059" => "untrusted Erlang term ADT validation; future G095 boundary",
    "DP-OBL-065" =>
      "diagnostics include JSON path or source span; P117 diagnostic-quality boundary"
  }

  describe "data-and-patterns 0.1.2 traceability coverage" do
    test "every tagged obligation is a known data-and-patterns obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the DP-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "data-and-patterns obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every DP-OBL-NNN identifier from a `@tag obligations: ~w(...)`
  # block in the c002 suite. The block may span lines, so match the whole
  # sigil rather than scanning line by line. Decoupled from ExUnit metadata so
  # coverage is machine-checkable from source.
  defp covered_obligations do
    source = File.read!("test/catena/c002_data_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/DP-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    |> Enum.uniq()
  end
end
