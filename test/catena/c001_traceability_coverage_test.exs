defmodule Catena.C001TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  # The type-system 0.1.1 obligation set (TS-OBL-001..044).
  # Source of truth: catena-research/10-maps/conformance-traceability.md (TS registry).
  @expected_obligations ~w(
    TS-OBL-001 TS-OBL-002 TS-OBL-003 TS-OBL-004 TS-OBL-005 TS-OBL-006 TS-OBL-007
    TS-OBL-008 TS-OBL-009 TS-OBL-010 TS-OBL-011 TS-OBL-012 TS-OBL-013 TS-OBL-014
    TS-OBL-015 TS-OBL-016 TS-OBL-017 TS-OBL-018 TS-OBL-019 TS-OBL-020 TS-OBL-021
    TS-OBL-022 TS-OBL-023 TS-OBL-024 TS-OBL-025 TS-OBL-026 TS-OBL-027 TS-OBL-028
    TS-OBL-029 TS-OBL-030 TS-OBL-031 TS-OBL-032 TS-OBL-033 TS-OBL-034 TS-OBL-035
    TS-OBL-036 TS-OBL-037 TS-OBL-038 TS-OBL-039 TS-OBL-040 TS-OBL-041 TS-OBL-042
    TS-OBL-043 TS-OBL-044
  )

  # Obligations not covered by a focused type_conformance/compiler test.
  # The type system is foundational: several entries are exercised transitively
  # by the data (c002), trait (c004), effect (c005), kernel (c010), and
  # resumption-token suites, noted here rather than double-tagged. This set
  # must shrink toward empty as dedicated tests are added.
  @allowed_uncovered %{
    "TS-OBL-002" => "shared static contract is exercised transitively by every c002-c010 suite",
    "TS-OBL-007" => "type-alias expansion and preservation has no focused test",
    "TS-OBL-010" => "generalization ambiguity rejection (T006) is partial",
    "TS-OBL-012" => "solver-order alpha-equivalence is exercised in the c004 trait suite",
    "TS-OBL-021" => "functional-dependency output determinism is partial",
    "TS-OBL-023" => "solver-step progress is exercised in the c004 trait suite",
    "TS-OBL-024" => "solver-scheduling independence is exercised in the c004 trait suite",
    "TS-OBL-025" => "GADT enclosing-signature rule is exercised in the c002 data suite",
    "TS-OBL-029" => "affine resumption token is exercised in resumption_token and c005",
    "TS-OBL-032" => "source-span preservation is exercised in the c010 kernel suite",
    "TS-OBL-033" => "independent core verifier is exercised in the c002 and c010 suites",
    "TS-OBL-034" => "verifier recheck of types/effects/evidence is exercised in c002 and c010",
    "TS-OBL-035" => "verifier failure framed as implementation defect is partial",
    "TS-OBL-038" => "diagnostic family-subdivision compatibility mapping has no focused test"
  }

  describe "type-system 0.1.1 traceability coverage" do
    test "every tagged obligation is a known type-system obligation" do
      expected = MapSet.new(@expected_obligations)
      unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

      assert Enum.empty?(unknown),
             "tagged obligations not in the TS-OBL set (typo?): #{inspect(MapSet.to_list(unknown))}"
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
             "type-system obligations lack coverage and are not allow-listed: " <>
               inspect(MapSet.to_list(unlisted))
    end
  end

  # Collect every TS-OBL-NNN identifier from `@tag obligations: ~w(...)` blocks
  # in the dedicated type-system suites. The type system is foundational and
  # also exercised across other slices; see the allow-list for those mappings.
  defp covered_obligations do
    ["test/catena/type_conformance_test.exs", "test/catena/compiler_test.exs"]
    |> Enum.flat_map(fn path ->
      source = File.read!(path)

      ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
      |> Regex.scan(source, capture: :all_but_first)
      |> Enum.flat_map(fn [chunk] -> Regex.scan(~r/TS-OBL-\d+/, chunk) |> Enum.map(&hd/1) end)
    end)
    |> Enum.uniq()
  end
end
