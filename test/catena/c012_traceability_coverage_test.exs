defmodule Catena.C012TraceabilityCoverageTest do
  use ExUnit.Case, async: true

  @expected_obligations ~w(
    IL-OBL-001 IL-OBL-002 IL-OBL-003 IL-OBL-004 IL-OBL-005 IL-OBL-006
    IL-OBL-007 IL-OBL-008 IL-OBL-009 IL-OBL-010 IL-OBL-011 IL-OBL-012
  )

  @allowed_uncovered %{
    "IL-OBL-012" => "C012 governance/version-axis rule; no executable compiler behavior"
  }

  test "every tagged obligation is a known implementation-limits obligation" do
    expected = MapSet.new(@expected_obligations)
    unknown = covered_obligations() |> MapSet.new() |> MapSet.difference(expected)

    assert Enum.empty?(unknown),
           "tagged obligations not in the IL-OBL set: #{inspect(MapSet.to_list(unknown))}"
  end

  test "every implementation-limits obligation is covered or explicitly allow-listed" do
    covered = MapSet.new(covered_obligations())
    expected = MapSet.new(@expected_obligations)
    allowed = MapSet.new(Map.keys(@allowed_uncovered))
    uncovered = MapSet.difference(expected, covered)
    stale_allowed = MapSet.intersection(allowed, covered)
    unlisted = MapSet.difference(uncovered, allowed)

    assert Enum.empty?(stale_allowed),
           "remove covered allow-list entries: #{inspect(MapSet.to_list(stale_allowed))}"

    assert Enum.empty?(unlisted),
           "implementation-limits obligations lack coverage: #{inspect(MapSet.to_list(unlisted))}"
  end

  defp covered_obligations do
    source = File.read!("test/catena/c012_implementation_limits_test.exs")

    ~r/@tag\s+obligations:\s*~w\(([^)]*)\)/
    |> Regex.scan(source, capture: :all_but_first)
    |> Enum.flat_map(fn [chunk] ->
      Regex.scan(~r/IL-OBL-\d+/, chunk) |> Enum.map(&hd/1)
    end)
    |> Enum.uniq()
  end
end
