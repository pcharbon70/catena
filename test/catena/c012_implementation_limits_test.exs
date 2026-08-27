defmodule Catena.C012ImplementationLimitsTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Catena.{ConformanceInfo, ImplementationLimits}

  @tag obligations: ~w(IL-OBL-001 IL-OBL-002 IL-OBL-003)
  test "conformance-info is deterministic and backed by the executable registry" do
    first = ConformanceInfo.document()
    second = ConformanceInfo.document()

    assert first == second
    assert JSON.encode!(first) == JSON.encode!(second)
    assert first["format"] == "catena-conformance-info"
    assert first["version"] == 1
    assert first["language"]["current_revision"] == "0.1.30"
    assert first["implementation"]["release"] == "0.1.0"

    assert Enum.map(first["permissions"], & &1["id"]) == [
             "export-signature-candidate",
             "legacy-selection-inference",
             "interface-consumption",
             "adt-layout",
             "gadt-coverage-equalities",
             "derived-fold-lowering",
             "selection-metadata",
             "claim-summaries"
           ]

    assert Map.keys(first["limits"]) |> Enum.sort() ==
             ImplementationLimits.all() |> Enum.map(&Atom.to_string(&1.id)) |> Enum.sort()

    Enum.each(ImplementationLimits.all(), fn limit ->
      reported = first["limits"][Atom.to_string(limit.id)]
      assert reported["portable_minimum"] == limit.portable_minimum
      assert reported["configured"] == limit.configured
      assert reported["unit"] == Atom.to_string(limit.unit)
    end)

    output = capture_io(fn -> Catena.CLI.main(["conformance-info"]) end)
    assert {:ok, cli_document} = JSON.decode(String.trim(output))
    assert cli_document == first
  end

  @tag obligations: ~w(IL-OBL-004 IL-OBL-011)
  test "the 253-argument portable floor reaches an effectful OTP worker of arity 255" do
    source = wide_effectful_kernel(253)

    assert {:ok, :C012Wide, binary, metadata} = Catena.compile_kernel(source)
    assert byte_size(binary) <= ImplementationLimits.configured(:generated_beam_bytes)

    assert Enum.any?(metadata.forms, fn
             {:function, _, :wide, 253, _} -> true
             _ -> false
           end)

    assert Enum.any?(metadata.forms, fn
             {:function, _, :__catena_kernel_cps_wide, 255, _} -> true
             _ -> false
           end)

    assert {:error,
            %{
              id: "LIM001",
              details: %{
                limit_id: "callable_arity",
                minimum_supported: 253,
                configured: 253,
                observed: 254,
                unit: "arguments"
              }
            }} = Catena.check_kernel(wide_effectful_kernel(254))
  end

  @tag obligations: ~w(IL-OBL-005 IL-OBL-011)
  test "both frontends accept 4096 integer digits and reject 4097 as LIM002" do
    accepted = String.duplicate("9", 4_096)
    refused = String.duplicate("9", 4_097)

    assert {:ok, _core} = Catena.check_json(integer_json(accepted))
    assert {:ok, _core} = Catena.check_kernel(integer_kernel(accepted))

    for result <- [
          Catena.check_json(integer_json(refused)),
          Catena.check_kernel(integer_kernel(refused))
        ] do
      assert {:error,
              %{
                id: "LIM002",
                details: %{
                  limit_id: "integer_literal_digits",
                  minimum_supported: 4_096,
                  configured: 4_096,
                  observed: 4_097,
                  unit: "decimal_digits"
                }
              }} = result
    end
  end

  @tag obligations: ~w(IL-OBL-006 IL-OBL-007 IL-OBL-011)
  test "literal and generated-module bounds have explicit applicability and diagnostics" do
    literal = ImplementationLimits.fetch!(:decoded_literal_bytes)
    assert literal.portable_minimum == 65_536
    assert literal.applies_to == "decoded text and byte literals"
    assert literal.exhaustion == %{kind: :diagnostic, id: "LIM004"}

    assert :ok =
             ImplementationLimits.validate_decoded_literal_bytes(
               :binary.copy(<<0>>, literal.configured)
             )

    assert {:error, %{id: "LIM004", details: %{observed: 65_537}}} =
             ImplementationLimits.validate_decoded_literal_bytes(
               :binary.copy(<<0>>, literal.configured + 1)
             )

    limit = ImplementationLimits.configured(:generated_beam_bytes)
    assert :ok = ImplementationLimits.validate_generated_module(:binary.copy(<<0>>, limit))

    assert {:error,
            %{
              id: "LIM003",
              details: %{
                limit_id: "generated_beam_bytes",
                minimum_supported: ^limit,
                configured: ^limit,
                observed: observed,
                unit: "bytes"
              }
            }} = ImplementationLimits.validate_generated_module(:binary.copy(<<0>>, limit + 1))

    assert observed == limit + 1
  end

  @tag obligations: ~w(IL-OBL-008 IL-OBL-009)
  test "analysis refusals and inconclusive evidence bounds remain distinct" do
    limits = ConformanceInfo.document()["limits"]

    assert limits["pattern_coverage_steps"]["classification"] == "implementation_limit"

    assert limits["pattern_coverage_steps"]["exhaustion"] == %{
             "id" => "M004",
             "kind" => "diagnostic"
           }

    for id <-
          ~w(condition_fact_nodes condition_fact_branch_steps kernel_reference_steps kernel_exploration_transitions kernel_exploration_configurations) do
      assert limits[id]["classification"] == "evidence_bound"
      assert limits[id]["exhaustion"]["kind"] == "inconclusive"
    end
  end

  @tag obligations: ~w(IL-OBL-010)
  test "mailbox capacity is a deployment concern without a compiler message-count cap" do
    profile = ConformanceInfo.document()
    mailbox = profile["runtime_capacities"]["mailbox"]

    assert mailbox["capacity"] == "deployment-defined"
    assert mailbox["policy_owner"] == "G068/G129"
    assert ImplementationLimits.configured(:mailbox_capacity) == nil

    assert Enum.any?(mailbox["semantic_constraints"], &String.contains?(&1, "reordering"))
    assert Enum.any?(mailbox["semantic_constraints"], &String.contains?(&1, "message loss"))
  end

  defp wide_effectful_kernel(arity) do
    parameters = Enum.map(1..arity, &"arg#{&1}")

    signature =
      parameters
      |> Enum.with_index()
      |> Enum.reduce("Int", fn {_parameter, index}, result ->
        effects = if index == 0, do: "(effects Ask)", else: "(effects)"
        "(Fn Int #{effects} #{result})"
      end)

    expression =
      Enum.reduce(Enum.reverse(parameters), "(request Ask ask)", fn parameter, body ->
        "(fn (#{parameter} Int) #{body})"
      end)

    """
    (module C012Wide
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c012-wide")
      (effect Ask (operation ask (params) Int))
      (def wide
        (signature #{signature} (uses))
        #{expression}))
    """
  end

  defp integer_kernel(integer) do
    """
    (module C012Integer
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c012-integer")
      (def main (signature Int (uses)) #{integer}))
    """
  end

  defp integer_json(integer) do
    """
    {
      "version": "0.1.1",
      "origin": "test://c012-integer",
      "module": "C012Integer",
      "exports": ["main"],
      "definitions": [{
        "name": "main",
        "signature": {"forall": [], "type": {"tag": "integer"}},
        "body": {"tag": "integer", "value": #{integer}}
      }]
    }
    """
  end
end
