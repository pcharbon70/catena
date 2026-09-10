defmodule Catena.DiagnosticContractTest do
  use ExUnit.Case, async: true

  alias Catena.{Diagnostic, Report, SourceSpan}
  alias Catena.Diagnostic.Contract

  test "related Unicode locations, causal provenance, and reports are structured" do
    source = "αβ\nvalue"
    assert {:ok, primary} = SourceSpan.from_bytes(source, 0, 4)
    assert {:ok, related} = SourceSpan.from_bytes(source, 5, 10)

    diagnostic =
      Diagnostic.new("T002", "type mismatch",
        span: primary,
        related: [%{label: "required here", span: related}],
        explanation: %{kind: :type_mismatch},
        provenance: [%{id: "constraint-1", relation: :requires_equal_types, path: "$.body"}]
      )

    assert {:ok, ^diagnostic} = Contract.validate(diagnostic, source)
    report = Report.diagnostic(diagnostic)
    assert report.related == [%{label: "required here", span: SourceSpan.to_map(related)}]
    assert report.provenance == diagnostic.provenance
  end

  test "type presentation is normalized, digest-bound, and bounded" do
    short = Contract.present_type({:function, :integer, :boolean})
    assert short.truncated == false
    assert short.text == "{:function, :integer, :boolean}"
    assert byte_size(short.digest) == 64

    long = Contract.present_type({:tuple, List.duplicate(:integer, 300)})
    assert long.truncated
    assert byte_size(long.text) <= 512
    assert String.ends_with?(long.text, "...")
  end

  test "edits are preimage-bound, UTF-8 aligned, and nonoverlapping" do
    source = "α + β"
    digest = :crypto.hash(:sha256, source) |> Base.encode16(case: :lower)

    good = [
      %{
        applicability: :machine_applicable,
        preimage_digest: digest,
        byte_start: 0,
        byte_end: 2,
        replacement: "x"
      },
      %{
        applicability: :maybe_incorrect,
        preimage_digest: digest,
        byte_start: 5,
        byte_end: 7,
        replacement: "y"
      }
    ]

    assert {:ok, _} = Contract.validate(Diagnostic.new("T002", "repair", fixes: good), source)

    stale = [Map.put(hd(good), :preimage_digest, String.duplicate("0", 64))]

    assert {:error, :invalid_diagnostic_contract} =
             Contract.validate(Diagnostic.new("T002", "repair", fixes: stale), source)

    split = [Map.merge(hd(good), %{byte_start: 1, byte_end: 2})]

    assert {:error, :invalid_diagnostic_contract} =
             Contract.validate(Diagnostic.new("T002", "repair", fixes: split), source)

    overlap = [hd(good), Map.merge(List.last(good), %{byte_start: 1, byte_end: 7})]

    assert {:error, :invalid_diagnostic_contract} =
             Contract.validate(Diagnostic.new("T002", "repair", fixes: overlap), source)
  end

  test "bounds and forged generated origins are rejected" do
    related =
      for index <- 0..8 do
        %{
          label: Integer.to_string(index),
          span: %SourceSpan{
            byte_start: 0,
            byte_end: 0,
            line_start: 1,
            column_start: 1,
            line_end: 1,
            column_end: 1
          }
        }
      end

    assert {:error, :invalid_diagnostic_contract} =
             Contract.validate(Diagnostic.new("T002", "many", related: related), "")

    assert {:ok, generated_digest} =
             Contract.generated_origin_digest("derive", hd(related).span, "")

    generated = %{
      generated_origin: %{
        node: "derive",
        digest: generated_digest,
        span: hd(related).span
      }
    }

    assert {:ok, _} =
             Contract.validate(Diagnostic.new("T002", "generated", explanation: generated), "")

    forged =
      put_in(
        generated,
        [:generated_origin, :digest],
        String.duplicate("0", 64)
      )

    assert {:error, :invalid_diagnostic_contract} =
             Contract.validate(Diagnostic.new("T002", "generated", explanation: forged), "")
  end

  test "coverage failures carry missing witnesses and guard explanations" do
    source = %{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "module" => "DiagnosticCoverage",
      "origin" => "test://diagnostic",
      "exports" => [],
      "definitions" => [
        %{
          "name" => "only_true",
          "signature" => %{
            "forall" => [],
            "type" => %{
              "tag" => "function",
              "parameter" => %{"tag" => "boolean"},
              "result" => %{"tag" => "integer"}
            }
          },
          "parameters" => ["x"],
          "body" => %{
            "tag" => "match",
            "scrutinee" => %{"tag" => "variable", "name" => "x"},
            "clauses" => [
              %{
                "pattern" => %{"tag" => "boolean", "value" => true},
                "body" => %{"tag" => "integer", "value" => 1}
              }
            ]
          }
        }
      ]
    }

    assert {:error, %{id: "M001", details: %{witness: "false", scrutinee_type: :boolean}}} =
             source |> JSON.encode!() |> Catena.check_json()

    guarded =
      put_in(source, ["definitions", Access.at(0), "body", "clauses"], [
        %{
          "pattern" => %{"tag" => "wildcard"},
          "guard" => %{"tag" => "boolean", "value" => false},
          "body" => %{"tag" => "integer", "value" => 1}
        },
        %{
          "pattern" => %{"tag" => "wildcard"},
          "body" => %{"tag" => "integer", "value" => 2}
        }
      ])

    assert {:error, %{id: "M002", details: %{clause: 1, guard_explanation: :guard_unsatisfiable}}} =
             guarded |> JSON.encode!() |> Catena.check_json()
  end

  test "the semantic contract is versioned while parse coverage remains held" do
    assert Catena.LanguageVersion.introduced(:diagnostic_contract) == "0.1.91"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("diagnostic-contract", "0.1.91")
    profile = Catena.ConformanceInfo.document()["diagnostic_contract"]
    assert profile["parse_diagnostics"] == "held_for_p109"
    assert profile["maximum_related_locations"] == 8
  end
end
