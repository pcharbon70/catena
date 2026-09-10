defmodule Catena.TestRunnerTest do
  use ExUnit.Case, async: true

  alias Catena.Tool.TestRunner

  @subject String.duplicate("a", 64)

  test "executes every promised result kind with finite evidence scope" do
    cases =
      Enum.map([:unit, :law, :model, :concurrency], fn kind ->
        %{id: Atom.to_string(kind), kind: kind, execute: fn _ -> :pass end}
      end) ++
        [
          %{
            id: "property",
            kind: :property,
            runs: 4,
            generate: fn seed, _size -> rem(seed, 11) end,
            valid: &is_integer/1,
            check: fn value, _ -> {:pass, %{"value" => value}} end,
            shrink: fn _ -> [] end
          },
          %{
            id: "specification",
            kind: :specification,
            execute: fn context ->
              {:pass, %{"subject_digest" => context.subject_digest, "obligations" => 1}}
            end
          }
        ]

    assert {:ok, suite} = TestRunner.define("all-kinds", @subject, cases, seed: 91)
    assert {:ok, report} = TestRunner.run(suite, @subject)
    assert report["status"] == "pass"
    assert report["scope"] == "finite-observation-not-proof"

    assert Enum.map(report["results"], & &1["kind"]) ==
             ~w(unit law model concurrency property specification)

    assert byte_size(report["report_digest"]) == 64
  end

  test "a seeded property failure reproduces and shrinks inside its invariant" do
    property = %{
      id: "positive-threshold",
      kind: :property,
      runs: 10,
      generate: fn seed, _size -> rem(seed, 10) + 10 end,
      valid: fn value -> is_integer(value) and value >= 0 end,
      check: fn value, _ -> if value <= 2, do: :pass, else: {:fail, %{"value" => value}} end,
      shrink: fn value -> [value - 1, -1] end
    }

    assert {:ok, suite} = TestRunner.define("shrinking", @subject, [property], seed: 17)
    assert {:ok, first} = TestRunner.run(suite, @subject)
    assert {:ok, second} = TestRunner.run(suite, @subject)
    assert first == second

    assert [result] = first["results"]
    assert result["status"] == "fail"
    assert result["evidence"]["counterexample"] == 3
    assert result["evidence"]["minimal"]
    assert result["evidence"]["shrink_steps"] > 0
  end

  test "semantic, schedule, shrink, and host bounds remain distinct" do
    property = %{
      id: "bounded-shrink",
      kind: :property,
      runs: 1,
      shrink_limit: 1,
      generate: fn _, _ -> 10 end,
      valid: &is_integer/1,
      check: fn value, _ -> if value < 1, do: :pass, else: {:fail, %{"value" => value}} end,
      shrink: fn value -> [value - 1] end
    }

    cases = [
      %{id: "fuel", kind: :model, execute: fn _ -> {:exhausted, :semantic_fuel, %{}} end},
      %{
        id: "schedule",
        kind: :concurrency,
        execute: fn _ -> {:exhausted, :schedule_bound, %{}} end
      },
      property,
      %{id: "clock", kind: :unit, host_timeout: 5, execute: fn _ -> Process.sleep(50) end}
    ]

    assert {:ok, suite} = TestRunner.define("bounds", @subject, cases, seed: 1)
    assert {:ok, report} = TestRunner.run(suite, @subject)
    assert report["status"] == "host-timeout"

    assert Enum.map(report["results"], &{&1["status"], &1["reason"]}) == [
             {"exhausted", "semantic_fuel"},
             {"exhausted", "schedule_bound"},
             {"exhausted", "shrink_bound"},
             {"host-timeout", "host-timeout"}
           ]

    shrink = Enum.at(report["results"], 2)
    refute shrink["evidence"]["minimal"]
    assert shrink["evidence"]["counterexample"] == 9
  end

  test "rejects missing tests, missing seeds, stale subjects, and tampered plans" do
    test_case = %{id: "unit", kind: :unit, execute: fn _ -> :pass end}

    assert {:error, :zero_test_run} = TestRunner.define("empty", @subject, [], seed: 1)
    assert {:error, :seed_required} = TestRunner.define("seed", @subject, [test_case])
    assert {:ok, suite} = TestRunner.define("subject", @subject, [test_case], seed: 1)
    assert {:error, :stale_subject} = TestRunner.run(suite, String.duplicate("b", 64))

    assert {:error, :invalid_test_suite} =
             TestRunner.run(put_in(suite["id"], "changed"), @subject)
  end

  test "invalid generators, nonportable evidence, and stale governed evidence fail" do
    cases = [
      %{
        id: "generator",
        kind: :property,
        runs: 1,
        generate: fn _, _ -> :outside_domain end,
        valid: &is_integer/1,
        check: fn _, _ -> :pass end,
        shrink: fn _ -> [] end
      },
      %{id: "evidence", kind: :unit, execute: fn _ -> {:pass, %{"pid" => self()}} end},
      %{
        id: "governed",
        kind: :specification,
        execute: fn _ -> {:pass, %{"subject_digest" => String.duplicate("b", 64)}} end
      }
    ]

    assert {:ok, suite} = TestRunner.define("invalid-evidence", @subject, cases, seed: 2)
    assert {:ok, report} = TestRunner.run(suite, @subject)
    assert report["status"] == "fail"

    assert Enum.map(report["results"], & &1["reason"]) ==
             ~w(invalid-generator invalid-evidence stale-evidence-subject)
  end

  test "declared effects are recorded and undeclared effects fail" do
    cases = [
      %{
        id: "declared",
        kind: :unit,
        effects: [:log],
        execute: fn context ->
          context.effect.(:log)
          :pass
        end
      },
      %{
        id: "undeclared",
        kind: :unit,
        execute: fn context ->
          context.effect.(:network)
          :pass
        end
      }
    ]

    assert {:ok, suite} = TestRunner.define("effects", @subject, cases, seed: 3)
    assert {:ok, report} = TestRunner.run(suite, @subject)
    assert Enum.at(report["results"], 0)["status"] == "pass"
    assert Enum.at(report["results"], 1)["reason"] == "undeclared-effect"
  end

  test "runner-owned children are terminated before a result is returned" do
    owner = self()

    test_case = %{
      id: "cleanup",
      kind: :concurrency,
      execute: fn context ->
        {:ok, pid} = context.spawn.(fn -> Process.sleep(:infinity) end)
        send(owner, {:child, pid})
        :pass
      end
    }

    assert {:ok, suite} = TestRunner.define("cleanup", @subject, [test_case], seed: 4)
    assert {:ok, report} = TestRunner.run(suite, @subject)
    assert_receive {:child, child}
    refute Process.alive?(child)
    assert hd(report["results"])["runner_owned_processes_cleaned"] == 1
  end

  test "profile states the scope and language revision" do
    assert Catena.LanguageVersion.latest() == "0.1.84"
    assert TestRunner.profile().version == "0.1.84"
    refute TestRunner.profile().passing_tests_are_proof
    refute TestRunner.profile().host_timeout_is_divergence
    refute TestRunner.profile().public_test_syntax
    assert Catena.ConformanceInfo.document()["testing_tools"]["version"] == "0.1.84"
  end
end
