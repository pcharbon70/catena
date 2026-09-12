Code.require_file("../support/semantic_generators.ex", __DIR__)

defmodule Catena.DifferentialTestingTest do
  use ExUnit.Case, async: false

  alias Catena.Reference.Differential
  alias Catena.Test.SemanticGenerators

  @subject String.duplicate("d", 64)

  test "retained data, conditions, calls, and patterns agree through kernel and BEAM" do
    source = File.read!("test/fixtures/c010-kernel.catena")
    assert {:ok, core} = Catena.check_kernel(source)

    assert {:ok, reference} =
             Catena.Reference.Observation.observe(:kernel, %{core: core, entry: "main"})

    assert {:ok, module, binary, _metadata} = Catena.compile_kernel(source)
    :code.purge(module)
    :code.delete(module)
    assert {:module, ^module} = :code.load_binary(module, ~c"p134-cross-feature.beam", binary)

    {value, _events} =
      Catena.Effect.Runtime.capture_trace(fn -> apply(module, :main, []) end)

    :code.purge(module)
    :code.delete(module)

    reference = %{status: reference.status, value: reference.value}

    production = %{status: :completed, value: value}

    assert {:ok, comparison} =
             Differential.compare(reference, production, fields: ~w(status value))

    assert comparison.agreement
  end

  test "foreign values compare a checked reference observation with the production codec" do
    {:ok, codec} = Catena.Foreign.Codec.new({:data, :integer})
    limits = %{nodes: 8, bytes: 64, depth: 4}

    assert {:ok, observation} =
             Catena.Reference.Observation.observe(:foreign, %{
               codec: codec,
               native: 41,
               limits: limits
             })

    assert {:ok, value} = Catena.Foreign.Codec.decode(codec, 41, limits)

    reference = %{
      status: observation.status,
      value: observation.value,
      reason: observation.reason
    }

    production = %{status: :completed, value: value, reason: nil}

    assert {:ok, comparison} =
             Differential.compare(reference, production, fields: ~w(status value reason))

    assert comparison.agreement
  end

  test "seeded typed kernel and comprehension programs agree on reference and BEAM" do
    assert {:ok, report} =
             Differential.run(
               "retained-kernel-and-comprehensions",
               @subject,
               SemanticGenerators.mixed(),
               SemanticGenerators.adapters(),
               seed: 13_457,
               observations: 64,
               size: 24,
               shrink_limit: 32,
               fields: ~w(status value reason events lifetime)
             )

    assert report["status"] == "pass"
    assert report["scope"] == "finite-observation-not-proof"
    assert [result] = report["results"]
    assert result["kind"] == "property"
    assert result["evidence"]["observations"] == 64
  end

  test "wrong values, event order, callback multiplicity, and cancellation are detected" do
    examples = [
      {:value, %{status: :completed, value: 3, events: [], callback_count: 1}},
      {:event_order, %{status: :completed, value: 3, events: [:source, :filter, :yield]}},
      {:callback_count, %{status: :completed, value: 3, events: [], callback_count: 2}},
      {:cancellation, %{status: :cancelled, value: nil, reason: :owner_cancelled, events: []}}
    ]

    for {fault, reference} <- examples do
      production = Differential.mutate(reference, fault)

      assert {:ok, comparison} =
               Differential.compare(reference, production,
                 fields: Map.keys(reference),
                 comparison: :exact_observation
               )

      refute comparison.agreement
      assert comparison.difference != []
    end
  end

  test "nondeterministic schedules use allowed observations rather than one VM interleaving" do
    allowed = [
      %{status: :completed, value: "left-first", events: ["left", "right"]},
      %{status: :completed, value: "right-first", events: ["right", "left"]}
    ]

    assert {:ok, accepted} =
             Differential.compare(allowed, Enum.at(allowed, 1),
               comparison: :allowed_observation_set,
               fields: ~w(status value events)
             )

    assert accepted.agreement

    assert {:ok, rejected} =
             Differential.compare(allowed, %{status: :completed, value: "invented", events: []},
               comparison: :allowed_observation_set,
               fields: ~w(status value events)
             )

    refute rejected.agreement
    assert rejected.difference == ["outside-allowed-set"]
  end

  test "adapter failures and unsupported generated scenarios remain failures" do
    generator = %{
      generate: fn _, _ -> %{"family" => "unsupported"} end,
      valid: fn _ -> true end,
      shrink: fn _ -> [] end
    }

    adapters = %{
      supports?: fn _ -> false end,
      reference: fn _ -> {:ok, %{status: :completed}} end,
      production: fn _ -> {:ok, %{status: :completed}} end
    }

    assert {:ok, report} =
             Differential.run("unsupported", @subject, generator, adapters,
               seed: 1,
               observations: 1
             )

    assert report["status"] == "fail"

    assert get_in(report, ["results", Access.at(0), "evidence", "failure", "reason"]) ==
             "unsupported-generated-scenario"
  end

  test "a generated disagreement shrinks and preserves the semantic domain" do
    generator = %{
      generate: fn _, _ -> %{"n" => 16} end,
      valid: fn value -> is_map(value) and is_integer(value["n"]) and value["n"] >= 0 end,
      shrink: fn value -> [%{"n" => value["n"] - 1}, %{"n" => -1}] end
    }

    adapters = %{
      supports?: fn _ -> true end,
      reference: fn value -> {:ok, %{status: :completed, value: value["n"]}} end,
      production: fn value ->
        observed = if value["n"] >= 3, do: value["n"] + 1, else: value["n"]
        {:ok, %{status: :completed, value: observed}}
      end
    }

    assert {:ok, report} =
             Differential.run("shrinking-disagreement", @subject, generator, adapters,
               seed: 22,
               observations: 1,
               shrink_limit: 32,
               fields: ~w(status value)
             )

    evidence = get_in(report, ["results", Access.at(0), "evidence"])
    assert evidence["counterexample"] == %{"n" => 3}
    assert evidence["minimal"]
  end

  test "retained minimized counterexamples bind source, toolchain, scenario, and observation" do
    path = Application.app_dir(:catena, "priv/conformance/differential-counterexamples.json")
    document = path |> File.read!() |> JSON.decode!()
    assert :ok = Differential.verify_corpus(document)

    assert Enum.map(document["cases"], & &1["fault"]) ==
             ~w(value event_order callback_count cancellation)

    refute :ok == Differential.verify_corpus(put_in(document["cases"], []))

    tampered = put_in(document, ["cases", Access.at(0), "minimized"], false)
    refute :ok == Differential.verify_corpus(tampered)
  end

  test "profile publishes matrix scope and honest proof boundary" do
    assert Catena.LanguageVersion.latest() == "0.1.96"
    assert Differential.profile().version == "0.1.85"
    assert Differential.profile().maximum_scenarios == 10_000
    refute Differential.profile().agreement_is_proof
    assert Catena.ConformanceInfo.document()["differential_testing"]["version"] == "0.1.85"
  end
end
