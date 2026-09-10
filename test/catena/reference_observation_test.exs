defmodule Catena.ReferenceObservationTest do
  use ExUnit.Case, async: false

  alias Catena.Reference.Observation

  @kernel """
  (module ReferenceObservation (edition 0.1) (revision 0.1.8) (origin "test://reference-observation")
    (export value main)
    (def main (signature Int (uses)) (add 40 2)))
  """

  test "pure, effect, kernel, and schedule machines share one observation shape" do
    pure = %{
      definitions: [
        %{
          name: "main",
          expression: %{
            tag: :binary,
            operator: :add,
            left: %{tag: :integer, value: 40},
            right: %{tag: :integer, value: 2}
          }
        }
      ]
    }

    assert {:ok, %{status: :completed, value: 42, bounds: %{steps: 3}}} =
             Observation.observe(:expression, %{core: pure, entry: "main"}, fuel: 20)

    {:ok, integrated} = effect_program() |> JSON.encode!() |> Catena.check_json()

    assert {:ok, effect} =
             Observation.observe(:effect, %{core: integrated, entry: "main"},
               host_timeout_ms: 1_000
             )

    assert effect.status == :completed
    assert effect.value == 43

    assert Enum.map(effect.events, fn event ->
             if is_tuple(event), do: elem(event, 0), else: event
           end) ==
             [:handle, :request, :clause, :resume, :return]

    assert {:ok, kernel} =
             Observation.observe(:source, %{format: :kernel, text: @kernel, entry: "main"})

    assert kernel.status == :completed and kernel.value == 42
    assert [%{pid: 0, status: :terminated}] = kernel.lifetime

    {:ok, core} = Catena.check_kernel(@kernel)
    assert {:ok, schedules} = Observation.observe(:schedules, %{core: core, entry: "main"})
    assert schedules.status == :completed
    assert length(schedules.value.outcomes) == 1
  end

  test "resource and foreign adapters expose lifetime and checked boundary observations" do
    events = [
      {:open, "scope", 10},
      {:begin_acquire, "file"},
      {:acquired, "file"},
      {:use, "owner", "file"},
      {:finish, {:ok, 7}},
      {:release_result, "file", :ok}
    ]

    assert {:ok, resource} =
             Observation.observe(:resource, %{owner: "owner", events: events},
               transition_limit: 100
             )

    assert resource.status == :completed

    assert Enum.any?(resource.lifetime, fn terminal ->
             terminal.resources == %{"file" => :released} and
               terminal.scopes == %{"scope" => :closed}
           end)

    {:ok, codec} = Catena.Foreign.Codec.new({:data, {:tuple, [:integer, :text]}})
    limits = %{nodes: 8, bytes: 64, depth: 4}

    assert {:ok, %{status: :completed, value: {7, "ok"}}} =
             Observation.observe(:foreign, %{codec: codec, native: {7, "ok"}, limits: limits})

    assert {:ok, %{status: :rejected}} =
             Observation.observe(:foreign, %{codec: codec, native: {7, <<255>>}, limits: limits})
  end

  test "abstract external responses keep traps, host deadlines, and missing models distinct" do
    responses = [
      %{request: {:clock, :now}, result: {:ok, 10}},
      %{request: {:network, :read}, result: {:trap, :closed}},
      %{request: {:process, :wait}, result: :host_timeout}
    ]

    assert {:ok, %{status: :completed, value: 10}} =
             Observation.observe(:external, %{request: {:clock, :now}, responses: responses})

    assert {:ok, %{status: :trapped, reason: :closed}} =
             Observation.observe(:external, %{request: {:network, :read}, responses: responses})

    assert {:ok, %{status: :host_timeout}} =
             Observation.observe(:external, %{request: {:process, :wait}, responses: responses})

    assert {:ok, %{status: :unsupported, reason: :unmodelled_external_response}} =
             Observation.observe(:external, %{request: {:random, :bytes}, responses: responses})
  end

  test "fuel exhaustion, forged core, unknown engines, and held public source are explicit" do
    nested = %{
      tag: :binary,
      operator: :add,
      left: %{tag: :integer, value: 1},
      right: %{tag: :integer, value: 2}
    }

    pure = %{definitions: [%{name: "main", expression: nested}]}

    assert {:ok, %{status: :budget_exhausted, bounds: %{fuel: 2}}} =
             Observation.observe(:expression, %{core: pure, entry: "main"}, fuel: 2)

    forged = %{definitions: [%{name: "main", arity: 0, expression: %{tag: :forged}}]}

    assert {:ok, %{status: :rejected}} =
             Observation.observe(:kernel, %{core: forged, entry: "main"})

    assert {:ok, %{status: :unsupported, reason: :unknown_reference_engine}} =
             Observation.observe(:future_machine, %{})

    assert {:ok, %{status: :unsupported, reason: :public_source_grammar_held_for_p109}} =
             Observation.observe(:source, %{format: :public})

    assert {:error, :invalid_reference_request} =
             Observation.observe(:kernel, %{core: forged, entry: "main"}, fuel: 0)
  end

  test "profile states bounded coverage without turning agreement into proof" do
    assert Catena.LanguageVersion.latest() == "0.1.87"
    assert Observation.profile().agreement_is_proof == false
    assert Observation.profile().public_source == :held_for_p109
    assert Observation.profile().semantic_fuel == 10_000_000
    assert Catena.ConformanceInfo.document()["reference_evaluator"]["version"] == "0.1.83"
  end

  defp effect_program do
    integer = %{"tag" => "integer"}
    variable = fn name -> %{"tag" => "variable", "name" => name} end
    literal = fn value -> %{"tag" => "integer", "value" => value} end

    add = fn left, right ->
      %{"tag" => "binary", "operator" => "add", "left" => left, "right" => right}
    end

    %{
      "version" => "0.1.5",
      "origin" => "pkg://p133/effect",
      "module" => "P133Effect",
      "exports" => ["main"],
      "type_exports" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "effects" => [
        %{
          "name" => "Ask",
          "parameters" => [],
          "visibility" => "public",
          "operations" => [
            %{
              "name" => "ask",
              "parameters" => [%{"name" => "value", "type" => integer}],
              "result" => integer
            }
          ]
        }
      ],
      "handlers" => [
        %{
          "name" => "AddOne",
          "effect" => "Ask",
          "arguments" => [],
          "forall" => [],
          "visibility" => "public",
          "parameters" => [],
          "input" => integer,
          "output" => integer,
          "uses" => [],
          "return" => %{"parameter" => "result", "body" => variable.("result")},
          "operations" => [
            %{
              "operation" => "ask",
              "parameters" => ["value"],
              "resumption" => "next",
              "body" => %{
                "tag" => "resume",
                "resumption" => "next",
                "value" => add.(variable.("value"), literal.(1))
              }
            }
          ]
        }
      ],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => integer, "uses" => []},
          "body" => %{
            "tag" => "handle",
            "handler" => "AddOne",
            "arguments" => [],
            "capability" => "ask",
            "expression" => %{
              "tag" => "let",
              "name" => "answer",
              "value" => %{
                "tag" => "request",
                "effect" => "Ask",
                "operation" => "ask",
                "arguments" => [literal.(41)]
              },
              "body" => add.(variable.("answer"), literal.(1))
            }
          }
        }
      ]
    }
  end
end
