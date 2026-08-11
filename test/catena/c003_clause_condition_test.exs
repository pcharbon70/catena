defmodule Catena.C003ClauseConditionTest do
  use ExUnit.Case, async: false

  alias Catena.{CanonicalJSON, Interface}
  alias Catena.Backend.ErlangAbstract
  alias Catena.Reference.Evaluator

  @tag obligations:
         ~w(CC-OBL-001 CC-OBL-003 CC-OBL-004 CC-OBL-007 CC-OBL-008 CC-OBL-009 CC-OBL-026 CC-OBL-027 CC-OBL-030 CC-OBL-035 CC-OBL-036 CC-OBL-037 CC-OBL-041)
  test "checks exhaustive integer condition partitions and lowers them both ways" do
    source = partition_program("ConditionPartitions") |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    classify = Enum.find(core.definitions, &(&1.name == "classify"))
    match_expression = classify.expression.body
    assert classify.clause_definition?
    assert match_expression.decision_tree.tag == :ordered_guard_tree
    assert Enum.all?(match_expression.clauses, &Map.has_key?(&1, :fact_evidence))

    for {value, expected} <- [{-9, -1}, {0, 0}, {12, 1}] do
      assert {:ok, ^expected} = Evaluator.run(core, "classify", [value])
    end

    lowered =
      for lowering <- [:native, :ordinary], into: %{} do
        assert {:ok, :ConditionPartitions, binary, metadata} =
                 Catena.compile_json(source, condition_lowering: lowering)

        assert metadata.condition_lowering == lowering

        assert {:module, :ConditionPartitions} =
                 :code.load_binary(:ConditionPartitions, ~c"condition-partitions.beam", binary)

        assert apply(:ConditionPartitions, :classify, [-4]) == -1
        assert apply(:ConditionPartitions, :classify, [0]) == 0
        assert apply(:ConditionPartitions, :classify, [4]) == 1
        unload(:ConditionPartitions)

        assert {:ok, {:ConditionPartitions, [compile_info: compile_info]}} =
                 :beam_lib.chunks(binary, [:compile_info])

        assert Keyword.get(compile_info, :catena_specification) == ~c"0.1.3"
        assert Keyword.get(compile_info, :catena_frontend) == ~c"json-ast-0.1.3"
        {lowering, metadata.forms}
      end

    refute lowered.native == lowered.ordinary
  end

  @tag obligations:
         ~w(CC-OBL-014 CC-OBL-015 CC-OBL-017 CC-OBL-019 CC-OBL-024 CC-OBL-043 CC-OBL-046 CC-OBL-047 CC-OBL-049)
  test "requires safe, typed, acyclic condition declarations" do
    unsafe =
      partition_program("UnsafeCondition")
      |> put_in(
        ["definitions", Access.at(0), "body"],
        call(variable("ordinary"), [variable("x")])
      )

    assert {:error, %{id: "CND003"}} = Catena.check_json(JSON.encode!(unsafe))

    cycle =
      module_03("ConditionCycle", [], [
        condition("first", ["x"], int_to_bool(), call(variable("second"), [variable("x")])),
        condition("second", ["x"], int_to_bool(), call(variable("first"), [variable("x")]))
      ])

    assert {:error, %{id: "CND004"}} = Catena.check_json(JSON.encode!(cycle))

    non_boolean = condition("bad", ["x"], int_to_int(), variable("x"))

    assert {:error, %{id: "CND002"}} =
             Catena.check_json(JSON.encode!(module_03("BadResult", [], [non_boolean])))

    assert {:error, %{id: "CND007", details: %{minimum_budget: 20_000}}} =
             Catena.check_json(JSON.encode!(partition_program("SmallBudget")),
               condition_budget: 19_999
             )

    non_boolean_guard =
      module_03("NonBooleanGuard", ["bad"], [
        clause_definition("bad", int_to_int(), [
          definition_clause([bind("value")], variable("value"), integer(1)),
          definition_clause(
            [bind("value")],
            binary(:equal, variable("value"), integer(0)),
            integer(0)
          )
        ])
      ])

    assert {:error, %{id: "CND002"}} = Catena.check_json(JSON.encode!(non_boolean_guard))
  end

  @tag obligations: ~w(CC-OBL-002 CC-OBL-022 CC-OBL-023 CC-OBL-029 CC-OBL-042)
  test "fact reasoning proves redundancy but remains conservative outside its theory" do
    redundant =
      module_03("RedundantFacts", ["classify"], [
        clause_definition("classify", int_to_int(), [
          definition_clause([bind("x")], binary(:greater, variable("x"), integer(0)), integer(1)),
          definition_clause([bind("x")], binary(:greater, variable("x"), integer(1)), integer(2)),
          definition_clause(
            [bind("x")],
            unary(:not, binary(:greater, variable("x"), integer(0))),
            integer(0)
          )
        ])
      ])

    assert {:error, %{id: "M002"}} = Catena.check_json(JSON.encode!(redundant))

    square = binary(:multiply, variable("x"), variable("x"))

    unsupported =
      module_03("UnknownFacts", ["classify"], [
        clause_definition("classify", int_to_int(), [
          definition_clause([bind("x")], binary(:greater, square, integer(0)), integer(1)),
          definition_clause([bind("x")], binary(:less_equal, square, integer(0)), integer(0))
        ])
      ])

    assert {:error, %{id: "M001"}} = Catena.check_json(JSON.encode!(unsupported))
  end

  @tag obligations: ~w(CC-OBL-005 CC-OBL-006 CC-OBL-012 CC-OBL-044 CC-OBL-045)
  test "exports canonical condition evidence and imports it explicitly" do
    producer =
      module_03("PredicateSource", ["positive"], [
        condition(
          "positive",
          ["x"],
          int_to_bool(),
          binary(:greater, variable("x"), integer(0))
        )
      ])

    assert {:ok, :PredicateSource, producer_binary, metadata} =
             Catena.compile_json(JSON.encode!(producer))

    assert metadata.interface["version"] == "0.1.3"
    assert get_in(metadata.interface, ["values", Access.at(0), "condition", "expanded_core"])
    assert {:ok, interface} = Interface.decode(metadata.interface_binary)
    assert hd(interface.values).condition.id == "PredicateSource.positive"

    consumer =
      module_03("PredicateConsumer", ["classify", "direct"], [
        clause_definition("classify", int_to_int(), [
          definition_clause(
            [bind("value")],
            call(variable("is_positive"), [variable("value")]),
            integer(1)
          ),
          definition_clause(
            [bind("value")],
            unary(:not, call(variable("is_positive"), [variable("value")])),
            integer(0)
          )
        ]),
        definition(
          "direct",
          ["value"],
          int_to_bool(),
          call(variable("is_positive"), [variable("value")])
        )
      ])
      |> Map.put("imports", [
        %{
          "kind" => "condition",
          "condition" => "PredicateSource.positive",
          "as" => "is_positive"
        }
      ])

    for lowering <- [:native, :ordinary] do
      assert {:ok, :PredicateConsumer, binary, _metadata} =
               Catena.compile_json(JSON.encode!(consumer),
                 interfaces: [interface],
                 condition_lowering: lowering
               )

      assert {:module, :PredicateSource} =
               :code.load_binary(:PredicateSource, ~c"predicate-source.beam", producer_binary)

      assert {:module, :PredicateConsumer} =
               :code.load_binary(:PredicateConsumer, ~c"predicate-consumer.beam", binary)

      assert apply(:PredicateConsumer, :classify, [2]) == 1
      assert apply(:PredicateConsumer, :classify, [-2]) == 0
      assert apply(:PredicateConsumer, :direct, [2])
      refute apply(:PredicateConsumer, :direct, [-2])
      unload(:PredicateConsumer)
      unload(:PredicateSource)
    end
  end

  @tag obligations: ~w(CC-OBL-020 CC-OBL-028)
  test "rejects tampered nested condition evidence independently of the interface digest" do
    assert {:ok, :EvidenceSource, _binary, metadata} =
             partition_program("EvidenceSource") |> JSON.encode!() |> Catena.compile_json()

    tampered_interface =
      Map.update!(metadata.interface, "values", fn values ->
        Enum.map(values, fn
          %{"name" => "positive", "condition" => condition} = value ->
            %{value | "condition" => Map.put(condition, "native", false)}

          value ->
            value
        end)
      end)

    tampered =
      tampered_interface
      |> refresh_interface_digest()
      |> Interface.encode()

    assert {:error, %{id: "CND005"}} = Interface.decode(tampered)
  end

  @tag obligations: ~w(CC-OBL-013 CC-OBL-025 CC-OBL-031)
  test "receive harness accepts only native conditions over a closed message type" do
    assert {:ok, core} =
             partition_program("ReceiveHarness") |> JSON.encode!() |> Catena.check_json()

    classify = Enum.find(core.definitions, &(&1.name == "classify"))
    clauses = classify.expression.body.clauses

    assert {:receive, _, receive_clauses} =
             ErlangAbstract.lower_receive!(core, clauses, message_type: :integer)

    assert length(receive_clauses) == 3
    assert Enum.all?(receive_clauses, fn {:clause, _, _, guards, _} -> guards != [] end)

    assert_raise Catena.TypeError, fn ->
      ErlangAbstract.lower_receive!(core, clauses, message_type: {:var, 0})
    end
  end

  @tag obligations: ~w(CC-OBL-014 CC-OBL-017 CC-OBL-018 CC-OBL-021)
  test "rejects unsupported partial and higher-order condition forms" do
    lambda = %{"tag" => "function", "parameter" => "y", "body" => variable("y")}

    bad = condition("bad", ["x"], int_to_bool(), call(lambda, [variable("x")]))

    assert {:error, %{id: "CND003"}} =
             Catena.check_json(JSON.encode!(module_03("HigherOrderCondition", [], [bad])))

    malformed =
      module_03("BadOperator", [], [])
      |> Map.put("definitions", [
        condition("bad", ["x"], int_to_bool(), %{
          "tag" => "binary",
          "operator" => "divide",
          "left" => variable("x"),
          "right" => integer(2)
        })
      ])

    assert {:error, %{id: "CND001"}} = Catena.check_json(JSON.encode!(malformed))

    old_ast = %{partition_program("OldConditionOperator") | "version" => "0.1.2"}
    assert {:error, %{id: "CND001"}} = Catena.check_json(JSON.encode!(old_ast))
  end

  @tag obligations: ~w(CC-OBL-016)
  test "condition signatures reject a nonempty effect" do
    effectful_signature =
      function_type(integer_type(), boolean_type())
      |> Map.put("effect", [%{"effect" => "Ask"}])

    bad = condition("bad", ["x"], forall(effectful_signature), variable("x"))

    assert {:error, %{id: "CND002"}} =
             Catena.check_json(JSON.encode!(module_03("EffectfulCondition", [], [bad])))
  end

  @tag obligations: ~w(CC-OBL-034)
  test "ordinary match expressions must be exhaustive" do
    non_exhaustive =
      definition(
        "bad",
        ["value"],
        forall(function_type(boolean_type(), integer_type())),
        match_expr(variable("value"), [
          match_clause(%{"tag" => "boolean", "value" => true}, integer(1))
        ])
      )

    assert {:error, %{id: "M001"}} =
             Catena.check_json(
               JSON.encode!(module_03("NonExhaustiveMatch", ["bad"], [non_exhaustive]))
             )
  end

  defp partition_program(module) do
    positive =
      condition(
        "positive",
        ["x"],
        int_to_bool(),
        binary(:greater, variable("x"), integer(0))
      )

    classify =
      clause_definition("classify", int_to_int(), [
        definition_clause(
          [bind("value")],
          call(variable("positive"), [variable("value")]),
          integer(1)
        ),
        definition_clause(
          [bind("value")],
          binary(:equal, variable("value"), integer(0)),
          integer(0)
        ),
        definition_clause(
          [bind("value")],
          binary(:less, variable("value"), integer(0)),
          integer(-1)
        )
      ])

    module_03(module, ["positive", "classify"], [positive, classify])
  end

  defp module_03(module, exports, definitions) do
    %{
      "version" => "0.1.3",
      "origin" => "test://c003",
      "module" => module,
      "type_groups" => [],
      "type_exports" => [],
      "imports" => [],
      "exports" => exports,
      "definitions" => definitions
    }
  end

  defp condition(name, parameters, signature, body),
    do: %{
      "kind" => "condition",
      "name" => name,
      "parameters" => parameters,
      "signature" => signature,
      "body" => body
    }

  defp clause_definition(name, signature, clauses),
    do: %{"name" => name, "signature" => signature, "clauses" => clauses}

  defp definition(name, parameters, signature, body),
    do: %{
      "name" => name,
      "parameters" => parameters,
      "signature" => signature,
      "body" => body
    }

  defp definition_clause(patterns, guard, body),
    do: %{"patterns" => patterns, "guard" => guard, "body" => body}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}
  defp bind(name), do: %{"tag" => "bind", "name" => name}

  defp unary(operator, operand),
    do: %{"tag" => "unary", "operator" => to_string(operator), "operand" => operand}

  defp binary(operator, left, right),
    do: %{
      "tag" => "binary",
      "operator" => to_string(operator),
      "left" => left,
      "right" => right
    }

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp match_expr(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp match_clause(pattern, body),
    do: %{"pattern" => pattern, "body" => body}

  defp forall(type), do: %{"forall" => [], "type" => type}
  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result, "effect" => []}

  defp int_to_bool, do: forall(function_type(integer_type(), boolean_type()))
  defp int_to_int, do: forall(function_type(integer_type(), integer_type()))

  defp refresh_interface_digest(interface) do
    payload = Map.delete(interface, "digest")
    digest = :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)
    Map.put(payload, "digest", digest)
  end

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
