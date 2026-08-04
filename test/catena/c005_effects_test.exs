defmodule Catena.C005EffectsTest do
  use ExUnit.Case, async: false

  alias Catena.Effect.Row

  test "deep handlers resume exactly once in the reference model and generated BEAM" do
    private_handler =
      handler("PrivateAddOne", "Ask", resume("next", variable("value")))
      |> Map.put("visibility", "private")

    source =
      resumptive_program("C005Resume")
      |> update_in(["handlers"], &(&1 ++ [private_handler]))
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    assert map_size(core.effects.handlers) == 2
    main = Enum.find(core.definitions, &(&1.name == "main"))
    assert Row.equal?(main.effect_row, Row.empty())

    request = main.expression.expression.value
    assert length(request.effects.entries) == 1
    assert hd(request.effects.entries).family_name == "Ask"

    assert {{:ok, 43}, reference_trace} =
             Catena.Effect.Runtime.capture_trace(fn ->
               Catena.Reference.Evaluator.run(core, "main")
             end)

    assert {:ok, :C005Resume, binary, metadata} = Catena.compile_json(source)

    assert {:module, :C005Resume} =
             :code.load_binary(:C005Resume, ~c"c005-resume.beam", binary)

    assert {43, beam_trace} =
             Catena.Effect.Runtime.capture_trace(fn -> apply(:C005Resume, :main, []) end)

    assert beam_trace == reference_trace

    assert Enum.map(reference_trace, fn
             event when is_tuple(event) -> elem(event, 0)
             event -> event
           end) == [:handle, :request, :clause, :resume, :return]

    unload(:C005Resume)

    assert metadata.interface["version"] == "0.1.5"
    assert length(metadata.interface["effects"]) == 1
    assert length(metadata.interface["handlers"]) == 1
    assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)
    assert hd(interface.effects).name == "Ask"
    assert hd(interface.handlers).name == "AddOne"
  end

  test "a clause can abort without invoking the captured continuation" do
    program = resumptive_program("C005Abort")

    aborting_handler =
      handler("Abort", "Ask", variable("value"))

    body =
      handle(
        let_expression(
          "answer",
          request("Ask", "ask", [integer(7)]),
          binary("add", variable("answer"), integer(100))
        ),
        "Abort",
        "ask"
      )

    source =
      program
      |> Map.put("handlers", [aborting_handler])
      |> put_in(["definitions", Access.at(0), "body"], body)
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 7} = Catena.Reference.Evaluator.run(core, "main")
    assert {:ok, :C005Abort, binary, _metadata} = Catena.compile_json(source)
    assert {:module, :C005Abort} = :code.load_binary(:C005Abort, ~c"c005-abort.beam", binary)
    assert apply(:C005Abort, :main, []) == 7
    unload(:C005Abort)
  end

  test "selection rejects missing and ambiguous capabilities and accepts a qualifier" do
    missing =
      resumptive_program("C005Missing")
      |> put_in(["definitions", Access.at(0), "signature", "uses"], [])
      |> put_in(["definitions", Access.at(0), "body"], request("Ask", "ask", [integer(1)]))

    assert {:error, %{id: "EFX004"}} = Catena.check_json(JSON.encode!(missing))

    ambiguous_body =
      handle(
        handle(request("Ask", "ask", [integer(1)]), "AddOne", "inner"),
        "AddOne",
        "outer"
      )

    ambiguous =
      resumptive_program("C005Ambiguous")
      |> put_in(["definitions", Access.at(0), "body"], ambiguous_body)

    assert {:error, %{id: "EFX005"}} = Catena.check_json(JSON.encode!(ambiguous))

    qualified =
      put_in(
        ambiguous,
        ["definitions", Access.at(0), "body", "expression", "expression", "capability"],
        "outer"
      )

    assert {:ok, core} = Catena.check_json(JSON.encode!(qualified))
    assert {:ok, 2} = Catena.Reference.Evaluator.run(core, "main")

    assert {:ok, :C005Ambiguous, binary, _metadata} =
             Catena.compile_json(JSON.encode!(qualified))

    assert {:module, :C005Ambiguous} =
             :code.load_binary(:C005Ambiguous, ~c"c005-qualified.beam", binary)

    assert apply(:C005Ambiguous, :main, []) == 2
    unload(:C005Ambiguous)
  end

  test "rejects incomplete handlers and statically non-affine resumptions" do
    incomplete =
      resumptive_program("C005Incomplete")
      |> put_in(["handlers", Access.at(0), "operations"], [])

    assert {:error, %{id: "EFX006"}} = Catena.check_json(JSON.encode!(incomplete))

    double_resume =
      tuple([
        resume("next", variable("value")),
        resume("next", variable("value"))
      ])

    non_affine =
      resumptive_program("C005NonAffine")
      |> put_in(["handlers", Access.at(0), "operations", Access.at(0), "body"], double_resume)

    assert {:error, %{id: "RES002"}} = Catena.check_json(JSON.encode!(non_affine))
  end

  test "operation callbacks must have a closed empty effect row" do
    bad_callback =
      function_type(integer_type(), integer_type())
      |> Map.put("effect", [%{"effect" => "Ask"}])

    invalid =
      resumptive_program("C005EffectfulCallback")
      |> put_in(
        ["effects", Access.at(0), "operations", Access.at(0), "parameters", Access.at(0), "type"],
        bad_callback
      )

    assert {:error, %{id: "EFX002"}} = Catena.check_json(JSON.encode!(invalid))
  end

  test "effectful definitions forward their selected capability through a CPS worker" do
    ask_once =
      %{
        "name" => "ask_once",
        "parameters" => [],
        "signature" =>
          signature(integer_type())
          |> Map.put("uses", [use_entry("Ask", "ask")]),
        "body" => request("Ask", "ask", [integer(10)]) |> Map.put("capability", "ask")
      }

    main_body =
      handle(
        let_expression(
          "first",
          call(variable("ask_once"), []),
          binary("add", variable("first"), call(variable("ask_once"), []))
        ),
        "AddOne",
        "ask"
      )

    source =
      resumptive_program("C005Forward")
      |> put_in(["definitions", Access.at(0), "body"], main_body)
      |> update_in(["definitions"], &[ask_once | &1])
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    forwarded = Enum.find(core.definitions, &(&1.name == "ask_once"))
    assert length(forwarded.effect_row.entries) == 1
    assert {:ok, 22} = Catena.Reference.Evaluator.run(core, "main")

    assert {:ok, :C005Forward, binary, metadata} = Catena.compile_json(source)

    assert {:module, :C005Forward} =
             :code.load_binary(:C005Forward, ~c"c005-forward.beam", binary)

    assert apply(:C005Forward, :main, []) == 22
    assert Enum.any?(metadata.forms, &match?({:function, _, :__catena_cps_ask_once, 2, _}, &1))
    unload(:C005Forward)
  end

  test "handler clauses may request an explicitly declared outer capability" do
    ask_with_log =
      handler(
        "AskWithLog",
        "Ask",
        let_expression(
          "logged",
          request("Log", "log", [variable("value")]) |> Map.put("capability", "log"),
          resume("next", binary("add", variable("logged"), integer(1)))
        )
      )
      |> Map.put("uses", [use_entry("Log", "log")])

    log_identity =
      handler("LogIdentity", "Log", resume("next", variable("value")))
      |> put_in(["operations", Access.at(0), "operation"], "log")

    body =
      handle(
        handle(request("Ask", "ask", [integer(5)]), "AskWithLog", "ask"),
        "LogIdentity",
        "log"
      )

    source =
      resumptive_program("C005ClauseEffects")
      |> Map.put("effects", [effect("Ask", "ask"), effect("Log", "log")])
      |> Map.put("handlers", [ask_with_log, log_identity])
      |> put_in(["definitions", Access.at(0), "body"], body)
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 6} = Catena.Reference.Evaluator.run(core, "main")
    assert {:ok, :C005ClauseEffects, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005ClauseEffects} =
             :code.load_binary(:C005ClauseEffects, ~c"c005-clause-effects.beam", binary)

    assert apply(:C005ClauseEffects, :main, []) == 6
    unload(:C005ClauseEffects)
  end

  test "effect diagnostics cover type mismatch, missing return, and capability escape" do
    wrong_type =
      resumptive_program("C005WrongRequestType")
      |> put_in(
        ["definitions", Access.at(0), "body", "expression", "value", "arguments"],
        [boolean(true)]
      )

    assert {:error, %{id: "EFX007"}} = Catena.check_json(JSON.encode!(wrong_type))

    missing_return =
      resumptive_program("C005MissingReturn")
      |> update_in(["handlers", Access.at(0)], &Map.delete(&1, "return"))

    assert {:error, %{id: "EFX006"}} = Catena.check_json(JSON.encode!(missing_return))

    function_type = function_type(integer_type(), integer_type())

    escaping_handler =
      handler(
        "FunctionResult",
        "Ask",
        resume("next", variable("value"))
      )
      |> Map.put("input", function_type)
      |> Map.put("output", function_type)

    escaping_body =
      handle(
        function_expression(
          "input",
          request("Ask", "ask", [variable("input")]) |> Map.put("capability", "ask")
        ),
        "FunctionResult",
        "ask"
      )

    escaping =
      resumptive_program("C005EscapingCapability")
      |> Map.put("handlers", [escaping_handler])
      |> put_in(["definitions", Access.at(0), "signature"], signature(function_type))
      |> put_in(["definitions", Access.at(0), "body"], escaping_body)

    assert {:error, %{id: "EFX003"}} = Catena.check_json(JSON.encode!(escaping))
  end

  test "the runtime token traps before a second continuation entry" do
    parent = self()
    resumption = Catena.Effect.Runtime.new_resumption(fn value -> send(parent, {:ran, value}) end)

    assert Catena.Effect.Runtime.resume(resumption, 1) == {:ran, 1}
    assert_receive {:ran, 1}

    assert_raise RuntimeError, ~r/resumed more than once/, fn ->
      Catena.Effect.Runtime.resume(resumption, 2)
    end

    refute_receive {:ran, 2}
  end

  test "generic effects, unnamed uses, open rows, and pure direct lowering round trip" do
    store_effect = %{
      "name" => "Store",
      "parameters" => ["key", "value"],
      "visibility" => "public",
      "operations" => [
        %{
          "name" => "get",
          "parameters" => [%{"name" => "key", "type" => variable_type("key")}],
          "result" => variable_type("value")
        }
      ]
    }

    store_handler =
      handler("IntegerStore", "Store", resume("next", variable("key")))
      |> Map.put("arguments", [integer_type(), integer_type()])
      |> put_in(["operations", Access.at(0), "operation"], "get")
      |> put_in(["operations", Access.at(0), "parameters"], ["key"])

    generic_read = %{
      "name" => "generic_read",
      "parameters" => [],
      "signature" => %{
        "forall" => ["a"],
        "type" => variable_type("a"),
        "uses" => [
          %{
            "effect" => "Store",
            "arguments" => [integer_type(), variable_type("a")],
            "capability" => "store"
          }
        ]
      },
      "body" =>
        request("Store", "get", [integer(9)])
        |> Map.put("capability", "store")
    }

    main =
      definition(
        "main",
        signature(integer_type()),
        handle(call(variable("generic_read"), []), "IntegerStore", "store")
      )

    ambient_signature =
      signature(integer_type())
      |> Map.put("uses", [
        %{"effect" => "Store", "arguments" => [integer_type(), integer_type()]}
      ])
      |> Map.put("uses_tail", "rest")

    ambient =
      definition(
        "ambient",
        ambient_signature,
        request("Store", "get", [integer(3)])
      )

    forwarded_signature =
      signature(integer_type())
      |> Map.put("uses", [
        %{"effect" => "Store", "arguments" => [integer_type(), integer_type()]}
      ])
      |> Map.put("uses_tail", "forwarded_rest")

    open_forward =
      definition(
        "open_forward",
        forwarded_signature,
        call(variable("ambient"), [])
      )

    direct = definition("direct", signature(integer_type()), integer(1))

    source =
      resumptive_program("C005Generic")
      |> Map.put("exports", ["main", "ambient", "open_forward", "direct"])
      |> Map.put("effects", [store_effect])
      |> Map.put("handlers", [store_handler])
      |> Map.put("definitions", [generic_read, ambient, open_forward, main, direct])
      |> JSON.encode!()

    assert {:ok, :C005Generic, binary, metadata} = Catena.compile_json(source)

    assert {:module, :C005Generic} =
             :code.load_binary(:C005Generic, ~c"c005-generic.beam", binary)

    assert apply(:C005Generic, :main, []) == 9
    assert apply(:C005Generic, :direct, []) == 1
    unload(:C005Generic)

    assert Enum.any?(metadata.forms, &match?({:function, _, :direct, 0, _}, &1))
    refute Enum.any?(metadata.forms, &match?({:function, _, :__catena_cps_direct, _, _}, &1))

    assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)
    ambient_interface = Enum.find(interface.values, &(&1.name == "ambient"))
    assert ambient_interface.uses.tail == "rest"
    assert [entry] = ambient_interface.uses.entries
    assert entry.family_name == "Store"
    assert is_nil(entry.name)

    open_forward_interface = Enum.find(interface.values, &(&1.name == "open_forward"))
    assert open_forward_interface.uses.tail == "forwarded_rest"
  end

  test "typed-core verification rejects forged effect-row evidence" do
    assert {:ok, core} =
             resumptive_program("C005ForgedCore") |> JSON.encode!() |> Catena.check_json()

    tampered =
      update_in(
        core,
        [:definitions, Access.at(0), :expression, :expression, :value],
        fn request ->
          %{request | effects: Row.empty()}
        end
      )

    assert {:error, reason} = Catena.TypedCore.Verifier.verify(tampered)
    assert reason =~ "effect"
  end

  test "operations accept ordinary data and closed pure functions" do
    callback_type = function_type(integer_type(), integer_type())

    evaluate_effect = %{
      "name" => "Evaluate",
      "parameters" => [],
      "visibility" => "public",
      "operations" => [
        %{
          "name" => "evaluate",
          "parameters" => [
            %{"name" => "callback", "type" => callback_type},
            %{"name" => "value", "type" => integer_type()}
          ],
          "result" => integer_type()
        }
      ]
    }

    evaluate_handler =
      handler(
        "RunEvaluation",
        "Evaluate",
        resume("next", call(variable("callback"), [variable("value")]))
      )
      |> put_in(["operations", Access.at(0), "operation"], "evaluate")
      |> put_in(["operations", Access.at(0), "parameters"], ["callback", "value"])

    main = %{
      "name" => "main",
      "parameters" => ["callback"],
      "signature" => signature(function_type(callback_type, integer_type())),
      "body" =>
        handle(
          request("Evaluate", "evaluate", [variable("callback"), integer(4)]),
          "RunEvaluation",
          "evaluation"
        )
    }

    source =
      resumptive_program("C005PureCallback")
      |> Map.put("effects", [evaluate_effect])
      |> Map.put("handlers", [evaluate_handler])
      |> Map.put("definitions", [main])
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)

    assert {:ok, 12} =
             Catena.Reference.Evaluator.run(core, "main", [{:closure, &(&1 * 3)}])

    assert {:ok, :C005PureCallback, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005PureCallback} =
             :code.load_binary(:C005PureCallback, ~c"c005-pure-callback.beam", binary)

    assert apply(:C005PureCallback, :main, [&(&1 * 3)]) == 12
    unload(:C005PureCallback)
  end

  test "effectful branches preserve existing exhaustive match semantics" do
    branch = %{
      "tag" => "match",
      "scrutinee" => integer(1),
      "clauses" => [
        %{
          "pattern" => %{"tag" => "integer", "value" => 1},
          "body" => request("Ask", "ask", [integer(5)])
        },
        %{"pattern" => %{"tag" => "wildcard"}, "body" => integer(0)}
      ]
    }

    source =
      resumptive_program("C005EffectfulMatch")
      |> put_in(
        ["definitions", Access.at(0), "body"],
        handle(branch, "AddOne", "ask")
      )
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 6} = Catena.Reference.Evaluator.run(core, "main")
    assert {:ok, :C005EffectfulMatch, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005EffectfulMatch} =
             :code.load_binary(:C005EffectfulMatch, ~c"c005-effectful-match.beam", binary)

    assert apply(:C005EffectfulMatch, :main, []) == 6
    unload(:C005EffectfulMatch)
  end

  test "interfaces preserve nominal effect identities across module checking" do
    assert {:ok, :C005EffectSource, source_binary, source_metadata} =
             resumptive_program("C005EffectSource")
             |> JSON.encode!()
             |> Catena.compile_json()

    assert {:ok, interface} = Catena.Interface.decode(source_metadata.interface_binary)

    local_handler =
      handler(
        "ImportedAddOne",
        "Ask",
        resume("next", binary("add", variable("value"), integer(1)))
      )

    consumer =
      resumptive_program("C005EffectConsumer")
      |> Map.put("effects", [])
      |> Map.put("handlers", [local_handler])
      |> put_in(
        ["definitions", Access.at(0), "body"],
        handle(request("Ask", "ask", [integer(8)]), "ImportedAddOne", "ask")
      )

    assert {:ok, :C005EffectConsumer, binary, _metadata} =
             Catena.compile_json(JSON.encode!(consumer), interfaces: [interface])

    assert {:module, :C005EffectConsumer} =
             :code.load_binary(:C005EffectConsumer, ~c"c005-effect-consumer.beam", binary)

    assert apply(:C005EffectConsumer, :main, []) == 9
    unload(:C005EffectConsumer)

    imported_handler_consumer =
      consumer
      |> Map.put("module", "C005ImportedHandlerConsumer")
      |> Map.put("origin", "pkg://c005/C005ImportedHandlerConsumer")
      |> Map.put("handlers", [])
      |> put_in(
        ["definitions", Access.at(0), "body"],
        handle(request("Ask", "ask", [integer(8)]), "AddOne", "ask")
      )

    assert {:ok, :C005ImportedHandlerConsumer, imported_binary, _metadata} =
             Catena.compile_json(JSON.encode!(imported_handler_consumer), interfaces: [interface])

    assert {:module, :C005EffectSource} =
             :code.load_binary(:C005EffectSource, ~c"c005-effect-source.beam", source_binary)

    assert {:module, :C005ImportedHandlerConsumer} =
             :code.load_binary(
               :C005ImportedHandlerConsumer,
               ~c"c005-imported-handler-consumer.beam",
               imported_binary
             )

    assert apply(:C005ImportedHandlerConsumer, :main, []) == 9
    unload(:C005ImportedHandlerConsumer)
    unload(:C005EffectSource)

    conflicting = Map.put(consumer, "effects", [effect("Ask", "ask")])

    assert {:error, %{id: "EFX001"}} =
             Catena.check_json(JSON.encode!(conflicting), interfaces: [interface])
  end

  test "handler arguments evaluate left to right in the outer capability scope" do
    offset_handler =
      handler(
        "Offset",
        "Ask",
        resume("next", binary("add", variable("value"), variable("offset")))
      )
      |> Map.put("parameters", [%{"name" => "offset", "type" => integer_type()}])

    log_identity =
      handler("LogIdentity", "Log", resume("next", variable("value")))
      |> put_in(["operations", Access.at(0), "operation"], "log")

    inner =
      handle(request("Ask", "ask", [integer(1)]), "Offset", "ask")
      |> Map.put("arguments", [
        request("Log", "log", [integer(10)]) |> Map.put("capability", "log")
      ])

    body = handle(inner, "LogIdentity", "log")

    source =
      resumptive_program("C005HandlerArguments")
      |> Map.put("effects", [effect("Ask", "ask"), effect("Log", "log")])
      |> Map.put("handlers", [offset_handler, log_identity])
      |> put_in(["definitions", Access.at(0), "body"], body)
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)

    assert {{:ok, 11}, reference_trace} =
             Catena.Effect.Runtime.capture_trace(fn ->
               Catena.Reference.Evaluator.run(core, "main")
             end)

    assert {:ok, :C005HandlerArguments, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005HandlerArguments} =
             :code.load_binary(:C005HandlerArguments, ~c"c005-handler-arguments.beam", binary)

    assert {11, beam_trace} =
             Catena.Effect.Runtime.capture_trace(fn ->
               apply(:C005HandlerArguments, :main, [])
             end)

    assert beam_trace == reference_trace
    unload(:C005HandlerArguments)
  end

  test "two capabilities of one family remain distinct and subtraction removes only one" do
    pair_type = %{"tag" => "tuple", "elements" => [integer_type(), integer_type()]}

    pair_handler =
      handler("PairAsk", "Ask", resume("next", variable("value")))
      |> Map.put("input", pair_type)
      |> Map.put("output", pair_type)

    subject =
      tuple([
        request("Ask", "ask", [integer(1)]) |> Map.put("capability", "outer"),
        request("Ask", "ask", [integer(2)]) |> Map.put("capability", "inner")
      ])

    inner = handle(subject, "PairAsk", "inner")
    body = handle(inner, "PairAsk", "outer")

    source =
      resumptive_program("C005DistinctCapabilities")
      |> Map.put("handlers", [pair_handler])
      |> put_in(["definitions", Access.at(0), "signature"], signature(pair_type))
      |> put_in(["definitions", Access.at(0), "body"], body)
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    main = Enum.find(core.definitions, &(&1.name == "main"))
    assert length(main.expression.expression.expression.effects.entries) == 2
    assert length(main.expression.expression.effects.entries) == 1
    assert Row.equal?(main.effect_row, Row.empty())
    assert {:ok, {1, 2}} = Catena.Reference.Evaluator.run(core, "main")

    assert {:ok, :C005DistinctCapabilities, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005DistinctCapabilities} =
             :code.load_binary(
               :C005DistinctCapabilities,
               ~c"c005-distinct-capabilities.beam",
               binary
             )

    assert apply(:C005DistinctCapabilities, :main, []) == {1, 2}
    unload(:C005DistinctCapabilities)
  end

  test "version 0.1.5 interfaces reject duplicate nominal effect identities" do
    assert {:ok, :C005MalformedInterface, _binary, metadata} =
             resumptive_program("C005MalformedInterface")
             |> JSON.encode!()
             |> Catena.compile_json()

    duplicate =
      metadata.interface
      |> update_in(["effects"], fn [effect] -> [effect, effect] end)
      |> refresh_interface_digest()
      |> Catena.Interface.encode()

    assert {:error, %{id: "A005"}} = Catena.Interface.decode(duplicate)
  end

  test "reversing nested handlers observably reverses their return transformations" do
    add_return =
      handler("AddReturn", "Ask", resume("next", variable("value")))
      |> put_in(
        ["return", "body"],
        binary("add", variable("result"), integer(100))
      )

    double_return =
      handler("DoubleReturn", "Ask", resume("next", variable("value")))
      |> put_in(
        ["return", "body"],
        binary("multiply", variable("result"), integer(2))
      )

    add_outside =
      handle(
        handle(
          request("Ask", "ask", [integer(1)]) |> Map.put("capability", "inner"),
          "DoubleReturn",
          "inner"
        ),
        "AddReturn",
        "outer"
      )

    double_outside =
      handle(
        handle(
          request("Ask", "ask", [integer(1)]) |> Map.put("capability", "inner"),
          "AddReturn",
          "inner"
        ),
        "DoubleReturn",
        "outer"
      )

    source =
      resumptive_program("C005HandlerOrder")
      |> Map.put("exports", ["add_outside", "double_outside"])
      |> Map.put("handlers", [add_return, double_return])
      |> Map.put("definitions", [
        definition("add_outside", signature(integer_type()), add_outside),
        definition("double_outside", signature(integer_type()), double_outside)
      ])
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 102} = Catena.Reference.Evaluator.run(core, "add_outside")
    assert {:ok, 202} = Catena.Reference.Evaluator.run(core, "double_outside")
    assert {:ok, :C005HandlerOrder, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005HandlerOrder} =
             :code.load_binary(:C005HandlerOrder, ~c"c005-handler-order.beam", binary)

    assert apply(:C005HandlerOrder, :add_outside, []) == 102
    assert apply(:C005HandlerOrder, :double_outside, []) == 202
    unload(:C005HandlerOrder)
  end

  test "affine checking permits one resume on each mutually exclusive branch" do
    branch_resume = %{
      "tag" => "match",
      "scrutinee" => variable("value"),
      "clauses" => [
        %{
          "pattern" => %{"tag" => "integer", "value" => 0},
          "body" => resume("next", integer(0))
        },
        %{
          "pattern" => %{"tag" => "wildcard"},
          "body" => resume("next", variable("value"))
        }
      ]
    }

    branching_handler = handler("Branching", "Ask", branch_resume)

    source =
      resumptive_program("C005BranchAffine")
      |> Map.put("handlers", [branching_handler])
      |> put_in(
        ["definitions", Access.at(0), "body"],
        handle(request("Ask", "ask", [integer(4)]), "Branching", "ask")
      )
      |> JSON.encode!()

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 4} = Catena.Reference.Evaluator.run(core, "main")
    assert {:ok, :C005BranchAffine, binary, _metadata} = Catena.compile_json(source)

    assert {:module, :C005BranchAffine} =
             :code.load_binary(:C005BranchAffine, ~c"c005-branch-affine.beam", binary)

    assert apply(:C005BranchAffine, :main, []) == 4
    unload(:C005BranchAffine)
  end

  defp resumptive_program(module) do
    %{
      "version" => "0.1.5",
      "origin" => "pkg://c005/#{module}",
      "module" => module,
      "exports" => ["main"],
      "type_exports" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "effects" => [effect("Ask", "ask")],
      "handlers" => [
        handler(
          "AddOne",
          "Ask",
          resume("next", binary("add", variable("value"), integer(1)))
        )
      ],
      "definitions" => [
        definition(
          "main",
          signature(integer_type()),
          handle(
            let_expression(
              "answer",
              request("Ask", "ask", [integer(41)]),
              binary("add", variable("answer"), integer(1))
            ),
            "AddOne",
            "ask"
          )
        )
      ]
    }
  end

  defp effect(name, operation) do
    %{
      "name" => name,
      "parameters" => [],
      "visibility" => "public",
      "operations" => [
        %{
          "name" => operation,
          "parameters" => [%{"name" => "value", "type" => integer_type()}],
          "result" => integer_type()
        }
      ]
    }
  end

  defp handler(name, effect, operation_body) do
    %{
      "name" => name,
      "effect" => effect,
      "arguments" => [],
      "forall" => [],
      "visibility" => "public",
      "parameters" => [],
      "input" => integer_type(),
      "output" => integer_type(),
      "uses" => [],
      "return" => %{"parameter" => "result", "body" => variable("result")},
      "operations" => [
        %{
          "operation" => "ask",
          "parameters" => ["value"],
          "resumption" => "next",
          "body" => operation_body
        }
      ]
    }
  end

  defp definition(name, signature, body),
    do: %{"name" => name, "parameters" => [], "signature" => signature, "body" => body}

  defp signature(type), do: %{"forall" => [], "type" => type, "uses" => []}
  defp integer_type, do: %{"tag" => "integer"}
  defp variable_type(name), do: %{"tag" => "variable", "name" => name}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp function_expression(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp use_entry(effect, capability),
    do: %{"effect" => effect, "arguments" => [], "capability" => capability}

  defp binary(operator, left, right),
    do: %{"tag" => "binary", "operator" => operator, "left" => left, "right" => right}

  defp tuple(elements), do: %{"tag" => "tuple", "elements" => elements}

  defp let_expression(name, value, body),
    do: %{"tag" => "let", "name" => name, "value" => value, "body" => body}

  defp request(effect, operation, arguments),
    do: %{
      "tag" => "request",
      "effect" => effect,
      "operation" => operation,
      "arguments" => arguments
    }

  defp resume(resumption, value),
    do: %{"tag" => "resume", "resumption" => resumption, "value" => value}

  defp handle(expression, handler, capability),
    do: %{
      "tag" => "handle",
      "expression" => expression,
      "handler" => handler,
      "arguments" => [],
      "capability" => capability
    }

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defp refresh_interface_digest(interface) do
    payload = Map.delete(interface, "digest")

    digest =
      :crypto.hash(:sha256, Catena.CanonicalJSON.encode(payload))
      |> Base.encode16(case: :lower)

    Map.put(payload, "digest", digest)
  end
end
