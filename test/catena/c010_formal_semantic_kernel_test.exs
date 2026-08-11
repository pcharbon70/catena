defmodule Catena.C010FormalSemanticKernelTest do
  use ExUnit.Case, async: false

  alias Catena.Kernel.{Explorer, Interface, Parser, SExpression, Stepper, Verifier}

  @fixture Path.expand("../fixtures/c010-kernel.catena", __DIR__)

  @tag obligations: ~w(FK-OBL-003 FK-OBL-007)
  test "the exact S-expression envelope preserves spans and rejects malformed input" do
    source = File.read!(@fixture)

    assert {:ok, module} = Parser.parse(source, source: @fixture)
    assert module.module == "C010Fixture"
    assert module.edition == "0.1"
    assert module.language_revision == "0.1.8"
    assert module.span.byte_start == 0
    assert module.span.byte_end == byte_size(String.trim_trailing(source))
    assert module.span.line_start == 1
    assert module.span.column_start == 1

    crlf = String.replace(source, "\n", "\r\n")
    assert {:ok, crlf_module} = Parser.parse(crlf)
    assert crlf_module.module == module.module

    assert {:error, %{id: "SYN001", span: %Catena.SourceSpan{}}} =
             Parser.parse(source <> " trailing")

    assert {:error, %{id: "SYN001"}} = Parser.parse("\uFEFF" <> source)

    assert {:error, %{id: "SYN001"}} =
             Parser.parse(String.replace(source, "Selective", "Sélective"))

    assert {:error, %{id: "SYN001"}} =
             Parser.parse(String.replace(source, "\n", "\r", global: false))

    assert {:error, %{id: "SYN001"}} = Parser.parse(<<255, 254, 0>>)

    malformed = String.replace(source, "(origin \"test://c010-fixture\")", "(origin \"\\q\")")
    assert {:error, %{id: "SYN001"}} = Parser.parse(malformed)

    unknown = String.replace(source, "(export value main)", "(mystery value main)")
    assert {:error, %{id: "SYN002"}} = Parser.parse(unknown)

    assert {:error, %{id: "SYN002"}} =
             source |> String.replace("test://c010-fixture", "") |> Parser.parse()

    duplicate_export =
      String.replace(source, "(export value main)", "(export value main) (export value main)")

    assert {:error, %{id: "T001"}} = Parser.parse(duplicate_export)
  end

  @tag obligations: ~w(FK-OBL-007)
  test "parser node and nesting limits are distinct from malformed syntax" do
    assert {:error, %{id: "SYN003"}} = SExpression.parse("(a b c)", node_limit: 3)
    assert {:error, %{id: "SYN003"}} = SExpression.parse("(a (b (c)))", depth_limit: 2)
    assert {:ok, _node} = SExpression.parse("(a (b (c)))", node_limit: 10, depth_limit: 3)
  end

  @tag obligations: ~w(FK-OBL-002 FK-OBL-015)
  test "kernel selection is exact and JSON frontends remain bounded at 0.1.7" do
    source = File.read!(@fixture)

    assert {:ok, _core} =
             Catena.check_kernel(source,
               language_selection: %{
                 edition: "0.1",
                 language_revision: "0.1.8",
                 previews: []
               }
             )

    assert {:error, %{id: "EDN001", span: %Catena.SourceSpan{}}} =
             Catena.check_kernel(source,
               language_selection: %{
                 edition: "0.1",
                 language_revision: "0.1.7",
                 previews: []
               }
             )

    json = %{"version" => "0.1.8", "module" => "NotKernel", "exports" => [], "definitions" => []}
    assert {:error, %{id: "T012", path: "$.version"}} = Catena.check_json(JSON.encode!(json))
  end

  @tag obligations: ~w(FK-OBL-001 FK-OBL-004 FK-OBL-008 FK-OBL-009)
  test "rows, closed variants, strict order, and integrated core evidence check together" do
    assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
    assert core.profile == :formal_semantic_kernel
    assert core.exports.values == ["main", "loop", "launch"]
    assert core.exports.types == ["Option"]
    assert core.exports.processes == ["Selective"]
    assert :ok = Verifier.verify(core)

    assert {:ok, {2, true, 5}, outcome} = Stepper.run(core, "main")
    assert outcome.root_status == :terminated

    assert Enum.map(outcome.trace, & &1.label) == [
             :handle,
             :request,
             :resume,
             :effect_return,
             :return
           ]

    assert List.last(outcome.trace) == %{label: :return, pid: 0, value: {2, true, 5}}

    record_source =
      minimal_module(
        "RecordResult",
        "(Record (row (field answer Int) (field ready Bool)))",
        "(record (field answer 1) (field ready true))"
      )

    assert {:ok, record_core} = Catena.check_kernel(record_source)
    assert {:ok, %{answer: 1, ready: true}, _outcome} = Stepper.run(record_core, "main")
    assert {:ok, record_module, record_binary, _metadata} = Catena.compile_kernel(record_source)
    load!(record_module, record_binary)
    assert apply(record_module, :main, []) == %{answer: 1, ready: true}
    unload(record_module)

    variant_source =
      minimal_module(
        "VariantResult",
        "(Variant (row (field left Int) (field right Bool)))",
        "(annotate (inject left 1) (Variant (row (field left Int) (field right Bool))))"
      )

    expected_variant = {:catena_variant, :left, 1}
    assert {:ok, variant_core} = Catena.check_kernel(variant_source)
    assert {:ok, ^expected_variant, _outcome} = Stepper.run(variant_core, "main")

    assert {:ok, variant_module, variant_binary, _metadata} =
             Catena.compile_kernel(variant_source)

    load!(variant_module, variant_binary)
    assert apply(variant_module, :main, []) == expected_variant
    unload(variant_module)

    duplicate =
      minimal_module(
        "DuplicateRow",
        "Int",
        "(select (record (field value 1) (field value 2)) value)"
      )

    assert {:error, %{id: "T005"}} = Catena.check_kernel(duplicate)

    missing = minimal_module("MissingField", "Int", "(select (record (field value 1)) absent)")
    assert {:error, %{id: "T005"}} = Catena.check_kernel(missing)

    open_extension =
      minimal_module(
        "OpenExtension",
        "(Fn (Record (row (tail r))) (effects) (Record (row (field value Int) (tail r))))",
        "(fn (record_value (Record (row (tail r)))) (extend (var record_value) value 1))"
      )

    assert {:error, %{id: "T005"}} = Catena.check_kernel(open_extension)

    open_match =
      minimal_module(
        "OpenVariant",
        "Int",
        "(match (inject left 1) (case (variant left (bind value)) (var value)))"
      )

    assert {:error, %{id: "M001"}} = Catena.check_kernel(open_match)

    guarded_catch_all =
      minimal_module(
        "GuardedCatchAll",
        "Int",
        "(match true (case _ (when false) 1))"
      )

    assert {:error, %{id: "M001"}} = Catena.check_kernel(guarded_catch_all)
  end

  @tag obligations: ~w(FK-OBL-008 FK-OBL-010)
  test "regular nominal data is typed, exhaustive, sendable, and fixed-layout" do
    source = """
    (module Nominal
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://nominal")
      (export type Option)
      (export value main)
      (data Option
        (params a)
        (constructor None (fields))
        (constructor Some (fields a)))
      (def main (signature (Option Int) (uses)) (construct Some 7)))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    expected = {:catena_constructor, :Some, {7}}
    assert {:ok, ^expected, _outcome} = Stepper.run(core, "main")

    assert {:ok, module, binary, metadata} = Catena.compile_kernel(source)
    assert [%{"name" => "Option"}] = metadata.interface["types"]
    load!(module, binary)
    assert apply(module, :main, []) == expected
    unload(module)

    non_exhaustive =
      String.replace(
        source,
        "(construct Some 7)",
        "(match (construct Some 7) (case (constructor Some (bind value)) (construct Some (var value))))"
      )

    assert {:error, %{id: "M001"}} = Catena.check_kernel(non_exhaustive)

    nested_partial = """
    (module NestedPartial
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://nested-partial")
      (data Choice
        (params)
        (constructor Empty (fields))
        (constructor Flag (fields Bool)))
      (export type Choice)
      (export value main)
      (def main (signature Int (uses))
        (match (construct Flag false)
          (case (constructor Empty) 0)
          (case (constructor Flag true) 1))))
    """

    assert {:error, %{id: "M001"}} = Catena.check_kernel(nested_partial)

    wrong_arity = String.replace(source, "(construct Some 7)", "(construct Some)")
    assert {:error, %{id: "A002"}} = Catena.check_kernel(wrong_arity)

    private_type = String.replace(source, "  (export type Option)\n", "")
    assert {:error, %{id: "A002"}} = Catena.check_kernel(private_type)

    non_sendable = """
    (module NominalBoundary
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://nominal-boundary")
      (data Box (params a) (constructor Boxed (fields a)))
      (process Bad
        (mailbox (Box (Fn Int (effects) Int)))
        (params)
        (unit)))
    """

    assert {:error, %{id: "PRC002"}} = Catena.check_kernel(non_sendable)
  end

  @tag obligations: ~w(FK-OBL-009 FK-OBL-011)
  test "trait evidence and deep affine handling are integrated and erased" do
    assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
    choose = Enum.find(core.definitions, &(&1.name == "choose_variant"))
    assert choose.uses == []

    assert get_in(choose.expression, [:expression, :payload, :selected_handler, :name]) ==
             "AddOne"

    unwrap_option = Enum.find(core.definitions, &(&1.name == "unwrap_option"))

    trait_call =
      get_in(unwrap_option.expression, [
        :body,
        :clauses,
        Access.at(1),
        :body
      ])

    assert trait_call.tag == :trait_call
    assert trait_call.selected_definition == "increment_int"
    assert trait_call.selected_instance == %{trait: "Increment", head: :integer}

    assert {:ok, {2, true, 5}, reference} = Stepper.run(core, "main")

    assert Enum.map(reference.trace, & &1.label) |> Enum.take(4) == [
             :handle,
             :request,
             :resume,
             :effect_return
           ]

    assert {:ok, module, binary, metadata} =
             @fixture |> File.read!() |> Catena.compile_kernel(source: @fixture)

    assert Enum.any?(
             metadata.forms,
             &match?({:function, _, :__catena_kernel_cps_choose_variant, 2, _}, &1)
           )

    load!(module, binary)
    assert apply(module, :main, []) == {2, true, 5}
    unload(module)

    doubled =
      File.read!(@fixture)
      |> String.replace(
        "(resume next (add (var value) 1))",
        "(tuple (resume next (var value)) (resume next (var value)))"
      )

    assert {:error, %{id: "RES002"}} = Catena.check_kernel(doubled)

    missing_instance =
      File.read!(@fixture)
      |> String.replace("(instance Increment Int\n    (method increment increment_int))", "")

    assert {:error, %{id: "TRT005"}} = Catena.check_kernel(missing_instance)

    unhandled =
      minimal_module_with_declarations(
        "Unhandled",
        "(effect Ask (operation ask (params Int) Int))",
        "Int",
        "Ask",
        "(request Ask ask 1)"
      )

    assert {:ok, unhandled_core} = Catena.check_kernel(unhandled)

    assert {:trap, {:unhandled_effect, "Ask", "ask"}, _outcome} =
             Stepper.run(unhandled_core, "main")

    assert {:ok, unhandled_module, unhandled_binary, _metadata} =
             Catena.compile_kernel(unhandled)

    load!(unhandled_module, unhandled_binary)

    assert catch_error(apply(unhandled_module, :main, [])) ==
             {:catena_trap, {:unhandled_effect, "Ask", "ask"}}

    unload(unhandled_module)

    global_request = """
    (module GlobalRequest
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://global-request")
      (export value main)
      (effect Ask (operation ask (params Int) Int))
      (handler AddOne
        (effect Ask)
        (input Int)
        (output Int)
        (return result (var result))
        (operation ask (params (value Int)) (resume next)
          (resume next (add (var value) 1))))
      (def requested (signature Int (uses Ask)) (request Ask ask 41))
      (def main (signature Int (uses)) (handle AddOne (var requested))))
    """

    assert {:ok, global_core} = Catena.check_kernel(global_request)
    assert {:ok, 42, _outcome} = Stepper.run(global_core, "main")

    assert {:ok, global_module, global_binary, _metadata} =
             Catena.compile_kernel(global_request)

    load!(global_module, global_binary)
    assert apply(global_module, :main, []) == 42
    unload(global_module)
  end

  @tag obligations: ~w(FK-OBL-005 FK-OBL-012)
  test "proper tail calls agree between the stepper and generated BEAM" do
    assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
    assert {:ok, 250, _outcome} = Stepper.run(core, "loop", [250, 0], budget: 10_000)

    assert {:ok, module, binary, metadata} =
             @fixture |> File.read!() |> Catena.compile_kernel(source: @fixture)

    assert module == :C010Fixture
    assert metadata.layout == :fixed
    assert metadata.selection.language_revision == "0.1.8"
    assert metadata.warnings == []
    assert Enum.any?(metadata.forms, &match?({:function, _, :loop, 2, _}, &1))

    load!(module, binary)
    assert apply(module, :main, []) == {2, true, 5}
    assert apply(module, :loop, [250_000, 0]) == 250_000
    unload(module)
  end

  @tag obligations: ~w(FK-OBL-009 FK-OBL-011)
  test "selective receive preserves skipped messages and process traps stay local" do
    assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
    assert {:ok, :unit, outcome} = Stepper.run(core, "launch")

    assert Enum.map(outcome.trace, & &1.label) == [
             :spawn,
             :send,
             :send,
             :return,
             :receive,
             :return
           ]

    assert Enum.filter(outcome.trace, &(&1.label == :send)) |> Enum.map(& &1.message) == [
             {:catena_constructor, :Some, {0}},
             {:catena_constructor, :Some, {1}}
           ]

    assert [%{message: {:catena_constructor, :Some, {1}}}] =
             Enum.filter(outcome.trace, &(&1.label == :receive))

    assert Enum.all?(outcome.processes, &(&1.status == :terminated))
    assert Enum.all?(outcome.processes, &(&1.mailbox == []))

    assert {:ok, module, binary, _metadata} =
             @fixture |> File.read!() |> Catena.compile_kernel(source: @fixture)

    load!(module, binary)
    pid = apply(module, :__catena_spawn_Selective, [])
    monitor = Process.monitor(pid)
    send(pid, {:catena_constructor, :Some, {0}})
    send(pid, {:catena_constructor, :Some, {1}})
    assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}, 1_000
    unload(module)

    trapped = """
    (module TrapChild
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://trap-child")
      (export value main)
      (process Fail (mailbox Unit) (params) (trap 9))
      (def main (signature Unit (uses Process))
        (let child (spawn Fail) (unit))))
    """

    assert {:ok, trapped_core} = Catena.check_kernel(trapped)
    assert {:ok, :unit, trapped_outcome} = Stepper.run(trapped_core, "main")
    assert Enum.any?(trapped_outcome.processes, &(&1.status == :trapped and &1.trap == 9))
    assert Enum.find(trapped_outcome.processes, &(&1.pid == 0)).status == :terminated
  end

  @tag obligations: ~w(FK-OBL-011)
  test "dead-target send drops the message and waiting configurations are quiescent" do
    source = """
    (module Lifetime
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://lifetime")
      (export value deliver)
      (export value wait)
      (process Done (mailbox Int) (params) (unit))
      (process Waiting (mailbox Int) (params) (receive (case (bind value) (unit))))
      (def deliver (signature Unit (uses Process))
        (let target (spawn Done) (send (var target) 7)))
      (def wait (signature Unit (uses Process))
        (let target (spawn Waiting) (unit))))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, configuration} = Stepper.initial(core, "deliver")

    configuration =
      step_until(configuration, &Enum.any?(&1.trace, fn event -> event.label == :spawn end), 50)

    assert Map.has_key?(configuration.processes, 1)
    {:ok, configuration} = Stepper.step(configuration, 1)
    {:ok, configuration} = Stepper.step(configuration, 1)
    assert configuration.processes[1].status == :terminated

    assert {:ok, :unit, outcome} = Stepper.run_configuration(configuration)
    assert Enum.any?(outcome.trace, &(&1.label == :send and &1.message == 7))
    assert Enum.find(outcome.processes, &(&1.pid == 1)).mailbox == []

    assert {:quiescent, waiting} = Stepper.run(core, "wait")
    assert Enum.any?(waiting.processes, &(&1.status == :waiting))
  end

  @tag obligations: ~w(FK-OBL-011 FK-OBL-014)
  test "bounded exploration admits both cross-sender receive orders" do
    source = """
    (module Schedules
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://schedules")
      (export value main)
      (process Sink (mailbox Int) (params)
        (receive (case (bind message) (unit))))
      (process Sender (mailbox Unit) (params (target (Process Int)) (message Int))
        (send (var target) (var message)))
      (def main (signature Unit (uses Process))
        (let target (spawn Sink)
          (sequence
            (spawn Sender (var target) 1)
            (sequence (spawn Sender (var target) 2) (unit))))))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, exploration} = Explorer.explore(core, "main")
    assert exploration.configurations > 1

    received =
      exploration.outcomes
      |> Enum.flat_map(& &1.trace)
      |> Enum.filter(&(&1.label == :receive))
      |> Enum.map(& &1.message)
      |> MapSet.new()

    assert received == MapSet.new([1, 2])

    assert {:exhausted, %{transitions: 1}} =
             Explorer.explore(core, "main", [], transition_limit: 1)
  end

  @tag obligations: ~w(FK-OBL-011)
  test "self-send preserves per-sender FIFO order" do
    source = """
    (module SenderOrder
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://sender-order")
      (export value main)
      (process SelfQueue
        (mailbox Int)
        (params)
        (sequence
          (send (self) 1)
          (sequence
            (send (self) 2)
            (receive
              (case (bind first) (when (equal (var first) 1))
                (receive
                  (case (bind second) (when (equal (var second) 2)) (unit))))))))
      (def main (signature Unit (uses Process))
        (let child (spawn SelfQueue) (unit))))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, :unit, outcome} = Stepper.run(core, "main")

    assert outcome.trace
           |> Enum.filter(&(&1.label == :receive))
           |> Enum.map(& &1.message) == [1, 2]
  end

  @tag obligations: ~w(FK-OBL-001 FK-OBL-013)
  test "generated closed terms make progress and preserve their checked result types" do
    cases =
      for integer <- -8..8 do
        [
          {"literal_#{encode_integer(integer)}", "Int", Integer.to_string(integer), integer},
          {"add_#{encode_integer(integer)}", "Int", "(add #{integer} 3)", integer + 3},
          {"less_#{encode_integer(integer)}", "Bool", "(less #{integer} 0)", integer < 0},
          {"let_#{encode_integer(integer)}", "Int",
           "(let value #{integer} (multiply (var value) 2))", integer * 2}
        ]
      end
      |> List.flatten()

    declarations =
      Enum.map_join(cases, "\n", fn {name, type, expression, _expected} ->
        "  (export value #{name})\n  (def #{name} (signature #{type} (uses)) #{expression})"
      end)

    source = """
    (module GeneratedTerms
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://generated-terms")
    #{declarations})
    """

    assert {:ok, core} = Catena.check_kernel(source)
    assert :ok = Verifier.verify(core)

    Enum.each(cases, fn {name, _type, _expression, expected} ->
      assert {:ok, ^expected, outcome} = Stepper.run(core, name)
      assert outcome.root_status == :terminated
      assert outcome.root_trap == nil
    end)

    assert {:ok, module, binary, metadata} = Catena.compile_kernel(source)
    assert metadata.warnings == []
    load!(module, binary)

    Enum.each(cases, fn {name, _type, _expression, expected} ->
      assert apply(module, String.to_atom(name), []) == expected
    end)

    unload(module)
  end

  @tag obligations: ~w(FK-OBL-004 FK-OBL-008)
  test "local let bindings generalize only under the value and effect restriction" do
    polymorphic =
      minimal_module(
        "LocalPolymorphism",
        "(Tuple Int Bool)",
        "(let id (fn (value a) (var value))\n" <>
          "  (tuple (call (var id) 1) (call (var id) true)))"
      )

    assert {:ok, core} = Catena.check_kernel(polymorphic)

    assert %{variables: ["a"], type: {:function, {:variable, "a"}, [], {:variable, "a"}}} =
             hd(core.definitions).expression.binding

    assert :ok = Verifier.verify(core)
    assert {:ok, {1, true}, _outcome} = Stepper.run(core, "main")

    forged = put_in(core, [:definitions, Access.at(0), :expression, :binding, :variables], [])
    assert {:error, _reason} = Verifier.verify(forged)

    assert {:ok, module, binary, _metadata} = Catena.compile_kernel(polymorphic)
    load!(module, binary)
    assert apply(module, :main, []) == {1, true}
    unload(module)

    restricted = """
    (module RestrictedGeneralization
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://restricted-generalization")
      (data Option
        (params a)
        (constructor None (fields)))
      (effect Tick (operation tick (params) Unit))
      (export type Option)
      (export value main)
      (def main (signature (Option Int) (uses Tick))
        (let choice
          (sequence (request Tick tick) (construct None))
          (var choice))))
    """

    assert {:ok, restricted_core} = Catena.check_kernel(restricted)

    assert %{variables: [], type: {:nominal, "Option", [:integer]}} =
             hd(restricted_core.definitions).expression.binding
  end

  @tag obligations: ~w(FK-OBL-010 FK-OBL-015)
  test "interfaces bind public process identities and reject substitution" do
    assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
    interface = Interface.build(core)
    encoded = Interface.encode(interface)

    assert {:ok, decoded} = Catena.Interface.decode(encoded)
    assert decoded.format == :kernel_interface
    assert decoded.language_revision == "0.1.8"
    assert [%{name: "Option"}] = decoded.types
    assert [process] = decoded.processes
    assert process.identity == "test://c010-fixture#C010Fixture.Selective"
    assert process.mailbox == {:nominal, "Option", [:integer]}
    assert process.spawn_symbol == "__catena_spawn_Selective"

    tampered = put_in(interface, ["processes", Access.at(0), "mailbox"], %{"tag" => "boolean"})
    assert {:error, %{id: "PRC004"}} = tampered |> Interface.encode() |> Catena.Interface.decode()

    forged_type =
      interface
      |> put_in(
        ["types", Access.at(0), "constructors", Access.at(1), "fields"],
        [%{"tag" => "nominal", "name" => "Missing", "arguments" => []}]
      )
      |> resign_interface()

    assert {:error, %{id: "PRC004"}} =
             forged_type |> Interface.encode() |> Catena.Interface.decode()

    consumer = """
    (module Consumer
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://consumer")
      (import C010Fixture "#{interface["digest"]}")
      (export value main)
      (def main (signature Unit (uses Process))
        (let child (spawn C010Fixture.Selective) (unit))))
    """

    assert {:ok, consumer_core} = Catena.check_kernel(consumer, interfaces: [decoded])
    assert hd(consumer_core.definitions).expression.value.selected_entry.imported?

    assert {:error, %{id: "PRC004"}} = Catena.check_kernel(consumer, interfaces: [])
  end

  @tag obligations: ~w(FK-OBL-004 FK-OBL-010)
  test "sendability, process contexts, and forged core evidence are rejected" do
    non_sendable = """
    (module NonSendable
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://non-sendable")
      (process Bad (mailbox (Fn Int (effects) Int)) (params) (unit)))
    """

    assert {:error, %{id: "PRC002"}} = Catena.check_kernel(non_sendable)

    assert {:error, %{id: "PRC003"}} =
             Catena.check_kernel(minimal_module("BadSelf", "(Process Int)", "(self)"))

    assert {:error, %{id: "PRC003"}} =
             Catena.check_kernel(
               minimal_module("BadReceive", "Unit", "(receive (case _ (unit)))")
             )

    assert {:ok, core} = @fixture |> File.read!() |> Catena.check_kernel(source: @fixture)
    forged = put_in(core, [:definitions, Access.at(0), :expression, :effects], [:process])
    assert {:error, _reason} = Verifier.verify(forged)

    forged_process =
      put_in(core, [:processes, Access.at(0), :mailbox], {:function, :integer, [], :integer})

    assert {:error, _reason} = Verifier.verify(forged_process)

    forged_constructor = put_in(core, [:data, :constructors, "Some", :fields], [])
    assert {:error, _reason} = Verifier.verify(forged_constructor)
  end

  @tag obligations: ~w(FK-OBL-011)
  test "explicit trap is a typed bottom and lowers to the fixed BEAM trap" do
    source = minimal_module("ExplicitTrap", "Int", "(trap 9)")
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:trap, 9, outcome} = Stepper.run(core, "main")
    assert outcome.root_status == :trapped

    assert {:ok, module, binary, _metadata} = Catena.compile_kernel(source)
    load!(module, binary)
    assert catch_error(apply(module, :main, [])) == {:catena_trap, 9}
    unload(module)
  end

  @tag obligations: ~w(FK-OBL-006 FK-OBL-015)
  test "kernel artifacts and interfaces are deterministic and record the kernel frontend" do
    source = File.read!(@fixture)

    assert {:ok, module, first_beam, first} =
             Catena.compile_kernel(source, source: "/checkout-a/c010-kernel.catena")

    assert {:ok, ^module, second_beam, second} =
             Catena.compile_kernel(source, source: "/checkout-b/c010-kernel.catena")

    assert first_beam == second_beam
    assert first.interface_binary == second.interface_binary

    assert {:ok, {^module, [compile_info: compile_info]}} =
             :beam_lib.chunks(first_beam, [:compile_info])

    assert compile_info[:catena_specification] == ~c"0.1.8"
    assert compile_info[:catena_frontend] == ~c"kernel-sexpr-0.1.8"
    assert compile_info[:catena_edition] == ~c"0.1"
    assert compile_info[:catena_language_revision] == ~c"0.1.8"
    assert compile_info[:catena_previews] == []
  end

  defp minimal_module(module, type, expression) do
    """
    (module #{module}
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://#{module}")
      (export value main)
      (def main (signature #{type} (uses)) #{expression}))
    """
  end

  defp minimal_module_with_declarations(module, declarations, type, uses, expression) do
    """
    (module #{module}
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://#{module}")
      #{declarations}
      (export value main)
      (def main (signature #{type} (uses #{uses})) #{expression}))
    """
  end

  defp step_until(configuration, predicate, remaining) when remaining > 0 do
    if predicate.(configuration) do
      configuration
    else
      [pid | _] = Stepper.runnable_pids(configuration)
      {:ok, configuration} = Stepper.step(configuration, pid)
      step_until(configuration, predicate, remaining - 1)
    end
  end

  defp load!(module, binary) do
    assert {:module, ^module} = :code.load_binary(module, ~c"c010-kernel.beam", binary)
  end

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
  end

  defp resign_interface(interface) do
    payload = Map.delete(interface, "digest")

    digest =
      payload
      |> Catena.CanonicalJSON.encode()
      |> then(&:crypto.hash(:sha256, &1))
      |> Base.encode16(case: :lower)

    Map.put(payload, "digest", digest)
  end

  defp encode_integer(integer) when integer < 0, do: "n#{abs(integer)}"
  defp encode_integer(integer), do: "p#{integer}"
end
