defmodule Catena.C002DataTest do
  use ExUnit.Case, async: false

  alias Catena.{AST.Decoder, Interface}
  alias Catena.Reference.Evaluator
  alias Catena.TypedCore.Verifier

  @tag obligations: ~w(DP-OBL-002 DP-OBL-051 DP-OBL-054 DP-OBL-055 DP-OBL-067)
  test "keeps the durable C002 conformance fixture executable" do
    source = File.read!("test/fixtures/c002-option.catena.json")
    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 7} = Evaluator.run(core, "main")

    for layout <- [:uniform, :compact] do
      assert {:ok, :C002Fixture, binary, _metadata} = Catena.compile_json(source, layout: layout)

      assert {:module, :C002Fixture} =
               :code.load_binary(:C002Fixture, ~c"c002-fixture.beam", binary)

      assert apply(:C002Fixture, :main, []) == 7

      type_id = "test://c002-conformance::C002Fixture::Option"

      case layout do
        :uniform ->
          assert apply(:C002Fixture, :make, []) ==
                   {:catena_adt, String.to_atom(type_id), 1, {7}}

        :compact ->
          assert apply(:C002Fixture, :make, []) == {String.to_atom(type_id <> "::Some"), 7}
      end

      unload(:C002Fixture)
    end

    assert {:ok, _module, _binary, metadata} = Catena.compile_json(source)
    refute metadata.interface_binary =~ "compact"
    refute metadata.interface_binary =~ "uniform"
  end

  @tag obligations: ~w(DP-OBL-070)
  test "normalizes AST 0.1.1 into the 0.1.2 compiler representation" do
    json = JSON.encode!(module_01("Legacy", [], []))
    assert {:ok, ast} = Decoder.decode(json)
    assert ast.version == "0.1.2"
    assert ast.frontend_version == "0.1.1"
    assert ast.type_groups == []
  end

  @tag obligations: ~w(DP-OBL-005 DP-OBL-014 DP-OBL-019 DP-OBL-035)
  test "uses stable semantic diagnostics for duplicate declarations and unsupported patterns" do
    duplicate = type_decl("Duplicate", [], [constructor("Same", []), constructor("Same", [])])
    program = module_02("DuplicateData", [type_group([duplicate])], [], [], [])
    assert {:error, %{id: "A002"}} = Catena.check_json(JSON.encode!(program))

    invalid_match =
      definition(
        "bad",
        ["value"],
        forall([], function_type(integer_type(), integer_type())),
        match_expr(variable("value"), [
          clause(%{"tag" => "list", "elements" => []}, integer(0))
        ])
      )

    program = module_02("UnsupportedPattern", [], [], [], [invalid_match])
    assert {:error, %{id: "M005"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations:
         ~w(DP-OBL-007 DP-OBL-008 DP-OBL-015 DP-OBL-018 DP-OBL-023 DP-OBL-027 DP-OBL-030 DP-OBL-031 DP-OBL-039 DP-OBL-049 DP-OBL-051 DP-OBL-055 DP-OBL-067 DP-OBL-068)
  test "infers, verifies, evaluates, and compiles exhaustive nominal matches in both layouts" do
    source = option_program("C002Option") |> JSON.encode!()
    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, 7} = Evaluator.run(core, "main")

    for layout <- [:uniform, :compact] do
      assert {:ok, :C002Option, first, metadata} = Catena.compile_json(source, layout: layout)
      assert {:ok, :C002Option, second, _metadata} = Catena.compile_json(source, layout: layout)
      assert first == second
      assert metadata.layout == layout
      assert metadata.interface_binary == Interface.encode(metadata.interface)
      assert {:ok, decoded} = Interface.decode(metadata.interface_binary)
      assert decoded.module == "C002Option"
      assert hd(decoded.types).variance == [:covariant]

      assert {:module, :C002Option} = :code.load_binary(:C002Option, ~c"c002-option.beam", first)
      assert apply(:C002Option, :main, []) == 7
      unload(:C002Option)
    end
  end

  @tag obligations: ~w(DP-OBL-032 DP-OBL-066)
  test "rejects non-exhaustive matches with a concrete witness" do
    program =
      option_program("MissingCase")
      |> put_main_match([
        clause(constructor_pattern("Option.None", []), integer(0))
      ])

    assert {:error, %{id: "M001", details: %{witness: witness}}} =
             Catena.check_json(JSON.encode!(program))

    assert witness == "Option.Some(_)"
  end

  @tag obligations: ~w(DP-OBL-033)
  test "rejects redundant clauses" do
    program =
      option_program("RedundantCase")
      |> put_main_match([
        clause(wildcard(), integer(0)),
        clause(constructor_pattern("Option.None", []), integer(1))
      ])

    assert {:error, %{id: "M002"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-001 DP-OBL-006 DP-OBL-013 DP-OBL-036)
  test "accepts empty and negative recursive declarations but limits structural metadata" do
    empty = type_decl("Empty", [], [])

    negative =
      type_decl("Negative", [parameter("a")], [
        constructor("Negative", [
          function_type(named_type("Negative", [variable_type("a")]), variable_type("a"))
        ])
      ])

    program = module_02("Shapes", [type_group([empty, negative])], [], [], [])
    assert {:ok, core} = Catena.check_json(JSON.encode!(program))
    assert Enum.find(core.data.types, &(&1.name == "Empty")).inhabitation == :empty
    negative_type = Enum.find(core.data.types, &(&1.name == "Negative"))
    refute negative_type.positive?
    assert negative_type.variance == [:invariant]
  end

  @tag obligations: ~w(DP-OBL-029 DP-OBL-036)
  test "permits an empty match only for a proven-empty type" do
    empty = type_decl("Empty", [], [])

    absurd =
      definition(
        "absurd",
        ["value"],
        forall([], function_type(named_type("Empty"), integer_type())),
        match_expr(variable("value"), [])
      )

    program = module_02("EmptyMatch", [type_group([empty])], [], ["absurd"], [absurd])
    assert {:ok, :EmptyMatch, _binary, _metadata} = Catena.compile_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-007 DP-OBL-016 DP-OBL-054)
  test "keeps named-field evaluation order while storing payloads in declaration order" do
    pair =
      type_decl("Pair", [], [
        named_constructor("Pair", [{"left", integer_type()}, {"right", integer_type()}])
      ])

    make =
      definition(
        "make",
        [],
        forall([], named_type("Pair")),
        named_construct("Pair.Pair", [{"right", integer(2)}, {"left", integer(1)}])
      )

    program =
      module_02(
        "NamedFields",
        [type_group([pair])],
        [%{"name" => "Pair", "visibility" => "transparent"}],
        ["make"],
        [make]
      )

    assert {:ok, core} = Catena.check_json(JSON.encode!(program))
    [definition] = Enum.reject(core.definitions, & &1.generated?)
    assert Enum.map(definition.expression.arguments, & &1.field_index) == [1, 0]

    assert {:ok, :NamedFields, binary, _metadata} = Catena.compile_json(JSON.encode!(program))

    assert {:module, :NamedFields} =
             :code.load_binary(:NamedFields, ~c"named-fields.beam", binary)

    assert {_, 1, 2} = apply(:NamedFields, :make, [])
    unload(:NamedFields)
  end

  @tag obligations: ~w(DP-OBL-060 DP-OBL-061 DP-OBL-062 DP-OBL-063 DP-OBL-064)
  test "generates and verifies an explicit constructor-complete fold" do
    program = option_program("DerivedFold", ["fold"])
    assert {:ok, :DerivedFold, binary, metadata} = Catena.compile_json(JSON.encode!(program))
    assert Enum.any?(metadata.core.definitions, &(&1.name == "Option.fold" and &1.generated?))

    assert {:module, :DerivedFold} =
             :code.load_binary(:DerivedFold, ~c"derived-fold.beam", binary)

    value = apply(:DerivedFold, :make, [])
    assert apply(:DerivedFold, :"Option.fold", [0, fn item -> item end, value]) == 7
    unload(:DerivedFold)
  end

  @tag obligations: ~w(DP-OBL-010 DP-OBL-011 DP-OBL-052)
  test "interfaces preserve nominal identity and hide abstract constructors" do
    producer = option_program("Producer")
    assert {:ok, :Producer, _binary, transparent} = Catena.compile_json(JSON.encode!(producer))
    assert {:ok, interface} = Interface.decode(transparent.interface_binary)

    consumer =
      module_02(
        "Consumer",
        [],
        [],
        ["main"],
        [
          definition(
            "main",
            [],
            forall([], named_type("Producer.Option", [integer_type()])),
            construct("Producer.Option.Some", [integer(9)])
          )
        ]
      )

    assert {:ok, :Consumer, _binary, _metadata} =
             Catena.compile_json(JSON.encode!(consumer), interfaces: [interface])

    abstract_producer =
      put_in(producer["type_exports"], [%{"name" => "Option", "visibility" => "abstract"}])

    assert {:ok, :Producer, _binary, abstract_metadata} =
             Catena.compile_json(JSON.encode!(abstract_producer))

    assert {:ok, abstract_interface} = Interface.decode(abstract_metadata.interface_binary)

    assert {:error, %{id: "A004"}} =
             Catena.check_json(JSON.encode!(consumer), interfaces: [abstract_interface])
  end

  @tag obligations: ~w(DP-OBL-011 DP-OBL-012)
  test "explicit constructor imports are the only unqualified imported access" do
    assert {:ok, :ImportSource, _binary, metadata} =
             option_program("ImportSource") |> JSON.encode!() |> Catena.compile_json()

    assert {:ok, interface} = Interface.decode(metadata.interface_binary)

    consumer =
      module_02(
        "ImportedAlias",
        [],
        [],
        ["main"],
        [
          definition(
            "main",
            [],
            forall([], named_type("ImportSource.Option", [integer_type()])),
            construct("Present", [integer(9)])
          )
        ]
      )
      |> Map.put("imports", [
        %{
          "kind" => "constructor",
          "constructor" => "ImportSource.Option.Some",
          "as" => "Present"
        }
      ])

    assert {:ok, :ImportedAlias, _binary, _metadata} =
             Catena.compile_json(JSON.encode!(consumer), interfaces: [interface])
  end

  @tag obligations: ~w(DP-OBL-050)
  test "rejects a tampered interface digest" do
    assert {:ok, :DigestSource, _binary, metadata} =
             option_program("DigestSource") |> JSON.encode!() |> Catena.compile_json()

    tampered =
      String.replace(metadata.interface_binary, "DigestSource", "DigestSourcf", global: false)

    assert {:error, %{id: "A005"}} = Interface.decode(tampered)
  end

  @tag obligations: ~w(DP-OBL-002 DP-OBL-050 DP-OBL-053)
  test "treats origin changes as nominal identity changes" do
    first = option_program("OriginIdentity")
    second = Map.put(first, "origin", "test://different-origin")

    assert {:ok, :OriginIdentity, _binary, first_metadata} =
             Catena.compile_json(JSON.encode!(first))

    assert {:ok, :OriginIdentity, _binary, second_metadata} =
             Catena.compile_json(JSON.encode!(second))

    assert {:ok, first_interface} = Interface.decode(first_metadata.interface_binary)
    assert {:ok, second_interface} = Interface.decode(second_metadata.interface_binary)
    refute hd(first_interface.types).id == hd(second_interface.types).id

    consumer = module_02("IdentityConsumer", [], [], [], [])

    assert {:error, %{id: "A005"}} =
             Catena.check_json(JSON.encode!(consumer),
               interfaces: [first_interface, second_interface]
             )
  end

  @tag obligations: ~w(DP-OBL-009 DP-OBL-041 DP-OBL-044 DP-OBL-045)
  test "uses local equalities for annotated GADT matches" do
    expr =
      type_decl("Expr", [parameter("a")], [
        constructor("IntLit", [integer_type()], [], named_type("Expr", [integer_type()])),
        constructor("BoolLit", [boolean_type()], [], named_type("Expr", [boolean_type()]))
      ])

    evaluate =
      definition(
        "evaluate",
        ["expression"],
        forall(
          ["a"],
          function_type(named_type("Expr", [variable_type("a")]), variable_type("a"))
        ),
        match_expr(variable("expression"), [
          clause(constructor_pattern("Expr.IntLit", [bind("value")]), variable("value")),
          clause(constructor_pattern("Expr.BoolLit", [bind("value")]), variable("value"))
        ])
      )

    main =
      definition(
        "main",
        [],
        forall([], integer_type()),
        call(variable("evaluate"), [construct("Expr.IntLit", [integer(11)])])
      )

    program =
      module_02(
        "GADTMatch",
        [type_group([expr])],
        [%{"name" => "Expr", "visibility" => "transparent"}],
        ["evaluate", "main"],
        [evaluate, main]
      )

    assert {:ok, :GADTMatch, binary, metadata} = Catena.compile_json(JSON.encode!(program))
    assert metadata.core.profile == :annotation_directed
    assert {:module, :GADTMatch} = :code.load_binary(:GADTMatch, ~c"gadt-match.beam", binary)
    assert apply(:GADTMatch, :main, []) == 11
    unload(:GADTMatch)
  end

  @tag obligations: ~w(DP-OBL-046)
  test "rejects existential values escaping a match branch" do
    packed =
      type_decl("Packed", [], [
        constructor("Pack", [variable_type("hidden")], [parameter("hidden")])
      ])

    bad =
      definition(
        "bad",
        ["packed"],
        forall(["result"], function_type(named_type("Packed"), variable_type("result"))),
        match_expr(variable("packed"), [
          clause(constructor_pattern("Packed.Pack", [bind("value")]), variable("value"))
        ])
      )

    program = module_02("ExistentialEscape", [type_group([packed])], [], [], [bad])
    assert {:error, %{id: "T009"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-027 DP-OBL-028 DP-OBL-033 DP-OBL-037 DP-OBL-039)
  test "preserves ordered fallthrough for guards not proved true" do
    decide =
      definition(
        "decide",
        ["value"],
        forall([], function_type(boolean_type(), integer_type())),
        match_expr(variable("value"), [
          guarded_clause(wildcard(), variable("value"), integer(1)),
          clause(wildcard(), integer(2))
        ])
      )

    program = module_02("GuardFallthrough", [], [], ["decide"], [decide])

    assert {:ok, :GuardFallthrough, binary, _metadata} =
             Catena.compile_json(JSON.encode!(program))

    assert {:module, :GuardFallthrough} =
             :code.load_binary(:GuardFallthrough, ~c"guard-fallthrough.beam", binary)

    assert apply(:GuardFallthrough, :decide, [true]) == 1
    assert apply(:GuardFallthrough, :decide, [false]) == 2
    unload(:GuardFallthrough)

    false_guard =
      put_in(
        program,
        ["definitions", Access.at(0), "body", "clauses", Access.at(0), "guard"],
        %{"tag" => "boolean", "value" => false}
      )

    assert {:error, %{id: "M002"}} = Catena.check_json(JSON.encode!(false_guard))
  end

  @tag obligations: ~w(DP-OBL-018 DP-OBL-020 DP-OBL-022 DP-OBL-033)
  test "expands exhaustive or patterns without changing branch bindings" do
    alternatives = %{
      "tag" => "or",
      "alternatives" => [
        %{"tag" => "boolean", "value" => true},
        %{"tag" => "boolean", "value" => false}
      ]
    }

    choose =
      definition(
        "choose",
        ["value"],
        forall([], function_type(boolean_type(), integer_type())),
        match_expr(variable("value"), [clause(alternatives, integer(1))])
      )

    program = module_02("OrPatterns", [], [], ["choose"], [choose])
    assert {:ok, :OrPatterns, binary, _metadata} = Catena.compile_json(JSON.encode!(program))
    assert {:module, :OrPatterns} = :code.load_binary(:OrPatterns, ~c"or-patterns.beam", binary)
    assert apply(:OrPatterns, :choose, [true]) == 1
    assert apply(:OrPatterns, :choose, [false]) == 1
    unload(:OrPatterns)
  end

  @tag obligations: ~w(DP-OBL-038 DP-OBL-066)
  test "reports deterministic coverage implementation limits" do
    source = option_program("CoverageBudget") |> JSON.encode!()
    assert {:error, %{id: "M004"}} = Catena.check_json(source, coverage_budget: 1)
  end

  @tag obligations: ~w(DP-OBL-004 DP-OBL-013)
  test "elaborates mutually recursive groups atomically" do
    left =
      type_decl("Left", [], [constructor("Left", [named_type("Right")])])

    right =
      type_decl("Right", [], [constructor("Right", []), constructor("More", [named_type("Left")])])

    program = module_02("Mutual", [type_group([left, right])], [], [], [])
    assert {:ok, core} = Catena.check_json(JSON.encode!(program))
    assert Enum.map(core.data.types, & &1.name) == ["Left", "Right"]
    assert Enum.all?(core.data.types, &(&1.inhabitation == :inhabited))
  end

  @tag obligations: ~w(DP-OBL-040 DP-OBL-047 DP-OBL-071)
  test "typed-core verifier independently rejects corrupted constructor and decision metadata" do
    assert {:ok, core} = option_program("VerifierGate") |> JSON.encode!() |> Catena.check_json()
    [make, main] = Enum.reject(core.definitions, & &1.generated?)
    corrupted_constructor = put_in(make.expression.constructor.id, "forged-constructor")

    assert {:error, reason} =
             Verifier.verify(%{core | definitions: [corrupted_constructor, main]})

    assert reason =~ "construction"

    corrupted_decision = put_in(main.expression.decision_tree.exhaustive?, false)
    assert {:error, reason} = Verifier.verify(%{core | definitions: [make, corrupted_decision]})
    assert reason =~ "decision tree"
  end

  @tag obligations: ~w(DP-OBL-030 DP-OBL-034 DP-OBL-069)
  test "bounded Boolean pattern corpus agrees with the finite coverage model" do
    candidates = [
      [],
      [boolean_pattern(true)],
      [boolean_pattern(false)],
      [wildcard()],
      [boolean_pattern(true), boolean_pattern(false)],
      [boolean_pattern(false), boolean_pattern(true)],
      [boolean_pattern(true), wildcard()],
      [wildcard(), boolean_pattern(false)],
      [boolean_pattern(true), boolean_pattern(true)]
    ]

    accepted = MapSet.new([["_"], ["true", "false"], ["false", "true"], ["true", "_"]])

    for {patterns, index} <- Enum.with_index(candidates) do
      clauses = Enum.map(patterns, &clause(&1, integer(1)))

      choose =
        definition(
          "choose",
          ["value"],
          forall([], function_type(boolean_type(), integer_type())),
          match_expr(variable("value"), clauses)
        )

      program = module_02("Corpus#{index}", [], [], ["choose"], [choose])
      key = Enum.map(patterns, &pattern_key/1)
      result = Catena.check_json(JSON.encode!(program))

      assert match?({:ok, _}, result) == MapSet.member?(accepted, key),
             "unexpected result for #{inspect(key)}: #{inspect(result)}"
    end
  end

  @tag obligations: ~w(DP-OBL-017)
  test "rejects implicit interchange of positional and named constructor styles" do
    pair =
      type_decl("Pair", [parameter("a"), parameter("b")], [
        constructor("Pair", [variable_type("a"), variable_type("b")])
      ])

    bad =
      definition(
        "bad",
        [],
        forall(["a", "b"], named_type("Pair", [variable_type("a"), variable_type("b")])),
        named_construct("Pair.Pair", [{"left", integer(1)}, {"right", integer(2)}])
      )

    program =
      module_02(
        "StyleInterchange",
        [type_group([pair])],
        [%{"name" => "Pair", "visibility" => "transparent"}],
        ["bad"],
        [bad]
      )

    assert {:error, %{id: "A003"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-021)
  test "rejects a variable name occurring more than once in a single pattern" do
    pair = type_decl("Pair", [], [constructor("Pair", [integer_type(), integer_type()])])

    bad =
      definition(
        "bad",
        ["p"],
        forall([], function_type(named_type("Pair"), integer_type())),
        match_expr(variable("p"), [
          clause(constructor_pattern("Pair.Pair", [bind("x"), bind("x")]), integer(0))
        ])
      )

    program =
      module_02(
        "DuplicateBinder",
        [type_group([pair])],
        [%{"name" => "Pair", "visibility" => "transparent"}],
        ["bad"],
        [bad]
      )

    assert {:error, %{id: "M003"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-024)
  test "patterns are pure and reject a call expression in a pattern position" do
    option = type_decl("Option", [parameter("a")], [constructor("Some", [variable_type("a")])])

    call_pattern = %{"tag" => "call", "callee" => variable("f"), "arguments" => [bind("x")]}

    bad =
      definition(
        "bad",
        ["v"],
        forall([], function_type(named_type("Option", [integer_type()]), integer_type())),
        match_expr(variable("v"), [clause(call_pattern, integer(0))])
      )

    program =
      module_02(
        "ImpurePattern",
        [type_group([option])],
        [%{"name" => "Option", "visibility" => "transparent"}],
        ["bad"],
        [bad]
      )

    assert {:error, %{id: "M005"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-025)
  test "rejects a constructor pattern with the wrong arity" do
    option =
      type_decl("Option", [parameter("a")], [
        constructor("None", []),
        constructor("Some", [variable_type("a")])
      ])

    bad =
      definition(
        "bad",
        ["v"],
        forall([], function_type(named_type("Option", [integer_type()]), integer_type())),
        match_expr(variable("v"), [
          clause(constructor_pattern("Option.Some", [bind("x"), bind("y")]), integer(0)),
          clause(constructor_pattern("Option.None", []), integer(1))
        ])
      )

    program =
      module_02(
        "WrongArity",
        [type_group([option])],
        [%{"name" => "Option", "visibility" => "transparent"}],
        ["bad"],
        [bad]
      )

    assert {:error, %{id: "M003"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-042)
  test "rejects an existential variable appearing in the datatype result" do
    box =
      type_decl("Box", [parameter("a")], [
        constructor(
          "Hold",
          [variable_type("a")],
          [parameter("e")],
          named_type("Box", [variable_type("e")])
        )
      ])

    program = module_02("ExistentialResult", [type_group([box])], [], [], [])

    assert {:error, %{id: "T009"}} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(DP-OBL-043)
  test "rejects a GADT pattern match without an enclosing signature" do
    expr =
      type_decl("Expr", [parameter("a")], [
        constructor("IntLit", [integer_type()], [], named_type("Expr", [integer_type()]))
      ])

    evaluate = %{
      "name" => "evaluate",
      "parameters" => ["expression"],
      "body" =>
        match_expr(variable("expression"), [
          clause(constructor_pattern("Expr.IntLit", [bind("value")]), variable("value"))
        ])
    }

    program =
      module_02(
        "GadtNoSignature",
        [type_group([expr])],
        [%{"name" => "Expr", "visibility" => "transparent"}],
        [],
        [evaluate]
      )

    assert {:error, %{id: "T010"}} = Catena.check_json(JSON.encode!(program))
  end

  defp option_program(module, derivations \\ []) do
    option =
      type_decl(
        "Option",
        [parameter("a")],
        [constructor("None", []), constructor("Some", [variable_type("a")])],
        derivations
      )

    make =
      definition(
        "make",
        [],
        forall([], named_type("Option", [integer_type()])),
        construct("Option.Some", [integer(7)])
      )

    main =
      definition(
        "main",
        [],
        forall([], integer_type()),
        match_expr(construct("Option.Some", [integer(7)]), [
          clause(constructor_pattern("Option.None", []), integer(0)),
          clause(constructor_pattern("Option.Some", [bind("value")]), variable("value"))
        ])
      )

    module_02(
      module,
      [type_group([option])],
      [%{"name" => "Option", "visibility" => "transparent"}],
      ["make", "main"],
      [make, main]
    )
  end

  defp put_main_match(program, clauses) do
    update_in(program["definitions"], fn definitions ->
      Enum.map(definitions, fn
        %{"name" => "main"} = definition ->
          %{definition | "body" => match_expr(construct("Option.Some", [integer(7)]), clauses)}

        definition ->
          definition
      end)
    end)
  end

  defp module_01(name, exports, definitions),
    do: %{
      "version" => "0.1.1",
      "module" => name,
      "exports" => exports,
      "definitions" => definitions
    }

  defp module_02(name, groups, type_exports, exports, definitions) do
    %{
      "version" => "0.1.2",
      "origin" => "test://c002",
      "module" => name,
      "type_groups" => groups,
      "type_exports" => type_exports,
      "imports" => [],
      "exports" => exports,
      "definitions" => definitions
    }
  end

  defp type_group(declarations), do: %{"declarations" => declarations}

  defp type_decl(name, parameters, constructors, derivations \\ []),
    do: %{
      "name" => name,
      "parameters" => parameters,
      "constructors" => constructors,
      "derivations" => derivations
    }

  defp parameter(name), do: %{"name" => name, "kind" => "Type"}

  defp constructor(name, fields, existentials \\ [], result \\ nil) do
    %{"name" => name, "fields" => fields, "existentials" => existentials}
    |> maybe_put("result", result)
  end

  defp named_constructor(name, fields) do
    %{
      "name" => name,
      "existentials" => [],
      "fields" => Enum.map(fields, fn {field, type} -> %{"name" => field, "type" => type} end)
    }
  end

  defp definition(name, parameters, signature, body),
    do: %{"name" => name, "parameters" => parameters, "signature" => signature, "body" => body}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}
  defp bind(name), do: %{"tag" => "bind", "name" => name}
  defp wildcard, do: %{"tag" => "wildcard"}
  defp boolean_pattern(value), do: %{"tag" => "boolean", "value" => value}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp construct(name, arguments),
    do: %{"tag" => "construct", "constructor" => name, "arguments" => arguments}

  defp named_construct(name, fields),
    do: %{
      "tag" => "construct",
      "constructor" => name,
      "fields" => Enum.map(fields, fn {field, value} -> %{"name" => field, "value" => value} end)
    }

  defp match_expr(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp clause(pattern, body), do: %{"pattern" => pattern, "body" => body}

  defp guarded_clause(pattern, guard, body),
    do: %{"pattern" => pattern, "guard" => guard, "body" => body}

  defp constructor_pattern(name, arguments),
    do: %{"tag" => "constructor", "constructor" => name, "arguments" => arguments}

  defp forall(variables, type), do: %{"forall" => variables, "type" => type}
  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}
  defp variable_type(name), do: %{"tag" => "variable", "name" => name}

  defp named_type(name, arguments \\ []),
    do: %{"tag" => "named", "name" => name, "arguments" => arguments}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result, "effect" => []}

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp pattern_key(%{"tag" => "wildcard"}), do: "_"
  defp pattern_key(%{"tag" => "boolean", "value" => value}), do: to_string(value)

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
