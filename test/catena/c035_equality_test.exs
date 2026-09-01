defmodule Catena.C035EqualityTest do
  use ExUnit.Case, async: false

  alias Catena.{Data, Effect.Runtime, LanguageLifecycle, LanguageVersion, Values}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43)

  describe "revision registration" do
    @tag obligations: ~w(EQ-OBL-001 EQ-OBL-008)
    test "0.1.30 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.43"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.30" in LanguageVersion.compilable_revisions()
      refute "0.1.30" in LanguageVersion.artifact_versions()
      refute "0.1.30" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("equality-and-ordering", "0.1.30")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-30-equality-and-ordering")
        )

      assert change["affects"] == ~w(static-meaning diagnostics)

      assert String.contains?(
               change["specification"],
               "equality-and-ordering/the-comparable-set.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.43"}}} = Catena.decode_source_text("")
      assert true = Values.value?(1.5)

      refute function_exported?(Values, :identity_equal, 2)
      refute function_exported?(Values, :mixed_compare, 2)
      refute function_exported?(Catena, :compare_source, 2)
    end
  end

  describe "the comparable set at the value level" do
    @tag obligations: ~w(EQ-OBL-002 EQ-OBL-003)
    test "classification covers both carriers and compare fixes the signed zeros" do
      assert Values.comparable?(1) and Values.comparable?(true) and Values.comparable?(-0.0)
      assert Values.comparable?({1, true, 1.5})
      assert Values.comparable?({:catena_variant, :some, {1, 0.0}})
      assert Values.comparable?({:catena_constructor, :point, {1, 2}})
      assert Values.comparable?(%{label: 1})

      refute Values.comparable?(:unit)
      refute Values.comparable?({:closure, "x", %{tag: :variable, name: "x"}, %{}})
      refute Values.comparable?({:catena_process, "p1"})
      refute Values.comparable?({1, {:catena_process, "p1"}})

      assert Values.orderable?(1) and Values.orderable?(-0.5)
      refute Values.orderable?(true)
      refute Values.orderable?({1, 2})

      assert :eq = Values.compare(1, 1)
      assert :lt = Values.compare(1, 2)
      assert :gt = Values.compare(0.5, -0.5)
      assert :lt = Values.compare(-0.0, 0.0)
      assert :gt = Values.compare(0.0, -0.0)
      assert :eq = Values.compare(-0.0, -0.0)
      assert :lt = Values.compare(-0.0, 5.0e-324)
      assert :lt = Values.compare(-1.0, -0.5)

      assert_raise ArgumentError, fn -> Values.compare(1, 1.0) end
      assert_raise ArgumentError, fn -> Values.compare(true, false) end
    end

    @tag obligations: ~w(EQ-OBL-002)
    test "Data.comparable_type? covers the closed type set" do
      data = sample_data()

      assert Data.comparable_type?(:integer, data)
      assert Data.comparable_type?(:boolean, data)
      assert Data.comparable_type?(:float, data)
      assert Data.comparable_type?({:tuple, [:integer, :boolean]}, data)
      assert Data.comparable_type?({:tuple, [{:tuple, [:float]}]}, data)
      refute Data.comparable_type?(:unit, data)
      refute Data.comparable_type?({:function, :integer, :integer}, data)
      refute Data.comparable_type?({:var, 0}, data)
    end
  end

  describe "general-expression equality" do
    @tag obligations: ~w(EQ-OBL-002)
    test "tuple equality agrees on evaluator and BEAM" do
      source =
        program(
          "C035TupleEq",
          bool_match(
            binary("equal", tuple([integer(1), integer(2)]), tuple([integer(1), integer(2)])),
            integer(7),
            integer(9)
          )
        )

      {reference, beam} = dual_trace(source, "C035TupleEq")

      assert reference == beam
      assert {7, %{}} = run_reference(source)

      unequal =
        program(
          "C035TupleNe",
          bool_match(
            binary("equal", tuple([integer(1), integer(2)]), tuple([integer(1), integer(3)])),
            integer(7),
            integer(9)
          )
        )

      assert {9, %{}} = run_reference(unequal)
    end

    @tag obligations: ~w(EQ-OBL-002)
    test "constructor-value equality agrees through declared datatypes" do
      source =
        program(
          "C035NominalEq",
          bool_match(
            binary(
              "equal",
              construct("Option.Some", integer(5)),
              construct("Option.Some", integer(5))
            ),
            integer(1),
            integer(0)
          ),
          types: sample_types()
        )

      {reference, beam} = dual_trace(source, "C035NominalEq")

      assert reference == beam
      assert {1, %{}} = run_reference(source)

      unequal =
        program(
          "C035NominalNe",
          bool_match(
            binary(
              "equal",
              construct("Option.Some", integer(5)),
              construct("Option.Some", integer(6))
            ),
            integer(1),
            integer(0)
          ),
          types: sample_types()
        )

      assert {0, %{}} = run_reference(unequal)
    end

    @tag obligations: ~w(EQ-OBL-004)
    test "closure comparison rejects as EQN001" do
      source =
        program(
          "C035FnEq",
          let_expression(
            "f",
            function_expression("x", variable("x")),
            bool_match(
              binary("equal", variable("f"), variable("f")),
              integer(1),
              integer(0)
            )
          )
        )

      assert {:error, %{id: "EQN001"}} = Catena.check_json(source)

      nested =
        program(
          "C035FnTuple",
          let_expression(
            "f",
            function_expression("x", variable("x")),
            bool_match(
              binary(
                "equal",
                tuple([integer(1), variable("f")]),
                tuple([integer(1), variable("f")])
              ),
              integer(1),
              integer(0)
            )
          )
        )

      assert {:error, %{id: "EQN001"}} = Catena.check_json(nested)
    end
  end

  describe "monomorphism and the guard split" do
    @tag obligations: ~w(EQ-OBL-005)
    test "ordering non-numeric types rejects as the existing type error" do
      source =
        program(
          "C035OrderBool",
          bool_match(binary("less", boolean(true), boolean(false)), integer(1), integer(0))
        )

      assert {:error, %{id: "T002"}} = Catena.check_json(source)
    end

    @tag obligations: ~w(EQ-OBL-007)
    test "guards keep the frozen Int/Bool fragment while general expressions widen" do
      guarded =
        program(
          "C035GuardTuple",
          match_on_int([
            clause(
              pattern_wildcard(),
              integer(1),
              binary("equal", tuple([integer(1)]), tuple([integer(1)]))
            )
          ])
        )

      assert {:error, %{id: id}} = Catena.check_json(guarded)
      assert String.starts_with?(id, "CND")

      general =
        program(
          "C035GeneralTuple",
          bool_match(
            binary("equal", tuple([integer(1)]), tuple([integer(1)])),
            integer(1),
            integer(0)
          )
        )

      {:ok, _core} = Catena.check_json(general)
    end
  end

  describe "closure of the set" do
    @tag obligations: ~w(EQ-OBL-006 EQ-OBL-008)
    test "no outside type compares and results are deterministic" do
      refute function_exported?(Values, :string_compare, 2)
      refute function_exported?(Values, :binary_compare, 2)

      source =
        program(
          "C035Determinism",
          bool_match(
            binary(
              "equal",
              tuple([integer(1), tuple([boolean(true)])]),
              tuple([integer(1), tuple([boolean(true)])])
            ),
            integer(4),
            integer(8)
          )
        )

      assert {4, %{}} = run_reference(source)
      assert {4, %{}} = run_reference(source)
      {:ok, core} = Catena.check_json(source)
      assert [] = core.diagnostics
    end
  end

  defp sample_types do
    [
      %{
        "declarations" => [
          %{
            "name" => "Option",
            "parameters" => [%{"name" => "a", "kind" => "Type"}],
            "constructors" => [
              %{"name" => "None", "fields" => [], "existentials" => []},
              %{
                "name" => "Some",
                "fields" => [
                  %{"name" => "value", "type" => %{"tag" => "variable", "name" => "a"}}
                ],
                "existentials" => []
              }
            ],
            "derivations" => []
          }
        ]
      }
    ]
  end

  defp sample_data do
    ast = %{
      origin: "test://c035",
      module: "Sample",
      types: [],
      type_groups: [],
      type_exports: [],
      imports: [],
      definitions: [],
      exports: [],
      traits: [],
      instances: [],
      effects: %{families: %{}, handlers: %{}}
    }

    Catena.Data.elaborate(ast, [])
  end

  defp run_reference(source) do
    {:ok, core} = Catena.check_json(source)

    {result, _trace} =
      Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    {value_of(result), %{}}
  end

  defp value_of({:ok, value}), do: value

  defp dual_trace(source, module) do
    {:ok, core} = Catena.check_json(source)
    module_atom = String.to_atom(module)

    {{:ok, _value}, reference_trace} =
      Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    {:ok, ^module_atom, binary, _metadata} = Catena.compile_json(source)

    assert {:module, ^module_atom} =
             :code.load_binary(module_atom, ~c"c035-#{module}.beam", binary)

    {_value, beam_trace} =
      Runtime.capture_trace(fn -> apply(module_atom, :main, []) end)

    on_exit(fn ->
      :code.purge(module_atom)
      :code.delete(module_atom)
    end)

    {reference_trace, beam_trace}
  end

  defp program(module, body, options \\ []) do
    result_type = Keyword.get(options, :result_type, integer_type())

    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c035/#{module}",
      "module" => module,
      "source" => "c035.catena.json",
      "exports" => ["main"],
      "type_exports" =>
        if(Keyword.get(options, :types),
          do: [%{"name" => "Option", "visibility" => "transparent"}],
          else: []
        ),
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => result_type, "uses" => []},
          "body" => body
        }
      ],
      "effects" => [],
      "handlers" => []
    })
    |> then(fn json ->
      case Keyword.get(options, :types) do
        nil -> json
        types -> json |> JSON.decode!() |> Map.put("type_groups", types) |> JSON.encode!()
      end
    end)
  end

  defp bool_match(condition, then_body, else_body) do
    %{
      "tag" => "match",
      "scrutinee" => condition,
      "clauses" => [
        %{"pattern" => %{"tag" => "boolean", "value" => true}, "body" => then_body},
        %{"pattern" => %{"tag" => "boolean", "value" => false}, "body" => else_body}
      ]
    }
  end

  defp match_on_int(clauses) do
    %{"tag" => "match", "scrutinee" => integer(0), "clauses" => clauses}
  end

  defp clause(pattern, body), do: %{"pattern" => pattern, "body" => body}
  defp clause(pattern, body, guard), do: %{"pattern" => pattern, "guard" => guard, "body" => body}

  defp pattern_wildcard, do: %{"tag" => "wildcard"}

  defp construct(constructor, value) do
    %{
      "tag" => "construct",
      "constructor" => constructor,
      "fields" => [%{"name" => "value", "value" => value}]
    }
  end

  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp function_expression(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp binary(operator, left, right),
    do: %{"tag" => "binary", "operator" => operator, "left" => left, "right" => right}

  defp tuple(elements), do: %{"tag" => "tuple", "elements" => elements}

  defp let_expression(name, value, body),
    do: %{"tag" => "let", "name" => name, "value" => value, "body" => body}
end
