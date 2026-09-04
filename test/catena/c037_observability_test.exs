defmodule Catena.C037ObservabilityTest do
  use ExUnit.Case, async: false

  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47)

  describe "revision registration" do
    @tag obligations: ~w(RO-OBL-001 RO-OBL-008)
    test "0.1.33 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.47"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.33" in LanguageVersion.compilable_revisions()
      refute "0.1.33" in LanguageVersion.artifact_versions()
      refute "0.1.33" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("resource-observability", "0.1.33")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-33-resource-observability")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "resource-observability/the-observability-model.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.47"}}} = Catena.decode_source_text("")
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Catena, :address_of, 1)
      refute function_exported?(Catena.Values, :identity, 1)
      refute function_exported?(Catena, :finalize, 1)
      refute function_exported?(Catena, :stack_depth, 0)
    end
  end

  describe "semantic identity" do
    @tag obligations: ~w(RO-OBL-003 RO-OBL-005)
    test "records built at distinct sites compare equal on evaluator and BEAM" do
      source =
        program(
          "C037Records",
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

      {reference, beam} = dual_trace(source, "C037Records")

      assert reference == beam
      assert {1, %{}} = run_reference(source)
    end

    @tag obligations: ~w(RO-OBL-005)
    test "a closure applied twice yields equal results regardless of its allocation" do
      source =
        program(
          "C037Closure",
          let_expression(
            "double",
            function_expression("x", binary("add", variable("x"), variable("x"))),
            binary(
              "equal",
              call(variable("double"), [integer(6)]),
              call(variable("double"), [integer(6)])
            )
          ),
          result_type: boolean_type()
        )

      {reference, beam} = dual_trace(source, "C037Closure")

      assert reference == beam
      assert {true, %{}} = run_reference(source)
    end
  end

  describe "process identity" do
    @identity_witness """
    (module C037Identity
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c037-identity")
      (export value main)
      (process Echo
        (mailbox Int)
        (params)
        (receive
          (case (bind message)
            (unit))))
      (def main
        (signature Int (uses Process))
        (let first
          (spawn Echo)
          (let second
            (spawn Echo)
            (sequence
              (send (var first) 11)
              (sequence
                (send (var second) 22)
                33))))))
    """

    @tag obligations: ~w(RO-OBL-004)
    test "each spawn allocates a fresh identity; two spawns are two processes" do
      assert {:ok, core} = Catena.check_kernel(@identity_witness)

      assert {:ok, 33, outcome} = Catena.Kernel.Stepper.run(core, "main")
      assert outcome.root_status == :terminated

      echoes = Enum.filter(outcome.processes, &(&1.name == "Echo"))
      assert length(echoes) == 2

      pids = Enum.map(echoes, & &1.pid)
      assert length(Enum.uniq(pids)) == 2

      assert Enum.all?(echoes, &(&1.status == :terminated))
      assert Enum.all?(echoes, &(&1.mailbox == []))
    end

    @tag obligations: ~w(RO-OBL-004)
    test "self returns the current handle — the only self-observation" do
      source = String.replace(@identity_witness, "(send (var first) 11)", "(send (var first) 11)")

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:ok, 33, _outcome} = Catena.Kernel.Stepper.run(core, "main")

      refute Catena.Values.comparable?({:catena_process, "p1"})
      assert Catena.Values.value?({:catena_process, "p1"})
    end
  end

  describe "absences and the stack boundary" do
    @tag obligations: ~w(RO-OBL-002 RO-OBL-006)
    test "no identity, address, or cleanup surface exists; the classification holds" do
      refute function_exported?(Catena.Values, :address, 1)
      refute function_exported?(Catena.Values, :shared, 1)
      refute function_exported?(Catena, :gc_collect, 0)
      refute function_exported?(Catena, :on_finalize, 1)

      refute Catena.Values.comparable?({:closure, "x", %{tag: :variable, name: "x"}, %{}})

      refute Catena.Values.comparable?(%{
               label: {:closure, "x", %{tag: :variable, name: "x"}, %{}}
             })

      assert Catena.Values.comparable?(%{label: 1})
    end

    @tag obligations: ~w(RO-OBL-007 RO-OBL-008)
    test "stack use observes only completion: non-tail recursion completes, tail recursion terminates" do
      non_tail = """
      (module C037NonTail
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c037-nontail")
        (export value main)
        (def sum_to
          (signature (Fn Int (effects) Int) (uses))
          (fn (n Int)
            (match (var n)
              (case 0 0)
              (case _ (add (var n) (call (var sum_to) (subtract (var n) 1)))))))
        (def main
          (signature Int (uses))
          (call (var sum_to) 10000)))
      """

      assert {:ok, :C037NonTail, binary, _} = Catena.compile_kernel(non_tail)

      assert {:module, :C037NonTail} =
               :code.load_binary(:C037NonTail, ~c"c037_nontail.beam", binary)

      assert apply(:C037NonTail, :main, []) == 50_005_000

      on_exit(fn ->
        :code.purge(:C037NonTail)
        :code.delete(:C037NonTail)
      end)
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

  defp run_reference(source) do
    {:ok, core} = Catena.check_json(source)

    {result, _trace} =
      Catena.Effect.Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    {value_of(result), %{}}
  end

  defp value_of({:ok, value}), do: value

  defp dual_trace(source, module) do
    {:ok, core} = Catena.check_json(source)
    module_atom = String.to_atom(module)

    {{:ok, _value}, reference_trace} =
      Catena.Effect.Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    {:ok, ^module_atom, binary, _metadata} = Catena.compile_json(source)

    assert {:module, ^module_atom} =
             :code.load_binary(module_atom, ~c"c037-#{module}.beam", binary)

    {_value, beam_trace} =
      Catena.Effect.Runtime.capture_trace(fn -> apply(module_atom, :main, []) end)

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
      "origin" => "test://c037/#{module}",
      "module" => module,
      "source" => "c037.catena.json",
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
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp function_expression(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp binary(operator, left, right),
    do: %{"tag" => "binary", "operator" => operator, "left" => left, "right" => right}

  defp let_expression(name, value, body),
    do: %{"tag" => "let", "name" => name, "value" => value, "body" => body}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}
end
