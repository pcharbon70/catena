defmodule Catena.C032FunctionsTest do
  use ExUnit.Case, async: false

  alias Catena.Effect.Runtime
  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33)

  describe "revision registration" do
    @tag obligations: ~w(FC-OBL-001 FC-OBL-008)
    test "0.1.28 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.33"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.28" in LanguageVersion.compilable_revisions()
      refute "0.1.28" in LanguageVersion.artifact_versions()
      refute "0.1.28" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("functions-and-calls", "0.1.28")

      change =
        Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-28-functions-and-calls"))

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "functions-and-calls/arity-and-application.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.33"}}} = Catena.decode_source_text("")
      assert {:ok, _} = Catena.build_namespace_environment([])
      assert {:ok, _} = Catena.compile_scc([])
      assert true = Catena.Values.value?(1.5)
      assert "BS001" in LanguageLifecycle.warning_ids()

      refute function_exported?(Catena, :arity_of, 1)
      refute function_exported?(Catena, :check_arity, 2)
    end
  end

  describe "the semantic-unary model" do
    @tag obligations: ~w(FC-OBL-002)
    test "multi-parameter definitions desugar to nested unary application" do
      source =
        program(
          "C032Curried",
          handle(
            let_expression(
              "result",
              call(call(variable("add"), [ask("first", 3)]), [ask("second", 4)]),
              variable("result")
            ),
            "LogAsk"
          ),
          definitions: [add_definition()]
        )

      {reference, beam} = dual_trace(source, "C032Curried")

      assert reference == beam
      assert requests(reference) == [:first, :second]
      assert {7, %{}} = run_reference(source)
    end

    @tag obligations: ~w(FC-OBL-003 FC-OBL-004)
    test "a prefix application is a callable value capturing its environment immutably" do
      source =
        program(
          "C032Partial",
          handle(
            let_expression(
              "base",
              ask("first", 10),
              let_expression(
                "partial",
                call(variable("add"), [variable("base")]),
                tuple([
                  call(variable("partial"), [ask("second", 5)]),
                  call(variable("partial"), [ask("second", 7)])
                ])
              )
            ),
            "LogAsk"
          ),
          result_type: tuple_of([integer_type(), integer_type()]),
          definitions: [add_definition()]
        )

      {reference, beam} = dual_trace(source, "C032Partial")

      assert reference == beam
      assert requests(reference) == [:first, :second, :second]
      assert {{15, 17}, %{}} = run_reference(source)
    end

    @tag obligations: ~w(FC-OBL-005)
    test "the let-bound closure is the local-function form, first-class" do
      source =
        program(
          "C032Local",
          handle(
            let_expression(
              "double",
              function_expression("x", binary("add", variable("x"), variable("x"))),
              let_expression(
                "twice",
                call(variable("apply2"), [variable("double")]),
                call(variable("twice"), [ask("first", 6)])
              )
            ),
            "LogAsk"
          ),
          definitions: [
            %{
              "name" => "apply2",
              "parameters" => ["f"],
              "signature" =>
                signature(
                  function_type(
                    function_type(integer_type(), integer_type()),
                    function_type(integer_type(), integer_type())
                  )
                ),
              "body" => function_expression("v", call(variable("f"), [variable("v")]))
            }
          ]
        )

      {reference, beam} = dual_trace(source, "C032Local")

      assert reference == beam
      assert requests(reference) == [:first]
      assert {12, %{}} = run_reference(source)
    end

    @tag obligations: ~w(FC-OBL-007)
    test "named functions are definitions with exported interface presence" do
      source =
        program(
          "C032Named",
          handle(call(variable("add"), [integer(2), integer(3)]), "LogAsk"),
          definitions: [add_definition()],
          exports: ["add", "main"]
        )

      {:ok, core} = Catena.check_json(source)
      assert {5, %{}} = run_value(core)

      {:ok, :C032Named, _binary, metadata} = Catena.compile_json(source)
      names = Enum.map(metadata.interface["values"], & &1["name"])
      assert "add" in names and "main" in names
    end
  end

  describe "proper tail calls" do
    @tail_source """
    (module C032Tail
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c032-tail")
      (export value main)
      (def count
        (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
        (fn (n Int)
          (fn (acc Int)
            (match (var n)
              (case 0 (var acc))
              (case _ (call (call (var count) (subtract (var n) 1)) (add (var acc) 1)))))))
      (def main
        (signature Int (uses))
        (call (call (var count) 5000000) 0)))
    """

    @tag obligations: ~w(FC-OBL-006)
    test "a five-million-iteration match-dispatched tail recursion completes on BEAM" do
      assert {:ok, core} = Catena.check_kernel(@tail_source)
      assert {:ok, :C032Tail, binary, _metadata} = Catena.compile_kernel(@tail_source)

      assert {:module, :C032Tail} =
               :code.load_binary(:C032Tail, ~c"c032_tail.beam", binary)

      assert apply(:C032Tail, :main, []) == 5_000_000

      on_exit(fn ->
        :code.purge(:C032Tail)
        :code.delete(:C032Tail)
      end)
    end

    @tag obligations: ~w(FC-OBL-006)
    test "the stepper terminates a tail recursion within its budget" do
      source =
        String.replace(
          @tail_source,
          "(call (call (var count) 5000000) 0)",
          "(call (call (var count) 500) 0)"
        )

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:ok, 500, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")
    end
  end

  describe "determinism and exclusions" do
    @tag obligations: ~w(FC-OBL-008)
    test "results are deterministic and no arity or capture machinery leaks" do
      source =
        program(
          "C032Determinism",
          handle(call(call(variable("add"), [integer(2)]), [integer(3)]), "LogAsk"),
          definitions: [add_definition()]
        )

      assert {5, %{}} = run_reference(source)
      assert {5, %{}} = run_reference(source)

      refute function_exported?(Catena, :capture_list, 1)
      refute function_exported?(Catena, :local_recursion, 1)
      refute function_exported?(Catena, :calling_convention, 1)
    end
  end

  defp run_reference(source) do
    {:ok, core} = Catena.check_json(source)
    run_value(core)
  end

  defp run_value(core) do
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
             :code.load_binary(module_atom, ~c"c032-#{module}.beam", binary)

    {_value, beam_trace} =
      Runtime.capture_trace(fn -> apply(module_atom, :main, []) end)

    on_exit(fn ->
      :code.purge(module_atom)
      :code.delete(module_atom)
    end)

    {reference_trace, beam_trace}
  end

  defp requests(trace) do
    Enum.flat_map(trace, fn
      {:request, _family, _capability, operation} -> [operation]
      _other -> []
    end)
  end

  defp add_definition do
    %{
      "name" => "add",
      "parameters" => ["left"],
      "signature" =>
        signature(function_type(integer_type(), function_type(integer_type(), integer_type()))),
      "body" => function_expression("right", binary("add", variable("left"), variable("right")))
    }
  end

  defp program(module, body, options \\ []) do
    result_type = Keyword.get(options, :result_type, integer_type())

    definitions =
      Keyword.get(options, :definitions, []) ++
        [
          %{
            "name" => "main",
            "parameters" => [],
            "signature" => %{"forall" => [], "type" => result_type, "uses" => []},
            "body" => body
          }
        ]

    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c032/#{module}",
      "module" => module,
      "source" => "c032.catena.json",
      "exports" => Keyword.get(options, :exports, ["main"]),
      "type_exports" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => definitions,
      "effects" => [effect("Ask")],
      "handlers" => [logging_handler(Keyword.get(options, :result_type, integer_type()))]
    })
  end

  defp effect(name) do
    %{
      "name" => name,
      "parameters" => [],
      "visibility" => "public",
      "operations" =>
        Enum.map(~w(first second), fn operation ->
          %{
            "name" => operation,
            "parameters" => [%{"name" => "value", "type" => integer_type()}],
            "result" => integer_type()
          }
        end)
    }
  end

  defp logging_handler(handler_type \\ integer_type()) do
    %{
      "name" => "LogAsk",
      "effect" => "Ask",
      "arguments" => [],
      "forall" => [],
      "visibility" => "public",
      "parameters" => [],
      "input" => handler_type,
      "output" => handler_type,
      "uses" => [],
      "return" => %{"parameter" => "result", "body" => variable("result")},
      "operations" =>
        Enum.map(~w(first second), fn operation ->
          %{
            "operation" => operation,
            "parameters" => ["value"],
            "resumption" => "next",
            "body" => resume("next", variable("value"))
          }
        end)
    }
  end

  defp signature(type), do: %{"forall" => [], "type" => type, "uses" => []}
  defp integer_type, do: %{"tag" => "integer"}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result}

  defp tuple_of(elements), do: %{"tag" => "tuple", "elements" => elements}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp function_expression(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp binary(operator, left, right),
    do: %{"tag" => "binary", "operator" => operator, "left" => left, "right" => right}

  defp let_expression(name, value, body),
    do: %{"tag" => "let", "name" => name, "value" => value, "body" => body}

  defp tuple(elements), do: %{"tag" => "tuple", "elements" => elements}

  defp ask(operation, value), do: request("Ask", operation, [integer(value)])

  defp request(effect, operation, arguments),
    do: %{
      "tag" => "request",
      "effect" => effect,
      "operation" => operation,
      "arguments" => arguments
    }

  defp resume(resumption, value),
    do: %{"tag" => "resume", "resumption" => resumption, "value" => value}

  defp handle(expression, handler_name),
    do: %{
      "tag" => "handle",
      "expression" => expression,
      "handler" => handler_name,
      "arguments" => [],
      "capability" => "ask"
    }
end
