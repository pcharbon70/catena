defmodule Catena.C030EvaluationOrderTest do
  use ExUnit.Case, async: false

  alias Catena.Effect.Runtime
  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31)

  describe "revision registration" do
    @tag obligations: ~w(EO-OBL-001 EO-OBL-008)
    test "0.1.26 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.31"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.26" in LanguageVersion.compilable_revisions()
      refute "0.1.26" in LanguageVersion.artifact_versions()
      refute "0.1.26" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("evaluation-order", "0.1.26")

      change =
        Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-26-evaluation-order"))

      assert String.contains?(
               change["specification"],
               "evaluation-order/ordered-forms-and-entry-rule.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.31"}}} = Catena.decode_source_text("")
      assert {:ok, _} = Catena.build_namespace_environment([])
      assert {:ok, _} = Catena.compile_scc([])
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Catena, :declared_order, 1)
      refute function_exported?(Catena.Effect.Runtime, :order_table, 0)
    end
  end

  describe "dual-target trace agreement" do
    @tag obligations: ~w(EO-OBL-002 EO-OBL-006)
    test "call arguments evaluate left-to-right after the callee" do
      source =
        program(
          "C030CallOrder",
          handle(
            call(variable("pair"), [ask("first", 10), ask("second", 20)]),
            "LogAsk"
          ),
          definitions: [
            pair_definition()
          ]
        )

      {reference, beam} = dual_trace(source, "C030CallOrder")

      assert reference == beam
      assert requests(reference) == [:first, :second]

      assert labels(reference) ==
               [:handle, :request, :clause, :resume, :request, :clause, :resume, :return]
    end

    @tag obligations: ~w(EO-OBL-002 EO-OBL-006)
    test "tuple and let order follow the table" do
      source =
        program(
          "C030TupleOrder",
          handle(
            let_expression(
              "first",
              tuple([ask("first", 1), ask("second", 2)]),
              let_expression("second", ask("third", 3), variable("first"))
            ),
            "LogAsk"
          ),
          result_type: tuple_of([integer_type(), integer_type()])
        )

      {reference, beam} = dual_trace(source, "C030TupleOrder")

      assert reference == beam
      assert requests(reference) == [:first, :second, :third]
    end

    @tag obligations: ~w(EO-OBL-002 EO-OBL-007)
    test "and skips its right operand and or evaluates both operands when required" do
      skipped =
        program(
          "C030SkipOrder",
          handle(
            binary(
              "and",
              equals(ask("first", 1), ask("first", 2)),
              binary("equal", ask("second", 1), ask("third", 2))
            ),
            "LogAsk"
          ),
          result_type: boolean_type()
        )

      {skipped_reference, skipped_beam} = dual_trace(skipped, "C030SkipOrder")
      assert skipped_reference == skipped_beam
      assert requests(skipped_reference) == [:first, :first]

      forced =
        program(
          "C030ForcedOrder",
          handle(
            binary(
              "or",
              equals(ask("first", 1), ask("first", 2)),
              binary("equal", ask("second", 1), ask("third", 2))
            ),
            "LogAsk"
          ),
          result_type: boolean_type()
        )

      {forced_reference, forced_beam} = dual_trace(forced, "C030ForcedOrder")
      assert forced_reference == forced_beam
      assert requests(forced_reference) == [:first, :first, :second, :third]
    end

    @tag obligations: ~w(EO-OBL-003 EO-OBL-006)
    test "curried multi-argument application is repeated unary left-to-right" do
      source =
        program(
          "C030CurriedOrder",
          handle(
            call(call(variable("triple"), [ask("first", 1)]), [ask("second", 2)]),
            "LogAsk"
          ),
          definitions: [
            %{
              "name" => "triple",
              "parameters" => ["left"],
              "signature" => %{
                "forall" => [],
                "type" =>
                  function_type(integer_type(), function_type(integer_type(), integer_type())),
                "uses" => []
              },
              "body" =>
                function_expression("right", binary("add", variable("left"), variable("right")))
            }
          ]
        )

      {reference, beam} = dual_trace(source, "C030CurriedOrder")

      assert reference == beam
      assert requests(reference) == [:first, :second]
    end

    @tag obligations: ~w(EO-OBL-003 EO-OBL-004 EO-OBL-006)
    test "handler installation precedes body evaluation" do
      source =
        program(
          "C030HandlerOrder",
          handle(ask("first", 5), "LogAsk")
        )

      {reference, beam} = dual_trace(source, "C030HandlerOrder")

      assert reference == beam
      assert labels(reference) == [:handle, :request, :clause, :resume, :return]
      assert hd(labels(reference)) == :handle
      assert requests(reference) == [:first]
    end

    @tag obligations: ~w(EO-OBL-002 EO-OBL-006)
    test "binary operators evaluate left then right exactly once" do
      source =
        program(
          "C030BinaryOrder",
          handle(
            binary("add", ask("first", 10), ask("second", 20)),
            "LogAsk"
          )
        )

      {reference, beam} = dual_trace(source, "C030BinaryOrder")

      assert reference == beam
      assert requests(reference) == [:first, :second]
    end
  end

  describe "closure and determinism" do
    @tag obligations: ~w(EO-OBL-004 EO-OBL-005)
    test "fragment rules keep their owning areas' shapes and closed forms stay unordered" do
      source =
        program(
          "C030Fragment",
          handle(
            let_expression("boxed", ask("first", 7), variable("boxed")),
            "LogAsk"
          )
        )

      {reference, _beam} = dual_trace(source, "C030Fragment")
      assert labels(reference) == [:handle, :request, :clause, :resume, :return]

      refute function_exported?(Catena, :collection_order, 0)
      refute function_exported?(Catena, :interpolation_order, 0)
    end

    @tag obligations: ~w(EO-OBL-006 EO-OBL-008)
    test "equal programs produce equal traces across repeated dual runs" do
      source =
        program(
          "C030Determinism",
          handle(
            tuple([ask("first", 1), ask("second", 2), ask("third", 3)]),
            "LogAsk"
          ),
          result_type: tuple_of([integer_type(), integer_type(), integer_type()])
        )

      {first_reference, first_beam} = dual_trace(source, "C030Determinism")
      {second_reference, second_beam} = dual_trace(source, "C030Determinism")

      assert first_reference == second_reference
      assert first_beam == second_beam
      assert first_reference == first_beam
    end
  end

  defp dual_trace(source, module) do
    {:ok, core} = Catena.check_json(source)
    module_atom = String.to_atom(module)

    {{:ok, _value}, reference_trace} =
      Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    {:ok, ^module_atom, binary, _metadata} = Catena.compile_json(source)

    assert {:module, ^module_atom} =
             :code.load_binary(module_atom, ~c"c030-#{module}.beam", binary)

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

  defp labels(trace) do
    Enum.map(trace, fn
      event when is_tuple(event) -> elem(event, 0)
      event -> event
    end)
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
      "version" => "0.1.5",
      "origin" => "test://c030/#{module}",
      "module" => module,
      "source" => "c030.catena.json",
      "exports" => Enum.map(definitions, & &1["name"]),
      "type_exports" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => definitions,
      "effects" => [effect("Ask", ~w(first second third))],
      "handlers" => [logging_handler(Keyword.get(options, :result_type, integer_type()))]
    })
  end

  defp pair_definition do
    %{
      "name" => "pair",
      "parameters" => ["left"],
      "signature" => %{
        "forall" => [],
        "type" => function_type(integer_type(), function_type(integer_type(), integer_type())),
        "uses" => []
      },
      "body" => function_expression("right", binary("add", variable("left"), variable("right")))
    }
  end

  defp ask(operation, value), do: request("Ask", operation, [integer(value)])

  defp effect(name, operations) do
    %{
      "name" => name,
      "parameters" => [],
      "visibility" => "public",
      "operations" =>
        Enum.map(operations, fn operation ->
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
        Enum.map(~w(first second third), fn operation ->
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
  defp boolean_type, do: %{"tag" => "boolean"}
  defp variable_type(name), do: %{"tag" => "variable", "name" => name}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result}

  defp tuple_of(elements),
    do: %{"tag" => "tuple", "elements" => elements}

  defp equals(left, right),
    do: binary("equal", left, right)

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp function_expression(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp use_entry(effect), do: %{"effect" => effect, "arguments" => [], "capability" => "ask"}

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

  defp handle(expression, handler_name),
    do: %{
      "tag" => "handle",
      "expression" => expression,
      "handler" => handler_name,
      "arguments" => [],
      "capability" => "ask"
    }
end
