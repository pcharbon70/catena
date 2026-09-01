defmodule Catena.C031BindingsTest do
  use ExUnit.Case, async: false

  alias Catena.Bindings
  alias Catena.Effect.Runtime
  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41)

  describe "revision registration" do
    @tag obligations: ~w(BS-OBL-001 BS-OBL-008)
    test "0.1.27 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.41"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.27" in LanguageVersion.compilable_revisions()
      refute "0.1.27" in LanguageVersion.artifact_versions()
      refute "0.1.27" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} ==
               LanguageLifecycle.state("bindings-and-sequencing", "0.1.27")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-27-bindings-and-sequencing")
        )

      assert change["affects"] == ~w(static-meaning diagnostics)

      assert String.contains?(
               change["specification"],
               "bindings-and-sequencing/binding-structure-and-scope.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.41"}}} = Catena.decode_source_text("")
      assert {:ok, _} = Catena.build_namespace_environment([])
      assert {:ok, _} = Catena.compile_scc([])
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Bindings, :recursive_let, 2)
      refute function_exported?(Bindings, :pattern_bind, 2)
      assert Bindings.diagnostic_id() == "BS001"
    end
  end

  describe "binding structure" do
    @tag obligations: ~w(BS-OBL-002)
    test "a self-referential let RHS is T001 unbound — non-recursion" do
      source =
        program("C031SelfRef", let_expression("x", variable("x"), variable("x")))

      assert {:error, %{id: "T001"}} = Catena.check_json(source)
    end

    @tag obligations: ~w(BS-OBL-003)
    test "inner bindings silently shadow outer, definitions, and imports" do
      source =
        program(
          "C031Shadow",
          nil,
          definitions: [
            pair_definition(),
            %{
              "name" => "main",
              "parameters" => [],
              "signature" => signature(integer_type()),
              "body" =>
                let_expression(
                  "value",
                  integer(1),
                  let_expression(
                    "value",
                    call(variable("pair"), [integer(10), integer(20)]),
                    variable("value")
                  )
                )
            }
          ]
        )

      {:ok, core} = Catena.check_json(source)
      assert [] = core.diagnostics

      assert {30, %{}} = run_reference(core)
    end

    @tag obligations: ~w(BS-OBL-004)
    test "recursion is definitions-only; a named recursive definition runs" do
      source = """
      (module C031Recursion
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c031-recursion")
        (export value main)
        (def count
          (signature (Fn Int (effects) Int) (uses))
          (fn (n Int)
            (match (var n)
              (case 0 0)
              (case _ (call (var count) (subtract (var n) 1))))))
        (def main
          (signature Int (uses))
          (call (var count) 5)))
      """

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:ok, 0, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")
    end
  end

  describe "unused bindings and sequencing" do
    @tag obligations: ~w(BS-OBL-005 BS-OBL-007)
    test "an unused binding's RHS effects are preserved; the let idiom sequences" do
      source =
        program(
          "C031Unused",
          handle(
            let_expression(
              "boxed",
              ask("first", 1),
              ask("second", 2)
            ),
            "LogAsk"
          )
        )

      {:ok, core} = Catena.check_json(source)

      {reference, beam} = dual_trace(source, "C031Unused")

      assert reference == beam
      assert requests(reference) == [:first, :second]
      assert {2, %{}} = run_reference(core)
    end

    @tag obligations: ~w(BS-OBL-006)
    test "BS001 fires exactly on non-underscore-prefixed unused binders" do
      unused =
        program(
          "C031UnusedWarn",
          handle(
            let_expression(
              "boxed",
              ask("first", 1),
              ask("second", 2)
            ),
            "LogAsk"
          )
        )

      {:ok, core} = Catena.check_json(unused)

      assert [%{id: "BS001", severity: :warning, details: %{binding: "boxed"}}] =
               core.diagnostics

      used =
        program(
          "C031Used",
          handle(
            let_expression(
              "boxed",
              ask("first", 1),
              binary("add", variable("boxed"), ask("second", 2))
            ),
            "LogAsk"
          )
        )

      {:ok, used_core} = Catena.check_json(used)
      assert used_core.diagnostics == []

      # The JSON AST value-name spelling cannot begin with an underscore
      # (lowercase-initial rule), so the exemption is exercised on
      # typed-core terms directly below.
      exempt_core = %{
        tag: :let,
        name: "_discard",
        path: "$.definitions[0].body",
        value: %{tag: :integer, value: 1, path: "$.definitions[0].body.value"},
        body: %{tag: :integer, value: 2, path: "$.definitions[0].body.body"}
      }

      assert [] == Bindings.unused_binding_warnings(%{expression: exempt_core}, "main")
    end

    @tag obligations: ~w(BS-OBL-006)
    test "denying BS001 promotes the warning to an error" do
      source =
        program(
          "C031Denied",
          handle(
            let_expression(
              "boxed",
              ask("first", 1),
              ask("second", 2)
            ),
            "LogAsk"
          )
        )

      assert {:error, %{id: "BS001", severity: :error, details: %{promoted_from_warning: true}}} =
               Catena.check_json(source, denied_diagnostics: ["BS001"])
    end
  end

  describe "determinism and exclusions" do
    @tag obligations: ~w(BS-OBL-008)
    test "warning walks are deterministic and predecessor revisions stay silent" do
      source =
        program(
          "C031Determinism",
          handle(
            let_expression(
              "boxed",
              ask("first", 1),
              ask("second", 2)
            ),
            "LogAsk"
          )
        )

      {:ok, first} = Catena.check_json(source)
      {:ok, second} = Catena.check_json(source)
      assert first.diagnostics == second.diagnostics

      legacy =
        program(
          "C031Legacy",
          handle(
            let_expression(
              "boxed",
              ask("first", 1),
              ask("second", 2)
            ),
            "LogAsk"
          )
        )
        |> JSON.decode!()
        |> Map.drop(["edition", "language_revision", "previews"])
        |> JSON.encode!()

      {:ok, legacy_core} = Catena.check_json(legacy)
      assert [%{id: "BS001"}] = legacy_core.diagnostics

      refute function_exported?(Bindings, :tail_call_guarantee, 0)
      refute function_exported?(Bindings, :branch_form, 0)
    end
  end

  defp run_reference(core) do
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
             :code.load_binary(module_atom, ~c"c031-#{module}.beam", binary)

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

  defp program(module, body, options \\ [])

  defp program(module, body, options) when is_map(body) do
    result_type = Keyword.get(options, :result_type, integer_type())

    own_main = [
      %{
        "name" => "main",
        "parameters" => [],
        "signature" => %{"forall" => [], "type" => result_type, "uses" => []},
        "body" => body
      }
    ]

    definitions =
      case Keyword.get(options, :definitions, []) do
        [] -> own_main
        provided -> provided
      end

    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c031/#{module}",
      "module" => module,
      "source" => "c031.catena.json",
      "exports" => ["main"],
      "type_exports" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => definitions,
      "effects" => [effect("Ask")],
      "handlers" => [logging_handler()]
    })
  end

  defp program(module, nil, options) do
    program(module, integer(0), options)
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

  defp logging_handler do
    %{
      "name" => "LogAsk",
      "effect" => "Ask",
      "arguments" => [],
      "forall" => [],
      "visibility" => "public",
      "parameters" => [],
      "input" => integer_type(),
      "output" => integer_type(),
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

  defp match_expression(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp clause(pattern, body),
    do: %{"pattern" => pattern, "body" => body}

  defp pattern_integer(value), do: %{"tag" => "integer", "value" => value}
  defp pattern_wildcard, do: %{"tag" => "wildcard"}

  defp signature(type), do: %{"forall" => [], "type" => type, "uses" => []}
  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result}

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
