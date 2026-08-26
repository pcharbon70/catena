defmodule Catena.C033BranchingTest do
  use ExUnit.Case, async: false

  alias Catena.Effect.Runtime
  alias Catena.LanguageLifecycle
  alias Catena.LanguageVersion

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29)

  describe "revision registration" do
    @tag obligations: ~w(BR-OBL-001 BR-OBL-002)
    test "0.1.29 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.29"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.29" in LanguageVersion.compilable_revisions()
      refute "0.1.29" in LanguageVersion.artifact_versions()
      refute "0.1.29" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("branching", "0.1.29")

      change = Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-29-branching"))

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "branching/the-branch-form-and-its-desugaring.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.29"}}} = Catena.decode_source_text("")
      assert {:ok, _} = Catena.build_namespace_environment([])
      assert {:ok, _} = Catena.compile_scc([])
      assert true = Catena.Values.value?(1.5)
      assert "BS001" in LanguageLifecycle.warning_ids()

      refute function_exported?(Catena, :if_expression, 2)
      refute function_exported?(Catena, :early_return, 1)
      refute function_exported?(Catena, :break_statement, 0)
    end
  end

  describe "the conditional as Bool-pattern match" do
    @tag obligations: ~w(BR-OBL-003 BR-OBL-006)
    test "true/false dispatch agrees on evaluator and BEAM with only the selected body's effects" do
      source =
        program(
          "C033Conditional",
          handle(
            match_expression(
              flag(1),
              [
                clause(pattern_boolean(true), ask("second", 10)),
                clause(pattern_boolean(false), ask("third", 20))
              ]
            ),
            "LogAsk"
          )
        )

      {reference, beam} = dual_trace(source, "C033Conditional")

      assert reference == beam
      assert requests(reference) == [:flag, :second]
      assert {10, %{}} = run_reference(source)
    end

    @tag obligations: ~w(BR-OBL-007)
    test "a false condition falls through to a later clause" do
      source =
        program(
          "C033Fallthrough",
          handle(
            match_expression(
              ask("first", 5),
              [
                clause(pattern_bind("n"), integer(1), guard_less_than_3()),
                clause(pattern_wildcard(), ask("second", 5))
              ]
            ),
            "LogAsk"
          )
        )

      {reference, beam} = dual_trace(source, "C033Fallthrough")

      assert reference == beam
      assert requests(reference) == [:first, :second]
      assert {5, %{}} = run_reference(source)
    end

    @tag obligations: ~w(BR-OBL-004)
    test "a true guard commits to its clause; branch typing unifies bodies with the match type" do
      source =
        program(
          "C033Commit",
          handle(
            match_expression(
              ask("first", 7),
              [
                clause(pattern_wildcard(), ask("second", 2), guard_true())
              ]
            ),
            "LogAsk"
          )
        )

      {reference, beam} = dual_trace(source, "C033Commit")

      assert reference == beam
      assert requests(reference) == [:first, :second]
      assert {2, %{}} = run_reference(source)

      both =
        program(
          "C033Typing",
          handle(
            match_expression(
              boolean(true),
              [
                clause(pattern_boolean(true), integer(1)),
                clause(pattern_boolean(false), integer(2))
              ]
            ),
            "LogAsk"
          )
        )

      {:ok, typed_core} = Catena.check_json(both)
      assert {1, %{}} = run_value(typed_core)
    end
  end

  describe "coverage and absence" do
    @tag obligations: ~w(BR-OBL-004)
    test "a non-exhaustive match rejects as M001 with a witness, unchanged" do
      source =
        program(
          "C033Exhaustive",
          handle(
            match_expression(
              boolean(true),
              [clause(pattern_boolean(true), integer(1))]
            ),
            "LogAsk"
          )
        )

      assert {:error, %{id: "M001"}} = Catena.check_json(source)
    end

    @tag obligations: ~w(BR-OBL-005)
    test "no statement or control entry points exist; effects sequence through the let idiom" do
      refute function_exported?(Catena, :return_statement, 1)
      refute function_exported?(Catena, :break_statement, 0)
      refute function_exported?(Catena, :continue_statement, 0)

      source =
        program(
          "C033Idiom",
          handle(
            let_expression(
              "boxed",
              ask("first", 9),
              ask("second", 4)
            ),
            "LogAsk"
          )
        )

      {reference, beam} = dual_trace(source, "C033Idiom")

      assert reference == beam
      assert requests(reference) == [:first, :second]
    end
  end

  describe "determinism" do
    @tag obligations: ~w(BR-OBL-008)
    test "equal matches select equal bodies with equal traces and no new families appear" do
      source =
        program(
          "C033Determinism",
          handle(
            match_expression(
              boolean(false),
              [
                clause(pattern_boolean(true), ask("first", 1)),
                clause(pattern_boolean(false), ask("second", 2))
              ]
            ),
            "LogAsk"
          )
        )

      assert {2, %{}} = run_reference(source)
      assert {2, %{}} = run_reference(source)

      {:ok, core} = Catena.check_json(source)
      assert [] = core.diagnostics

      refute function_exported?(Catena.Diagnostic, :br_family, 0)
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
             :code.load_binary(module_atom, ~c"c033-#{module}.beam", binary)

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

  defp program(module, body, options \\ []) do
    result_type = Keyword.get(options, :result_type, integer_type())

    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c033/#{module}",
      "module" => module,
      "source" => "c033.catena.json",
      "exports" => ["main"],
      "type_exports" => [],
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
        Enum.map(~w(first second third), fn operation ->
          %{
            "name" => operation,
            "parameters" => [%{"name" => "value", "type" => integer_type()}],
            "result" => integer_type()
          }
        end) ++
          [
            %{
              "name" => "flag",
              "parameters" => [%{"name" => "value", "type" => integer_type()}],
              "result" => %{"tag" => "boolean"}
            }
          ]
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
        end) ++
          [
            %{
              "operation" => "flag",
              "parameters" => ["value"],
              "resumption" => "next",
              "body" => resume("next", boolean(true))
            }
          ]
    }
  end

  defp match_expression(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp clause(pattern, body), do: %{"pattern" => pattern, "body" => body}

  defp clause(pattern, body, guard),
    do: %{"pattern" => pattern, "guard" => guard, "body" => body}

  defp pattern_boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp pattern_wildcard, do: %{"tag" => "wildcard"}
  defp pattern_bind(name), do: %{"tag" => "bind", "name" => name}

  defp guard_true(), do: boolean(true)

  defp guard_less_than_3(),
    do: %{
      "tag" => "binary",
      "operator" => "less",
      "left" => variable("n"),
      "right" => integer(3)
    }

  defp signature(type), do: %{"forall" => [], "type" => type, "uses" => []}
  defp integer_type, do: %{"tag" => "integer"}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp let_expression(name, value, body),
    do: %{"tag" => "let", "name" => name, "value" => value, "body" => body}

  defp ask(operation, value), do: request("Ask", operation, [integer(value)])

  defp flag(value), do: request("Ask", "flag", [integer(value)])

  defp request(effect, operation, arguments),
    do: %{
      "tag" => "request",
      "effect" => effect,
      "operation" => operation,
      "arguments" => arguments
    }

  defp resume(resumption, value),
    do: %{"tag" => "resume", "resumption" => resumption, "value" => value}

  defp handle(expression, handler_name) do
    %{
      "tag" => "handle",
      "expression" => expression,
      "handler" => handler_name,
      "arguments" => [],
      "capability" => "first"
    }
  end
end
