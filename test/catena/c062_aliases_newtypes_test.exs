defmodule Catena.C062AliasesNewtypesTest do
  use ExUnit.Case, async: false

  alias Catena.{Effect.Runtime, LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42)

  @newtype_kernel """
  (module C062Email
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c062/email")
    (export type Email)
    (export value unwrap)
    (export value main)
    (data Email
      (params)
      (constructor Email (fields Int)))
    (def unwrap
      (signature (Fn Email (effects) Int) (uses))
      (fn (email Email)
        (match (var email)
          (case (constructor Email (bind value)) (var value)))))
    (def main
      (signature Int (uses))
      (match (construct Email 7)
        (case (constructor Email (bind value)) (add (var value) 1)))))
  """

  describe "revision registration" do
    @tag obligations: ~w(AN-OBL-001)
    test "0.1.41 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.42"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.41" in LanguageVersion.compilable_revisions()
      refute "0.1.41" in LanguageVersion.artifact_versions()
      refute "0.1.41" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("aliases-and-newtypes", "0.1.41")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-41-aliases-and-newtypes")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "aliases-and-newtypes/the-newtype-form.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.42"}}} = Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :type_alias, 2)
      refute function_exported?(Catena, :newtype_decl, 1)
      refute function_exported?(Catena, :unwrap_coercion, 1)
    end
  end

  describe "the newtype form" do
    @tag obligations: ~w(AN-OBL-003 AN-OBL-005)
    test "construct, match, and compare through the declared wrapper on both targets" do
      assert {:ok, core} = Catena.check_kernel(@newtype_kernel)

      assert {:ok, 8, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C062Email, binary, _} = Catena.compile_kernel(@newtype_kernel)

      assert {:module, :C062Email} =
               :code.load_binary(:C062Email, ~c"c062_email.beam", binary)

      assert apply(:C062Email, :main, []) == 8

      on_exit(fn ->
        :code.purge(:C062Email)
        :code.delete(:C062Email)
      end)

      equal_program =
        json_program(
          "C062Eq",
          "main",
          bool_match(
            binary_op(
              "equal",
              construct("Email.Email", [integer(7)]),
              construct("Email.Email", [integer(7)])
            ),
            integer(1),
            integer(0)
          ),
          result_type: integer_type(),
          types: [email_group()],
          exports: ["main"]
        )

      {reference, beam} = dual_trace(equal_program, "C062Eq")

      assert reference == beam
      assert {:ok, 1, %{}} = run_reference(equal_program)
    end

    @tag obligations: ~w(AN-OBL-006)
    test "coercion stays explicit: wrapper-wrapped confusion rejects" do
      confused =
        String.replace(
          @newtype_kernel,
          "(case (constructor Email (bind value)) (add (var value) 1)))",
          "(case (constructor Email (bind value)) (add (construct Email (var value)) 1)))"
        )

      assert {:error, %{id: id}} = Catena.check_kernel(confused)
      assert id in ["T002", "T001"]

      json_confused =
        json_program(
          "C062Confuse",
          "main",
          binary_op("add", construct("Email.Email", [integer(1)]), integer(1)),
          result_type: integer_type(),
          types: [email_group()],
          exports: ["main"]
        )

      assert {:error, _} = Catena.check_json(json_confused)
    end

    @tag obligations: ~w(AN-OBL-005)
    test "nominal-spelled diagnostics: unknown constructors name themselves" do
      typo =
        json_program("C062Typo", "main", construct("Email.Typo", [integer(1)]),
          result_type: named_type("Email"),
          types: [email_group()],
          exports: ["main"]
        )

      assert {:error, %{id: "A004", message: message}} = Catena.check_json(typo)
      assert message =~ "Email.Typo"
    end
  end

  describe "opaque routing and the abstract export" do
    @tag obligations: ~w(AN-OBL-005 AN-OBL-003)
    test "an abstract newtype runs its smart-constructor idiom unchanged" do
      source = idiom_program()

      assert {:ok, core} = Catena.check_json(source)
      assert {:ok, 42, %{}} = run_reference(source)

      broken =
        put_type_exports(source, [
          %{"name" => "Email", "visibility" => "construction-only"}
        ])

      assert {:error, _diagnostic} = Catena.check_json(broken)

      {:ok, env} =
        Catena.build_namespace_environment([
          %{event: :declare, category: :types, spelling: "Email"},
          %{event: :export, category: :types, spelling: "Email", transparency: :abstract}
        ])

      assert Enum.any?(env.exports, &match?({:types, "Email", :abstract}, &1))

      for bad <- [:construction_only, :matching_only, :opaque, "alias"] do
        assert {:error, %{id: "EXP001"}} =
                 Catena.build_namespace_environment([
                   %{event: :declare, category: :types, spelling: "Email"},
                   %{event: :export, category: :types, spelling: "Email", transparency: bad}
                 ])
      end
    end
  end

  describe "deriving and exclusions" do
    @tag obligations: ~w(AN-OBL-007)
    test "instances attach explicitly and never flow through the wrapper" do
      with_instance = trait_program(true)

      assert {:ok, core} = Catena.check_kernel(with_instance)

      assert {:ok, 43, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      without_instance = trait_program(false)

      assert {:error, %{}} = Catena.check_kernel(without_instance)
    end

    @tag obligations: ~w(AN-OBL-002 AN-OBL-008)
    test "no alias form exists on any frontend" do
      aliased = """
      (module C062Alias
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c062/alias")
        (type_alias Email Int)
        (export value main)
        (def main
          (signature Int (uses))
          (construct Email 1)))
      """

      assert {:error, _} = Catena.check_kernel(aliased)

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :elaborate_alias, 1)
      refute function_exported?(Catena, :alias_environment, 1)
    end

    @tag obligations: ~w(AN-OBL-004 AN-OBL-002)
    test "no cost, layout, or automatic-deriving entry points exist" do
      refute function_exported?(Catena, :newtype_layout, 1)
      refute function_exported?(Catena, :newtype_cost, 1)
      refute function_exported?(Catena, :derive_newtype_instances, 1)
      refute function_exported?(Catena, :zero_cost, 1)
    end

    @tag obligations: ~w(AN-OBL-001 AN-OBL-002)
    test "determinism: repeated runs repeat" do
      assert {:ok, core} = Catena.check_kernel(@newtype_kernel)

      assert {:ok, first, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, second, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert first == second

      {result, _trace} =
        Runtime.capture_trace(fn -> Catena.Kernel.Stepper.run(core, "main") end)

      assert {:ok, 8, %{root_status: :terminated}} = result
    end
  end

  defp trait_program(with_instance) do
    instance =
      if with_instance do
        """
        (instance Reveal Email
          (method reveal reveal_email))
        """
      else
        ""
      end

    """
    (module C062Trait
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c062/trait")
      (export type Email)
      (export value main)
      (data Email
        (params)
        (constructor Email (fields Int)))
      (trait Reveal
        (parameter a)
        (method reveal (Fn a (effects) Int)))
      #{instance}
      (def reveal_email
        (signature (Fn Email (effects) Int) (uses))
        (fn (email Email)
          (match (var email)
            (case (constructor Email (bind value)) (add (var value) 1)))))
      (def main
        (signature Int (uses))
        (trait-call Reveal reveal (construct Email 42))))
    """
  end

  defp idiom_program do
    parse =
      definition(
        "parse",
        ["x"],
        forall(function_type(integer_type(), named_type("Email", []))),
        match_expr(variable("x"), [
          clause(integer_pattern(0), construct("Email.Email", [integer(0)])),
          clause(wildcard_pattern(), construct("Email.Email", [variable("x")]))
        ])
      )

    unwrap =
      definition(
        "unwrap",
        ["e"],
        forall(function_type(named_type("Email", []), integer_type())),
        match_expr(variable("e"), [
          clause(constructor_pattern("Email.Email", [bind("n")]), variable("n"))
        ])
      )

    main =
      definition(
        "main",
        [],
        forall(integer_type()),
        call_of("unwrap", [call_of("parse", [integer(42)])])
      )

    JSON.encode!(%{
      "version" => "0.1.2",
      "origin" => "test://c062/idiom",
      "module" => "C062Idiom",
      "type_groups" => [email_group()],
      "type_exports" => [%{"name" => "Email", "visibility" => "abstract"}],
      "imports" => [],
      "exports" => ["parse", "unwrap", "main"],
      "definitions" => [parse, unwrap, main]
    })
  end

  defp source_type_exports, do: [%{"name" => "Email", "visibility" => "abstract"}]

  defp put_type_exports(source, exports) do
    source |> JSON.decode!() |> Map.put("type_exports", exports) |> JSON.encode!()
  end

  defp email_group do
    %{
      "declarations" => [
        %{
          "name" => "Email",
          "parameters" => [],
          "constructors" => [
            %{"name" => "Email", "fields" => [%{"tag" => "integer"}], "existentials" => []}
          ],
          "derivations" => []
        }
      ]
    }
  end

  defp json_program(module, entry, body, options) do
    result_type = Keyword.fetch!(options, :result_type)

    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c062/#{module}",
      "module" => module,
      "source" => "c062.catena.json",
      "exports" => [entry],
      "type_exports" => [%{"name" => "Email", "visibility" => "transparent"}],
      "type_groups" => Keyword.fetch!(options, :types),
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => [
        %{
          "name" => entry,
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => result_type, "uses" => []},
          "body" => body
        }
      ],
      "effects" => [],
      "handlers" => []
    })
  end

  defp run_reference(source) do
    {:ok, core} = Catena.check_json(source)

    {result, _trace} =
      Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    with {:ok, value} <- result, do: {:ok, value, %{}}
  end

  defp dual_trace(source, module) do
    {:ok, core} = Catena.check_json(source)
    module_atom = String.to_atom(module)

    {{:ok, _value}, reference_trace} =
      Runtime.capture_trace(fn -> Catena.Reference.Evaluator.run(core, "main") end)

    {:ok, ^module_atom, binary, _metadata} = Catena.compile_json(source)

    assert {:module, ^module_atom} =
             :code.load_binary(module_atom, ~c"c062-#{module}.beam", binary)

    {_value, beam_trace} =
      Runtime.capture_trace(fn -> apply(module_atom, :main, []) end)

    on_exit(fn ->
      :code.purge(module_atom)
      :code.delete(module_atom)
    end)

    {reference_trace, beam_trace}
  end

  defp definition(name, parameters, signature, body),
    do: %{"name" => name, "parameters" => parameters, "signature" => signature, "body" => body}

  defp forall(type), do: %{"forall" => [], "type" => type, "uses" => []}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result, "effect" => []}

  defp match_expr(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp clause(pattern, body), do: %{"pattern" => pattern, "body" => body}

  defp integer_pattern(value), do: %{"tag" => "integer", "value" => value}
  defp wildcard_pattern, do: %{"tag" => "wildcard"}

  defp constructor_pattern(name, arguments),
    do: %{"tag" => "constructor", "constructor" => name, "arguments" => arguments}

  defp bool_match(condition, then_body, else_body) do
    match_expr(condition, [
      clause(%{"tag" => "boolean", "value" => true}, then_body),
      clause(%{"tag" => "boolean", "value" => false}, else_body)
    ])
  end

  defp construct(constructor, arguments),
    do: %{"tag" => "construct", "constructor" => constructor, "arguments" => arguments}

  defp call_of(name, arguments),
    do: %{
      "tag" => "call",
      "callee" => %{"tag" => "variable", "name" => name},
      "arguments" => arguments
    }

  defp binary_op(operator, left, right),
    do: %{"tag" => "binary", "operator" => operator, "left" => left, "right" => right}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}
  defp bind(name), do: %{"tag" => "bind", "name" => name}

  defp integer_type, do: %{"tag" => "integer"}

  defp named_type(name, arguments \\ []),
    do: %{"tag" => "named", "name" => name, "arguments" => arguments}
end
