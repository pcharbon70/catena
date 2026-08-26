defmodule Catena.C023AbstractionTest do
  use ExUnit.Case, async: false

  alias Catena.{Interface, LanguageLifecycle, LanguageVersion}

  @email %{
    "name" => "Email",
    "parameters" => [],
    "constructors" => [
      %{
        "name" => "Validated",
        "fields" => [%{"tag" => "integer"}]
      }
    ],
    "derivations" => []
  }
  @email_error %{
    "name" => "EmailError",
    "parameters" => [],
    "constructors" => [
      %{"name" => "BadInput", "fields" => [%{"tag" => "integer"}]}
    ],
    "derivations" => []
  }
  @email_result %{
    "name" => "EmailResult",
    "parameters" => [],
    "constructors" => [
      %{
        "name" => "Ok",
        "fields" => [%{"tag" => "named", "name" => "Email", "arguments" => []}]
      },
      %{
        "name" => "Err",
        "fields" => [%{"tag" => "named", "name" => "EmailError", "arguments" => []}]
      }
    ],
    "derivations" => []
  }

  @tag obligations: ~w(AB-OBL-001 AB-OBL-007)
  test "0.1.19 is an exact registered revision with no new frontend surface" do
    assert LanguageVersion.latest() == "0.1.29"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29)

    refute "0.1.19" in LanguageVersion.compilable_revisions()
    refute "0.1.19" in LanguageVersion.interface_versions()
    refute "0.1.19" in LanguageVersion.artifact_versions()
    refute "0.1.19" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("abstraction-boundaries", "0.1.19")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-19-abstraction-boundaries")
      )

    assert change["affects"] == ~w(static-meaning diagnostics)

    assert String.contains?(
             change["specification"],
             "abstraction-boundaries/authority-and-representation-exclusions.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "namespaces-and-shadowing", required: "0.1.22"}
            }} =
             Catena.build_namespace_environment([], language_selection: selection("0.1.17"))

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, %{selection: %{language_revision: "0.1.15"}}} = Catena.tokenize_source("1")

    assert {:ok, %{selection: %{language_revision: "0.1.16"}}} =
             Catena.resolve_file_unit("", "A.cat", [])

    assert {:ok, _} = Catena.build_namespace_environment([])

    refute function_exported?(Catena, :opt_stable_layout, 2)
    refute function_exported?(Catena, :export_with_layout, 3)
    refute function_exported?(Catena, :construction_only_export, 2)
  end

  @tag obligations: ~w(AB-OBL-002)
  test "the transparent/abstract pair is the complete authority vocabulary" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{event: :declare, category: :types, spelling: "Email"},
        %{event: :export, category: :types, spelling: "Email", transparency: :abstract}
      ])

    assert Enum.any?(env.exports, &match?({:types, "Email", :abstract}, &1))

    for bad <- [:layout_stable, :stable, :construction_only, :matching_only, :opaque, "abstract"] do
      assert {:error, %{id: "EXP001", details: %{reason: "invalid_transparency_mode"}}} =
               Catena.build_namespace_environment([
                 %{event: :declare, category: :types, spelling: "Email"},
                 %{event: :export, category: :types, spelling: "Email", transparency: bad}
               ]),
             "expected transparency closure for #{inspect(bad)}"
    end

    program = idiom_program("Closure")

    for bad <- ["layout-stable", "stable", "construction-only", "matching-only"] do
      broken =
        update_in(program["type_exports"], fn exports ->
          Enum.map(
            exports,
            &%{&1 | "visibility" => (&1["visibility"] == "abstract" && bad) || &1["visibility"]}
          )
        end)

      assert {:error, _diagnostic} = Catena.check_json(JSON.encode!(broken))
    end

    assert {:ok, _} = Catena.check_json(JSON.encode!(program))
  end

  @tag obligations: ~w(AB-OBL-003)
  test "no stable-layout form is admitted and both layouts stay mandatory" do
    for extra <- [layout: :uniform, layout: :compact, stable_layout: true, layout: "compact"] do
      assert {:error, %{id: "NSP001", details: %{reason: "invalid_event"}}} =
               Catena.build_namespace_environment([
                 %{event: :declare, category: :types, spelling: "Email"},
                 Map.merge(
                   %{event: :export, category: :types, spelling: "Email"},
                   Map.new([extra])
                 )
               ])
    end

    for layout <- [:uniform, :compact] do
      assert {:ok, module, binary, metadata} =
               Catena.compile_json(JSON.encode!(idiom_program("BothLayouts")), layout: layout)

      assert metadata.layout == layout
      assert {:module, :BothLayouts} = :code.load_binary(:BothLayouts, ~c"c023.beam", binary)
      assert apply(module, :main, []) == 0
      assert apply(module, :main_ok, []) == 42
      unload(module)
    end

    {:ok, _module, _binary, metadata} =
      Catena.compile_json(JSON.encode!(idiom_program("InterfaceShape")))

    {:ok, interface} = Interface.decode(metadata.interface_binary)

    assert interface.types
           |> Enum.find(&(&1.name == "Email"))
           |> Map.get(:visibility) == :abstract

    refute interface.types |> Enum.any?(&Map.has_key?(&1, :layout))
  end

  @tag obligations: ~w(AB-OBL-004)
  test "the smart-constructor idiom enforces invariants by typing" do
    for layout <- [:uniform, :compact] do
      assert {:ok, module, binary, _metadata} =
               Catena.compile_json(JSON.encode!(idiom_program("Idiom")), layout: layout)

      assert {:module, :Idiom} = :code.load_binary(:Idiom, ~c"c023.beam", binary)
      assert apply(module, :main, []) == 0
      assert apply(module, :main_ok, []) == 42
      assert apply(module, :bad_input, []) == true
      unload(module)
    end

    wrapper = wrapper_program("Wrapper")

    assert {:ok, _core} = Catena.check_json(JSON.encode!(wrapper))

    bypass =
      module_02(
        "Bypass",
        [],
        [],
        ["main"],
        [
          definition(
            "main",
            [],
            forall(integer_type()),
            match_expr(
              construct("Wrapper.Email.Email", [integer(-1)]),
              [clause(wildcard_pattern(), integer(-1))]
            )
          )
        ]
      )

    {:ok, _wrapper_module, _binary, wrapper_metadata} =
      Catena.compile_json(JSON.encode!(wrapper))

    {:ok, wrapper_interface} = Interface.decode(wrapper_metadata.interface_binary)

    assert {:ok, :Bypass, _binary, _metadata} =
             Catena.compile_json(JSON.encode!(bypass), interfaces: [wrapper_interface])

    unload(:Bypass)
  end

  @tag obligations: ~w(AB-OBL-005)
  test "abstract scrutinees cover with wildcard plus observers" do
    program = idiom_program("Coverage")
    assert {:ok, _core} = Catena.check_json(JSON.encode!(program))

    constructor_inside_definer =
      update_in(program["definitions"], fn definitions ->
        Enum.map(definitions, fn
          %{"name" => "classify"} = defn ->
            %{
              defn
              | "body" =>
                  match_expr(variable("e"), [
                    clause(constructor_pattern("Email.Validated", [bind("n")]), variable("n"))
                  ])
            }

          defn ->
            defn
        end)
      end)

    assert {:ok, _core} =
             Catena.check_json(JSON.encode!(constructor_inside_definer))
  end

  @tag obligations: ~w(AB-OBL-006)
  test "abstract constructors stay unconstructible and unmatchable through interfaces" do
    {:ok, _module, _binary, metadata} = Catena.compile_json(JSON.encode!(idiom_program("Vault")))
    {:ok, interface} = Interface.decode(metadata.interface_binary)

    construct_attempt =
      module_02(
        "ConstructAttempt",
        [],
        [],
        ["main"],
        [
          definition(
            "main",
            [],
            forall(named_type("Vault.Email", [])),
            construct("Vault.Email.Validated", [integer(1)])
          )
        ]
      )

    assert {:error, %{id: "A004"}} =
             Catena.check_json(JSON.encode!(construct_attempt), interfaces: [interface])

    match_attempt =
      module_02(
        "MatchAttempt",
        [],
        [],
        ["main"],
        [
          definition(
            "main",
            [],
            forall(integer_type()),
            match_expr(
              construct("Vault.EmailResult.Ok", [construct("Vault.Email.Validated", [integer(1)])]),
              [clause(wildcard_pattern(), integer(0))]
            )
          )
        ]
      )

    assert {:error, %{id: "A004"}} =
             Catena.check_json(JSON.encode!(match_attempt), interfaces: [interface])

    wildcard_client =
      module_02(
        "WildcardClient",
        [],
        [],
        ["main"],
        [
          definition(
            "main",
            [],
            forall(integer_type()),
            match_expr(
              construct("Vault.EmailResult.Err", [
                construct("Vault.EmailError.BadInput", [integer(0)])
              ]),
              [clause(wildcard_pattern(), integer(7))]
            )
          )
        ]
      )

    assert {:ok, :WildcardClient, _binary, _metadata} =
             Catena.compile_json(JSON.encode!(wildcard_client), interfaces: [interface])

    unload(:WildcardClient)
  end

  defp idiom_program(module) do
    parse =
      definition(
        "parse",
        ["x"],
        forall(function_type(integer_type(), named_type("EmailResult", []))),
        match_expr(variable("x"), [
          clause(
            integer_pattern(0),
            construct("EmailResult.Err", [construct("EmailError.BadInput", [integer(0)])])
          ),
          clause(
            wildcard_pattern(),
            construct("EmailResult.Ok", [construct("Email.Validated", [variable("x")])])
          )
        ])
      )

    domain =
      definition(
        "domain",
        ["e"],
        forall(function_type(named_type("Email", []), integer_type())),
        match_expr(variable("e"), [
          clause(constructor_pattern("Email.Validated", [bind("n")]), variable("n"))
        ])
      )

    classify =
      definition(
        "classify",
        ["e"],
        forall(function_type(named_type("Email", []), integer_type())),
        match_expr(variable("e"), [
          clause(wildcard_pattern(), call_of("domain", [variable("e")]))
        ])
      )

    bad_input =
      definition(
        "bad_input",
        [],
        forall(boolean_type()),
        match_expr(call_of("parse", [integer(0)]), [
          clause(constructor_pattern("EmailResult.Err", [bind("b")]), boolean(true)),
          clause(constructor_pattern("EmailResult.Ok", [bind("b")]), boolean(false))
        ])
      )

    main =
      definition(
        "main",
        [],
        forall(integer_type()),
        match_expr(call_of("parse", [integer(0)]), [
          clause(
            constructor_pattern("EmailResult.Ok", [bind("v")]),
            call_of("domain", [variable("v")])
          ),
          clause(constructor_pattern("EmailResult.Err", [bind("r")]), integer(0))
        ])
      )

    main_ok =
      definition(
        "main_ok",
        [],
        forall(integer_type()),
        match_expr(call_of("parse", [integer(42)]), [
          clause(
            constructor_pattern("EmailResult.Ok", [bind("v")]),
            call_of("domain", [variable("v")])
          ),
          clause(constructor_pattern("EmailResult.Err", [bind("r")]), integer(0))
        ])
      )

    module_02(
      module,
      [type_group([@email, @email_error, @email_result])],
      [
        %{"name" => "Email", "visibility" => "abstract"},
        %{"name" => "EmailError", "visibility" => "transparent"},
        %{"name" => "EmailResult", "visibility" => "transparent"}
      ],
      ["parse", "domain", "classify", "bad_input", "main", "main_ok"],
      [parse, domain, classify, bad_input, main, main_ok]
    )
  end

  defp wrapper_program(module) do
    wrap = %{
      "name" => "Email",
      "parameters" => [],
      "constructors" => [
        %{"name" => "Email", "fields" => [%{"tag" => "integer"}]}
      ],
      "derivations" => []
    }

    module_02(
      module,
      [type_group([wrap])],
      [%{"name" => "Email", "visibility" => "transparent"}],
      [],
      []
    )
  end

  defp module_02(name, groups, type_exports, exports, definitions) do
    %{
      "version" => "0.1.2",
      "origin" => "test://c023",
      "module" => name,
      "type_groups" => groups,
      "type_exports" => type_exports,
      "imports" => [],
      "exports" => exports,
      "definitions" => definitions
    }
  end

  defp type_group(declarations), do: %{"declarations" => declarations}

  defp definition(name, parameters, signature, body),
    do: %{"name" => name, "parameters" => parameters, "signature" => signature, "body" => body}

  defp lambda(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}
  defp bind(name), do: %{"tag" => "bind", "name" => name}

  defp wildcard_pattern, do: %{"tag" => "wildcard"}
  defp integer_pattern(value), do: %{"tag" => "integer", "value" => value}

  defp constructor_pattern(name, arguments),
    do: %{"tag" => "constructor", "constructor" => name, "arguments" => arguments}

  defp call_of(callee, arguments),
    do: %{"tag" => "call", "callee" => variable(callee), "arguments" => arguments}

  defp construct(name, arguments),
    do: %{"tag" => "construct", "constructor" => name, "arguments" => arguments}

  defp match_expr(scrutinee, clauses),
    do: %{"tag" => "match", "scrutinee" => scrutinee, "clauses" => clauses}

  defp clause(pattern, body), do: %{"pattern" => pattern, "body" => body}

  defp forall(type), do: %{"forall" => [], "type" => type}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result, "effect" => []}

  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}

  defp named_type(name, arguments \\ []),
    do: %{"tag" => "named", "name" => name, "arguments" => arguments}

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
