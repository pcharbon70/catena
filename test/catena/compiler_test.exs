defmodule Catena.CompilerTest do
  use ExUnit.Case, async: false

  alias Catena.Type

  test "checks a signed polymorphic identity" do
    assert {:ok, core} = Catena.check_json(identity_json())
    [definition] = core.definitions
    assert definition.scheme.variables == [0]

    assert Type.normalize(definition.scheme.type) ==
             {:function, {:variable, "a"}, {:variable, "a"}}
  end

  test "requires signatures on exports" do
    json = module_json("Unsigned", ["answer"], [definition("answer", [], nil, integer(42))])
    assert {:error, %{id: "T008"}} = Catena.check_json(json)
  end

  test "rejects an unbound value with T001" do
    json = module_json("Unbound", [], [definition("bad", [], nil, variable("missing"))])
    assert {:error, %{id: "T001"}} = Catena.check_json(json)
  end

  test "skolemization rejects a falsely universal signature" do
    signature = forall(["a"], function_type(variable_type("a"), variable_type("a")))

    json =
      module_json("FalseUniversal", ["constant"], [
        definition("constant", ["x"], signature, integer(1))
      ])

    assert {:error, %{id: "T002"}} = Catena.check_json(json)
  end

  test "occurs check rejects self application" do
    self_call = call(variable("x"), [variable("x")])
    json = module_json("Infinite", [], [definition("omega", ["x"], nil, self_call)])
    assert {:error, %{id: "T003"}} = Catena.check_json(json)
  end

  test "reports the executable effect-annotation boundary with T010" do
    signature =
      forall(
        [],
        function_type(integer_type(), integer_type())
        |> Map.put("effect", [%{"family" => "Console", "identity" => "console"}])
      )

    json =
      module_json("EffectBoundary", ["read"], [
        definition("read", ["value"], signature, variable("value"))
      ])

    assert {:error, %{id: "T010"}} = Catena.check_json(json)
  end

  test "compiles deterministic BEAM through OTP 29 and executes it" do
    signature = forall([], tuple_type([integer_type(), boolean_type()]))

    body =
      let_expression(
        "identity",
        function_expression("x", variable("x")),
        tuple_expression([
          call(variable("identity"), [integer(7)]),
          call(variable("identity"), [boolean(true)])
        ])
      )

    json =
      module_json(
        "C001Runtime",
        ["main"],
        [definition("main", [], signature, body)],
        "fixtures/c001-runtime.catena.json"
      )

    module = :C001Runtime
    assert {:ok, ^module, first, metadata} = Catena.compile_json(json)
    assert Enum.any?(metadata.forms, &match?({:attribute, _, :module, ^module}, &1))
    assert {:ok, ^module, second, _metadata} = Catena.compile_json(json)
    assert first == second

    assert {:module, ^module} = :code.load_binary(module, ~c"c001_runtime.beam", first)
    assert apply(module, :main, []) == {7, true}
    :code.purge(module)
    :code.delete(module)

    assert {:ok, {^module, [compile_info: compile_info]}} =
             :beam_lib.chunks(first, [:compile_info])

    assert Enum.any?(metadata.forms, fn
             {:attribute, _, :file, {~c"fixtures/c001-runtime.catena.json", 1}} -> true
             _ -> false
           end)

    assert Keyword.get(compile_info, :catena_specification) == ~c"0.1"
    assert Keyword.get(compile_info, :catena_frontend) == ~c"json-ast-0.1"
  end

  test "preserves curried top-level functions when used as values" do
    choose_signature =
      forall(
        ["a", "b"],
        function_type(
          variable_type("a"),
          function_type(variable_type("b"), variable_type("a"))
        )
      )

    main_body = call(call(variable("choose_first"), [integer(9)]), [boolean(false)])

    json =
      module_json(
        "CurriedGlobal",
        ["choose_first", "main"],
        [
          definition("choose_first", ["left", "right"], choose_signature, variable("left")),
          definition("main", [], forall([], integer_type()), main_body)
        ]
      )

    module = :CurriedGlobal
    assert {:ok, ^module, binary, _metadata} = Catena.compile_json(json)
    assert {:module, ^module} = :code.load_binary(module, ~c"curried_global.beam", binary)
    assert apply(module, :main, []) == 9
    :code.purge(module)
    :code.delete(module)
  end

  defp identity_json do
    signature = forall(["a"], function_type(variable_type("a"), variable_type("a")))

    module_json("Identity", ["identity"], [
      definition("identity", ["value"], signature, variable("value"))
    ])
  end

  defp module_json(name, exports, definitions, source \\ "fixture.catena.json") do
    JSON.encode!(%{
      "version" => "0.1",
      "module" => name,
      "source" => source,
      "exports" => exports,
      "definitions" => definitions
    })
  end

  defp definition(name, parameters, signature, body) do
    %{"name" => name, "parameters" => parameters, "signature" => signature, "body" => body}
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp boolean(value), do: %{"tag" => "boolean", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp function_expression(parameter, body),
    do: %{"tag" => "function", "parameter" => parameter, "body" => body}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp tuple_expression(elements), do: %{"tag" => "tuple", "elements" => elements}

  defp let_expression(name, value, body),
    do: %{"tag" => "let", "name" => name, "value" => value, "body" => body}

  defp forall(variables, type), do: %{"forall" => variables, "type" => type}
  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}
  defp variable_type(name), do: %{"tag" => "variable", "name" => name}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result, "effect" => []}

  defp tuple_type(elements), do: %{"tag" => "tuple", "elements" => elements}
end
