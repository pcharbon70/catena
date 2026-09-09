defmodule Catena.EnvironmentProgramTest do
  use ExUnit.Case, async: false
  alias Catena.Runtime.Environment.{Kernel, Program, Schema, Policy, Manifest}
  alias Catena.Runtime.Environment, as: E
  @limits %{nodes: 10000, bytes: 100_000, depth: 100}
  defp input(service, operation, argument, module) do
    {:ok, description} = Schema.operation(service, operation)

    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module #{module} (edition 0.1) (revision 0.1.8) (origin "test://environment/#{module}")
        (export value main)
        (effect Service (operation perform (params Int) Int))
        (def main (signature Int (uses Service)) (request Service perform 0)))
      """)

    [effect] = parsed.effects
    [op] = effect.operations
    [definition] = parsed.definitions
    argument_type = elem(description.input.schema, 1)
    result_type = Schema.core_type(elem(description.result.schema, 1))
    op = %{op | parameters: [Schema.core_type(argument_type)], result: result_type}
    effect = %{effect | operations: [op]}

    expression = %{
      definition.expression
      | arguments: [literal(argument_type, argument, definition.expression.span)]
    }

    definition = %{definition | signature: result_type, expression: expression}
    parsed = %{parsed | effects: [effect], definitions: [definition]}
    {:ok, core} = Kernel.check(parsed, %{"Service" => Schema.family(service)})
    [slot] = Map.keys(core.capabilities)
    %{core: core, entry: "main", bindings: %{slot => %{"perform" => description}}}
  end

  defp literal(:unit, :unit, span), do: %{tag: :unit, span: span}

  defp literal({:tuple, types}, value, span),
    do: %{
      tag: :tuple,
      elements:
        Enum.zip(types, Tuple.to_list(value)) |> Enum.map(fn {t, v} -> literal(t, v, span) end),
      span: span
    }

  defp literal(type, value, span), do: %{tag: type, value: value, span: span}

  defp grant(service, operations, extra \\ %{}) do
    {:ok, g} =
      Policy.new(
        service,
        Map.merge(%{operations: operations, max_bytes: 4096, ttl_ms: 5000}, extra)
      )

    g
  end

  defp unload(module) do
    :code.delete(module)
    :code.purge(module)
  end

  test "compiled and reference entries install explicit service handlers and agree on traces" do
    input = input(:random, :bytes, 3, "EnvironmentRandomEntry")
    {:ok, artifact} = Program.build(input.core, input.entry, input.bindings, @limits)

    assert artifact.description.entry == %{
             arity: 1,
             parameter: {:environment_bundle, [:random]},
             effects: []
           }

    g = grant(:random, [:bytes])
    script = [{:random, :bytes, 3, Schema.success(<<1, 2, 3>>), 0}]

    try do
      reference =
        E.run(
          [g],
          @limits,
          fn bundle ->
            result =
              Program.reference(
                artifact,
                input.core,
                input.entry,
                input.bindings,
                @limits,
                bundle
              )

            {result, E.events(bundle)}
          end,
          fake: script
        )

      compiled =
        E.run(
          [g],
          @limits,
          fn bundle ->
            result =
              Program.invoke(artifact, input.core, input.entry, input.bindings, @limits, bundle)

            {result, E.events(bundle)}
          end,
          fake: script
        )

      assert reference == compiled
      assert elem(compiled, 0) == {:ok, Schema.success(<<1, 2, 3>>)}

      assert {:ok, %{status: :completed, value: Schema.success(<<1, 2, 3>>)}} ==
               Catena.Entry.launch_environment(artifact, input, [g], @limits, fake: script)

      assert {:error, _} = Catena.Entry.launch_environment(artifact, input, [], @limits)
      tampered = put_in(artifact.description.entry.effects, [:random])

      assert {:error, :unverified_environment_artifact} =
               Program.verify(tampered, input.core, input.entry, input.bindings, @limits)

      manifest = %{
        "format" => "catena-environment-entry",
        "version" => "0.1.68",
        "entry" => "main",
        "services" => ["random"]
      }

      assert {:ok, ^manifest} = Catena.Package.Manifest.decode_environment(JSON.encode!(manifest))
      assert :ok = Manifest.validate(manifest, artifact.description)
      assert {:error, _} = Manifest.validate(%{manifest | "services" => []}, artifact.description)
      assert {:error, _} = Catena.Package.Manifest.decode(JSON.encode!(manifest))
    after
      unload(artifact.module)
    end
  end

  test "real compiled entry writes only through its explicit supplied I/O authority" do
    {:ok, device} = StringIO.open("")
    input = input(:io, :write, "from-language", "EnvironmentIOEntry")
    {:ok, artifact} = Program.build(input.core, input.entry, input.bindings, @limits)

    try do
      assert {:ok, %{status: :completed, value: Schema.success(:unit)}} ==
               Catena.Entry.launch_environment(
                 artifact,
                 input,
                 [grant(:io, [:write], %{device: device})],
                 @limits
               )

      assert {"", "from-language"} = StringIO.contents(device)
    after
      unload(artifact.module)
      StringIO.close(device)
    end
  end

  test "empty authority sets launch a pure entry and unbound requests are refused" do
    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module EmptyEnvironmentEntry (edition 0.1) (revision 0.1.8) (origin "test://empty-environment")
        (export value main) (def main (signature Int (uses)) 42))
      """)

    {:ok, core} = Kernel.check(parsed, %{})
    input = %{core: core, entry: "main", bindings: %{}}
    {:ok, artifact} = Program.build(core, "main", %{}, @limits)

    try do
      assert Catena.Entry.launch_environment(artifact, input, [], @limits) ==
               {:ok, %{status: :completed, value: 42}}
    after
      unload(artifact.module)
    end

    other = input(:random, :bytes, 3, "UnboundEnvironment")
    assert {:error, :invalid_environment_entry} = Program.build(other.core, "main", %{}, @limits)
    [slot] = Map.keys(other.bindings)
    {:ok, wrong} = Schema.operation(:time, :monotonic)

    assert {:error, :invalid_environment_entry} =
             Program.build(other.core, "main", %{slot => %{"perform" => wrong}}, @limits)
  end

  test "closed entries retain ordinary lexical handlers" do
    {:ok, description} = Schema.operation(:time, :monotonic)

    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module LocalEnvironmentHandler (edition 0.1) (revision 0.1.8) (origin "test://local-environment")
        (export value main)
        (effect Clock (operation read (params Unit) Int))
        (handler Local (effect Clock) (input Int) (output Int)
          (return result (var result))
          (operation read (params (ignored Unit)) (resume next) (resume next 7)))
        (def main (signature Int (uses)) (handle Local (request Clock read (unit)))))
      """)

    # This local operation intentionally has its own Int contract, so the entry
    # needs no external service binding even though the family is declared.
    {:ok, core} = Kernel.check(parsed, %{"Clock" => description.family})
    {:ok, artifact} = Program.build(core, "main", %{}, @limits)

    try do
      for mode <- [:reference, :invoke] do
        result =
          E.run([], @limits, fn bundle ->
            apply(Program, mode, [artifact, core, "main", %{}, @limits, bundle])
          end)

        assert result == {:ok, 7}
      end
    after
      unload(artifact.module)
    end
  end
end
