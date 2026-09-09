defmodule Catena.MinimumPreludeTest do
  use ExUnit.Case, async: false
  alias Catena.Standard.Minimum, as: M
  alias Catena.Standard.Outcomes, as: O
  @limits %{nodes: 10000, bytes: 100_000, depth: 1000}

  defp context(services \\ false), do: elem(M.resolve(M.selection(services)), 1)

  defp unload(module) do
    :code.delete(module)
    :code.purge(module)
  end

  defp load(application) do
    for value <- application.libraries ++ [application] do
      if :code.is_loaded(value.module) == false do
        assert {:module, _} = :code.load_binary(value.module, ~c"minimum-test", value.binary)
      else
        assert {:ok, {_, digest}} = :beam_lib.md5(value.binary)
        assert value.module.module_info(:md5) == digest
      end

      on_exit({:minimum_unload, value.module}, fn -> unload(value.module) end)
    end
  end

  test "exact catalog pins ordinary component content and retained hierarchy" do
    assert :ok = M.verify_catalog(M.catalog!())
    assert M.catalog!() == Catena.Categorical.Standard.minimum_package()
    assert M.catalog!()["hierarchy_digest"] == Catena.Categorical.Standard.interface!()["digest"]
    assert M.catalog!()["implicit_imports"] == []

    assert {:error, :invalid_minimum_catalog} =
             M.verify_catalog(Map.put(M.catalog!(), "digest", "wrong"))

    for field <- ["package_version", "hierarchy_digest"] do
      changed = Map.put(M.catalog!(), field, "forged")

      changed =
        Map.put(
          changed,
          "digest",
          Catena.Categorical.Standard.digest(Map.delete(changed, "digest"))
        )

      assert {:error, :invalid_minimum_catalog} = M.verify_catalog(changed)
    end
  end

  test "exact selection refuses missing, duplicate, changed and oversized manifests" do
    assert {:ok, selected} = Catena.Package.Manifest.decode_minimum(JSON.encode!(M.selection()))
    assert {:ok, _} = M.resolve(selected)
    assert {:error, :missing_minimum_package} = M.resolve(selected, [])

    assert {:error, :conflicting_minimum_package} =
             M.resolve(selected, [M.catalog!(), M.catalog!()])

    for bad <- [
          Map.put(selected, "digest", "wrong"),
          Map.put(selected, "extra", true),
          Map.put(selected, "services", "true"),
          Map.put(selected, "package_version", "0.2.0")
        ] do
      assert {:error, :invalid_minimum_selection} = M.decode(JSON.encode!(bad))
    end

    assert {:error, :invalid_minimum_selection} = M.decode(:binary.copy(" ", 16385))
    assert {:error, _} = Catena.Package.Manifest.decode(JSON.encode!(selected))
    assert {:error, :invalid_minimum_context} = M.verify(Map.put(context(), :extra, :forged))
  end

  test "opt-out supplies no libraries or implicit standard names and permits empty programs" do
    {:ok, disabled} = M.resolve(nil)
    assert {:ok, []} = Catena.Interface.minimum_libraries(disabled)

    assert {:error, _} =
             M.compile_application(disabled, File.read!("test/fixtures/minimum-transform.json"))

    empty = %{
      "version" => "0.1.4",
      "module" => "MinimumEmpty",
      "origin" => "test://minimum-empty",
      "definitions" => [],
      "exports" => [],
      "type_groups" => [],
      "type_exports" => [],
      "imports" => [],
      "traits" => [],
      "instances" => [],
      "templates" => []
    }

    assert {:ok, %{libraries: []}} = M.compile_application(disabled, JSON.encode!(empty))

    bad =
      put_in(empty["definitions"], [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "variable", "name" => "identity"}
        }
      ])
      |> Map.put("exports", ["main"])

    assert {:error, _} = M.compile_application(context(), JSON.encode!(bad))
  end

  test "minimum selection resolves and replays through the existing prelude dependency engine" do
    assert {:ok, root, environment} = M.dependency(context())
    assert root.prelude == %{"package" => "catena-minimum", "requirement" => "0.1.0"}
    assert {:ok, [resolved]} = Catena.Package.Deps.resolve(root, environment)
    assert length(resolved.interface_digests) == 5
    assert Enum.all?(resolved.interface_digests, &(is_binary(&1) and byte_size(&1) == 64))
    assert {:ok, lock} = M.lock(context())
    assert {:ok, _} = M.replay(context(), lock)
    assert {:error, :minimum_lock_mismatch} = M.replay(context(), lock <> " ")
    {:ok, disabled} = M.resolve(nil)
    assert {:ok, empty} = M.lock(disabled)
    assert {:ok, []} = M.replay(disabled, empty)
    assert {:error, :minimum_lock_mismatch} = M.replay(disabled, lock)
  end

  test "ordinary foundations preserve identity composition products and callback order" do
    assert {:ok, libraries} = M.compile_libraries(context())
    foundation = Enum.find(libraries, &(&1.role == "foundation"))

    assert {:module, module} =
             :code.load_binary(foundation.module, ~c"foundation", foundation.binary)

    on_exit(fn -> unload(module) end)

    for value <- -30..30 do
      assert apply(module, :identity, [value]) == value
      assert apply(module, :compose, [&(&1 + 2), &(&1 * 3), value]) == (value + 2) * 3
      assert apply(module, :compose, [& &1, &(&1 + 2), value]) == value + 2
      assert apply(module, :compose, [&(&1 + 2), & &1, value]) == value + 2
      f = &(&1 + 2)
      g = &(&1 * 3)
      h = &(&1 - 7)

      assert apply(module, :compose, [fn x -> apply(module, :compose, [f, g, x]) end, h, value]) ==
               apply(module, :compose, [f, fn x -> apply(module, :compose, [g, h, x]) end, value])

      pair = apply(module, :pair, [value, true])
      assert apply(module, :first, [pair]) == value
      assert apply(module, :second, [pair]) == true
      assert apply(module, :constant, [value, :ignored]) == value

      assert {:ok, ^value} =
               Catena.Reference.Evaluator.run(foundation.metadata.core, "identity", [value])
    end

    parent = self()

    assert apply(module, :compose, [
             fn x ->
               send(parent, :first)
               x + 1
             end,
             fn x ->
               send(parent, :next)
               x * 2
             end,
             4
           ]) == 10

    assert_receive :first
    assert_receive :next
    refute_receive :first, 0
    refute_receive :next, 0
  end

  test "selected ordinary types support compiled data transformation and validation applications" do
    for {file, cases} <- [
          {"minimum-transform", [{O.absent(), 0}, {O.present(7), 14}]},
          {"minimum-validation", [{O.absent(), O.failure(404)}, {O.present(7), O.success(7)}]}
        ] do
      assert {:ok, app} =
               M.compile_application(context(), File.read!("test/fixtures/" <> file <> ".json"))

      load(app)
      assert app.catalog_digest == M.catalog!()["digest"]

      for {input, expected} <- cases do
        assert apply(app.module, :main, [input]) == expected
      end
    end
  end

  test "explicit constructor aliases retain conflicting import refusal" do
    source = File.read!("test/fixtures/minimum-transform.json") |> JSON.decode!()

    imports =
      for constructor <- [
            "CatenaOutcomeRoles.Optional.Present",
            "CatenaNumericRoles.Decimal.Scaled"
          ],
          do: %{"kind" => "constructor", "constructor" => constructor, "as" => "Same"}

    assert {:error, _} =
             M.compile_application(context(), JSON.encode!(Map.put(source, "imports", imports)))
  end

  test "selected numeric programs execute actual retained computation before checked operations" do
    {:ok, core} =
      Catena.check_kernel("""
      (module MinimumNumeric (edition 0.1) (revision 0.1.8) (origin "test://minimum-numeric")
        (export value work)
        (def work (signature (Fn Int (effects) Int) (uses)) (fn (value Int) (add (var value) 2))))
      """)

    steps = [{:int_to_float, :exact}]
    assert {:ok, artifact} = M.prepare(context(), :numeric, core, "work", steps, @limits)
    on_exit(fn -> unload(artifact.module) end)

    assert {:ok, O.success(9.0)} ==
             M.invoke(context(), :numeric, artifact, core, "work", steps, 7, @limits)

    assert Catena.Standard.Numeric.Program.reference(core, "work", steps, 7, @limits) ==
             {:ok, O.success(9.0)}

    {:ok, disabled} = M.resolve(nil)

    assert {:error, :minimum_not_selected} =
             M.prepare(disabled, :numeric, core, "work", steps, @limits)

    assert {:error, :minimum_not_selected} =
             M.invoke(disabled, :numeric, artifact, core, "work", steps, 7, @limits)
  end

  test "selecting environmental contracts grants no authority to a real compiled application" do
    alias Catena.Runtime.Environment.{Kernel, Schema, Policy}
    alias Catena.Runtime.Environment, as: E
    {:ok, op} = Schema.operation(:io, :write)

    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module MinimumEnvironment (edition 0.1) (revision 0.1.8) (origin "test://minimum-environment")
        (export value main) (effect IO (operation write (params Int) Int))
        (def main (signature Int (uses IO)) (request IO write 0)))
      """)

    [effect] = parsed.effects
    [operation] = effect.operations
    [definition] = parsed.definitions
    result = Schema.core_type(elem(op.result.schema, 1))
    operation = %{operation | parameters: [:bytes], result: result}

    body = %{
      definition.expression
      | arguments: [%{tag: :bytes, value: "selected", span: definition.expression.span}]
    }

    parsed = %{
      parsed
      | effects: [%{effect | operations: [operation]}],
        definitions: [%{definition | signature: result, expression: body}]
    }

    {:ok, core} = Kernel.check(parsed, %{"IO" => Schema.family(:io)})
    [slot] = Map.keys(core.capabilities)
    bindings = %{slot => %{"write" => op}}

    assert {:error, :environment_not_selected} =
             M.prepare(context(), :environment, core, "main", bindings, @limits)

    enabled = context(true)
    {:ok, artifact} = M.prepare(enabled, :environment, core, "main", bindings, @limits)
    on_exit(fn -> unload(artifact.module) end)

    assert {:error, _} =
             E.run([], @limits, fn bundle ->
               M.invoke(enabled, :environment, artifact, core, "main", bindings, bundle, @limits)
             end)

    {:ok, device} = StringIO.open("")

    {:ok, grant} =
      Policy.new(:io, %{operations: [:write], max_bytes: 32, ttl_ms: 5000, device: device})

    assert {:ok, Schema.success(:unit)} ==
             E.run([grant], @limits, fn bundle ->
               M.invoke(enabled, :environment, artifact, core, "main", bindings, bundle, @limits)
             end)

    assert {"", "selected"} = StringIO.contents(device)
    StringIO.close(device)
  end

  test "selected text operations retain exact units and checked compiled adoption" do
    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module MinimumText (edition 0.1) (revision 0.1.8) (origin "test://minimum-text")
        (export value work)
        (def work (signature (Fn Int (effects) Int) (uses)) (fn (value Int) (var value))))
      """)

    {:ok, core} = Catena.ValueBoundary.Kernel.check(text_tree(parsed))
    steps = [{:measure, :grapheme}]
    assert {:ok, artifact} = M.prepare(context(), :text, core, "work", steps, @limits)
    on_exit(fn -> unload(artifact.module) end)

    assert {:ok, O.success(2)} ==
             M.invoke(context(), :text, artifact, core, "work", steps, "a\u0301😀", @limits)

    assert {:error, _} =
             M.invoke(context(), :text, artifact, core, "work", steps, <<255>>, @limits)
  end

  defp text_tree(:integer), do: :text
  defp text_tree(%Catena.SourceSpan{} = span), do: span
  defp text_tree(map) when is_map(map), do: Map.new(map, fn {k, v} -> {k, text_tree(v)} end)
  defp text_tree(list) when is_list(list), do: Enum.map(list, &text_tree/1)

  defp text_tree(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&text_tree/1) |> List.to_tuple()

  defp text_tree(value), do: value
end
