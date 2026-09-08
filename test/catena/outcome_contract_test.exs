defmodule Catena.OutcomeContractTest do
  use ExUnit.Case, async: false
  alias Catena.Standard.Outcomes, as: O
  alias Catena.Type.Trait
  alias Catena.Categorical.TypeTerm

  setup_all do
    assert {:ok, module, binary, metadata} = O.compile()
    assert {:module, ^module} = :code.load_binary(module, ~c"outcomes.beam", binary)

    on_exit(fn ->
      :code.purge(module)
      :code.delete(module)
    end)

    %{beam_module: module, binary: binary, metadata: metadata}
  end

  test "explicit digest-bound package compiles deterministically with ordinary nominal ADTs", c do
    assert {:ok, _, binary, metadata} = O.compile()
    assert binary == c.binary
    assert metadata.layout == :uniform
    assert length(metadata.core.data.types) == 5
    assert metadata.core.frontend_version == "0.1.4"
    assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)
    assert interface.standard_digest == O.package!()["hierarchy_digest"]
    assert :ok == Catena.TypedCore.Verifier.verify(metadata.core)
  end

  test "package corruption and incompatible hierarchy are rejected before compilation" do
    package = O.package!()

    assert_raise ArgumentError, fn ->
      O.validate_package!(Map.put(package, "digest", "invalid"))
    end

    for {field, value} <- [
          {"layout", "compact"},
          {"hierarchy_digest", "wrong"},
          {"contract", "0.1.53"}
        ] do
      altered = package |> Map.delete("digest") |> Map.put(field, value)
      altered = Map.put(altered, "digest", Catena.Categorical.Standard.digest(altered))
      assert_raise ArgumentError, fn -> O.validate_package!(altered) end
    end
  end

  test "coherent Workflow evidence is available for dependent outcomes only", c do
    origin = O.package!()["ast"]["origin"]

    for {role, kind} <- [
          {"Optional", "Type -> Type"},
          {"Dependent", "Type -> Type -> Type"},
          {"Independent", "Type -> Type -> Type"}
        ] do
      head = %{
        "tag" => "constructor",
        "id" => origin <> "::CatenaOutcomeRoles::" <> role,
        "kind" => kind,
        "owner" => origin
      }

      head =
        if role == "Optional",
          do: head,
          else: %{
            "tag" => "application",
            "callee" => head,
            "argument" => %{
              "tag" => "constructor",
              "id" => "Int",
              "kind" => "Type",
              "owner" => "catena://builtins"
            }
          }

      term = TypeTerm.decode!(head)

      assert {:ok, _} =
               Trait.resolve(c.metadata.core.categorical.registry, "ValueEmbedder", [term])

      if role == "Independent" do
        assert {:error, _} =
                 Trait.resolve(c.metadata.core.categorical.registry, "Workflow", [term])
      else
        assert {:ok, _} = Trait.resolve(c.metadata.core.categorical.registry, "Workflow", [term])
      end
    end
  end

  test "ordinary compiled APIs and independent evaluator agree for polymorphic mappings", c do
    for {name, value} <- [
          {"optional_map", O.absent()},
          {"optional_map", O.present(3)},
          {"dependent_map", O.failure(8)},
          {"dependent_map", O.success(3)},
          {"independent_map", O.invalid(O.errors([7, 8]))},
          {"independent_map", O.valid(3)}
        ] do
      native = apply(c.beam_module, String.to_atom(name), [&(&1 + 2), value])
      library = apply(O, String.to_atom(name), [&(&1 + 2), value])
      assert native == library

      assert {:ok, reference} =
               Catena.Reference.Evaluator.run(c.metadata.core, name, [
                 {:closure, &(&1 + 2)},
                 reference(value, c.metadata.core.data)
               ])

      assert reference == reference(native, c.metadata.core.data)
    end
  end

  test "failure skips dependent callbacks; independent combination accumulates in order" do
    observer = self()

    callback = fn value ->
      send(observer, {:called, value})
      O.success(value + 1)
    end

    assert O.dependent_chain(callback, O.failure(9)) == O.failure(9)
    refute_received {:called, _}
    assert O.dependent_chain(callback, O.success(2)) == O.success(3)
    assert_received {:called, 2}
    bad = fn _ -> flunk("combination callback must be skipped on failure") end
    first = O.invalid(O.errors([1, 2]))
    second = O.invalid(O.errors([3]))
    assert O.independent_map2(bad, first, second) == O.invalid(O.errors([1, 2, 3]))
    assert O.dependent_map2(bad, O.failure(1), O.failure(2)) == O.failure(1)
    refute_received {:called, _}
  end

  test "bounded identity composition and associativity laws retain nested values" do
    for value <- [O.absent(), O.present(0), O.present(O.absent()), O.present(O.present(3))] do
      assert O.optional_map(& &1, value) == value
      assert O.optional_chain(&O.present/1, value) == value
      f = fn x -> {x, 1} end
      g = fn x -> {x, 2} end

      assert O.optional_map(fn x -> g.(f.(x)) end, value) ==
               O.optional_map(g, O.optional_map(f, value))
    end

    f = fn x -> if rem(x, 2) == 0, do: O.success(x + 1), else: O.failure(x) end
    g = fn x -> O.success(x * 2) end

    for value <- [O.failure(7), O.success(0), O.success(1), O.success(2)] do
      assert O.dependent_chain(g, O.dependent_chain(f, value)) ==
               O.dependent_chain(fn x -> O.dependent_chain(g, f.(x)) end, value)
    end

    for xs <- [[1], [1, 2]], ys <- [[3], [3, 4]], zs <- [[5], [5, 6]] do
      a = O.errors(xs)
      b = O.errors(ys)
      c = O.errors(zs)

      assert O.errors_append(O.errors_append(a, b), c) ==
               O.errors_append(a, O.errors_append(b, c))

      assert O.errors_list(O.errors_append(a, b)) == xs ++ ys
    end
  end

  test "empty error input is explicit absence and cannot fabricate invalid validation", c do
    assert_raise ArgumentError, fn -> O.errors([]) end
    assert apply(c.beam_module, :errors_from_sequence, [O.sequence([])]) == O.absent()

    assert apply(c.beam_module, :errors_from_sequence, [O.sequence([4])]) ==
             O.present(O.errors([4]))

    assert apply(c.beam_module, :dependent_to_independent, [O.failure(4)]) ==
             O.invalid(O.errors([4]))

    assert apply(c.beam_module, :independent_to_dependent, [O.invalid(O.errors([4, 5]))]) ==
             O.failure(O.errors([4, 5]))
  end

  test "no trap or exit is caught by outcome combinators" do
    assert catch_throw(O.optional_map(fn _ -> throw({:catena_trap, 7}) end, O.present(1))) ==
             {:catena_trap, 7}

    assert catch_exit(O.dependent_chain(fn _ -> exit(:dead) end, O.success(1))) == :dead
    assert_raise ArithmeticError, fn -> O.independent_map(fn _ -> div(1, 0) end, O.valid(1)) end
  end

  test "large error accumulation is stack safe and preserves every error" do
    xs = Enum.to_list(1..50_000)
    assert O.errors_list(O.errors_append(O.errors(xs), O.errors([50_001]))) == xs ++ [50_001]
  end

  test "wrong payload and empty invalid constructor are rejected by ordinary typing" do
    ast = O.package!()["ast"]

    bad = %{
      "name" => "bad",
      "parameters" => [],
      "signature" => %{
        "forall" => [],
        "type" => %{
          "tag" => "named",
          "name" => "Independent",
          "arguments" => [%{"tag" => "integer"}, %{"tag" => "integer"}]
        }
      },
      "body" => %{
        "tag" => "construct",
        "constructor" => "Independent.Invalid",
        "arguments" => [
          %{"tag" => "construct", "constructor" => "Sequence.End", "arguments" => []}
        ]
      }
    }

    ast = %{
      ast
      | "definitions" => ast["definitions"] ++ [bad],
        "exports" => ast["exports"] ++ ["bad"]
    }

    assert {:error, _} = Catena.check_json(JSON.encode!(ast))
  end

  test "independent validation equation agrees with specialized BEAM dictionary", c do
    assert {:ok, interface} = Catena.Interface.decode(c.metadata.interface_binary)

    manifest = %{
      companion_module: "OutcomeValidationSpecialization",
      modules: [],
      interfaces: [],
      output: "OutcomeValidationSpecialization.beam",
      roots: [
        %{
          "template" => "independent_map2_specialized",
          "export" => "validate",
          "types" => [],
          "instances" => []
        }
      ]
    }

    assert {:ok, module, binary, info} = Catena.Package.Linker.link(manifest, [interface])
    assert info.evidence_erased
    assert {:ok, ^module, ^binary, _} = Catena.Package.Linker.link(manifest, [interface])
    assert {:module, ^module} = :code.load_binary(module, ~c"outcome-specialized.beam", binary)

    on_exit(fn ->
      :code.purge(module)
      :code.delete(module)
    end)

    callback = fn x -> fn y -> x + y end end
    ref_callback = {:closure, fn x -> {:closure, fn y -> x + y end} end}
    data = c.metadata.core.data

    for first <- [O.valid(2), O.invalid(O.errors([1, 2]))],
        second <- [O.valid(3), O.invalid(O.errors([3, 4]))] do
      compiled = apply(module, :validate, [callback, first, second])

      assert compiled ==
               apply(c.beam_module, :independent_map2_with_errors, [
                 fn x -> fn y -> O.errors_append(x, y) end end,
                 callback,
                 first,
                 second
               ])

      combine =
        {:closure,
         fn x ->
           {:closure, fn y -> reference(O.errors(ref_errors(x) ++ ref_errors(y)), data) end}
         end}

      assert {:ok, result} =
               Catena.Reference.Evaluator.run(c.metadata.core, "independent_map2_with_errors", [
                 combine,
                 ref_callback,
                 reference(first, data),
                 reference(second, data)
               ])

      assert result == reference(compiled, data)
    end
  end

  test "compiled explicit conversions skip unnecessary callbacks and preserve nested absence",
       c do
    observer = self()

    error = fn {} ->
      send(observer, :absence_adapter)
      9
    end

    assert apply(c.beam_module, :optional_to_dependent, [error, O.present(O.absent())]) ==
             O.success(O.absent())

    refute_received :absence_adapter
    assert apply(c.beam_module, :optional_to_dependent, [error, O.absent()]) == O.failure(9)
    assert_received :absence_adapter
    refute_received :absence_adapter
    assert apply(c.beam_module, :dependent_to_optional, [O.failure(9)]) == O.absent()
  end

  defp ref_errors({:catena_value, _id, [head, tail]}), do: [head | ref_sequence(tail)]
  defp ref_sequence({:catena_value, _id, []}), do: []
  defp ref_sequence({:catena_value, _id, [head, tail]}), do: [head | ref_sequence(tail)]

  defp reference({:catena_adt, id, index, fields}, data) do
    type = Map.fetch!(data.types_by_id, Atom.to_string(id))
    constructor = Enum.at(type.constructors, index)
    {:catena_value, constructor.id, Enum.map(Tuple.to_list(fields), &reference(&1, data))}
  end

  defp reference(value, _), do: value
end
