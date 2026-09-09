defmodule Catena.CollectionProtocolTest do
  use ExUnit.Case, async: false
  alias Catena.Standard.Collections, as: C
  alias Catena.Standard.Collections.Order
  alias Catena.Standard.Outcomes, as: O
  @limits %{nodes: 1_000_000, bytes: 20_000_000, depth: 200_000}
  @moduletag obligations:
               ~w(CL-OBL-001 CL-OBL-002 CL-OBL-003 CL-OBL-004 CL-OBL-005 CL-OBL-006 CL-OBL-007 CL-OBL-008 CL-OBL-009 CL-OBL-010)

  defp descriptor(kind, schema \\ :integer) do
    order = if kind == :list, do: nil, else: elem(Order.new(schema), 1)
    {:ok, d} = C.describe(kind, schema, order)
    d
  end

  defp value(d, entries) do
    assert {:ok, {:catena_adt, _, 1, {value}}} = C.construct(d, entries, @limits)
    value
  end

  defp callback(module, input_type, output_type, expression, input_schema, output_schema) do
    source = """
    (module #{module} (edition 0.1) (revision 0.1.8) (origin "test://collections/#{module}")
      (export value work)
      (def work (signature (Fn #{input_type} (effects) #{output_type}) (uses))
        (fn (value #{input_type}) #{expression})))
    """

    {:ok, core} = Catena.check_kernel(source)
    {:ok, artifact} = Catena.Calling.Artifact.build(core)
    {:ok, input} = Catena.Foreign.Codec.new({:data, input_schema})
    {:ok, output} = Catena.Foreign.Codec.new({:data, output_schema})
    {:ok, cb} = Catena.Foreign.Callback.new(artifact, core, "work", [], input, output, @limits)

    on_exit(fn ->
      :code.delete(artifact.module)
      :code.purge(artifact.module)
    end)

    cb
  end

  test "ordinary nominal package compiles and rejects changed packages" do
    assert {:ok, module, binary, metadata} = C.compile()
    assert :ok = Catena.TypedCore.Verifier.verify(metadata.core)
    assert {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"collection-roles", binary)

    try do
      assert apply(module, :sequence_empty, []) == value(descriptor(:list), [])
      assert apply(module, :keyed_empty, []) == value(descriptor(:map), [])
      assert apply(module, :unique_empty, []) == value(descriptor(:set), [])
      map = value(descriptor(:map), [{3, 1}, {1, 2}])
      assert apply(module, :keyed_entries, [map]) == C.sequence([{1, 2}, {3, 1}])
      assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)
      assert interface.standard_digest == C.package!()["hierarchy_digest"]
      assert Catena.Categorical.collection_package() == C.package!()

      assert {:error, :invalid_collection_package} =
               C.validate_package(Map.put(C.package!(), "contract", "0.1.64"))
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  test "strict construction detects the first duplicate in input order, with explicit immutable replacement" do
    d = descriptor(:map)
    assert {:ok, failure} = C.construct(d, [{2, 10}, {1, 20}, {2, 30}, {1, 40}], @limits)
    assert failure == O.failure({2, 2})
    original = value(d, [{3, 30}, {1, 10}])
    assert {:ok, [{1, 10}, {3, 30}]} = C.entries(d, original, @limits)
    assert {:ok, present} = C.lookup(d, original, 1, @limits)
    assert present == O.present(10)
    assert {:ok, absent} = C.lookup(d, original, 2, @limits)
    assert absent == O.absent()
    assert {:ok, {:catena_adt, _, 1, {changed}}} = C.replace(d, original, 1, 99, @limits)
    assert {:ok, [{1, 99}, {3, 30}]} = C.entries(d, changed, @limits)
    assert {:ok, [{1, 10}, {3, 30}]} = C.entries(d, original, @limits)
    assert {:ok, absent} = C.replace(d, original, 2, 99, @limits)
    assert absent == O.absent()
    assert {:error, :invalid_collection_description} = C.verify(Map.put(d, :extra, true))
  end

  test "semantic Float key identity distinguishes the signed zeros and rejects fabricated ordering" do
    d = descriptor(:set, :float)
    set = value(d, [0.0, -0.0, 1.0])
    assert {:ok, [negative, positive, 1.0]} = C.entries(d, set, @limits)
    assert <<negative::float-64>> == <<0x8000000000000000::64>>
    assert <<positive::float-64>> == <<0::64>>
    assert {:error, :invalid_key_order} = Order.verify(%{d.key_order | order: :descending})
    assert {:error, :unsupported_key_order} = Order.new(:boolean)
    assert {:error, :invalid_collection_description} = C.describe(:set, :integer, d.key_order)
  end

  test "verified pure maps preserve shape and deterministic keyed value order" do
    cb =
      callback("CollectionDouble", "Int", "Int", "(multiply (var value) 2)", :integer, :integer)

    d = descriptor(:map)
    original = value(d, [{2, 3}, {1, 4}])
    assert {:ok, ^d, result} = C.map_values(d, original, cb, :integer, @limits)
    assert {:ok, [{1, 8}, {2, 6}]} = C.entries(d, result, @limits)

    assert {:error, :invalid_pure_collection_callback} =
             C.map_values(d, original, fn x -> send(self(), x) end, :integer, @limits)

    ld = descriptor(:list)
    assert {:ok, ^ld, empty} = C.map_values(ld, value(ld, []), cb, :integer, @limits)
    assert {:ok, []} = C.entries(ld, empty, @limits)
  end

  test "combining callbacks execute in input order and traps remain traps" do
    cb =
      callback(
        "CollectionCombineTrap",
        "(Tuple Int Int)",
        "Int",
        "(match (var value) (case (tuple (bind left) (bind right)) (trap (var right))) (case _ (trap -1)))",
        {:tuple, [:integer, :integer]},
        :integer
      )

    assert {:catena_trap, 8} =
             catch_error(
               C.combine(descriptor(:map), [{2, 1}, {1, 2}, {2, 8}, {1, 9}], cb, @limits)
             )
  end

  test "generic categorical implementations satisfy independent map and fold equations on large lists" do
    values = Enum.to_list(1..50_000)
    sequence = C.sequence(values)
    assert C.sequence_map(&Function.identity/1, sequence) == sequence
    left = C.sequence_map(fn x -> x * 2 + 1 end, sequence)

    right =
      sequence
      |> then(&C.sequence_map(fn x -> x * 2 end, &1))
      |> then(&C.sequence_map(fn x -> x + 1 end, &1))

    assert left == right

    assert C.sequence_summarize(fn acc -> fn x -> acc + x end end, 0, sequence) ==
             div(50_000 * 50_001, 2)

    assert {:ok, got} = C.entries(descriptor(:list), left, @limits)
    assert got == Enum.map(values, &(&1 * 2 + 1))
  end

  test "typed variant callbacks support early stop and explicit outcome traversal" do
    type = "(Variant (row (field continue Int) (field stop Int)))"

    cb =
      callback(
        "CollectionStop",
        "(Tuple Int Int)",
        type,
        "(annotate (inject stop 42) #{type})",
        {:tuple, [:integer, :integer]},
        {:variant, %{"continue" => :integer, "stop" => :integer}}
      )

    d = descriptor(:list)
    assert {:ok, 42} = C.fold_while(d, value(d, [1, 2, 3]), cb, 0, :integer, @limits)
    assert {:ok, 0} = C.fold_while(d, value(d, []), cb, 0, :integer, @limits)

    type = "(Variant (row (field success Int) (field failure Int)))"

    cb =
      callback(
        "CollectionTraverse",
        "Int",
        type,
        "(annotate (inject failure (var value)) #{type})",
        :integer,
        {:variant, %{"success" => :integer, "failure" => :integer}}
      )

    assert {:ok, ^d, failed} =
             C.traverse(d, value(d, [3, 1, 2]), cb, :dependent, :integer, :integer, @limits)

    assert failed == O.failure(3)

    assert {:ok, ^d, failed} =
             C.traverse(d, value(d, [3, 1, 2]), cb, :independent, :integer, :integer, @limits)

    assert failed == O.invalid(O.errors([3, 1, 2]))

    assert {:ok, ^d, empty} =
             C.traverse(d, value(d, []), cb, :independent, :integer, :integer, @limits)

    assert empty == O.valid(value(d, []))
  end

  test "specialized ordinary dictionaries execute list and keyed laws" do
    {:ok, _, _, metadata} = C.compile()
    {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)

    int = %{
      "tag" => "constructor",
      "id" => "Int",
      "kind" => "Type",
      "owner" => "catena://builtins"
    }

    roots =
      for family <- ["sequence", "keyed"], method <- ["map", "summarize"] do
        %{
          "template" => "#{family}_#{method}_specialized",
          "export" => "#{family}_#{method}",
          "types" => if(family == "keyed", do: [int], else: []),
          "instances" => []
        }
      end

    manifest = %{
      companion_module: "CollectionSpecialization",
      modules: [],
      interfaces: [],
      output: "CollectionSpecialization.beam",
      roots: roots
    }

    assert {:ok, module, binary, info} = Catena.Package.Linker.link(manifest, [interface])
    assert info.evidence_erased
    assert {:module, ^module} = :code.load_binary(module, ~c"collection-specialized", binary)

    try do
      for xs <- [[], [2], [3, 1, 5]], kind <- [:list, :map] do
        d = descriptor(kind)
        input = if kind == :list, do: xs, else: Enum.with_index(xs)
        subject = value(d, input)
        map = if kind == :list, do: :sequence_map, else: :keyed_map
        fold = if kind == :list, do: :sequence_summarize, else: :keyed_summarize
        assert apply(module, map, [&Function.identity/1, subject]) == subject
        first = apply(module, map, [fn x -> x * 2 end, subject])

        assert apply(module, map, [fn x -> x + 1 end, first]) ==
                 apply(module, map, [fn x -> x * 2 + 1 end, subject])

        expected =
          if kind == :list,
            do: Enum.sum(xs),
            else: Enum.sum(Enum.to_list(0..max(length(xs) - 1, 0)))

        assert apply(module, fold, [fn acc -> fn x -> acc + x end end, 0, subject]) == expected
      end
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  test "closed combining evidence obeys associative equations and rejects fabricated laws" do
    alias Catena.Standard.Collections.Combining

    for operation <- [:integer_sum, :integer_minimum, :integer_maximum] do
      {:ok, evidence} = Combining.new(operation)

      for a <- [-5, 0, 8], b <- [-2, 0, 4], c <- [-9, 0, 3] do
        assert Combining.apply(evidence, Combining.apply(evidence, a, b), c) ==
                 Combining.apply(evidence, a, Combining.apply(evidence, b, c))
      end

      d = descriptor(:map)
      assert {:ok, collection} = C.combine_lawful(d, [{2, 1}, {1, 3}, {2, 4}], evidence, @limits)
      assert {:ok, [{1, 3}, {2, combined}]} = C.entries(d, collection, @limits)
      assert combined == Combining.apply(evidence, 1, 4)

      assert {:error, :invalid_combining_evidence} =
               Combining.verify(%{evidence | associative: false})
    end
  end

  test "effectful compiled callbacks cannot masquerade as pure collection maps" do
    {:ok, core} =
      Catena.check_kernel("""
      (module CollectionEffect (edition 0.1) (revision 0.1.8) (origin "test://collection-effect")
        (export value work) (effect Ask (operation ask (params) Int))
        (def work (signature (Fn Int (effects Ask) Int) (uses))
          (fn (value Int) (request Ask ask))))
      """)

    {:ok, artifact} = Catena.Calling.Artifact.build(core)
    {:ok, codec} = Catena.Foreign.Codec.new({:data, :integer})

    assert {:error, :invalid_foreign_callback} =
             Catena.Foreign.Callback.new(artifact, core, "work", [], codec, codec, @limits)
  end

  test "early dependent failure skips a later trap while independent traversal preserves it" do
    type = "(Variant (row (field success Int) (field failure Int)))"

    cb =
      callback(
        "CollectionSkipTrap",
        "Int",
        type,
        "(match (equal (var value) 1) (case true (annotate (inject failure 9) #{type})) (case false (trap 77)))",
        :integer,
        {:variant, %{"success" => :integer, "failure" => :integer}}
      )

    d = descriptor(:list)
    xs = value(d, [1, 2])
    assert {:ok, ^d, failed} = C.traverse(d, xs, cb, :dependent, :integer, :integer, @limits)
    assert failed == O.failure(9)

    assert {:catena_trap, 77} =
             catch_error(C.traverse(d, xs, cb, :independent, :integer, :integer, @limits))
  end

  test "checked set transformations return collisions and complete output bounds are enforced" do
    cb = callback("CollectionCollapse", "Int", "Int", "0", :integer, :integer)
    d = descriptor(:set)
    assert {:ok, failure} = C.transform_set(d, value(d, [3, 1]), cb, d.key_order, @limits)
    assert failure == O.failure({0, 1})
    assert {:ok, :eq} = Order.compare(d.key_order, 3, 3)
    assert {:error, _} = C.construct(descriptor(:list), [], %{nodes: 10, bytes: 1, depth: 10})
    assert {:error, :invalid_collection_description} = C.lookup(nil, nil, 1, @limits)
  end
end
