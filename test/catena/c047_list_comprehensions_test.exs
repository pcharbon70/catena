defmodule Catena.C047ListComprehensionsTest do
  use ExUnit.Case, async: false

  alias Catena.{Comprehension, LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45)

  @option_type "(data Option (params a) (constructor None (fields)) (constructor Some (fields a)))"

  describe "revision registration" do
    @tag obligations: ~w(LC-OBL-001)
    test "0.1.39 is an exact registered revision with the elaboration boundary declared" do
      assert LanguageVersion.latest() == "0.1.45"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.39" in LanguageVersion.compilable_revisions()
      refute "0.1.39" in LanguageVersion.artifact_versions()
      refute "0.1.39" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("list-comprehensions", "0.1.39")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-39-list-comprehensions")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "list-comprehensions/the-surface-contract.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.45"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Comprehension)
      assert true = function_exported?(Comprehension, :elaborate, 1)
      assert true = function_exported?(Comprehension, :new, 1)
    end
  end

  describe "the dormant elaboration boundary" do
    @tag obligations: ~w(LC-OBL-002)
    test "the grammar's semantic roles fix the qualifier shapes" do
      assert_raise ArgumentError, ~r/first qualifier must be a generator/, fn ->
        Comprehension.new(
          module: "C047Bad",
          origin: "test://c047/bad",
          qualifiers: [{:filter, [expr: "true"]}],
          yield: "1",
          result_element_type: "Int"
        )
        |> Comprehension.elaborate()
      end

      {:ok, roles_source, []} = filter_let_spec() |> Comprehension.elaborate()

      for role <- ["(data List", "(constructor Cons", "(case true", "(case false"] do
        assert roles_source =~ role
      end
    end

    @tag obligations: ~w(LC-OBL-003)
    test "sources must be lists: a non-list source is a typing error" do
      {:ok, source, []} =
        Comprehension.new(
          module: "C047Src",
          origin: "test://c047/src",
          qualifiers: [
            {:generator,
             [
               pattern: "(bind v)",
               element_type: "Int",
               source: "(add 1 2)",
               binds: [{"v", "Int"}]
             ]}
          ],
          yield: "(var v)",
          result_element_type: "Int"
        )
        |> Comprehension.elaborate()

      assert {:error, %{id: id}} = Catena.check_kernel(source)
      assert id in ["T002", "A002"]
    end

    @tag obligations: ~w(LC-OBL-004 LC-OBL-009)
    test "left-to-right depth-first traversal with dependency and empty input behavior" do
      {:ok, source, []} = cartesian_spec() |> Comprehension.elaborate()

      assert {:ok, [14, 15, 24, 25], []} = run_and_flatten(source)

      empty =
        Comprehension.new(
          module: "C047Empty",
          origin: "test://c047/empty",
          context: [{"xs", "(List Int)", "(construct Nil)"}],
          qualifiers: [
            {:generator,
             [pattern: "(bind x)", element_type: "Int", source: "(var xs)", binds: [{"x", "Int"}]]}
          ],
          yield: "(var x)",
          result_element_type: "Int"
        )

      {:ok, empty_source, []} = Comprehension.elaborate(empty)
      assert {:ok, [], []} = run_and_flatten(empty_source)
    end

    @tag obligations: ~w(LC-OBL-005)
    test "when filters: false skips the element, other failures propagate, no guard fragment" do
      {:ok, source, []} = filter_let_spec() |> Comprehension.elaborate()
      assert {:ok, [30, 40], []} = run_and_flatten(source)

      {:ok, non_bool, []} =
        Comprehension.new(
          module: "C047NonBool",
          origin: "test://c047/nonbool",
          context: [{"xs", "(List Int)", ints([1, 2])}],
          qualifiers: [
            {:generator,
             [pattern: "(bind v)", element_type: "Int", source: "(var xs)", binds: [{"v", "Int"}]]},
            {:filter, [expr: "(add (var v) 1)"]}
          ],
          yield: "(var v)",
          result_element_type: "Int"
        )
        |> Comprehension.elaborate()

      assert {:error, %{id: "T002"}} = Catena.check_kernel(non_bool)
    end

    @tag obligations: ~w(LC-OBL-006)
    test "the pattern-generator split: total, mismatch-as-skip, and the markers" do
      {:ok, source, []} = case_spec() |> Comprehension.elaborate()
      assert {:ok, [1, 3], []} = run_and_flatten(source)

      {:ok, non_total, []} =
        Comprehension.new(
          module: "C047NT",
          origin: "test://c047/nt",
          types: [@option_type],
          context: [{"options", "(List (Option Int))", options([1])}],
          qualifiers: [
            {:generator,
             [
               pattern: "(constructor Some (bind v))",
               element_type: "(Option Int)",
               source: "(var options)",
               binds: [{"v", "Int"}]
             ]}
          ],
          yield: "(var v)",
          result_element_type: "Int"
        )
        |> Comprehension.elaborate()

      assert {:error, %{id: "M001"}} = Catena.check_kernel(non_total)

      assert {:error, %{id: "LCP002"}} =
               Comprehension.elaborate(case_spec_with_never_pattern())

      {:ok, _source, advisories} = Comprehension.elaborate(case_spec_with_total_pattern())
      assert [%{id: "LCP003"}] = advisories
    end

    @tag obligations: ~w(LC-OBL-007)
    test "scope: rebinding rejects, unused bindings report BS001, nothing escapes" do
      rebinding =
        Comprehension.new(
          module: "C047Rebind",
          origin: "test://c047/rebind",
          context: [{"xs", "(List Int)", ints([1, 2])}],
          qualifiers: [
            {:generator,
             [pattern: "(bind x)", element_type: "Int", source: "(var xs)", binds: [{"x", "Int"}]]},
            {:let,
             [pattern: "(bind x)", value_type: "Int", expr: "(var x)", binds: [{"x", "Int"}]]}
          ],
          yield: "(var x)",
          result_element_type: "Int"
        )

      assert {:error, %{id: "LCP001"}} = Comprehension.elaborate(rebinding)

      {:ok, _source, advisories} =
        Comprehension.new(
          module: "C047Unused",
          origin: "test://c047/unused",
          context: [{"xs", "(List Int)", ints([1])}],
          qualifiers: [
            {:generator,
             [pattern: "(bind v)", element_type: "Int", source: "(var xs)", binds: [{"v", "Int"}]]}
          ],
          yield: "7",
          result_element_type: "Int"
        )
        |> Comprehension.elaborate()

      assert [%{id: "BS001"}] = advisories
    end

    @tag obligations: ~w(LC-OBL-008 LC-OBL-012)
    test "exact order and multiplicity with sequential execution and visible effect rows" do
      {:ok, source, []} = filter_let_spec() |> Comprehension.elaborate()
      assert source =~ "(case true"
      assert source =~ "(case false"

      effectful =
        Comprehension.new(
          module: "C047Ask",
          origin: "test://c047/ask",
          uses: ["Ask"],
          context: [{"xs", "(List Int)", ints([1, 2, 3])}],
          qualifiers: [
            {:generator,
             [pattern: "(bind x)", element_type: "Int", source: "(var xs)", binds: [{"x", "Int"}]]},
            {:filter, [expr: "(greater (var x) 1)"]}
          ],
          yield: "(var x)",
          result_element_type: "Int"
        )

      {:ok, effect_source, []} = Comprehension.elaborate(effectful)
      assert effect_source =~ "(uses Ask)"

      refute function_exported?(Catena, :parallel_comprehension, 1)
      refute function_exported?(Catena, :concurrent_yield, 0)
    end

    @tag obligations: ~w(LC-OBL-010 LC-OBL-013)
    test "the fused worker chain and the extensional map equation" do
      {:ok, source, []} = map_spec() |> Comprehension.elaborate()

      assert {:ok, [2, 3, 4], []} = run_and_flatten(source)
      assert {:ok, hand_values, []} = run_and_flatten(hand_written_map())
      assert {:ok, [2, 3, 4], []} = {:ok, hand_values, []}

      {:ok, cartesian_source, []} = cartesian_spec() |> Comprehension.elaborate()

      def_count =
        Regex.scan(~r/\n  \(def /, cartesian_source) |> length()

      assert def_count == 2 + 2 + 1 + 1

      refute cartesian_source =~ "trait-call"
      refute cartesian_source =~ "dispatch"
    end

    @tag obligations: ~w(LC-OBL-011)
    test "the result is a list: List B output with no other target entry points" do
      {:ok, source, []} = map_spec() |> Comprehension.elaborate()

      assert source =~
               "(def main\n    (signature (List Int) (uses))"

      refute function_exported?(Comprehension, :elaborate_map, 1)
      refute function_exported?(Comprehension, :elaborate_set, 1)
      refute function_exported?(Comprehension, :elaborate_binary, 1)
      refute function_exported?(Comprehension, :elaborate_stream, 1)
    end

    @tag obligations: ~w(LC-OBL-013)
    test "stack-safe production for deep inputs on BEAM" do
      deep = 900

      {:ok, source, []} =
        Comprehension.new(
          module: "C047Deep",
          origin: "test://c047/deep",
          context: [{"xs", "(List Int)", ints(Enum.to_list(1..deep))}],
          qualifiers: [
            {:generator,
             [pattern: "(bind v)", element_type: "Int", source: "(var xs)", binds: [{"v", "Int"}]]}
          ],
          yield: "(var v)",
          result_element_type: "Int"
        )
        |> Comprehension.elaborate()

      assert {:ok, :C047Deep, binary, _} = Catena.compile_kernel(source)
      assert {:module, :C047Deep} = :code.load_binary(:C047Deep, ~c"c047_deep.beam", binary)
      assert deep == flatten_value(apply(:C047Deep, :main, [])) |> length()

      on_exit(fn ->
        :code.purge(:C047Deep)
        :code.delete(:C047Deep)
      end)
    end

    @tag obligations: ~w(LC-OBL-014)
    test "determinism and the exclusion boundary" do
      {:ok, first, []} = map_spec() |> Comprehension.elaborate()
      {:ok, second, []} = map_spec() |> Comprehension.elaborate()
      assert first == second

      refute function_exported?(Catena, :comprehension_expression, 1)
      refute function_exported?(Catena, :lazy_comprehension, 0)
      refute function_exported?(Catena, :stream_comprehension, 0)
      refute function_exported?(Comprehension, :elaborate_iterator, 1)
    end
  end

  defp map_spec do
    Comprehension.new(
      module: "C047Map",
      origin: "test://c047/map",
      context: [{"xs", "(List Int)", ints([1, 2, 3])}],
      qualifiers: [
        {:generator,
         [pattern: "(bind x)", element_type: "Int", source: "(var xs)", binds: [{"x", "Int"}]]}
      ],
      yield: "(add (var x) 1)",
      result_element_type: "Int"
    )
  end

  defp cartesian_spec do
    Comprehension.new(
      module: "C047Cart",
      origin: "test://c047/cart",
      context: [
        {"xs", "(List Int)", ints([1, 2])},
        {"ys", "(List Int)", ints([4, 5])}
      ],
      qualifiers: [
        {:generator,
         [pattern: "(bind x)", element_type: "Int", source: "(var xs)", binds: [{"x", "Int"}]]},
        {:generator,
         [pattern: "(bind y)", element_type: "Int", source: "(var ys)", binds: [{"y", "Int"}]]}
      ],
      yield: "(add (multiply (var x) 10) (var y))",
      result_element_type: "Int"
    )
  end

  defp filter_let_spec do
    Comprehension.new(
      module: "C047When",
      origin: "test://c047/when",
      context: [{"xs", "(List Int)", ints([1, 2, 3, 4])}],
      qualifiers: [
        {:generator,
         [pattern: "(bind x)", element_type: "Int", source: "(var xs)", binds: [{"x", "Int"}]]},
        {:filter, [expr: "(greater (var x) 2)"]},
        {:let,
         [
           pattern: "(bind y)",
           value_type: "Int",
           expr: "(multiply (var x) 10)",
           binds: [{"y", "Int"}]
         ]}
      ],
      yield: "(var y)",
      result_element_type: "Int"
    )
  end

  defp case_spec do
    Comprehension.new(
      module: "C047Case",
      origin: "test://c047/case",
      types: [@option_type],
      context: [{"options", "(List (Option Int))", options([1, nil, 3])}],
      qualifiers: [
        {:case_generator,
         [
           pattern: "(constructor Some (bind v))",
           element_type: "(Option Int)",
           source: "(var options)",
           binds: [{"v", "Int"}]
         ]}
      ],
      yield: "(var v)",
      result_element_type: "Int"
    )
  end

  defp case_spec_with_never_pattern do
    Comprehension.new(
      module: "C047Never",
      origin: "test://c047/never",
      types: [@option_type],
      context: [{"options", "(List (Option Int))", options([1])}],
      qualifiers: [
        {:case_generator,
         [
           pattern: "(constructor Left (bind v))",
           element_type: "(Option Int)",
           source: "(var options)",
           binds: [{"v", "Int"}]
         ]}
      ],
      yield: "(var v)",
      result_element_type: "Int"
    )
  end

  defp case_spec_with_total_pattern do
    Comprehension.new(
      module: "C047Total",
      origin: "test://c047/total",
      types: [@option_type],
      context: [{"xs", "(List Int)", ints([1])}],
      qualifiers: [
        {:case_generator,
         [pattern: "(bind v)", element_type: "Int", source: "(var xs)", binds: [{"v", "Int"}]]}
      ],
      yield: "(var v)",
      result_element_type: "Int"
    )
  end

  defp hand_written_map do
    """
    (module C047HandMap
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://c047/hand-map")
      (export type List)
      (export value go)
      (export value rev)
      (export value main)
      (data List
        (params a)
        (constructor Nil (fields))
        (constructor Cons (fields a (List a))))
      (def xs
        (signature (List Int) (uses))
        #{ints([1, 2, 3])})
      (def go
        (signature (Fn (List Int) (effects) (Fn (List Int) (effects) (List Int))) (uses))
        (fn (source (List Int)) (fn (acc (List Int)) (match (var source)
          (case (constructor Nil) (call (call (var rev) (var acc)) (construct Nil)))
          (case (constructor Cons (bind head) (bind rest))
            (call (call (var go) (var rest)) (construct Cons (add (var head) 1) (var acc))))))))
      (def rev
        (signature (Fn (List Int) (effects) (Fn (List Int) (effects) (List Int))) (uses))
        (fn (source (List Int)) (fn (acc (List Int)) (match (var source)
          (case (constructor Nil) (var acc))
          (case (constructor Cons (bind head) (bind rest))
            (call (call (var rev) (var rest)) (construct Cons (var head) (var acc))))))))
      (def main
        (signature (List Int) (uses))
        (call (call (var go) (var xs)) (construct Nil))))
    """
  end

  defp ints(values) do
    Enum.reduce(Enum.reverse(values), "(construct Nil)", fn value, acc ->
      "(construct Cons #{value} #{acc})"
    end)
  end

  defp options(values) do
    values
    |> Enum.map(fn
      nil -> "(construct None)"
      value -> "(construct Some #{value})"
    end)
    |> Enum.reverse()
    |> Enum.reduce("(construct Nil)", fn option, acc ->
      "(construct Cons #{option} #{acc})"
    end)
  end

  defp run_and_flatten(source) do
    with {:ok, core} <- Catena.check_kernel(source),
         {:ok, value, %{root_status: :terminated}} <-
           Catena.Kernel.Stepper.run(core, "main") do
      {:ok, flatten_value(value), []}
    else
      {:error, %{id: id}} -> {:error, id, []}
      other -> other
    end
  end

  defp flatten_value({:catena_constructor, :Nil, {}}), do: []
  defp flatten_value({:catena_constructor, :Cons, {head, tail}}), do: [head | flatten_value(tail)]
  defp flatten_value(other), do: other
end
