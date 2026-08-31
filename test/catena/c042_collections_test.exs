defmodule Catena.C042CollectionsTest do
  use ExUnit.Case, async: false

  alias Catena.{Data, LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37)

  @list_kernel """
  (module C042List
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c042-list")
    (export type List)
    (export value length)
    (export value replace_head)
    (export value head_of)
    (export value second_of)
    (export value main)
    (data List
      (params a)
      (constructor Nil (fields))
      (constructor Cons (fields a (List a))))
    (def length
      (signature (Fn (List Int) (effects) Int) (uses))
      (fn (list (List Int))
        (match (var list)
          (case (constructor Nil) 0)
          (case (constructor Cons (bind head) (bind tail))
            (add 1 (call (var length) (var tail)))))))
    (def replace_head
      (signature (Fn (List Int) (effects) (Fn Int (effects) (List Int))) (uses))
      (fn (list (List Int))
        (fn (value Int)
          (match (var list)
            (case (constructor Nil) (var list))
            (case (constructor Cons (bind old_head) (bind tail))
              (construct Cons (var value) (var tail)))))))
    (def head_of
      (signature (Fn (List Int) (effects) Int) (uses))
      (fn (list (List Int))
        (match (var list)
          (case (constructor Nil) 0)
          (case (constructor Cons (bind head) (bind tail)) (var head)))))
    (def second_of
      (signature (Fn (List Int) (effects) Int) (uses))
      (fn (list (List Int))
        (match (var list)
          (case (constructor Nil) 0)
          (case (constructor Cons (bind head) (bind tail))
            (match (var tail)
              (case (constructor Nil) 0)
              (case (constructor Cons (bind second) (bind rest)) (var second)))))))
    (def main
      (signature (Tuple Int Int Int Int) (uses))
      (let one_two_three
        (construct Cons 1 (construct Cons 2 (construct Cons 3 (construct Nil))))
        (let one_ten_three
          (call (call (var replace_head) (var one_two_three)) 10)
          (tuple
            (call (var length) (var one_two_three))
            (call (var length) (var one_ten_three))
            (call (var head_of) (var one_ten_three))
            (call (var second_of) (var one_two_three)))))))
  """

  @find_kernel """
  (module C042Find
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c042-find")
    (export type List)
    (export type Pair)
    (export type PairMap)
    (export value find)
    (export value present)
    (export value main)
    (data List
      (params a)
      (constructor Nil (fields))
      (constructor Cons (fields a (List a))))
    (data Pair
      (params k v)
      (constructor Pair (fields k v)))
    (data PairMap
      (params k v)
      (constructor None (fields))
      (constructor Some (fields (Pair k v))))
    (def find
      (signature
        (Fn (List (Pair Int Int)) (effects) (Fn Int (effects) (PairMap Int Int)))
        (uses))
      (fn (entries (List (Pair Int Int)))
        (fn (target Int)
          (match (var entries)
            (case (constructor Nil) (construct None))
            (case (constructor Cons (bind entry) (bind rest))
              (match (var entry)
                (case (constructor Pair (bind key) (bind value))
                  (match (equal (var key) (var target))
                    (case true (construct Some (construct Pair (var key) (var value))))
                    (case false (call (call (var find) (var rest)) (var target)))))))))))
    (def present
      (signature (Fn (PairMap Int Int) (effects) Int) (uses))
      (fn (result (PairMap Int Int))
        (match (var result)
          (case (constructor None) 0)
          (case (constructor Some (bind bound_entry))
            (match (var bound_entry)
              (case (constructor Pair (bind bound_key) (bind value)) (var value)))))))
    (def main
      (signature Int (uses))
      (call (var present)
        (call (call (var find) (var entries)) TARGET))))
  """

  describe "revision registration" do
    @tag obligations: ~w(CO-OBL-001 CO-OBL-005)
    test "0.1.37 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.37"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.37" in LanguageVersion.compilable_revisions()
      refute "0.1.37" in LanguageVersion.artifact_versions()
      refute "0.1.37" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} ==
               LanguageLifecycle.state("collection-construction-and-update", "0.1.37")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-37-collection-construction-and-update")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "collection-construction-and-update/the-six-topic-decision.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.37"}}} = Catena.decode_source_text("")
      assert true = Catena.Values.value?(1.5)

      refute function_exported?(Catena, :update_operator, 2)
      refute function_exported?(Catena, :collection_literal, 0)
      refute function_exported?(Catena, :complexity_of, 1)
    end
  end

  describe "the declared-List witness" do
    @tag obligations: ~w(CO-OBL-002 CO-OBL-003)
    test "construction, match recursion, length, and replace-head agree on both targets" do
      assert {:ok, core} = Catena.check_kernel(@list_kernel)

      assert {:ok, {3, 3, 10, 2}, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C042List, binary, _metadata} = Catena.compile_kernel(@list_kernel)

      assert {:module, :C042List} =
               :code.load_binary(:C042List, ~c"c042_list.beam", binary)

      assert apply(:C042List, :main, []) == {3, 3, 10, 2}

      on_exit(fn ->
        :code.purge(:C042List)
        :code.delete(:C042List)
      end)
    end
  end

  describe "the miss witness" do
    @tag obligations: ~w(CO-OBL-004 CO-OBL-003)
    test "a lookup miss is an ordinary value: total, typed, never a trap" do
      miss =
        find_program(
          "C042Miss",
          "(construct Cons (construct Pair 10 20) (construct Cons (construct Pair 11 30) (construct Nil)))",
          42
        )

      assert {:ok, core} = Catena.check_kernel(miss)
      assert {:ok, 0, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C042Miss, miss_binary, _} = Catena.compile_kernel(miss)

      assert {:module, :C042Miss} = :code.load_binary(:C042Miss, ~c"c042_miss.beam", miss_binary)
      assert apply(:C042Miss, :main, []) == 0

      on_exit(fn ->
        :code.purge(:C042Miss)
        :code.delete(:C042Miss)
      end)

      hit =
        find_program(
          "C042Hit",
          "(construct Cons (construct Pair 10 20) (construct Cons (construct Pair 42 7) (construct Nil)))",
          42
        )

      assert {:ok, hit_core} = Catena.check_kernel(hit)
      assert {:ok, 7, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(hit_core, "main")

      assert {:ok, :C042Hit, hit_binary, _} = Catena.compile_kernel(hit)

      assert {:module, :C042Hit} =
               :code.load_binary(:C042Hit, ~c"c042_hit.beam", hit_binary)

      assert apply(:C042Hit, :main, []) == 7

      on_exit(fn ->
        :code.purge(:C042Hit)
        :code.delete(:C042Hit)
      end)
    end

    @tag obligations: ~w(CO-OBL-007)
    test "key equality rides C035's comparable set" do
      source =
        find_program(
          "C042Keys",
          "(construct Cons (construct Pair 10 20) (construct Cons (construct Pair 50 9) (construct Nil)))",
          50
        )

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:ok, 9, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C042Keys, binary, _} = Catena.compile_kernel(source)

      assert {:module, :C042Keys} =
               :code.load_binary(:C042Keys, ~c"c042_keys.beam", binary)

      assert apply(:C042Keys, :main, []) == 9

      on_exit(fn ->
        :code.purge(:C042Keys)
        :code.delete(:C042Keys)
      end)

      data = sample_data()

      assert Data.comparable_type?(:integer, data)
      assert Data.comparable_type?({:tuple, [:integer, :integer]}, data)
      refute Data.comparable_type?({:function, :integer, :integer}, data)
    end
  end

  describe "classification and absences" do
    @tag obligations: ~w(CO-OBL-006 CO-OBL-005)
    test "no dedicated collection machinery exists to classify" do
      refute function_exported?(Catena, :lookup_bang, 2)
      refute function_exported?(Catena, :update_at, 3)
      refute function_exported?(Catena, :collection_complexity, 1)
    end

    @tag obligations: ~w(CO-OBL-008)
    test "determinism: repeated runs repeat" do
      assert {:ok, core} = Catena.check_kernel(@list_kernel)

      assert {:ok, first, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")
      assert {:ok, second, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")
      assert first == second

      repeat = String.replace(@list_kernel, "C042List", "C042Repeat", global: false)
      assert {:ok, :C042Repeat, binary, _} = Catena.compile_kernel(repeat)

      assert {:module, :C042Repeat} =
               :code.load_binary(:C042Repeat, ~c"c042_repeat.beam", binary)

      assert apply(:C042Repeat, :main, []) == apply(:C042Repeat, :main, [])

      on_exit(fn ->
        :code.purge(:C042Repeat)
        :code.delete(:C042Repeat)
      end)
    end
  end

  defp find_program(module, entries, target) do
    @find_kernel
    |> String.replace("C042Find", module, global: false)
    |> String.replace(
      "(call (call (var find) (var entries)) TARGET)",
      "(call (call (var find) #{entries}) #{target})"
    )
  end

  defp sample_data do
    Catena.Data.elaborate(
      %{
        origin: "test://c042",
        module: "Sample",
        types: [],
        type_groups: [],
        type_exports: [],
        imports: [],
        definitions: [],
        exports: [],
        traits: [],
        instances: [],
        effects: %{families: %{}, handlers: %{}}
      },
      []
    )
  end
end
