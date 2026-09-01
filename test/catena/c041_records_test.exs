defmodule Catena.C041RecordsTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Values}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43)

  @fixture """
  (module C041Fixture
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c041-fixture")
    (export value main)
    (export value choose_variant)
    (def choose_variant
      (signature (Variant (row (field left Int) (field right Bool))) (uses))
      (inject left 41))
    (def main
      (signature (Tuple Int Bool Int) (uses))
      (let record_value
        (record (field answer 1) (field ready true))
        (let updated
          (update (var record_value) answer 2)
          (tuple
            (select (var updated) answer)
            (select (restrict (extend (var updated) temporary 9) temporary) ready)
            (match (var choose_variant)
              (case (variant left (bind value)) (var value))
              (case (variant right (bind ignored)) 5)))))))
  """

  @variant_dispatch """
  (module C041Variant
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c041-variant")
    (export value main)
    (def choose
      (signature (Variant (row (field left Int) (field right Bool))) (uses))
      (inject left 41))
    (def main
      (signature Int (uses))
      (match (var choose)
        (case (variant left (bind value)) (var value))
        (case (variant right (bind ignored)) 0))))
  """

  @duplicate """
  (module C041Duplicate
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c041-duplicate")
    (export value main)
    (def main
      (signature (Record (row (field answer Int))) (uses))
      (record (field answer 1) (field answer 2))))
  """

  describe "revision registration" do
    @tag obligations: ~w(SR-OBL-001 SR-OBL-006)
    test "0.1.36 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.43"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.36" in LanguageVersion.compilable_revisions()
      refute "0.1.36" in LanguageVersion.artifact_versions()
      refute "0.1.36" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} ==
               LanguageLifecycle.state("structural-records-and-variants", "0.1.36")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-36-structural-records-and-variants")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "structural-records-and-variants/the-operation-table.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.43"}}} = Catena.decode_source_text("")
      assert true = Values.value?(1.5)
      assert true = Values.value?("record content")

      refute function_exported?(Catena, :open_record, 1)
      refute function_exported?(Catena, :record_layout, 1)
    end
  end

  describe "the operation table" do
    @tag obligations: ~w(SR-OBL-002 SR-OBL-004)
    test "the fixture's operation round-trip agrees on stepper and compiled BEAM" do
      assert {:ok, core} = Catena.check_kernel(@fixture)

      assert {:ok, {2, true, 41}, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C041Fixture, binary, _metadata} = Catena.compile_kernel(@fixture)

      assert {:module, :C041Fixture} =
               :code.load_binary(:C041Fixture, ~c"c041_fixture.beam", binary)

      assert apply(:C041Fixture, :main, []) == {2, true, 41}

      on_exit(fn ->
        :code.purge(:C041Fixture)
        :code.delete(:C041Fixture)
      end)
    end

    @tag obligations: ~w(SR-OBL-007)
    test "variant inject is a value and dispatch selects by semantic label then payload" do
      assert {:ok, core} = Catena.check_kernel(@variant_dispatch)

      assert {:ok, 41, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C041Variant, binary, _} = Catena.compile_kernel(@variant_dispatch)

      assert {:module, :C041Variant} =
               :code.load_binary(:C041Variant, ~c"c041_variant.beam", binary)

      assert apply(:C041Variant, :main, []) == 41

      on_exit(fn ->
        :code.purge(:C041Variant)
        :code.delete(:C041Variant)
      end)
    end

    @tag obligations: ~w(SR-OBL-003)
    test "a duplicate-label literal rejects at the kernel boundary" do
      assert {:error, %{id: id}} = Catena.check_kernel(@duplicate)
      assert id in ["T005", "SYN002"]
    end
  end

  describe "rows and representation" do
    @tag obligations: ~w(SR-OBL-003 SR-OBL-004)
    test "open tails compose through type positions over closed records" do
      assert {:ok, core} = Catena.check_kernel(@fixture)

      choose_variant =
        Enum.find(core.definitions, &(&1.name == "choose_variant"))

      {:variant, %{fields: fields, tail: nil}} = choose_variant.signature
      assert Map.get(fields, "left") == :integer
      assert Map.get(fields, "right") == :boolean
    end

    @tag obligations: ~w(SR-OBL-005 SR-OBL-008)
    test "records compare as semantic maps: order never affects equality" do
      assert Values.comparable?(%{answer: 2, ready: true})
      assert %{a: 1, b: 2} == %{b: 2, a: 1}

      assert {:ok, core} = Catena.check_kernel(@fixture)
      assert {:ok, first, _} = Catena.Kernel.Stepper.run(core, "main")
      assert {:ok, second, _} = Catena.Kernel.Stepper.run(core, "main")
      assert first == second
    end

    @tag obligations: ~w(SR-OBL-006 SR-OBL-008)
    test "the JSON-AST absence holds: no record tags, no frontend entry points" do
      refute function_exported?(Catena, :compile_record, 1)
      refute function_exported?(Catena, :record_expression, 0)
      refute "record" in LanguageVersion.compilable_revisions()
    end
  end
end
