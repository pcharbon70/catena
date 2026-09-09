defmodule Catena.ForeignCodecTest do
  use ExUnit.Case, async: false

  @moduletag obligations:
               ~w(ET-OBL-001 ET-OBL-002 ET-OBL-003 ET-OBL-004 ET-OBL-005 ET-OBL-006 ET-OBL-007 ET-OBL-008 ET-OBL-009 ET-OBL-010)
  alias Catena.Foreign.{Codec, Budget}
  @limits %{nodes: 10_000, bytes: 100_000, depth: 100}

  test "closed nested data preserves scalar meanings with typed expected refusals" do
    schema =
      {:record,
       %{
         "text" => :text,
         "bytes" => :bytes,
         "result" => {:variant, %{"ok" => {:tuple, [:integer, :boolean, :character, :float]}}}
       }}

    {:ok, codec} = Catena.Type.foreign_codec(schema)

    native = %{
      text: "é",
      bytes: <<255, 0>>,
      result: {:catena_variant, :ok, {1, true, 0x1F600, -0.0}}
    }

    assert {:ok, semantic} = Codec.decode(codec, native, @limits)
    assert {:ok, roundtrip} = Codec.encode(codec, semantic, @limits)
    assert :erlang.term_to_binary(roundtrip) == :erlang.term_to_binary(native)

    assert {:ok, ^semantic} =
             Catena.TypedCore.Verifier.verify_foreign_value(codec, native, @limits)

    for invalid <- [
          %{native | text: <<255>>},
          Map.put(native, :extra, 0),
          Map.delete(native, :text),
          %{native | result: {:catena_variant, :unknown, 0}},
          %{native | bytes: <<1::1>>}
        ] do
      assert {:error, %{kind: :conversion_failure}} = Codec.decode(codec, invalid, @limits)
    end

    for {type, value} <- [integer: false, boolean: 1, float: 1, character: 0xD800, text: <<255>>] do
      {:ok, codec} = Codec.new({:data, type})
      assert {:error, %{kind: :conversion_failure}} = Codec.decode(codec, value, @limits)
    end

    assert {:error, _} = Codec.new({:data, {:function, :integer, :integer}})
    assert {:error, _} = Codec.new({:data, {:var, 1}})
  end

  test "payload-wide budgets have deterministic exact thresholds and reject raw authority carriers" do
    value = {<<>>, 256}
    assert :ok = Budget.check(value, %{nodes: 3, bytes: 2, depth: 1})
    assert {:error, :node_budget_exhausted} = Budget.check(value, %{nodes: 2, bytes: 2, depth: 1})
    assert {:error, :byte_budget_exhausted} = Budget.check(value, %{nodes: 3, bytes: 1, depth: 1})

    assert {:error, :depth_budget_exhausted} =
             Budget.check(value, %{nodes: 3, bytes: 2, depth: 0})

    assert {:error, :invalid_validation_budget} = Budget.check(value, %{nodes: 3, bytes: 2})
    {:ok, codec} = Codec.new({:data, :integer})

    for value <- [self(), make_ref(), fn -> :ok end, {Codec, self(), make_ref()}] do
      assert {:error, %{kind: :conversion_failure, reason: :unsupported_carrier}} =
               Codec.decode(codec, value, @limits)
    end

    {:ok, empty} = Codec.new({:data, :bytes})
    assert {:ok, <<>>} = Codec.decode(empty, <<>>, %{nodes: 1, bytes: 0, depth: 0})
    assert {:ok, huge} = Codec.decode(codec, Integer.pow(2, 4096), @limits)
    assert huge == Integer.pow(2, 4096)
  end

  test "ordinary nominal codecs retain compact and uniform identities and reject forged metadata" do
    {:ok, core} = File.read!("test/fixtures/c002-option.catena.json") |> Catena.check_json()

    for layout <- [:compact, :uniform] do
      {:ok, codec} = Codec.for_export(core, "make", layout)
      {:nominal, description} = codec.schema

      {:ok, native} =
        Catena.ValueBoundary.Nominal.encode(
          description,
          {:catena_value, "test://c002-conformance::C002Fixture::Option::Some", [7]},
          %{nodes: 10, bytes: 100}
        )

      assert {:ok, semantic} = Codec.decode(codec, native, @limits)
      assert {:ok, ^native} = Codec.encode(codec, semantic, @limits)

      assert {:error, _} =
               Codec.decode(
                 %{codec | schema: {:nominal, %{description | export: "missing"}}},
                 native,
                 @limits
               )

      assert {:error, _} = Codec.decode(codec, {:wrong_constructor, 7}, @limits)
    end
  end

  test "proper Erlang lists bridge to verified declared constructors, not a new built-in list type" do
    {core, codec, empty, cons} = sequence()
    assert {:ok, semantic} = Codec.decode(codec, [10, 20], @limits)

    assert semantic ==
             {:catena_value, cons, [10, {:catena_value, cons, [20, {:catena_value, empty, []}]}]}

    assert {:ok, [10, 20]} = Codec.encode(codec, semantic, @limits)
    assert {:ok, native} = Codec.to_native(codec, semantic, @limits)

    assert native ==
             {:catena_constructor, :Link,
              {10, {:catena_constructor, :Link, {20, {:catena_constructor, :End, {}}}}}}

    assert {:ok, ^semantic} = Codec.from_native(codec, native, @limits)
    assert {:error, %{reason: :improper_sequence}} = Codec.decode(codec, [1 | 2], @limits)
    assert {:error, _} = Codec.decode(codec, [false], @limits)
    assert {:error, _} = Codec.sequence_for_export(core, "sample", :fixed, cons, empty)
    private = %{core | exports: %{core.exports | types: []}}
    assert {:error, _} = Codec.sequence_for_export(private, "sample", :fixed, empty, cons)
    assert {:error, _} = Codec.encode(codec, {:catena_value, "forged", []}, @limits)

    assert {:error, %{reason: :depth_budget_exhausted}} =
             Codec.decode(codec, [1, 2, 3], %{@limits | depth: 2})
  end

  test "both lowering owners emit the checked Catena carrier and independent Erlang observes it" do
    {_core, codec, _, _} = sequence()
    {:ok, semantic} = Codec.decode(codec, [4, 5], @limits)

    for {backend, module} <- [
          {Catena.Backend.ErlangAbstract, :ForeignOrdinaryLiteral},
          {Catena.Kernel.Backend, :ForeignFixedLiteral}
        ] do
      assert {:ok, literal} = backend.lower_foreign_value(codec, semantic, @limits)

      forms = [
        {:attribute, 1, :module, module},
        {:attribute, 1, :export, [value: 0]},
        {:function, 1, :value, 0, [{:clause, 1, [], [], [literal]}]}
      ]

      {:ok, ^module, binary, _} =
        Catena.OTP.Compiler.compile(forms, frontend: "foreign-codec-fixture")

      {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"foreign-codec", binary)
      native = apply(module, :value, [])

      assert {:ok, ^semantic} =
               Catena.TypedCore.Verifier.verify_foreign_value(codec, native, @limits)

      :code.purge(module)
      :code.delete(module)
    end
  end

  test "nominal sequence preservation holds across a bounded generated corpus and rejects complete bad suffixes" do
    {_core, codec, _, _} = sequence()

    for count <- 0..32 do
      values = Enum.take(Stream.iterate(-16, &(&1 + 1)), count)
      assert {:ok, semantic} = Codec.decode(codec, values, @limits)
      assert {:ok, ^values} = Codec.encode(codec, semantic, @limits)
      assert {:ok, carrier} = Codec.to_native(codec, semantic, @limits)
      assert {:ok, ^semantic} = Codec.from_native(codec, carrier, @limits)

      assert {:error, %{kind: :conversion_failure}} =
               Codec.decode(codec, values ++ [false], @limits)
    end
  end

  test "exact 0.1.60 codec selection does not widen retained executable or serialized formats" do
    assert Catena.LanguageVersion.latest() == "0.1.65"
    refute "0.1.60" in Catena.LanguageVersion.compilable_revisions()
    refute "0.1.60" in Catena.LanguageVersion.interface_versions()
    refute "0.1.60" in Catena.LanguageVersion.signed_format_versions()
    {:ok, codec} = Codec.new({:data, :integer})
    assert codec.version == "0.1.60"

    for revision <- Catena.LanguageVersion.before(:erlang_type_boundary) do
      assert {:error, %{reason: :invalid_foreign_codec_selection}} =
               Codec.new({:data, :integer},
                 language_selection: Catena.LanguageVersion.legacy_selection(revision)
               )
    end

    assert {:error, _} = Codec.decode(%{codec | version: "0.1.59"}, 1, @limits)
    assert {:error, _} = Codec.decode(Map.put(codec, :unchecked, true), 1, @limits)
  end

  defp sequence do
    {:ok, core} =
      Catena.check_kernel("""
      (module ForeignSequence (edition 0.1) (revision 0.1.8) (origin "test://foreign-sequence")
        (export type Chain) (export value sample)
        (data Chain (params a) (constructor End (fields)) (constructor Link (fields a (Chain a))))
        (def sample (signature (Chain Int) (uses)) (construct Link 1 (construct End))))
      """)

    empty = "test://foreign-sequence::ForeignSequence::Chain::End"
    cons = "test://foreign-sequence::ForeignSequence::Chain::Link"
    {:ok, codec} = Codec.sequence_for_export(core, "sample", :fixed, empty, cons)
    {core, codec, empty, cons}
  end
end
