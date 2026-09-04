defmodule Catena.C040DataModelTest do
  use ExUnit.Case, async: false

  alias Catena.{Data, LanguageLifecycle, LanguageVersion, Text, Values}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47)

  describe "revision registration" do
    @tag obligations: ~w(BM-OBL-001 BM-OBL-008)
    test "0.1.35 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.47"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.35" in LanguageVersion.compilable_revisions()
      refute "0.1.35" in LanguageVersion.artifact_versions()
      refute "0.1.35" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("built-in-data-model", "0.1.35")

      change =
        Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-35-built-in-data-model"))

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "built-in-data-model/the-twelve-way-classification.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.47"}}} = Catena.decode_source_text("")
      assert true = Values.value?(1.5)

      refute function_exported?(Catena, :list_type, 0)
      refute function_exported?(Catena, :ref_of, 1)
      refute function_exported?(Text, :interpolate, 1)
    end
  end

  describe "elaboration" do
    @tag obligations: ~w(BM-OBL-003 BM-OBL-007)
    test "cooked text, raw text, character, and bytes elaborate to typed meanings" do
      assert {:ok, %Text.Meaning{kind: :text, type: :Text, value: "héllo"}} =
               scan_then_elaborate(~s("héllo"))

      assert {:ok, %Text.Meaning{kind: :text, type: :Text, value: "héllo"}} =
               scan_then_elaborate(~s(r#"héllo"#))

      assert {:ok, %Text.Meaning{kind: :character, type: :Character, value: ?é}} =
               scan_then_elaborate("'é'")

      assert {:ok, %Text.Meaning{kind: :bytes, type: :Bytes, value: <<0, 255>>}} =
               scan_then_elaborate(~s(b"\\x00\\xff"))

      assert {:ok, first} = scan_then_elaborate(~s("again"))
      assert {:ok, second} = scan_then_elaborate(~s("again"))
      assert first == second
    end

    @tag obligations: ~w(BM-OBL-003)
    test "cooked and raw forms of equal content elaborate to equal meanings" do
      assert {:ok, cooked} = scan_then_elaborate(~s("line"))
      assert {:ok, raw} = scan_then_elaborate(~s(r"line"))
      assert cooked == raw
      assert {:ok, raw_hashed} = scan_then_elaborate(~s(r##"line"##))
      assert cooked == raw_hashed
    end

    @tag obligations: ~w(BM-OBL-007)
    test "elaboration covers exactly the three text kinds" do
      {:ok, %{literal: literal}} = Catena.scan_literal("42")

      assert_raise ArgumentError, fn -> Text.elaborate(literal) end
    end
  end

  describe "the classification and comparability" do
    @tag obligations: ~w(BM-OBL-002 BM-OBL-004)
    test "the three new types are values, comparable, and orderable" do
      assert Values.value?("text content")
      assert Values.value?(<<0, 255>>)
      assert Values.comparable?("text content")
      assert Values.comparable?(<<0, 255>>)
      assert Values.orderable?("text content")
      assert Values.orderable?(<<0, 255>>)

      assert Values.comparable?(%Text.Meaning{kind: :text, type: :Text, value: "a"})

      refute Values.comparable?(:unit)
      refute Values.comparable?({:catena_process, "p1"})
      refute Values.orderable?(true)

      assert :eq = Values.compare("abc", "abc")
      assert :lt = Values.compare("abc", "abd")
      assert :gt = Values.compare("b", "a")
      assert :lt = Values.compare(<<1>>, <<1, 0>>)
      assert :eq = Values.compare(<<0, 255>>, <<0, 255>>)

      assert_raise ArgumentError, fn -> Values.compare("a", 1) end
    end

    @tag obligations: ~w(BM-OBL-004)
    test "content orders: code-point and byte sequences, deterministically" do
      assert :lt = Text.compare(meaning(:text, "abc"), meaning(:text, "abd"))
      assert :gt = Text.compare(meaning(:text, "é"), meaning(:text, "z"))
      assert :eq = Text.compare(meaning(:text, "é"), meaning(:text, "é"))

      assert :lt = Text.compare(meaning(:bytes, <<1>>), meaning(:bytes, <<2>>))
      assert :eq = Text.compare(meaning(:character, ?é), meaning(:character, ?é))
      assert :lt = Text.compare(meaning(:character, ?a), meaning(:character, ?b))

      assert_raise ArgumentError, fn ->
        Text.compare(meaning(:text, "a"), meaning(:bytes, "a"))
      end
    end

    @tag obligations: ~w(BM-OBL-002 BM-OBL-004)
    test "the type classifier admits the three new type atoms" do
      assert Data.comparable_type?(:Text, sample_environment())
      assert Data.comparable_type?(:Character, sample_environment())
      assert Data.comparable_type?(:Bytes, sample_environment())
      assert Data.comparable_type?(:integer, sample_environment())
      refute Data.comparable_type?(:List, sample_environment())
      refute Data.comparable_type?(:Reference, sample_environment())
    end
  end

  describe "absences" do
    @tag obligations: ~w(BM-OBL-005 BM-OBL-006)
    test "collections stay library territory, references excluded, no frontend encodes text" do
      refute function_exported?(Catena, :list_literal, 0)
      refute function_exported?(Catena, :map_type, 0)
      refute function_exported?(Catena, :set_type, 0)
      refute function_exported?(Catena, :ref, 1)

      refute function_exported?(Catena, :compile_text, 1)

      refute "text" in Catena.LanguageVersion.compilable_revisions()
    end
  end

  defp scan_then_elaborate(source) do
    {:ok, %{literal: literal}} = Catena.scan_literal(source)
    Text.elaborate(literal)
  end

  defp meaning(kind, value) do
    type = %{text: :Text, character: :Character, bytes: :Bytes}[kind]
    %Text.Meaning{kind: kind, type: type, value: value}
  end

  defp sample_environment do
    %{}
  end
end
