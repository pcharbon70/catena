defmodule Catena.ValueBoundaryTest do
  use ExUnit.Case, async: false

  test "both independent verifiers reject forged primitive payloads despite trusted type annotations" do
    for {tag, value, wrong, spelling} <- [
          {"integer", 1, false, "Int"},
          {"boolean", true, 1, "Bool"}
        ] do
      source =
        JSON.encode!(%{
          "version" => "0.1.1",
          "module" => "ValueBoundaryProbe",
          "exports" => [],
          "definitions" => [
            %{"name" => "main", "parameters" => [], "body" => %{"tag" => tag, "value" => value}}
          ]
        })

      {:ok, core} = Catena.check_json(source)
      [definition] = core.definitions
      corrupted = %{definition | expression: %{definition.expression | value: wrong}}
      assert {:error, _} = Catena.TypedCore.Verifier.verify(%{core | definitions: [corrupted]})

      literal = if value == true, do: "true", else: "1"

      {:ok, kernel} =
        Catena.check_kernel("""
        (module KernelValueBoundary (edition 0.1) (revision 0.1.8) (origin "test://value-boundary")
          (def main (signature #{spelling} (uses)) #{literal}))
        """)

      [definition] = kernel.definitions
      corrupted = %{definition | expression: %{definition.expression | value: wrong}}
      assert {:error, _} = Catena.Kernel.Verifier.verify(%{kernel | definitions: [corrupted]})
    end
  end

  test "ordinary nominal layouts round-trip to the independent semantic observation" do
    source = File.read!("test/fixtures/c002-option.catena.json")
    {:ok, core} = Catena.check_json(source)
    {:ok, semantic} = Catena.Reference.Evaluator.run(core, "make")

    for layout <- [:uniform, :compact] do
      {:ok, description} = Catena.ValueBoundary.Nominal.describe(core, "make", layout)
      {:ok, module, binary, _} = Catena.compile_json(source, layout: layout)
      {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"value-boundary.beam", binary)
      native = apply(module, :make, [])

      assert {:ok, ^semantic} =
               Catena.ValueBoundary.Nominal.decode(description, native, %{nodes: 10, bytes: 100})

      assert {:ok, ^native} =
               Catena.ValueBoundary.Nominal.encode(description, semantic, %{nodes: 10, bytes: 100})

      none = {:catena_value, "test://c002-conformance::C002Fixture::Option::None", []}

      assert {:ok, empty_native} =
               Catena.ValueBoundary.Nominal.encode(description, none, %{nodes: 10, bytes: 100})

      assert {:ok, ^none} =
               Catena.ValueBoundary.Nominal.decode(description, empty_native, %{
                 nodes: 10,
                 bytes: 100
               })

      if layout == :compact do
        assert {:error, _} =
                 Catena.ValueBoundary.Nominal.decode(description, {empty_native}, %{
                   nodes: 10,
                   bytes: 100
                 })
      end

      assert {:error, :validation_budget_exhausted} =
               Catena.ValueBoundary.Nominal.decode(description, native, %{nodes: 1, bytes: 100})

      assert {:error, _} =
               Catena.ValueBoundary.Nominal.decode(%{description | type: :boolean}, native, %{
                 nodes: 10,
                 bytes: 100
               })

      assert {:error, _} =
               Catena.ValueBoundary.Nominal.encode(
                 description,
                 {:catena_value, "other::Some", [7]},
                 %{nodes: 10, bytes: 100}
               )

      assert {:error, _} =
               Catena.ValueBoundary.Nominal.encode(
                 description,
                 put_elem(semantic, 2, [false]),
                 %{nodes: 10, bytes: 100}
               )

      for malformed <- [
            :unknown_tag,
            {:catena_adt, :wrong, 1, {7}},
            {elem(native, 0), false},
            make_ref()
          ] do
        assert {:error, _} =
                 Catena.ValueBoundary.Nominal.decode(description, malformed, %{
                   nodes: 10,
                   bytes: 100
                 })
      end

      :code.purge(module)
      :code.delete(module)
    end
  end

  test "checked scalar and structural lowering agrees across both backend owners" do
    alias Catena.ValueBoundary.Data
    limits = %{nodes: 100, bytes: 4096}

    type =
      {:record,
       %{
         "payload" =>
           {:variant,
            %{"ready" => {:tuple, [:integer, :float, :text, :bytes, :character, :unit, :boolean]}}}
       }}

    large = Integer.pow(2, 4096)

    semantic = %{
      "payload" => {:catena_variant, "ready", {large, -0.0, "é🙂", <<>>, 0x10FFFF, :unit, true}}
    }

    assert {:ok, native} = Data.encode(type, semantic, limits)
    assert {:ok, ^semantic} = Data.decode(type, native, limits)

    for backend <- [Catena.Backend.ErlangAbstract, Catena.Kernel.Backend] do
      assert {:ok, expression} = backend.lower_boundary_value(type, semantic, limits)

      forms = [
        {:attribute, 1, :module, ValueDataProbe},
        {:attribute, 1, :export, [result: 0]},
        {:function, 1, :result, 0, [{:clause, 1, [], [], [expression]}]}
      ]

      assert {:ok, module, binary, _} =
               Catena.OTP.Compiler.compile(forms,
                 specification: "experimental/value-boundary",
                 frontend: "experimental/value-boundary"
               )

      assert {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"value-data.beam", binary)
      assert ^native = apply(module, :result, [])
      assert <<128, 0, 0, 0, 0, 0, 0, 0>> = <<elem(elem(native.payload, 2), 1)::float-64>>
      :code.purge(module)
      :code.delete(module)
    end

    assert {:error, :validation_budget_exhausted} =
             Data.encode(type, semantic, %{limits | bytes: 10})

    assert {:error, :validation_budget_exhausted} =
             Data.decode(type, native, %{limits | nodes: 2})

    assert {:ok, %{}} = Data.encode({:record, %{}}, %{}, limits)
    assert {:ok, %{}} = Data.decode({:record, %{}}, %{}, limits)

    assert {:error, :payload_type_mismatch} =
             Data.decode(type, Map.put(native, :extra, 1), limits)

    assert {:error, :payload_type_mismatch} = Data.decode(type, semantic, limits)
  end

  test "data ingress rejects malformed scalars, untrusted handles and exhausted budgets" do
    alias Catena.ValueBoundary.Data
    limits = %{nodes: 20, bytes: 64}

    for {type, malformed} <- [
          {:integer, true},
          {:boolean, 1},
          {:float, 1},
          {:text, <<255>>},
          {:bytes, <<1::1>>},
          {:character, 0xD800},
          {:character, 0x110000},
          {:unit, nil},
          {:text, make_ref()},
          {:integer, self()},
          {:integer, fn -> 1 end},
          {{:record, %{"x" => :integer}}, %{"x" => 1}}
        ] do
      assert {:error, :payload_type_mismatch} = Data.decode(type, malformed, limits)
    end

    assert {:ok, <<255>>} = Data.decode(:bytes, <<255>>, limits)
    assert {:ok, ""} = Data.decode(:text, "", %{nodes: 1, bytes: 0})
    assert {:error, :validation_budget_exhausted} = Data.decode(:text, "a", %{nodes: 1, bytes: 0})

    assert {:error, :invalid_boundary_type} =
             Data.decode({:function, :integer, :integer}, fn x -> x end, limits)

    assert {:error, :invalid_validation_budget} = Data.decode(:integer, 1, %{nodes: 0, bytes: 1})

    assert {:error, :invalid_boundary_description} =
             Catena.ValueBoundary.Nominal.decode(%{}, 1, %{nodes: 10, bytes: 100})

    assert {:error, :invalid_boundary_description} =
             Catena.ValueBoundary.Nominal.describe(%{}, "x", :compact)

    {deep_type, deep_value} =
      Enum.reduce(1..50, {:integer, 1}, fn _, {t, v} -> {{:tuple, [t]}, {v}} end)

    assert {:error, :validation_budget_exhausted} = Data.decode(deep_type, deep_value, limits)
    assert {:ok, ^deep_value} = Data.decode(deep_type, deep_value, %{nodes: 51, bytes: 1})
  end

  test "the descriptive value classifier does not bless malformed text meanings or crash on absent payloads" do
    for kind <- [:integer, :boolean, :float] do
      refute Catena.Values.value?(%{tag: kind})
    end

    for value <- [
          %Catena.Text.Meaning{kind: :text, type: :Text, value: <<255>>},
          %Catena.Text.Meaning{kind: :bytes, type: :Text, value: "abc"},
          %Catena.Text.Meaning{kind: :character, type: :Character, value: 0xD800}
        ] do
      refute Catena.Values.value?(value)
      refute Catena.Values.comparable?(value)
      refute Catena.Values.orderable?(value)
    end

    assert Catena.Values.value?(%Catena.Text.Meaning{kind: :text, type: :Text, value: ""})
    assert Catena.Values.value?(%Catena.Text.Meaning{kind: :bytes, type: :Bytes, value: <<255>>})
  end

  test "scanned and elaborated Float/Text/Character/Bytes meanings reach checked BEAM literals" do
    for {source, type} <- [
          {"1.125", :float},
          {~s("héllo"), :text},
          {"'é'", :character},
          {~s(b""), :bytes}
        ] do
      assert {:ok, %{literal: literal}} = Catena.scan_literal(source)

      meaning =
        case type do
          :float ->
            assert {:ok, result} = Catena.Numeric.elaborate(literal.payload)
            result

          _ ->
            assert {:ok, result} = Catena.Text.elaborate(literal)
            result
        end

      limits = %{nodes: 1, bytes: 64}
      assert {:ok, form} = Catena.Kernel.Backend.lower_boundary_value(type, meaning.value, limits)
      assert {:value, native, _} = :erl_eval.expr(form, :erl_eval.new_bindings())
      assert {:ok, decoded} = Catena.ValueBoundary.Data.decode(type, native, limits)
      assert decoded === meaning.value
    end
  end

  test "captured closures retain semantic products across both nominal layout choices" do
    int = %{"tag" => "integer"}
    fun = fn a, b -> %{"tag" => "function", "parameter" => a, "result" => b} end
    variable = fn name -> %{"tag" => "variable", "name" => name} end

    source =
      JSON.encode!(%{
        "version" => "0.1.1",
        "module" => "CapturedBoundary",
        "exports" => ["capture"],
        "definitions" => [
          %{
            "name" => "capture",
            "parameters" => ["x"],
            "signature" => %{
              "forall" => [],
              "type" => fun.(int, fun.(int, %{"tag" => "tuple", "elements" => [int, int]}))
            },
            "body" => %{
              "tag" => "function",
              "parameter" => "y",
              "body" => %{"tag" => "tuple", "elements" => [variable.("x"), variable.("y")]}
            }
          }
        ]
      })

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, {41, 7}} = Catena.Reference.Evaluator.run(core, "capture", [41, 7])

    for layout <- [:compact, :uniform] do
      assert {:ok, module, binary, _} = Catena.compile_json(source, layout: layout)

      assert {:module, ^module} =
               Catena.OTP.Compiler.load(module, ~c"capture-boundary.beam", binary)

      closure = apply(module, :capture, [41])
      assert is_function(closure, 1)
      assert {41, 7} = closure.(7)
      assert {41, 9} = closure.(9)
      limits = %{nodes: 3, bytes: 2}

      assert {:ok, {41, 7}} =
               Catena.ValueBoundary.Data.decode(
                 {:tuple, [:integer, :integer]},
                 closure.(7),
                 limits
               )

      assert {:error, :invalid_boundary_type} =
               Catena.ValueBoundary.Data.decode({:function, :integer, :integer}, closure, limits)

      :code.purge(module)
      :code.delete(module)
    end
  end

  test "integrated scalar carriers are typed, verified, stepped and compiled without widening retained revisions" do
    alias Catena.Kernel.{Parser, Stepper, Verifier, Backend}

    assert {:ok, parsed} =
             Parser.parse("""
             (module IntegratedValueBoundary (edition 0.1) (revision 0.1.8)
               (origin "test://integrated-value-boundary") (export value main)
               (def main (signature Int (uses)) 1))
             """)

    [main] = parsed.definitions

    for {type, value} <- [
          {:float, -0.0},
          {:text, "é🙂"},
          {:character, 0x10FFFF},
          {:bytes, <<0, 255>>}
        ] do
      expression = %{main.expression | tag: type, value: value}
      changed = %{parsed | definitions: [%{main | signature: type, expression: expression}]}
      assert {:ok, core} = Catena.ValueBoundary.Kernel.check(changed)
      assert :ok = Verifier.verify(core)
      assert Catena.Values.value?(hd(core.definitions).expression)
      assert {:ok, ^value, _} = Stepper.run(core, "main")
      assert {:ok, module, binary, _} = Backend.compile(core)

      assert {:module, ^module} =
               Catena.OTP.Compiler.load(module, ~c"integrated-value.beam", binary)

      assert value === apply(module, :main, [])
      :code.purge(module)
      :code.delete(module)
      assert {:error, _} = Verifier.verify(%{core | version: "0.1.8"})
      [typed] = core.definitions
      bad = %{typed | expression: %{typed.expression | value: make_ref()}}
      assert {:error, _} = Verifier.verify(%{core | definitions: [bad]})
      assert {:error, _} = Catena.Kernel.Checker.check(changed)
    end
  end

  test "integrated Text values survive typed closure capture and structural transport" do
    alias Catena.Kernel.{Parser, Stepper, Verifier, Backend}

    assert {:ok, parsed} =
             Parser.parse("""
             (module CapturedTextBoundary (edition 0.1) (revision 0.1.8)
               (origin "test://captured-text-boundary") (export value main)
               (def main (signature (Tuple Int Int) (uses))
                 (call (fn (captured Int)
                   (call (fn (argument Int) (tuple (var captured) (var argument))) 2)) 1)))
             """)

    changed = text_tree(parsed)
    assert {:ok, core} = Catena.ValueBoundary.Kernel.check(changed)
    assert :ok = Verifier.verify(core)
    assert {:ok, {"captured", "argument"}, _} = Stepper.run(core, "main")
    assert {:ok, module, binary, _} = Backend.compile(core)
    assert {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"captured-text.beam", binary)
    result = apply(module, :main, [])
    assert {"captured", "argument"} = result

    assert {:ok, ^result} =
             Catena.ValueBoundary.Data.decode({:tuple, [:text, :text]}, result, %{
               nodes: 3,
               bytes: 16
             })

    :code.purge(module)
    :code.delete(module)
    assert {:error, _} = Catena.Kernel.Checker.check(changed)
  end

  test "fixed nominal layout preserves qualified identity and nested Text payloads" do
    alias Catena.Kernel.{Parser, Stepper, Backend}
    alias Catena.ValueBoundary.Nominal

    assert {:ok, parsed} =
             Parser.parse("""
             (module FixedTextBoundary (edition 0.1) (revision 0.1.8)
               (origin "test://fixed-text-boundary") (export value main) (export type Option)
               (data Option (params a) (constructor None (fields)) (constructor Some (fields a)))
               (def main (signature (Option Int) (uses)) (construct Some 1)))
             """)

    assert {:ok, core} = Catena.ValueBoundary.Kernel.check(text_tree(parsed))
    assert {:ok, reference, _} = Stepper.run(core, "main")
    assert {:ok, descriptor} = Nominal.describe(core, "main", :fixed)
    limits = %{nodes: 2, bytes: 8}

    expected =
      {:catena_value, "test://fixed-text-boundary::FixedTextBoundary::Option::Some", ["captured"]}

    assert {:ok, ^expected} = Nominal.decode(descriptor, reference, limits)
    assert {:ok, ^reference} = Nominal.encode(descriptor, expected, limits)
    assert {:ok, module, binary, _} = Backend.compile(core)
    assert {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"fixed-text.beam", binary)
    assert ^reference = apply(module, :main, [])

    assert {:error, :validation_budget_exhausted} =
             Nominal.decode(descriptor, reference, %{limits | bytes: 7})

    assert {:error, _} =
             Nominal.encode(descriptor, put_elem(expected, 2, ["captured" | :improper]), limits)

    assert {:error, _} = Nominal.decode(descriptor, {:catena_constructor, :Some, {1}}, limits)

    assert {:error, _} =
             Nominal.decode(descriptor, {:catena_constructor, :Other, {"captured"}}, limits)

    private = %{core | exports: %{core.exports | types: []}}
    assert {:ok, private_descriptor} = Nominal.describe(private, "main", :fixed)
    assert {:error, _} = Nominal.decode(private_descriptor, reference, limits)
    :code.purge(module)
    :code.delete(module)
  end

  test "the value tree enforces decoded literal limits and rejects latent process effects" do
    alias Catena.Kernel.Parser

    assert {:ok, parsed} =
             Parser.parse("""
             (module LiteralLimitBoundary (edition 0.1) (revision 0.1.8)
               (origin "test://literal-limit-boundary") (export value main)
               (def main (signature Int (uses)) 1))
             """)

    [main] = parsed.definitions
    ceiling = Catena.ImplementationLimits.configured(:decoded_literal_bytes)

    for {size, valid} <- [{ceiling, true}, {ceiling + 1, false}] do
      expression = %{main.expression | tag: :text, value: String.duplicate("x", size)}
      tree = %{parsed | definitions: [%{main | signature: :text, expression: expression}]}

      if valid do
        assert {:ok, _} = Catena.ValueBoundary.Kernel.check(tree)
      else
        assert {:error, %{id: "LIM004"}} = Catena.ValueBoundary.Kernel.check(tree)
      end
    end

    assert {:ok, process_tree} =
             Parser.parse("""
             (module LatentProcessBoundary (edition 0.1) (revision 0.1.8)
               (origin "test://latent-process-boundary") (export value main)
               (def main (signature (Fn (Process Int) (effects Process) Unit) (uses))
                 (fn (target (Process Int)) (send (var target) 1))))
             """)

    assert {:error, _} = Catena.ValueBoundary.Kernel.check(process_tree)
  end

  @tag obligations: ~w(VB-OBL-001 VB-OBL-005)
  test "exact 0.1.58 selection and artifact provenance retain historical interface boundaries" do
    alias Catena.Kernel.Parser
    assert Catena.LanguageVersion.latest() == "0.1.78"
    assert Catena.LanguageVersion.value_frontend_versions() == ["0.1.58"]
    refute "0.1.58" in Catena.LanguageVersion.interface_versions()
    refute "0.1.58" in Catena.LanguageVersion.signed_format_versions()
    assert :error = Catena.Kernel.Type.decode(%{"tag" => "text"})

    assert {:ok, parsed} =
             Parser.parse("""
             (module ValueSelection (edition 0.1) (revision 0.1.8)
               (origin "test://value-selection") (export value main)
               (def main (signature Int (uses)) 1))
             """)

    assert {:error, %{id: "EDN001"}} =
             Catena.ValueBoundary.Kernel.check(parsed,
               language_selection: Catena.LanguageVersion.legacy_selection("0.1.57")
             )

    assert {:ok, core} = Catena.ValueBoundary.Kernel.check(parsed)
    assert core.version == "0.1.58"
    assert {:ok, module, binary, metadata} = Catena.Kernel.Backend.compile(core)
    assert metadata.artifact_version == "0.1.58"
    assert metadata.interface == nil
    assert {:ok, {^module, [compile_info: info]}} = :beam_lib.chunks(binary, [:compile_info])
    assert info[:catena_specification] == ~c"0.1.58"
    assert info[:catena_frontend] == ~c"value-tree-0.1.58"
    assert info[:catena_language_revision] == ~c"0.1.58"
    assert {:ok, ^module, ^binary, _} = Catena.Kernel.Backend.compile(core)
  end

  defp text_tree(%{tag: :integer, value: value} = expression),
    do: %{expression | tag: :text, value: if(value == 1, do: "captured", else: "argument")}

  defp text_tree(%_{} = value), do: value

  defp text_tree(value) when is_map(value),
    do: Map.new(value, fn {key, item} -> {key, text_tree(item)} end)

  defp text_tree(value) when is_list(value), do: Enum.map(value, &text_tree/1)

  defp text_tree(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.map(&text_tree/1) |> List.to_tuple()

  defp text_tree(:integer), do: :text
  defp text_tree(value), do: value
end
