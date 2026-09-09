defmodule Catena.TextProgramTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(TB-OBL-001 TB-OBL-002 TB-OBL-009 TB-OBL-010 TB-OBL-011)
  alias Catena.Standard.Text, as: T
  alias Catena.Standard.Text.Program
  alias Catena.Standard.Outcomes, as: O
  @limits %{nodes: 10000, bytes: 100_000, depth: 1000}
  defp tree(type, name) do
    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module #{name} (edition 0.1) (revision 0.1.8) (origin "test://text-program/#{name}")
        (export value work)
        (def work (signature (Fn Int (effects) Int) (uses)) (fn (value Int) (var value))))
      """)

    {:ok, core} = Catena.ValueBoundary.Kernel.check(replace(parsed, type))
    core
  end

  defp replace(:integer, type), do: type
  defp replace(%Catena.SourceSpan{} = span, _), do: span

  defp replace(map, type) when is_map(map),
    do: Map.new(map, fn {k, v} -> {k, replace(v, type)} end)

  defp replace(tuple, type) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&replace(&1, type)) |> List.to_tuple()

  defp replace(list, type) when is_list(list), do: Enum.map(list, &replace(&1, type))
  defp replace(value, _), do: value

  defp unload(module),
    do:
      (
        :code.delete(module)
        :code.purge(module)
      )

  test "retained pure value trees execute typed pipelines with exact rebuilt sidecars" do
    core = tree(:text, "TextPipeline")
    {:ok, first} = T.index(:grapheme, 0)
    {:ok, last} = T.index(:grapheme, 1)

    steps = [
      {:encode, :utf16_le},
      {:decode, :utf16_le},
      {:normalize, :nfc},
      {:slice, first, last}
    ]

    assert {:ok, artifact} = Catena.Text.compile_operations(core, "work", steps, @limits)
    assert artifact.sidecar.profile == T.profile()
    assert artifact.sidecar.failure == T.failure_schema()

    try do
      for _ <- 1..3 do
        assert {:ok, result} = Program.invoke(artifact, core, "work", steps, "a\u0301😀", @limits)
        assert result == O.success("á")
      end

      assert {:error, _} = Program.invoke(artifact, core, "work", steps, <<0xFF>>, @limits)
      changed = put_in(artifact.sidecar.profile.unicode, "16.0.0")

      assert {:error, :unverified_text_program} =
               Program.verify(changed, core, "work", steps, @limits)

      assert {:error, :unverified_text_program} =
               Program.verify(artifact, core, "work", [], @limits)

      {:ok, other} = Program.build(core, "work", [{:measure, :scalar}], @limits)

      assert {:error, :text_module_conflict} =
               Program.invoke(other, core, "work", [{:measure, :scalar}], "abc", @limits)
    after
      unload(artifact.module)
    end
  end

  test "ill-typed or mixed-unit steps fail before compilation while encoding failure short-circuits" do
    core = tree(:bytes, "BytesPipeline")

    assert {:error, :ill_typed_text_step} =
             Program.build(core, "work", [{:normalize, :nfc}], @limits)

    {:ok, first} = T.index(:byte, 0)
    {:ok, last} = T.index(:scalar, 1)

    assert {:error, :mixed_or_invalid_text_indices} =
             Program.build(core, "work", [{:slice_bytes, first, last}], @limits)

    steps = [{:decode, :utf8}, {:measure, :grapheme}]
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      assert {:ok, failure} = Program.invoke(artifact, core, "work", steps, <<97, 0xFF>>, @limits)
      assert failure == O.failure(T.failure({:malformed_encoding, 1}))
    after
      unload(artifact.module)
    end
  end

  test "ordinary retained JSON functions execute source computation before text formatting" do
    source =
      JSON.encode!(%{
        "version" => "0.1.3",
        "origin" => "test://ordinary-text-pipeline",
        "module" => "OrdinaryTextPipeline",
        "exports" => ["work"],
        "definitions" => [
          %{
            "name" => "work",
            "parameters" => ["value"],
            "signature" => %{
              "forall" => [],
              "type" => %{
                "tag" => "function",
                "parameter" => %{"tag" => "integer"},
                "result" => %{"tag" => "integer"}
              }
            },
            "body" => %{
              "tag" => "binary",
              "operator" => "add",
              "left" => %{"tag" => "variable", "name" => "value"},
              "right" => %{"tag" => "integer", "value" => 1}
            }
          }
        ]
      })

    {:ok, core} = Catena.check_json(source)
    steps = [:format_integer, {:measure, :scalar}]
    assert {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      assert {:ok, result} = Program.invoke(artifact, core, "work", steps, 99, @limits)
      assert result == O.success(3)
    after
      unload(artifact.module)
    end
  end

  test "ordinary nominal indices preserve units through explicit descriptor conversion" do
    alias Catena.Standard.Text.Indices
    assert {:ok, module, binary, metadata} = Indices.compile()
    assert :ok = Catena.TypedCore.Verifier.verify(metadata.core)
    assert {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"text-indices", binary)

    try do
      for unit <- [:byte, :scalar, :grapheme] do
        native = apply(module, String.to_existing_atom("#{unit}_index"), [3])
        assert apply(module, String.to_existing_atom("#{unit}_offset"), [native]) == 3
        assert {:ok, descriptor} = Indices.from_nominal(native)
        assert descriptor.unit == unit
        assert descriptor.offset == 3
      end

      ast = Indices.package!()["ast"]
      [first | rest] = ast["definitions"]
      forged = put_in(first["body"]["constructor"], "ScalarIndex.At")

      assert {:error, _} =
               Catena.check_json(JSON.encode!(%{ast | "definitions" => [forged | rest]}))

      assert {:error, :invalid_text_package} =
               Indices.verify_package(Map.put(Indices.package!(), "contract", "0.1.65"))
    after
      unload(module)
    end
  end

  test "typed formatting and binary matching execute through the compiled pipeline" do
    core = tree({:tuple, [:text, :character, :integer, :bytes]}, "FormatTextPipeline")
    steps = [{:format, [:text, :character, :integer, :bytes_hex]}]
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      assert {:ok, result} =
               Program.invoke(artifact, core, "work", steps, {"x", 0x1F600, -1, <<255>>}, @limits)

      assert result == O.success("x😀-1ff")

      assert {:error, :ill_typed_text_step} =
               Program.build(
                 core,
                 "work",
                 [{:format, [:text, :text, :integer, :bytes_hex]}],
                 @limits
               )
    after
      unload(artifact.module)
    end

    core = tree(:bytes, "PatternTextPipeline")
    {:ok, pattern} = Catena.Standard.Binary.Pattern.describe([{:integer, 16, :unsigned, :big}])
    steps = [{:match, pattern}]
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      assert {:ok, result} = Program.invoke(artifact, core, "work", steps, <<1, 2>>, @limits)
      assert result == O.success(O.present({258}))
    after
      unload(artifact.module)
    end
  end

  test "pipeline caps and source effects cannot bypass the explicit profile" do
    core = tree(:text, "TextPipelineLimit")
    steps = List.duplicate({:normalize, :nfc}, 253)
    assert {:ok, _} = Program.build(core, "work", steps, @limits)

    assert {:error, :invalid_text_program} =
             Program.build(core, "work", steps ++ [{:normalize, :nfc}], @limits)

    {:ok, effectful} =
      Catena.check_kernel("""
      (module TextEffectRefusal (edition 0.1) (revision 0.1.8) (origin "test://text-effect")
        (export value work) (effect Ask (operation ask (params) Int))
        (def work (signature (Fn Int (effects Ask) Int) (uses)) (fn (value Int) (request Ask ask))))
      """)

    assert {:error, :unsupported_text_entry} =
             Program.build(effectful, "work", [:format_integer], @limits)

    {:ok, trapped} =
      Catena.check_kernel("""
      (module TextTrapPreservation (edition 0.1) (revision 0.1.8) (origin "test://text-trap")
        (export value work)
        (def work (signature (Fn Int (effects) Int) (uses)) (fn (value Int) (trap 41))))
      """)

    {:ok, artifact} = Program.build(trapped, "work", [:format_integer], @limits)

    try do
      assert {:catena_trap, 41} =
               catch_error(
                 Program.invoke(artifact, trapped, "work", [:format_integer], 0, @limits)
               )
    after
      unload(artifact.module)
    end
  end
end
