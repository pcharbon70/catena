defmodule Catena.CallingMetadataTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(CV-OBL-004 CV-OBL-011 CV-OBL-012)
  alias Catena.Calling.{Adapter, Artifact, Descriptor, Frames, Scope}

  test "sidecar and BEAM bind the actual compiler and lowering, retaining source frame provenance" do
    {:ok, core} =
      Catena.check_kernel("""
      (module CallingFrameOrigin (edition 0.1) (revision 0.1.8)
        (origin "test://calling-frame") (export value main)
        (def main (signature Int (uses)) (trap 12)))
      """)

    assert {:ok, artifact} = Artifact.build(core)
    assert {:ok, {_, [compile_info: info]}} = :beam_lib.chunks(artifact.binary, [:compile_info])
    assert info[:catena_calling_descriptor] == artifact.descriptor.digest
    assert artifact.descriptor.compiler_digest == Descriptor.compiler_digest()
    assert {:ok, descriptor} = Catena.Kernel.Interface.calling_descriptor(core)
    assert descriptor == artifact.descriptor
    forms = Catena.Kernel.Backend.lower(core, calling: true)

    assert {:error, _} =
             Catena.OTP.Compiler.compile_calling(forms, %{descriptor | compiler_digest: "forged"})

    assert {:error, _} = Catena.OTP.Compiler.compile_calling(tl(forms), descriptor)

    assert {:module, module} =
             Catena.OTP.Compiler.load(artifact.module, ~c"calling-frame", artifact.binary)

    try do
      stack =
        try do
          apply(module, :main, [])
        catch
          :error, {:catena_trap, 12} -> __STACKTRACE__
        end

      assert {:ok, mapped} = Frames.explain(artifact, core, stack)

      assert Enum.any?(
               mapped,
               &match?(%{source: %{name: "main", origin: "test://calling-frame"}}, &1)
             )

      assert Enum.map(mapped, & &1.technical) == stack

      assert {:ok, [%{source: nil}]} =
               Frames.explain(artifact, core, [{:unrelated, :helper, 0, []}])

      assert {:error, _} = Frames.explain(%{artifact | binary_digest: "forged"}, core, stack)
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  test "checked closed handlers preserve effects and reject unhandled external effects before entry" do
    {:ok, core} = File.read!("test/fixtures/c010-kernel.catena") |> Catena.check_kernel()
    {:ok, artifact} = Artifact.build(core)

    try do
      assert {:ok, {2, true, 5}} =
               Adapter.invoke(artifact, core, "main", [], %{nodes: 16, bytes: 64})

      Scope.run(artifact, core, %{nodes: 16, bytes: 64}, fn scope ->
        assert {:ok, {2, true, 5}} = Scope.entry(scope, "main")
        assert {:error, :unadmitted_initial_effect} = Scope.entry(scope, "launch")
      end)
    after
      :code.purge(artifact.module)
      :code.delete(artifact.module)
      :code.purge(artifact.module)
    end
  end

  test "the 253 explicit argument floor retains private 255 argument CPS workers" do
    parameters = Enum.map(1..253, &"arg#{&1}")

    signature =
      parameters
      |> Enum.with_index()
      |> Enum.reduce("Int", fn {_, index}, rest ->
        effects = if index == 0, do: "(effects Ask)", else: "(effects)"
        "(Fn Int #{effects} #{rest})"
      end)

    body =
      Enum.reduce(Enum.reverse(parameters), "(request Ask ask)", fn parameter, body ->
        "(fn (#{parameter} Int) #{body})"
      end)

    {:ok, core} =
      Catena.check_kernel("""
      (module CallingWide (edition 0.1) (revision 0.1.8) (origin "test://calling-wide")
        (export value wide) (effect Ask (operation ask (params) Int))
        (def wide (signature #{signature} (uses)) #{body}))
      """)

    assert {:ok, artifact} = Artifact.build(core)
    assert %{beam_arity: 253, stages: stages} = hd(artifact.descriptor.entries)
    assert length(stages) == 253
    assert List.last(stages).effects == [effect: "Ask"]

    assert Enum.any?(
             artifact.descriptor.functions,
             &match?(
               %{arity: 255, exported: false, source: %{name: "wide", hidden_arguments: 2}},
               &1
             )
           )

    Scope.run(artifact, core, %{nodes: 16, bytes: 64}, fn scope ->
      assert {:error, _} = Scope.entry(scope, "wide")
    end)

    :code.delete(artifact.module)
    :code.purge(artifact.module)
  end

  test "deep tail recursion passes through both saturated and partial checked calls" do
    {:ok, core} = File.read!("test/fixtures/c010-kernel.catena") |> Catena.check_kernel()
    {:ok, artifact} = Artifact.build(core)

    try do
      limits = %{nodes: 16, bytes: 64}
      assert {:ok, 1_000_000} = Adapter.invoke(artifact, core, "loop", [1_000_000, 0], limits)

      Scope.run(artifact, core, limits, fn scope ->
        assert {:ok, loop} = Scope.entry(scope, "loop")
        assert {:ok, start} = Scope.call(scope, loop, 1_000_000)
        assert {:ok, 1_000_000} = Scope.call(scope, start, 0)
      end)
    after
      :code.purge(artifact.module)
      :code.delete(artifact.module)
      :code.purge(artifact.module)
    end
  end
end
