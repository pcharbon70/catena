defmodule Catena.CallingCallbackTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(CV-OBL-008 CV-OBL-010)
  alias Catena.Calling.{Artifact, Scope}
  @limits %{nodes: 16, bytes: 64}

  defp artifact do
    {:ok, core} =
      Catena.check_kernel("""
      (module CallingCallback (edition 0.1) (revision 0.1.8) (origin "test://calling-callback")
        (export value increment) (export value invoke) (export value boolean) (export value fail)
        (def increment (signature (Fn Int (effects) Int) (uses))
          (fn (x Int) (add (var x) 1)))
        (def invoke (signature (Fn (Fn Int (effects) Int) (effects) (Fn Int (effects) Int)) (uses))
          (fn (f (Fn Int (effects) Int)) (fn (x Int) (call (var f) (var x)))))
        (def boolean (signature (Fn Bool (effects) Bool) (uses)) (fn (x Bool) (var x)))
        (def fail (signature (Fn Int (effects) Int) (uses)) (fn (x Int) (trap 17))))
      """)

    {:ok, artifact} = Artifact.build(core)

    on_exit(fn ->
      :code.purge(artifact.module)
      :code.delete(artifact.module)
      :code.purge(artifact.module)
    end)

    {core, artifact}
  end

  test "higher-order input accepts only a verified same-scope handle of the exact function type" do
    {core, artifact} = artifact()

    Scope.run(artifact, core, @limits, fn scope ->
      {:ok, increment} = Scope.entry(scope, "increment")
      {:ok, invoke} = Scope.entry(scope, "invoke")
      {:ok, boolean} = Scope.entry(scope, "boolean")
      assert {:error, :callback_type_mismatch} = Scope.call(scope, invoke, fn x -> x end)
      assert {:error, :callback_type_mismatch} = Scope.call(scope, invoke, boolean)
      assert {:ok, applied} = Scope.call(scope, invoke, increment)
      assert {:ok, 42} = Scope.call(scope, applied, 41)

      # Revocation ends this boundary handle, not an immutable value already captured by verified code.
      assert :ok = Scope.revoke(scope, increment)
      assert {:ok, 43} = Scope.call(scope, applied, 42)
    end)
  end

  test "separately compiled Erlang invokes a typed callback and expiry is checked at every ingress" do
    {core, artifact} = artifact()

    forms = [
      {:attribute, 1, :module, :calling_callback_erlang},
      {:attribute, 1, :export, [invoke: 2]},
      {:function, 1, :invoke, 2,
       [
         {:clause, 1, [{:var, 1, :F}, {:var, 1, :X}], [],
          [{:call, 1, {:var, 1, :F}, [{:var, 1, :X}]}]}
       ]}
    ]

    {:ok, fixture, binary, _} =
      Catena.OTP.Compiler.compile(forms, frontend: "erlang-callback-fixture")

    {:module, ^fixture} = Catena.OTP.Compiler.load(fixture, ~c"erlang-callback", binary)

    on_exit(fn ->
      :code.purge(fixture)
      :code.delete(fixture)
      :code.purge(fixture)
    end)

    Scope.run(artifact, core, @limits, fn scope ->
      {:ok, increment} = Scope.entry(scope, "increment")
      assert {:error, :missing_callback_authority} = Scope.callback(scope, increment)
    end)

    callback =
      Scope.run(
        artifact,
        core,
        @limits,
        fn scope ->
          {:ok, increment} = Scope.entry(scope, "increment")
          {:ok, callback} = Scope.callback(scope, increment)
          {:ok, fail} = Scope.entry(scope, "fail")
          {:ok, failing_callback} = Scope.callback(scope, fail)
          assert catch_error(failing_callback.(1)) == {:catena_trap, 17}
          assert apply(fixture, :invoke, [callback, 41]) == 42

          assert {:catena_trap, {:callback_boundary, _}} =
                   catch_error(apply(fixture, :invoke, [callback, false]))

          assert Task.async(fn -> catch_error(callback.(1)) end) |> Task.await() ==
                   {:catena_trap, {:callback_boundary, :invalid_call_scope_owner}}

          callback
        end,
        allow_callbacks: true
      )

    assert catch_error(callback.(41)) == {:catena_trap, {:callback_boundary, :expired_call_scope}}
  end
end
