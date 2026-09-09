defmodule Catena.CallingScopeTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(CV-OBL-006 CV-OBL-007)
  alias Catena.Calling.{Adapter, Artifact, Scope}
  @limits %{nodes: 16, bytes: 64}

  test "kernel partial calls preserve captures and agree with the saturated wrapper" do
    {core, artifact} =
      kernel(
        "CallingScopedCapture",
        """
        (def capture (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
          (fn (x Int) (let saved (add (var x) 1) (fn (y Int) (add (var saved) (var y))))))
        """,
        "capture"
      )

    Scope.run(artifact, core, @limits, fn scope ->
      assert {:ok, entry} = Scope.entry(scope, "capture")
      assert {:ok, first} = Scope.call(scope, entry, 10)
      assert {:ok, second} = Scope.call(scope, entry, 20)
      assert {:ok, 42} = Scope.call(scope, first, 31)
      assert {:ok, 52} = Scope.call(scope, second, 31)
      assert {:ok, 42} = Scope.call(scope, first, 31)
    end)

    assert {:ok, 42} = Adapter.invoke(artifact, core, "capture", [10, 31], @limits)
  end

  test "an intermediate trap occurs at the first application, not deferred to saturation" do
    {core, artifact} =
      kernel(
        "CallingScopedTrap",
        """
        (def staged (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
          (fn (x Int) (sequence (trap 7) (fn (y Int) (var y)))))
        """,
        "staged"
      )

    Scope.run(artifact, core, @limits, fn scope ->
      assert {:ok, entry} = Scope.entry(scope, "staged")

      assert {:error, {:execution_failure, :error, {:catena_trap, 7}}} =
               Scope.call(scope, entry, 10)
    end)
  end

  test "wrong data never enters a stage and forged, cross-owner, revoked and expired handles fail" do
    {core, artifact} =
      kernel(
        "CallingScopedOwnership",
        """
        (def trap (signature (Fn Int (effects) Int) (uses)) (fn (x Int) (trap 9)))
        """,
        "trap"
      )

    {scope, entry} =
      Scope.run(artifact, core, @limits, fn scope ->
        assert {:ok, entry} = Scope.entry(scope, "trap")
        assert {:error, _} = Scope.call(scope, entry, false)
        assert {:error, :invalid_call_handle} = Scope.call(scope, fn x -> x end, 1)

        assert {:error, :invalid_call_handle} =
                 Scope.call(scope, {Scope, make_ref(), make_ref()}, 1)

        assert {:error, :invalid_call_scope_owner} =
                 Task.async(fn -> Scope.call(scope, entry, 1) end) |> Task.await()

        assert :ok = Scope.revoke(scope, entry)
        assert {:error, :invalid_call_handle} = Scope.call(scope, entry, 1)
        {scope, entry}
      end)

    assert {:error, :expired_call_scope} = Scope.call(scope, entry, 1)
  end

  test "handle bounds are enforced before executing a stage and cleanup survives exceptions" do
    {core, artifact} =
      kernel(
        "CallingScopedLimits",
        """
        (def capture (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
          (fn (x Int) (sequence (trap 7) (fn (y Int) (var y)))))
        """,
        "capture"
      )

    assert :thrown ==
             catch_throw(
               Scope.run(
                 artifact,
                 core,
                 @limits,
                 fn scope ->
                   send(self(), {:scope, scope})
                   assert {:ok, entry} = Scope.entry(scope, "capture")
                   assert {:error, :call_handle_limit} = Scope.call(scope, entry, 1)
                   assert :ok = Scope.revoke(scope, entry)
                   assert {:ok, _} = Scope.entry(scope, "capture")
                   throw(:thrown)
                 end,
                 max_handles: 1
               )
             )

    assert_receive {:scope, scope}
    assert {:error, :expired_call_scope} = Scope.entry(scope, "capture")
  end

  test "ordinary written-arity calls can return and reuse checked closures" do
    int = %{"tag" => "integer"}
    fn_type = fn a, b -> %{"tag" => "function", "parameter" => a, "result" => b} end

    source =
      JSON.encode!(%{
        "version" => "0.1.1",
        "module" => "CallingOrdinaryClosure",
        "exports" => ["capture"],
        "definitions" => [
          %{
            "name" => "capture",
            "parameters" => ["x"],
            "signature" => %{"forall" => [], "type" => fn_type.(int, fn_type.(int, int))},
            "body" => %{
              "tag" => "function",
              "parameter" => "y",
              "body" => %{"tag" => "variable", "name" => "x"}
            }
          }
        ]
      })

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, artifact} = Artifact.build(core)
    cleanup(artifact.module)

    Scope.run(artifact, core, @limits, fn scope ->
      assert {:ok, entry} = Scope.entry(scope, "capture")
      assert {:ok, partial} = Scope.call(scope, entry, 42)
      assert {:ok, 42} = Scope.call(scope, partial, 1)
      assert {:ok, 42} = Scope.call(scope, partial, 2)
    end)
  end

  test "replacing loaded code invalidates an outstanding scope before entry" do
    {core, artifact} =
      kernel("CallingScopedReplacement", "(def main (signature Int (uses)) 1)", "main")

    {:ok, changed} =
      Catena.check_kernel("""
      (module CallingScopedReplacement (edition 0.1) (revision 0.1.8)
        (origin "test://calling-scope") (export value main) (def main (signature Int (uses)) 2))
      """)

    {:ok, replacement} = Artifact.build(changed)

    Scope.run(artifact, core, @limits, fn scope ->
      assert {:ok, 1} = Scope.entry(scope, "main")

      assert {:module, _} =
               Catena.OTP.Compiler.load(replacement.module, ~c"replacement", replacement.binary)

      assert {:error, :call_artifact_replaced} = Scope.entry(scope, "main")
    end)
  end

  defp kernel(module, definitions, entry) do
    assert {:ok, core} =
             Catena.check_kernel("""
             (module #{module} (edition 0.1) (revision 0.1.8) (origin "test://calling-scope")
               (export value #{entry}) #{definitions})
             """)

    assert {:ok, artifact} = Artifact.build(core)
    cleanup(artifact.module)
    {core, artifact}
  end

  defp cleanup(module) do
    on_exit(fn ->
      :code.purge(module)
      :code.delete(module)
      :code.purge(module)
    end)
  end
end
