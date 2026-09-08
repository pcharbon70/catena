defmodule Catena.ResourceHandleTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Stepper, Verifier, Backend}
  alias Catena.Resource.{Kernel, Runtime}

  @tag obligations: ~w(RS-OBL-006)
  test "a checked read copies the immutable payload while the scoped handle remains local" do
    assert {:ok, core} = Kernel.check(fixture(:read), %{})
    assert {:ok, 7, _} = Stepper.run(core, "main")

    assert {:ok, module, binary, _} =
             Backend.compile(core)

    assert {:module, ^module} = :code.load_binary(module, ~c"resource-read.beam", binary)

    try do
      assert apply(module, :main, []) == 7
    after
      :code.delete(module)
      :code.purge(module)
    end

    [main] = core.definitions

    forged = %{
      core
      | definitions: [%{main | expression: %{main.expression | resource_id: "forged"}}]
    }

    assert {:error, _} = Verifier.verify(forged)
  end

  @tag obligations: ~w(RS-OBL-006)
  test "direct handle escape and hidden closure capture reject before execution" do
    assert {:error, %{id: "T002"}} = Kernel.check(fixture(:escape), %{})
    assert {:error, %{id: "T002", message: message}} = Kernel.check(fixture(:capture), %{})
    assert message =~ "captured"

    assert {:error, %{id: "T002", message: container_message}} =
             Kernel.check(fixture(:container_capture), %{})

    assert container_message =~ "captured"
  end

  @tag obligations: ~w(RS-OBL-006)
  test "runtime ownership and released-state checks defend the internal boundary" do
    parent = self()

    returned =
      Runtime.run(
        7,
        fn _ -> send(parent, :released) end,
        fn finish, handle ->
          assert Runtime.read(handle) == 7

          {pid, monitor} =
            spawn_monitor(fn ->
              result =
                try do
                  Runtime.read(handle)
                catch
                  :error, reason -> reason
                end

              send(parent, {:cross_owner, result})
            end)

          assert_receive {:cross_owner, {:catena_trap, :invalid_resource_owner}}
          assert_receive {:DOWN, ^monitor, :process, ^pid, :normal}
          finish.({:ok, 7})
          finish.({:ok, 7})
          assert catch_error(Runtime.read(handle)) == {:catena_trap, :invalid_resource_owner}
          handle
        end,
        1_000_000_000
      )

    assert_receive :released
    refute_receive :released, 0
    assert catch_error(Runtime.read(returned)) == {:catena_trap, :invalid_resource_owner}
  end

  @tag obligations: ~w(RS-OBL-007)
  test "owned cancellation unwinds the scope and a release failure remains terminal" do
    for {mode, expected} <- [
          {:cancel, {:cancelled, 12}},
          {:cancel_release_failure, {:trap, {:mandatory_release_failed, 88}}},
          {:exit, {:exited, 12}},
          {:exit_release_failure, {:trap, {:mandatory_release_failed, 88}}}
        ] do
      assert {:ok, core} = Kernel.check(fixture(mode), %{})

      case expected do
        {:cancelled, reason} -> assert {:cancelled, ^reason, _} = Stepper.run(core, "main")
        {:exited, reason} -> assert {:exited, ^reason, _} = Stepper.run(core, "main")
        {:trap, reason} -> assert {:trap, ^reason, _} = Stepper.run(core, "main")
      end

      assert {:ok, module, binary, _} =
               Backend.compile(core)

      assert {:module, ^module} = :code.load_binary(module, ~c"resource-cancel.beam", binary)

      try do
        outcome =
          try do
            {:ok, apply(module, :main, [])}
          catch
            :exit, {:catena_resource_exit, reason} -> {:exited, reason}
            :throw, {:catena_resource_cancelled, reason} -> {:cancelled, reason}
            :error, {:catena_trap, reason} -> {:trap, reason}
          end

        assert outcome == expected
      after
        :code.delete(module)
        :code.purge(module)
      end
    end
  end

  @tag obligations: ~w(RS-OBL-009)
  test "forced external owner death does not claim cleanup" do
    parent = self()

    {owner, monitor} =
      spawn_monitor(fn ->
        Runtime.run(
          7,
          fn _ -> send(parent, :unexpected_release) end,
          fn _, _ ->
            send(parent, :resource_active)

            receive do
              :never -> :ok
            end
          end,
          1_000_000_000
        )
      end)

    assert_receive :resource_active
    Process.exit(owner, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^owner, :killed}
    refute_receive :unexpected_release, 0
  end

  @tag obligations: ~w(RS-OBL-005)
  test "a typed failure value remains a value after mandatory cleanup" do
    assert {:ok, core} = Kernel.check(fixture(:typed_failure), %{})
    expected = {:catena_constructor, :Failure, {7}}
    assert {:ok, ^expected, _} = Stepper.run(core, "main")
    assert {:ok, module, binary, _} = Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"resource-value-failure.beam", binary)

    try do
      assert apply(module, :main, []) == expected
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  defp fixture(mode) do
    {signature, body} =
      if mode in [:capture, :container_capture],
        do: {"(Fn Unit (effects) Int)", "(fn (ignored Unit) (var held))"},
        else: {"Int", "(var held)"}

    body =
      if mode == :container_capture,
        do: "(let box (tuple (var held)) (fn (ignored Unit) (sequence (var box) 1)))",
        else: body

    {signature, body} =
      if mode == :typed_failure,
        do: {"(Outcome Int)", "(construct Failure (var held))"},
        else: {signature, body}

    source = """
    (module ResourceHandle (edition 0.1) (revision 0.1.8) (origin "test://resource/handle")
      (export value main)
      (export type Outcome)
      (data Outcome (params a) (constructor Failure (fields a)))
      (def acquire (signature Int (uses)) 7)
      (def release (signature (Fn Int (effects) Unit) (uses)) (fn (value Int) (unit)))
      (def main (signature #{signature} (uses)) #{body}))
    """

    {:ok, parsed} = Parser.parse(source)
    [acquire, release, main] = parsed.definitions
    read = fn variable -> %{tag: :resource_read, resource: variable, span: variable.span} end

    body =
      case mode do
        :read ->
          read.(main.expression)

        :typed_failure ->
          %{main.expression | arguments: Enum.map(main.expression.arguments, read)}

        :capture ->
          %{main.expression | body: read.(main.expression.body)}

        mode when mode in [:escape, :container_capture] ->
          main.expression

        mode when mode in [:cancel, :cancel_release_failure, :exit, :exit_release_failure] ->
          %{
            tag:
              if(mode in [:exit, :exit_release_failure],
                do: :resource_exit,
                else: :resource_cancel
              ),
            resource: main.expression,
            reason: %{acquire.expression | value: 12},
            span: main.span
          }
      end

    release =
      if mode in [:cancel_release_failure, :exit_release_failure] do
        reason = %{acquire.expression | value: 88}

        %{
          release
          | expression: %{
              release.expression
              | body: %{tag: :trap, expression: reason, span: main.span}
            }
        }
      else
        release
      end

    scope = %{
      tag: :resource_scope,
      binder: "held",
      acquire: acquire.expression,
      release: release.expression,
      body: body,
      grace_ns: 1_000_000_000,
      span: main.span
    }

    %{parsed | definitions: [%{main | expression: scope}]}
  end
end
