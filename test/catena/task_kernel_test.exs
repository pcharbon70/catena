defmodule Catena.TaskKernelTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Verifier, Backend}
  alias Catena.Task.{Kernel, Instrument}

  @tag obligations: ~w(OT-OBL-003)
  test "checked task scopes join generated workers without selecting a production revision" do
    assert {:ok, core} = Kernel.check(fixture(:normal), %{})
    assert :ok = Verifier.verify(core)
    assert {:ok, :unit, _} = Catena.Kernel.Stepper.run(core, "main")
    assert :unit = beam(core)
    assert {:error, %{id: "I001"}} = Backend.compile(core)
    assert Catena.LanguageVersion.latest() == "0.1.58"

    for version <- ["0.1.8", "0.1.50", "0.1.51"] do
      forged =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          core,
          &Map.put(&2, &1, version)
        )

      assert {:error, _} = Verifier.verify(forged)
    end
  end

  @tag obligations: ~w(OT-OBL-001)
  test "selected lifetime artifacts are deterministic and exclude general time admission" do
    assert {:ok, core} = Kernel.check_selected(fixture(:normal), %{})
    assert core.version == "0.1.52"
    assert :ok = Verifier.verify(core)

    assert {:ok, module, binary,
            %{artifact_version: "0.1.52", interface: nil, interface_binary: nil}} =
             Backend.compile(core)

    assert {:ok, ^module, ^binary, _} = Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"selected-task-lifetime", binary)

    try do
      assert :unit = apply(module, :main, [])
    after
      :code.purge(module)
      :code.delete(module)
    end

    assert {:ok, :unit, _} = Catena.Kernel.Stepper.run(core, "main")
    parsed = fixture(:normal)
    [spin, worker, main] = parsed.definitions

    sleep = %{
      tag: :task_sleep,
      scope: variable("tasks", main.span),
      duration: %{tag: :integer, value: 0, span: main.span},
      span: main.span
    }

    altered = %{main | expression: %{main.expression | body: sleep}}

    assert {:error, %{id: "T002"}} =
             Kernel.check_selected(%{parsed | definitions: [spin, worker, altered]}, %{})

    assert {:error, %{id: "EDN001"}} =
             Kernel.check_selected(parsed, %{},
               language_selection: %Catena.LanguageSelection{
                 edition: "0.1",
                 language_revision: "0.1.51",
                 previews: []
               }
             )
  end

  @tag obligations: ~w(OT-OBL-011)
  test "a checked cancellation reaches generated recursive child code automatically" do
    assert {:ok, core} = Kernel.check(fixture(:cancel), %{})
    assert {:exited, {:child, _, {:cancelled, 7}}, _} = Catena.Kernel.Stepper.run(core, "main")
    assert {:catena_resource_exit, {:child, _, {:cancelled, 7}}} = catch_exit(beam(core))
  end

  @tag obligations: ~w(OT-OBL-004)
  test "normal child outcome evidence remains available at the scope boundary" do
    assert {:ok, core} = Kernel.check(fixture(:normal), %{})
    assert {:ok, :unit, result} = Catena.Kernel.Stepper.run(core, "main")

    assert [%{outcome: {:ok, :unit}}] =
             Enum.filter(result.trace, &(&1.label == :task_child_completed))

    assert {:unit, trace} = Catena.Effect.Runtime.capture_trace(fn -> beam(core) end)
    assert [{:task_child_completed, _, {:ok, :unit}}] = trace
  end

  @tag obligations: ~w(OT-OBL-012)
  test "an owned child cannot inherit its owner's surrounding effect handler" do
    source = """
    (module TaskIsolationProbe (edition 0.1) (revision 0.1.8) (origin "test://task/isolation")
      (export value main)
      (effect Trace (operation mark (params Int) Int))
      (handler Echo (effect Trace) (input Unit) (output Unit)
        (return value (var value)) (operation mark (params (n Int)) (resume next) (resume next (var n))))
      (def main (signature Unit (uses))
        (handle Echo (sequence (fn (u Unit) (sequence (request Trace mark 1) (unit))) (unit)))))
    """

    assert {:ok, parsed} = Parser.parse(source)
    [main] = parsed.definitions
    callback = main.expression.expression.first

    start = %{
      tag: :task_start,
      scope: variable("tasks", main.span),
      body: callback,
      span: main.span
    }

    body = %{main.expression.expression | first: start}

    scope = %{
      tag: :task_scope,
      binder: "tasks",
      body: body,
      grace_ns: 1_000_000_000,
      span: main.span
    }

    main = %{main | expression: %{main.expression | expression: scope}}

    assert {:error, %{id: "T002"}} =
             Kernel.check_selected(%{parsed | definitions: [main]}, %{"Trace" => "trace"})
  end

  @tag obligations: ~w(OT-OBL-002)
  test "task handles cannot escape through the result or a captured callback" do
    parsed = fixture(:normal)
    [spin, worker, main] = parsed.definitions
    scope = main.expression
    escaped = %{scope | body: variable("tasks", main.span)}

    assert {:error, _} =
             Kernel.check(
               %{parsed | definitions: [spin, worker, %{main | expression: escaped}]},
               %{}
             )

    captured = %{scope.body.value.body | body: scope.body.value}
    body = put_in(scope.body.value.body, captured)

    assert {:error, _} =
             Kernel.check(
               %{
                 parsed
                 | definitions: [spin, worker, %{main | expression: %{scope | body: body}}]
               },
               %{}
             )

    assert {:ok, core} = Kernel.check(parsed, %{})
    [spin, worker, main] = core.definitions

    forged = %{
      core
      | definitions: [
          spin,
          worker,
          %{main | expression: %{main.expression | task_scope_id: "forged"}}
        ]
    }

    assert {:error, _} = Verifier.verify(forged)
  end

  @tag obligations: ~w(OT-OBL-005)
  test "actual enclosing-handler abandonment cancels generated child work before returning" do
    source = """
    (module TaskAbortProbe (edition 0.1) (revision 0.1.8) (origin "test://task/abort")
      (export value main)
      (effect Trace (operation mark (params Int) Int))
      (handler Echo (effect Trace) (input Int) (output Int)
        (return value (var value)) (operation mark (params (n Int)) (resume next) 77))
      (def spin (signature (Fn Unit (effects) Unit) (uses)) (fn (u Unit) (call (var spin) (var u))))
      (def main (signature Int (uses)) (handle Echo (request Trace mark 1))))
    """

    assert {:ok, parsed} = Parser.parse(source)
    [spin, main] = parsed.definitions

    start = %{
      tag: :task_start,
      scope: variable("tasks", main.span),
      body: spin.expression,
      span: main.span
    }

    body = %{
      tag: :let,
      name: "child",
      value: start,
      body: main.expression.expression,
      span: main.span
    }

    scope = %{
      tag: :task_scope,
      binder: "tasks",
      body: body,
      grace_ns: 1_000_000_000,
      span: main.span
    }

    main = %{main | expression: %{main.expression | expression: scope}}

    assert {:ok, core} =
             Kernel.check(%{parsed | definitions: [spin, main]}, %{"Trace" => "trace"})

    assert {:ok, 77, _} = Catena.Kernel.Stepper.run(core, "main")
    assert 77 == beam(core)
    refute_receive {:task_failed, _, _}, 0
  end

  @tag obligations: ~w(OT-OBL-005)
  test "owner trap releases inner resources, joins cancelled children, then releases outer resources" do
    parsed = fixture(:cancel)
    [spin, worker, main] = parsed.definitions
    release = %{worker.expression | parameter_type: :integer}

    trap = %{
      tag: :trap,
      expression: %{tag: :integer, value: 99, span: main.span},
      span: main.span
    }

    resource = fn body, payload ->
      %{
        tag: :resource_scope,
        acquire: %{tag: :integer, value: payload, span: main.span},
        release: release,
        body: body,
        grace_ns: 1_000_000_000,
        span: main.span
      }
    end

    scope = %{main.expression | body: %{main.expression.body | body: resource.(trap, 22)}}
    main = %{main | expression: resource.(scope, 11)}
    assert {:ok, core} = Kernel.check(%{parsed | definitions: [spin, worker, main]}, %{})
    assert {:trap, 99, result} = Catena.Kernel.Stepper.run(core, "main")

    assert for(%{label: :resource_release_started, payload: payload} <- result.trace, do: payload) ==
             [22, 11]

    assert Enum.find(result.processes, &(&1.pid == 1)).status == :cancelled

    assert {{:catena_trap, 99}, trace} =
             Catena.Effect.Runtime.capture_trace(fn -> catch_error(beam(core)) end)

    assert for({:resource_release_started, payload} <- trace, do: payload) == [22, 11]
  end

  test "checked sleep uses an explicit virtual deadline and rejects negative durations" do
    parsed = fixture(:normal)
    [spin, worker, main] = parsed.definitions

    for duration <- [0, 10, -1] do
      sleep = %{
        tag: :task_sleep,
        scope: variable("tasks", main.span),
        duration: %{tag: :integer, value: duration, span: main.span},
        span: main.span
      }

      main = %{main | expression: %{main.expression | body: sleep}}
      assert {:ok, core} = Kernel.check(%{parsed | definitions: [spin, worker, main]}, %{})

      if duration < 0 do
        assert {:trap, :invalid_duration, _} = Catena.Kernel.Stepper.run(core, "main")
        assert {:catena_trap, :invalid_duration} = catch_error(beam(core))
      else
        assert :unit == beam(core)
        assert {:ok, initial} = Catena.Kernel.Stepper.initial(core, "main")
        waiting = until_sleep(initial)

        if duration > 0 do
          assert Catena.Kernel.Stepper.runnable_pids(waiting) == []
          assert {:ok, early} = Catena.Task.Reference.advance(waiting, duration - 1)
          assert Catena.Kernel.Stepper.runnable_pids(early) == []
        end

        assert {:ok, due} = Catena.Task.Reference.advance(waiting, duration)
        assert {:ok, :unit, _} = Catena.Kernel.Stepper.run_configuration(due)

        if duration > 0,
          do: assert({:error, :clock_reversed} = Catena.Task.Reference.advance(due, duration - 1))
      end
    end
  end

  @tag obligations: ~w(OT-OBL-011)
  test "shutdown exhaustion is distinct from cooperative completion and cannot win twice" do
    assert {:ok, core} = Kernel.check(fixture(:cancel), %{})
    assert {:ok, initial} = Catena.Kernel.Stepper.initial(core, "main")

    pending =
      Enum.reduce_while(1..100, initial, fn _, c ->
        if get_in(c, [:processes, 1, :task_pending]) do
          {:halt, c}
        else
          assert {:ok, next} = Catena.Kernel.Stepper.step(c, 0)
          {:cont, next}
        end
      end)

    assert pending.processes[1].task_pending == {:cancelled, 7}

    assert {:error, :shutdown_deadline_not_due} =
             Catena.Task.Reference.expire_shutdown(pending, 1)

    assert {:ok, due} = Catena.Task.Reference.advance(pending, 1_000_000_000)
    assert {:ok, forced} = Catena.Task.Reference.expire_shutdown(due, 1)
    assert forced.processes[1].status == :exited
    assert forced.processes[1].result == :shutdown_deadline_exhausted
    assert {:error, :shutdown_deadline_not_due} = Catena.Task.Reference.expire_shutdown(forced, 1)

    assert {:exited, {:child, 1, {:exit, :shutdown_deadline_exhausted}}, _} =
             Catena.Kernel.Stepper.run_configuration(forced)

    assert {:ok, cooperative} = Catena.Kernel.Stepper.step(pending, 1)
    assert cooperative.processes[1].status == :cancelled
    assert {:ok, late} = Catena.Task.Reference.advance(cooperative, 1_000_000_000)
    assert {:error, :shutdown_deadline_not_due} = Catena.Task.Reference.expire_shutdown(late, 1)
  end

  defp until_sleep(c) do
    if c.processes[0].status == :task_sleeping do
      c
    else
      assert {:ok, next} = Catena.Kernel.Stepper.step(c, 0)
      until_sleep(next)
    end
  end

  defp fixture(mode) do
    source = """
    (module TaskCoreProbe (edition 0.1) (revision 0.1.8) (origin "test://task/core")
      (export value main)
      (def spin (signature (Fn Unit (effects) Unit) (uses)) (fn (u Unit) (call (var spin) (var u))))
      (def worker (signature (Fn Unit (effects) Unit) (uses)) (fn (u Unit) (unit)))
      (def main (signature Unit (uses)) (unit)))
    """

    {:ok, parsed} = Parser.parse(source)
    [spin, worker, main] = parsed.definitions
    worker_body = if mode == :cancel, do: spin.expression, else: worker.expression

    start = %{
      tag: :task_start,
      scope: variable("tasks", main.span),
      body: worker_body,
      span: main.span
    }

    result =
      if mode == :cancel do
        %{
          tag: :task_cancel,
          task: variable("child", main.span),
          reason: %{tag: :integer, value: 7, span: main.span},
          span: main.span
        }
      else
        main.expression
      end

    body = %{tag: :let, name: "child", value: start, body: result, span: main.span}

    scope = %{
      tag: :task_scope,
      binder: "tasks",
      body: body,
      grace_ns: 1_000_000_000,
      span: main.span
    }

    %{parsed | definitions: [spin, worker, %{main | expression: scope}]}
  end

  defp variable(name, span), do: %{tag: :variable, name: name, span: span}

  defp beam(core) do
    assert {:ok, forms} = Instrument.lower(core)
    assert {:ok, module, binary} = :compile.forms(forms, [:binary, :return_errors])
    assert {:module, ^module} = :code.load_binary(module, ~c"task-core-experiment", binary)

    try do
      apply(module, :main, [])
    after
      :code.purge(module)
      :code.delete(module)
    end
  end
end
