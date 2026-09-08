defmodule Catena.TaskTimeKernelTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Stepper, Verifier}
  alias Catena.Task.{Kernel, Monitor}

  @labels %{
    completed: "done",
    trapped: "fault",
    cancelled: "stopped",
    exited: "exit",
    absent: "missing",
    runtime_failure: "runtime",
    external_loss: "lost"
  }

  @tag obligations: ~w(TM-OBL-003 TM-OBL-006)
  test "a queued matching message wins zero timeout and an empty queue takes fallback" do
    for {messages, expected} <- [
          {[0, 1], {:catena_variant, :done, :unit}},
          {[1], {:catena_variant, :fault, 55}}
        ] do
      assert {:ok, core} = Catena.Task.TimeKernel.check(fixture(messages, 0), %{})
      assert :ok = Verifier.verify(core)
      assert {:ok, ^expected, _} = Stepper.run(core, "main")
      assert beam(core) == expected
    end
  end

  @tag obligations: ~w(TM-OBL-002)
  test "a positive timeout is eligible only after its virtual deadline" do
    assert {:ok, core} = Catena.Task.TimeKernel.check(fixture([1], 1_000_000), %{})
    assert {:ok, initial} = Stepper.initial(core, "main")

    waiting =
      Enum.reduce_while(1..200, initial, fn _, c ->
        case Stepper.runnable_pids(c) do
          [] ->
            {:halt, c}

          [pid | _] ->
            assert {:ok, next} = Stepper.step(c, pid)
            {:cont, next}
        end
      end)

    assert waiting.processes[1].status == :waiting
    assert {:ok, early} = Catena.Task.Reference.advance(waiting, 999_999)
    assert Stepper.runnable_pids(early) == []
    assert {:ok, due} = Catena.Task.Reference.advance(waiting, 1_000_000)
    assert {:ok, {:catena_variant, :fault, 55}, _} = Stepper.run_configuration(due)
    assert beam(core) == {:catena_variant, :fault, 55}
  end

  @tag obligations: ~w(TM-OBL-006)
  test "deadline selection and a queued reply have distinct permitted schedules" do
    assert {:ok, core} = Catena.Task.TimeKernel.check(fixture([1], 10), %{})
    assert {:ok, initial} = Stepper.initial(core, "main")

    waiting =
      Enum.reduce_while(1..200, initial, fn _, c ->
        case Stepper.runnable_pids(c) do
          [] ->
            {:halt, c}

          [pid | _] ->
            assert {:ok, next} = Stepper.step(c, pid)
            {:cont, next}
        end
      end)

    assert {:ok, due} = Catena.Task.Reference.advance(waiting, 10)
    reply_first = put_in(due.processes[1].mailbox, [{99, 0}])
    assert {:ok, {:catena_variant, :done, :unit}, _} = Stepper.run_configuration(reply_first)
    assert {:ok, selected} = Stepper.step(due, 1)
    reply_late = put_in(selected.processes[1].mailbox, [{99, 0}])
    assert {:ok, {:catena_variant, :fault, 55}, result} = Stepper.run_configuration(reply_late)
    refute Enum.any?(result.trace, &match?(%{label: :receive, message: 0}, &1))
    assert Enum.find(result.processes, &(&1.pid == 1)).mailbox == []
  end

  @tag obligations: ~w(TM-OBL-002 TM-OBL-003)
  test "invalid duration is checked before the queued matching message" do
    assert {:ok, core} = Catena.Task.TimeKernel.check(fixture([0, 1], -1), %{})
    assert {:ok, {:catena_variant, :runtime, :unit}, _} = Stepper.run(core, "main")
    assert beam(core) == {:catena_variant, :runtime, :unit}
  end

  @tag obligations: ~w(TM-OBL-003 TM-OBL-008)
  test "fallback preserves messages skipped by the timed receive" do
    parsed = fixture([99, 1], 0)
    [worker] = parsed.processes
    [initial] = worker.body.clauses
    timed = initial.body
    [match] = timed.clauses
    kept = %{match | pattern: %{match.pattern | value: 99}}
    fallback = %{tag: :receive, clauses: [kept], span: worker.span}

    worker = %{
      worker
      | body: %{worker.body | clauses: [%{initial | body: %{timed | fallback: fallback}}]}
    }

    assert {:ok, core} = Catena.Task.TimeKernel.check(%{parsed | processes: [worker]}, %{})
    assert {:ok, {:catena_variant, :done, :unit}, result} = Stepper.run(core, "main")
    assert for(%{label: :receive, message: n} <- result.trace, do: n) == [1, 99]
    assert beam(core) == {:catena_variant, :done, :unit}
  end

  @tag obligations: ~w(TM-OBL-003)
  test "timeout evaluation happens once before scanning rejected candidates" do
    parsed = fixture([0, 1, 99, 0], 0)

    {:ok, donor} =
      Parser.parse("""
      (module DurationProbe (edition 0.1) (revision 0.1.8) (origin "test://task/duration")
        (process Duration (mailbox Int) (params) (sequence (receive (case (bind n) (var n))) (unit))))
      """)

    [duration_worker] = donor.processes
    duration = duration_worker.body.first
    [worker] = parsed.processes
    [initial] = worker.body.clauses

    worker = %{
      worker
      | body: %{worker.body | clauses: [%{initial | body: %{initial.body | duration: duration}}]}
    }

    assert {:ok, core} = Catena.Task.TimeKernel.check(%{parsed | processes: [worker]}, %{})
    assert {:ok, {:catena_variant, :done, :unit}, result} = Stepper.run(core, "main")
    assert for(%{label: :receive, message: n} <- result.trace, do: n) == [1, 0, 0]
    assert beam(core) == {:catena_variant, :done, :unit}
  end

  @tag obligations: ~w(TM-OBL-001 TM-OBL-004)
  test "opaque absolute deadlines share one budget across repeated waits and receive" do
    for {messages, expected} <- [
          {[0, 1], {:catena_variant, :done, :unit}},
          {[1], {:catena_variant, :fault, 55}}
        ] do
      parsed = fixture(messages, 10)
      [worker] = parsed.processes
      [initial] = worker.body.clauses

      timed =
        initial.body
        |> Map.delete(:duration)
        |> Map.put(:tag, :timed_receive_until)
        |> Map.put(:deadline, variable("deadline", worker.span))

      make = %{
        tag: :task_deadline,
        scope: variable("clock", worker.span),
        duration: integer(10, worker.span),
        span: worker.span
      }

      wait = %{
        tag: :task_wait_until,
        deadline: variable("deadline", worker.span),
        span: worker.span
      }

      repeated = %{
        tag: :sequence,
        first: wait,
        second: %{tag: :sequence, first: wait, second: timed, span: worker.span},
        span: worker.span
      }

      scope = %{
        tag: :task_scope,
        binder: "clock",
        grace_ns: 1_000_000_000,
        body: %{tag: :let, name: "deadline", value: make, body: repeated, span: worker.span},
        span: worker.span
      }

      worker = %{worker | body: %{worker.body | clauses: [%{initial | body: scope}]}}
      parsed = %{parsed | processes: [worker]}
      assert {:ok, core} = Catena.Task.TimeKernel.check(parsed, %{})
      assert :ok = Verifier.verify(core)
      assert {:error, %{id: "T002"}} = Kernel.check_selected(parsed, %{})
      assert {:ok, initial} = Stepper.initial(core, "main")

      waiting =
        Enum.reduce_while(1..200, initial, fn _, c ->
          case Stepper.runnable_pids(c) do
            [] ->
              {:halt, c}

            [pid | _] ->
              assert {:ok, next} = Stepper.step(c, pid)
              {:cont, next}
          end
        end)

      assert waiting.processes[1].task_wake == 10
      assert {:ok, early} = Catena.Task.Reference.advance(waiting, 9)
      assert Stepper.runnable_pids(early) == []
      assert {:ok, due} = Catena.Task.Reference.advance(waiting, 10)
      assert {:ok, ^expected, _} = Stepper.run_configuration(due)
      assert beam(core) == expected
    end
  end

  @tag obligations: ~w(TM-OBL-004)
  test "absolute deadline handles cannot escape the scope which supplies their origin" do
    parsed = fixture([1], 0)
    [main] = parsed.definitions

    escaped = %{
      tag: :task_deadline,
      scope: variable("tasks", main.span),
      duration: integer(10, main.span),
      span: main.span
    }

    main = %{main | expression: %{main.expression | body: escaped}}

    assert {:error, %{id: "T002"}} =
             Catena.Task.TimeKernel.check(%{parsed | definitions: [main]}, %{})
  end

  @tag obligations: ~w(TM-OBL-005)
  test "a raw actor's explicit owned scope observes child failure while blocked in receive" do
    parsed = fixture([1], 0)
    [worker] = parsed.processes
    [initial] = worker.body.clauses

    waiting =
      initial.body |> Map.delete(:duration) |> Map.delete(:fallback) |> Map.put(:tag, :receive)

    child = %{
      tag: :function,
      parameter: "u",
      parameter_type: :unit,
      body: %{tag: :trap, expression: integer(99, worker.span), span: worker.span},
      span: worker.span
    }

    start = %{
      tag: :task_start,
      scope: variable("children", worker.span),
      body: child,
      span: worker.span
    }

    scope = %{
      tag: :task_scope,
      binder: "children",
      grace_ns: 1_000_000_000,
      body: %{tag: :sequence, first: start, second: waiting, span: worker.span},
      span: worker.span
    }

    worker = %{worker | body: %{worker.body | clauses: [%{initial | body: scope}]}}
    [main] = parsed.definitions
    main = put_in(main.expression.body.value.tag, :spawn)
    main = put_in(main.expression.body.body.body.first.tag, :send)

    assert {:ok, core} =
             Catena.Task.TimeKernel.check(
               %{parsed | processes: [worker], definitions: [main]},
               %{}
             )

    assert {:ok, {:catena_variant, :exit, :unit}, _} = Stepper.run(core, "main")
    assert beam(core) == {:catena_variant, :exit, :unit}
  end

  @tag obligations: ~w(TM-OBL-001)
  test "time target rejects old selection and forged typed timeout evidence" do
    parsed = fixture([1], 0)

    assert {:error, %{id: "EDN001"}} =
             Catena.Task.TimeKernel.check(parsed, %{},
               language_selection: %Catena.LanguageSelection{
                 edition: "0.1",
                 language_revision: "0.1.52",
                 previews: []
               }
             )

    assert {:ok, core} = Catena.Task.TimeKernel.check(parsed, %{})
    [worker] = core.processes
    [initial] = worker.body.clauses
    forged = put_in(initial.body.duration.type, :boolean)
    worker = %{worker | body: %{worker.body | clauses: [forged]}}
    assert {:error, _} = Verifier.verify(%{core | processes: [worker]})

    old =
      Enum.reduce(
        [:version, :frontend_format, :frontend_version, :language_revision],
        core,
        &Map.put(&2, &1, "0.1.52")
      )

    assert {:error, _} = Verifier.verify(old)
  end

  @tag obligations: ~w(TM-OBL-005 TM-OBL-006)
  test "cancellation after wait selection uses the next safe point without selecting another branch" do
    assert {:ok, core} = Catena.Task.TimeKernel.check(fixture([1], 10), %{})
    assert {:ok, initial} = Stepper.initial(core, "main")

    waiting =
      Enum.reduce_while(1..200, initial, fn _, c ->
        case Stepper.runnable_pids(c) do
          [] ->
            {:halt, c}

          [pid | _] ->
            assert {:ok, next} = Stepper.step(c, pid)
            {:cont, next}
        end
      end)

    assert {:ok, due} = Catena.Task.Reference.advance(waiting, 10)
    assert {:ok, selected} = Stepper.step(due, 1)
    assert selected.processes[1].task_after_wait
    interrupted = update_in(selected.processes[1], &Map.put(&1, :task_pending, {:exit, 7}))

    assert {:ok, {:catena_variant, :exit, :unit}, result} =
             Stepper.run_configuration(interrupted)

    refute Enum.any?(result.trace, &match?(%{label: :trap, reason: 55}, &1))
  end

  defp fixture(messages, duration) do
    source = """
    (module TimedReceiveProbe (edition 0.1) (revision 0.1.8) (origin "test://task/timed-receive")
      (export value main)
      (process Worker (mailbox Int) (params)
        (receive (case 1 (receive (case 0 (unit))))))
      (def main (signature Unit (uses Process)) (let peer (spawn Worker) (unit))))
    """

    {:ok, parsed} = Parser.parse(source)
    [main] = parsed.definitions
    [worker] = parsed.processes
    [first] = worker.body.clauses

    timed =
      Map.merge(first.body, %{
        tag: :timed_receive,
        duration: integer(duration, main.span),
        fallback: %{tag: :trap, expression: integer(55, main.span), span: main.span}
      })

    worker = %{worker | body: %{worker.body | clauses: [%{first | body: timed}]}}
    observed = %{tag: :task_observe, monitor: variable("watcher", main.span), span: main.span}

    sent =
      Enum.reduce(Enum.reverse(messages), observed, fn n, next ->
        %{
          tag: :sequence,
          first: %{
            tag: :managed_send,
            left: variable("peer", main.span),
            right: integer(n, main.span),
            span: main.span
          },
          second: next,
          span: main.span
        }
      end)

    monitor = %{
      tag: :task_monitor,
      target: variable("peer", main.span),
      scope: variable("tasks", main.span),
      labels: @labels,
      span: main.span
    }

    bound = %{tag: :let, name: "watcher", value: monitor, body: sent, span: main.span}
    spawn = Map.merge(main.expression.value, %{tag: :managed_spawn, grace_ns: 1_000_000_000})

    scope = %{
      tag: :task_scope,
      binder: "tasks",
      grace_ns: 1_000_000_000,
      body: %{main.expression | value: spawn, body: bound},
      span: main.span
    }

    %{
      parsed
      | processes: [worker],
        definitions: [%{main | expression: scope, signature: Monitor.type(@labels)}]
    }
  end

  defp variable(name, span), do: %{tag: :variable, name: name, span: span}
  defp integer(value, span), do: %{tag: :integer, value: value, span: span}

  defp beam(core) do
    assert {:ok, module, binary, metadata} = Catena.Kernel.Backend.compile(core)
    assert metadata.artifact_version == "0.1.53"
    assert metadata.interface == nil
    assert {:ok, ^module, ^binary, _} = Catena.Kernel.Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"timed-receive-experiment", binary)

    try do
      apply(module, :main, [])
    after
      :code.purge(module)
      :code.delete(module)
    end
  end
end
