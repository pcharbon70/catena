defmodule Catena.TaskTimeKernelTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Stepper, Verifier}
  alias Catena.Task.{Kernel, Instrument, Monitor}

  @labels %{
    completed: "done",
    trapped: "fault",
    cancelled: "stopped",
    exited: "exit",
    absent: "missing",
    runtime_failure: "runtime",
    external_loss: "lost"
  }

  test "a queued matching message wins zero timeout and an empty queue takes fallback" do
    for {messages, expected} <- [
          {[0, 1], {:catena_variant, "done", :unit}},
          {[1], {:catena_variant, "fault", 55}}
        ] do
      assert {:ok, core} = Kernel.check(fixture(messages, 0), %{})
      assert :ok = Verifier.verify(core)
      assert {:ok, ^expected, _} = Stepper.run(core, "main")
      assert beam(core) == expected
    end
  end

  test "a positive timeout is eligible only after its virtual deadline" do
    assert {:ok, core} = Kernel.check(fixture([1], 1_000_000), %{})
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
    assert {:ok, {:catena_variant, "fault", 55}, _} = Stepper.run_configuration(due)
    assert beam(core) == {:catena_variant, "fault", 55}
  end

  test "deadline selection and a queued reply have distinct permitted schedules" do
    assert {:ok, core} = Kernel.check(fixture([1], 10), %{})
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
    assert {:ok, {:catena_variant, "done", :unit}, _} = Stepper.run_configuration(reply_first)
    assert {:ok, selected} = Stepper.step(due, 1)
    reply_late = put_in(selected.processes[1].mailbox, [{99, 0}])
    assert {:ok, {:catena_variant, "fault", 55}, result} = Stepper.run_configuration(reply_late)
    refute Enum.any?(result.trace, &match?(%{label: :receive, message: 0}, &1))
    assert Enum.find(result.processes, &(&1.pid == 1)).mailbox == []
  end

  test "invalid duration is checked before the queued matching message" do
    assert {:ok, core} = Kernel.check(fixture([0, 1], -1), %{})
    assert {:ok, {:catena_variant, "runtime", :unit}, _} = Stepper.run(core, "main")
    assert beam(core) == {:catena_variant, "runtime", :unit}
  end

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

    assert {:ok, core} = Kernel.check(%{parsed | processes: [worker]}, %{})
    assert {:ok, {:catena_variant, "done", :unit}, result} = Stepper.run(core, "main")
    assert for(%{label: :receive, message: n} <- result.trace, do: n) == [1, 99]
    assert beam(core) == {:catena_variant, "done", :unit}
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
    assert {:ok, forms} = Instrument.lower(core)
    assert {:ok, module, binary} = :compile.forms(forms, [:binary, :return_errors])
    assert {:module, ^module} = :code.load_binary(module, ~c"timed-receive-experiment", binary)

    try do
      apply(module, :main, [])
    after
      :code.purge(module)
      :code.delete(module)
    end
  end
end
