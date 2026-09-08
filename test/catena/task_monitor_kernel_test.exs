defmodule Catena.TaskMonitorKernelTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Stepper, Verifier}
  alias Catena.Task.{Kernel, Monitor, Instrument}

  @labels %{
    completed: "done",
    trapped: "fault",
    cancelled: "stopped",
    exited: "exit",
    absent: "missing",
    runtime_failure: "runtime",
    external_loss: "lost"
  }

  @tag obligations: ~w(OT-OBL-006)
  test "checked process monitoring returns the same closed outcome on CEK and BEAM" do
    for {body, expected} <- [
          {"(unit)", {:catena_variant, "done", :unit}},
          {"(trap 99)", {:catena_variant, "fault", 99}}
        ] do
      parsed = fixture(body)
      assert {:ok, core} = Kernel.check(parsed, %{})
      assert :ok = Verifier.verify(core)
      assert {:ok, ^expected, _} = Stepper.run(core, "main")
      assert expected == beam(core)
    end
  end

  @tag obligations: ~w(OT-OBL-006)
  test "managed process monitoring agrees across CEK and BEAM after cleanup" do
    for {body, expected} <- [
          {"(unit)", {:catena_variant, "done", :unit}},
          {"(trap 99)", {:catena_variant, "fault", 99}}
        ] do
      parsed = managed(fixture(body))
      assert {:ok, core} = Kernel.check(parsed, %{})
      assert :ok = Verifier.verify(core)
      assert {:ok, ^expected, _} = Stepper.run(core, "main")
      assert expected == beam(core)
    end
  end

  @tag obligations: ~w(OT-OBL-005 OT-OBL-008 OT-OBL-010 OT-OBL-011)
  test "checked link failure is observed in a trapping region and exits a nontrapping actor" do
    for {trapping, abort, timed} <- [
          {true, false, false},
          {false, false, false},
          {false, true, false},
          {false, false, true}
        ] do
      body =
        "(let peer (spawn Failed) (sequence (send (var peer) (unit)) (receive (case (bind never) (unit)))))"

      extra = "(process Failed (mailbox Unit) (params) (receive (case (bind go) (trap 99))))"

      extra =
        extra <>
          """
          (effect Trace (operation mark (params Int) Int))
          (handler Echo (effect Trace) (input Int) (output Int)
            (return value (var value)) (operation mark (params (n Int)) (resume next) 77))
          """

      body = if abort, do: "(sequence (handle Echo (request Trace mark 1)) #{body})", else: body
      parsed = managed(fixture(body, extra))
      [worker, failed] = parsed.processes
      [clause] = worker.body.clauses
      bound = if abort, do: clause.body.second, else: clause.body

      link = %{
        tag: :managed_link,
        target: variable("peer", worker.span),
        labels: @labels,
        span: worker.span
      }

      observed = %{
        tag: :managed_observe,
        link: variable("relationship", worker.span),
        span: worker.span
      }

      continuation =
        if trapping,
          do: %{
            tag: :sequence,
            first: observed,
            second: %{tag: :unit, span: worker.span},
            span: worker.span
          },
          else: bound.body.second

      continuation =
        if timed do
          Map.merge(continuation, %{
            tag: :timed_receive,
            duration: %{tag: :integer, value: 10_000_000_000, span: worker.span},
            fallback: %{tag: :unit, span: worker.span}
          })
        else
          continuation
        end

      linked = %{
        tag: :let,
        name: "relationship",
        value: link,
        body: %{bound.body | second: continuation},
        span: worker.span
      }

      body = %{bound | body: linked}
      body = if trapping, do: %{tag: :managed_trapping, body: body, span: worker.span}, else: body

      body =
        if abort do
          handle = clause.body.first
          trapped = %{tag: :managed_trapping, body: handle.expression, span: worker.span}
          %{clause.body | first: %{handle | expression: trapped}, second: body}
        else
          body
        end

      worker = %{worker | body: %{worker.body | clauses: [%{clause | body: body}]}}

      assert {:ok, core} =
               Kernel.check(%{parsed | processes: [worker, failed]}, %{"Trace" => "trace"})

      expected = {:catena_variant, if(trapping, do: "done", else: "exit"), :unit}
      assert {:ok, ^expected, _} = Stepper.run(core, "main")
      assert expected == beam(core)
    end
  end

  defp managed(%{tag: :spawn} = expression),
    do: expression |> Map.put(:tag, :managed_spawn) |> Map.put(:grace_ns, 1_000_000_000)

  defp managed(%{tag: :send} = expression),
    do: expression |> Map.put(:tag, :managed_send)

  defp managed(%_{} = value), do: value
  defp managed(map) when is_map(map), do: Map.new(map, fn {k, v} -> {k, managed(v)} end)
  defp managed(list) when is_list(list), do: Enum.map(list, &managed/1)
  defp managed(value), do: value

  @tag obligations: ~w(OT-OBL-002 OT-OBL-006)
  test "monitor outcome labels are checked and monitor handles cannot escape" do
    parsed = fixture("(unit)")
    [main] = parsed.definitions
    monitor = main.expression.body.body.value
    bad = %{monitor | labels: Map.put(@labels, :completed, "fault")}
    changed = put_in(main.expression.body.body.value, bad)
    assert {:error, %{id: "T002"}} = Kernel.check(%{parsed | definitions: [changed]}, %{})
    escaped = put_in(main.expression.body.body.body, variable("watcher", main.span))
    assert {:error, _} = Kernel.check(%{parsed | definitions: [escaped]}, %{})
  end

  defp fixture(body, extra \\ "") do
    source = """
    (module TaskMonitorProbe (edition 0.1) (revision 0.1.8) (origin "test://task/monitor")
      (export value main)
      (process Worker (mailbox Unit) (params) (receive (case (bind start) #{body})))
      #{extra}
      (def main (signature Unit (uses Process))
        (let peer (spawn Worker) (sequence (send (var peer) (unit)) (unit)))))
    """

    {:ok, parsed} = Parser.parse(source)
    [main] = parsed.definitions

    monitor = %{
      tag: :task_monitor,
      scope: variable("tasks", main.span),
      target: variable("peer", main.span),
      labels: @labels,
      span: main.span
    }

    observed = %{tag: :task_observe, monitor: variable("watcher", main.span), span: main.span}
    sequence = %{main.expression.body | second: observed}
    bound = %{tag: :let, name: "watcher", value: monitor, body: sequence, span: main.span}

    scope = %{
      tag: :task_scope,
      binder: "tasks",
      body: %{main.expression | body: bound},
      grace_ns: 1_000_000_000,
      span: main.span
    }

    %{parsed | definitions: [%{main | signature: Monitor.type(@labels), expression: scope}]}
  end

  defp variable(name, span), do: %{tag: :variable, name: name, span: span}

  defp beam(core) do
    assert {:ok, forms} = Instrument.lower(core)
    assert {:ok, module, binary} = :compile.forms(forms, [:binary, :return_errors])
    assert {:module, ^module} = :code.load_binary(module, ~c"task-monitor-experiment", binary)

    try do
      apply(module, :main, [])
    after
      :code.purge(module)
      :code.delete(module)
    end
  end
end
