defmodule Catena.C086ReceiveCompletionTest do
  use ExUnit.Case, async: false

  alias Catena.Kernel.Stepper

  @tag obligations: ~w(RC-OBL-001)
  test "the correction is separately registered without changing retained formats" do
    assert Catena.LanguageVersion.latest() == "0.1.56"

    assert {:ok, :stable} =
             Catena.LanguageLifecycle.state("selective-receive-correction", "0.1.49")

    change =
      Enum.find(
        Catena.LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-49-selective-receive-correction")
      )

    assert change["from"] == "0.1.48"
    assert change["to"] == "0.1.49"
    assert change["classification"] == "compatible-correction"
    assert change["affects"] == ["static-meaning"]
    assert change["migration"] =~ "Historical selections"
    assert change["specification"] =~ "waiting-and-scan-cost-amendment.md#waiting-and-selection"
    assert {:ok, %{selection: %{language_revision: "0.1.56"}}} = Catena.decode_source_text("")

    assert {:ok, %{selection: %{language_revision: "0.1.46"}}} =
             Catena.decode_source_text("",
               language_selection: %{
                 "edition" => "0.1",
                 "language_revision" => "0.1.46",
                 "previews" => []
               }
             )

    for versions <- [
          Catena.LanguageVersion.compilable_revisions(),
          Catena.LanguageVersion.artifact_versions(),
          Catena.LanguageVersion.signed_format_versions()
        ] do
      refute "0.1.49" in versions
    end
  end

  @tag obligations: ~w(RC-OBL-002 RC-OBL-004 RC-OBL-008)
  test "two receives bypass rejected messages, select oldest matches once and preserve residual order" do
    body = """
    (receive (case (bind first) (when (greater (var first) 0))
      (receive (case (bind second) (when (greater (var second) 0))
        (sequence
          (send (var observer) (tuple (var first) (var second)))
          (receive (case 999 (unit))))))))
    """

    source = fixture([0, 2, -1, 1, 3], body)
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:quiescent, outcome} = Stepper.run(core, "main")
    assert selected(outcome) == [2, 1]
    assert mailbox(outcome, "Worker") == [0, -1, 3]
    assert mailbox(outcome, "Observer") == [{2, 1}]

    with_beam(source, fn worker ->
      assert_receive {2, 1}, 1000
      assert_waiting(worker, [0, -1, 3])
    end)
  end

  for messages <- [[], [0, -1, 0]] do
    @tag obligations: ~w(RC-OBL-002 RC-OBL-004)
    test "no matching candidate waits with the exact mailbox #{inspect(messages)}" do
      messages = unquote(messages)

      source =
        fixture(messages, "(receive (case (bind item) (when (greater (var item) 0)) (unit)))")

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:quiescent, outcome} = Stepper.run(core, "main")
      assert selected(outcome) == []
      assert mailbox(outcome, "Worker") == messages
      assert Enum.find(outcome.processes, &(&1.name == "Worker")).status == :waiting
      with_beam(source, fn worker -> assert_waiting(worker, messages) end)
    end
  end

  defp fixture(messages, body) do
    queued =
      Enum.reduce(Enum.reverse(messages), body, fn message, rest ->
        "(sequence (send (self) #{message}) #{rest})"
      end)

    """
    (module C086Completion
      (edition 0.1) (revision 0.1.8) (origin "test://c086/completion")
      (export value main) (export process Worker)
      (process Observer (mailbox (Tuple Int Int)) (params)
        (receive (case (bind ignored) (when false) (unit))))
      (process Worker (mailbox Int) (params (observer (Process (Tuple Int Int))))
        #{queued})
      (def main (signature Unit (uses Process))
        (let observer (spawn Observer)
          (let worker (spawn Worker (var observer)) (unit)))))
    """
  end

  defp selected(outcome),
    do: outcome.trace |> Enum.filter(&(&1.label == :receive)) |> Enum.map(& &1.message)

  defp mailbox(outcome, name),
    do:
      outcome.processes
      |> Enum.find(&(&1.name == name))
      |> Map.fetch!(:mailbox)
      |> Enum.map(& &1.value)

  defp with_beam(source, fun) do
    assert {:ok, core} = Catena.check_kernel(source)
    entry = Enum.find(core.processes, &(&1.name == "Worker"))
    assert {:ok, module, binary, _} = Catena.compile_kernel(source)
    assert {:module, ^module} = :code.load_binary(module, ~c"c086-completion.beam", binary)
    worker = apply(module, String.to_atom(entry.spawn_symbol), [self()])
    monitor = Process.monitor(worker)

    try do
      fun.(worker)
    after
      Process.exit(worker, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^worker, _}, 1000
      :code.delete(module)
      :code.purge(module)
    end
  end

  # Await an explicit VM state, never infer semantic waiting from elapsed time.
  defp assert_waiting(worker, mailbox) do
    deadline = System.monotonic_time(:millisecond) + 1000
    await_waiting(worker, mailbox, deadline)
  end

  defp await_waiting(worker, mailbox, deadline) do
    state = Process.info(worker, [:status, :messages])

    if state == [status: :waiting, messages: mailbox] do
      :ok
    else
      assert System.monotonic_time(:millisecond) < deadline,
             "expected waiting with #{inspect(mailbox)}, got #{inspect(state)}"

      Process.sleep(1)
      await_waiting(worker, mailbox, deadline)
    end
  end
end
