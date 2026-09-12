defmodule Catena.InteractiveSessionTest do
  use ExUnit.Case, async: false

  alias Catena.Tool.Session

  test "retained modules evaluate with bounded redacted history" do
    assert {:ok, session} = Session.open()

    assert {:ok, {Session, owner, ^session, "InteractiveOne", 0, digest}} =
             Session.load(session, module_json("InteractiveOne", 41))

    assert owner == self()
    assert byte_size(digest) == 64
    assert {:ok, 41, steps} = Session.evaluate(session, "InteractiveOne", "answer")
    assert steps > 0
    assert {:ok, %{entries: entries, dropped: 0}} = Session.history(session)
    assert Enum.map(entries, & &1.action) == [:load, :evaluate]
    assert Enum.all?(entries, &(&1.value == :redacted))

    assert {:ok, %{status: :closed, cleanup: :confirmed, cancelled_jobs: 0, generations: 1}} =
             Session.close(session)

    assert {:error, :closed_or_unavailable_session} = Session.history(session)
  end

  test "replacement creates immutable generations and retains exact selection" do
    assert {:ok, session} = Session.open([], capture_values: true)
    assert {:ok, _} = Session.load(session, module_json("ReplaceMe", 1))

    assert {:error, :session_replacement_requires_explicit_selection} =
             Session.load(session, module_json("ReplaceMe", 2))

    assert {:ok, _} = Session.load(session, module_json("ReplaceMe", 2), replace: true)
    assert {:ok, 2, _} = Session.evaluate(session, "ReplaceMe", "answer")
    assert {:ok, 1, _} = Session.evaluate(session, "ReplaceMe", "answer", [], generation: 0)

    assert {:ok, %{entries: entries}} = Session.history(session)
    assert Enum.any?(entries, &(&1.value == 2))
    assert {:ok, _} = Session.close(session)
  end

  test "governed loads, invalid capabilities, and foreign owners fail closed" do
    assert {:error, :invalid_interactive_session} =
             Session.open(Enum.map(1..17, &"cap#{&1}"))

    assert {:ok, session} = Session.open(["console"])

    assert {:error, :governance_action_requires_external_admission} =
             Session.load(session, module_json("Governed", 1), governed: true)

    parent = self()
    spawn(fn -> send(parent, {:foreign, Session.history(session)}) end)
    assert_receive {:foreign, {:error, :invalid_session_owner}}
    assert {:ok, _} = Session.close(session)
  end

  test "owned asynchronous work completes or is interrupted and close cancels children" do
    assert {:ok, session} = Session.open()
    assert {:ok, _} = Session.load(session, module_json("Async", 7))
    assert {:ok, handle} = Session.start_evaluation(session, "Async", "answer")
    assert {:ok, 7, _} = Session.await(handle)
    assert :already_completed = Session.interrupt(handle)

    assert {:ok, _} = Session.load(session, looping_kernel(), format: :kernel)

    assert {:ok, running} =
             Session.start_evaluation(session, "Looping", "main", [],
               evaluation_steps: 10_000_000
             )

    assert :ok = Session.interrupt(running)
    assert {:cancelled, :session_interrupt} = Session.await(running)

    assert {:ok, running_on_close} =
             Session.start_evaluation(session, "Looping", "main", [],
               evaluation_steps: 10_000_000
             )

    {Session, _, _, reference} = running_on_close

    assert {:ok, %{status: :closed, cleanup: :confirmed, cancelled_jobs: 1}} =
             Session.close(session)

    assert is_reference(reference)
    assert {:error, :closed_or_unavailable_session} = Session.await(running_on_close)
  end

  test "the preparatory session is versioned and exposes the P109 gate" do
    assert Catena.LanguageVersion.introduced(:interactive_session) == "0.1.95"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("interactive-session", "0.1.95")
    profile = Catena.ConformanceInfo.document()["interactive_session"]
    assert profile["public_repl"] == "held_for_p109"
    assert profile["history"] == "bounded_metadata_redacted_by_default"
    assert profile["governance"] == "external_admission_required"
    assert profile["cleanup_confirmation_timeout_ms"] == 1_000
  end

  test "owner death terminates the session and its running evaluation" do
    parent = self()

    owner =
      spawn(fn ->
        {:ok, session} = Session.open()
        {:ok, _} = Session.load(session, looping_kernel(), format: :kernel)

        {:ok, handle} =
          Session.start_evaluation(session, "Looping", "main", [], evaluation_steps: 10_000_000)

        send(parent, {:owned_session, session, handle})
        receive do: (:remain_owner -> :ok)
      end)

    assert_receive {:owned_session, session, handle}
    assert {Session, ^owner, ^session, reference} = handle
    worker = :sys.get_state(session).jobs[reference].worker
    session_monitor = Process.monitor(session)
    worker_monitor = Process.monitor(worker)

    Process.exit(owner, :kill)

    assert_receive {:DOWN, ^worker_monitor, :process, ^worker, _}
    assert_receive {:DOWN, ^session_monitor, :process, ^session, :normal}
  end

  test "capability admission rejects an effect before evaluation" do
    assert {:ok, session} = Session.open()
    assert {:ok, _} = Session.load(session, effectful_module_json())

    assert {:error, :interactive_capability_denied} =
             Session.evaluate(session, "Effectful", "main")

    assert {:ok, _} = Session.close(session)
  end

  test "generation and history retention enforce their published bounds" do
    assert {:ok, session} = Session.open()
    assert {:ok, _} = Session.load(session, module_json("Bounded", 0))

    for value <- 1..32 do
      assert {:ok, _} = Session.load(session, module_json("Bounded", value), replace: true)
    end

    for _ <- 1..224 do
      assert {:ok, 32, _} = Session.evaluate(session, "Bounded", "answer")
    end

    assert {:ok, %{entries: entries, dropped: 1}} = Session.history(session)
    assert length(entries) == 256

    assert {:error, :stale_or_unknown_session_generation} =
             Session.evaluate(session, "Bounded", "answer", [], generation: 0)

    assert {:ok, 1, _} = Session.evaluate(session, "Bounded", "answer", [], generation: 1)

    assert {:error, :interactive_session_limit_exceeded} =
             Session.evaluate(session, "Bounded", "answer", [], evaluation_steps: 10_000_001)

    assert {:ok, _} = Session.close(session)
  end

  defp module_json(module, value) do
    JSON.encode!(%{
      "version" => "0.1.1",
      "module" => module,
      "source" => "session://#{module}",
      "exports" => ["answer"],
      "definitions" => [
        %{
          "name" => "answer",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "integer", "value" => value}
        }
      ]
    })
  end

  defp effectful_module_json do
    JSON.encode!(%{
      "version" => "0.1.5",
      "origin" => "session://Effectful",
      "module" => "Effectful",
      "exports" => ["main"],
      "type_exports" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "effects" => [
        %{
          "name" => "Ask",
          "parameters" => [],
          "visibility" => "public",
          "operations" => [
            %{
              "name" => "ask",
              "parameters" => [%{"name" => "value", "type" => %{"tag" => "integer"}}],
              "result" => %{"tag" => "integer"}
            }
          ]
        }
      ],
      "handlers" => [],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{
            "forall" => [],
            "type" => %{"tag" => "integer"},
            "uses" => [%{"effect" => "Ask", "arguments" => [], "capability" => "ask"}]
          },
          "body" => %{
            "tag" => "request",
            "effect" => "Ask",
            "operation" => "ask",
            "arguments" => [%{"tag" => "integer", "value" => 1}],
            "capability" => "ask"
          }
        }
      ]
    })
  end

  defp looping_kernel do
    """
    (module Looping
      (edition 0.1)
      (revision 0.1.8)
      (origin "session://Looping")
      (export value main)
      (def spin
        (signature (Fn Int (effects) Int) (uses))
        (fn (n Int) (call (var spin) (var n))))
      (def main
        (signature Int (uses))
        (call (var spin) 1)))
    """
  end
end
