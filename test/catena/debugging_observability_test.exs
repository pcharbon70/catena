defmodule Catena.DebuggingObservabilityTest do
  use ExUnit.Case, async: false

  alias Catena.Tool.Debugger

  @moduletag obligations:
               ~w(DO-OBL-001 DO-OBL-002 DO-OBL-003 DO-OBL-004 DO-OBL-005 DO-OBL-006 DO-OBL-007 DO-OBL-008 DO-OBL-009 DO-OBL-010 DO-OBL-011 DO-OBL-012 DO-OBL-013 DO-OBL-014 DO-OBL-015 DO-OBL-016)

  defp kernel_input do
    %{
      kind: :kernel,
      path: "src/debug-session.catena",
      source: """
      (module DebugSession (edition 0.1) (revision 0.1.8) (origin "test://debug-session")
        (export value main)
        (def main (signature Int (uses)) (trap 91)))
      """
    }
  end

  defp artifact(options \\ []) do
    input = kernel_input()
    assert {:ok, artifact} = Catena.Debugging.build(input, options)
    {artifact, input}
  end

  test "a verified source breakpoint pauses an actually executed compiled program" do
    {artifact, input} = artifact()
    assert {:ok, debug} = Debugger.open(artifact, input)
    assert {:ok, nodes} = Debugger.nodes(debug)
    node = Enum.find_value(nodes, fn %{id: id, origin: origin} -> origin && id end)
    assert :ok = Debugger.add_breakpoint(debug, node)

    assert {:module, module} =
             Catena.OTP.Compiler.load(artifact.module, ~c"debug-session", artifact.binary)

    task = Task.async(fn -> Debugger.run(debug, node, fn -> apply(module, :main, []) end) end)
    assert_receive {:catena_debug_paused, pause, event}, 1_000
    assert event.node == node
    assert event.origin.primary.path == "src/debug-session.catena"
    assert {:ok, %{active_pauses: 1}} = Catena.Report.debug_snapshot(debug)
    assert :ok = Debugger.continue(debug, pause)
    assert {:trap, report} = Task.await(task)
    assert report.reason == :redacted
    assert Enum.any?(report.frames, &(&1.origin != nil))
    assert Enum.all?(report.frames, &(&1.values == :redacted))
    assert :ok = Debugger.close(debug)

    :code.delete(module)
    :code.purge(module)
  end

  test "bounded traces preserve semantic identity, pseudonymize processes, and report loss" do
    {artifact, input} = artifact()
    assert {:ok, debug} = Debugger.open(artifact, input, maximum_events: 4)
    [node | _] = artifact.sidecar.nodes |> Map.keys() |> Enum.sort()

    assert :ok = Debugger.event(debug, :"process-spawn", node, %{child: self(), value: "hidden"})

    assert :ok =
             Debugger.event(debug, :"message-send", node, %{receiver: self(), payload: "hidden"})

    assert :ok = Debugger.event(debug, :derivation, node, %{kind: :generated_fold})
    assert :ok = Debugger.event(debug, :unavailable, node, %{reason: :optimized_value})
    assert :ok = Debugger.event(debug, :handler, node, %{handler: "Ask", outcome: :resumed})
    assert {:ok, snapshot} = Debugger.snapshot(debug)
    assert snapshot.dropped == 1
    refute snapshot.trace_complete
    assert Enum.map(snapshot.events, & &1.sequence) == [2, 3, 4, 5]

    assert Enum.map(snapshot.events, & &1.kind) == [
             :"message-send",
             :derivation,
             :unavailable,
             :handler
           ]

    assert hd(snapshot.events).attributes.payload == :redacted
    assert hd(snapshot.events).attributes.receiver == "process-1"
    assert Enum.at(snapshot.events, 2).attributes.reason == :optimized_value
    assert Enum.all?(snapshot.events, &(is_binary(&1.id) and &1.elapsed_native >= 0))
    assert snapshot.perturbs_execution and not snapshot.timing_semantic
    assert {:ok, profile} = Catena.Report.debug_profile(debug)
    assert profile.dropped == 1 and not profile.complete
    assert Enum.all?(profile.groups, &(&1.observations == 1 and &1.origin != nil))
    assert profile.perturbing and not profile.portable_timing

    assert {:error, :debug_event_limit} =
             Debugger.event(debug, :"foreign-enter", node, %{
               payload: String.duplicate("x", 20_000)
             })

    assert :ok = Debugger.close(debug)
  end

  test "stripped artifacts, owner boundaries, process death, and closed sessions fail explicitly" do
    {artifact, input} = artifact(mode: :stripped)
    assert {:ok, debug} = Debugger.open(artifact, input)
    [node | _] = Map.keys(artifact.sidecar.nodes)
    assert {:error, :unavailable_breakpoint_origin} = Debugger.add_breakpoint(debug, node)
    assert {:ok, nodes} = Debugger.nodes(debug)
    assert Enum.all?(nodes, &is_nil(&1.origin))

    parent = self()
    thief = spawn(fn -> send(parent, {:denied, Debugger.snapshot(debug)}) end)
    assert is_pid(thief)
    assert_receive {:denied, {:error, :debug_owner_denied}}
    assert :ok = Debugger.close(debug)
    assert {:error, :expired_debug_session} = Debugger.event(debug, :cancellation)

    owner =
      spawn(fn ->
        {artifact, input} = artifact()
        {:ok, owned} = Debugger.open(artifact, input)
        send(parent, {:owned, owned})
        receive do: (:stop -> :ok)
      end)

    assert_receive {:owned, owned}
    send(owner, :stop)
    ref = Process.monitor(owner)
    assert_receive {:DOWN, ^ref, :process, ^owner, _}
    assert {:error, :expired_debug_session} = Debugger.event(owned, :cancellation)
  end

  test "erased declarations remain external evidence and the conformance profile is exact" do
    document = %{
      "version" => "0.1.6",
      "module" => "DebugEvidenceSession",
      "origin" => "test://debug-evidence-session",
      "exports" => ["main"],
      "definitions" => [
        %{
          "name" => "main",
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "parameters" => [],
          "body" => %{"tag" => "integer", "value" => 1}
        },
        %{
          "name" => "proof",
          "verification_only" => true,
          "parameters" => [],
          "body" => %{"tag" => "boolean", "value" => true}
        }
      ]
    }

    input = %{kind: :json, path: "src/evidence.json", source: JSON.encode!(document)}
    assert {:ok, artifact} = Catena.Debugging.build(input)
    assert [{id, reference}] = Map.to_list(artifact.sidecar.evidence)
    assert {:ok, debug} = Debugger.open(artifact, input)
    assert {:ok, ^reference} = Debugger.evidence(debug, id)
    refute artifact.binary =~ id

    profile = Catena.ConformanceInfo.document()["debugging_and_observability"]
    assert profile["version"] == "0.1.89"
    assert profile["trace_perturbation"] == "observable"
    assert profile["erased_declarations"] == "external_evidence_only"
    assert profile["optimized_values"] == "explicitly_unavailable"
    assert "derivation" in profile["event_kinds"] and "unavailable" in profile["event_kinds"]
    assert Catena.LanguageVersion.latest() == "0.1.89"
    assert Catena.LanguageVersion.introduced(:debugging_and_observability) == "0.1.89"

    assert {:ok, :stable} =
             Catena.LanguageLifecycle.state("debugging-and-observability", "0.1.89")
  end
end
