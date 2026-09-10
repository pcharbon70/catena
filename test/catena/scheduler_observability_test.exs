defmodule Catena.SchedulerObservabilityTest.Host do
  def block(value, control) do
    Catena.Foreign.Control.checkpoint(control)
    Process.sleep(25)
    value
  end
end

defmodule Catena.SchedulerObservabilityTest do
  use ExUnit.Case, async: false

  alias Catena.Foreign.{Adapter, Codec, Descriptor}
  alias Catena.Kernel.Explorer
  alias Catena.Scheduler
  alias Catena.SchedulerObservabilityTest.Host

  @limits %{nodes: 32, bytes: 256, depth: 8}

  defp descriptor do
    integer = elem(Codec.new({:data, :integer}), 1)

    elem(
      Descriptor.new({Host, :block}, [integer], integer, "test://scheduler-block",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      ),
      1
    )
  end

  test "profile rejects deterministic fairness and observable reduction claims" do
    profile = Scheduler.profile()
    assert :ok = Scheduler.validate_profile(profile)
    refute profile.deterministic_scheduling
    refute profile.fairness_guarantee
    refute profile.reduction_count_observable
    assert profile.priority_semantics == :deployment_policy

    assert {:error, :invalid_scheduler_profile} =
             Scheduler.validate_profile(%{profile | fairness_guarantee: true})
  end

  test "foreign work is classified and bounded while unsafe work is refused" do
    declaration = descriptor()
    assert {:ok, :preemptible} = Scheduler.classify(:catena)
    assert {:ok, :scheduled_blocking} = Scheduler.classify({:foreign, declaration})
    assert {:error, :unclassified_foreign_work} = Scheduler.classify({:foreign, %{}})

    assert {:ok, policy} = Scheduler.new(priority: :high, foreign_workers: 1)
    assert {:ok, token, policy} = Scheduler.reserve(policy, {:foreign, declaration})

    assert {:error, :foreign_worker_capacity_exhausted, ^policy} =
             Scheduler.reserve(policy, {:native, :nif})

    assert {:error, :unsafe_unbounded_work, ^policy} =
             Scheduler.reserve(policy, {:unsafe, :blocking_call})

    assert {:ok, policy} = Scheduler.release(policy, token)
    assert {:error, :invalid_scheduler_token, ^policy} = Scheduler.release(policy, token)
  end

  test "scheduled foreign blocking does not prevent an independent runnable process" do
    declaration = descriptor()
    parent = self()

    Adapter.run([declaration], @limits, fn scope ->
      assert {:ok, handle} = Adapter.start(scope, declaration, [7])
      spawn(fn -> Kernel.send(parent, :independent_progress) end)
      assert_receive :independent_progress, 100
      assert {:ok, {:completed, 7}} = Adapter.await(scope, handle, 1_000)
    end)
  end

  test "bounded exploration retains multiple schedules without a fairness claim" do
    source = """
    (module SchedulerSchedules
      (edition 0.1) (revision 0.1.8) (origin "test://scheduler-schedules")
      (export value main)
      (process Sink (mailbox Int) (params) (receive (case (bind x) (unit))))
      (process Sender (mailbox Unit) (params (target (Process Int)) (x Int))
        (send (var target) (var x)))
      (def main (signature Unit (uses Process))
        (let target (spawn Sink)
          (sequence (spawn Sender (var target) 1)
            (sequence (spawn Sender (var target) 2) (unit))))))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, exploration} = Explorer.explore(core, "main")
    assert exploration.configurations > 1
    assert {:exhausted, _} = Explorer.explore(core, "main", [], transition_limit: 1)
  end

  test "machine profile discloses scheduler choices and work classes" do
    profile = Catena.ConformanceInfo.document()["scheduler"]
    assert profile["version"] == "0.1.78"
    assert profile["work_classes"] == ["preemptible", "scheduled_blocking", "unsafe_unbounded"]
    assert profile["unsafe_unbounded_admitted"] == false
    assert Catena.LanguageVersion.introduced(:scheduler_observability) == "0.1.78"
  end
end
