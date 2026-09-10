defmodule Catena.HotCodeUpgradeTest.Server do
  use GenServer
  def start_link, do: GenServer.start_link(__MODULE__, :ok)
  def init(:ok), do: {:ok, %{generation: 1}}
  def code_change(:old, state, :upgrade), do: {:ok, %{state | generation: 2}}
  def handle_call(:state, _, state), do: {:reply, state, state}
end

defmodule Catena.HotCodeUpgradeTest do
  use ExUnit.Case, async: false
  alias Catena.Upgrade
  @old String.duplicate("a", 64)
  @new String.duplicate("b", 64)
  @other String.duplicate("c", 64)
  defp schema(id, g), do: %{id: id, validate: &match?(%{generation: ^g}, &1)}

  defp interface(module \\ "Counter"),
    do: %{origin: "test://upgrade", module: module, values: [], types: []}

  defp descriptor(overrides \\ %{}),
    do:
      Map.merge(
        %{
          old_artifact: @old,
          new_artifact: @new,
          new_interface: interface(),
          old_schema: schema("s1", 1),
          new_schema: schema("s2", 2),
          migrate: fn %{count: n} -> {:ok, %{generation: 2, count: n}} end,
          reverse: fn %{count: n} -> {:ok, %{generation: 1, count: n}} end,
          evidence_digest: String.duplicate("d", 64),
          nodes: [local: @old],
          max_state_bytes: 1024,
          max_migration_ms: 100
        },
        overrides
      )

  test "migration waits for quiescence and retains messages arriving during drain" do
    {:ok, s} = Upgrade.new(@old, interface(), schema("s1", 1), %{generation: 1, count: 7})
    {:ok, s} = Upgrade.preflight(s, descriptor())

    assert {:error, {:not_quiescent, :live_capability}, ^s} =
             Upgrade.quiesce(s, [:live_capability])

    {:ok, s} = Upgrade.quiesce(s)
    {:queued, s} = Upgrade.admit_message(s, :message)
    {:ok, s} = Upgrade.migrate(s)
    {:ok, s} = Upgrade.activate(s)
    assert s.active == @new and s.state.count == 7 and s.delivered == [:message]
  end

  test "wrong artifact, interface identity, and node version refuse preflight" do
    {:ok, s} = Upgrade.new(@old, interface(), schema("s1", 1), %{generation: 1})

    for d <- [
          descriptor(%{old_artifact: @new}),
          descriptor(%{new_interface: interface("Other")}),
          descriptor(%{nodes: [local: @other]})
        ] do
      assert {:error, :incompatible_upgrade, ^s} = Upgrade.preflight(s, d)
    end
  end

  test "failed or exhausted migration restores the old snapshot" do
    for fun <- [fn _ -> {:error, :bad} end, fn _ -> Process.sleep(:infinity) end] do
      d = descriptor(%{migrate: fun, max_migration_ms: 5})
      {:ok, s} = Upgrade.new(@old, interface(), schema("s1", 1), %{generation: 1, count: 9})
      {:ok, s} = Upgrade.preflight(s, d)
      {:ok, s} = Upgrade.quiesce(s)
      assert {:error, reason, restored} = Upgrade.migrate(s)
      assert reason in [:migration_failed, :migration_exhausted]
      assert restored.phase == :active and restored.active == @old and restored.state.count == 9
    end
  end

  test "one draining generation is enforced and postcommit rollback requires reverse migration" do
    {:ok, s} = Upgrade.new(@old, interface(), schema("s1", 1), %{generation: 1, count: 1})
    {:ok, s} = Upgrade.preflight(s, descriptor())
    {:ok, draining} = Upgrade.quiesce(s)
    assert {:error, :incompatible_upgrade, ^draining} = Upgrade.preflight(draining, descriptor())
    {:ok, migrated} = Upgrade.migrate(draining)
    {:ok, active} = Upgrade.activate(migrated)

    reverse =
      descriptor(%{
        old_artifact: @new,
        new_artifact: @old,
        old_schema: schema("s2", 2),
        new_schema: schema("s1", 1),
        new_interface: interface()
      })

    assert {:ok, rolled} = Upgrade.rollback(active, reverse)
    assert rolled.active == @old and rolled.state.generation == 1

    assert {:error, :reverse_migration_required, ^active} =
             Upgrade.rollback(active, Map.delete(reverse, :reverse))
  end

  test "OTP adapter performs suspend, checked code change, and resume" do
    {:ok, pid} = Catena.HotCodeUpgradeTest.Server.start_link()

    assert :ok =
             Catena.Upgrade.OTP.change_code(
               pid,
               Catena.HotCodeUpgradeTest.Server,
               :old,
               :upgrade,
               1000
             )

    assert %{generation: 2} = GenServer.call(pid, :state)
  end

  test "profile fixes coexistence and rollback limits" do
    assert %{
             version: "0.1.79",
             coexistence: :one_active_one_draining,
             external_effect_rollback: false
           } = Upgrade.profile()
  end
end
