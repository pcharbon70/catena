defmodule Catena.Upgrade do
  @moduledoc "Checked bounded hot-upgrade state machine."
  alias Catena.Package.Compat
  @version "0.1.79"
  @max_bytes 16_777_216
  @max_ms 5_000

  def profile,
    do: %{
      version: @version,
      coexistence: :one_active_one_draining,
      activation: :explicit_quiescent_point,
      migration: :pure_checked_bounded,
      rollback: :snapshot_before_commit_reverse_migration_after_commit,
      external_effect_rollback: false,
      max_state_bytes: @max_bytes,
      max_migration_ms: @max_ms
    }

  def new(artifact, interface, schema, state) do
    with :ok <- digest(artifact),
         :ok <- interface(interface),
         :ok <- schema(schema),
         :ok <- state(state, schema, @max_bytes) do
      {:ok,
       %{
         phase: :active,
         active: artifact,
         interface: interface,
         schema: schema,
         state: state,
         snapshot: nil,
         upgrade: nil,
         queued: [],
         delivered: [],
         evidence: []
       }}
    end
  end

  def preflight(system, d) do
    with :ok <- descriptor(d),
         true <- system.phase == :active,
         true <- d.old_artifact == system.active,
         {:ok, %{class: class}} <- Compat.diff(system.interface, d.new_interface),
         true <- class in [:identical, :patch, :minor],
         true <- Enum.all?(d.nodes, fn {_node, v} -> v in [d.old_artifact, d.new_artifact] end) do
      {:ok,
       %{
         system
         | phase: :validated,
           upgrade: d,
           evidence: system.evidence ++ [{:validated, d.evidence_digest}]
       }}
    else
      _ -> {:error, :incompatible_upgrade, system}
    end
  end

  def quiesce(system, blockers \\ [])

  def quiesce(%{phase: :validated} = system, blockers) do
    blocked =
      Enum.find(
        blockers,
        &(&1 in [
            :live_capability,
            :affine_resumption,
            :unsupported_active_frame,
            :unfinished_child,
            :owned_resource
          ])
      )

    if blocked,
      do: {:error, {:not_quiescent, blocked}, system},
      else: {:ok, %{system | phase: :draining, snapshot: system.state}}
  end

  def quiesce(system, _), do: {:error, :invalid_upgrade_transition, system}

  def admit_message(%{phase: :draining} = s, msg), do: {:queued, %{s | queued: s.queued ++ [msg]}}
  def admit_message(s, msg), do: {:active, msg, s}

  def migrate(%{phase: :draining, upgrade: d} = system) do
    case run(d.migrate, system.state, d.max_migration_ms) do
      {:ok, migrated} ->
        case state(migrated, d.new_schema, d.max_state_bytes) do
          :ok -> {:ok, %{system | phase: :migrated, state: migrated}}
          error -> {:error, elem(error, 1), restore(system)}
        end

      {:error, reason} ->
        {:error, reason, restore(system)}
    end
  end

  def migrate(system), do: {:error, :invalid_upgrade_transition, system}

  def activate(%{phase: :migrated, upgrade: d} = s),
    do:
      {:ok,
       %{
         s
         | phase: :active,
           active: d.new_artifact,
           interface: d.new_interface,
           schema: d.new_schema,
           snapshot: nil,
           upgrade: nil,
           queued: [],
           delivered: s.queued
       }}

  def activate(s), do: {:error, :invalid_upgrade_transition, s}

  def rollback(%{phase: p} = s) when p in [:validated, :draining, :migrated],
    do: {:ok, restore(s)}

  def rollback(%{phase: :active} = s, d) do
    with :ok <- descriptor(d),
         true <- d.old_artifact == s.active,
         reverse when is_function(reverse, 1) <- Map.get(d, :reverse),
         {:ok, migrated} <- run(reverse, s.state, d.max_migration_ms),
         :ok <- state(migrated, d.new_schema, d.max_state_bytes) do
      {:ok,
       %{
         s
         | active: d.new_artifact,
           interface: d.new_interface,
           schema: d.new_schema,
           state: migrated
       }}
    else
      _ -> {:error, :reverse_migration_required, s}
    end
  end

  defp restore(s),
    do: %{
      s
      | phase: :active,
        state: s.snapshot || s.state,
        snapshot: nil,
        upgrade: nil,
        queued: []
    }

  defp run(fun, value, timeout) do
    task = Task.async(fn -> fun.(value) end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, {:ok, result}} -> {:ok, result}
      nil -> {:error, :migration_exhausted}
      _ -> {:error, :migration_failed}
    end
  end

  defp descriptor(d) when is_map(d) do
    keys = [
      :old_artifact,
      :new_artifact,
      :new_interface,
      :old_schema,
      :new_schema,
      :migrate,
      :evidence_digest,
      :nodes,
      :max_state_bytes,
      :max_migration_ms
    ]

    with true <- Enum.all?(keys, &Map.has_key?(d, &1)),
         :ok <- digest(d.old_artifact),
         :ok <- digest(d.new_artifact),
         true <- d.old_artifact != d.new_artifact,
         :ok <- digest(d.evidence_digest),
         :ok <- interface(d.new_interface),
         :ok <- schema(d.old_schema),
         :ok <- schema(d.new_schema),
         true <- is_function(d.migrate, 1),
         true <- is_list(d.nodes) and d.nodes != [],
         true <- d.max_state_bytes in 1..@max_bytes,
         true <- d.max_migration_ms in 1..@max_ms,
         do: :ok,
         else: (_ -> {:error, :invalid_upgrade_descriptor})
  end

  defp descriptor(_), do: {:error, :invalid_upgrade_descriptor}

  defp digest(v) when is_binary(v) and byte_size(v) == 64,
    do: if(String.match?(v, ~r/\A[0-9a-f]{64}\z/), do: :ok, else: {:error, :invalid_digest})

  defp digest(_), do: {:error, :invalid_digest}

  defp interface(v) when is_map(v),
    do:
      if(
        is_binary(v[:origin]) and is_binary(v[:module]) and
          is_list(v[:values]) and is_list(v[:types]),
        do: :ok,
        else: {:error, :invalid_interface}
      )

  defp interface(_), do: {:error, :invalid_interface}
  defp schema(%{id: id, validate: f}) when is_binary(id) and is_function(f, 1), do: :ok
  defp schema(_), do: {:error, :invalid_schema}

  defp state(value, schema, limit) do
    if schema.validate.(value) and
         byte_size(:erlang.term_to_binary(value, [:deterministic])) <= limit,
       do: :ok,
       else: {:error, :invalid_migrated_state}
  rescue
    _ -> {:error, :invalid_migrated_state}
  end
end

defmodule Catena.Upgrade.OTP do
  @moduledoc "Narrow OTP system-message adapter."
  def change_code(pid, module, old, extra, timeout) do
    with :ok <- :sys.suspend(pid, timeout),
         :ok <- :sys.change_code(pid, module, old, extra, timeout) do
      :sys.resume(pid, timeout)
    else
      error ->
        safe_resume(pid, timeout)
        error
    end
  end

  defp safe_resume(pid, timeout) do
    :sys.resume(pid, timeout)
  catch
    _, _ -> :ok
  end
end
