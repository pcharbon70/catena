defmodule Catena.Task.Kernel do
  @moduledoc "Checked owned-task lifetime target at exact 0.1.52, with separate unselected time experiments."
  @profile :owned_task_experiment
  alias Catena.Kernel.{CapabilityKernel, Checker}

  def check(parsed, bindings) do
    with {:ok, module} <- CapabilityKernel.prepare(parsed, bindings) do
      module =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          module,
          &Map.put(&2, &1, @profile)
        )

      module = module |> Catena.Resource.Kernel.assign_scopes() |> assign_scopes()

      if raw_into_managed?(module),
        do:
          {:error,
           Catena.Diagnostic.new("PRC003", "raw spawn cannot enter a managed-context process")},
        else: Checker.check(module)
    end
  end

  def check_selected(parsed, bindings, options \\ []) do
    with {:ok, _} <- selection(options),
         false <- future_time?(parsed),
         {:ok, core} <- check(parsed, bindings) do
      core =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          core,
          &Map.put(&2, &1, "0.1.52")
        )
        |> Map.put(:profile, :owned_task_lifetimes)

      case Catena.Kernel.Verifier.verify(core) do
        :ok ->
          {:ok, core}

        {:error, reason} ->
          {:error, Catena.Diagnostic.new("I001", "task-core verification failed: #{reason}")}
      end
    else
      true ->
        {:error, Catena.Diagnostic.new("T002", "general time nodes require separate admission")}

      error ->
        error
    end
  end

  def selection(options) do
    requested =
      Keyword.get(options, :language_selection, %Catena.LanguageSelection{
        edition: "0.1",
        language_revision: "0.1.52",
        previews: []
      })

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{edition: "0.1", language_revision: "0.1.52", previews: []} = selected} ->
        {:ok, selected}

      {:ok, _} ->
        {:error, Catena.Diagnostic.new("EDN001", "task-tree input requires exact 0.1.52")}

      error ->
        error
    end
  end

  def compile(core) do
    alias Catena.{ImplementationLimits, Kernel.Verifier, OTP.Compiler}

    with :ok <- Verifier.verify(core),
         true <- core.version == "0.1.52",
         %{arity: 0} = entry <- Enum.find(core.definitions, &(&1.name == "main")),
         true <- MapSet.size(CapabilityKernel.slots([entry.signature, entry.uses])) == 0,
         true <- Catena.Kernel.Type.closed?(entry.signature),
         :ok <- ImplementationLimits.validate_source_arities(core),
         {:ok, forms} <- Catena.Task.Instrument.lower(core),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, selected} <- selection([]),
         {:ok, module, binary, warnings} <-
           Compiler.compile(forms,
             source: core.origin,
             artifact_version: "0.1.52",
             frontend_version: "0.1.52",
             frontend: "task-tree-0.1.52",
             specification: "0.1.52",
             language_selection: selected
           ) do
      {:ok, module, binary,
       %{
         core: core,
         forms: forms,
         warnings: warnings,
         diagnostics: core.diagnostics,
         selection: selected,
         artifact_version: "0.1.52",
         layout: :fixed,
         interface: nil,
         interface_binary: nil
       }}
    else
      {:error, %Catena.Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      {:error, reason} ->
        {:error, Catena.Diagnostic.new("I001", "task-core verification failed: #{reason}")}

      _ ->
        {:error, Catena.Diagnostic.new("EFX003", "task artifact requires a closed main entry")}
    end
  end

  def boundary(%{version: "0.1.52"} = core) do
    if Enum.all?(
         [:frontend_format, :frontend_version, :language_revision],
         &(Map.get(core, &1) == "0.1.52")
       ) and not future_time?(core) do
      core =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          core,
          &Map.put(&2, &1, @profile)
        )

      boundary(core)
    else
      {:error, "inconsistent task lifetime target or unadmitted general time node"}
    end
  end

  def boundary(%{version: @profile} = core) do
    expected = assign_scopes(core)

    if Enum.all?(
         [:frontend_format, :frontend_version, :language_revision],
         &(Map.get(core, &1) == @profile)
       ) and
         expected.definitions == core.definitions and expected.processes == core.processes and
         not nodes?(core.handlers) and not raw_into_managed?(core),
       do: :ok,
       else: {:error, "inconsistent experimental task scope identity"}
  end

  def boundary(core),
    do:
      if(nodes?(core) or managed_types?(core),
        do: {:error, "task nodes are not admitted by this revision"},
        else: :ok
      )

  defp raw_into_managed?(module) do
    names = module.processes |> Enum.filter(&context_nodes?(&1.body)) |> MapSet.new(& &1.name)
    raw_target?(module.definitions, names) or raw_target?(module.processes, names)
  end

  defp context_nodes?(%{tag: tag})
       when tag in [
              :timed_receive,
              :managed_self,
              :managed_link,
              :managed_unlink,
              :managed_observe,
              :managed_trapping
            ],
       do: true

  defp context_nodes?(%_{}), do: false
  defp context_nodes?(v) when is_map(v), do: Enum.any?(Map.values(v), &context_nodes?/1)
  defp context_nodes?(v) when is_list(v), do: Enum.any?(v, &context_nodes?/1)
  defp context_nodes?(_), do: false
  defp raw_target?(%{tag: :spawn, entry: entry}, names), do: MapSet.member?(names, entry)
  defp raw_target?(%_{}, _), do: false
  defp raw_target?(v, names) when is_map(v), do: Enum.any?(Map.values(v), &raw_target?(&1, names))
  defp raw_target?(v, names) when is_list(v), do: Enum.any?(v, &raw_target?(&1, names))
  defp raw_target?(_, _), do: false

  def assign_scopes(module) do
    qualify = fn entries, field, role ->
      entries
      |> Enum.with_index()
      |> Enum.map(fn {entry, index} ->
        {expression, _} =
          walk(Map.fetch!(entry, field), 0, fn node, count ->
            {:ok, id} =
              Catena.Kernel.CapabilityBinding.identity(module.origin, module.module, [
                role,
                index,
                count
              ])

            {Map.put(node, :task_scope_id, id), count + 1}
          end)

        Map.put(entry, field, expression)
      end)
    end

    %{
      module
      | definitions: qualify.(module.definitions, :expression, 2),
        processes: qualify.(module.processes, :body, 3)
    }
  end

  defp walk(%_{} = value, count, _), do: {value, count}

  defp walk(value, count, fun) when is_map(value) do
    {value, count} =
      if Map.get(value, :tag) == :task_scope, do: fun.(value, count), else: {value, count}

    value
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.reduce({%{}, count}, fn {key, child}, {acc, count} ->
      {child, count} =
        if key == :selected_handler, do: {child, count}, else: walk(child, count, fun)

      {Map.put(acc, key, child), count}
    end)
  end

  defp walk(value, count, fun) when is_list(value),
    do: Enum.map_reduce(value, count, &walk(&1, &2, fun))

  defp walk(value, count, _), do: {value, count}

  defp future_time?(%{tag: tag}) when tag in [:task_sleep, :timed_receive], do: true
  defp future_time?(%_{}), do: false
  defp future_time?(v) when is_map(v), do: Enum.any?(Map.values(v), &future_time?/1)
  defp future_time?(v) when is_list(v), do: Enum.any?(v, &future_time?/1)
  defp future_time?(_), do: false

  defp managed_types?({:managed_process, _}), do: true
  defp managed_types?({:managed_link, _}), do: true
  defp managed_types?(%_{}), do: false
  defp managed_types?(v) when is_map(v), do: Enum.any?(Map.values(v), &managed_types?/1)
  defp managed_types?(v) when is_tuple(v), do: v |> Tuple.to_list() |> managed_types?()
  defp managed_types?(v) when is_list(v), do: Enum.any?(v, &managed_types?/1)
  defp managed_types?(_), do: false

  defp nodes?(%{tag: tag})
       when tag in [
              :managed_spawn,
              :managed_send,
              :timed_receive,
              :managed_self,
              :managed_link,
              :managed_unlink,
              :managed_observe,
              :managed_trapping,
              :task_scope,
              :task_start,
              :task_cancel,
              :task_sleep,
              :task_monitor,
              :task_observe,
              :task_demonitor
            ],
       do: true

  defp nodes?(%_{}), do: false
  defp nodes?(value) when is_map(value), do: Enum.any?(Map.values(value), &nodes?/1)
  defp nodes?(value) when is_list(value), do: Enum.any?(value, &nodes?/1)
  defp nodes?(_), do: false
end
