defmodule Catena.Resource.Kernel do
  @moduledoc "Checked local resource-scope compound boundary at exact 0.1.51."
  alias Catena.Kernel.{CapabilityKernel, Checker}
  alias Catena.Diagnostic
  @profile "0.1.51"

  def check(parsed, bindings, options \\ []) do
    with {:ok, _} <- selection(options),
         {:ok, module} <- CapabilityKernel.prepare(parsed, bindings) do
      module =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          module,
          &Map.put(&2, &1, @profile)
        )

      module = assign_scopes(module)
      Checker.check(module)
    end
  end

  def selection(options) do
    requested =
      Keyword.get(options, :language_selection, %Catena.LanguageSelection{
        edition: "0.1",
        language_revision: @profile,
        previews: []
      })

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{edition: "0.1", language_revision: @profile, previews: []} = selection} ->
        {:ok, selection}

      {:ok, _} ->
        {:error, Diagnostic.new("EDN001", "resource-tree input requires exact 0.1.51")}

      error ->
        error
    end
  end

  def compile(core) do
    alias Catena.{ImplementationLimits, Kernel.Backend, Kernel.Verifier, OTP.Compiler}

    with :ok <- Verifier.verify(core),
         true <- core.version == @profile,
         %{arity: 0} = entry <- Enum.find(core.definitions, &(&1.name == "main")),
         true <- MapSet.size(CapabilityKernel.slots([entry.signature, entry.uses])) == 0,
         true <- MapSet.size(resource_ids(entry.signature)) == 0,
         :ok <- ImplementationLimits.validate_source_arities(core),
         forms <- Backend.lower(core),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, selected} <- selection([]),
         {:ok, module, binary, warnings} <-
           Compiler.compile(forms,
             source: core.origin,
             artifact_version: @profile,
             frontend_version: @profile,
             frontend: "resource-tree-0.1.51",
             specification: @profile,
             language_selection: selected
           ) do
      {:ok, module, binary,
       %{
         core: core,
         forms: forms,
         warnings: warnings,
         diagnostics: core.diagnostics,
         selection: selected,
         artifact_version: @profile,
         layout: :fixed,
         interface: nil,
         interface_binary: nil
       }}
    else
      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      {:error, reason} ->
        {:error, Diagnostic.new("I001", "resource-core verification failed: #{reason}")}

      _ ->
        {:error,
         Diagnostic.new("EFX003", "resource artifact requires a fully handled main entry")}
    end
  end

  def boundary(%{version: @profile} = core) do
    expected = assign_scopes(core)

    if Enum.all?(
         [:frontend_format, :frontend_version, :language_revision],
         &(Map.get(core, &1) == @profile)
       ) and
         expected.definitions == core.definitions and expected.processes == core.processes and
         not scopes?(core.handlers) do
      :ok
    else
      {:error, "inconsistent local resource profile or scope identity"}
    end
  end

  def boundary(core) do
    if scopes?(core),
      do: {:error, "resource scopes are not part of the selected retained revision"},
      else: :ok
  end

  def assign_scopes(module) do
    qualify = fn entries, field, role ->
      entries
      |> Enum.with_index()
      |> Enum.map(fn {entry, index} ->
        {expression, _} =
          map_scopes(Map.fetch!(entry, field), 0, fn node, count ->
            {:ok, id} =
              Catena.Kernel.CapabilityBinding.identity(module.origin, module.module, [
                role,
                index,
                count
              ])

            {node |> Map.put(:resource_id, id) |> Map.put_new(:binder, nil), count + 1}
          end)

        Map.put(entry, field, expression)
      end)
    end

    %{
      module
      | definitions: qualify.(module.definitions, :expression, 0),
        processes: qualify.(module.processes, :body, 1)
    }
  end

  def resource_ids({:resource, id, _payload}), do: MapSet.new([id])
  def resource_ids(%_{}), do: MapSet.new()
  def resource_ids(value) when is_map(value), do: value |> Map.values() |> resource_ids()
  def resource_ids(value) when is_tuple(value), do: value |> Tuple.to_list() |> resource_ids()

  def resource_ids(value) when is_list(value),
    do: Enum.reduce(value, MapSet.new(), &MapSet.union(resource_ids(&1), &2))

  def resource_ids(_), do: MapSet.new()

  def captures?(body, environment) do
    outer = resource_ids(environment)
    captured?(body, outer)
  end

  defp captured?(%{tag: :variable, type: type}, outer),
    do: not MapSet.disjoint?(resource_ids(type), outer)

  defp captured?(%_{}, _), do: false

  defp captured?(value, outer) when is_map(value),
    do: Enum.any?(Map.values(value), &captured?(&1, outer))

  defp captured?(value, outer) when is_list(value), do: Enum.any?(value, &captured?(&1, outer))
  defp captured?(_, _), do: false

  defp map_scopes(%_{} = value, count, _fun), do: {value, count}

  defp map_scopes(value, count, fun) when is_map(value) do
    {value, count} =
      if Map.get(value, :tag) == :resource_scope, do: fun.(value, count), else: {value, count}

    value
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.reduce({%{}, count}, fn {key, child}, {result, count} ->
      {child, count} =
        if key == :selected_handler, do: {child, count}, else: map_scopes(child, count, fun)

      {Map.put(result, key, child), count}
    end)
  end

  defp map_scopes(value, count, fun) when is_list(value),
    do: Enum.map_reduce(value, count, &map_scopes(&1, &2, fun))

  defp map_scopes(value, count, _), do: {value, count}

  defp scopes?(%{tag: tag})
       when tag in [:resource_scope, :resource_read, :resource_cancel, :resource_exit],
       do: true

  defp scopes?(%_{}), do: false
  defp scopes?(value) when is_map(value), do: Enum.any?(Map.values(value), &scopes?/1)
  defp scopes?(value) when is_list(value), do: Enum.any?(value, &scopes?/1)
  defp scopes?(_), do: false
end
