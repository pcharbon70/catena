defmodule Catena.Entry do
  @moduledoc """
  Entry points at 0.1.23: entries validation against compiled package
  modules, the derived library distinction, and invocation-only launch
  with return-is-shutdown reports.
  """

  alias Catena.{Diagnostic, Effect.Row}

  @spec library?(list()) :: boolean()
  def library?(entries), do: entries in [nil, []]

  @doc """
  Validates decoded manifest entries against the package's compiled
  modules. Each declared entry must name exactly one exported,
  zero-argument, effect-closed definition whose rendered result type
  equals the declared `result`. Returns the resolved entries with the
  owning BEAM module per entry, or `ENT001`.
  """
  @spec validate(list(), list()) :: {:ok, list()} | {:error, Diagnostic.t()}
  def validate(entries, modules), do: prepare_entries(entries || [], modules)

  @doc """
  Launches one declared entry of a compiled package: the named entry's
  function is invoked with no arguments and evaluated to completion
  under the unchanged kernel semantics. The report is the entry's
  returned value (`completed`) or the trap identity (`failed`).
  """
  @spec launch(map(), String.t()) ::
          {:ok, %{status: :completed, value: term()}} | {:error, Diagnostic.t()}
  def launch(package, name) do
    entry = Enum.find(package.entries, &(&1.name == name))

    if is_nil(entry) do
      {:error,
       Diagnostic.new("ENT002", "entry #{inspect(name)} is not declared by this package",
         path: "$.entries",
         details: %{
           requested: name,
           declared: package.entries |> Enum.map(& &1.name) |> Enum.sort()
         }
       )}
    else
      invoke(entry, Map.fetch!(package.entry_modules, name))
    end
  end

  @doc """
  Renders one result type in the canonical entry spelling: primitive
  tags, variables, functions, tuples, and nominal identity with
  applied arguments. Deterministic and total over the closed type
  grammar.
  """
  @spec render_type(term()) :: String.t()
  def render_type(:integer), do: "integer"
  def render_type(:boolean), do: "boolean"
  def render_type({:var, id}), do: "v#{id}"
  def render_type({:skolem, id}), do: "s#{id}"

  def render_type({:function, parameter, result}),
    do: "(#{render_type(parameter)}) -> #{render_type(result)}"

  def render_type({:tuple, elements}),
    do: "{#{Enum.map_join(elements, ", ", &render_type/1)}}"

  def render_type({:nominal, id, []}), do: id

  def render_type({:nominal, id, arguments}),
    do: "#{id}[#{Enum.map_join(arguments, ", ", &render_type/1)}]"

  defp prepare_entries([], _modules), do: {:ok, []}

  defp prepare_entries(entries, modules) do
    index = export_index(modules)

    Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, resolved} ->
      case resolve_export(index, entry) do
        {:ok, definition} ->
          with :ok <- require_zero_arity(entry, definition),
               :ok <- require_effect_closed(entry, definition),
               :ok <- require_result(entry, definition) do
            {:cont, {:ok, [Map.put(entry, :module, definition.module) | resolved]}}
          else
            {:error, _} = error -> {:halt, error}
          end

        {:error, _} = error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, resolved} -> {:ok, Enum.reverse(resolved)}
      error -> error
    end
  end

  defp export_index(modules) do
    Enum.flat_map(modules, fn module ->
      exports = MapSet.new(module.core.exports)

      module.core.definitions
      |> Enum.filter(&(&1.name in exports))
      |> Enum.map(&Map.put(&1, :module, module.beam_module))
    end)
  end

  defp resolve_export(index, entry) do
    case Enum.filter(index, &(&1.name == entry.name)) do
      [] ->
        {:error, entry_error("unknown_export", entry, %{declared: entry.name})}

      [definition] ->
        {:ok, definition}

      definitions ->
        {:error,
         entry_error("ambiguous_export", entry, %{
           modules: definitions |> Enum.map(& &1.module) |> Enum.sort()
         })}
    end
  end

  defp require_zero_arity(entry, definition) do
    if definition.parameters == [] do
      :ok
    else
      {:error, entry_error("non_zero_arity", entry, %{arity: length(definition.parameters)})}
    end
  end

  defp require_effect_closed(entry, definition) do
    closed? =
      closed_row?(Map.get(definition, :effect_row)) and
        closed_row?(Map.get(definition, :verified_uses_row))

    if closed? do
      :ok
    else
      {:error, entry_error("not_effect_closed", entry, %{name: entry.name})}
    end
  end

  defp closed_row?(nil), do: true
  defp closed_row?(%Row{entries: [], tail: nil}), do: true
  defp closed_row?(_), do: false

  defp require_result(entry, definition) do
    rendered = render_type(definition.scheme.type)

    if rendered == entry.result do
      :ok
    else
      {:error,
       entry_error("result_mismatch", entry, %{declared: entry.result, recorded: rendered})}
    end
  end

  defp invoke(entry, %{module: module, binary: binary}) do
    {:module, ^module} = :code.load_binary(module, ~c"#{module}.beam", binary)
    function = String.to_atom(entry.name)

    try do
      {:ok, %{status: :completed, value: apply(module, function, [])}}
    rescue
      exception ->
        {:error,
         Diagnostic.new(
           "ENT003",
           "entry #{entry.name} failed with #{inspect(exception.__struct__)}",
           path: "$.entries",
           details: %{
             entry: entry.name,
             trap: inspect(exception.__struct__),
             reason: Exception.message(exception)
           }
         )}
    catch
      kind, value ->
        {:error,
         Diagnostic.new(
           "ENT003",
           "entry #{entry.name} failed with #{kind}",
           path: "$.entries",
           details: %{entry: entry.name, trap: "#{kind}", reason: inspect(value)}
         )}
    end
  end

  defp entry_error(reason, entry, details) do
    Diagnostic.new(
      "ENT001",
      "entry #{inspect(entry.name)} is invalid: #{String.replace(reason, "_", " ")}",
      path: "$.entries",
      details: Map.merge(%{reason: reason, entry: entry.name}, details)
    )
  end
end
