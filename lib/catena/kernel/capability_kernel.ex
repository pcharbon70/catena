defmodule Catena.Kernel.CapabilityKernel do
  @moduledoc """
  Closed-slot 0.1.50 semantic model using the retained parser as a quoted syntax decoder.

  Explicit slot-to-family bindings give the compound input its identity-bearing
  meaning. Bare 0.1.8 source remains unchanged. No historical interface or
  signed format is extended by this boundary.
  """
  alias Catena.{Diagnostic, Kernel.CapabilityBinding, Kernel.Checker, Kernel.Parser}
  @version "0.1.50"

  def check(source, bindings, options \\ []) do
    with {:ok, _selection} <- selection(options),
         {:ok, parsed} <- Parser.parse(source),
         {:ok, module} <- prepare(parsed, bindings) do
      Checker.check(module)
    end
  end

  def prepare(parsed, bindings) when is_map(bindings) do
    names = Enum.map(parsed.effects, & &1.name)

    if Enum.sort(names) == Enum.sort(Map.keys(bindings)) and
         Enum.all?(bindings, fn {_, family} ->
           is_binary(family) and family != "" and String.valid?(family)
         end) do
      index =
        parsed.effects
        |> Enum.with_index()
        |> Map.new(fn {effect, position} ->
          {:ok, slot} = CapabilityBinding.identity(parsed.origin, parsed.module, [position])
          {effect.name, %{slot: slot, family: bindings[effect.name], position: position}}
        end)

      module = convert(parsed, index)
      capabilities = Map.new(index, fn {_, entry} -> {entry.slot, entry} end)

      module =
        Map.merge(module, %{
          version: @version,
          frontend_format: @version,
          frontend_version: @version,
          language_revision: @version,
          capabilities: capabilities
        })

      if descriptors_valid?(module),
        do: {:ok, module},
        else: error("inconsistent capability family descriptors")
    else
      error("every capability slot requires exactly one family binding")
    end
  rescue
    _ -> error("malformed capability-kernel input")
  end

  def prepare(_, _), do: error("expected capability family bindings")

  def selection(options) do
    requested =
      Keyword.get(options, :language_selection, %Catena.LanguageSelection{
        edition: "0.1",
        language_revision: @version,
        previews: []
      })

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{edition: "0.1", language_revision: @version, previews: []} = selection} ->
        {:ok, selection}

      {:ok, _} ->
        {:error, Diagnostic.new("EDN001", "closed capability-tree input requires exact 0.1.50")}

      error ->
        error
    end
  end

  def compile(core) do
    alias Catena.{ImplementationLimits, Kernel.Backend, Kernel.Verifier, OTP.Compiler}

    with :ok <- Verifier.verify(core),
         true <- core.version == @version,
         %{arity: 0} = entry <- Enum.find(core.definitions, &(&1.name == "main")),
         true <- MapSet.size(slots([entry.signature, entry.uses])) == 0,
         :ok <- ImplementationLimits.validate_source_arities(core),
         forms <- Backend.lower(core),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, selected} <- selection([]),
         {:ok, module, binary, warnings} <-
           Compiler.compile(forms,
             source: core.origin,
             artifact_version: @version,
             frontend_version: @version,
             frontend: "closed-capability-tree-0.1.50",
             specification: @version,
             language_selection: selected
           ) do
      {:ok, module, binary,
       %{
         core: core,
         forms: forms,
         warnings: warnings,
         diagnostics: core.diagnostics,
         selection: selected,
         artifact_version: @version,
         layout: :fixed,
         interface: nil,
         interface_binary: nil
       }}
    else
      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      {:error, reason} ->
        {:error, Diagnostic.new("I001", "capability-core verification failed: #{reason}")}

      _ ->
        {:error,
         Diagnostic.new("EFX003", "capability artifact requires a fully handled main entry")}
    end
  end

  def version, do: @version

  # A verifier gate, not inference: check slot derivation, descriptors, visibility and escape.
  def verify_scope(%{version: version} = core)
      when version in ["0.1.52", "0.1.53", :owned_task_experiment] do
    core =
      Enum.reduce(
        [:version, :frontend_format, :frontend_version, :language_revision],
        core,
        &Map.put(&2, &1, "0.1.51")
      )

    verify_scope(core)
  end

  def verify_scope(%{version: "0.1.51"} = core) do
    core =
      Enum.reduce(
        [:version, :frontend_format, :frontend_version, :language_revision],
        core,
        &Map.put(&2, &1, @version)
      )

    verify_scope(core)
  end

  def verify_scope(%{version: @version} = core) do
    index = core.capabilities
    effects = entries(core.effects)

    valid_index =
      Enum.all?(index, fn {slot, entry} ->
        CapabilityBinding.identity(core.origin, core.module, [entry.position]) == {:ok, slot} and
          entry.slot == slot and is_binary(entry.family) and entry.family != "" and
          String.valid?(entry.family)
      end)

    valid_effects =
      Enum.all?(effects, fn effect ->
        Map.has_key?(index, effect.name) and effect.occurrence == {:capability, effect.name} and
          Enum.all?(entries(effect.operations), fn operation ->
            Enum.all?(
              [operation.result | operation.parameters],
              &(MapSet.size(Catena.Kernel.Type.variables(&1)) == 0)
            )
          end)
      end)

    valid_handlers =
      Enum.all?(entries(core.handlers), fn handler ->
        Map.has_key?(index, handler.effect) and handler.capability_dispatch == true
      end)

    known = MapSet.new(Map.keys(index))

    valid_definitions =
      Enum.all?(core.definitions, fn definition ->
        bound = MapSet.union(slots(definition.signature), slots(definition.uses))
        MapSet.subset?(bound, known) and scope?(definition.expression, bound, core)
      end)

    valid_processes = Enum.all?(core.processes, &scope?(&1.body, MapSet.new(), core))

    if core.language_revision == @version and core.frontend_version == @version and
         core.frontend_format == @version and core.edition == "0.1" and core.previews == [] and
         not ordinary_rows?(core) and valid_rows?(core, known) and valid_index and valid_effects and
         valid_handlers and
         valid_definitions and valid_processes and
         MapSet.new(Enum.map(effects, & &1.name)) == known and descriptors_valid?(core),
       do: :ok,
       else: {:error, "capability identity, lexical scope or escape evidence is inconsistent"}
  rescue
    _ -> {:error, "malformed capability evidence"}
  end

  def verify_scope(%{version: "0.1.58"} = core),
    do: verify_scope(%{core | version: "0.1.8"})

  def verify_scope(%{version: "0.1.8"} = core) do
    if Map.has_key?(core, :capabilities) or MapSet.size(slots(core)) > 0 or marked?(core),
      do: {:error, "capability evidence is not part of retained kernel 0.1.8"},
      else: :ok
  end

  def verify_scope(_), do: {:error, "unknown capability profile"}

  def slots({:capability, slot}), do: MapSet.new([slot])
  def slots(%_{}), do: MapSet.new()
  def slots(value) when is_map(value), do: value |> Map.values() |> slots()
  def slots(value) when is_tuple(value), do: value |> Tuple.to_list() |> slots()

  def slots(value) when is_list(value),
    do: Enum.reduce(value, MapSet.new(), &MapSet.union(slots(&1), &2))

  def slots(_), do: MapSet.new()

  defp scope?(%{tag: :handle} = expression, bound, core) do
    handler = Map.fetch!(core.handlers, expression.handler)
    slot = handler.effect

    not MapSet.member?(bound, slot) and visible?(expression, bound) and
      scope?(expression.expression, MapSet.put(bound, slot), core)
  end

  defp scope?(%{tag: :request} = expression, bound, core) do
    expression.capability_dispatch == true and MapSet.member?(bound, expression.effect) and
      visible?(expression, bound) and scope?(expression.arguments, bound, core)
  end

  defp scope?(%_{}, _bound, _core), do: true

  defp scope?(value, bound, core) when is_map(value) do
    visible?(value, bound) and
      Enum.all?(Map.drop(value, [:selected_handler, :span]), fn {_key, child} ->
        scope?(child, bound, core)
      end)
  end

  defp scope?(values, bound, core) when is_list(values),
    do: Enum.all?(values, &scope?(&1, bound, core))

  defp scope?(_value, _bound, _core), do: true

  defp visible?(expression, bound),
    do:
      MapSet.subset?(
        slots([Map.get(expression, :type), Map.get(expression, :effects, [])]),
        bound
      )

  defp convert(%Catena.SourceSpan{} = span, _index), do: span
  defp convert({:effect, name}, index), do: {:capability, Map.fetch!(index, name).slot}

  defp convert(value, index) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.map(&convert(&1, index)) |> List.to_tuple()

  defp convert(value, index) when is_list(value), do: Enum.map(value, &convert(&1, index))

  defp convert(value, index) when is_map(value) do
    converted = Map.new(value, fn {key, child} -> {key, convert(child, index)} end)

    cond do
      value[:kind] == :effect ->
        slot = Map.fetch!(index, value.name).slot
        Map.merge(converted, %{name: slot, occurrence: {:capability, slot}})

      value[:kind] == :handler or value[:tag] == :request ->
        Map.merge(converted, %{
          effect: Map.fetch!(index, value.effect).slot,
          capability_dispatch: true
        })

      true ->
        converted
    end
  end

  defp convert(value, _index), do: value

  defp descriptors_valid?(module) do
    index = module.capabilities

    entries(module.effects)
    |> Enum.group_by(&index[&1.name].family)
    |> Enum.all?(fn {_family, effects} ->
      effects
      |> Enum.map(fn effect -> effect.operations |> entries() |> strip_spans() |> Enum.sort() end)
      |> Enum.uniq()
      |> length() == 1
    end)
  end

  defp entries(value) when is_map(value), do: Map.values(value)
  defp entries(value) when is_list(value), do: value

  defp strip_spans(value) when is_map(value),
    do: value |> Map.delete(:span) |> Map.new(fn {k, v} -> {k, strip_spans(v)} end)

  defp strip_spans(value) when is_list(value), do: Enum.map(value, &strip_spans/1)
  defp strip_spans(value), do: value

  defp valid_row?(row, known) when is_list(row),
    do:
      Enum.all?(row, fn
        :process -> true
        {:capability, slot} -> MapSet.member?(known, slot)
        _ -> false
      end)

  defp valid_row?(_, _), do: false

  defp valid_rows?({:function, parameter, row, result}, known),
    do: valid_row?(row, known) and valid_rows?(parameter, known) and valid_rows?(result, known)

  defp valid_rows?(%_{}, _known), do: true

  defp valid_rows?(value, known) when is_map(value) do
    effects = Map.get(value, :effects, [])

    (is_map(effects) or valid_row?(effects, known)) and
      valid_row?(Map.get(value, :uses, []), known) and
      Enum.all?(Map.values(value), &valid_rows?(&1, known))
  end

  defp valid_rows?(value, known) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.all?(&valid_rows?(&1, known))

  defp valid_rows?(value, known) when is_list(value),
    do: Enum.all?(value, &valid_rows?(&1, known))

  defp valid_rows?(_, _known), do: true

  defp ordinary_rows?({:effect, _}), do: true
  defp ordinary_rows?(%_{}), do: false

  defp ordinary_rows?(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.any?(&ordinary_rows?/1)

  defp ordinary_rows?(value) when is_map(value),
    do: value |> Map.values() |> Enum.any?(&ordinary_rows?/1)

  defp ordinary_rows?(value) when is_list(value), do: Enum.any?(value, &ordinary_rows?/1)
  defp ordinary_rows?(_), do: false

  defp marked?(%_{}), do: false

  defp marked?(value) when is_map(value),
    do:
      Map.has_key?(value, :occurrence) or Map.has_key?(value, :capability_dispatch) or
        Enum.any?(Map.values(value), &marked?/1)

  defp marked?(value) when is_list(value), do: Enum.any?(value, &marked?/1)
  defp marked?(_), do: false
  defp error(message), do: {:error, Diagnostic.new("T002", message)}
end
