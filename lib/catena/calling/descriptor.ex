defmodule Catena.Calling.Descriptor do
  @moduledoc "Exact 0.1.59 call-shape sidecar derived from verified core and actual lowering."

  def build(core, options \\ []) do
    with {:ok, selection} <- selection(options),
         {:ok, family, forms, interface, entries} <- derive(core, options),
         :ok <- Catena.ImplementationLimits.validate_generated_arities(forms),
         {:ok, host} <- Catena.OTP.Profile.require_supported() do
      exported = for {:attribute, _, :export, names} <- forms, name <- names, do: name

      origins =
        Enum.find_value(forms, %{}, fn
          {:attribute, _, :catena_calling_origins, origins} -> origins
          _ -> nil
        end)

      functions =
        for {:function, annotation, name, arity, _} <- forms do
          %{
            name: Atom.to_string(name),
            arity: arity,
            exported: {name, arity} in exported,
            source: Map.get(origins, {name, arity}),
            line: :erl_anno.line(annotation)
          }
        end

      if Enum.all?(entries, fn entry ->
           Enum.any?(
             functions,
             &(&1.name == entry.symbol and &1.arity == entry.beam_arity and &1.exported)
           )
         end) do
        description = %{
          format: :calling_descriptor,
          version: "0.1.59",
          selection: selection,
          family: family,
          module: core.module,
          origin: core.origin,
          core_digest: digest(core),
          forms_digest: digest(forms),
          interface_digest: interface && interface["digest"],
          compiler_digest: compiler_digest(),
          toolchain_digest: Catena.OTP.Profile.digest(host),
          entries: Enum.sort_by(entries, &{&1.kind, &1.name}),
          functions: Enum.sort_by(functions, &{&1.name, &1.arity}),
          unadmitted: [:general_foreign_call, :general_foreign_callback]
        }

        {:ok, Map.put(description, :digest, digest(description))}
      else
        {:error, :entry_lowering_mismatch}
      end
    end
  rescue
    _ -> {:error, :invalid_call_description}
  end

  def selection(options) do
    requested =
      Keyword.get(options, :language_selection, Catena.LanguageVersion.legacy_selection("0.1.59"))

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{language_revision: "0.1.59", previews: []} = selection} -> {:ok, selection}
      _ -> {:error, :invalid_calling_selection}
    end
  end

  def verify(description, core, options \\ []) do
    case build(core, options) do
      {:ok, ^description} -> :ok
      _ -> {:error, :invalid_call_description}
    end
  end

  defp derive(%{format: :kernel_core, version: version} = core, _options)
       when version in ["0.1.8", "0.1.58"] do
    with :ok <- Catena.Kernel.Verifier.verify(core) do
      entries =
        for definition <- core.definitions, definition.name in core.exports.values do
          entry(
            definition.name,
            definition.name,
            definition.arity,
            definition.signature,
            :value,
            Map.get(definition, :span)
          )
          |> Map.put(:initial_effects, Map.get(definition, :uses))
        end

      processes =
        for process <- core.processes, process.name in core.exports.processes do
          %{
            name: process.name,
            symbol: process.spawn_symbol,
            beam_arity: length(process.parameters),
            semantic_parameters: Enum.map(process.parameters, & &1.type),
            result_type: {:process, process.mailbox},
            kind: :process_entry,
            span: process.span
          }
        end

      interface = if core.version == "0.1.8", do: Catena.Kernel.Interface.build(core), else: nil

      {:ok, :kernel, Catena.Kernel.Backend.lower(core, calling: true), interface,
       entries ++ processes}
    end
  end

  defp derive(%{format: :kernel_core}, _options), do: {:error, :unsupported_call_profile}

  defp derive(core, options) do
    with :ok <- Catena.TypedCore.Verifier.verify(core) do
      entries =
        for definition <- core.definitions, definition.name in core.exports do
          entry(
            definition.name,
            definition.name,
            length(definition.parameters),
            definition.scheme.type,
            :value,
            Map.get(definition, :span)
          )
          |> Map.put(:initial_effects, Map.get(definition, :uses))
        end

      {:ok, :ordinary,
       Catena.Backend.ErlangAbstract.lower(core, Keyword.put(options, :calling, true)),
       Catena.Interface.build(core), entries}
    end
  end

  defp entry(name, symbol, arity, type, kind, span) do
    {parameters, result} = function_spine(type)

    %{
      name: name,
      symbol: symbol,
      beam_arity: arity,
      factory: Atom.to_string(Catena.Calling.Lowering.factory(name)),
      stages: stages(type),
      semantic_parameters: parameters,
      result_type: result,
      kind: kind,
      span: span
    }
  end

  defp stages({:function, parameter, effects, result}),
    do: [
      %{
        parameter: parameter,
        effects: effects,
        result: result,
        native_arity: if(effects == [], do: 1, else: 3),
        hidden_arguments: if(effects == [], do: 0, else: 2)
      }
      | stages(result)
    ]

  defp stages({:function, parameter, result}),
    do: [
      %{parameter: parameter, effects: [], result: result, native_arity: 1, hidden_arguments: 0}
      | stages(result)
    ]

  defp stages(_), do: []

  defp function_spine({:function, parameter, result}) do
    {rest, result} = function_spine(result)
    {[parameter | rest], result}
  end

  defp function_spine({:function, parameter, _effects, result}) do
    {rest, result} = function_spine(result)
    {[parameter | rest], result}
  end

  defp function_spine(type), do: {[], type}

  def compiler_digest do
    # Bind the complete loaded Catena compiler build, including verifier and runtime modules.
    {:ok, modules} = :application.get_key(:catena, :modules)

    modules
    |> Enum.sort()
    |> Enum.map(fn module ->
      Code.ensure_loaded!(module)
      {module, module.module_info(:md5)}
    end)
    |> digest()
  end

  def digest(value),
    do:
      :crypto.hash(:sha256, :erlang.term_to_binary(value, [:deterministic]))
      |> Base.encode16(case: :lower)
end
