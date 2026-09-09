defmodule Catena.Runtime.Environment.Program do
  @moduledoc "A verified closed entry accepts explicit service authority and installs lexical handlers."
  alias Catena.Runtime.Environment.{Kernel, Schema, Runtime}
  alias Catena.Calling.Descriptor, as: Identity
  alias Catena.Foreign.{Codec, Budget}
  @entry :__catena_environment_entry
  def describe(core, name, bindings, limits) do
    with :ok <- Kernel.verify(core),
         true <- core.version == "0.1.68",
         true <- Budget.valid?(limits),
         true <- is_map(bindings),
         true <- name in core.exports.values,
         %{arity: 0} = entry <- Enum.find(core.definitions, &(&1.name == name)),
         true <- Enum.all?(entry.uses, &match?({:capability, _}, &1)),
         true <- Enum.sort(Enum.map(entry.uses, &elem(&1, 1))) == Enum.sort(Map.keys(bindings)),
         true <-
           Enum.all?(bindings, fn {slot, operations} ->
             valid_operations?(core, slot, operations)
           end),
         {:ok, result} <- Catena.Calling.Adapter.schema(entry.signature),
         {:ok, result} <- Codec.new({:data, result}) do
      services =
        bindings
        |> Map.values()
        |> Enum.flat_map(&Map.values/1)
        |> Enum.map(& &1.service)
        |> Enum.uniq()
        |> Enum.sort()

      {:ok,
       %{
         version: "0.1.68",
         name: name,
         bindings: bindings,
         result: result,
         services: services,
         entry: %{arity: 1, parameter: {:environment_bundle, services}, effects: []},
         limits: limits,
         core: Identity.digest(core),
         compiler: Identity.compiler_digest(),
         profile: Catena.Runtime.Environment.profile()
       }}
    else
      _ -> {:error, :invalid_environment_entry}
    end
  rescue
    _ -> {:error, :invalid_environment_entry}
  end

  defp valid_operations?(core, slot, operations) when is_map(operations) do
    effect = core.effects[slot]

    Enum.sort(Map.keys(effect.operations)) == Enum.sort(Map.keys(operations)) and
      Enum.all?(operations, fn {name, description} ->
        operation = effect.operations[name]

        Schema.verify(description) == :ok and description.family == core.capabilities[slot].family and
          operation.parameters == [Schema.core_type(elem(description.input.schema, 1))] and
          operation.result == Schema.core_type(elem(description.result.schema, 1))
      end)
  end

  defp valid_operations?(_, _, _), do: false

  def build(core, name, bindings, limits) do
    with {:ok, description} <- describe(core, name, bindings, limits),
         :ok <- Catena.ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- Catena.ImplementationLimits.validate_source_arities(core),
         {:ok, forms, symbol} <- lower_entry(core, name, bindings),
         false <- Enum.any?(forms, &match?({:function, _, @entry, _, _}, &1)),
         forms <- attach(forms, symbol, description),
         :ok <- Catena.ImplementationLimits.validate_generated_arities(forms),
         {:ok, module, binary, warnings} <-
           Catena.OTP.Compiler.compile(forms,
             source: core.origin,
             frontend: "environment-entry-0.1.68",
             specification: "0.1.68",
             artifact_version: "0.1.68",
             language_selection: Catena.LanguageVersion.legacy_selection("0.1.68")
           ) do
      {:ok,
       %{
         description: Map.put(description, :forms, Identity.digest(forms)),
         module: module,
         binary: binary,
         warnings: warnings
       }}
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_environment_lowering}
    end
  rescue
    _ -> {:error, :invalid_environment_lowering}
  end

  def verify(artifact, core, name, bindings, limits) do
    case build(core, name, bindings, limits) do
      {:ok, ^artifact} -> :ok
      _ -> {:error, :unverified_environment_artifact}
    end
  end

  def invoke(artifact, core, name, bindings, limits, bundle) do
    with :ok <- verify(artifact, core, name, bindings, limits, bundle) do
      :global.trans({{__MODULE__, artifact.module}, self()}, fn ->
        with :ok <- load_exact(artifact), do: apply(artifact.module, @entry, [bundle])
      end)
    end
  end

  defp verify(artifact, core, name, bindings, limits, bundle) do
    with :ok <- verify(artifact, core, name, bindings, limits),
         do: Runtime.validate_bundle(bundle, artifact.description.services)
  end

  def reference(artifact, core, name, bindings, limits, bundle) do
    with :ok <- verify(artifact, core, name, bindings, limits, bundle),
         {:ok, configuration} <- Catena.Kernel.Stepper.initial(core, name) do
      frames =
        Enum.map(bindings, fn {slot, operations} ->
          {:environment_handler, slot, operations, bundle}
        end)

      configuration = put_in(configuration.processes[0].stack, frames)

      case Catena.Kernel.Stepper.run_configuration(configuration) do
        {:ok, value, _} ->
          with {:ok, _} <- Codec.from_native(artifact.description.result, value, limits),
               do: {:ok, value}

        {:trap, reason, _} ->
          {:trap, reason}

        other ->
          other
      end
    end
  end

  defp lower_entry(core, name, bindings) when map_size(bindings) == 0,
    do: {:ok, Catena.Kernel.Backend.lower(core), {:direct, String.to_existing_atom(name)}}

  defp lower_entry(core, name, _), do: Catena.Kernel.Backend.lower_foreign_entry(core, name)

  defp load_exact(artifact) do
    {:ok, {module, digest}} = :beam_lib.md5(artifact.binary)

    if :code.is_loaded(module) == false do
      case Catena.OTP.Compiler.load(module, ~c"environment-entry", artifact.binary) do
        {:module, ^module} -> :ok
        error -> error
      end
    else
      if module.module_info(:md5) == digest, do: :ok, else: {:error, :environment_module_conflict}
    end
  end

  defp attach(forms, symbol, description) do
    bundle = {:var, 1, :EnvironmentAuthority}

    call =
      {:call, 1, {:remote, 1, {:atom, 1, Runtime}, {:atom, 1, :enter}},
       [bundle, :erl_parse.abstract(description), :erl_parse.abstract(symbol)]}

    # Runtime resolves only this compiled module, carried explicitly by the wrapper.
    call = put_elem(call, 3, [{:atom, 1, module_name(forms)} | elem(call, 3)])
    wrapper = {:function, 1, @entry, 1, [{:clause, 1, [bundle], [], [call]}]}

    Enum.map(forms, fn
      {:attribute, a, :export, exports} -> {:attribute, a, :export, [{@entry, 1} | exports]}
      form -> form
    end) ++ [wrapper]
  end

  defp module_name(forms),
    do:
      Enum.find_value(forms, fn
        {:attribute, _, :module, name} -> name
        _ -> nil
      end)
end

defmodule Catena.Runtime.Environment.Runtime do
  @moduledoc false
  alias Catena.Runtime.Environment, as: E
  alias Catena.Foreign.Codec

  def validate_bundle(
        %{
          format: :environment_bundle,
          version: "0.1.68",
          manager: manager,
          authorities: authorities
        } = bundle,
        services
      )
      when map_size(bundle) == 4 and is_pid(manager) and is_map(authorities) do
    Enum.reduce_while(services, :ok, fn service, :ok ->
      result =
        with {:ok, ^manager, _, _} <-
               Catena.Runtime.Environment.Authority.decode(authorities[service]),
             do: E.validate(authorities[service], service)

      case result do
        :ok -> {:cont, :ok}
        {:error, _} = error -> {:halt, error}
        _ -> {:halt, {:error, :invalid_environment_bundle}}
      end
    end)
  end

  def validate_bundle(_, _), do: {:error, :invalid_environment_bundle}

  def enter(module, bundle, description, symbol) do
    with :ok <- validate_bundle(bundle, description.services) do
      handlers =
        Map.new(description.bindings, fn {slot, operations} ->
          {slot,
           fn operation, arguments, continuation ->
             case request(bundle, operations, operation, arguments) do
               {:ok, value} -> continuation.(value)
               {:error, reason} -> :erlang.error({:catena_trap, {:environment_boundary, reason}})
             end
           end}
        end)

      result =
        case symbol do
          {:direct, name} -> apply(module, name, [])
          symbol when is_atom(symbol) -> apply(module, symbol, [handlers])
        end

      with {:ok, _} <- Codec.from_native(description.result, result, description.limits),
           do: {:ok, result}
    end
  end

  def request(bundle, operations, operation, [argument]) do
    with %{service: service, operation: role} = description <-
           operations[if(is_atom(operation), do: Atom.to_string(operation), else: operation)],
         :ok <- Catena.Runtime.Environment.Schema.verify(description) do
      E.call(bundle.authorities[service], role, argument)
    else
      _ -> {:error, :unbound_environment_operation}
    end
  end

  def request(_, _, _, _), do: {:error, :invalid_environment_arguments}
end
