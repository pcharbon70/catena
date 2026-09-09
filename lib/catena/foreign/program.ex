defmodule Catena.Foreign.Program do
  @moduledoc "Checked host binding for retained closed capability core, without new source vocabulary."
  alias Catena.Foreign.{Adapter, Codec, Descriptor}
  alias Catena.Calling.Descriptor, as: Identity

  def describe(%{format: :kernel_core, version: "0.1.50"} = core, name, bindings)
      when is_map(bindings) do
    with :ok <- Catena.Kernel.Verifier.verify(core),
         true <- name in core.exports.values,
         %{arity: 0} = entry <- Enum.find(core.definitions, &(&1.name == name)),
         {:ok, result} <- Catena.Calling.Adapter.schema(entry.signature),
         {:ok, result} <- Codec.new({:data, result}),
         true <- entry.uses != [],
         true <- Enum.all?(entry.uses, &match?({:capability, _}, &1)),
         slots <- Enum.map(entry.uses, &elem(&1, 1)),
         true <- Enum.sort(slots) == Enum.sort(Map.keys(bindings)),
         true <-
           Enum.all?(bindings, fn {slot, operations} ->
             valid_operations?(core, slot, operations)
           end) do
      {:ok,
       %{
         format: :foreign_program_description,
         version: "0.1.61",
         selection: Catena.LanguageVersion.legacy_selection("0.1.61"),
         core: core,
         name: name,
         bindings: bindings,
         result: result,
         compiler: Identity.compiler_digest()
       }}
    else
      _ -> {:error, :invalid_foreign_program_binding}
    end
  rescue
    _ -> {:error, :invalid_foreign_program_binding}
  end

  def describe(_, _, _), do: {:error, :unsupported_foreign_program_profile}

  defp valid_operations?(core, slot, declarations) when is_map(declarations) do
    effect = core.effects[slot]

    Enum.sort(Map.keys(effect.operations)) == Enum.sort(Map.keys(declarations)) and
      Enum.all?(declarations, fn {name, declaration} ->
        operation = effect.operations[name]

        Descriptor.verify(declaration) == :ok and
          declaration.effect == core.capabilities[slot].family and
          length(operation.parameters) == length(declaration.arguments) and
          Enum.all?(Enum.zip(operation.parameters, declaration.arguments), fn {type, codec} ->
            matches?(type, codec)
          end) and matches?(operation.result, declaration.result)
      end)
  end

  defp valid_operations?(_, _, _), do: false

  defp matches?(type, codec) do
    with {:ok, schema} <- Catena.Calling.Adapter.schema(type),
         {:ok, ^codec} <- Codec.new({:data, schema}),
         do: true,
         else: (_ -> false)
  end

  def build(core, name, bindings) do
    with {:ok, description} <- describe(core, name, bindings),
         :ok <- Catena.ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- Catena.ImplementationLimits.validate_source_arities(core),
         {:ok, forms, symbol} <- Catena.Kernel.Backend.lower_foreign_entry(core, name),
         :ok <- Catena.ImplementationLimits.validate_generated_arities(forms),
         {:ok, module, binary, warnings} <-
           Catena.OTP.Compiler.compile_foreign(
             forms,
             Map.put(description, :forms_digest, Identity.digest(forms)),
             source: core.origin,
             frontend: "foreign-program-0.1.61",
             artifact_version: "0.1.61",
             frontend_version: "0.1.61",
             specification: "0.1.61",
             language_selection: description.selection
           ) do
      {:ok,
       %{
         description: description,
         module: module,
         binary: binary,
         forms: forms,
         symbol: symbol,
         warnings: warnings
       }}
    end
  rescue
    _ -> {:error, :invalid_foreign_program}
  end

  def verify(program) do
    d = program.description

    case build(d.core, d.name, d.bindings) do
      {:ok, ^program} -> :ok
      _ -> {:error, :invalid_foreign_program}
    end
  rescue
    _ -> {:error, :invalid_foreign_program}
  end

  def invoke(program, scope, limits, wait_ms) do
    with :ok <- verify(program),
         true <- Catena.Foreign.Budget.valid?(limits),
         true <- is_integer(wait_ms) and wait_ms >= 0 and wait_ms <= 4_294_967_295,
         :ok <- authorize(program, scope),
         {:module, module} <-
           Catena.OTP.Compiler.load(program.module, ~c"foreign-program", program.binary) do
      handlers =
        Map.new(program.description.bindings, fn {slot, operations} ->
          {slot,
           fn operation, values, continuation ->
             declaration = operations[Atom.to_string(operation)]

             Catena.Effect.Runtime.foreign_request(
               scope,
               declaration,
               values,
               continuation,
               limits,
               wait_ms
             )
           end}
        end)

      result = apply(module, program.symbol, [handlers])
      Codec.from_native(program.description.result, result, limits)
    else
      false -> {:error, :invalid_foreign_program_limits}
      error -> error
    end
  catch
    :throw, {:catena_resource_cancelled, _} = cancellation -> throw(cancellation)
    :error, {:catena_trap, reason} -> {:trap, reason}
    class, reason -> {:trap, {:foreign_program_failure, class, reason}}
  end

  defp authorize(program, scope) do
    program.description.bindings
    |> Map.values()
    |> Enum.flat_map(&Map.values/1)
    |> Enum.reduce_while(:ok, fn declaration, :ok ->
      case Adapter.request(scope, {:authorize, declaration}) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end
end
