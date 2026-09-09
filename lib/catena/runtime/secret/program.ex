defmodule Catena.Runtime.Secret.Program do
  @moduledoc "Closed checked credential entry over the retained Int/Unit capability kernel."
  alias Catena.Runtime.Secret
  alias Catena.Calling.Descriptor, as: Identity
  @family "catena://secret/0.1.72"
  @roles %{
    fetch: {[:integer], :integer},
    base64: {[:integer], :integer},
    hex: {[:integer], :integer},
    deliver: {[:integer, :integer], :unit}
  }
  def family, do: @family

  def build(core, name, bindings, providers, recipients) do
    with :ok <- Catena.Kernel.Verifier.verify(core),
         :ok <- Catena.ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- Catena.ImplementationLimits.validate_source_arities(core),
         true <- core.version == "0.1.50" and core.processes == [] and core.imports == [],
         true <- name in core.exports.values,
         %{arity: 0, signature: :unit, uses: uses} <-
           Enum.find(core.definitions, &(&1.name == name)),
         true <- uses != [] and Enum.all?(uses, &match?({:capability, _}, &1)),
         true <-
           is_map(bindings) and
             Enum.sort(Map.keys(bindings)) == Enum.sort(Enum.map(uses, &elem(&1, 1))),
         true <- names?(providers) and names?(recipients),
         true <-
           Enum.all?(bindings, fn {slot, operations} -> valid_binding?(core, slot, operations) end),
         {:ok, forms, symbol} <- Catena.Kernel.Backend.lower_foreign_entry(core, name),
         :ok <- Catena.ImplementationLimits.validate_generated_arities(forms),
         {:ok, module, binary, warnings} <-
           Catena.OTP.Compiler.compile(forms,
             source: core.origin,
             frontend: "secret-entry-0.1.72",
             specification: "0.1.72",
             artifact_version: "0.1.72",
             language_selection: Catena.LanguageVersion.legacy_selection("0.1.72")
           ) do
      description = %{
        version: "0.1.72",
        profile: Secret.profile(),
        core: core,
        name: name,
        bindings: bindings,
        providers: providers,
        recipients: recipients,
        compiler: Identity.compiler_digest(),
        forms: Identity.digest(forms)
      }

      {:ok,
       %{
         description: description,
         module: module,
         binary: binary,
         symbol: symbol,
         warnings: warnings
       }}
    else
      _ -> {:error, :invalid_secret_program}
    end
  rescue
    _ -> {:error, :invalid_secret_program}
  end

  def verify(program) do
    d = program.description

    case build(d.core, d.name, d.bindings, d.providers, d.recipients) do
      {:ok, ^program} -> :ok
      _ -> {:error, :invalid_secret_program}
    end
  rescue
    _ -> {:error, :invalid_secret_program}
  end

  defp names?(xs),
    do:
      is_list(xs) and length(xs) <= 64 and xs == Enum.uniq(xs) and
        Enum.all?(xs, &(is_binary(&1) and byte_size(&1) in 1..128 and String.valid?(&1)))

  defp valid_binding?(core, slot, ops) do
    core.capabilities[slot].family == @family and is_map(ops) and
      Enum.sort(Map.keys(ops)) == Enum.sort(Map.keys(core.effects[slot].operations)) and
      Enum.all?(ops, fn {name, role} ->
        op = core.effects[slot].operations[name]
        @roles[role] == {op.parameters, op.result}
      end)
  end

  def invoke(program, scope) do
    with :ok <- verify(program), {:ok, _} <- Secret.audit(scope), :ok <- load(program) do
      key = make_ref()
      Process.put(key, %{})

      try do
        handlers =
          Map.new(program.description.bindings, fn {slot, ops} ->
            {slot,
             fn operation, arguments, continuation ->
               case perform(
                      ops[Atom.to_string(operation)],
                      arguments,
                      program.description,
                      scope,
                      key
                    ) do
                 {:ok, value} -> continuation.(value)
                 _ -> :erlang.error({:catena_trap, :secret_request_denied})
               end
             end}
          end)

        case apply(program.module, program.symbol, [handlers]) do
          :unit -> {:ok, :unit}
          _ -> {:error, :invalid_secret_entry_result}
        end
      catch
        _, _ -> {:error, :secret_program_failed}
      after
        Process.delete(key)
      end
    end
  end

  defp load(p) do
    {:ok, {module, digest}} = :beam_lib.md5(p.binary)

    case :code.is_loaded(module) do
      false ->
        case Catena.OTP.Compiler.load(module, ~c"secret-entry", p.binary) do
          {:module, ^module} -> :ok
          _ -> {:error, :secret_module_conflict}
        end

      _ ->
        if module.module_info(:md5) == digest, do: :ok, else: {:error, :secret_module_conflict}
    end
  end

  defp perform(:fetch, [index], d, scope, key) when is_integer(index) and index >= 0 do
    with name when is_binary(name) <- Enum.at(d.providers, index),
         {:ok, ref} <- Secret.fetch(scope, name),
         do: store(key, ref)
  end

  defp perform(role, [index], _, scope, key) when role in [:base64, :hex] do
    with %Secret.Ref{} = ref <- Process.get(key)[index],
         {:ok, derived} <- Secret.derive(scope, {role, ref}),
         do: store(key, derived)
  end

  defp perform(:deliver, [recipient, index], d, scope, key)
       when is_integer(recipient) and recipient >= 0 do
    with name when is_binary(name) <- Enum.at(d.recipients, recipient),
         %Secret.Ref{} = ref <- Process.get(key)[index],
         {:ok, %Secret.Ref{}} <- Secret.deliver(scope, ref, name),
         do: {:ok, :unit}
  end

  defp perform(_, _, _, _, _), do: {:error, :invalid_secret_operation}

  defp store(key, ref) do
    refs = Process.get(key)
    index = map_size(refs)
    Process.put(key, Map.put(refs, index, ref))
    {:ok, index}
  end
end
