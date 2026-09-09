defmodule Catena.Standard.Text.Program do
  @moduledoc "Verified retained-source adoption of a closed typed text-operation pipeline."
  alias Catena.Calling.{Adapter, Descriptor}
  alias Catena.Standard.Text, as: Text
  alias Catena.Standard.Binary.Pattern
  alias Catena.Foreign.{Budget, Codec}
  @entry :__catena_text_program

  def build(core, entry, steps, limits) do
    with true <- is_list(steps) and length(steps) <= 253,
         :ok <- Budget.check(steps, limits),
         {:ok, calling} <- Catena.Calling.Artifact.build(core),
         descriptor <- calling.descriptor,
         %{kind: :value} <- Enum.find(descriptor.entries, &(&1.name == entry)),
         definition when not is_nil(definition) <-
           Enum.find(core.definitions, &(&1.name == entry)),
         true <- Adapter.initial_pure?(definition),
         {:ok, input, output} <-
           pure_stage(Map.get(definition, :signature) || definition.scheme.type),
         {:ok, input_schema} <- Adapter.schema(input),
         {:ok, schema} <- Adapter.schema(output),
         {:ok, input_codec} <- Codec.new({:data, input_schema}),
         {:ok, result_schema} <- check_steps(schema, steps),
         :ok <- Catena.ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- Catena.ImplementationLimits.validate_source_arities(core) do
      forms = lower(core)
      symbol = String.to_existing_atom(entry)
      existing = for {:function, _, name, arity, _} <- forms, do: {name, arity}

      if {symbol, 1} in existing and not Enum.any?(existing, &(elem(&1, 0) == @entry)) do
        forms = attach(forms, symbol, steps, limits)

        with {:ok, module, binary, warnings} <-
               Catena.OTP.Compiler.compile(forms,
                 frontend: "text-0.1.66",
                 specification: "0.1.66",
                 artifact_version: "0.1.66",
                 language_selection: Catena.LanguageVersion.legacy_selection("0.1.66"),
                 source: core.origin
               ) do
          {:ok,
           %{
             module: module,
             binary: binary,
             warnings: warnings,
             sidecar: %{
               version: "0.1.66",
               input: input_codec,
               result: result_schema,
               failure: Text.failure_schema(),
               core: Descriptor.digest(core),
               entry: entry,
               steps: steps,
               limits: limits,
               profile: Text.profile(),
               forms: Descriptor.digest(forms),
               compiler: Descriptor.compiler_digest(),
               binary: Descriptor.digest(binary)
             }
           }}
        end
      else
        {:error, :invalid_text_entry_lowering}
      end
    else
      false -> {:error, :invalid_text_program}
      {:error, _} = error -> error
      _ -> {:error, :unsupported_text_entry}
    end
  rescue
    _ -> {:error, :invalid_text_program}
  end

  def verify(artifact, core, entry, steps, limits) do
    case build(core, entry, steps, limits) do
      {:ok, ^artifact} -> :ok
      _ -> {:error, :unverified_text_program}
    end
  end

  def invoke(artifact, core, entry, steps, argument, limits) do
    with :ok <- verify(artifact, core, entry, steps, limits),
         {:ok, _} <- Codec.from_native(artifact.sidecar.input, argument, limits) do
      :global.trans({{__MODULE__, artifact.module}, self()}, fn ->
        with :ok <- load_exact(artifact), do: apply(artifact.module, @entry, [argument])
      end)
    end
  end

  defp load_exact(artifact) do
    {:ok, {module, digest}} = :beam_lib.md5(artifact.binary)

    if :code.is_loaded(module) == false do
      case Catena.OTP.Compiler.load(module, ~c"catena-text", artifact.binary) do
        {:module, ^module} -> :ok
        error -> error
      end
    else
      if module.module_info(:md5) == digest, do: :ok, else: {:error, :text_module_conflict}
    end
  end

  def check_steps(schema, []), do: {:ok, schema}

  def check_steps(schema, [step | rest]) do
    with {:ok, next} <- step_schema(schema, step), do: check_steps(next, rest)
  end

  defp pure_stage({:function, input, [], output}), do: {:ok, input, output}
  defp pure_stage({:function, input, output}), do: {:ok, input, output}
  defp pure_stage(_), do: {:error, :unsupported_text_entry}

  defp step_schema(:integer, :format_integer), do: {:ok, :text}

  defp step_schema({:tuple, schemas}, :concatenate) do
    if Enum.all?(schemas, &(&1 == :text)), do: {:ok, :text}, else: {:error, :ill_typed_text_step}
  end

  defp step_schema({:tuple, schemas}, {:format, roles}) when is_list(roles) do
    expected = %{text: :text, character: :character, integer: :integer, bytes_hex: :bytes}

    if length(schemas) == length(roles) and
         Enum.all?(Enum.zip(schemas, roles), fn {schema, role} -> expected[role] == schema end),
       do: {:ok, :text},
       else: {:error, :ill_typed_text_step}
  end

  defp step_schema(:text, {:normalize, form}) when form in [:nfc, :nfd, :nfkc, :nfkd],
    do: {:ok, :text}

  defp step_schema(:text, {:measure, unit}) when unit in [:byte, :scalar, :grapheme],
    do: {:ok, :integer}

  defp step_schema(:text, {:slice, first, last}) do
    with :ok <- Text.verify_index(first),
         :ok <- Text.verify_index(last),
         true <- first.unit == last.unit,
         do: {:ok, :text},
         else: (_ -> {:error, :mixed_or_invalid_text_indices})
  end

  defp step_schema(:bytes, {:slice_bytes, first, last}) do
    with :ok <- Text.verify_index(first),
         :ok <- Text.verify_index(last),
         true <- first.unit == :byte and last.unit == :byte,
         do: {:ok, :bytes},
         else: (_ -> {:error, :mixed_or_invalid_text_indices})
  end

  defp step_schema(:text, {:encode, encoding}), do: encoding(encoding, :bytes)
  defp step_schema(:bytes, {:decode, encoding}), do: encoding(encoding, :text)

  defp step_schema(:bytes, {:match, pattern}) do
    with :ok <- Pattern.verify(pattern), do: {:ok, {:optional, pattern.captures.schema}}
  end

  defp step_schema(_, _), do: {:error, :ill_typed_text_step}

  defp encoding(encoding, type) do
    if Catena.Standard.Text.Encoding.supported?(encoding),
      do: {:ok, type},
      else: {:error, :unsupported_encoding}
  end

  defp lower(%{format: :kernel_core} = core), do: Catena.Kernel.Backend.lower(core)
  defp lower(core), do: Catena.Backend.ErlangAbstract.lower(core)

  defp attach(forms, source_entry, steps, limits) do
    argument = {:var, 1, :Argument}
    source = {:call, 1, {:atom, 1, source_entry}, [argument]}

    call =
      {:call, 1, {:remote, 1, {:atom, 1, Catena.Standard.Text.Program.Runtime}, {:atom, 1, :run}},
       [source, :erl_parse.abstract(steps), :erl_parse.abstract(limits)]}

    wrapper = {:function, 1, @entry, 1, [{:clause, 1, [argument], [], [call]}]}

    Enum.map(forms, fn
      {:attribute, ann, :export, exports} -> {:attribute, ann, :export, [{@entry, 1} | exports]}
      form -> form
    end) ++ [wrapper]
  end
end

defmodule Catena.Standard.Text.Program.Runtime do
  @moduledoc false
  alias Catena.Standard.Text, as: Text
  alias Catena.Standard.Outcomes, as: Outcome
  @dependent :"catena://outcome-contract/0.1.54::CatenaOutcomeRoles::Dependent"

  def run(value, [], limits) do
    result = Outcome.success(value)
    with :ok <- Catena.Foreign.Budget.check(result, limits), do: {:ok, result}
  end

  def run(value, [step | rest], limits) do
    case apply_step(value, step, limits) do
      {:ok, {:catena_adt, @dependent, 0, {_}} = failure} -> {:ok, failure}
      {:ok, {:catena_adt, @dependent, 1, {next}}} -> run(next, rest, limits)
      {:ok, next} -> run(next, rest, limits)
      error -> error
    end
  end

  defp apply_step(value, :format_integer, limits), do: Text.format([{:integer, value}], limits)
  defp apply_step(value, :concatenate, limits), do: Text.concatenate(Tuple.to_list(value), limits)

  defp apply_step(value, {:format, roles}, limits),
    do: Text.format(Enum.zip(roles, Tuple.to_list(value)), limits)

  defp apply_step(value, {:normalize, form}, limits), do: Text.normalize(value, form, limits)
  defp apply_step(value, {:measure, unit}, limits), do: Text.measure(value, unit, limits)

  defp apply_step(value, {:slice, first, last}, limits),
    do: Text.slice(value, first, last, limits)

  defp apply_step(value, {:slice_bytes, first, last}, limits),
    do: Text.slice_bytes(value, first, last, limits)

  defp apply_step(value, {:encode, encoding}, limits), do: Text.encode(value, encoding, limits)
  defp apply_step(value, {:decode, encoding}, limits), do: Text.decode(value, encoding, limits)

  defp apply_step(value, {:match, pattern}, limits),
    do: Catena.Standard.Binary.Pattern.match(pattern, value, limits)
end
