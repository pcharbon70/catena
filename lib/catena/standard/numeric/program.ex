defmodule Catena.Standard.Numeric.Program do
  @moduledoc "Verified retained-source adoption of a closed typed numeric-operation pipeline."
  alias Catena.Calling.{Adapter, Descriptor}
  alias Catena.Standard.Numeric, as: Numeric
  alias Catena.Foreign.{Budget, Codec}
  @entry :__catena_numeric_program

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
                 frontend: "numeric-0.1.67",
                 specification: "0.1.67",
                 artifact_version: "0.1.67",
                 language_selection: Catena.LanguageVersion.legacy_selection("0.1.67"),
                 source: core.origin
               ) do
          {:ok,
           %{
             module: module,
             binary: binary,
             warnings: warnings,
             sidecar: %{
               version: "0.1.67",
               input: input_codec,
               result: result_schema,
               failure: Numeric.failure_schema(),
               core: Descriptor.digest(core),
               entry: entry,
               steps: steps,
               limits: limits,
               profile: Numeric.profile(),
               forms: Descriptor.digest(forms),
               compiler: Descriptor.compiler_digest(),
               binary: Descriptor.digest(binary)
             }
           }}
        end
      else
        {:error, :invalid_numeric_entry_lowering}
      end
    else
      false -> {:error, :invalid_numeric_program}
      {:error, _} = error -> error
      _ -> {:error, :unsupported_numeric_entry}
    end
  rescue
    _ -> {:error, :invalid_numeric_program}
  end

  def verify(artifact, core, entry, steps, limits) do
    case build(core, entry, steps, limits) do
      {:ok, ^artifact} -> :ok
      _ -> {:error, :unverified_numeric_program}
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
      case Catena.OTP.Compiler.load(module, ~c"catena-numeric", artifact.binary) do
        {:module, ^module} -> :ok
        error -> error
      end
    else
      if module.module_info(:md5) == digest, do: :ok, else: {:error, :numeric_module_conflict}
    end
  end

  def check_steps(schema, []), do: {:ok, schema}

  def check_steps(schema, [step | rest]) do
    with {:ok, next} <- step_schema(schema, step), do: check_steps(next, rest)
  end

  defp pure_stage({:function, input, [], output}), do: {:ok, input, output}
  defp pure_stage({:function, input, output}), do: {:ok, input, output}
  defp pure_stage(_), do: {:error, :unsupported_numeric_entry}

  defp step_schema({:tuple, [:integer, :integer]}, :euclidean),
    do: {:ok, {:tuple, [:integer, :integer]}}

  defp step_schema({:tuple, [:integer, :integer]}, {:integer, op})
       when op in [:add, :subtract, :multiply], do: {:ok, :integer}

  defp step_schema({:tuple, [:float, :float]}, {:float, op})
       when op in [:add, :subtract, :multiply, :divide], do: {:ok, :float}

  defp step_schema({:tuple, [type, type]}, {:primitive, op})
       when type in [:integer, :float] and op in [:add, :subtract, :multiply], do: {:ok, type}

  defp step_schema(:float, :sqrt), do: {:ok, :float}
  defp step_schema(:float, :format_float), do: {:ok, :text}
  defp step_schema(:text, :parse_float), do: {:ok, :float}
  defp step_schema(:integer, :float_from_bits), do: {:ok, :float}
  defp step_schema(:float, :float_bits), do: {:ok, :integer}

  defp step_schema(:integer, {:int_to_float, mode}) when mode in [:exact, :nearest_even],
    do: {:ok, :float}

  defp step_schema(:float, {:float_to_int, mode}) do
    if mode in Numeric.Decimal.modes(), do: {:ok, :integer}, else: {:error, :invalid_rounding}
  end

  defp step_schema({:tuple, [:integer, :integer]}, :decimal), do: {:ok, :numeric_decimal}

  defp step_schema(
         {:tuple, [:integer, :integer, :integer, :integer]},
         {:decimal_pair, op, context}
       )
       when op in [:add, :subtract, :multiply, :divide],
       do: decimal_context(context, :numeric_decimal)

  defp step_schema(:numeric_decimal, :decimal_parts), do: {:ok, {:tuple, [:integer, :integer]}}

  defp step_schema(:numeric_decimal, {:rescale, context}),
    do: decimal_context(context, :numeric_decimal)

  defp step_schema(:float, {:float_to_decimal, context}),
    do: decimal_context(context, :numeric_decimal)

  defp step_schema(:numeric_decimal, {:decimal_to_float, mode})
       when mode in [:exact, :nearest_even], do: {:ok, :float}

  defp step_schema(_, _), do: {:error, :ill_typed_numeric_step}

  defp decimal_context(context, result) do
    with :ok <- Numeric.Decimal.verify_context(context), do: {:ok, result}
  end

  def reference(core, entry, steps, argument, limits) do
    with {:ok, artifact} <- build(core, entry, steps, limits),
         {:ok, semantic} <- Codec.from_native(artifact.sidecar.input, argument, limits) do
      case core do
        %{format: :kernel_core} ->
          case Catena.Kernel.Stepper.run(core, entry, [argument]) do
            {:ok, value, _} -> Catena.Standard.Numeric.Program.Runtime.run(value, steps, limits)
            other -> other
          end

        _ ->
          # The retained ordinary reference represents closed scalar/tuple data
          # directly; validate the same input boundary before evaluating it.
          _ = semantic

          case Catena.Reference.Evaluator.run(core, entry, [argument]) do
            {:ok, value} -> Catena.Standard.Numeric.Program.Runtime.run(value, steps, limits)
            other -> other
          end
      end
    end
  catch
    :error, {:catena_trap, reason} -> {:trap, reason}
  end

  defp lower(%{format: :kernel_core} = core), do: Catena.Kernel.Backend.lower(core)
  defp lower(core), do: Catena.Backend.ErlangAbstract.lower(core)

  defp attach(forms, source_entry, steps, limits) do
    argument = {:var, 1, :Argument}
    source = {:call, 1, {:atom, 1, source_entry}, [argument]}

    call =
      {:call, 1,
       {:remote, 1, {:atom, 1, Catena.Standard.Numeric.Program.Runtime}, {:atom, 1, :run}},
       [source, :erl_parse.abstract(steps), :erl_parse.abstract(limits)]}

    wrapper = {:function, 1, @entry, 1, [{:clause, 1, [argument], [], [call]}]}

    Enum.map(forms, fn
      {:attribute, ann, :export, exports} -> {:attribute, ann, :export, [{@entry, 1} | exports]}
      form -> form
    end) ++ [wrapper]
  end
end

defmodule Catena.Standard.Numeric.Program.Runtime do
  @moduledoc false
  alias Catena.Standard.Outcomes, as: Outcome
  @dependent :"catena://outcome-contract/0.1.54::CatenaOutcomeRoles::Dependent"
  def run(value, [], limits) do
    result = Outcome.success(value)
    with :ok <- Catena.Foreign.Budget.check(result, limits), do: {:ok, result}
  end

  def run(value, [step | rest], limits) do
    case Catena.Standard.Numeric.run(step, value, limits) do
      {:ok, {:catena_adt, @dependent, 0, {_}} = failure} -> {:ok, failure}
      {:ok, {:catena_adt, @dependent, 1, {next}}} -> run(next, rest, limits)
      error -> error
    end
  end
end
