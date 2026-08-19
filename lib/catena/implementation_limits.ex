defmodule Catena.ImplementationLimits do
  @moduledoc "Portable minima and bootstrap bounds for Catena conformance."

  alias Catena.Diagnostic

  @fixed_limits [
    %{
      id: :callable_arity,
      classification: :implementation_limit,
      unit: :arguments,
      portable_minimum: 253,
      configured: 253,
      applies_to: "source and generated callable signatures",
      exhaustion: %{kind: :diagnostic, id: "LIM001"}
    },
    %{
      id: :integer_literal_digits,
      classification: :implementation_limit,
      unit: :decimal_digits,
      portable_minimum: 4_096,
      configured: 4_096,
      applies_to: "integer literals in retained JSON and kernel inputs",
      exhaustion: %{kind: :diagnostic, id: "LIM002"}
    },
    %{
      id: :decoded_literal_bytes,
      classification: :implementation_limit,
      unit: :bytes,
      portable_minimum: 65_536,
      configured: 65_536,
      applies_to: "decoded text and byte literals",
      exhaustion: %{kind: :diagnostic, id: "LIM004"}
    },
    %{
      id: :generated_beam_bytes,
      classification: :implementation_limit,
      unit: :bytes,
      portable_minimum: 1_048_576,
      configured: 1_048_576,
      applies_to: "each generated BEAM module",
      exhaustion: %{kind: :diagnostic, id: "LIM003"}
    },
    %{
      id: :kernel_parser_depth,
      classification: :implementation_limit,
      unit: :nesting_levels,
      portable_minimum: 1_024,
      configured: 1_024,
      applies_to: "kernel S-expression parsing",
      exhaustion: %{kind: :diagnostic, id: "SYN003"}
    },
    %{
      id: :mailbox_capacity,
      classification: :runtime_capacity,
      unit: :messages,
      portable_minimum: nil,
      configured: nil,
      applies_to: "deployment runtime memory rather than language acceptance",
      exhaustion: %{kind: :deferred, owner: "G068/G129"}
    }
  ]

  @diagnostic_limits [
    {:pattern_coverage_steps, :analysis_steps, "M004", :diagnostic},
    {:condition_normalization_nodes, :nodes, "CND007", :diagnostic},
    {:trait_resolution_steps, :solver_steps, "TRT008", :diagnostic},
    {:package_specialization_steps, :specialization_steps, "TRT007", :diagnostic},
    {:specification_example_steps, :semantic_steps, "EVD003", :diagnostic},
    {:governance_policy_steps, :policy_steps, "GOV002", :diagnostic_and_denial},
    {:kernel_parser_nodes, :nodes, "SYN003", :diagnostic}
  ]

  @evidence_limits [
    {:condition_fact_nodes, :nodes, :unknown},
    {:condition_fact_branch_steps, :analysis_steps, :unknown},
    {:kernel_reference_steps, :small_steps, :budget_exhausted},
    {:kernel_exploration_transitions, :transitions, :exhausted},
    {:kernel_exploration_configurations, :configurations, :exhausted}
  ]

  @limits @fixed_limits ++
            (for {id, unit, diagnostic, kind} <- @diagnostic_limits do
               %{
                 id: id,
                 classification: :implementation_limit,
                 unit: unit,
                 portable_minimum: 20_000,
                 configured: 20_000,
                 applies_to: Atom.to_string(id),
                 exhaustion: %{kind: kind, id: diagnostic}
               }
             end) ++
            (for {id, unit, result} <- @evidence_limits do
               %{
                 id: id,
                 classification: :evidence_bound,
                 unit: unit,
                 portable_minimum: 20_000,
                 configured: 20_000,
                 applies_to: Atom.to_string(id),
                 exhaustion: %{kind: :inconclusive, result: result}
               }
             end)

  @limits_by_id Map.new(@limits, &{&1.id, &1})

  @spec all() :: [map()]
  def all, do: @limits

  @spec fetch!(atom()) :: map()
  def fetch!(id), do: Map.fetch!(@limits_by_id, id)

  @spec configured(atom()) :: non_neg_integer() | nil
  def configured(id), do: fetch!(id).configured

  @spec portable_minimum(atom()) :: non_neg_integer() | nil
  def portable_minimum(id), do: fetch!(id).portable_minimum

  @spec validate_integer_magnitudes(term(), Catena.SourceSpan.t() | nil) ::
          :ok | {:error, Diagnostic.t()}
  def validate_integer_magnitudes(value, span \\ nil) do
    observed = largest_integer_digits(value, 0)

    if observed <= configured(:integer_literal_digits) do
      :ok
    else
      {:error, limit_diagnostic(:integer_literal_digits, observed, span: span)}
    end
  end

  @spec validate_decoded_literal_bytes(binary(), Catena.SourceSpan.t() | nil) ::
          :ok | {:error, Diagnostic.t()}
  def validate_decoded_literal_bytes(payload, span \\ nil) when is_binary(payload) do
    observed = byte_size(payload)

    if observed <= configured(:decoded_literal_bytes) do
      :ok
    else
      {:error, limit_diagnostic(:decoded_literal_bytes, observed, span: span)}
    end
  end

  @spec validate_source_arities(term()) :: :ok | {:error, Diagnostic.t()}
  def validate_source_arities(value) do
    observed = largest_source_arity(value, 0)

    if observed <= configured(:callable_arity) do
      :ok
    else
      {:error, limit_diagnostic(:callable_arity, observed)}
    end
  end

  @spec validate_generated_arities(term()) :: :ok | {:error, Diagnostic.t()}
  def validate_generated_arities(forms) do
    observed = largest_generated_arity(forms, 0)

    if observed <= 255 do
      :ok
    else
      {:error,
       limit_diagnostic(:callable_arity, observed,
         configured: 255,
         message: "generated BEAM callable arity exceeds the OTP 29 limit"
       )}
    end
  end

  @spec validate_generated_module(binary()) :: :ok | {:error, Diagnostic.t()}
  def validate_generated_module(binary) when is_binary(binary) do
    observed = byte_size(binary)

    if observed <= configured(:generated_beam_bytes) do
      :ok
    else
      {:error, limit_diagnostic(:generated_beam_bytes, observed)}
    end
  end

  @spec details(atom(), non_neg_integer(), keyword()) :: map()
  def details(id, observed, options \\ []) do
    limit = fetch!(id)

    %{
      limit_id: Atom.to_string(id),
      minimum_supported: limit.portable_minimum,
      configured: Keyword.get(options, :configured, limit.configured),
      observed: observed,
      unit: Atom.to_string(limit.unit)
    }
  end

  defp limit_diagnostic(id, observed, options \\ []) do
    limit = fetch!(id)
    diagnostic_id = limit.exhaustion.id

    default_message =
      case id do
        :callable_arity -> "callable arity exceeds the published implementation limit"
        :integer_literal_digits -> "integer literal exceeds the published digit limit"
        :decoded_literal_bytes -> "decoded literal exceeds the published byte limit"
        :generated_beam_bytes -> "generated BEAM module exceeds the published size limit"
      end

    Diagnostic.new(diagnostic_id, Keyword.get(options, :message, default_message),
      span: Keyword.get(options, :span),
      details: details(id, observed, options)
    )
  end

  defp largest_integer_digits(value, largest) when is_integer(value) do
    max(largest, value |> Integer.to_string() |> String.trim_leading("-") |> byte_size())
  end

  defp largest_integer_digits(value, largest) when is_struct(value),
    do: value |> Map.from_struct() |> largest_integer_digits(largest)

  defp largest_integer_digits(value, largest) when is_map(value),
    do: Enum.reduce(value, largest, fn {_key, item}, acc -> largest_integer_digits(item, acc) end)

  defp largest_integer_digits(value, largest) when is_list(value),
    do: Enum.reduce(value, largest, &largest_integer_digits/2)

  defp largest_integer_digits(value, largest) when is_tuple(value),
    do: value |> Tuple.to_list() |> largest_integer_digits(largest)

  defp largest_integer_digits(_value, largest), do: largest

  defp largest_source_arity(value, largest) when is_struct(value),
    do: value |> Map.from_struct() |> largest_source_arity(largest)

  defp largest_source_arity(value, largest) when is_map(value) do
    largest =
      Enum.reduce([:arity, "arity"], largest, fn key, acc ->
        case Map.get(value, key) do
          arity when is_integer(arity) and arity >= 0 -> max(acc, arity)
          _ -> acc
        end
      end)

    largest =
      Enum.reduce([:parameters, "parameters", :arguments, "arguments"], largest, fn key, acc ->
        case Map.get(value, key) do
          items when is_list(items) -> max(acc, length(items))
          _ -> acc
        end
      end)

    Enum.reduce(value, largest, fn {_key, item}, acc -> largest_source_arity(item, acc) end)
  end

  defp largest_source_arity(value, largest) when is_list(value),
    do: Enum.reduce(value, largest, &largest_source_arity/2)

  defp largest_source_arity(value, largest) when is_tuple(value),
    do: value |> Tuple.to_list() |> largest_source_arity(largest)

  defp largest_source_arity(_value, largest), do: largest

  defp largest_generated_arity({:function, _annotation, _name, arity, clauses}, largest),
    do: largest_generated_arity(clauses, max(largest, arity))

  defp largest_generated_arity({:clause, _annotation, parameters, _guards, body}, largest),
    do: largest_generated_arity(body, max(largest, length(parameters)))

  defp largest_generated_arity(value, largest) when is_tuple(value),
    do: value |> Tuple.to_list() |> largest_generated_arity(largest)

  defp largest_generated_arity(value, largest) when is_list(value),
    do: Enum.reduce(value, largest, &largest_generated_arity/2)

  defp largest_generated_arity(_value, largest), do: largest
end
