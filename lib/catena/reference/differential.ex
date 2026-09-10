defmodule Catena.Reference.Differential do
  @moduledoc """
  Subject-bound differential observations over independent reference and
  production adapters.

  Exact comparisons and allowed-set membership have distinct contracts. The
  module produces bounded test evidence and does not claim proof.
  """

  alias Catena.{CanonicalJCS, Tool.TestRunner}

  @version "0.1.85"
  @default_fields ~w(status value reason events lifetime)
  @faults ~w(value event_order callback_count cancellation)a
  @max_scenarios 10_000

  def profile do
    %{
      version: @version,
      comparisons: [:exact_observation, :allowed_observation_set],
      generated_domains: [:typed_kernel, :comprehension],
      boundary_families: [:effects, :schedules, :resources, :foreign_values, :cancellation],
      adversarial_faults: @faults,
      retained_counterexamples: true,
      maximum_scenarios: @max_scenarios,
      public_source: :held_for_p109,
      agreement_is_proof: false
    }
  end

  def run(id, subject_digest, generator, adapters, options \\ []) do
    with :ok <- valid_generator(generator),
         :ok <- valid_adapters(adapters),
         {:ok, observations} <- positive(Keyword.get(options, :observations, 100)),
         true <- observations <= @max_scenarios do
      fields = Keyword.get(options, :fields, @default_fields)
      comparison = Keyword.get(options, :comparison, :exact_observation)

      test_case = %{
        id: "generated-differential",
        kind: :property,
        runs: observations,
        size: Keyword.get(options, :size, 100),
        shrink_limit: Keyword.get(options, :shrink_limit, 100),
        host_timeout: Keyword.get(options, :host_timeout, 5_000),
        generate: generator.generate,
        valid: generator.valid,
        shrink: generator.shrink,
        check: fn scenario, _context ->
          check(scenario, adapters,
            fields: fields,
            comparison: comparison,
            subject_digest: subject_digest
          )
        end
      }

      with {:ok, suite} <-
             TestRunner.define(id, subject_digest, [test_case],
               seed: Keyword.fetch!(options, :seed),
               host_timeout: Keyword.get(options, :host_timeout, 5_000)
             ) do
        TestRunner.run(suite, subject_digest)
      end
    else
      false -> {:error, :differential_scenario_limit}
      error -> error
    end
  rescue
    KeyError -> {:error, :seed_required}
    _ -> {:error, :invalid_differential_suite}
  end

  def check(scenario, adapters, options \\ []) do
    with true <- adapters.supports?.(scenario),
         {:ok, reference} <- invoke(adapters.reference, scenario),
         {:ok, production} <- invoke(adapters.production, scenario),
         {:ok, result} <- compare(reference, production, options) do
      case result do
        %{agreement: true} ->
          {:pass,
           %{
             "scenario_digest" => digest(scenario),
             "subject_digest" => Keyword.fetch!(options, :subject_digest),
             "comparison" => Atom.to_string(result.comparison),
             "reference_digest" => digest(result.reference),
             "production_digest" => digest(result.production)
           }}

        %{agreement: false} ->
          {:fail,
           %{
             "scenario" => portable(scenario),
             "scenario_digest" => digest(scenario),
             "reference" => result.reference,
             "production" => result.production,
             "difference" => result.difference
           }}
      end
    else
      false -> {:fail, %{"reason" => "unsupported-generated-scenario"}}
      {:error, reason} -> {:fail, %{"reason" => portable(reason)}}
    end
  rescue
    KeyError -> {:fail, %{"reason" => "subject-digest-required"}}
  end

  def compare(reference, production, options \\ []) do
    fields = Keyword.get(options, :fields, @default_fields)
    comparison = Keyword.get(options, :comparison, :exact_observation)
    reference = project(reference, fields)
    production = project(production, fields)

    case comparison do
      :exact_observation ->
        {:ok,
         %{
           agreement: reference == production,
           comparison: comparison,
           reference: reference,
           production: production,
           difference: differences(reference, production)
         }}

      :allowed_observation_set when is_list(reference) ->
        allowed = Enum.map(reference, &portable/1)

        {:ok,
         %{
           agreement: production in allowed,
           comparison: comparison,
           reference: allowed,
           production: production,
           difference: if(production in allowed, do: [], else: ["outside-allowed-set"])
         }}

      _ ->
        {:error, :invalid_comparison}
    end
  rescue
    _ -> {:error, :invalid_observation}
  end

  def mutate(observation, :value) when is_map(observation),
    do: put_field(observation, :value, %{"injected" => "wrong-value"})

  def mutate(observation, :event_order) when is_map(observation) do
    events = field(observation, :events, [])
    put_field(observation, :events, Enum.reverse(events))
  end

  def mutate(observation, :callback_count) when is_map(observation) do
    count = field(observation, :callback_count, 0)
    put_field(observation, :callback_count, count + 1)
  end

  def mutate(observation, :cancellation) when is_map(observation),
    do: put_field(observation, :status, :completed)

  def mutate(_observation, fault) when fault in @faults, do: {:error, :invalid_observation}
  def mutate(_observation, _fault), do: {:error, :unknown_injected_fault}

  def verify_corpus(document) when is_map(document) do
    digest = document["digest"]
    payload = Map.delete(document, "digest")

    if document["format"] == "catena-differential-counterexamples" and
         document["contract"] == @version and document["version"] == 1 and
         is_list(document["cases"]) and document["cases"] != [] and
         length(document["cases"]) <= @max_scenarios and
         Enum.all?(document["cases"], &valid_corpus_case?/1) and
         digest == CanonicalJCS.digest(payload),
       do: :ok,
       else: {:error, :invalid_counterexample_corpus}
  rescue
    _ -> {:error, :invalid_counterexample_corpus}
  end

  def verify_corpus(_), do: {:error, :invalid_counterexample_corpus}

  defp valid_corpus_case?(entry) do
    is_map(entry) and
      Enum.sort(Map.keys(entry)) ==
        ~w(fault minimized observation_digest scenario_digest source toolchain_digest) and
      entry["fault"] in Enum.map(@faults, &Atom.to_string/1) and
      is_boolean(entry["minimized"]) and entry["minimized"] and
      valid_digest?(entry["observation_digest"]) and valid_digest?(entry["scenario_digest"]) and
      valid_digest?(entry["toolchain_digest"]) and is_binary(entry["source"])
  end

  defp valid_generator(generator) do
    if is_map(generator) and is_function(generator[:generate], 2) and
         is_function(generator[:valid], 1) and is_function(generator[:shrink], 1),
       do: :ok,
       else: {:error, :invalid_semantic_generator}
  end

  defp valid_adapters(adapters) do
    if is_map(adapters) and is_function(adapters[:supports?], 1) and
         is_function(adapters[:reference], 1) and is_function(adapters[:production], 1),
       do: :ok,
       else: {:error, :invalid_differential_adapters}
  end

  defp invoke(adapter, scenario) do
    case adapter.(scenario) do
      {:ok, observation} when is_map(observation) or is_list(observation) ->
        {:ok, observation}

      {:error, reason} ->
        {:error, reason}

      _ ->
        {:error, :invalid_adapter_result}
    end
  rescue
    error -> {:error, {:adapter_crashed, Exception.message(error)}}
  catch
    kind, reason -> {:error, {:adapter_crashed, {kind, reason}}}
  end

  defp project(observations, fields) when is_list(observations),
    do: Enum.map(observations, &project(&1, fields))

  defp project(observation, fields) when is_map(observation) and is_list(fields) do
    normalized = portable(observation)
    Map.new(fields, fn field -> {to_string(field), Map.get(normalized, to_string(field))} end)
  end

  defp field(map, key, default), do: Map.get(map, key, Map.get(map, to_string(key), default))

  defp put_field(map, key, value) do
    if Map.has_key?(map, key),
      do: Map.put(map, key, value),
      else: Map.put(map, to_string(key), value)
  end

  defp differences(reference, production) do
    keys = (Map.keys(reference) ++ Map.keys(production)) |> Enum.uniq() |> Enum.sort()
    Enum.filter(keys, &(Map.get(reference, &1) != Map.get(production, &1)))
  end

  defp portable(%{__struct__: _} = value), do: value |> Map.from_struct() |> portable()

  defp portable(value) when is_map(value),
    do: Map.new(value, fn {key, item} -> {to_string(key), portable(item)} end)

  defp portable(value) when is_tuple(value), do: value |> Tuple.to_list() |> Enum.map(&portable/1)
  defp portable(value) when is_list(value), do: Enum.map(value, &portable/1)
  defp portable(value) when is_atom(value), do: Atom.to_string(value)

  defp portable(value)
       when is_binary(value) or is_integer(value) or is_boolean(value) or is_nil(value), do: value

  defp portable(value), do: %{"opaque_digest" => digest_term(value)}

  defp digest(value), do: value |> portable() |> CanonicalJCS.digest()

  defp digest_term(value),
    do:
      :crypto.hash(:sha256, :erlang.term_to_binary(value, [:deterministic]))
      |> Base.encode16(case: :lower)

  defp valid_digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)

  defp positive(value) when is_integer(value) and value > 0, do: {:ok, value}
  defp positive(_), do: {:error, :invalid_differential_bound}
end
