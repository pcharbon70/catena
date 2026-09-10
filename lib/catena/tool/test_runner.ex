defmodule Catena.Tool.TestRunner do
  @moduledoc """
  Bounded deterministic runner for Catena implementation evidence.

  This internal contract does not select public test syntax. Passing runs are
  finite observations and never proof claims.
  """

  alias Catena.CanonicalJCS

  @version "0.1.84"
  @kinds ~w(unit law property model concurrency specification)a
  @exhaustion ~w(semantic_fuel schedule_bound shrink_bound)a
  @default_timeout 1_000
  @default_runs 100
  @default_size 100
  @default_shrink_limit 100
  @digest ~r/^[0-9a-f]{64}$/

  def profile do
    %{
      version: @version,
      kinds: @kinds,
      explicit_seed: true,
      typed_generators: true,
      invariant_preserving_shrinking: true,
      runner_owned_cleanup: true,
      exhaustion_kinds: @exhaustion,
      host_timeout_is_divergence: false,
      passing_tests_are_proof: false,
      public_test_syntax: false
    }
  end

  def define(id, subject_digest, cases, options \\ []) do
    with :ok <- valid_id(id),
         :ok <- valid_digest(subject_digest),
         :ok <- nonempty_cases(cases),
         {:ok, seed} <- required_seed(options),
         {:ok, timeout} <- positive_bound(Keyword.get(options, :host_timeout, @default_timeout)),
         :ok <- unique_case_ids(cases),
         :ok <- validate_cases(cases) do
      portable = %{
        "format" => "catena-test-suite",
        "version" => 1,
        "contract" => @version,
        "id" => id,
        "subject_digest" => subject_digest,
        "seed" => seed,
        "host_timeout" => timeout,
        "cases" => Enum.map(cases, &case_projection/1)
      }

      {:ok,
       portable
       |> Map.put("plan_digest", CanonicalJCS.digest(portable))
       |> Map.put(:callbacks, cases)}
    end
  rescue
    _ -> {:error, :invalid_test_suite}
  end

  def run(%{"subject_digest" => subject} = suite, expected_subject) do
    with :ok <- valid_digest(expected_subject),
         :ok <- same_subject(subject, expected_subject),
         :ok <- verify_suite(suite) do
      results =
        suite.callbacks
        |> Enum.with_index()
        |> Enum.map(fn {test_case, index} -> run_case(test_case, suite, index) end)

      report = %{
        "format" => "catena-test-report",
        "version" => 1,
        "contract" => @version,
        "suite_id" => suite["id"],
        "plan_digest" => suite["plan_digest"],
        "subject_digest" => subject,
        "seed" => suite["seed"],
        "scope" => "finite-observation-not-proof",
        "status" => aggregate_status(results),
        "results" => results
      }

      {:ok, Map.put(report, "report_digest", CanonicalJCS.digest(report))}
    end
  rescue
    _ -> {:error, :invalid_test_suite}
  end

  def run(_, _), do: {:error, :invalid_test_suite}

  defp run_case(test_case, suite, index) do
    {:ok, supervisor} = Task.Supervisor.start_link()
    {:ok, effects} = Agent.start_link(fn -> [] end)
    seed = derive_seed(suite["seed"], test_case.id, index)

    context = %{
      seed: seed,
      subject_digest: suite["subject_digest"],
      effect: fn effect -> Agent.update(effects, &[effect | &1]) end,
      spawn: fn function when is_function(function, 0) ->
        Task.Supervisor.start_child(supervisor, function)
      end
    }

    task =
      Task.Supervisor.async_nolink(supervisor, fn ->
        if test_case.kind == :property,
          do: run_property(test_case, context),
          else: invoke(test_case.execute, context)
      end)

    timeout = Map.get(test_case, :host_timeout, suite["host_timeout"])

    outcome =
      case Task.yield(task, timeout) do
        {:ok, value} ->
          value

        {:exit, reason} ->
          {:crashed, safe_reason(reason)}

        nil ->
          Task.shutdown(task, :brutal_kill)
          {:host_timeout, %{"milliseconds" => timeout}}
      end

    observed_effects = Agent.get(effects, &Enum.reverse/1)
    owned_children = live_children(supervisor, task.pid)
    Supervisor.stop(supervisor, :normal, timeout)
    Agent.stop(effects)

    test_case
    |> base_result(seed, observed_effects, length(owned_children))
    |> finish_result(outcome, test_case, observed_effects, suite["subject_digest"])
  end

  defp run_property(test_case, context) do
    runs = Map.get(test_case, :runs, @default_runs)
    size = Map.get(test_case, :size, @default_size)

    Enum.reduce_while(0..(runs - 1), {:pass, %{"observations" => runs}}, fn index, _ ->
      generated = test_case.generate.(derive_seed(context.seed, test_case.id, index), size)

      if test_case.valid.(generated) do
        case invoke(fn -> test_case.check.(generated, context) end) do
          {:pass, _} -> {:cont, {:pass, %{"observations" => runs}}}
          {:fail, evidence} -> {:halt, shrink(test_case, generated, evidence, context)}
          other -> {:halt, other}
        end
      else
        {:halt, {:invalid_generator, %{"run" => index}}}
      end
    end)
  end

  defp shrink(test_case, initial, evidence, context) do
    limit = Map.get(test_case, :shrink_limit, @default_shrink_limit)
    do_shrink(test_case, initial, evidence, context, 0, limit)
  end

  defp do_shrink(test_case, value, evidence, context, steps, limit) when steps >= limit do
    case failing_candidate(test_case, valid_candidates(test_case, value), context) do
      nil ->
        property_failure(value, evidence, steps)

      _ ->
        {:exhausted, :shrink_bound,
         %{
           "counterexample" => portable_counterexample(value),
           "failure" => evidence,
           "shrink_steps" => steps,
           "minimal" => false
         }}
    end
  end

  defp do_shrink(test_case, value, evidence, context, steps, limit) do
    case failing_candidate(test_case, valid_candidates(test_case, value), context) do
      nil ->
        property_failure(value, evidence, steps)

      {candidate, next_evidence} ->
        do_shrink(test_case, candidate, next_evidence, context, steps + 1, limit)
    end
  end

  defp valid_candidates(test_case, value) do
    case test_case.shrink.(value) do
      candidates when is_list(candidates) -> Enum.filter(candidates, test_case.valid)
      _ -> []
    end
  rescue
    _ -> []
  end

  defp failing_candidate(test_case, candidates, context) do
    Enum.find_value(candidates, fn candidate ->
      case invoke(fn -> test_case.check.(candidate, context) end) do
        {:fail, evidence} -> {candidate, evidence}
        _ -> nil
      end
    end)
  end

  defp property_failure(value, evidence, steps) do
    {:fail,
     %{
       "counterexample" => portable_counterexample(value),
       "failure" => evidence,
       "shrink_steps" => steps,
       "minimal" => true
     }}
  end

  defp finish_result(result, outcome, test_case, observed, subject_digest) do
    undeclared = observed -- Map.get(test_case, :effects, [])

    if undeclared != [] do
      result
      |> Map.put("status", "fail")
      |> Map.put("reason", "undeclared-effect")
      |> Map.put("evidence", %{"effects" => Enum.map(undeclared, &to_string/1)})
    else
      apply_outcome(result, outcome, test_case.kind, subject_digest)
    end
  end

  defp apply_outcome(result, outcome, kind, subject_digest) do
    case normalize(outcome) do
      {:ok, status, reason, evidence} ->
        cond do
          not portable?(evidence) ->
            result
            |> Map.put("status", "fail")
            |> Map.put("reason", "invalid-evidence")
            |> Map.put("evidence", %{})

          kind == :specification and evidence_subject(evidence) != subject_digest ->
            result
            |> Map.put("status", "fail")
            |> Map.put("reason", "stale-evidence-subject")
            |> Map.put("evidence", evidence)

          true ->
            result
            |> Map.put("status", status)
            |> Map.put("reason", reason)
            |> Map.put("evidence", evidence)
        end

      :error ->
        result
        |> Map.put("status", "fail")
        |> Map.put("reason", "invalid-result")
        |> Map.put("evidence", %{})
    end
  end

  defp normalize(:pass), do: {:ok, "pass", nil, %{}}
  defp normalize({:pass, evidence}), do: {:ok, "pass", nil, evidence}
  defp normalize({:fail, evidence}), do: {:ok, "fail", "assertion", evidence}

  defp normalize({:exhausted, kind, evidence}) when kind in @exhaustion,
    do: {:ok, "exhausted", Atom.to_string(kind), evidence}

  defp normalize({:host_timeout, evidence}),
    do: {:ok, "host-timeout", "host-timeout", evidence}

  defp normalize({:crashed, reason}), do: {:ok, "fail", "crashed", %{"detail" => reason}}
  defp normalize({:invalid_generator, evidence}), do: {:ok, "fail", "invalid-generator", evidence}
  defp normalize(_), do: :error

  defp invoke(function, argument) when is_function(function, 1) do
    function.(argument)
  rescue
    error -> {:crashed, Exception.message(error)}
  catch
    kind, reason -> {:crashed, safe_reason({kind, reason})}
  end

  defp invoke(function) when is_function(function, 0) do
    function.()
  rescue
    error -> {:crashed, Exception.message(error)}
  catch
    kind, reason -> {:crashed, safe_reason({kind, reason})}
  end

  defp base_result(test_case, seed, observed_effects, cleaned) do
    %{
      "id" => test_case.id,
      "kind" => Atom.to_string(test_case.kind),
      "seed" => seed,
      "declared_effects" => Enum.map(Map.get(test_case, :effects, []), &to_string/1),
      "observed_effects" => Enum.map(observed_effects, &to_string/1),
      "runner_owned_processes_cleaned" => cleaned
    }
  end

  defp aggregate_status(results) do
    statuses = Enum.map(results, & &1["status"])

    cond do
      "fail" in statuses -> "fail"
      "host-timeout" in statuses -> "host-timeout"
      "exhausted" in statuses -> "exhausted"
      true -> "pass"
    end
  end

  defp verify_suite(%{callbacks: callbacks} = suite) do
    portable = Map.drop(suite, [:callbacks, "plan_digest"])

    with :ok <- nonempty_cases(callbacks),
         :ok <- validate_cases(callbacks),
         true <- CanonicalJCS.digest(portable) == suite["plan_digest"] do
      :ok
    else
      _ -> {:error, :invalid_test_suite}
    end
  end

  defp verify_suite(_), do: {:error, :invalid_test_suite}

  defp validate_cases(cases) do
    if Enum.all?(cases, &valid_case?/1), do: :ok, else: {:error, :invalid_test_case}
  end

  defp valid_case?(%{id: id, kind: :property} = test_case) do
    valid_id(id) == :ok and is_function(test_case[:generate], 2) and
      is_function(test_case[:valid], 1) and is_function(test_case[:check], 2) and
      is_function(test_case[:shrink], 1) and valid_effects?(test_case) and
      valid_optional_bounds?(test_case, [:runs, :size, :shrink_limit, :host_timeout])
  end

  defp valid_case?(%{id: id, kind: kind, execute: execute} = test_case)
       when kind in @kinds do
    valid_id(id) == :ok and is_function(execute, 1) and
      valid_effects?(test_case) and valid_optional_bounds?(test_case, [:host_timeout])
  end

  defp valid_case?(_), do: false

  defp valid_effects?(test_case) do
    effects = Map.get(test_case, :effects, [])

    is_list(effects) and Enum.all?(effects, &(is_atom(&1) or is_binary(&1))) and
      length(effects) == length(Enum.uniq(effects))
  end

  defp valid_optional_bounds?(test_case, keys) do
    Enum.all?(keys, fn key ->
      not Map.has_key?(test_case, key) or match?({:ok, _}, positive_bound(test_case[key]))
    end)
  end

  defp case_projection(test_case) do
    test_case
    |> Map.take([:id, :kind, :effects, :runs, :size, :shrink_limit, :host_timeout])
    |> Map.put_new(:effects, [])
    |> stringify()
  end

  defp stringify(value) when is_map(value),
    do: Map.new(value, fn {key, item} -> {to_string(key), stringify(item)} end)

  defp stringify(value) when is_list(value), do: Enum.map(value, &stringify/1)
  defp stringify(value) when is_atom(value), do: Atom.to_string(value)
  defp stringify(value), do: value

  defp portable?(value) when is_binary(value) or is_boolean(value) or is_nil(value), do: true
  defp portable?(value) when is_integer(value) and abs(value) <= 9_007_199_254_740_991, do: true
  defp portable?(value) when is_list(value), do: Enum.all?(value, &portable?/1)

  defp portable?(value) when is_map(value) do
    Enum.all?(value, fn {key, item} ->
      (is_binary(key) or is_atom(key)) and portable?(item)
    end)
  end

  defp portable?(_), do: false

  defp portable_counterexample(value) do
    if portable?(value), do: value, else: %{"digest" => digest_term(value), "redacted" => true}
  end

  defp evidence_subject(evidence) when is_map(evidence),
    do: Map.get(evidence, "subject_digest") || Map.get(evidence, :subject_digest)

  defp evidence_subject(_), do: nil

  defp unique_case_ids(cases) do
    ids = Enum.map(cases, &Map.get(&1, :id))
    if length(ids) == length(Enum.uniq(ids)), do: :ok, else: {:error, :duplicate_test_id}
  end

  defp nonempty_cases(cases) when is_list(cases) and cases != [], do: :ok
  defp nonempty_cases(_), do: {:error, :zero_test_run}

  defp required_seed(options) do
    case Keyword.fetch(options, :seed) do
      {:ok, seed} when is_integer(seed) and seed >= 0 and seed <= 9_007_199_254_740_991 ->
        {:ok, seed}

      :error ->
        {:error, :seed_required}

      _ ->
        {:error, :invalid_seed}
    end
  end

  defp positive_bound(value) when is_integer(value) and value > 0, do: {:ok, value}
  defp positive_bound(_), do: {:error, :invalid_bound}

  defp valid_id(id) when is_binary(id) and byte_size(id) in 1..128, do: :ok
  defp valid_id(_), do: {:error, :invalid_id}

  defp valid_digest(digest) when is_binary(digest) do
    if Regex.match?(@digest, digest), do: :ok, else: {:error, :invalid_subject_digest}
  end

  defp valid_digest(_), do: {:error, :invalid_subject_digest}

  defp same_subject(subject, subject), do: :ok
  defp same_subject(_, _), do: {:error, :stale_subject}

  defp derive_seed(seed, id, index) do
    <<value::unsigned-big-integer-size(64), _::binary>> =
      :crypto.hash(:sha256, :erlang.term_to_binary({seed, id, index}, [:deterministic]))

    rem(value, 9_007_199_254_740_992)
  end

  defp digest_term(value),
    do:
      :crypto.hash(:sha256, :erlang.term_to_binary(value, [:deterministic]))
      |> Base.encode16(case: :lower)

  defp live_children(supervisor, task_pid) do
    supervisor
    |> Task.Supervisor.children()
    |> Enum.reject(&(&1 == task_pid))
    |> Enum.filter(&Process.alive?/1)
  end

  defp safe_reason(reason) do
    reason
    |> inspect(limit: 20, printable_limit: 200)
    |> String.slice(0, 512)
  end
end
