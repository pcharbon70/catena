defmodule Catena.Performance.Envelope do
  @moduledoc "Supported-host performance envelopes with semantic gates and reproducible identities."

  alias Catena.{CanonicalJCS, OTP.Profile}

  @version "0.1.88"
  @families ~w(direct-call curried-call trait-operation adt pattern guard comprehension handler process-message resource foreign erasure code-size compile-time diagnostic)
  @max_workloads 64
  @max_sizes 16
  @max_repetitions 100
  @default_timeout 30_000

  def profile do
    %{
      version: @version,
      families: @families,
      maximum_workloads: @max_workloads,
      maximum_sizes: @max_sizes,
      maximum_repetitions: @max_repetitions,
      comparison: :median_relative_to_semantic_baseline,
      absolute_timing_portable: false,
      semantic_gate_required: true,
      negative_results_retained: true,
      public_source: :held_for_p109
    }
  end

  def run(workloads, options \\ [])

  def run(workloads, options) when is_list(workloads) do
    repetitions = Keyword.get(options, :repetitions, 7)
    warmup = Keyword.get(options, :warmup, 2)
    timeout = Keyword.get(options, :timeout_ms, @default_timeout)
    seed = Keyword.get(options, :seed, 13_138)

    with {:ok, host} <- Profile.require_supported(),
         :ok <- validate_options(repetitions, warmup, timeout, seed),
         :ok <- validate_workloads(workloads) do
      :rand.seed(:exsss, {seed, seed + 1, seed + 2})
      results = Enum.flat_map(workloads, &measure(&1, repetitions, warmup, timeout))

      payload = %{
        "format" => "catena-performance-envelope",
        "version" => @version,
        "seed" => seed,
        "host" => host,
        "host_digest" => Profile.digest(host),
        "contract_digest" => Catena.Standard.Contract.inventory()["digest"],
        "repetitions" => repetitions,
        "warmup" => warmup,
        "timeout_ms" => timeout,
        "workloads" => results,
        "all_semantic_gates_pass" => Enum.all?(results, &(&1["outcome"] == "measured")),
        "portable_claim" => false
      }

      {:ok, Map.put(payload, "digest", CanonicalJCS.digest(payload))}
    end
  rescue
    _ -> {:error, :performance_envelope_failed}
  end

  def run(_, _), do: {:error, :performance_envelope_failed}

  def verify(%{"digest" => digest, "workloads" => workloads} = report)
      when is_binary(digest) and is_list(workloads) do
    unsigned = Map.delete(report, "digest")

    valid =
      report["format"] == "catena-performance-envelope" and report["version"] == @version and
        report["portable_claim"] == false and
        report["host_digest"] == Profile.digest(report["host"]) and
        report["contract_digest"] == Catena.Standard.Contract.inventory()["digest"] and
        is_integer(report["seed"]) and is_integer(report["repetitions"]) and
        is_integer(report["warmup"]) and is_integer(report["timeout_ms"]) and
        Enum.all?(workloads, &result?/1) and
        report["all_semantic_gates_pass"] == Enum.all?(workloads, &(&1["outcome"] == "measured")) and
        digest == CanonicalJCS.digest(unsigned)

    if valid, do: :ok, else: {:error, :invalid_performance_envelope}
  rescue
    _ -> {:error, :invalid_performance_envelope}
  end

  def verify(_), do: {:error, :invalid_performance_envelope}

  def compare(old, new, options \\ []) do
    threshold = Keyword.get(options, :regression_ratio, 1.25)

    with :ok <- verify(old),
         :ok <- verify(new),
         true <- old["host_digest"] == new["host_digest"],
         true <- old["contract_digest"] == new["contract_digest"],
         true <- is_number(threshold) and threshold >= 1.0 do
      previous = Map.new(old["workloads"], &{{&1["id"], &1["size"]}, &1})

      changes =
        Enum.map(new["workloads"], fn result ->
          prior = previous[{result["id"], result["size"]}]
          classify(prior, result, threshold)
        end)

      {:ok,
       %{
         regressions: Enum.filter(changes, &(&1.class == :regression)),
         exclusions: Enum.filter(changes, &(&1.class == :excluded)),
         comparisons: changes
       }}
    else
      _ -> {:error, :incomparable_performance_envelopes}
    end
  end

  def workload(id, family, sizes, run, baseline)
      when is_binary(id) and family in @families and is_list(sizes) and
             is_function(run, 1) and is_function(baseline, 1),
      do: %{id: id, family: family, sizes: sizes, run: run, baseline: baseline}

  defp validate_options(repetitions, warmup, timeout, seed) do
    if repetitions in 1..@max_repetitions and warmup in 0..@max_repetitions and
         is_integer(timeout) and timeout in 1..300_000 and is_integer(seed),
       do: :ok,
       else: {:error, :invalid_performance_options}
  end

  defp validate_workloads(workloads) do
    ids = Enum.map(workloads, &Map.get(&1, :id))
    families = MapSet.new(Enum.map(workloads, &Map.get(&1, :family)))

    if length(workloads) in length(@families)..@max_workloads and ids == Enum.uniq(ids) and
         families == MapSet.new(@families) and Enum.all?(workloads, &workload?/1),
       do: :ok,
       else: {:error, :invalid_performance_workloads}
  end

  defp workload?(w),
    do:
      is_binary(w.id) and Regex.match?(~r/^[a-z0-9][a-z0-9-]*$/, w.id) and
        w.family in @families and length(w.sizes) in 1..@max_sizes and
        w.sizes == Enum.sort(w.sizes) and w.sizes == Enum.uniq(w.sizes) and
        Enum.all?(w.sizes, &(is_integer(&1) and &1 >= 0)) and
        is_function(w.run, 1) and is_function(w.baseline, 1)

  defp measure(workload, repetitions, warmup, timeout) do
    if warmup > 0 do
      Enum.each(1..warmup, fn _ -> timed(workload.run, hd(workload.sizes), timeout) end)
    end

    Enum.map(workload.sizes, fn size ->
      baseline = timed(workload.baseline, size, timeout)
      samples = Enum.map(1..repetitions, fn _ -> timed(workload.run, size, timeout) end)
      successful = Enum.filter(samples, &match?({:ok, _, _, _}, &1))

      outcome =
        cond do
          match?({:timeout}, baseline) or Enum.any?(samples, &match?({:timeout}, &1)) ->
            "timeout"

          not match?({:ok, _, _, _}, baseline) or length(successful) != repetitions ->
            "failure"

          Enum.any?(successful, fn {:ok, digest, _, _} -> digest != elem(baseline, 1) end) ->
            "semantic-mismatch"

          true ->
            "measured"
        end

      times = for {:ok, _, ns, _} <- successful, do: ns
      memory = for {:ok, _, _, bytes} <- successful, do: bytes

      %{
        "id" => workload.id,
        "family" => workload.family,
        "size" => size,
        "workload_digest" =>
          CanonicalJCS.digest(%{"id" => workload.id, "family" => workload.family, "size" => size}),
        "outcome" => outcome,
        "median_ns" => median(times),
        "minimum_ns" => Enum.min(times, fn -> nil end),
        "maximum_ns" => Enum.max(times, fn -> nil end),
        "median_memory_bytes" => median(memory),
        "baseline_ns" =>
          case baseline do
            {:ok, _, ns, _} -> ns
            _ -> nil
          end,
        "relative_to_baseline_ppm" => ratio(median(times), baseline),
        "samples_ns" => times
      }
    end)
  end

  defp timed(function, size, timeout) do
    task =
      Task.async(fn ->
        try do
          before_memory = :erlang.memory(:total)
          started = System.monotonic_time(:nanosecond)
          result = function.(size)
          elapsed = System.monotonic_time(:nanosecond) - started
          memory = max(:erlang.memory(:total) - before_memory, 0)
          {:ok, digest(result), elapsed, memory}
        rescue
          _ -> {:failure}
        catch
          _, _ -> {:failure}
        end
      end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      _ -> {:timeout}
    end
  rescue
    _ -> {:failure}
  end

  defp digest(term),
    do:
      :crypto.hash(:sha256, :erlang.term_to_binary(term, [:deterministic]))
      |> Base.encode16(case: :lower)

  defp median([]), do: nil
  defp median(values), do: values |> Enum.sort() |> Enum.at(div(length(values), 2))
  defp ratio(_, value) when not is_tuple(value), do: nil
  defp ratio(nil, _), do: nil
  defp ratio(value, {:ok, _, baseline, _}) when baseline > 0, do: div(value * 1_000_000, baseline)
  defp ratio(_, _), do: nil

  defp classify(nil, current, _), do: %{id: current["id"], size: current["size"], class: :new}

  defp classify(previous, current, threshold) do
    class =
      cond do
        previous["outcome"] != "measured" or current["outcome"] != "measured" -> :excluded
        previous["workload_digest"] != current["workload_digest"] -> :excluded
        current["median_ns"] > previous["median_ns"] * threshold -> :regression
        true -> :within_envelope
      end

    %{id: current["id"], size: current["size"], class: class}
  end

  defp result?(r) do
    is_map(r) and r["family"] in @families and
      r["outcome"] in ~w(measured timeout failure semantic-mismatch) and
      is_binary(r["id"]) and is_integer(r["size"]) and is_binary(r["workload_digest"]) and
      is_list(r["samples_ns"])
  end
end
