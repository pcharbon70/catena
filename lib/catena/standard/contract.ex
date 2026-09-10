defmodule Catena.Standard.Contract do
  @moduledoc """
  Machine-readable stability and performance contracts for the 0.1 standard roles.

  Role identifiers are internal catalog keys. They neither introduce source
  vocabulary nor expose host representation or timing as portable behavior.
  """

  alias Catena.CanonicalJCS

  @version "0.1.87"
  @orders ~w(none left-to-right key-order request-order)
  @callback_counts ~w(none zero-or-one once-per-input once-per-collision until-stop)
  @complexities ~w(constant logarithmic linear n-log-n quadratic implementation-bounded)
  @stacks ~w(constant bounded-by-input implementation-bounded)
  @stabilities ~w(language interface package empirical)

  @operations [
    {"foundation.identity", "0.1.69", "constant", "constant"},
    {
      "foundation.compose",
      "0.1.69",
      "constant",
      "constant",
      "left-to-right",
      "once-per-input"
    },
    {
      "outcomes.optional-map",
      "0.1.54",
      "constant",
      "constant",
      "left-to-right",
      "zero-or-one"
    },
    {
      "outcomes.dependent-chain",
      "0.1.54",
      "constant",
      "constant",
      "left-to-right",
      "zero-or-one"
    },
    {
      "outcomes.independent-map2",
      "0.1.54",
      "linear",
      "linear",
      "left-to-right",
      "zero-or-one"
    },
    {"collections.construct-list", "0.1.65", "linear", "linear"},
    {"collections.construct-ordered", "0.1.65", "n-log-n", "linear", "key-order"},
    {"collections.entries", "0.1.65", "linear", "linear", "key-order"},
    {"collections.lookup", "0.1.65", "linear", "linear", "key-order"},
    {"collections.replace", "0.1.65", "n-log-n", "linear", "key-order"},
    {
      "collections.map-values",
      "0.1.65",
      "linear",
      "linear",
      "key-order",
      "once-per-input"
    },
    {"collections.fold", "0.1.65", "linear", "linear", "key-order", "once-per-input"},
    {
      "collections.combine",
      "0.1.65",
      "n-log-n",
      "linear",
      "left-to-right",
      "once-per-collision"
    },
    {"collections.fold-while", "0.1.65", "linear", "linear", "key-order", "until-stop"},
    {"text.measure", "0.1.66", "linear", "linear"},
    {"text.slice", "0.1.66", "linear", "linear"},
    {"text.normalize", "0.1.66", "linear", "linear"},
    {"text.transcode", "0.1.66", "linear", "linear"},
    {"numeric.checked", "0.1.67", "implementation-bounded", "implementation-bounded"},
    {
      "environment.request",
      "0.1.68",
      "implementation-bounded",
      "implementation-bounded",
      "request-order"
    }
  ]

  def profile do
    %{
      version: @version,
      public_source: :held_for_p109,
      stable_beam_abi: false,
      portable_wall_clock_bounds: false,
      benchmark_results_are_normative: false,
      operation_count: length(@operations)
    }
  end

  def inventory do
    payload = %{
      "format" => "catena-standard-contracts",
      "version" => @version,
      "operations" => operations()
    }

    Map.put(payload, "digest", CanonicalJCS.digest(payload))
  end

  def verify_inventory(%{"digest" => digest} = inventory) when is_binary(digest) do
    unsigned = Map.delete(inventory, "digest")

    with %{
           "format" => "catena-standard-contracts",
           "version" => @version,
           "operations" => operations
         } <- unsigned,
         true <- is_list(operations) and operations != [],
         true <- Enum.all?(operations, &operation?/1),
         ids <- Enum.map(operations, & &1["id"]),
         true <- ids == Enum.sort(ids) and ids == Enum.uniq(ids),
         true <- digest == CanonicalJCS.digest(unsigned),
         do: :ok,
         else: (_ -> {:error, :invalid_standard_contract_inventory})
  rescue
    _ -> {:error, :invalid_standard_contract_inventory}
  end

  def verify_inventory(_), do: {:error, :invalid_standard_contract_inventory}

  def compare(old, new) when is_map(old) and is_map(new) do
    with true <- operation?(old) and operation?(new) and old["id"] == new["id"] do
      changes =
        []
        |> changed(old, new, "laws", :law_contract_changed)
        |> changed(old, new, "order", :order_changed)
        |> changed(old, new, "callbacks", :callback_multiplicity_changed)
        |> changed(old, new, "failures", :failure_contract_changed)
        |> changed(old, new, "stack", :stack_contract_changed)
        |> changed(old, new, "representation", :representation_promise_changed)
        |> strengthened_requirements(old, new)
        |> regressed(old, new, "time", @complexities, :time_complexity_regressed)
        |> regressed(old, new, "space", @complexities, :space_complexity_regressed)

      if changes == [], do: {:compatible, []}, else: {:breaking, Enum.reverse(changes)}
    else
      _ -> {:error, :incomparable_standard_contract}
    end
  end

  def compare(_, _), do: {:error, :incomparable_standard_contract}

  def observation(operation_id, toolchain, samples)
      when is_binary(operation_id) and is_map(toolchain) and is_list(samples) do
    with true <- Enum.any?(operations(), &(&1["id"] == operation_id)),
         true <- complete_toolchain?(toolchain),
         true <- samples != [] and length(samples) <= 128,
         true <- Enum.all?(samples, &sample?/1),
         sizes <- Enum.map(samples, & &1["size"]),
         true <- sizes == Enum.sort(sizes) and sizes == Enum.uniq(sizes) do
      payload = %{
        "format" => "catena-performance-observation",
        "version" => @version,
        "contract" => inventory()["digest"],
        "operation" => operation_id,
        "toolchain" => toolchain,
        "samples" => samples,
        "portable_claim" => false
      }

      {:ok, Map.put(payload, "digest", CanonicalJCS.digest(payload))}
    else
      _ -> {:error, :invalid_performance_observation}
    end
  rescue
    _ -> {:error, :invalid_performance_observation}
  end

  def observation(_, _, _), do: {:error, :invalid_performance_observation}

  def verify_observation(
        %{
          "digest" => digest,
          "operation" => operation,
          "toolchain" => toolchain,
          "samples" => samples
        } = value
      ) do
    with {:ok, rebuilt} <- observation(operation, toolchain, samples),
         true <- value == rebuilt and digest == rebuilt["digest"],
         do: :ok,
         else: (_ -> {:error, :invalid_performance_observation})
  end

  def verify_observation(_), do: {:error, :invalid_performance_observation}

  defp operations do
    @operations
    |> Enum.map(fn
      {id, package, time, space} ->
        operation(id, package, time, space)

      {id, package, time, space, order} ->
        operation(id, package, time, space, order)

      {id, package, time, space, order, callbacks} ->
        operation(id, package, time, space, order, callbacks)
    end)
    |> Enum.sort_by(& &1["id"])
  end

  defp operation(id, package, time, space, order \\ "none", callbacks \\ "none") do
    %{
      "id" => id,
      "package" => package,
      "stability" => "package",
      "laws" => "normative-standard-role",
      "requirements" => [],
      "order" => order,
      "callbacks" => callbacks,
      "failures" => "typed-or-declared-trap",
      "time" => time,
      "space" => space,
      "stack" => "constant",
      "representation" => "none"
    }
  end

  defp operation?(value) do
    is_map(value) and
      Enum.sort(Map.keys(value)) ==
        ~w(callbacks failures id laws order package representation requirements space stability stack time) and
      id?(value["id"]) and version?(value["package"]) and value["stability"] in @stabilities and
      is_binary(value["laws"]) and value["laws"] != "" and is_list(value["requirements"]) and
      Enum.all?(value["requirements"], &is_binary/1) and
      value["requirements"] == Enum.uniq(value["requirements"]) and
      value["order"] in @orders and value["callbacks"] in @callback_counts and
      is_binary(value["failures"]) and value["failures"] != "" and
      value["time"] in @complexities and value["space"] in @complexities and
      value["stack"] in @stacks and is_binary(value["representation"])
  end

  defp id?(id), do: is_binary(id) and Regex.match?(~r/^[a-z][a-z0-9.-]*$/, id)
  defp version?(version), do: is_binary(version) and Regex.match?(~r/^0\.1\.[0-9]+$/, version)

  defp changed(changes, old, new, key, reason),
    do: if(old[key] == new[key], do: changes, else: [reason | changes])

  defp strengthened_requirements(changes, old, new) do
    if MapSet.subset?(MapSet.new(new["requirements"]), MapSet.new(old["requirements"])),
      do: changes,
      else: [:input_requirements_strengthened | changes]
  end

  defp regressed(changes, old, new, key, order, reason) do
    if Enum.find_index(order, &(&1 == new[key])) <= Enum.find_index(order, &(&1 == old[key])),
      do: changes,
      else: [reason | changes]
  end

  defp complete_toolchain?(toolchain),
    do:
      Enum.sort(Map.keys(toolchain)) == ~w(architecture compiler os otp) and
        Enum.all?(toolchain, fn {_, value} -> is_binary(value) and value != "" end)

  defp sample?(sample),
    do:
      is_map(sample) and Enum.sort(Map.keys(sample)) == ~w(elapsed_ns size work_units) and
        is_integer(sample["size"]) and sample["size"] >= 0 and
        is_integer(sample["work_units"]) and sample["work_units"] >= 0 and
        is_integer(sample["elapsed_ns"]) and sample["elapsed_ns"] >= 0
end
