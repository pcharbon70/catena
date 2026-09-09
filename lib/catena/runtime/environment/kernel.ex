defmodule Catena.Runtime.Environment.Kernel do
  @moduledoc "Exact 0.1.68 capability/value-tree input; retained source grammar is only a quoted decoder."
  alias Catena.Kernel.{CapabilityKernel, Checker, Verifier}
  alias Catena.ImplementationLimits
  @version "0.1.68"
  def check(parsed, families) do
    with %{format: :kernel, version: "0.1.8"} <- parsed,
         {:ok, prepared} <- CapabilityKernel.prepare(parsed, families),
         :ok <- ImplementationLimits.validate_integer_magnitudes(prepared),
         :ok <- literal_limits(prepared),
         module <-
           Enum.reduce(
             [:version, :frontend_format, :frontend_version, :language_revision],
             prepared,
             &Map.put(&2, &1, @version)
           ),
         {:ok, core} <- Checker.check(module),
         :ok <- boundary(core) do
      {:ok, core}
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_environment_tree}
    end
  rescue
    _ -> {:error, :invalid_environment_tree}
  end

  def verify(core), do: Verifier.verify(core)

  def boundary(%{version: @version} = core) do
    if core.frontend_format == @version and core.frontend_version == @version and
         core.language_revision == @version and core.edition == "0.1" and core.previews == [] and
         core.imports == [] and core.processes == [] and core.profile == :environmental_effects do
      literal_limits(core)
    else
      {:error, "environment core requires the exact closed local value/capability profile"}
    end
  end

  def boundary(_), do: {:error, "invalid environment profile"}

  def literal_limits(%{tag: tag, value: value} = node)
      when tag in [:text, :bytes] and is_binary(value),
      do: ImplementationLimits.validate_decoded_literal_bytes(value, Map.get(node, :span))

  def literal_limits(%_{}), do: :ok
  def literal_limits(map) when is_map(map), do: map |> Map.values() |> literal_limits()
  def literal_limits(tuple) when is_tuple(tuple), do: tuple |> Tuple.to_list() |> literal_limits()

  def literal_limits(list) when is_list(list) do
    Enum.reduce_while(list, :ok, fn value, :ok ->
      case literal_limits(value) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  def literal_limits(_), do: :ok
end
