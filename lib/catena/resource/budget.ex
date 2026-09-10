defmodule Catena.Resource.Budget do
  @moduledoc "Aggregate compilation and publication budgets for revision 0.1.75."

  alias Catena.{Diagnostic, ImplementationLimits}

  def profile do
    %{
      version: "0.1.75",
      source_files: ImplementationLimits.configured(:aggregate_source_files),
      source_bytes: ImplementationLimits.configured(:aggregate_source_bytes),
      syntax_nodes: ImplementationLimits.configured(:aggregate_syntax_nodes),
      output_bytes: ImplementationLimits.configured(:aggregate_output_bytes),
      transactional_outputs: true
    }
  end

  def validate_sources(sources) when is_list(sources) do
    with :ok <- limit(:aggregate_source_files, length(sources)),
         true <- Enum.all?(sources, &is_binary/1),
         do: limit(:aggregate_source_bytes, Enum.sum(Enum.map(sources, &byte_size/1))),
         else: (
           false -> {:error, Diagnostic.new("LIM006", "invalid aggregate source input")}
           {:error, _} = error -> error
         )
  end

  def validate_sources(_),
    do: {:error, Diagnostic.new("LIM006", "invalid aggregate source input")}

  def validate_file_sizes(sizes) when is_list(sizes) do
    with true <- Enum.all?(sizes, &(is_integer(&1) and &1 >= 0)),
         :ok <- limit(:aggregate_source_files, length(sizes)),
         do: limit(:aggregate_source_bytes, Enum.sum(sizes)),
         else: (
           false -> {:error, Diagnostic.new("LIM006", "invalid aggregate file sizes")}
           {:error, _} = error -> error
         )
  end

  def validate_tree(value), do: limit(:aggregate_syntax_nodes, nodes(value))

  def validate_outputs(outputs) when is_list(outputs) do
    with true <- Enum.all?(outputs, &match?(%{binary: binary} when is_binary(binary), &1)),
         do: validate_output_sizes(Enum.map(outputs, &byte_size(&1.binary))),
         else: (false -> {:error, Diagnostic.new("LIM009", "invalid aggregate output set")})
  end

  def validate_outputs(_), do: {:error, Diagnostic.new("LIM009", "invalid aggregate output set")}

  def validate_output_sizes(sizes) when is_list(sizes) do
    with true <- Enum.all?(sizes, &(is_integer(&1) and &1 >= 0)),
         do: limit(:aggregate_output_bytes, Enum.sum(sizes)),
         else: (false -> {:error, Diagnostic.new("LIM009", "invalid aggregate output sizes")})
  end

  def validate_output_sizes(_),
    do: {:error, Diagnostic.new("LIM009", "invalid aggregate output sizes")}

  defp limit(id, observed) do
    configured = ImplementationLimits.configured(id)

    if observed <= configured do
      :ok
    else
      limit = ImplementationLimits.fetch!(id)

      {:error,
       Diagnostic.new(limit.exhaustion.id, "aggregate resource budget exceeded",
         details: ImplementationLimits.details(id, observed)
       )}
    end
  end

  defp nodes(value) when is_struct(value), do: 1 + nodes(Map.from_struct(value))

  defp nodes(value) when is_map(value),
    do: 1 + Enum.sum(Enum.map(value, fn {k, v} -> nodes(k) + nodes(v) end))

  defp nodes(value) when is_list(value), do: 1 + Enum.sum(Enum.map(value, &nodes/1))

  defp nodes(value) when is_tuple(value),
    do: 1 + (value |> Tuple.to_list() |> Enum.map(&nodes/1) |> Enum.sum())

  defp nodes(_), do: 1
end
