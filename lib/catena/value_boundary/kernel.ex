defmodule Catena.ValueBoundary.Kernel do
  @moduledoc "Exact 0.1.58 pure value-tree integration with retained kernel typing and execution."
  alias Catena.Kernel.{Checker, Verifier, Backend}
  alias Catena.{Diagnostic, ImplementationLimits}
  @profile "0.1.58"
  @new [:float, :text, :character, :bytes]

  def check(parsed, options \\ []) do
    with {:ok, _} <- selection(options),
         %{format: :kernel, version: "0.1.8"} <- parsed,
         :ok <- ImplementationLimits.validate_integer_magnitudes(parsed),
         :ok <- literal_limits(parsed) do
      parsed =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          parsed,
          &Map.put(&2, &1, @profile)
        )

      Checker.check(parsed)
    else
      {:error, _} = error ->
        error

      _ ->
        {:error,
         Diagnostic.new("I001", "value-tree input requires a retained parsed kernel shape")}
    end
  rescue
    _ -> {:error, Diagnostic.new("I001", "malformed value-tree input")}
  end

  defp selection(options) do
    requested =
      Keyword.get(options, :language_selection, Catena.LanguageVersion.legacy_selection(@profile))

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{language_revision: @profile, previews: []} = selected} -> {:ok, selected}
      _ -> {:error, Diagnostic.new("EDN001", "value-tree input requires exact 0.1.58")}
    end
  end

  def boundary(%{version: @profile} = core) do
    if core.language_revision == @profile and core.frontend_version == @profile and
         core.frontend_format == @profile and core.edition == "0.1" and core.previews == [] and
         core.processes == [] and core.imports == [] and map_size(core.effects) == 0 and
         map_size(core.handlers) == 0 and Enum.all?(core.definitions, &(&1.uses == [])) and
         pure?(core) do
      :ok
    else
      {:error, "value-boundary profile requires a closed pure module"}
    end
  end

  def boundary(core) do
    if contains_new?(core),
      do: {:error, "new scalar carriers are excluded from retained kernel revisions"},
      else: :ok
  end

  def compile(core) do
    with true <- core.version == @profile,
         :ok <- Verifier.verify(core),
         :ok <- ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- literal_limits(core),
         {:ok, selected} <- selection([]),
         forms <- Backend.lower(core),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, module, binary, warnings} <-
           Catena.OTP.Compiler.compile(forms,
             source: core.origin,
             specification: @profile,
             frontend: "value-tree-0.1.58",
             artifact_version: @profile,
             frontend_version: @profile,
             language_selection: selected
           ) do
      {:ok, module, binary,
       %{
         core: core,
         forms: forms,
         warnings: warnings,
         interface: nil,
         interface_binary: nil,
         profile: :value_boundaries,
         selection: selected,
         artifact_version: @profile
       }}
    else
      {:error, %Diagnostic{}} = error -> error
      _ -> {:error, Diagnostic.new("I001", "invalid checked value boundary")}
    end
  end

  defp literal_limits(%{tag: tag, value: value} = node)
       when tag in [:text, :bytes] and is_binary(value),
       do: ImplementationLimits.validate_decoded_literal_bytes(value, Map.get(node, :span))

  defp literal_limits(value) when is_map(value), do: value |> Map.values() |> literal_limits()

  defp literal_limits(value) when is_list(value) do
    Enum.reduce_while(value, :ok, fn item, :ok ->
      case literal_limits(item) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp literal_limits(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> literal_limits()

  defp literal_limits(_), do: :ok

  defp pure?({:function, parameter, effects, result}),
    do: effects == [] and pure?(parameter) and pure?(result)

  defp pure?({tag, _}) when tag in [:process, :managed_process], do: false
  defp pure?(%{effects: effects}) when is_list(effects) and effects != [], do: false
  defp pure?(value) when is_map(value), do: value |> Map.values() |> Enum.all?(&pure?/1)
  defp pure?(value) when is_list(value), do: Enum.all?(value, &pure?/1)
  defp pure?(value) when is_tuple(value), do: value |> Tuple.to_list() |> Enum.all?(&pure?/1)
  defp pure?(_), do: true

  defp contains_new?(value) when value in @new, do: true

  defp contains_new?(value) when is_map(value),
    do: value |> Map.values() |> Enum.any?(&contains_new?/1)

  defp contains_new?(value) when is_list(value), do: Enum.any?(value, &contains_new?/1)

  defp contains_new?(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.any?(&contains_new?/1)

  defp contains_new?(_), do: false
end
