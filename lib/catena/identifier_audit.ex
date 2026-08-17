defmodule Catena.IdentifierAudit do
  @moduledoc false

  alias Catena.{Diagnostic, LanguageLifecycle, QualifiedName}

  @confusable_warning "IDN007"

  @spec audit([binary()], keyword()) ::
          {:ok, [QualifiedName.t()], [Diagnostic.t()]} | {:error, Diagnostic.t()}
  def audit(names, options \\ [])

  def audit(names, options) when is_list(names) and is_list(options) do
    with true <- names != [] and Enum.all?(names, &is_binary/1),
         {:ok, parsed} <- parse_names(names, options),
         diagnostics = confusable_diagnostics(parsed),
         :ok <- enforce_diagnostics(diagnostics, options) do
      {:ok, parsed, diagnostics}
    else
      false ->
        {:error,
         Diagnostic.new("IDN001", "identifier auditing requires one or more name strings",
           path: "$"
         )}

      {:error, _diagnostic} = error ->
        error
    end
  end

  def audit(_names, _options),
    do: {:error, Diagnostic.new("IDN001", "identifier auditing requires a list of names")}

  defp parse_names(names, options) do
    names
    |> Enum.reduce_while({:ok, []}, fn name, {:ok, parsed} ->
      case QualifiedName.parse(name, options) do
        {:ok, qualified} -> {:cont, {:ok, [qualified | parsed]}}
        {:error, _diagnostic} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, parsed} -> {:ok, Enum.reverse(parsed)}
      error -> error
    end
  end

  defp confusable_diagnostics(names) do
    {_seen, diagnostics} =
      Enum.reduce(names, {%{}, []}, fn name, {seen, diagnostics} ->
        prior =
          seen
          |> Map.get(name.skeleton, [])
          |> Enum.find(&(&1.canonical != name.canonical))

        next_seen = Map.update(seen, name.skeleton, [name], &(&1 ++ [name]))

        if prior do
          diagnostic =
            Diagnostic.new(
              @confusable_warning,
              "#{inspect(name.canonical)} is visually confusable with #{inspect(prior.canonical)}",
              span: name.span,
              severity: :warning,
              details: %{
                name: name.canonical,
                confusable_with: prior.canonical,
                skeleton: name.skeleton,
                unicode_version: Catena.UnicodeData.version()
              }
            )

          {next_seen, diagnostics ++ [diagnostic]}
        else
          {next_seen, diagnostics}
        end
      end)

    diagnostics
  end

  defp enforce_diagnostics(diagnostics, options) do
    with {:ok, denied_ids} <-
           LanguageLifecycle.validate_denied_diagnostics(
             Keyword.get(options, :denied_diagnostics, [])
           ) do
      denied = MapSet.new(denied_ids)

      case Enum.find(diagnostics, &MapSet.member?(denied, &1.id)) do
        nil ->
          :ok

        diagnostic ->
          {:error,
           %{
             diagnostic
             | severity: :error,
               details: Map.put(diagnostic.details, :promoted_from_warning, true)
           }}
      end
    end
  end
end
