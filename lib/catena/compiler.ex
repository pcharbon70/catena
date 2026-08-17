defmodule Catena.Compiler do
  @moduledoc "The cumulative Catena compiler pipeline and its typed-core and assurance gates."

  alias Catena.{
    Backend.ErlangAbstract,
    Diagnostic,
    ImplementationLimits,
    Interface,
    LanguageLifecycle,
    LanguageSelection,
    LanguageVersion,
    Specification,
    Type.Infer
  }

  alias Catena.OTP.Compiler, as: OTPCompiler
  alias Catena.TypedCore.Verifier

  @spec check(map(), keyword()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def check(ast, options \\ []) do
    protect(fn ->
      selection = selection(ast)

      with :ok <- ImplementationLimits.validate_source_arities(ast),
           :ok <-
             LanguageLifecycle.validate_interfaces(
               selection,
               Keyword.get(options, :interfaces, [])
             ),
           core <- Infer.module(ast, options) |> Map.put(:source, ast.source),
           specifications <- Specification.elaborate!(ast, core),
           core <- Map.put(core, :specifications, specifications),
           :ok <- enforce_diagnostics(core.diagnostics, options) do
        case Verifier.verify(core) do
          :ok ->
            {:ok, core}

          {:error, reason} ->
            {:error, Diagnostic.new("I001", "typed-core verification failed: #{reason}")}
        end
      end
    end)
  end

  @spec compile(map(), keyword()) :: {:ok, module(), binary(), map()} | {:error, Diagnostic.t()}
  def compile(ast, options \\ []) do
    protect(fn ->
      layout = Keyword.get(options, :layout, :compact)
      condition_lowering = Keyword.get(options, :condition_lowering, :auto)

      artifact_version =
        Keyword.get(
          options,
          :artifact_version,
          LanguageVersion.default_artifact_version(ast.frontend_format, ast.language_revision)
        )

      with :ok <- validate_layout(layout),
           :ok <- validate_condition_lowering(condition_lowering),
           {:ok, core} <- check(ast, options),
           forms <-
             ErlangAbstract.lower(core,
               layout: layout,
               condition_lowering: condition_lowering
             ),
           :ok <- ImplementationLimits.validate_generated_arities(forms),
           {:ok, module, binary, warnings} <-
             OTPCompiler.compile(
               forms,
               options
               |> Keyword.put_new(:source, ast.source)
               |> Keyword.put(:artifact_version, artifact_version)
               |> Keyword.put(:frontend_version, ast.frontend_format)
               |> Keyword.put(:specification, ast.language_revision)
               |> Keyword.put(:language_selection, selection(ast))
             ) do
        interface = Interface.build(core, artifact_version: artifact_version)

        {:ok, module, binary,
         %{
           core: core,
           warnings: warnings,
           diagnostics: core.diagnostics,
           selection: selection(ast),
           artifact_version: artifact_version,
           forms: forms,
           layout: layout,
           condition_lowering: condition_lowering,
           interface: interface,
           interface_binary: Interface.encode(interface)
         }}
      end
    end)
  end

  defp validate_layout(layout) when layout in [:uniform, :compact], do: :ok

  defp validate_layout(layout),
    do: {:error, Diagnostic.new("L001", "unknown ADT layout #{inspect(layout)}")}

  defp validate_condition_lowering(lowering) when lowering in [:auto, :native, :ordinary], do: :ok

  defp validate_condition_lowering(lowering),
    do: {:error, Diagnostic.new("CND001", "unknown condition lowering #{inspect(lowering)}")}

  defp selection(ast) do
    %LanguageSelection{
      edition: ast.edition,
      language_revision: ast.language_revision,
      previews: ast.previews
    }
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

  defp protect(function) do
    function.()
  rescue
    error in Catena.TypeError -> {:error, error.diagnostic}
  end
end
