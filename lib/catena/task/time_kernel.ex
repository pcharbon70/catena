defmodule Catena.Task.TimeKernel do
  @moduledoc "Checked cancellation and time compound target at exact 0.1.53."
  alias Catena.Kernel.{CapabilityKernel, Verifier}

  def check(parsed, bindings, options \\ []) do
    with {:ok, _} <- selection(options),
         {:ok, core} <- Catena.Task.Kernel.check(parsed, bindings) do
      core =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          core,
          &Map.put(&2, &1, "0.1.53")
        )
        |> Map.put(:profile, :cancellation_and_time)

      case Verifier.verify(core) do
        :ok ->
          {:ok, core}

        {:error, reason} ->
          {:error, Catena.Diagnostic.new("I001", "time-core verification failed: #{reason}")}
      end
    end
  end

  def selection(options) do
    requested =
      Keyword.get(options, :language_selection, %Catena.LanguageSelection{
        edition: "0.1",
        language_revision: "0.1.53",
        previews: []
      })

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{edition: "0.1", language_revision: "0.1.53", previews: []} = selected} ->
        {:ok, selected}

      {:ok, _} ->
        {:error, Catena.Diagnostic.new("EDN001", "time-tree input requires exact 0.1.53")}

      error ->
        error
    end
  end

  def compile(core) do
    alias Catena.{ImplementationLimits, OTP.Compiler}

    with :ok <- Verifier.verify(core),
         true <- core.version == "0.1.53",
         %{arity: 0} = entry <- Enum.find(core.definitions, &(&1.name == "main")),
         true <- MapSet.size(CapabilityKernel.slots([entry.signature, entry.uses])) == 0,
         true <- Catena.Kernel.Type.closed?(entry.signature),
         :ok <- ImplementationLimits.validate_source_arities(core),
         {:ok, forms} <- Catena.Task.Instrument.lower(core),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, selected} <- selection([]),
         {:ok, module, binary, warnings} <-
           Compiler.compile(forms,
             source: core.origin,
             artifact_version: "0.1.53",
             frontend_version: "0.1.53",
             frontend: "time-tree-0.1.53",
             specification: "0.1.53",
             language_selection: selected
           ) do
      {:ok, module, binary,
       %{
         core: core,
         forms: forms,
         warnings: warnings,
         diagnostics: core.diagnostics,
         selection: selected,
         artifact_version: "0.1.53",
         layout: :fixed,
         interface: nil,
         interface_binary: nil
       }}
    else
      {:error, %Catena.Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      {:error, reason} ->
        {:error, Catena.Diagnostic.new("I001", "time-core verification failed: #{reason}")}

      _ ->
        {:error, Catena.Diagnostic.new("EFX003", "time artifact requires a closed main entry")}
    end
  end
end
