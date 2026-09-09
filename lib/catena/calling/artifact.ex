defmodule Catena.Calling.Artifact do
  @moduledoc "Exact 0.1.59 call artifact binding by deterministic rebuild, not trusted embedded claims."
  alias Catena.Calling.Descriptor

  def build(core, options \\ []) do
    with :ok <- Catena.ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- Catena.ImplementationLimits.validate_source_arities(core),
         {:ok, descriptor} <- Descriptor.build(core, options),
         {:ok, module, binary, warnings} <- compile(core, descriptor, options) do
      {:ok,
       %{
         module: module,
         binary: binary,
         descriptor: descriptor,
         binary_digest: digest(binary),
         warnings: warnings
       }}
    end
  rescue
    _ -> {:error, :invalid_call_artifact}
  end

  def verify(artifact, core, options \\ []) do
    case build(core, options) do
      {:ok, ^artifact} -> :ok
      _ -> {:error, :invalid_call_artifact}
    end
  end

  defp compile(%{format: :kernel_core} = core, descriptor, _options) do
    # Retain the profile owner's verification and all literal limits before
    # compiling this calling extension of its forms.
    with {:ok, _, _, _} <- Catena.Kernel.Backend.compile(core) do
      Catena.OTP.Compiler.compile_calling(
        Catena.Kernel.Backend.lower(core, calling: true),
        descriptor,
        source: core.origin,
        frontend: "calling-0.1.59",
        specification: "0.1.59",
        artifact_version: "0.1.59",
        language_selection: descriptor.selection
      )
    end
  end

  defp compile(core, descriptor, options) do
    Catena.OTP.Compiler.compile_calling(lower(core, options), descriptor,
      source: core.origin,
      frontend: "calling-0.1.59",
      specification: "0.1.59",
      artifact_version: "0.1.59",
      language_selection: descriptor.selection
    )
  end

  defp lower(core, options),
    do: Catena.Backend.ErlangAbstract.lower(core, Keyword.put(options, :calling, true))

  defp digest(binary), do: :crypto.hash(:sha256, binary) |> Base.encode16(case: :lower)
end
