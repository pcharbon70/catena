defmodule Catena.Calling.Lifecycle do
  @moduledoc "Calling sidecar for the retained checked OTP supervision lifecycle."
  alias Catena.Calling.Descriptor
  alias Catena.Supervision.Description

  def build(description, options \\ []) do
    with {:ok, selection} <- Descriptor.selection(options),
         {:ok, module, binary, metadata} <- Description.compile(description),
         {:ok, {^module, exports}} <- :beam_lib.chunks(binary, [:exports]),
         true <-
           Enum.all?(metadata.children, fn child ->
             {^module, name, arguments} = child.start
             {name, length(arguments)} in exports[:exports]
           end),
         {:ok, host} <- Catena.OTP.Profile.require_supported() do
      processes =
        Enum.map(metadata.children, fn child ->
          {_, symbol, arguments} = child.start

          %{
            kind: :process_entry,
            module: module,
            symbol: symbol,
            beam_arity: length(arguments),
            parameters: [],
            result: :managed_start_result,
            hidden_arguments: 0,
            child: child.id,
            provisioning: :fresh_empty
          }
        end)

      descriptor = %{
        format: :calling_lifecycle,
        version: "0.1.59",
        selection: selection,
        compiler_digest: Descriptor.compiler_digest(),
        toolchain_digest: Catena.OTP.Profile.digest(host),
        description_digest: Descriptor.digest(description),
        artifact_digest: metadata.manifest["artifact_digest"],
        manifest_digest: Descriptor.digest(metadata.manifest),
        entries:
          processes ++
            [
              %{
                kind: :foreign_call,
                module: Catena.Supervision.Runtime,
                symbol: :start_link,
                beam_arity: 2,
                parameters: [:checked_supervision_flags, :checked_child_specs],
                result: :linked_supervisor_result,
                hidden_arguments: 0,
                authority: :managed_owner
              },
              %{
                kind: :callback,
                module: Catena.Supervision.Runtime,
                symbol: :init,
                beam_arity: 1,
                parameters: [{:tuple, [:checked_supervision_flags, :checked_child_specs]}],
                result: :supervisor_init_result,
                hidden_arguments: 0,
                authority: :otp_supervisor
              }
            ]
      }

      {:ok,
       %{
         module: module,
         binary: binary,
         manifest: metadata.manifest,
         descriptor: Map.put(descriptor, :digest, Descriptor.digest(descriptor))
       }}
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_lifecycle_calling_descriptor}
    end
  rescue
    _ -> {:error, :invalid_lifecycle_calling_descriptor}
  end

  def verify(artifact, description) do
    case build(description) do
      {:ok, ^artifact} -> :ok
      _ -> {:error, :invalid_lifecycle_calling_artifact}
    end
  end

  def start(artifact, description) do
    with :ok <- verify(artifact, description) do
      Description.start_artifact(description, artifact.module, artifact.binary, artifact.manifest)
    end
  end
end
