defmodule Catena.OTP.Compiler do
  @moduledoc "The sole `.beam` production boundary: OTP 29 `compile:noenv_forms/2`."

  alias Catena.{Diagnostic, ImplementationLimits, LanguageVersion}

  @default_version LanguageVersion.introduced(:data_and_patterns)
  @selection_versions LanguageVersion.compilable_from(:editions_and_feature_lifecycle)

  @doc "Compile a foreign binding sidecar tied to these exact forms and compiler."
  def compile_foreign(forms, description, options) do
    alias Catena.Calling.Descriptor

    if description.forms_digest == Descriptor.digest(forms) and
         description.compiler == Descriptor.compiler_digest() and
         description.version == "0.1.61" do
      compile(forms, Keyword.put(options, :foreign_descriptor, Descriptor.digest(description)))
    else
      {:error, Diagnostic.new("B001", "foreign descriptor does not match compilation")}
    end
  rescue
    _ -> {:error, Diagnostic.new("B001", "malformed foreign descriptor")}
  end

  @doc "Compile a calling sidecar only when it describes these exact forms and compiler."
  def compile_calling(forms, descriptor, options \\ []) do
    expected = Map.delete(descriptor, :digest)

    with true <- descriptor.digest == Catena.Calling.Descriptor.digest(expected),
         true <- descriptor.forms_digest == Catena.Calling.Descriptor.digest(forms),
         true <- descriptor.compiler_digest == Catena.Calling.Descriptor.compiler_digest(),
         {:ok, host} <- Catena.OTP.Profile.require_supported(),
         true <- descriptor.toolchain_digest == Catena.OTP.Profile.digest(host) do
      compile(forms, Keyword.put(options, :calling_descriptor, descriptor.digest))
    else
      _ -> {:error, Diagnostic.new("B001", "calling descriptor does not match compilation")}
    end
  rescue
    _ -> {:error, Diagnostic.new("B001", "malformed calling descriptor")}
  end

  @spec compile([term()], keyword()) ::
          {:ok, module(), binary(), [term()]} | {:error, Diagnostic.t()}
  def compile(forms, options \\ []) do
    with {:ok, fingerprint} <- Catena.OTP.Profile.require_supported() do
      compile_verified_host(forms, options, fingerprint)
    end
  end

  defp compile_verified_host(forms, options, fingerprint) do
    source = Keyword.get(options, :source, "<catena-json>")

    specification =
      options |> Keyword.get(:specification, @default_version) |> String.to_charlist()

    frontend =
      options
      |> Keyword.get_lazy(:frontend, fn ->
        "json-ast-" <> Keyword.get(options, :frontend_version, @default_version)
      end)
      |> String.to_charlist()

    compile_info = [
      {:catena_specification, specification},
      {:catena_frontend, frontend},
      {:catena_toolchain, fingerprint},
      {:catena_toolchain_digest, Catena.OTP.Profile.digest(fingerprint)}
    ]

    compile_info =
      case Keyword.fetch(options, :foreign_descriptor) do
        {:ok, digest} -> compile_info ++ [{:catena_foreign_descriptor, digest}]
        :error -> compile_info
      end

    compile_info =
      case Keyword.fetch(options, :calling_descriptor) do
        {:ok, digest} -> compile_info ++ [{:catena_calling_descriptor, digest}]
        :error -> compile_info
      end

    compile_info =
      case {Keyword.get(options, :artifact_version), Keyword.get(options, :language_selection)} do
        {version, selection} when version in @selection_versions and not is_nil(selection) ->
          compile_info ++
            [
              {:catena_edition, String.to_charlist(selection.edition)},
              {:catena_language_revision, String.to_charlist(selection.language_revision)},
              {:catena_previews, Enum.map(selection.previews, &String.to_charlist/1)}
            ]

        _ ->
          compile_info
      end

    compiler_options = [
      :binary,
      :return_errors,
      :return_warnings,
      :deterministic,
      {:source, String.to_charlist(source)},
      {:compile_info, compile_info}
    ]

    case :compile.noenv_forms(forms, compiler_options) do
      {:ok, module, binary} ->
        with :ok <- ImplementationLimits.validate_generated_module(binary) do
          {:ok, module, binary, []}
        end

      {:ok, module, binary, warnings} ->
        with :ok <- ImplementationLimits.validate_generated_module(binary) do
          {:ok, module, binary, warnings}
        end

      {:error, errors, warnings} ->
        {:error,
         Diagnostic.new("B001", "OTP 29 rejected generated Erlang Abstract Format",
           details: %{errors: inspect(errors), warnings: inspect(warnings)}
         )}
    end
  end

  def load(module, filename, binary) do
    with {:ok, {^module, [compile_info: info]}} <- :beam_lib.chunks(binary, [:compile_info]),
         :ok <-
           Catena.OTP.Profile.artifact_compatible(
             info[:catena_toolchain],
             Catena.OTP.Profile.observe()
           ),
         true <-
           info[:catena_toolchain_digest] == Catena.OTP.Profile.digest(info[:catena_toolchain]) do
      :code.load_binary(module, filename, binary)
    else
      {:error, %Diagnostic{}} = error ->
        error

      _ ->
        {:error,
         Diagnostic.new(
           "OTP003",
           "missing or malformed artifact toolchain provenance; rebuild from retained source"
         )}
    end
  end
end
