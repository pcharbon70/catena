defmodule Catena.OTP.Compiler do
  @moduledoc "The sole `.beam` production boundary: OTP 29 `compile:noenv_forms/2`."

  alias Catena.{Diagnostic, ImplementationLimits, LanguageVersion}

  @default_version LanguageVersion.introduced(:data_and_patterns)
  @selection_versions LanguageVersion.compilable_from(:editions_and_feature_lifecycle)

  @spec compile([term()], keyword()) ::
          {:ok, module(), binary(), [term()]} | {:error, Diagnostic.t()}
  def compile(forms, options \\ []) do
    source = Keyword.get(options, :source, "<catena-json>")

    specification =
      options |> Keyword.get(:specification, @default_version) |> String.to_charlist()

    frontend =
      options
      |> Keyword.get_lazy(:frontend, fn ->
        "json-ast-" <> Keyword.get(options, :frontend_version, @default_version)
      end)
      |> String.to_charlist()

    compile_info = [{:catena_specification, specification}, {:catena_frontend, frontend}]

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
end
