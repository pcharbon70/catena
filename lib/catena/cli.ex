defmodule Catena.CLI do
  @moduledoc "Command-line entry point for the versioned JSON AST compiler."

  alias Catena.{Assurance, Interface, LanguageInfo, LanguageSelection, LanguageVersion, Report}
  alias Catena.Governance.TrustRoot
  alias Catena.Package.Linker

  def main(arguments) do
    {options, positional, invalid} =
      OptionParser.parse(arguments,
        strict: [
          interface: :keep,
          layout: :string,
          condition_lowering: :string,
          action: :string,
          trust_root: :string,
          edition: :string,
          language_revision: :string,
          preview: :keep,
          deny_diagnostic: :keep
        ],
        aliases: [i: :interface]
      )

    if invalid != [], do: halt_with(usage(), 64)

    case positional do
      ["language-info"] ->
        print(LanguageInfo.document())

      _ ->
        with {:ok, interfaces} <- load_interfaces(Keyword.get_values(options, :interface)),
             {:ok, layout} <- layout(Keyword.get(options, :layout, "compact")),
             {:ok, condition_lowering} <-
               condition_lowering(Keyword.get(options, :condition_lowering, "auto")),
             {:ok, language_selection} <- language_selection(options) do
          compiler_options =
            [
              interfaces: interfaces,
              layout: layout,
              condition_lowering: condition_lowering,
              denied_diagnostics:
                options |> Keyword.get_values(:deny_diagnostic) |> Enum.uniq() |> Enum.sort()
            ]
            |> put_option(:action, Keyword.get(options, :action))
            |> put_option(:trust_root, Keyword.get(options, :trust_root))
            |> put_option(:language_selection, language_selection)

          case positional do
            ["check-ir", path] -> check(path, compiler_options)
            ["elaborate-ir", path] -> check(path, compiler_options)
            ["compile-ir", path] -> compile(path, compiler_options)
            ["compile-package-ir", path] -> compile_package(path, compiler_options)
            ["verify-assurance", path] -> verify_assurance(path, compiler_options)
            _ -> halt_with(usage(), 64)
          end
        else
          {:error, diagnostic} -> diagnostic(diagnostic)
        end
    end
  end

  defp check(path, options) do
    case path |> File.read!() |> Catena.check_json(options) do
      {:ok, core} -> print(%{status: "ok", module: Report.module(core)})
      {:error, diagnostic} -> diagnostic(diagnostic)
    end
  end

  defp compile(path, options) do
    case path
         |> File.read!()
         |> Catena.compile_json(Keyword.put(options, :source, Path.expand(path))) do
      {:ok, module, binary, metadata} ->
        directory = Path.dirname(path)
        stem = Atom.to_string(module)
        beam_output = Path.join(directory, stem <> ".beam")
        interface_output = Path.join(directory, stem <> ".cati.json")
        File.write!(beam_output, binary)
        File.write!(interface_output, metadata.interface_binary)

        print(%{
          status: "ok",
          module: stem,
          output: beam_output,
          interface: interface_output,
          selection: LanguageSelection.to_map(metadata.selection),
          diagnostics: Enum.map(metadata.diagnostics, &Report.diagnostic/1),
          layout: Atom.to_string(metadata.layout),
          condition_lowering: Atom.to_string(metadata.condition_lowering),
          warnings: inspect(metadata.warnings)
        })

      {:error, diagnostic} ->
        diagnostic(diagnostic)
    end
  end

  defp compile_package(path, options) do
    case Linker.compile_manifest(path, options) do
      {:ok, result} ->
        print(%{
          status: "ok",
          module: Atom.to_string(result.module),
          output: result.output,
          module_outputs: result.module_outputs,
          specialization_keys: result.specialization_keys,
          selection: LanguageSelection.to_map(result.selection),
          diagnostics: Enum.map(result.diagnostics, &Report.diagnostic/1),
          evidence_erased: result.evidence_erased,
          assurance: result.assurance,
          assurance_digest: result.assurance_digest,
          signing_payload: result.signing_payload,
          signing_payload_digest: result.signing_payload_digest
        })

      {:error, diagnostic} ->
        diagnostic(diagnostic)
    end
  end

  defp verify_assurance(path, options) do
    with trust_path when is_binary(trust_path) <- Keyword.get(options, :trust_root),
         {:ok, root} <- trust_path |> File.read!() |> TrustRoot.decode(),
         {:ok, result} <- path |> File.read!() |> Assurance.verify(Path.dirname(path), root) do
      print(%{
        status: "ok",
        package: result.package,
        action: result.action,
        state: result.state,
        digest: result.digest,
        signing_payload_digest: result.payload_digest
      })
    else
      nil ->
        diagnostic(
          Catena.Diagnostic.new("GOV003", "verify-assurance requires --trust-root FILE",
            path: "$"
          )
        )

      {:error, diagnostic} ->
        diagnostic(diagnostic)
    end
  end

  defp load_interfaces(paths) do
    Enum.reduce_while(paths, {:ok, []}, fn path, {:ok, interfaces} ->
      case path |> File.read!() |> Interface.decode() do
        {:ok, interface} -> {:cont, {:ok, [interface | interfaces]}}
        {:error, diagnostic} -> {:halt, {:error, diagnostic}}
      end
    end)
    |> case do
      {:ok, interfaces} -> {:ok, Enum.reverse(interfaces)}
      error -> error
    end
  end

  defp layout("compact"), do: {:ok, :compact}
  defp layout("uniform"), do: {:ok, :uniform}

  defp layout(other),
    do: {:error, Catena.Diagnostic.new("L001", "unknown ADT layout #{inspect(other)}")}

  defp condition_lowering("auto"), do: {:ok, :auto}
  defp condition_lowering("native"), do: {:ok, :native}
  defp condition_lowering("ordinary"), do: {:ok, :ordinary}

  defp condition_lowering(other),
    do: {:error, Catena.Diagnostic.new("CND001", "unknown condition lowering #{inspect(other)}")}

  defp language_selection(options) do
    selected? =
      Keyword.has_key?(options, :edition) or Keyword.has_key?(options, :language_revision) or
        Keyword.has_key?(options, :preview)

    if selected? do
      current = LanguageVersion.current_selection()

      LanguageVersion.resolve_selection(%{
        edition: Keyword.get(options, :edition, current.edition),
        language_revision: Keyword.get(options, :language_revision, current.language_revision),
        previews: options |> Keyword.get_values(:preview) |> Enum.uniq() |> Enum.sort()
      })
    else
      {:ok, nil}
    end
  end

  defp diagnostic(diagnostic) do
    IO.puts(:stderr, JSON.encode!(%{status: "error", diagnostic: Report.diagnostic(diagnostic)}))
    System.halt(1)
  end

  defp print(value), do: IO.puts(JSON.encode!(value))

  defp put_option(options, _key, nil), do: options
  defp put_option(options, key, value), do: Keyword.put(options, key, value)

  defp halt_with(message, status) do
    IO.puts(:stderr, message)
    System.halt(status)
  end

  defp usage do
    "usage: catena [--interface FILE.cati.json] [--layout compact|uniform] " <>
      "[--condition-lowering auto|native|ordinary] " <>
      "[--edition MAJOR.MINOR] [--language-revision MAJOR.MINOR.PATCH] " <>
      "[--preview NAME] [--deny-diagnostic ID] " <>
      "[--action build|publish|activate] [--trust-root FILE] " <>
      "({check-ir|elaborate-ir|compile-ir|compile-package-ir|verify-assurance} FILE.json" <>
      " | language-info)"
  end
end
