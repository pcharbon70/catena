defmodule Catena.CLI do
  @moduledoc "Command-line entry point for the versioned JSON AST compiler."

  alias Catena.{Interface, Report}
  alias Catena.Package.Linker

  def main(arguments) do
    {options, positional, invalid} =
      OptionParser.parse(arguments,
        strict: [interface: :keep, layout: :string, condition_lowering: :string],
        aliases: [i: :interface]
      )

    if invalid != [], do: halt_with(usage(), 64)

    with {:ok, interfaces} <- load_interfaces(Keyword.get_values(options, :interface)),
         {:ok, layout} <- layout(Keyword.get(options, :layout, "compact")),
         {:ok, condition_lowering} <-
           condition_lowering(Keyword.get(options, :condition_lowering, "auto")) do
      compiler_options = [
        interfaces: interfaces,
        layout: layout,
        condition_lowering: condition_lowering
      ]

      case positional do
        ["check-ir", path] -> check(path, compiler_options)
        ["elaborate-ir", path] -> check(path, compiler_options)
        ["compile-ir", path] -> compile(path, compiler_options)
        ["compile-package-ir", path] -> compile_package(path, compiler_options)
        _ -> halt_with(usage(), 64)
      end
    else
      {:error, diagnostic} -> diagnostic(diagnostic)
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
          evidence_erased: result.evidence_erased
        })

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

  defp diagnostic(diagnostic) do
    IO.puts(:stderr, JSON.encode!(%{status: "error", diagnostic: Report.diagnostic(diagnostic)}))
    System.halt(1)
  end

  defp print(value), do: IO.puts(JSON.encode!(value))

  defp halt_with(message, status) do
    IO.puts(:stderr, message)
    System.halt(status)
  end

  defp usage do
    "usage: catena [--interface FILE.cati.json] [--layout compact|uniform] " <>
      "[--condition-lowering auto|native|ordinary] " <>
      "{check-ir|elaborate-ir|compile-ir|compile-package-ir} FILE.json"
  end
end
