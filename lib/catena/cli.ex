defmodule Catena.CLI do
  @moduledoc "Command-line entry point for the versioned JSON AST compiler."

  alias Catena.Report

  def main(arguments) do
    case arguments do
      ["check-ir", path] -> check(path)
      ["elaborate-ir", path] -> elaborate(path)
      ["compile-ir", path] -> compile(path)
      _ -> halt_with("usage: catena {check-ir|elaborate-ir|compile-ir} FILE.json", 64)
    end
  end

  defp check(path) do
    case path |> File.read!() |> Catena.check_json() do
      {:ok, core} -> print(%{status: "ok", module: Report.module(core)})
      {:error, diagnostic} -> diagnostic(diagnostic)
    end
  end

  defp elaborate(path), do: check(path)

  defp compile(path) do
    case path |> File.read!() |> Catena.compile_json(source: Path.expand(path)) do
      {:ok, module, binary, metadata} ->
        output = Path.join(Path.dirname(path), Atom.to_string(module) <> ".beam")
        File.write!(output, binary)

        print(%{
          status: "ok",
          module: Atom.to_string(module),
          output: output,
          warnings: inspect(metadata.warnings)
        })

      {:error, diagnostic} ->
        diagnostic(diagnostic)
    end
  end

  defp diagnostic(diagnostic) do
    IO.puts(:stderr, JSON.encode!(%{status: "error", diagnostic: Report.diagnostic(diagnostic)}))
    System.halt(1)
  end

  defp print(value), do: IO.puts(JSON.encode!(value))

  defp halt_with(message, status) do
    IO.puts(:stderr, message)
    System.halt(status)
  end
end
