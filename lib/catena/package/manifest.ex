defmodule Catena.Package.Manifest do
  @moduledoc "Strict decoder for the Catena 0.4 toolchain-only package build manifest."

  alias Catena.Diagnostic

  @module_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @value_name ~r/^[a-z][A-Za-z0-9_]*$/

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(binary) do
    with {:ok, value} <- JSON.decode(binary),
         true <- is_map(value),
         "catena-package-manifest" <- Map.get(value, "format"),
         "0.4" <- Map.get(value, "version"),
         module when is_binary(module) <- Map.get(value, "companion_module"),
         true <- Regex.match?(@module_name, module),
         modules when is_list(modules) <- Map.get(value, "modules", []),
         true <- Enum.all?(modules, &valid_module?/1),
         interfaces when is_list(interfaces) <- Map.get(value, "interfaces", []),
         true <- Enum.all?(interfaces, &is_binary/1),
         roots when is_list(roots) and roots != [] <- Map.get(value, "roots"),
         true <- Enum.all?(roots, &valid_root?/1),
         output when is_binary(output) <- Map.get(value, "output") do
      {:ok,
       %{
         companion_module: module,
         modules: modules,
         interfaces: interfaces,
         roots: roots,
         output: output
       }}
    else
      _ ->
        {:error,
         Diagnostic.new(
           "LNK001",
           "malformed Catena 0.4 package manifest; explicit modules, interfaces, roots, companion_module, and output are required",
           path: "$"
         )}
    end
  end

  defp valid_module?(%{"source" => source, "beam" => beam, "interface" => interface}),
    do: is_binary(source) and is_binary(beam) and is_binary(interface)

  defp valid_module?(_module), do: false

  defp valid_root?(%{"template" => template, "export" => export} = root) do
    is_binary(template) and is_binary(export) and Regex.match?(@value_name, export) and
      is_list(Map.get(root, "types", [])) and is_list(Map.get(root, "instances", []))
  end

  defp valid_root?(_root), do: false
end
