defmodule Catena.Package.Manifest do
  @moduledoc "Strict decoder for Catena 0.1.4 and governed 0.1.6 package manifests."

  alias Catena.{Diagnostic, LanguageVersion}

  @categorical_version LanguageVersion.introduced(:traits_and_categories)
  @governance_version LanguageVersion.introduced(:specifications_and_governance)
  @module_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @value_name ~r/^[a-z][A-Za-z0-9_]*$/

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(binary) do
    with {:ok, value} <- JSON.decode(binary),
         true <- is_map(value),
         "catena-package-manifest" <- Map.get(value, "format") do
      decode_version(value)
    else
      _ -> malformed("malformed Catena package manifest")
    end
  end

  defp decode_version(%{"version" => @categorical_version} = value) do
    with module when is_binary(module) <- Map.get(value, "companion_module"),
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
         version: @categorical_version,
         governed?: false,
         package: nil,
         profile: nil,
         governance: nil,
         assurance: nil,
         companion_module: module,
         modules: modules,
         interfaces: interfaces,
         roots: roots,
         output: output
       }}
    else
      _ ->
        malformed(
          "malformed Catena 0.1.4 package manifest; explicit modules, interfaces, roots, companion_module, and output are required"
        )
    end
  end

  defp decode_version(%{"version" => @governance_version} = value) do
    with package when is_binary(package) and byte_size(package) > 0 <- Map.get(value, "package"),
         module when is_binary(module) <- Map.get(value, "companion_module"),
         true <- Regex.match?(@module_name, module),
         modules when is_list(modules) <- Map.get(value, "modules", []),
         true <- Enum.all?(modules, &valid_module?/1),
         interfaces when is_list(interfaces) <- Map.get(value, "interfaces", []),
         true <- Enum.all?(interfaces, &is_binary/1),
         roots when is_list(roots) <- Map.get(value, "roots", []),
         true <- Enum.all?(roots, &valid_root?/1),
         output when is_binary(output) <- Map.get(value, "output"),
         assurance when is_binary(assurance) <- Map.get(value, "assurance"),
         profile when is_binary(profile) and byte_size(profile) > 0 <-
           Map.get(value, "profile", "static"),
         governance <- Map.get(value, "governance"),
         true <- is_nil(governance) or is_binary(governance) do
      {:ok,
       %{
         version: @governance_version,
         governed?: is_binary(governance),
         package: package,
         profile: profile,
         governance: governance,
         assurance: assurance,
         companion_module: module,
         modules: modules,
         interfaces: interfaces,
         roots: roots,
         output: output
       }}
    else
      _ ->
        malformed(
          "malformed Catena 0.1.6 package manifest; package, modules, interfaces, companion_module, output, assurance, and profile are required"
        )
    end
  end

  defp decode_version(_value), do: malformed("unsupported Catena package manifest version")

  defp valid_module?(%{"source" => source, "beam" => beam, "interface" => interface}),
    do: is_binary(source) and is_binary(beam) and is_binary(interface)

  defp valid_module?(_module), do: false

  defp valid_root?(%{"template" => template, "export" => export} = root) do
    is_binary(template) and is_binary(export) and Regex.match?(@value_name, export) and
      is_list(Map.get(root, "types", [])) and is_list(Map.get(root, "instances", []))
  end

  defp valid_root?(_root), do: false

  defp malformed(message),
    do: {:error, Diagnostic.new("LNK001", message, path: "$")}
end
