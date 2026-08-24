defmodule Catena.Package.Manifest do
  @moduledoc "Strict decoder for retained and edition-aware Catena package manifests."

  alias Catena.{Diagnostic, LanguageLifecycle, LanguageVersion}

  @categorical_version LanguageVersion.introduced(:traits_and_categories)
  @governance_version LanguageVersion.introduced(:specifications_and_governance)
  @edition_version LanguageVersion.introduced(:editions_and_feature_lifecycle)
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
    with {:ok, selection, advisories} <- legacy_selection(value, @categorical_version),
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
         version: @categorical_version,
         artifact_version: @categorical_version,
         selection: selection,
         advisories: advisories,
         denied_diagnostics: [],
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
      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      _ ->
        malformed(
          "malformed Catena 0.1.4 package manifest; explicit modules, interfaces, roots, companion_module, and output are required"
        )
    end
  end

  defp decode_version(%{"version" => @governance_version} = value) do
    with {:ok, selection, advisories} <- legacy_selection(value, @governance_version),
         package when is_binary(package) and byte_size(package) > 0 <- Map.get(value, "package"),
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
         artifact_version: @governance_version,
         selection: selection,
         advisories: advisories,
         denied_diagnostics: [],
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
      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      _ ->
        malformed(
          "malformed Catena 0.1.6 package manifest; package, modules, interfaces, companion_module, output, assurance, and profile are required"
        )
    end
  end

  defp decode_version(%{"version" => @edition_version} = value) do
    with {:ok, selection} <- LanguageVersion.resolve_selection(value),
         :ok <- require_compilable_selection(selection),
         package when is_binary(package) and byte_size(package) > 0 <- Map.get(value, "package"),
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
         true <- is_nil(governance) or is_binary(governance),
         {:ok, dependencies} <- dependencies(Map.get(value, "dependencies", %{})),
         {:ok, prelude} <- prelude(Map.get(value, "prelude")),
         {:ok, denied_diagnostics} <- diagnostics(Map.get(value, "diagnostics", %{})) do
      {:ok,
       %{
         version: @edition_version,
         artifact_version: @edition_version,
         selection: selection,
         advisories: [],
         denied_diagnostics: denied_diagnostics,
         governed?: is_binary(governance),
         package: package,
         profile: profile,
         governance: governance,
         assurance: assurance,
         companion_module: module,
         modules: modules,
         interfaces: interfaces,
         roots: roots,
         output: output,
         dependencies: dependencies,
         prelude: prelude
       }}
    else
      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      _ ->
        malformed(
          "malformed Catena 0.1.7 package manifest; edition, language_revision, previews, package, modules, interfaces, companion_module, output, assurance, and profile are required"
        )
    end
  end

  defp decode_version(_value), do: malformed("unsupported Catena package manifest version")

  defp require_compilable_selection(selection) do
    if selection.language_revision in LanguageVersion.compilable_revisions() do
      :ok
    else
      {:error,
       Diagnostic.new(
         "EDN001",
         "package IR compilation does not support source-only language revision #{selection.language_revision}",
         path: "$.language_revision",
         details: %{
           frontend: "package-ir",
           selected: selection.language_revision,
           supported: LanguageVersion.compilable_revisions()
         }
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

  defp dependencies(%{} = deps) do
    valid? =
      Enum.all?(deps, fn {name, req} ->
        is_binary(name) and is_binary(req) and name != "" and req != ""
      end)

    if valid? do
      {:ok, deps}
    else
      {:error,
       Diagnostic.new("PKG001", "dependencies must map package names to requirement strings",
         path: "$.dependencies"
       )}
    end
  end

  defp dependencies(_) do
    {:error,
     Diagnostic.new("PKG001", "dependencies must map package names to requirement strings",
       path: "$.dependencies"
     )}
  end

  defp prelude(nil), do: {:ok, nil}

  defp prelude(%{"package" => package, "requirement" => requirement})
       when is_binary(package) and is_binary(requirement) do
    {:ok, %{"package" => package, "requirement" => requirement}}
  end

  defp prelude(_), do: {:error, prelude_error()}

  defp prelude_error do
    Diagnostic.new("PRE001", "prelude must be a package and requirement object",
      path: "$.prelude"
    )
  end

  defp diagnostics(%{} = diagnostics) do
    denied = Map.get(diagnostics, "deny", [])
    LanguageLifecycle.validate_denied_diagnostics(denied)
  end

  defp diagnostics(_diagnostics) do
    {:error, Diagnostic.new("EDN001", "diagnostics must be an object", path: "$.diagnostics")}
  end

  defp legacy_selection(value, revision) do
    fields = ~w(edition language_revision previews)
    present = Enum.filter(fields, &Map.has_key?(value, &1))
    inferred = LanguageVersion.legacy_selection(revision)

    case present do
      [] ->
        {:ok, inferred, [legacy_advisory(revision)]}

      ^fields ->
        with {:ok, selection} <- LanguageVersion.resolve_selection(value),
             true <- selection == inferred do
          {:ok, selection, []}
        else
          {:error, %Diagnostic{} = diagnostic} ->
            {:error, diagnostic}

          false ->
            {:error,
             Diagnostic.new(
               "EDN001",
               "explicit language revision must match legacy manifest version #{revision}",
               path: "$.language_revision",
               details: %{
                 manifest_version: revision,
                 language_revision: Map.get(value, "language_revision")
               }
             )}
        end

      _partial ->
        {:error,
         Diagnostic.new(
           "EDN001",
           "legacy language selection must provide edition, language_revision, and previews together",
           path: "$",
           details: %{present: present, required: fields}
         )}
    end
  end

  defp legacy_advisory(revision) do
    %Diagnostic{
      id: "EDN002",
      severity: :warning,
      message: "legacy manifest implies edition 0.1 and language revision #{revision}",
      path: "$",
      details: %{edition: "0.1", language_revision: revision, previews: []},
      fixes: [
        json_add("$.edition", "0.1"),
        json_add("$.language_revision", revision),
        json_add("$.previews", [])
      ]
    }
  end

  defp json_add(path, value) do
    %{
      "kind" => "json-edit",
      "operation" => "add",
      "path" => path,
      "value" => value,
      "applicability" => "machine-applicable"
    }
  end

  defp malformed(message),
    do: {:error, Diagnostic.new("LNK001", message, path: "$")}
end
