defmodule Catena.LanguageVersion do
  @moduledoc """
  Canonical prototype language-slice versions.

  Catena's current language line is `0.1`. Each implemented semantic slice
  uses the next patch component. Compiler-package releases are versioned
  independently in `mix.exs`.
  """

  alias Catena.{Diagnostic, LanguageLifecycle, LanguageSelection}

  @versions [
    type_system: "0.1.1",
    data_and_patterns: "0.1.2",
    clause_conditions: "0.1.3",
    traits_and_categories: "0.1.4",
    effects_and_handlers: "0.1.5",
    specifications_and_governance: "0.1.6",
    editions_and_feature_lifecycle: "0.1.7",
    formal_semantic_kernel: "0.1.8",
    source_text: "0.1.9",
    identifiers: "0.1.10",
    whitespace_and_layout: "0.1.11",
    comments_and_documentation_comments: "0.1.12",
    literal_grammar: "0.1.13",
    numeric_literal_semantics: "0.1.14",
    operators_and_punctuation: "0.1.15",
    files_and_modules: "0.1.16",
    namespaces_and_shadowing: "0.1.17",
    imports_and_exports: "0.1.18",
    abstraction_boundaries: "0.1.19",
    module_dependency_cycles: "0.1.20",
    package_identity_and_dependencies: "0.1.21",
    prelude_policy: "0.1.22",
    entry_points: "0.1.23",
    api_and_abi_compatibility: "0.1.24"
  ]
  @ordered Keyword.values(@versions)
  @json_frontends ~w(0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7)
  @kernel_frontends ~w(0.1.8)
  @source_text_frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24)
  @compilable @json_frontends ++ @kernel_frontends
  @interfaces ~w(0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7 0.1.8)
  @signed_formats ~w(0.1.6 0.1.7 0.1.8)
  @retired ~w(0.1 0.2 0.3 0.4 0.5 0.6)
  @core_semver ~r/^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$/

  @type feature ::
          :type_system
          | :data_and_patterns
          | :clause_conditions
          | :traits_and_categories
          | :effects_and_handlers
          | :specifications_and_governance
          | :editions_and_feature_lifecycle
          | :formal_semantic_kernel
          | :source_text
          | :identifiers
          | :whitespace_and_layout
          | :comments_and_documentation_comments
          | :literal_grammar
          | :numeric_literal_semantics
          | :operators_and_punctuation
          | :files_and_modules
          | :namespaces_and_shadowing
          | :imports_and_exports
          | :abstraction_boundaries
          | :module_dependency_cycles
          | :package_identity_and_dependencies
          | :prelude_policy
          | :entry_points
          | :api_and_abi_compatibility

  @spec all() :: [String.t()]
  def all, do: @ordered

  @spec json_frontend_versions() :: [String.t()]
  def json_frontend_versions, do: @json_frontends

  @spec kernel_frontend_versions() :: [String.t()]
  def kernel_frontend_versions, do: @kernel_frontends

  @spec source_text_frontend_versions() :: [String.t()]
  def source_text_frontend_versions, do: @source_text_frontends

  @spec compilable_revisions() :: [String.t()]
  def compilable_revisions, do: @compilable

  @spec compilable_from(feature()) :: [String.t()]
  def compilable_from(feature), do: Enum.filter(from(feature), &(&1 in @compilable))

  @spec interface_versions() :: [String.t()]
  def interface_versions, do: @interfaces

  @spec artifact_versions() :: [String.t()]
  def artifact_versions, do: @interfaces

  @spec signed_format_versions() :: [String.t()]
  def signed_format_versions, do: @signed_formats

  @spec retired() :: [String.t()]
  def retired, do: @retired

  @spec latest() :: String.t()
  def latest, do: List.last(@ordered)

  @spec editions() :: [map()]
  def editions do
    [
      %{
        "id" => "0.1",
        "status" => "prototype",
        "revisions" => @ordered
      }
    ]
  end

  @spec current_selection() :: LanguageSelection.t()
  def current_selection do
    %LanguageSelection{edition: "0.1", language_revision: latest(), previews: []}
  end

  @spec legacy_selection(String.t()) :: LanguageSelection.t()
  def legacy_selection(revision) when revision in @ordered do
    %LanguageSelection{edition: "0.1", language_revision: revision, previews: []}
  end

  @spec resolve_selection(map() | LanguageSelection.t()) ::
          {:ok, LanguageSelection.t()} | {:error, Diagnostic.t()}
  def resolve_selection(%LanguageSelection{} = selection), do: validate_selection(selection)

  def resolve_selection(selection) when is_map(selection) do
    resolved = %LanguageSelection{
      edition: Map.get(selection, :edition, Map.get(selection, "edition")),
      language_revision:
        Map.get(selection, :language_revision, Map.get(selection, "language_revision")),
      previews: Map.get(selection, :previews, Map.get(selection, "previews"))
    }

    validate_selection(resolved)
  end

  def resolve_selection(_selection), do: selection_error("language selection must be an object")

  @spec introduced(feature()) :: String.t()
  def introduced(feature), do: Keyword.fetch!(@versions, feature)

  @spec from(feature()) :: [String.t()]
  def from(feature) do
    introduced = introduced(feature)
    Enum.drop_while(@ordered, &(&1 != introduced))
  end

  @spec before(feature()) :: [String.t()]
  def before(feature) do
    introduced = introduced(feature)
    Enum.take_while(@ordered, &(&1 != introduced))
  end

  @spec internal_representation(String.t()) :: String.t()
  def internal_representation("0.1.1"), do: "0.1.2"
  def internal_representation(version) when version in @compilable, do: version

  @spec default_artifact_version(String.t(), String.t()) :: String.t()
  def default_artifact_version(frontend_format, language_revision)
      when frontend_format in @compilable and language_revision in @compilable do
    if frontend_format == language_revision do
      internal_representation(frontend_format)
    else
      introduced(:editions_and_feature_lifecycle)
    end
  end

  @spec at_or_after?(String.t(), String.t()) :: boolean()
  def at_or_after?(left, right) do
    case {Enum.find_index(@ordered, &(&1 == left)), Enum.find_index(@ordered, &(&1 == right))} do
      {left_index, right_index} when is_integer(left_index) and is_integer(right_index) ->
        left_index >= right_index

      _ ->
        false
    end
  end

  @spec between?(String.t(), String.t(), String.t()) :: boolean()
  def between?(revision, first, last),
    do: at_or_after?(revision, first) and at_or_after?(last, revision)

  @spec valid_core_semver?(term()) :: boolean()
  def valid_core_semver?(version) when is_binary(version),
    do: Regex.match?(@core_semver, version)

  def valid_core_semver?(_version), do: false

  defp validate_selection(%LanguageSelection{} = selection) do
    cond do
      selection.edition != "0.1" ->
        selection_error(
          "unknown or unsupported edition #{inspect(selection.edition)}",
          "$.edition"
        )

      selection.language_revision not in @ordered ->
        selection_error(
          "unknown or unsupported language revision #{inspect(selection.language_revision)}",
          "$.language_revision"
        )

      not String.starts_with?(selection.language_revision, selection.edition <> ".") ->
        selection_error("language revision does not belong to the selected edition")

      true ->
        case LanguageLifecycle.validate_previews(selection) do
          :ok -> {:ok, selection}
          {:error, _} = error -> error
        end
    end
  end

  defp selection_error(message, path \\ "$") do
    {:error, Diagnostic.new("EDN001", message, path: path)}
  end
end
