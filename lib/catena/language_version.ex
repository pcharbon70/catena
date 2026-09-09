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
    api_and_abi_compatibility: "0.1.24",
    values_and_evaluation: "0.1.25",
    evaluation_order: "0.1.26",
    bindings_and_sequencing: "0.1.27",
    functions_and_calls: "0.1.28",
    branching: "0.1.29",
    equality_and_ordering: "0.1.30",
    recursion_and_termination: "0.1.31",
    runtime_failure_taxonomy: "0.1.32",
    resource_observability: "0.1.33",
    compile_time_evaluation: "0.1.34",
    built_in_data_model: "0.1.35",
    structural_records_and_variants: "0.1.36",
    collection_construction_and_update: "0.1.37",
    pattern_contexts: "0.1.38",
    list_comprehensions: "0.1.39",
    numeric_relationships: "0.1.40",
    aliases_and_newtypes: "0.1.41",
    name_resolution: "0.1.42",
    dynamic_and_unsafe_boundaries: "0.1.43",
    excluded_advanced_type_features: "0.1.44",
    progress_and_preservation: "0.1.45",
    selective_receive: "0.1.46",
    exception_boundary: "0.1.47",
    top_level_effects: "0.1.48",
    selective_receive_correction: "0.1.49",
    closed_capability_kernel: "0.1.50",
    resource_scopes: "0.1.51",
    owned_task_lifetimes: "0.1.52",
    cancellation_and_time: "0.1.53",
    outcome_contracts: "0.1.54",
    local_protocol_contracts: "0.1.55",
    typed_supervision: "0.1.56",
    otp_compatibility: "0.1.57",
    value_boundaries: "0.1.58",
    calling_conventions: "0.1.59",
    erlang_type_boundary: "0.1.60",
    foreign_adapters: "0.1.61",
    native_value_roles: "0.1.62",
    native_services: "0.1.63",
    debugging_metadata: "0.1.64",
    collection_protocols: "0.1.65",
    text_binary_model: "0.1.66"
  ]
  @ordered Keyword.values(@versions)
  @json_frontends ~w(0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7)
  @kernel_frontends ~w(0.1.8)
  @source_text_frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47 0.1.48 0.1.49 0.1.50 0.1.51 0.1.52 0.1.53 0.1.54 0.1.55 0.1.56 0.1.57 0.1.58 0.1.59 0.1.60 0.1.61 0.1.62 0.1.63 0.1.64 0.1.65 0.1.66)
  @capability_frontends ~w(0.1.50)
  @resource_frontends ~w(0.1.51)
  @task_frontends ~w(0.1.52)
  @time_frontends ~w(0.1.53)
  @protocol_frontends ~w(0.1.55)
  @supervision_frontends ~w(0.1.56)
  @value_frontends ~w(0.1.58)
  @calling_frontends ~w(0.1.59)
  @foreign_frontends ~w(0.1.61)
  @debug_frontends ~w(0.1.64)
  @text_frontends ~w(0.1.66)
  @compilable @json_frontends ++
                @kernel_frontends ++
                @capability_frontends ++
                @resource_frontends ++
                @task_frontends ++
                @time_frontends ++
                @protocol_frontends ++
                @supervision_frontends ++
                @value_frontends ++
                @calling_frontends ++ @foreign_frontends ++ @debug_frontends ++ @text_frontends
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
          | :values_and_evaluation
          | :evaluation_order
          | :bindings_and_sequencing
          | :functions_and_calls
          | :branching
          | :equality_and_ordering
          | :recursion_and_termination
          | :runtime_failure_taxonomy
          | :resource_observability
          | :compile_time_evaluation
          | :built_in_data_model
          | :structural_records_and_variants
          | :collection_construction_and_update
          | :pattern_contexts
          | :list_comprehensions
          | :numeric_relationships
          | :aliases_and_newtypes
          | :name_resolution
          | :dynamic_and_unsafe_boundaries
          | :excluded_advanced_type_features
          | :progress_and_preservation
          | :selective_receive
          | :exception_boundary
          | :top_level_effects
          | :selective_receive_correction
          | :closed_capability_kernel
          | :resource_scopes
          | :owned_task_lifetimes
          | :cancellation_and_time
          | :outcome_contracts
          | :local_protocol_contracts
          | :typed_supervision
          | :otp_compatibility
          | :value_boundaries
          | :calling_conventions
          | :erlang_type_boundary
          | :foreign_adapters
          | :native_value_roles

  @spec all() :: [String.t()]
  def all, do: @ordered

  @spec json_frontend_versions() :: [String.t()]
  def json_frontend_versions, do: @json_frontends

  @spec kernel_frontend_versions() :: [String.t()]
  def kernel_frontend_versions, do: @kernel_frontends

  @spec capability_frontend_versions() :: [String.t()]
  def capability_frontend_versions, do: @capability_frontends

  def foreign_frontend_versions, do: @foreign_frontends

  def calling_frontend_versions, do: @calling_frontends

  def value_frontend_versions, do: @value_frontends

  def supervision_frontend_versions, do: @supervision_frontends

  def protocol_frontend_versions, do: @protocol_frontends

  def time_frontend_versions, do: @time_frontends

  def task_frontend_versions, do: @task_frontends

  def resource_frontend_versions, do: @resource_frontends

  @spec source_text_frontend_versions() :: [String.t()]
  def source_text_frontend_versions, do: @source_text_frontends

  @spec compilable_revisions() :: [String.t()]
  def compilable_revisions, do: @compilable

  @spec compilable_from(feature()) :: [String.t()]
  def compilable_from(feature), do: Enum.filter(from(feature), &(&1 in @compilable))

  @spec interface_versions() :: [String.t()]
  def interface_versions, do: @interfaces

  @spec artifact_versions() :: [String.t()]
  def artifact_versions,
    do:
      @interfaces ++
        @capability_frontends ++
        @resource_frontends ++
        @task_frontends ++
        @time_frontends ++
        @protocol_frontends ++
        @supervision_frontends ++
        @value_frontends ++
        @calling_frontends ++ @foreign_frontends ++ @debug_frontends ++ @text_frontends

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
