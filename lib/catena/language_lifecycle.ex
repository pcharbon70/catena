defmodule Catena.LanguageLifecycle do
  @moduledoc "Deterministic Catena language-feature and migration registry."

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion}

  @states ~w(preview stable withdrawn deprecated removed)
  @warning_ids ~w(DEP001 EDN002 IDN007 IMP001)
  @classifications ~w(breaking compatible-addition compatible-correction)
  @dimensions ~w(source-acceptance static-meaning dynamic-behavior diagnostics interfaces artifacts)
  @identifier ~r/^[a-z][a-z0-9]*(?:-[a-z0-9]+)*$/
  @specification_base "https://github.com/pcharbon70/catena-research/blob/main/60-specification"
  @transitions %{
    "preview" => ~w(stable withdrawn),
    "stable" => ~w(deprecated),
    "deprecated" => ~w(removed),
    "withdrawn" => [],
    "removed" => []
  }

  @spec features() :: [map()]
  def features do
    [
      feature(
        "type-system",
        "0.1.1",
        specification("type-system/type-system-overview.md#type-system-overview")
      ),
      feature(
        "data-and-patterns",
        "0.1.2",
        specification("data-and-patterns/data-and-pattern-overview.md#data-and-pattern-overview")
      ),
      feature(
        "clause-conditions",
        "0.1.3",
        specification("clause-conditions/clause-condition-overview.md#clause-condition-overview")
      ),
      feature(
        "traits-and-categorical-operations",
        "0.1.4",
        specification(
          "traits-and-categorical-operations/trait-and-categorical-operation-overview.md#trait-and-categorical-operation-overview"
        )
      ),
      feature(
        "effects-and-handlers",
        "0.1.5",
        specification(
          "effects-and-handlers/effect-and-handler-overview.md#effect-and-handler-overview"
        )
      ),
      feature(
        "specifications-and-governance",
        "0.1.6",
        specification(
          "specifications-and-governance/overview-and-adoption.md#specification-and-governance-overview-and-adoption"
        )
      ),
      feature(
        "editions-and-feature-lifecycle",
        "0.1.7",
        specification(
          "editions-and-feature-lifecycle/edition-selection-and-applicability.md#edition-selection-and-applicability"
        )
      ),
      feature(
        "source-text",
        "0.1.9",
        specification("source-text/source-text-envelope.md#source-text-envelope")
      ),
      feature(
        "identifiers",
        "0.1.10",
        specification(
          "identifiers/identifier-syntax-and-equivalence.md#identifier-syntax-and-equivalence"
        )
      ),
      feature(
        "whitespace-and-layout",
        "0.1.11",
        specification(
          "whitespace-and-layout/whitespace-and-indentation.md#whitespace-and-indentation"
        )
      ),
      feature(
        "comments-and-documentation-comments",
        "0.1.12",
        specification(
          "comments-and-documentation-comments/comment-lexing-and-layout.md#comment-lexing-and-layout"
        )
      ),
      feature(
        "literal-grammar",
        "0.1.13",
        specification(
          "literal-grammar/literal-forms-and-boundaries.md#literal-forms-and-boundaries"
        )
      ),
      feature(
        "numeric-literal-semantics",
        "0.1.14",
        specification(
          "numeric-literal-semantics/numeric-types-and-literal-typing.md#numeric-types-and-literal-typing"
        )
      ),
      feature(
        "operators-and-punctuation",
        "0.1.15",
        specification(
          "operators-and-punctuation/token-inventory-and-maximal-munch.md#token-inventory-and-maximal-munch"
        )
      ),
      feature(
        "files-and-modules",
        "0.1.16",
        specification(
          "files-and-modules/file-units-and-module-binding.md#file-units-and-module-binding"
        )
      ),
      feature(
        "namespaces-and-shadowing",
        "0.1.17",
        specification(
          "namespaces-and-shadowing/namespace-inventory-and-spelling.md#namespace-inventory-and-spelling"
        )
      ),
      feature(
        "imports-and-exports",
        "0.1.18",
        specification(
          "imports-and-exports/export-declarations-and-visibility.md#export-declarations-and-visibility"
        )
      ),
      feature(
        "abstraction-boundaries",
        "0.1.19",
        specification(
          "abstraction-boundaries/authority-and-representation-exclusions.md#authority-and-representation-exclusions"
        )
      ),
      feature(
        "module-dependency-cycles",
        "0.1.20",
        specification(
          "module-dependency-cycles/scc-admission-and-resolution.md#scc-admission-and-resolution"
        )
      )
    ]
  end

  @spec changes() :: [map()]
  def changes do
    Enum.map(features(), fn entry ->
      %{
        "id" => entry["change"],
        "from" => previous_revision(entry["introduced"]),
        "to" => entry["introduced"],
        "classification" => "compatible-addition",
        "affects" => affected_dimensions(entry["id"]),
        "summary" => "Introduces " <> String.replace(entry["id"], "-", " "),
        "specification" => entry["specification"],
        "migration" => migration(entry["id"]),
        "fixes" => fixes(entry["id"])
      }
    end)
  end

  @spec states() :: [String.t()]
  def states, do: @states

  @spec warning_ids() :: [String.t()]
  def warning_ids, do: @warning_ids

  @spec validate_denied_diagnostics(term(), String.t()) ::
          {:ok, [String.t()]} | {:error, Diagnostic.t()}
  def validate_denied_diagnostics(values, path \\ "$.diagnostics.deny") do
    if is_list(values) and Enum.all?(values, &(&1 in @warning_ids)) and
         values == Enum.sort(Enum.uniq(values)) do
      {:ok, values}
    else
      {:error,
       Diagnostic.new("EDN001", "denied diagnostics must be a sorted list of known warning IDs",
         path: path,
         details: %{known_warning_ids: @warning_ids}
       )}
    end
  end

  @spec preview_ids() :: [String.t()]
  def preview_ids, do: preview_ids(LanguageVersion.latest())

  @spec preview_ids(String.t()) :: [String.t()]
  def preview_ids(revision) do
    features()
    |> Enum.filter(&(state(&1["id"], revision) == {:ok, :preview}))
    |> Enum.map(& &1["id"])
    |> Enum.sort()
  end

  @spec valid_transition?(String.t(), String.t()) :: boolean()
  def valid_transition?(from, to), do: to in Map.get(@transitions, from, [])

  @spec valid_emergency_transition?(map(), map()) :: boolean()
  def valid_emergency_transition?(
        %{"state" => "stable"},
        %{"state" => "removed", "emergency" => emergency}
      ),
      do: valid_emergency_record?(emergency)

  def valid_emergency_transition?(_from, _to), do: false

  @spec valid_registry?([map()]) :: boolean()
  def valid_registry?(entries) when is_list(entries) do
    ids = Enum.map(entries, &Map.get(&1, "id"))

    ids == Enum.uniq(ids) and
      Enum.all?(entries, &valid_feature_entry?/1) and
      Enum.all?(entries, fn entry ->
        is_nil(entry["replacement"]) or entry["replacement"] in ids
      end) and
      entries == Enum.sort_by(entries, &{revision_index(&1["introduced"]), &1["id"]})
  end

  def valid_registry?(_entries), do: false

  @spec valid_changes?([map()]) :: boolean()
  def valid_changes?(entries) when is_list(entries) do
    ids = Enum.map(entries, &Map.get(&1, "id"))

    ids == Enum.uniq(ids) and Enum.all?(entries, &valid_change?/1) and
      entries == Enum.sort_by(entries, &{revision_index(&1["to"]), &1["id"]})
  end

  def valid_changes?(_entries), do: false

  @spec state(String.t(), String.t()) :: {:ok, atom()} | :unavailable | :unknown
  def state(id, revision) when is_binary(id) and is_binary(revision) do
    case Enum.find(features(), &(&1["id"] == id)) do
      nil ->
        :unknown

      feature ->
        state_from_history(feature["history"], revision)
    end
  end

  @spec validate_previews(LanguageSelection.t()) :: :ok | {:error, Diagnostic.t()}
  def validate_previews(%LanguageSelection{} = selection) do
    previews = selection.previews

    cond do
      not is_list(previews) or not Enum.all?(previews, &is_binary/1) ->
        error("previews must be a sorted list of names", "$.previews")

      previews != Enum.sort(Enum.uniq(previews)) ->
        error("previews must be sorted and contain no duplicates", "$.previews")

      true ->
        case Enum.find(previews, fn preview ->
               state(preview, selection.language_revision) != {:ok, :preview}
             end) do
          nil ->
            :ok

          preview ->
            error(
              "preview #{inspect(preview)} is unavailable at #{selection.language_revision}",
              "$.previews"
            )
        end
    end
  end

  @spec validate_interfaces(LanguageSelection.t(), [map()]) ::
          :ok | {:error, Diagnostic.t()}
  def validate_interfaces(%LanguageSelection{} = selection, interfaces)
      when is_list(interfaces) do
    enabled = MapSet.new(selection.previews)

    case Enum.find_value(interfaces, fn interface ->
           required = Map.get(interface, :required_previews, [])

           case Enum.find(required, &(not MapSet.member?(enabled, &1))) do
             nil -> nil
             preview -> {interface, preview}
           end
         end) do
      nil ->
        :ok

      {interface, preview} ->
        {:error,
         Diagnostic.new(
           "PRV002",
           "interface #{interface.module} requires preview #{preview}",
           path: "$.interfaces",
           details: %{
             preview: preview,
             interface: interface.module,
             required_previews: interface.required_previews
           }
         )}
    end
  end

  defp feature(id, revision, specification) do
    %{
      "id" => id,
      "state" => "stable",
      "introduced" => revision,
      "stabilized" => revision,
      "deprecated" => nil,
      "withdrawn" => nil,
      "removed" => nil,
      "replacement" => nil,
      "change" => change_id(revision, id),
      "specification" => specification,
      "history" => [%{"revision" => revision, "state" => "stable"}]
    }
  end

  defp state_from_history(history, revision) do
    history
    |> Enum.filter(&LanguageVersion.at_or_after?(revision, &1["revision"]))
    |> List.last()
    |> case do
      nil -> :unavailable
      entry -> {:ok, String.to_existing_atom(entry["state"])}
    end
  end

  defp valid_history?(history) when is_list(history) and history != [] do
    revisions = Enum.map(history, &Map.get(&1, "revision"))
    states = Enum.map(history, &Map.get(&1, "state"))

    revisions == Enum.uniq(revisions) and
      Enum.all?(revisions, &(&1 in LanguageVersion.all())) and
      revisions == Enum.sort_by(revisions, &revision_index/1) and
      Enum.all?(states, &(&1 in @states)) and
      valid_state_path?(history)
  end

  defp valid_history?(_history), do: false

  defp valid_feature_entry?(entry) when is_map(entry) do
    id = entry["id"]
    history = entry["history"]

    is_binary(id) and Regex.match?(@identifier, id) and valid_history?(history) and
      entry["introduced"] == hd(history)["revision"] and
      entry["state"] == List.last(history)["state"] and
      lifecycle_boundary(entry, history, "stable", "stabilized") and
      lifecycle_boundary(entry, history, "deprecated", "deprecated") and
      lifecycle_boundary(entry, history, "withdrawn", "withdrawn") and
      lifecycle_boundary(entry, history, "removed", "removed") and
      valid_replacement?(entry["replacement"], id) and
      entry["change"] == change_id(entry["introduced"], id) and
      is_binary(entry["specification"]) and String.contains?(entry["specification"], "#")
  end

  defp valid_feature_entry?(_entry), do: false

  defp lifecycle_boundary(entry, history, state, field) do
    expected =
      Enum.find_value(history, fn boundary ->
        if boundary["state"] == state, do: boundary["revision"]
      end)

    entry[field] == expected
  end

  defp valid_replacement?(nil, _id), do: true

  defp valid_replacement?(replacement, id),
    do: is_binary(replacement) and replacement != id and Regex.match?(@identifier, replacement)

  defp valid_change?(entry) when is_map(entry) do
    id = entry["id"]
    target = entry["to"]
    affected = entry["affects"]
    fixes = entry["fixes"]

    is_binary(id) and Regex.match?(@identifier, id) and target in LanguageVersion.all() and
      entry["from"] == previous_revision(target) and
      entry["classification"] in @classifications and is_list(affected) and affected != [] and
      affected == Enum.uniq(affected) and Enum.all?(affected, &(&1 in @dimensions)) and
      is_binary(entry["summary"]) and byte_size(entry["summary"]) > 0 and
      is_binary(entry["specification"]) and String.contains?(entry["specification"], "#") and
      is_binary(entry["migration"]) and byte_size(entry["migration"]) > 0 and is_list(fixes) and
      Enum.all?(fixes, &valid_fix?/1)
  end

  defp valid_change?(_entry), do: false

  defp valid_fix?(fix) when is_map(fix) do
    keys = MapSet.new(Map.keys(fix))
    required = MapSet.new(~w(kind operation path applicability))
    allowed = MapSet.put(required, "value")
    operation = fix["operation"]

    MapSet.subset?(required, keys) and MapSet.subset?(keys, allowed) and
      fix["kind"] == "json-edit" and operation in ~w(add replace remove) and
      (operation == "remove" or Map.has_key?(fix, "value")) and is_binary(fix["path"]) and
      fix["applicability"] in ~w(machine-applicable manual)
  end

  defp valid_fix?(_fix), do: false

  defp valid_state_path?([%{"state" => state}]), do: state in ~w(preview stable)

  defp valid_state_path?([%{"state" => first} = first_entry, second_entry | rest]) do
    first in ~w(preview stable) and valid_boundary_transition?(first_entry, second_entry) and
      valid_state_tail?([second_entry | rest])
  end

  defp valid_state_tail?([_entry]), do: true

  defp valid_state_tail?([first, second | rest]),
    do: valid_boundary_transition?(first, second) and valid_state_tail?([second | rest])

  defp valid_boundary_transition?(%{"state" => from} = first, %{"state" => to} = second),
    do: valid_transition?(from, to) or valid_emergency_transition?(first, second)

  defp valid_emergency_record?(record) when is_map(record) do
    MapSet.new(Map.keys(record)) ==
      MapSet.new(~w(basis affected_rules reason exposure replacement_or_containment migration)) and
      record["basis"] in ~w(security soundness) and
      nonempty_strings?(record["affected_rules"]) and
      Enum.all?(record["affected_rules"], &String.contains?(&1, "#")) and
      Enum.all?(~w(reason exposure replacement_or_containment migration), fn field ->
        is_binary(record[field]) and byte_size(record[field]) > 0
      end)
  end

  defp valid_emergency_record?(_record), do: false

  defp nonempty_strings?(values),
    do: is_list(values) and values != [] and Enum.all?(values, &is_binary/1)

  defp revision_index(revision), do: Enum.find_index(LanguageVersion.all(), &(&1 == revision))

  defp previous_revision("0.1.1"), do: "0.1.0"

  defp previous_revision(revision) do
    revisions = LanguageVersion.all()
    index = Enum.find_index(revisions, &(&1 == revision))
    Enum.at(revisions, index - 1)
  end

  defp affected_dimensions("editions-and-feature-lifecycle"),
    do: ~w(diagnostics interfaces artifacts)

  defp affected_dimensions("type-system"), do: ~w(source-acceptance static-meaning diagnostics)

  defp affected_dimensions("data-and-patterns"),
    do: ~w(source-acceptance static-meaning dynamic-behavior diagnostics interfaces artifacts)

  defp affected_dimensions("clause-conditions"),
    do: ~w(source-acceptance static-meaning dynamic-behavior diagnostics interfaces artifacts)

  defp affected_dimensions("traits-and-categorical-operations"),
    do: ~w(source-acceptance static-meaning dynamic-behavior diagnostics interfaces artifacts)

  defp affected_dimensions("effects-and-handlers"),
    do: ~w(source-acceptance static-meaning dynamic-behavior diagnostics interfaces artifacts)

  defp affected_dimensions("specifications-and-governance"),
    do: ~w(source-acceptance static-meaning diagnostics interfaces artifacts)

  defp affected_dimensions("source-text"), do: ~w(source-acceptance diagnostics)
  defp affected_dimensions("identifiers"), do: ~w(source-acceptance static-meaning diagnostics)

  defp affected_dimensions("whitespace-and-layout"),
    do: ~w(source-acceptance static-meaning diagnostics)

  defp affected_dimensions("comments-and-documentation-comments"),
    do: ~w(source-acceptance static-meaning diagnostics)

  defp affected_dimensions("literal-grammar"), do: ~w(source-acceptance diagnostics)

  defp affected_dimensions("numeric-literal-semantics"),
    do: ~w(static-meaning diagnostics)

  defp affected_dimensions("operators-and-punctuation"),
    do: ~w(source-acceptance static-meaning diagnostics)

  defp affected_dimensions("files-and-modules"),
    do: ~w(source-acceptance diagnostics)

  defp affected_dimensions("namespaces-and-shadowing"),
    do: ~w(static-meaning diagnostics)

  defp affected_dimensions("imports-and-exports"),
    do: ~w(static-meaning diagnostics)

  defp affected_dimensions("abstraction-boundaries"),
    do: ~w(static-meaning diagnostics)

  defp affected_dimensions("module-dependency-cycles"),
    do: ~w(source-acceptance static-meaning diagnostics interfaces)

  defp migration("editions-and-feature-lifecycle"),
    do:
      "Upgrade the manifest format to 0.1.7 and add explicit edition, language_revision, and previews fields."

  defp migration("source-text"),
    do:
      "Select 0.1.9 to validate the ergonomic source envelope; exact 0.1.8 kernel inputs are not migrated or reinterpreted."

  defp migration("identifiers"),
    do:
      "Select 0.1.10 to validate standalone ergonomic identifiers and qualified names; retained JSON and exact kernel names are unchanged."

  defp migration("whitespace-and-layout"),
    do:
      "Select 0.1.11 to classify source layout over lexer-supplied token events; retained JSON and exact kernel inputs are unchanged."

  defp migration("comments-and-documentation-comments"),
    do:
      "Select 0.1.12 to scan comments and attach outer documentation comments to parser-supplied declaration targets; retained frontends and persisted formats are unchanged."

  defp migration("literal-grammar"),
    do:
      "Select 0.1.13 to scan one atomic literal with exact decoded payload and source provenance; retained full-language frontends and persisted formats are unchanged."

  defp migration("numeric-literal-semantics"),
    do:
      "Select 0.1.14 to elaborate scanned numeric literals into typed Int and Float values; literal scanning remains exact 0.1.13 and persisted formats are unchanged."

  defp migration("operators-and-punctuation"),
    do:
      "Select 0.1.15 to tokenize complete source files and resolve operator expressions; identifier, layout, comment, literal, and numeric APIs retain their exact selections and persisted formats are unchanged."

  defp migration("files-and-modules"),
    do:
      "Select 0.1.16 to resolve one .cat file unit against its filename, module-declaration events, and generated marker; the concrete header syntax remains future grammar work and persisted formats are unchanged."

  defp migration("namespaces-and-shadowing"),
    do:
      "Select 0.1.17 to build namespace environments from scope events and resolve references with local-over-imported precedence; import syntax remains future work and persisted formats are unchanged."

  defp migration("imports-and-exports"),
    do:
      "Select 0.1.18 to validate exports and imports against digest-bound export sets and report deny-able unused-import warnings; the concrete use/export punctuation remains future grammar work and persisted formats are unchanged."

  defp migration("abstraction-boundaries"),
    do:
      "Select 0.1.19 for the abstraction boundary: the transparent/abstract pair is the complete authority vocabulary, no stable-layout form exists, and abstract types with validating constructors are the sanctioned invariant idiom; nothing about accepted input changes."

  defp migration("module-dependency-cycles"),
    do:
      "Select 0.1.20 to admit module dependency cycles as strongly-connected components: intra-component imports resolve against declared signatures without digests, cross-component imports stay digest-bound, and each component checks, caches, and digests as one unit."

  defp migration(_id), do: "Select the introducing revision to adopt this stable feature."

  defp fixes("editions-and-feature-lifecycle") do
    [
      %{
        "kind" => "json-edit",
        "operation" => "replace",
        "path" => "$.version",
        "value" => "0.1.7",
        "applicability" => "machine-applicable"
      },
      %{
        "kind" => "json-edit",
        "operation" => "add",
        "path" => "$.edition",
        "value" => "0.1",
        "applicability" => "machine-applicable"
      },
      %{
        "kind" => "json-edit",
        "operation" => "add",
        "path" => "$.language_revision",
        "value" => "0.1.7",
        "applicability" => "machine-applicable"
      },
      %{
        "kind" => "json-edit",
        "operation" => "add",
        "path" => "$.previews",
        "value" => [],
        "applicability" => "machine-applicable"
      }
    ]
  end

  defp fixes(_id), do: []

  defp specification(path), do: @specification_base <> "/" <> path

  defp change_id(revision, feature_id),
    do: "change-" <> String.replace(revision, ".", "-") <> "-" <> feature_id

  defp error(message, path),
    do: {:error, Diagnostic.new("PRV001", message, path: path)}
end
