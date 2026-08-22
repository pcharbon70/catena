defmodule Catena.Namespace do
  @moduledoc """
  The source-only Catena 0.1.17 namespace and shadowing resolver.

  An ordered scope-event stream (declarations, scope boundaries, import
  sets) is resolved into one environment, and references resolve to
  nominal identities or exactly one stable diagnostic. It does not parse
  source, tokenize, check types, evaluate, or compile.
  """

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion}

  @namespace_revision "0.1.17"

  defmodule Environment do
    @moduledoc "One resolved scope-event stream."

    @enforce_keys [:scopes, :imports]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            scopes: [%{optional({atom(), String.t()}) => String.t()}],
            imports: %{{atom(), String.t()} => [String.t()]}
          }
  end

  defmodule Resolution do
    @moduledoc "One resolved reference."

    @enforce_keys [:category, :spelling, :origin, :scope_depth]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            category: atom(),
            spelling: String.t(),
            origin: String.t() | nil,
            scope_depth: non_neg_integer()
          }
  end

  @value_class ~w(values fields operations typevars)a
  @capitalized_class ~w(types constructors traits effects handlers entries modules)a
  @program_categories @value_class ++ @capitalized_class
  @governed_categories ~w(governed)a

  @scope_events ~w(open_scope close_scope)a
  @import_event :import_set

  @spec build_environment([map() | atom()], keyword()) ::
          {:ok, Environment.t()} | {:error, Diagnostic.t()}
  def build_environment(events, options \\ [])
      when is_list(events) and is_list(options) do
    with {:ok, _selection} <- resolve_selection(Keyword.get(options, :language_selection)) do
      build(events, [%{}], %{}, 0)
    end
  end

  @spec resolve(Environment.t(), map()) ::
          {:ok, Resolution.t()} | {:error, Diagnostic.t()}
  def resolve(%Environment{} = env, reference) when is_map(reference) do
    category = Map.fetch!(reference, :category)
    spelling = Map.fetch!(reference, :spelling)
    qualified = Map.get(reference, :qualified, false)

    with :ok <- require_program_category(category),
         {:ok, spelling, module} <- check_qualification(spelling, qualified, reference) do
      resolve_spelling(env, category, spelling, module, reference)
    end
  end

  defp resolve_selection(nil),
    do: require_namespace_revision(LanguageVersion.legacy_selection(@namespace_revision))

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_namespace_revision(resolved)
    end
  end

  defp require_namespace_revision(
         %LanguageSelection{language_revision: @namespace_revision} = selection
       ),
       do: {:ok, selection}

  defp require_namespace_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "namespace resolution requires language revision #{@namespace_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @namespace_revision,
         frontend: "namespaces-and-shadowing"
       }
     )}
  end

  defp build([], scopes, imports, _depth),
    do: {:ok, %Environment{scopes: scopes, imports: imports}}

  defp build([event | rest], scopes, imports, depth) when event in @scope_events do
    case event do
      :open_scope ->
        build(rest, [%{} | scopes], imports, depth + 1)

      :close_scope ->
        case scopes do
          [_current | outer] when outer != [] ->
            build(rest, outer, imports, depth - 1)

          [_only] ->
            {:error,
             Diagnostic.new("NSP001", "a close_scope event has no matching open_scope",
               details: %{reason: "unbalanced_scope_events"}
             )}

          [] ->
            {:error,
             Diagnostic.new("NSP001", "a close_scope event has no matching open_scope",
               details: %{reason: "unbalanced_scope_events"}
             )}
        end
    end
  end

  defp build([event | rest], scopes, imports, depth) when is_map(event) do
    case Map.get(event, :event) do
      :declare ->
        category = Map.fetch!(event, :category)
        spelling = Map.fetch!(event, :spelling)

        with :ok <- require_program_category(category),
             :ok <- check_spelling_class(category, spelling),
             :ok <- check_duplicate(scopes, category, spelling, Map.get(event, :span)) do
          [current | outer] = scopes
          build(rest, [Map.put(current, {category, spelling}, spelling) | outer], imports, depth)
        end

      @import_event ->
        origin = Map.fetch!(event, :origin)
        category = Map.fetch!(event, :category)
        names = Map.fetch!(event, :names)

        imports =
          Map.update(imports, {category, origin}, Enum.uniq(names), fn existing ->
            Enum.uniq(existing ++ names)
          end)

        build(rest, scopes, imports, depth)

      nil ->
        {:error,
         Diagnostic.new("NSP001", "a scope event must carry an :event key",
           details: %{reason: "invalid_event", event: inspect(event)}
         )}

      other when other in @scope_events ->
        build([other | rest], scopes, imports, depth)

      other ->
        {:error,
         Diagnostic.new("NSP001", "unknown scope event",
           details: %{reason: "invalid_event", event: inspect(other)}
         )}
    end
  end

  defp build([event | _], _scopes, _imports, _depth) do
    {:error,
     Diagnostic.new("NSP001", "a scope event must be a map",
       details: %{reason: "invalid_event", event: inspect(event)}
     )}
  end

  defp check_qualification(spelling, true = _qualified, reference) do
    case String.split(spelling, ".") do
      [module, member] ->
        {:ok, member, module}

      _ ->
        {:error,
         Diagnostic.new(
           "NSP005",
           "a qualified reference is exactly two segments",
           span: Map.get(reference, :span),
           details: %{reason: "invalid_qualification_depth", spelling: spelling}
         )}
    end
  end

  defp check_qualification(spelling, false, _reference), do: {:ok, spelling, nil}

  defp resolve_spelling(%Environment{} = env, category, spelling, module, reference) do
    scope_hit = find_in_scopes(env.scopes, category, spelling)

    cond do
      scope_hit != nil and module == nil ->
        {:ok,
         %Resolution{
           category: category,
           spelling: spelling,
           origin: nil,
           scope_depth: scope_hit
         }}

      scope_hit == nil and module != nil ->
        resolve_qualified(env, category, spelling, module, reference)

      module != nil ->
        resolve_qualified(env, category, spelling, module, reference)

      true ->
        resolve_imports(env, category, spelling, reference)
    end
  end

  defp resolve_qualified(env, category, spelling, module, reference) do
    module_declared? =
      env.scopes != [] and Map.has_key?(List.last(env.scopes), {:modules, module})

    module_imported? =
      env.imports
      |> Enum.any?(fn {{cat, origin}, _names} -> cat == :modules and origin == module end)

    if module_declared? or module_imported? do
      exported =
        env.imports
        |> Enum.filter(fn {{_cat, origin}, _names} -> origin == module end)
        |> Enum.flat_map(fn {_key, names} -> names end)

      if spelling in exported do
        {:ok,
         %Resolution{
           category: category,
           spelling: spelling,
           origin: module,
           scope_depth: 0
         }}
      else
        {:error, unbound(reference, category, spelling)}
      end
    else
      {:error, unbound(reference, category, spelling)}
    end
  end

  defp resolve_imports(%Environment{} = env, category, spelling, reference) do
    origins =
      env.imports
      |> Enum.filter(fn {{cat, _origin}, names} -> cat == category and spelling in names end)
      |> Enum.map(fn {{_cat, origin}, _names} -> origin end)
      |> Enum.sort()

    case {find_module_level(env, category, spelling), origins} do
      {nil, []} ->
        {:error, unbound(reference, category, spelling)}

      {nil, [origin]} ->
        {:ok,
         %Resolution{
           category: category,
           spelling: spelling,
           origin: origin,
           scope_depth: 0
         }}

      {nil, multiple} ->
        {:error,
         Diagnostic.new(
           "NSP004",
           "an unqualified reference is ambiguous across import origins",
           span: Map.get(reference, :span),
           details: %{
             reason: "ambiguous_import",
             spelling: spelling,
             category: category,
             origins: multiple
           }
         )}

      {_local, _} ->
        {:ok,
         %Resolution{
           category: category,
           spelling: spelling,
           origin: nil,
           scope_depth: 0
         }}
    end
  end

  defp find_in_scopes(scopes, category, spelling) do
    scopes
    |> Enum.with_index()
    |> Enum.find_value(fn {scope, index} ->
      if Map.has_key?(scope, {category, spelling}), do: length(scopes) - 1 - index
    end)
  end

  defp find_module_level(%Environment{scopes: scopes}, category, spelling) do
    if scopes != [] and Map.has_key?(List.last(scopes), {category, spelling}), do: :local
  end

  defp require_program_category(category) when category in @program_categories, do: :ok

  defp require_program_category(category) when category in @governed_categories,
    do:
      {:error,
       Diagnostic.new(
         "NSP001",
         "governed identities never participate in program-name resolution",
         details: %{reason: "governed_separation", category: category}
       )}

  defp require_program_category(category),
    do:
      {:error,
       Diagnostic.new("NSP001", "unknown namespace category",
         details: %{reason: "unknown_category", category: inspect(category)}
       )}

  defp check_spelling_class(category, spelling) do
    uppercase? =
      spelling != "" and String.upcase(String.first(spelling)) == String.first(spelling)

    expected_upper? = category in @capitalized_class

    if uppercase? == expected_upper? do
      :ok
    else
      {:error,
       Diagnostic.new(
         "NSP002",
         "a declaration's spelling violates its category's spelling class",
         details: %{
           reason: "spelling_class_violation",
           category: category,
           spelling: spelling
         }
       )}
    end
  end

  defp check_duplicate(scopes, category, spelling, span) do
    [current | _] = scopes

    if Map.has_key?(current, {category, spelling}) do
      {:error,
       Diagnostic.new(
         "NSP001",
         "a duplicate declaration of one spelling in one category within one scope",
         span: span,
         details: %{reason: "duplicate_declaration", category: category, spelling: spelling}
       )}
    else
      :ok
    end
  end

  defp unbound(reference, category, spelling) do
    Diagnostic.new("NSP003", "a reference has no binding and no import in scope",
      span: Map.get(reference, :span),
      details: %{reason: "unbound_reference", category: category, spelling: spelling}
    )
  end
end
