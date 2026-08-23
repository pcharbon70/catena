defmodule Catena.Namespace do
  @moduledoc """
  The source-only Catena 0.1.17 namespace resolver extended at 0.1.18
  with imports and exports.

  An ordered scope-event stream (declarations, scope boundaries, import
  sets, and at 0.1.18 export, provided-module, and import-module events)
  is resolved into one environment, and references resolve to nominal
  identities or exactly one stable diagnostic. Unused-import analysis
  returns deny-able `IMP001` warnings only. It does not parse source,
  tokenize, check types, evaluate, or compile.
  """

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion}

  @namespace_revision "0.1.18"

  defmodule Environment do
    @moduledoc "One resolved scope-event stream."

    @enforce_keys [:scopes, :imports, :exports, :provided, :module_imports]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            scopes: [%{optional({atom(), String.t()}) => String.t()}],
            imports: %{{atom(), String.t()} => [String.t()]},
            exports: [{atom(), String.t(), atom() | nil}],
            provided: %{optional(String.t()) => %{digest: String.t(), exports: MapSet.t()}},
            module_imports: [map()]
          }
  end

  defmodule ImportWarning do
    @moduledoc """
    One deny-able IMP001 unused-import warning: either one admitted
    unqualified name never referenced, or one imported module with no
    qualified or unqualified use.
    """

    @enforce_keys [:module, :kind, :category, :spelling]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            module: String.t(),
            kind: :unused_name | :unused_module,
            category: atom() | nil,
            spelling: String.t() | nil
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
      build(events, [%{}], %{}, [], %{}, [], 0)
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

  @doc """
  Analyzes unused imports over a built environment and a reference set.

  Returns deny-able IMP001-equivalent warnings only — never errors and
  never resolutions. Each reference is a map with :category, :spelling,
  and :qualified (the qualifier module or nil).
  """
  @spec check_unused_imports(Environment.t(), [map()]) :: {:ok, [ImportWarning.t()]}
  def check_unused_imports(%Environment{} = env, references) when is_list(references) do
    referenced_unqualified =
      MapSet.new(
        Enum.filter(references, &is_nil(Map.get(&1, :qualified))),
        fn ref -> {Map.fetch!(ref, :category), Map.fetch!(ref, :spelling)} end
      )

    referenced_modules = MapSet.new(references, fn ref -> Map.get(ref, :qualified) end)

    warnings =
      env.module_imports
      |> Enum.reverse()
      |> Enum.flat_map(fn admission ->
        module = admission.module

        used_names =
          Enum.filter(admission.admitted, fn {category, spelling} ->
            MapSet.member?(referenced_unqualified, {category, spelling})
          end)

        name_warnings =
          Enum.filter(admission.admitted, fn {category, spelling} ->
            not MapSet.member?(referenced_unqualified, {category, spelling})
          end)

        unused_module? =
          not MapSet.member?(referenced_modules, module) and used_names == []

        module_warning =
          if unused_module?,
            do: [
              %ImportWarning{module: module, kind: :unused_module, category: nil, spelling: nil}
            ],
            else: []

        Enum.map(name_warnings, fn {category, spelling} ->
          %ImportWarning{
            module: module,
            kind: :unused_name,
            category: category,
            spelling: spelling
          }
        end) ++ module_warning
      end)

    {:ok, warnings}
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

  defp build([], scopes, imports, exports, provided, module_imports, _depth),
    do:
      {:ok,
       %Environment{
         scopes: scopes,
         imports: imports,
         exports: exports,
         provided: provided,
         module_imports: module_imports
       }}

  defp build([event | rest], scopes, imports, exports, provided, module_imports, depth)
       when event in @scope_events do
    case event do
      :open_scope ->
        build(rest, [%{} | scopes], imports, exports, provided, module_imports, depth + 1)

      :close_scope ->
        case scopes do
          [_current | outer] when outer != [] ->
            build(rest, outer, imports, exports, provided, module_imports, depth - 1)

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

  defp build([event | rest], scopes, imports, exports, provided, module_imports, depth)
       when is_map(event) do
    case Map.get(event, :event) do
      :declare ->
        category = Map.fetch!(event, :category)
        spelling = Map.fetch!(event, :spelling)

        with :ok <- require_program_category(category),
             :ok <- check_spelling_class(category, spelling),
             :ok <- check_duplicate(scopes, category, spelling, Map.get(event, :span)) do
          [current | outer] = scopes

          build(
            rest,
            [Map.put(current, {category, spelling}, spelling) | outer],
            imports,
            exports,
            provided,
            module_imports,
            depth
          )
        end

      :export ->
        category = Map.fetch!(event, :category)
        spelling = Map.fetch!(event, :spelling)
        transparency = Map.get(event, :transparency)

        with :ok <- reject_extra_keys(event, [:event, :category, :spelling, :transparency]),
             :ok <- require_program_category(category),
             :ok <- check_spelling_class(category, spelling),
             :ok <- check_transparency(category, transparency),
             :ok <- check_exported_declaration(scopes, category, spelling),
             :ok <- check_duplicate_export(exports, category, spelling) do
          build(
            rest,
            scopes,
            imports,
            [{category, spelling, transparency} | exports],
            provided,
            module_imports,
            depth
          )
        end

      :provide_module ->
        module = Map.fetch!(event, :module)
        digest = Map.get(event, :digest, "")
        export_list = Map.get(event, :exports, [])

        with {:ok, export_set} <- normalize_provided_exports(export_list) do
          provided = Map.put(provided, module, %{digest: digest, exports: export_set})
          build(rest, scopes, imports, exports, provided, module_imports, depth)
        end

      :import_module ->
        module = Map.fetch!(event, :module)
        digest = Map.get(event, :digest, "")
        names = Map.get(event, :names, [])

        with :ok <- reject_extra_keys(event, [:event, :module, :digest, :names]),
             :ok <- require_list(names),
             :ok <- require_known_module(provided, module),
             {:ok, validated} <- validate_import_names(provided, module, names),
             :ok <- check_duplicate_module_import(module_imports, module, names) do
          imports =
            imports
            |> Map.update({:modules, module}, [module], &[module | &1])
            |> then(fn acc ->
              Enum.reduce(validated, acc, fn {category, spelling}, inner ->
                Map.update(inner, {category, module}, [spelling], &[spelling | &1])
              end)
            end)

          admission = %{
            module: module,
            digest: digest,
            admitted: validated
          }

          build(rest, scopes, imports, exports, provided, [admission | module_imports], depth)
        end

      @import_event ->
        origin = Map.fetch!(event, :origin)
        category = Map.fetch!(event, :category)
        names = Map.fetch!(event, :names)

        imports =
          Map.update(imports, {category, origin}, Enum.uniq(names), fn existing ->
            Enum.uniq(existing ++ names)
          end)

        build(rest, scopes, imports, exports, provided, module_imports, depth)

      nil ->
        {:error,
         Diagnostic.new("NSP001", "a scope event must carry an :event key",
           details: %{reason: "invalid_event", event: inspect(event)}
         )}

      other when other in @scope_events ->
        build([other | rest], scopes, imports, exports, provided, module_imports, depth)

      other ->
        {:error,
         Diagnostic.new("NSP001", "unknown scope event",
           details: %{reason: "invalid_event", event: inspect(other)}
         )}
    end
  end

  defp build([event | _], _scopes, _imports, _exports, _provided, _module_imports, _depth) do
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
      provided_exported =
        case env.provided do
          %{^module => %{exports: set}} ->
            [{category, spelling}] |> Enum.any?(&MapSet.member?(set, &1))

          _ ->
            false
        end

      legacy_match? =
        env.imports
        |> Enum.any?(fn {{cat, origin}, names} ->
          origin == module and cat == category and spelling in names
        end)

      if legacy_match? or (provided_exported and module_imported?) do
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

  defp check_transparency(:types, mode) when mode in [nil, :transparent, :abstract], do: :ok
  defp check_transparency(:types, _), do: {:error, bad_export("invalid_transparency_mode")}

  defp check_transparency(category, nil) when category != :types, do: :ok

  defp check_transparency(category, _) when category != :types,
    do: {:error, bad_export("transparency_only_on_types")}

  defp check_exported_declaration(scopes, category, spelling) do
    declared? = Enum.any?(scopes, fn scope -> Map.has_key?(scope, {category, spelling}) end)

    if declared? do
      :ok
    else
      {:error,
       Diagnostic.new(
         "EXP001",
         "an export declaration names a name the module does not declare in that category",
         details: %{reason: "undeclared_export", category: category, spelling: spelling}
       )}
    end
  end

  defp check_duplicate_export(exports, category, spelling) do
    if Enum.any?(exports, &match?({^category, ^spelling, _}, &1)) do
      {:error,
       Diagnostic.new(
         "NSP001",
         "a duplicate declaration of one spelling in one category within one scope",
         details: %{reason: "duplicate_declaration", category: category, spelling: spelling}
       )}
    else
      :ok
    end
  end

  defp normalize_provided_exports(export_list) do
    Enum.reduce_while(export_list, {:ok, MapSet.new()}, fn entry, {:ok, set} ->
      category = Map.fetch!(entry, :category)
      spelling = Map.fetch!(entry, :spelling)
      transparency = Map.get(entry, :transparency)

      with :ok <- require_program_category(category),
           :ok <- check_spelling_class(category, spelling),
           :ok <- check_transparency(category, transparency) do
        {:cont, {:ok, MapSet.put(set, {category, spelling})}}
      else
        {:error, %Diagnostic{}} = error -> {:halt, error}
      end
    end)
  end

  defp require_known_module(provided, module) do
    if Map.has_key?(provided, module) do
      :ok
    else
      {:error,
       Diagnostic.new(
         "IMP003",
         "an imported module is not known to the resolution context",
         details: %{reason: "unknown_module", module: module}
       )}
    end
  end

  defp validate_import_names(provided, module, names) do
    export_set = Map.fetch!(provided, module).exports

    Enum.reduce_while(names, {:ok, []}, fn {category, spelling}, {:ok, acc} ->
      with :ok <- require_program_category(category),
           :ok <- check_spelling_class(category, spelling),
           :ok <- require_exported(export_set, module, category, spelling) do
        {:cont, {:ok, [{category, spelling} | acc]}}
      else
        {:error, %Diagnostic{}} = error -> {:halt, error}
      end
    end)
  end

  defp require_exported(export_set, module, category, spelling) do
    if MapSet.member?(export_set, {category, spelling}) do
      :ok
    else
      {:error,
       Diagnostic.new(
         "IMP002",
         "a listed import name is absent from the module's exported set",
         details: %{
           reason: "unexported_import",
           module: module,
           category: category,
           spelling: spelling
         }
       )}
    end
  end

  defp check_duplicate_module_import(module_imports, module, names) do
    imported_before =
      Enum.flat_map(module_imports, fn admission ->
        if admission.module == module, do: admission.admitted, else: []
      end)

    duplicate =
      Enum.find(names, fn {category, spelling} -> {category, spelling} in imported_before end)

    case duplicate do
      nil ->
        :ok

      {category, spelling} ->
        {:error,
         Diagnostic.new(
           "NSP001",
           "a duplicate declaration of one spelling in one category within one scope",
           details: %{reason: "duplicate_declaration", category: category, spelling: spelling}
         )}
    end
  end

  defp reject_extra_keys(event, allowed) do
    if Map.keys(event) |> Enum.all?(&(&1 in allowed)) do
      :ok
    else
      {:error,
       Diagnostic.new("NSP001", "unknown scope event",
         details: %{reason: "invalid_event", event: inspect(event)}
       )}
    end
  end

  defp require_list(names) when is_list(names), do: :ok

  defp require_list(_names),
    do:
      {:error,
       Diagnostic.new("NSP001", "an import name list must be a list",
         details: %{reason: "invalid_event"}
       )}

  defp bad_export(reason) do
    Diagnostic.new("EXP001", "an export declaration is malformed", details: %{reason: reason})
  end

  defp unbound(reference, category, spelling) do
    Diagnostic.new("NSP003", "a reference has no binding and no import in scope",
      span: Map.get(reference, :span),
      details: %{reason: "unbound_reference", category: category, spelling: spelling}
    )
  end
end
