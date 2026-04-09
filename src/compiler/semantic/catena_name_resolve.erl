%%%-------------------------------------------------------------------
%%% @doc Catena Name Resolution Module
%%%
%%% This module handles name resolution for Catena programs, including
%%% module imports, qualified names, and shadowing rules. It constructs
%%% symbol tables from module declarations and resolves identifiers to their
%%% definitions following Catena's scoping rules.
%%%
%%% == Name Resolution Rules ==
%%%
%%% <ul>
%%%   <li><b>Local definitions shadow imports</b> - Definitions in the current
%%%       module take precedence over imported names.</li>
%%%   <li><b>Imports shadow prelude</b> - Imported names shadow prelude definitions.</li>
%%%   <li><b>Qualified names bypass shadowing</b> - Names like `Module.name` always
%%%       refer to the exported definition from `Module`, regardless of shadowing.</li>
%%%   <li><b>Later imports shadow earlier imports</b> - When multiple imports define
%%%       the same name, the later import wins (for unqualified references).</li>
%%% </ul>
%%%
%%% == Symbol Table Structure ==
%%%
%%% ```
%%% #{
%%%   module => #{
%%%     exports => [Name1, Name2, ...],
%%%     imports => [#import{module = ..., items = ..., ...}, ...],
%%%     definitions => #{Name1 => Definition1, Name2 => Definition2, ...}
%%%   }
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_name_resolve).

%% API
-export([
    resolve_module/2,
    resolve_name/3,
    build_symbol_table/1,
    qualify_name/2,
    is_qualified/1,
    split_qualified/1,
    format_name_error/2
]).

%% Types
-type location() :: {integer(), integer()}.
-type symbol_table() :: #{
    module => module_info()
}.
-type module_info() :: #{
    exports => [atom()],
    imports => [import()],
    definitions => #{atom() => definition()}
}.
-type definition() :: #{
    type => local | exported | imported,
    module => atom(),
    location => location()
}.
-type import() :: #{
    module => atom(),
    items => all | [atom()],
    qualified => boolean(),
    alias => atom() | undefined,
    location => location()
}.

%%%=============================================================================
%%% Symbol Table Construction
%%%=============================================================================

%% @doc Build a symbol table from a parsed module AST.
%% @param ModuleAST The parsed module AST from catena_parser
%% @return A symbol table map with module information
-spec build_symbol_table(tuple()) -> symbol_table().
build_symbol_table({module, Name, Exports, Imports, Declarations, _Location}) ->
    %% Build export list - parser returns list of export items directly
    ExportList = extract_exports(Exports),

    %% Build definitions map
    Definitions = build_definitions(Declarations, Name),

    %% Create symbol table entry for this module
    #{
        Name => #{
            exports => ExportList,
            imports => Imports,
            definitions => Definitions
        }
    }.

%% @doc Extract export list from export items.
%% Parser returns list of {export_type, Name} tuples directly.
-spec extract_exports(term()) -> [atom()].
extract_exports(Exports) when is_list(Exports) ->
    [extract_export_name(Item) || Item <- Exports];
extract_exports(_) ->
    [].

%% @doc Extract export items from export list
extract_export_items({export_list, Items}) ->
    [extract_export_name(Item) || Item <- Items].

%% @doc Extract name from export item
extract_export_name({export_trait, Name}) -> Name;
extract_export_name({export_type, Name}) -> Name;
extract_export_name({export_transform, Name}) -> Name;
extract_export_name({export_effect, Name}) -> Name;
extract_export_name(Name) when is_atom(Name) -> Name.

%% @doc Build definitions map from declarations
-spec build_definitions([term()], atom()) -> #{atom() => definition()}.
build_definitions(Declarations, ModuleName) ->
    lists:foldl(fun(Decl, Acc) ->
        extract_definitions(Decl, ModuleName, Acc)
    end, #{}, Declarations).

%% @doc Extract definitions from a declaration
extract_definitions(Decl, ModuleName, Acc) ->
    case Decl of
        {type_decl, Name, _Params, _Constructors, Location} ->
            Acc#{Name => #{type => exported, module => ModuleName, location => Location}};
        {transform_decl, Name, _Type, _Clauses, Location} ->
            Acc#{Name => #{type => exported, module => ModuleName, location => Location}};
        {trait_decl, Name, Location} ->
            Acc#{Name => #{type => exported, module => ModuleName, location => Location}};
        {instance_decl, _, _, _} ->
            Acc;
        _ ->
            Acc
    end.

%% @doc Extract definitions marked with export keyword
extract_definition_exports(Declarations) ->
    lists:usort(lists:flatmap(fun(Decl) ->
        case Decl of
            {type_decl, Name, _, _, _} -> [Name];
            {transform_decl, Name, _, _, _} -> [Name];
            {trait_decl, Name, _} -> [Name];
            _ -> []
        end
    end, Declarations)).

%%%=============================================================================
%%% Name Resolution
%%%=============================================================================

%% @doc Resolve a module with all its dependencies and return symbol table.
%% @param ModuleName The name of the module to resolve
%% @param SymbolTables The symbol tables of all available modules
%% @return {ok, ResolvedTable} | {error, Reason}
-spec resolve_module(atom(), symbol_table()) -> {ok, symbol_table()} | {error, term()}.
resolve_module(ModuleName, SymbolTables) ->
    case maps:get(ModuleName, SymbolTables, undefined) of
        undefined ->
            {error, {module_not_found, ModuleName}};
        #{imports := Imports, definitions := Definitions} = ModuleInfo ->
            %% Process imports and build combined symbol table
            ResolvedDefinitions = resolve_imports(Imports, SymbolTables, Definitions),

            {ok, #{
                ModuleName => #{
                    exports => maps:get(exports, ModuleInfo, []),
                    imports => [],
                    definitions => ResolvedDefinitions
                }
            }}
    end.

%% @doc Resolve imports from the import list, combining definitions from
%% all imported modules into a single definitions map.
-spec resolve_imports([import()], symbol_table(), #{atom() => definition()}) ->
                      #{atom() => definition()}.
resolve_imports(Imports, SymbolTables, LocalDefinitions) ->
    lists:foldl(fun(Import, Acc) ->
        resolve_single_import(Import, SymbolTables, LocalDefinitions, Acc)
    end, LocalDefinitions, Imports).

%% @doc Resolve a single import, merging its exported definitions into the
%% accumulator map. Handles qualified imports, selective imports, and aliasing.
-spec resolve_single_import(import(), symbol_table(), #{atom() => definition()}, #{atom() => definition()}) ->
                      #{atom() => definition()}.
resolve_single_import(#{module := ModuleName, items := Items, qualified := Qualified, alias := Alias},
                       SymbolTables, _LocalDefinitions, Acc) ->
    case maps:get(ModuleName, SymbolTables, undefined) of
        undefined ->
            %% Module not found in symbol tables
            Acc;
        #{exports := Exports, definitions := Definitions} ->
            %% Determine which items to import
            ItemsToImport = case Items of
                all -> Exports;
                ItemList -> ItemList
            end,

            %% Filter to only exported items
            AvailableDefinitions = maps:filter(
                fun(Key, _Def) ->
                    lists:member(Key, ItemsToImport)
                end,
                Definitions
            ),

            %% Build imported definitions map
            if
                Items =:= all andalso Qualified =:= false ->
                    %% Unqualified import: add all with their original names
                    maps:fold(
                        fun(Name, Def, AccIn) ->
                            AccIn#{Name => Def}
                        end,
                        Acc,
                        AvailableDefinitions
                    );
                Items =:= all andalso Qualified andalso Alias =:= undefined ->
                    %% Qualified import without alias: not useful, skip
                    Acc;
                Items =:= all andalso Qualified andalso Alias =/= undefined ->
                    %% Qualified import with alias: create alias entries
                    maps:fold(
                        fun(Name, Def, AccIn) ->
                            AliasedName = list_to_atom(atom_to_list(Alias) ++ "." ++ atom_to_list(Name)),
                            AccIn#{AliasedName => Def#{original_module => ModuleName}}
                        end,
                        Acc,
                        AvailableDefinitions
                    );
                is_list(Items) andalso Qualified =:= false ->
                    %% Selective unqualified import: add specific items
                    maps:fold(
                        fun(Name, Def, AccIn) ->
                            AccIn#{Name => Def}
                        end,
                        Acc,
                        AvailableDefinitions
                    );
                is_list(Items) andalso Qualified andalso Alias =/= undefined ->
                    %% Qualified selective import: add with alias prefix
                    maps:fold(
                        fun(Name, Def, AccIn) ->
                            AliasedName = list_to_atom(atom_to_list(Alias) ++ "." ++ atom_to_list(Name)),
                            AccIn#{AliasedName => Def#{original_module => ModuleName}}
                        end,
                        Acc,
                        AvailableDefinitions
                    );
                true ->
                    Acc
            end
    end.

%% @doc Resolve a name in the context of a specific module.
%% Returns the definition if found, or an error if not found.
-spec resolve_name(atom(), atom(), symbol_table()) ->
    {ok, definition()} | {error, term()}.
resolve_name(Name, CurrentModule, SymbolTables) ->
    case maps:get(CurrentModule, SymbolTables, undefined) of
        undefined ->
            {error, {module_not_loaded, CurrentModule}};
        #{definitions := Definitions} ->
            %% Check local definitions first (highest priority)
            case maps:find(Name, Definitions) of
                {ok, Definition} ->
                    {ok, Definition};
                error ->
                    %% Not found locally, check prelude
                    resolve_from_prelude(Name, SymbolTables)
            end
    end.

%% @doc Resolve a name by searching through prelude.
-spec resolve_from_prelude(atom(), symbol_table()) ->
    {ok, definition()} | {error, term()}.
resolve_from_prelude(Name, SymbolTables) ->
    case maps:get(prelude, SymbolTables, undefined) of
        undefined ->
            {error, {name_not_found, Name}};
        #{definitions := Definitions} ->
            case maps:find(Name, Definitions) of
                {ok, Definition} ->
                    {ok, Definition};
                error ->
                    {error, {name_not_found, Name}}
            end
    end.

%%%=============================================================================
%%% Qualified Name Utilities
%%%=============================================================================

%% @doc Check if a name is qualified (contains a dot).
-spec is_qualified(atom() | string()) -> boolean().
is_qualified(Name) when is_atom(Name) ->
    is_qualified(atom_to_list(Name));
is_qualified(Name) when is_list(Name) ->
    string:find(Name, ".") =/= nomatch.

%% @doc Qualify a name with a module prefix.
%% @param ModuleName The module to use as prefix
%% @param Name The name to qualify
%% @return A qualified atom like 'Module.Name'
-spec qualify_name(atom(), atom() | string()) -> atom().
qualify_name(ModuleName, Name) when is_atom(Name) ->
    qualify_name(ModuleName, atom_to_list(Name));
qualify_name(ModuleName, Name) when is_list(Name) ->
    list_to_atom(atom_to_list(ModuleName) ++ "." ++ Name).

%% @doc Split a qualified name into {Module, Name} parts.
%% @param QualifiedName The qualified name (e.g., 'Data.List.map')
%% @return {ModuleName, Name}
-spec split_qualified(atom() | string()) -> {atom(), atom()}.
split_qualified(QualifiedName) when is_atom(QualifiedName) ->
    split_qualified(atom_to_list(QualifiedName));
split_qualified(QualifiedName) when is_list(QualifiedName) ->
    Parts = string:split(QualifiedName, ".", all),
    case length(Parts) of
        2 ->
            [Module, Name] = Parts,
            {list_to_atom(Module), list_to_atom(Name)};
        _ when length(Parts) > 2 ->
            ModulePart = string:join(lists:sublist(Parts, length(Parts) - 1), "."),
            NamePart = lists:last(Parts),
            {list_to_atom(ModulePart), list_to_atom(NamePart)};
        _ ->
            {undefined, list_to_atom(QualifiedName)}
    end.

%%%=============================================================================
%%% Error Formatting
%%%=============================================================================

%% @doc Format a name error with helpful suggestions.
-spec format_name_error({error, term()}, atom() | string()) -> iolist().
format_name_error({error, {name_not_found, Name}}, _Context) ->
    ["Name not found: ", atom_to_list(Name), "\n"];
format_name_error({error, {module_not_loaded, ModuleName}}, _Context) ->
    ["Module not loaded: ", atom_to_list(ModuleName), "\n"];
format_name_error({error, {module_not_found, ModuleName}}, _Context) ->
    ["Module not found: ", atom_to_list(ModuleName), "\n"];
format_name_error({error, Reason}, _Context) ->
    ["Error: ", io_lib:format("~p", [Reason]), "\n"].

%%%=============================================================================
%%% Location Extraction Helpers
%%%=============================================================================

%% @doc Extract location from various AST nodes.
-spec extract_location(term()) -> location().
extract_location({import, _, _, _, _, Location}) -> Location;
extract_location({type_decl, _, _, _, _, Location}) -> Location;
extract_location({transform_decl, _, _, _, _, Location}) -> Location;
extract_location({trait_decl, _, _, Location}) -> Location;
extract_location({instance_decl, _, _, _, Location}) -> Location;
extract_location({export_decl, _}) -> {1, 0};
extract_location({module_decl, _, _, Location}) -> Location;
extract_location({location, Line, Col}) -> {Line, Col};
extract_location(_) -> {1, 0}.
