%%%-------------------------------------------------------------------
%%% @doc Executable imported-symbol resolution.
%%%
%%% The resolution inventory is built from versioned module interfaces before
%%% Core Erlang generation.  Qualified bindings use structured keys during
%%% typing and explicit imported-reference nodes during lowering; module and
%%% function names are never reconstructed from concatenated strings.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_import_resolution).

-export([
    empty/1,
    build/4,
    is_resolution/1,
    entries/1,
    resolve_transform/4,
    resolve_constructor/4,
    resolve_value/3,
    resolve_qualified/4,
    rewrite_module/2,
    type_binding/1
]).

-define(RESOLUTION_VERSION, 1).

-type resolution() :: map().
-export_type([resolution/0]).

-spec empty(atom()) -> resolution().
empty(Module) ->
    #{
        '$catena_import_resolution' => ?RESOLUTION_VERSION,
        module => Module,
        entries => [],
        unqualified => #{},
        qualified => #{},
        qualifiers => [],
        resolved_modules => []
    }.

-spec build(atom(), [term()], #{atom() => map()}, [atom()]) ->
    {ok, resolution()} | {error, term()}.
build(Module, Imports, Interfaces, LocalNames)
  when is_atom(Module), is_list(Imports), is_map(Interfaces),
       is_list(LocalNames) ->
    LocalSet = sets:from_list(LocalNames),
    lists:foldl(
        fun
            (_Import, {error, _} = Error) ->
                Error;
            (Import, {ok, Resolution}) ->
                add_import(Import, Interfaces, LocalSet, Resolution)
        end,
        {ok, empty(Module)},
        Imports
    ).

-spec is_resolution(term()) -> boolean().
is_resolution(#{
    '$catena_import_resolution' := ?RESOLUTION_VERSION,
    module := Module,
    entries := Entries,
    unqualified := Unqualified,
    qualified := Qualified
}) ->
    is_atom(Module) andalso
        is_list(Entries) andalso
        is_map(Unqualified) andalso
        is_map(Qualified);
is_resolution(_) ->
    false.

-spec entries(resolution()) -> [map()].
entries(Resolution) ->
    maps:get(entries, Resolution).

-spec resolve_transform(atom(), non_neg_integer(), resolution(), map()) ->
    {ok, map()} | {error, term()}.
resolve_transform(Name, Arity, Resolution, Context) ->
    resolve_kind(transform, Name, Arity, Resolution, Context).

-spec resolve_constructor(atom(), non_neg_integer(), resolution(), map()) ->
    {ok, map()} | {error, term()}.
resolve_constructor(Name, Arity, Resolution, Context) ->
    resolve_kind(constructor, Name, Arity, Resolution, Context).

-spec resolve_value(atom(), resolution(), map()) ->
    {ok, map()} | {error, term()}.
resolve_value(Name, Resolution, Context) ->
    case [
        Entry
        || Entry <- maps:get(
            Name,
            maps:get(unqualified, Resolution),
            []
        ),
           lists:member(
               maps:get(kind, Entry),
               [transform, constructor]
           )
    ] of
        [Entry] ->
            {ok, Entry};
        [] ->
            {error, catena_backend_error:unresolved_call(
                Name,
                0,
                Context#{usage => imported_callable_value}
            )};
        Candidates ->
            {error, catena_backend_error:ambiguous_call(
                Name,
                0,
                Candidates,
                Context#{usage => imported_callable_value}
            )}
    end.

-spec resolve_qualified(atom(), atom(), non_neg_integer() | value,
    resolution()) -> {ok, map()} | {error, term()}.
resolve_qualified(Prefix, Name, ExpectedArity, Resolution) ->
    case maps:get({Prefix, Name}, maps:get(qualified, Resolution), []) of
        [Entry] ->
            check_arity(Name, ExpectedArity, Entry, #{
                qualifier => Prefix,
                source_module => maps:get(source_module, Entry)
            });
        [] ->
            {error, {
                unresolved_qualified_import,
                Prefix,
                Name,
                ExpectedArity
            }};
        Candidates ->
            {error, {
                ambiguous_qualified_import,
                Prefix,
                Name,
                Candidates
            }}
    end.

-spec rewrite_module(term(), resolution()) ->
    {ok, term()} | {error, term()}.
rewrite_module(
    {module, Name, Exports, Imports, Declarations, Location},
    Resolution
) ->
    try
        {ok, {
            module,
            Name,
            Exports,
            Imports,
            rewrite_term(Declarations, Resolution),
            Location
        }}
    catch
        throw:{import_resolution_error, Reason} ->
            {error, Reason}
    end;
rewrite_module(Other, _Resolution) ->
    {error, {invalid_module_ast, Other}}.

-spec type_binding(map()) -> atom() | tuple().
type_binding(#{binding := Binding}) ->
    Binding;
type_binding(#{name := Name}) ->
    Name.

add_import(
    {import, ImportedModule, Items, Qualified, Alias, Location},
    Interfaces,
    LocalSet,
    Resolution
) ->
    case maps:get(ImportedModule, Interfaces, undefined) of
        undefined ->
            {error, #{
                reason => missing_import_interface,
                module => maps:get(module, Resolution),
                imported_module => ImportedModule,
                location => Location
            }};
        Interface ->
            case catena_module_interface:is_interface(Interface) of
                false ->
                    {error, {
                        invalid_import_interface,
                        ImportedModule,
                        Interface
                    }};
                true ->
                    select_and_add(
                        ImportedModule,
                        Items,
                        Qualified,
                        Alias,
                        Location,
                        Interface,
                        LocalSet,
                        Resolution
                    )
            end
    end;
add_import(Other, _Interfaces, _LocalSet, _Resolution) ->
    {error, {invalid_import_declaration, Other}}.

select_and_add(
    ImportedModule,
    Items,
    Qualified,
    Alias,
    Location,
    Interface,
    LocalSet,
    Resolution
) ->
    Available = catena_module_interface:exported_symbols(Interface),
    case select_symbols(Items, Available) of
        {ok, Selected} ->
            Prefix = qualifier(ImportedModule, Alias),
            add_selected(
                Selected,
                Qualified,
                Prefix,
                Location,
                LocalSet,
                Resolution
            );
        {error, Missing} ->
            {error, #{
                reason => symbol_not_exported,
                imported_module => ImportedModule,
                symbols => Missing,
                location => Location
            }}
    end.

select_symbols(all, Available) ->
    {ok, Available};
select_symbols(Items, Available) when is_list(Items) ->
    AvailableNames = sets:from_list([
        maps:get(name, Entry)
        || Entry <- Available
    ]),
    Missing = [
        Name
        || Name <- Items,
           not sets:is_element(Name, AvailableNames)
    ],
    case Missing of
        [] ->
            {ok, [
                Entry
                || Entry <- Available,
                   lists:member(maps:get(name, Entry), Items) orelse
                       (
                           maps:get(kind, Entry) =:= trait_method andalso
                           lists:member(
                               maps:get(owner, Entry),
                               Items
                           )
                       )
            ]};
        _ ->
            {error, Missing}
    end.

qualifier(ImportedModule, undefined) ->
    ImportedModule;
qualifier(_ImportedModule, Alias) ->
    Alias.

add_selected(Selected, true, Prefix, Location, _LocalSet, Resolution) ->
    Qualified0 = maps:get(qualified, Resolution),
    case add_qualified_entries(Selected, Prefix, Location, Qualified0) of
        {ok, Qualified1} ->
            Entries = [
                imported_entry(Entry, {qualified, Prefix}, Location)
                || Entry <- Selected
            ],
            {ok, Resolution#{
                entries := maps:get(entries, Resolution) ++ Entries,
                qualified := Qualified1,
                resolved_modules := lists:usort(
                    [
                        maps:get(source_module, Entry)
                        || Entry <- Entries
                    ] ++ maps:get(resolved_modules, Resolution)
                ),
                qualifiers := lists:usort(
                    [Prefix | maps:get(qualifiers, Resolution)]
                )
            }};
        {error, _} = Error ->
            Error
    end;
add_selected(Selected, false, _Prefix, Location, LocalSet, Resolution) ->
    Entries = [
        imported_entry(Entry, unqualified, Location)
        || Entry <- Selected
    ],
    Unqualified0 = maps:get(unqualified, Resolution),
    Unqualified1 = lists:foldl(
        fun(Entry, Index) ->
            Name = maps:get(name, Entry),
            case sets:is_element(Name, LocalSet) of
                true ->
                    Index;
                false ->
                    maps:update_with(
                        Name,
                        fun(Existing) -> Existing ++ [Entry] end,
                        [Entry],
                        Index
                    )
            end
        end,
        Unqualified0,
        Entries
    ),
    {ok, Resolution#{
        entries := maps:get(entries, Resolution) ++ Entries,
        unqualified := Unqualified1,
        resolved_modules := lists:usort(
            [maps:get(source_module, Entry) || Entry <- Entries] ++
                maps:get(resolved_modules, Resolution)
        )
    }}.

add_qualified_entries([], _Prefix, _Location, Index) ->
    {ok, Index};
add_qualified_entries([Symbol | Rest], Prefix, Location, Index) ->
    Entry = imported_entry(Symbol, {qualified, Prefix}, Location),
    Key = {Prefix, maps:get(name, Entry)},
    case maps:get(Key, Index, []) of
        [] ->
            add_qualified_entries(
                Rest,
                Prefix,
                Location,
                Index#{Key => [Entry]}
            );
        Existing ->
            {error, {
                ambiguous_import_qualifier,
                Prefix,
                maps:get(name, Entry),
                Existing ++ [Entry]
            }}
    end.

imported_entry(Symbol, Qualification, ImportLocation) ->
    Name = maps:get(name, Symbol),
    Binding = case Qualification of
        unqualified ->
            Name;
        {qualified, Prefix} ->
            {catena_import, Prefix, Name}
    end,
    Symbol#{
        imported => true,
        qualification => Qualification,
        binding => Binding,
        import_location => ImportLocation
    }.

resolve_kind(Kind, Name, Arity, Resolution, Context) ->
    Candidates = [
        Entry
        || Entry <- maps:get(Name, maps:get(unqualified, Resolution), []),
           maps:get(kind, Entry) =:= Kind
    ],
    case Candidates of
        [Entry] ->
            check_arity(Name, Arity, Entry, Context);
        [] ->
            {error, catena_backend_error:unresolved_call(
                Name,
                Arity,
                Context#{callable_kind => Kind, scope => imports}
            )};
        _ ->
            {error, catena_backend_error:ambiguous_call(
                Name,
                Arity,
                Candidates,
                Context#{callable_kind => Kind, scope => imports}
            )}
    end.

check_arity(_Name, value, Entry, _Context) ->
    {ok, Entry};
check_arity(Name, Arity, Entry, Context) ->
    Expected = maps:get(arity, Entry),
    case Expected =:= Arity of
        true ->
            {ok, Entry};
        false ->
            {error, catena_backend_error:arity_mismatch(
                Name,
                Expected,
                Arity,
                Context#{
                    callable_kind => maps:get(kind, Entry),
                    declaration_location => maps:get(location, Entry),
                    declaration_identity => {
                        maps:get(source_module, Entry),
                        Name,
                        Expected
                    }
                }
            )}
    end.

rewrite_term(
    {record_access, {var, Prefix, _}, Name, Location} = Access,
    Resolution
) ->
    case maps:get({Prefix, Name}, maps:get(qualified, Resolution), []) of
        [Entry] ->
            {imported_ref, Entry, Location};
        [] ->
            case lists:member(Prefix, maps:get(qualifiers, Resolution)) of
                true ->
                    throw({import_resolution_error, {
                        unresolved_qualified_import,
                        Prefix,
                        Name,
                        Location
                    }});
                false ->
                    rewrite_tuple(Access, Resolution)
            end;
        Candidates ->
            throw({import_resolution_error, {
                ambiguous_qualified_import,
                Prefix,
                Name,
                Candidates,
                Location
            }})
    end;
rewrite_term(Term, Resolution) when is_tuple(Term) ->
    rewrite_tuple(Term, Resolution);
rewrite_term(Terms, Resolution) when is_list(Terms) ->
    [rewrite_term(Term, Resolution) || Term <- Terms];
rewrite_term(Term, _Resolution) ->
    Term.

rewrite_tuple(Term, Resolution) ->
    list_to_tuple([
        rewrite_term(Element, Resolution)
        || Element <- tuple_to_list(Term)
    ]).
