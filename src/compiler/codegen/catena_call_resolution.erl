%%%-------------------------------------------------------------------
%%% @doc Module-local callable inventory and resolution.
%%%
%%% The inventory is built from the complete normalized module before any
%%% declaration is lowered or erased.  It is therefore independent of source
%%% declaration order and retains the source identity needed for backend
%%% diagnostics.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_call_resolution).

-export([
    build/3,
    is_inventory/1,
    callables/1,
    lookup/2,
    resolve_transform/4,
    resolve_constructor/4
]).

-define(INVENTORY_VERSION, 1).

-type callable_kind() :: transform | constructor.
-type visibility() :: public | private.
-type callable() :: #{
    kind := callable_kind(),
    name := atom(),
    arity := non_neg_integer(),
    module := atom(),
    visibility := visibility(),
    type := term(),
    location := term(),
    identity := {atom(), non_neg_integer()}
}.
-opaque inventory() :: #{
    '$catena_callable_inventory' := pos_integer(),
    module := atom(),
    entries := [callable()],
    by_name := #{atom() => [callable()]}
}.

-export_type([callable_kind/0, callable/0, inventory/0]).

%% @doc Build the complete implemented-transform and constructor inventory.
-spec build(atom(), [term()], [term()]) ->
    {ok, inventory()} | {error, catena_backend_error:diagnostic()}.
build(Module, Exports, Declarations)
  when is_atom(Module), is_list(Exports), is_list(Declarations) ->
    Entries = lists:append([
        declaration_callables(Module, Exports, Declaration)
        || Declaration <- Declarations
    ]),
    ByName = lists:foldl(
        fun(Entry, Index) ->
            Name = maps:get(name, Entry),
            maps:update_with(Name, fun(Existing) -> Existing ++ [Entry] end,
                [Entry], Index)
        end,
        #{},
        Entries
    ),
    case first_ambiguous_name(ByName) of
        none ->
            {ok, #{
                '$catena_callable_inventory' => ?INVENTORY_VERSION,
                module => Module,
                entries => Entries,
                by_name => ByName
            }};
        {Name, Candidates} ->
            First = hd(Candidates),
            Context = catena_backend_error:context(
                callable_inventory,
                callable,
                maps:get(location, First),
                #{
                    module => Module,
                    location => maps:get(location, First),
                    reason => duplicate_or_overloaded_callable
                }
            ),
            {error, catena_backend_error:ambiguous_call(
                Name,
                maps:get(arity, First),
                Candidates,
                Context
            )}
    end.

%% @doc Return whether a term is a maintained callable inventory.
-spec is_inventory(term()) -> boolean().
is_inventory(#{
    '$catena_callable_inventory' := ?INVENTORY_VERSION,
    module := Module,
    entries := Entries,
    by_name := ByName
}) ->
    is_atom(Module) andalso is_list(Entries) andalso is_map(ByName);
is_inventory(_) ->
    false.

%% @doc Return all callables in source declaration order.
-spec callables(inventory()) -> [callable()].
callables(Inventory) ->
    maps:get(entries, Inventory).

%% @doc Return all local callables with a source name.
-spec lookup(atom(), inventory()) -> [callable()].
lookup(Name, Inventory) ->
    maps:get(Name, maps:get(by_name, Inventory), []).

%% @doc Resolve one direct local transform call and enforce exact arity.
-spec resolve_transform(atom(), non_neg_integer(), inventory(), map()) ->
    {ok, callable()} | {error, catena_backend_error:diagnostic()}.
resolve_transform(Name, Arity, Inventory, Context) ->
    resolve(transform, Name, Arity, Inventory, Context).

%% @doc Resolve one constructor application and enforce exact arity.
-spec resolve_constructor(atom(), non_neg_integer(), inventory(), map()) ->
    {ok, callable()} | {error, catena_backend_error:diagnostic()}.
resolve_constructor(Name, Arity, Inventory, Context) ->
    resolve(constructor, Name, Arity, Inventory, Context).

declaration_callables(
    Module,
    Exports,
    {transform_decl, Name, Type, Clauses, Location}
) when Clauses =/= [] ->
    [#{
        kind => transform,
        name => Name,
        arity => transform_arity(Clauses),
        module => Module,
        visibility => transform_visibility(Name, Exports),
        type => Type,
        location => Location,
        identity => {Name, transform_arity(Clauses)}
    }];
declaration_callables(
    Module,
    Exports,
    {transform, Name, Params, _Body, Location}
) ->
    [#{
        kind => transform,
        name => Name,
        arity => length(Params),
        module => Module,
        visibility => transform_visibility(Name, Exports),
        type => undefined,
        location => Location,
        identity => {Name, length(Params)}
    }];
declaration_callables(
    Module,
    Exports,
    {transform_typed, Name, Type, Params, _Body, Location}
) ->
    [#{
        kind => transform,
        name => Name,
        arity => length(Params),
        module => Module,
        visibility => transform_visibility(Name, Exports),
        type => Type,
        location => Location,
        identity => {Name, length(Params)}
    }];
declaration_callables(
    Module,
    Exports,
    {type_decl, Owner, _Params, Constructors, _Derives, _Location}
) ->
    [
        #{
            kind => constructor,
            name => Name,
            arity => length(Fields),
            module => Module,
            visibility => constructor_visibility(Owner, Exports),
            type => Fields,
            location => Location,
            identity => {Name, length(Fields)},
            owner => Owner
        }
        || {constructor, Name, Fields, Location} <- Constructors
    ];
declaration_callables(_Module, _Exports, _Declaration) ->
    [].

transform_arity([{transform_clause, Patterns, _, _, _} | _]) ->
    length(Patterns).

transform_visibility(_Name, []) ->
    public;
transform_visibility(Name, Exports) ->
    case lists:member({export_transform, Name}, Exports) orelse
        lists:any(
            fun
                ({Name0, Arity}) when Name0 =:= Name, is_integer(Arity) ->
                    true;
                (_) ->
                    false
            end,
            Exports
        )
    of
        true -> public;
        false -> private
    end.

constructor_visibility(_Owner, []) ->
    public;
constructor_visibility(Owner, Exports) ->
    case lists:member({export_type, Owner}, Exports) of
        true -> public;
        false -> private
    end.

first_ambiguous_name(ByName) ->
    first_ambiguous_entry(lists:sort(maps:to_list(ByName))).

first_ambiguous_entry([]) ->
    none;
first_ambiguous_entry([{Name, [_First, _Second | _] = Candidates} | _]) ->
    {Name, Candidates};
first_ambiguous_entry([_ | Rest]) ->
    first_ambiguous_entry(Rest).

resolve(Kind, Name, Arity, Inventory, Context) ->
    Candidates = [
        Candidate
        || Candidate <- lookup(Name, Inventory),
           maps:get(kind, Candidate) =:= Kind
    ],
    case Candidates of
        [] ->
            {error, catena_backend_error:unresolved_call(
                Name,
                Arity,
                Context#{callable_kind => Kind}
            )};
        [Candidate] ->
            Expected = maps:get(arity, Candidate),
            case Expected =:= Arity of
                true ->
                    {ok, Candidate};
                false ->
                    {error, catena_backend_error:arity_mismatch(
                        Name,
                        Expected,
                        Arity,
                        Context#{
                            callable_kind => Kind,
                            declaration_location =>
                                maps:get(location, Candidate),
                            declaration_identity =>
                                maps:get(identity, Candidate)
                        }
                    )}
            end;
        _ ->
            {error, catena_backend_error:ambiguous_call(
                Name,
                Arity,
                Candidates,
                Context#{callable_kind => Kind}
            )}
    end.
