%%%-------------------------------------------------------------------
%%% @doc Authoritative direct/resumable control-mode analysis.
%%%
%%% This pass records local suspension reasons and callable edges, then solves
%%% the complete local call graph to a deterministic fixed point. Unknown
%%% imported, higher-order, trait-dispatched, and open-effect capabilities are
%%% resumable conservatively so Core lowering never guesses a convention.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_mode).

-export([
    analyze/5,
    is_inventory/1,
    entries/1,
    lookup/2,
    mode/2,
    regions/2,
    handler_modes/2
]).

-define(INVENTORY_VERSION, 2).

-type control_mode() :: direct | resumable.
-type edge_kind() :: local | imported | higher_order | trait_dispatch.
-type edge() :: #{
    kind := edge_kind(),
    target := term(),
    location := term(),
    capability := control_mode() | unknown
}.
-type entry() :: #{
    name := atom(),
    arity := non_neg_integer(),
    identity := {atom(), non_neg_integer()},
    mode := control_mode(),
    reason := term(),
    reasons := [term()],
    edges := [edge()],
    regions := [map()],
    handler_modes := [map()],
    type := term(),
    effect_row := term(),
    location := term()
}.
-opaque inventory() :: #{
    '$catena_control_modes' := pos_integer(),
    module := atom(),
    entries := [entry()],
    by_name := #{atom() => entry()},
    fixed_point_iterations := non_neg_integer()
}.

-export_type([control_mode/0, edge/0, entry/0, inventory/0]).

-spec analyze(atom(), [term()], [term()],
    catena_call_resolution:inventory(), map()) ->
    {ok, inventory()} | {error, term()}.
analyze(Module, Declarations, TypedDeclarations, Callables, Options)
        when is_atom(Module), is_list(Declarations),
             is_list(TypedDeclarations), is_map(Options) ->
    try
        TypedByName = typed_declarations_by_name(TypedDeclarations),
        LocalNames = sets:from_list([
            maps:get(name, Callable)
            || Callable <- catena_call_resolution:callables(Callables),
               maps:get(kind, Callable) =:= transform
        ]),
        LocalConstructors = sets:from_list([
            maps:get(name, Callable)
            || Callable <- catena_call_resolution:callables(Callables),
               maps:get(kind, Callable) =:= constructor
        ]),
        AnalysisOptions = Options#{
            local_constructors => LocalConstructors
        },
        Initial = [
            analyze_transform(
                Declaration,
                maps:get(element(2, Declaration), TypedByName, undefined),
                LocalNames,
                AnalysisOptions
            )
            || Declaration <- Declarations,
               is_transform_definition(Declaration)
        ],
        {Solved, Iterations} = solve_fixed_point(Initial, 0),
        ByName = maps:from_list([
            {maps:get(name, Entry), Entry}
            || Entry <- Solved
        ]),
        {ok, #{
            '$catena_control_modes' => ?INVENTORY_VERSION,
            module => Module,
            entries => Solved,
            by_name => ByName,
            fixed_point_iterations => Iterations
        }}
    catch
        error:Reason:Stack ->
            {error, {control_mode_analysis_error, Reason, Stack}}
    end.

-spec is_inventory(term()) -> boolean().
is_inventory(#{
    '$catena_control_modes' := ?INVENTORY_VERSION,
    module := Module,
    entries := Entries,
    by_name := ByName,
    fixed_point_iterations := Iterations
}) ->
    is_atom(Module) andalso
        is_list(Entries) andalso
        is_map(ByName) andalso
        is_integer(Iterations) andalso
        Iterations >= 0 andalso
        lists:all(fun valid_entry/1, Entries);
is_inventory(_) ->
    false.

-spec entries(inventory()) -> [entry()].
entries(Inventory) ->
    maps:get(entries, Inventory).

-spec lookup(atom(), inventory()) -> {ok, entry()} | none.
lookup(Name, Inventory) ->
    case maps:find(Name, maps:get(by_name, Inventory)) of
        {ok, Entry} -> {ok, Entry};
        error -> none
    end.

-spec mode(atom(), inventory()) -> {ok, control_mode()} | none.
mode(Name, Inventory) ->
    case lookup(Name, Inventory) of
        {ok, Entry} -> {ok, maps:get(mode, Entry)};
        none -> none
    end.

-spec regions(atom(), inventory()) -> [map()].
regions(Name, Inventory) ->
    case lookup(Name, Inventory) of
        {ok, Entry} -> maps:get(regions, Entry);
        none -> []
    end.

-spec handler_modes(atom(), inventory()) -> [map()].
handler_modes(Name, Inventory) ->
    case lookup(Name, Inventory) of
        {ok, Entry} -> maps:get(handler_modes, Entry, []);
        none -> []
    end.

analyze_transform(
    {transform_decl, Name, _DeclaredType, Clauses, Location},
    TypedDeclaration,
    LocalNames,
    Options
) ->
    Type = typed_transform_type(TypedDeclaration),
    Scan0 = #{
        reasons => [],
        edges => [],
        regions => [],
        handler_modes => [],
        provider_effect => false,
        handled_effects => []
    },
    Scan = lists:foldl(
        fun
            (
                {transform_clause, _Patterns, Guards, Body, _ClauseLocation},
                Current
            ) ->
                scan_term(
                    Body,
                    LocalNames,
                    Options,
                    scan_term(Guards, LocalNames, Options, Current)
                )
        end,
        Scan0,
        Clauses
    ),
    LocalReasons0 = maps:get(reasons, Scan),
    LocalReasons1 = case type_contains_open_effect_row(Type) of
        true -> [open_effect_row | LocalReasons0];
        false -> LocalReasons0
    end,
    LocalReasons = stable_unique(LocalReasons1),
    {Mode, Reason} = initial_mode(
        LocalReasons,
        maps:get(provider_effect, Scan)
    ),
    #{
        name => Name,
        arity => transform_arity(Clauses),
        identity => {Name, transform_arity(Clauses)},
        mode => Mode,
        reason => Reason,
        reasons => LocalReasons,
        edges => stable_unique(maps:get(edges, Scan)),
        regions => lists:reverse(maps:get(regions, Scan)),
        handler_modes => stable_unique(maps:get(handler_modes, Scan)),
        type => Type,
        effect_row => callable_effect_row(Type),
        location => Location
    }.

scan_term(undefined, _LocalNames, _Options, Scan) ->
    Scan;
scan_term(
    {resume_expr, Target, Value, Location},
    LocalNames,
    Options,
    Scan
) ->
    Scan1 = add_reason(
        resume_use,
        add_region(resumable, resume_use, Location, Scan)
    ),
    scan_term(
        Value,
        LocalNames,
        Options,
        scan_term(Target, LocalNames, Options, Scan1)
    );
scan_term(
    {handle_expr, Body, Handlers, Location},
    LocalNames,
    Options,
    Scan
) ->
    scan_term(
        {
            handle_expr,
            catena_resumption_mode:default(Location),
            Body,
            Handlers,
            Location
        },
        LocalNames,
        Options,
        Scan
    );
scan_term(
    {handle_expr, Mode, Body, Handlers, Location},
    LocalNames,
    Options,
    Scan
) ->
    Effects = [
        Effect
        || {handler_clause, Effect, _Operations, _HandlerLocation} <-
            Handlers
    ],
    Scan1 = add_reason(
        handler_delimiter,
        add_region(resumable, handler_delimiter, Location, Scan)
    ),
    Scan2 = Scan1#{
        handler_modes =>
            maps:get(handler_modes, Scan1) ++
                [catena_resumption_mode:interface_view(Mode)]
    },
    BodyScan = scan_term(
        Body,
        LocalNames,
        Options,
        Scan2#{
            handled_effects =>
                Effects ++ maps:get(handled_effects, Scan2)
        }
    ),
    scan_term(Handlers, LocalNames, Options, BodyScan#{
        handled_effects => maps:get(handled_effects, Scan)
    });
scan_term(
    {perform_expr, Effect, Operation, Arguments, Location},
    LocalNames,
    Options,
    Scan
) ->
    IsSuspension = lists:member(
        Effect,
        maps:get(handled_effects, Scan)
    ),
    {Mode, Reason, Scan1} = case IsSuspension of
        true ->
            {
                resumable,
                {suspension_point, Effect, Operation},
                add_reason(handler_delimiter, Scan)
            };
        false ->
            {
                direct,
                {provider_only, Effect, Operation},
                Scan#{provider_effect => true}
            }
    end,
    scan_term(
        Arguments,
        LocalNames,
        Options,
        add_region(Mode, Reason, Location, Scan1)
    );
scan_term(
    {app, _Function, _Arguments, _Location} = Application,
    LocalNames,
    Options,
    Scan
) ->
    {Function, Arguments, Location} = application_spine(Application),
    {Edge, EdgeReason} = classify_application(
        Function,
        length(Arguments),
        Location,
        LocalNames,
        Options
    ),
    Scan1 = case Edge of
        none -> Scan;
        _ -> add_edge(Edge, Scan)
    end,
    Scan2 = case EdgeReason of
        none -> Scan1;
        Reason -> add_reason(Reason, Scan1)
    end,
    scan_term(
        [Function, Arguments],
        LocalNames,
        Options,
        Scan2
    );
scan_term({lam, _Parameter, Body}, LocalNames, Options, Scan) ->
    scan_term(Body, LocalNames, Options, Scan);
scan_term(Term, LocalNames, Options, Scan) when is_tuple(Term) ->
    scan_term(tuple_to_list(Term), LocalNames, Options, Scan);
scan_term(Terms, LocalNames, Options, Scan) when is_list(Terms) ->
    lists:foldl(
        fun(Term, Current) ->
            scan_term(Term, LocalNames, Options, Current)
        end,
        Scan,
        Terms
    );
scan_term(_Other, _LocalNames, _Options, Scan) ->
    Scan.

classify_application(
    {var, Name, _Location},
    Arity,
    Location,
    LocalNames,
    Options
) ->
    case sets:is_element(Name, LocalNames) of
        true ->
            {edge(local, Name, Location, unknown), none};
        false ->
            case sets:is_element(
                Name,
                maps:get(local_constructors, Options, sets:new())
            ) of
                true ->
                    {none, none};
                false ->
                    classify_nonlocal_application(
                        Name,
                        Arity,
                        Location,
                        Options
                    )
            end
    end;
classify_application(
    {imported_ref, #{kind := constructor}, _RefLocation},
    _Arity,
    _CallLocation,
    _LocalNames,
    _Options
) ->
    {none, none};
classify_application(
    {imported_ref, Entry, _Location},
    _Arity,
    Location,
    _LocalNames,
    _Options
) ->
    Capability = maps:get(control_mode, Entry, unknown),
    Reason = case Capability of
        direct -> none;
        _ -> imported_mode_unknown
    end,
    {
        edge(
            imported,
            imported_identity(Entry),
            Location,
            Capability
        ),
        Reason
    };
classify_application(
    _Function,
    _Arity,
    Location,
    _LocalNames,
    _Options
) ->
    {
        edge(higher_order, dynamic_callable, Location, resumable),
        higher_order_call
    }.

classify_nonlocal_application(Name, Arity, Location, Options) ->
    case resolved_import(Name, Arity, Options) of
        {ok, #{kind := constructor}} ->
            {none, none};
        {ok, Entry} ->
            Capability = maps:get(
                control_mode,
                Entry,
                resumable
            ),
            Reason = case Capability of
                direct -> none;
                resumable -> imported_resumable
            end,
            {
                edge(
                    imported,
                    imported_identity(Entry),
                    Location,
                    Capability
                ),
                Reason
            };
        none ->
            case imported_constructor_binding(Name, Options) of
                true -> {none, none};
                false -> classify_dynamic_application(Name, Location)
            end
    end.

imported_constructor_binding(Name, Options) ->
    Resolution = maps:get(import_resolution, Options, undefined),
    case is_map(Resolution) andalso
        catena_import_resolution:is_resolution(Resolution)
    of
        true ->
            lists:any(
                fun(Entry) ->
                    maps:get(kind, Entry) =:= constructor andalso
                        maps:get(binding, Entry, undefined) =:= Name
                end,
                catena_import_resolution:entries(Resolution)
            );
        false ->
            false
    end.

classify_dynamic_application(Name, Location) ->
    case catena_trait_resolve:is_trait_method(Name) of
        true ->
            {
                edge(trait_dispatch, Name, Location, resumable),
                trait_dispatch
            };
        false ->
            {
                edge(higher_order, Name, Location, resumable),
                higher_order_call
            }
    end.

resolved_import(Name, Arity, Options) ->
    Resolution = maps:get(import_resolution, Options, undefined),
    case is_map(Resolution) andalso
        catena_import_resolution:is_resolution(Resolution)
    of
        true ->
            Matches = [
                Entry
                || Entry <- catena_import_resolution:entries(Resolution),
                   lists:member(
                       maps:get(kind, Entry),
                       [transform, constructor]
                   ),
                   maps:get(binding, Entry, undefined) =:= Name,
                   maps:get(arity, Entry) =:= Arity
            ],
            case Matches of
                [Entry] -> {ok, Entry};
                _ -> none
            end;
        false ->
            none
    end.

solve_fixed_point(Entries, Iterations) ->
    Modes = maps:from_list([
        {maps:get(name, Entry), maps:get(mode, Entry)}
        || Entry <- Entries
    ]),
    Updated = [
        propagate_entry(Entry, Modes)
        || Entry <- Entries
    ],
    case Updated =:= Entries of
        true -> {Entries, Iterations};
        false -> solve_fixed_point(Updated, Iterations + 1)
    end.

propagate_entry(#{mode := resumable} = Entry, _Modes) ->
    Entry;
propagate_entry(Entry, Modes) ->
    case first_resumable_local_edge(maps:get(edges, Entry), Modes) of
        none ->
            Entry;
        {ok, Target} ->
            Reasons = stable_unique(
                maps:get(reasons, Entry) ++
                    [{calls_resumable, Target}]
            ),
            Entry#{
                mode => resumable,
                reason => {calls_resumable, Target},
                reasons => Reasons
            }
    end.

first_resumable_local_edge([], _Modes) ->
    none;
first_resumable_local_edge(
    [#{kind := local, target := Target} | Rest],
    Modes
) ->
    case maps:get(Target, Modes, direct) of
        resumable -> {ok, Target};
        direct -> first_resumable_local_edge(Rest, Modes)
    end;
first_resumable_local_edge([_ | Rest], Modes) ->
    first_resumable_local_edge(Rest, Modes).

initial_mode([Reason | _], _ProviderEffect) ->
    {resumable, Reason};
initial_mode([], true) ->
    {direct, provider_only};
initial_mode([], false) ->
    {direct, pure}.

add_reason(Reason, Scan) ->
    Scan#{reasons => maps:get(reasons, Scan) ++ [Reason]}.

add_edge(Edge, Scan) ->
    Scan#{edges => maps:get(edges, Scan) ++ [Edge]}.

add_region(Mode, Reason, Location, Scan) ->
    Region = #{
        mode => Mode,
        reason => Reason,
        origin => Location
    },
    Scan#{regions => [Region | maps:get(regions, Scan)]}.

edge(Kind, Target, Location, Capability) ->
    #{
        kind => Kind,
        target => Target,
        location => Location,
        capability => Capability
    }.

application_spine({app, Function, Arguments, Location}) ->
    case Function of
        {app, _, [], _} ->
            {Function, Arguments, Location};
        {app, _, _, _} ->
            {Root, EarlierArguments, _EarlierLocation} =
                application_spine(Function),
            {Root, EarlierArguments ++ Arguments, Location};
        _ ->
            {Function, Arguments, Location}
    end.

imported_identity(Entry) ->
    {
        maps:get(source_module, Entry, undefined),
        maps:get(name, Entry, undefined),
        maps:get(arity, Entry, undefined)
    }.

typed_declarations_by_name(TypedDeclarations) ->
    maps:from_list([
        {element(2, Declaration), Declaration}
        || Declaration <- TypedDeclarations,
           is_typed_transform(Declaration)
    ]).

typed_transform_type(
    {typed_transform, _Name, Type, _Clauses, _Location}
) ->
    Type;
typed_transform_type(
    {typed_transform, _Name, Type, _Clauses, _Evidence, _Location}
) ->
    Type;
typed_transform_type(undefined) ->
    undefined.

callable_effect_row({tfun, _Input, Output, Effects}) ->
    merge_effect_rows(Effects, callable_effect_row(Output));
callable_effect_row(_Type) ->
    {teffectrow, [], closed}.

merge_effect_rows({effect_set, Labels}, {teffectrow, Rest, Tail}) ->
    {teffectrow, lists:usort(Labels ++ Rest), Tail};
merge_effect_rows(
    {teffectrow, Labels, Tail},
    {teffectrow, Rest, closed}
) ->
    {teffectrow, lists:usort(Labels ++ Rest), Tail};
merge_effect_rows(
    {teffectrow, Labels, closed},
    {teffectrow, Rest, Tail}
) ->
    {teffectrow, lists:usort(Labels ++ Rest), Tail};
merge_effect_rows(
    {teffectrow, Labels, _Tail} = Row,
    {teffectrow, Rest, _RestTail}
) ->
    case Labels ++ Rest of
        [] -> Row;
        Combined -> {teffectrow, lists:usort(Combined), open}
    end;
merge_effect_rows(_Effects, Row) ->
    Row.

type_contains_open_effect_row({teffectrow, _Labels, closed}) ->
    false;
type_contains_open_effect_row({teffectrow, _Labels, _Tail}) ->
    true;
type_contains_open_effect_row({tfun, Input, Output, Effects}) ->
    type_contains_open_effect_row(Input) orelse
        type_contains_open_effect_row(Output) orelse
        type_contains_open_effect_row(Effects);
type_contains_open_effect_row({tresumption, Kind, Input, Output, Effects}) ->
    type_contains_open_effect_row(Kind) orelse
        type_contains_open_effect_row(Input) orelse
        type_contains_open_effect_row(Output) orelse
        type_contains_open_effect_row(Effects);
type_contains_open_effect_row(Term) when is_tuple(Term) ->
    type_contains_open_effect_row(tuple_to_list(Term));
type_contains_open_effect_row(Terms) when is_list(Terms) ->
    lists:any(fun type_contains_open_effect_row/1, Terms);
type_contains_open_effect_row(_Type) ->
    false.

valid_entry(#{
    name := Name,
    arity := Arity,
    mode := Mode,
    reason := _Reason,
    edges := Edges,
    regions := Regions,
    handler_modes := HandlerModes
}) ->
    is_atom(Name) andalso
        is_integer(Arity) andalso
        Arity >= 0 andalso
        lists:member(Mode, [direct, resumable]) andalso
        is_list(Edges) andalso
        is_list(Regions) andalso
        is_list(HandlerModes) andalso
        lists:all(fun valid_handler_mode/1, HandlerModes);
valid_entry(_) ->
    false.

valid_handler_mode(#{depth := Depth, kind := Kind}) ->
    lists:member(Depth, [deep, shallow]) andalso
        lists:member(Kind, [one_shot, multi_shot]);
valid_handler_mode(_) ->
    false.

is_transform_definition(
    {transform_decl, Name, _Type, Clauses, _Location}
) ->
    is_atom(Name) andalso Clauses =/= [];
is_transform_definition(_) ->
    false.

is_typed_transform({typed_transform, _, _, _, _}) ->
    true;
is_typed_transform({typed_transform, _, _, _, _, _}) ->
    true;
is_typed_transform(_) ->
    false.

transform_arity([
    {transform_clause, Patterns, _Guards, _Body, _Location}
    | _
]) ->
    length(Patterns).

stable_unique(Items) ->
    stable_unique(Items, sets:new(), []).

stable_unique([], _Seen, Acc) ->
    lists:reverse(Acc);
stable_unique([Item | Rest], Seen, Acc) ->
    case sets:is_element(Item, Seen) of
        true -> stable_unique(Rest, Seen, Acc);
        false ->
            stable_unique(
                Rest,
                sets:add_element(Item, Seen),
                [Item | Acc]
            )
    end.
