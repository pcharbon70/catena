%%%-------------------------------------------------------------------
%%% @doc Declared effect-operation inventory and source-use resolution.
%%%
%%% The inventory is built before type checking and retained by the validated
%%% compilation unit.  It gives every performed operation one declaration
%%% identity and prevents effect declarations from being erased before their
%%% signatures have been consumed by the frontend and backend.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_resolution).

-export([
    build/1,
    is_inventory/1,
    effects/1,
    operations/1,
    uses/1,
    handlers/1,
    lookup_effect/2,
    lookup_operation/3,
    binding_name/2,
    effectful_transforms/1
]).

-define(INVENTORY_VERSION, 2).

-type operation_identity() :: {atom(), atom(), non_neg_integer()}.
-type operation() :: #{
    identity := operation_identity(),
    effect := atom(),
    operation := atom(),
    arity := non_neg_integer(),
    parameter_types := [term()],
    result_type := term(),
    declared_type := term(),
    declared_effects := term(),
    effect_location := term(),
    location := term()
}.
-opaque inventory() :: #{
    '$catena_effect_inventory' := pos_integer(),
    effects := #{atom() => map()},
    operations := #{{atom(), atom()} => operation()},
    uses := [map()],
    handlers := [map()]
}.

-export_type([inventory/0, operation/0, operation_identity/0]).

%% @doc Build and validate the complete local effect-operation inventory.
-spec build([term()]) -> {ok, inventory()} | {error, term()}.
build(Declarations) when is_list(Declarations) ->
    case index_declarations(Declarations, #{}, #{}) of
        {ok, Effects, Operations} ->
            Inventory0 = #{
                '$catena_effect_inventory' => ?INVENTORY_VERSION,
                effects => Effects,
                operations => Operations,
                uses => [],
                handlers => []
            },
            case collect_terms(Declarations, Inventory0, []) of
                {ok, Uses} ->
                    case collect_handlers(
                        Declarations,
                        Inventory0,
                        []
                    ) of
                        {ok, Handlers} ->
                            {ok, Inventory0#{
                                uses := lists:reverse(Uses),
                                handlers := lists:reverse(Handlers)
                            }};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec is_inventory(term()) -> boolean().
is_inventory(#{
    '$catena_effect_inventory' := ?INVENTORY_VERSION,
    effects := Effects,
    operations := Operations,
    uses := Uses,
    handlers := Handlers
}) ->
    is_map(Effects) andalso
        is_map(Operations) andalso
        is_list(Uses) andalso
        is_list(Handlers);
is_inventory(_) ->
    false.

-spec effects(inventory()) -> #{atom() => map()}.
effects(Inventory) ->
    maps:get(effects, Inventory).

-spec operations(inventory()) -> #{{atom(), atom()} => operation()}.
operations(Inventory) ->
    maps:get(operations, Inventory).

-spec uses(inventory()) -> [map()].
uses(Inventory) ->
    maps:get(uses, Inventory).

-spec handlers(inventory()) -> [map()].
handlers(Inventory) ->
    maps:get(handlers, Inventory).

-spec lookup_effect(atom(), inventory()) -> {ok, map()} | error.
lookup_effect(Effect, Inventory) ->
    maps:find(Effect, effects(Inventory)).

-spec lookup_operation(atom(), atom(), inventory()) -> {ok, operation()} | error.
lookup_operation(Effect, Operation, Inventory) ->
    maps:find({Effect, Operation}, operations(Inventory)).

%% @doc Stable private type-environment key for an effect operation.
-spec binding_name(atom(), atom()) -> atom().
binding_name(Effect, Operation) ->
    list_to_atom(
        "$catena_effect$" ++ atom_to_list(Effect) ++ "$" ++
            atom_to_list(Operation)
    ).

%% @doc Return local transforms that require an explicit runtime context.
%%
%% Direct effect syntax starts the set and local call edges propagate the
%% requirement so an effectful helper shares its caller's context.
-spec effectful_transforms([term()]) ->
    #{atom() => non_neg_integer()}.
effectful_transforms(Declarations) ->
    TransformInfo = maps:from_list([
        {Name, #{
            arity => transform_arity(Clauses),
            direct => clauses_require_runtime(Clauses),
            calls => lists:usort(clauses_local_calls(Clauses))
        }}
        || {transform_decl, Name, _Type, Clauses, _Location} <- Declarations,
           Clauses =/= []
    ]),
    Initial = sets:from_list([
        Name
        || {Name, Info} <- maps:to_list(TransformInfo),
           maps:get(direct, Info)
    ]),
    Effectful = close_effectful_calls(TransformInfo, Initial),
    maps:from_list([
        {Name, maps:get(arity, maps:get(Name, TransformInfo))}
        || Name <- sets:to_list(Effectful)
    ]).

index_declarations([], Effects, Operations) ->
    {ok, Effects, Operations};
index_declarations(
    [{effect_decl, Effect, OperationDecls, EffectLocation} = Declaration | Rest],
    Effects,
    Operations
) ->
    case maps:is_key(Effect, Effects) of
        true ->
            resolution_error(
                duplicate_effect,
                Declaration,
                #{effect => Effect}
            );
        false ->
            case index_operations(
                Effect,
                EffectLocation,
                OperationDecls,
                Operations,
                []
            ) of
                {ok, Operations1, Identities} ->
                    EffectInfo = #{
                        identity => Effect,
                        operations => lists:reverse(Identities),
                        location => EffectLocation
                    },
                    index_declarations(
                        Rest,
                        maps:put(Effect, EffectInfo, Effects),
                        Operations1
                    );
                {error, _} = Error ->
                    Error
            end
    end;
index_declarations([_Declaration | Rest], Effects, Operations) ->
    index_declarations(Rest, Effects, Operations).

index_operations(_Effect, _EffectLocation, [], Operations, Identities) ->
    {ok, Operations, Identities};
index_operations(
    Effect,
    _EffectLocation,
    [{effect_operation, Operation, undefined, Location} = Declaration | _Rest],
    _Operations,
    _Identities
) ->
    resolution_error(
        missing_operation_signature,
        Declaration,
        #{
            effect => Effect,
            operation => Operation,
            location => Location
        }
    );
index_operations(
    Effect,
    EffectLocation,
    [{effect_operation, Operation, Type, Location} = Declaration | Rest],
    Operations,
    Identities
) ->
    Key = {Effect, Operation},
    case maps:is_key(Key, Operations) of
        true ->
            resolution_error(
                duplicate_operation,
                Declaration,
                #{effect => Effect, operation => Operation}
            );
        false ->
            {ParameterTypes, ResultType, DeclaredEffects} =
                split_signature(Type),
            Arity = length(ParameterTypes),
            Identity = {Effect, Operation, Arity},
            OperationInfo = #{
                identity => Identity,
                effect => Effect,
                operation => Operation,
                arity => Arity,
                parameter_types => ParameterTypes,
                result_type => ResultType,
                declared_type => Type,
                declared_effects => DeclaredEffects,
                effect_location => EffectLocation,
                location => Location
            },
            index_operations(
                Effect,
                EffectLocation,
                Rest,
                maps:put(Key, OperationInfo, Operations),
                [Identity | Identities]
            )
    end;
index_operations(
    Effect,
    _EffectLocation,
    [Invalid | _Rest],
    _Operations,
    _Identities
) ->
    resolution_error(
        invalid_operation_declaration,
        Invalid,
        #{effect => Effect}
    ).

split_signature(Type) ->
    split_signature(Type, [], undefined).

split_signature(
    {type_fun, Parameter, Rest, _Location},
    Parameters,
    Effects
) ->
    split_signature(Rest, [Parameter | Parameters], Effects);
split_signature(
    {type_effect, Result, DeclaredEffects, _Location},
    Parameters,
    _Effects
) ->
    {lists:reverse(Parameters), Result, DeclaredEffects};
split_signature(Result, Parameters, Effects) ->
    {lists:reverse(Parameters), Result, Effects}.

collect_terms([], _Inventory, Uses) ->
    {ok, Uses};
collect_terms([Term | Rest], Inventory, Uses) ->
    case collect_term(Term, Inventory, Uses) of
        {ok, Uses1} ->
            collect_terms(Rest, Inventory, Uses1);
        {error, _} = Error ->
            Error
    end.

collect_term(
    {perform_expr, Effect, Operation, Arguments, Location} = Perform,
    Inventory,
    Uses
) ->
    case lookup_effect(Effect, Inventory) of
        error ->
            resolution_error(
                unknown_effect,
                Perform,
                #{
                    effect => Effect,
                    operation => Operation,
                    location => Location
                }
            );
        {ok, _} ->
            case lookup_operation(Effect, Operation, Inventory) of
                error ->
                    resolution_error(
                        unknown_operation,
                        Perform,
                        #{
                            effect => Effect,
                            operation => Operation,
                            location => Location
                        }
                    );
                {ok, OperationInfo} ->
                    Expected = maps:get(arity, OperationInfo),
                    Actual = length(Arguments),
                    case Expected =:= Actual of
                        false ->
                            resolution_error(
                                operation_arity_mismatch,
                                Perform,
                                #{
                                    effect => Effect,
                                    operation => Operation,
                                    expected_arity => Expected,
                                    actual_arity => Actual,
                                    declaration_location =>
                                        maps:get(location, OperationInfo),
                                    location => Location
                                }
                            );
                        true ->
                            Use = #{
                                identity => maps:get(identity, OperationInfo),
                                effect => Effect,
                                operation => Operation,
                                arity => Actual,
                                declared_type =>
                                    maps:get(declared_type, OperationInfo),
                                location => Location
                            },
                            collect_terms(Arguments, Inventory, [Use | Uses])
                    end
            end
    end;
collect_term(Term, Inventory, Uses) when is_tuple(Term) ->
    collect_terms(tl(tuple_to_list(Term)), Inventory, Uses);
collect_term(Terms, Inventory, Uses) when is_list(Terms) ->
    collect_terms(Terms, Inventory, Uses);
collect_term(_Term, _Inventory, Uses) ->
    {ok, Uses}.

collect_handlers([], _Inventory, Handlers) ->
    {ok, Handlers};
collect_handlers([Term | Rest], Inventory, Handlers) ->
    case collect_handler_term(Term, Inventory, Handlers) of
        {ok, Handlers1} ->
            collect_handlers(Rest, Inventory, Handlers1);
        {error, _} = Error ->
            Error
    end.

collect_handler_term(
    {handle_expr, Body, HandlerClauses, _Location},
    Inventory,
    Handlers
) ->
    case validate_handler_clauses(
        HandlerClauses,
        Inventory,
        sets:new(),
        Handlers
    ) of
        {ok, Handlers1} ->
            HandlerBodies = [
                OperationBody
                || {handler_clause, _Effect, Operations0, _HandlerLocation} <-
                    HandlerClauses,
                   {operation_case, _Operation, _Params, OperationBody, _OpLocation} <-
                    Operations0
            ],
            collect_handlers(
                [Body | HandlerBodies],
                Inventory,
                Handlers1
            );
        {error, _} = Error ->
            Error
    end;
collect_handler_term(
    {try_with_expr, Body, HandlerClauses, Location},
    Inventory,
    Handlers
) ->
    collect_handler_term(
        {handle_expr, Body, HandlerClauses, Location},
        Inventory,
        Handlers
    );
collect_handler_term(Term, Inventory, Handlers) when is_tuple(Term) ->
    collect_handlers(tl(tuple_to_list(Term)), Inventory, Handlers);
collect_handler_term(Terms, Inventory, Handlers) when is_list(Terms) ->
    collect_handlers(Terms, Inventory, Handlers);
collect_handler_term(_Term, _Inventory, Handlers) ->
    {ok, Handlers}.

validate_handler_clauses(
    [],
    _Inventory,
    _HandledEffects,
    Handlers
) ->
    {ok, Handlers};
validate_handler_clauses(
    [{handler_clause, Effect, Cases, Location} = Handler | Rest],
    Inventory,
    HandledEffects,
    Handlers
) ->
    case sets:is_element(Effect, HandledEffects) of
        true ->
            resolution_error(
                duplicate_effect_handler,
                Handler,
                #{effect => Effect, location => Location}
            );
        false ->
            case lookup_effect(Effect, Inventory) of
                error ->
                    resolution_error(
                        unknown_handled_effect,
                        Handler,
                        #{effect => Effect, location => Location}
                    );
                {ok, EffectInfo} ->
                    case validate_operation_cases(
                        Effect,
                        Cases,
                        Inventory,
                        sets:new(),
                        []
                    ) of
                        {ok, CaseIdentities, SeenOperations} ->
                            DeclaredOperations = [
                                Operation
                                || {Effect0, Operation, _Arity} <-
                                    maps:get(operations, EffectInfo),
                                   Effect0 =:= Effect
                            ],
                            Missing = lists:sort(
                                DeclaredOperations --
                                    sets:to_list(SeenOperations)
                            ),
                            case Missing of
                                [] ->
                                    HandlerInfo = #{
                                        effect => Effect,
                                        operations =>
                                            lists:reverse(CaseIdentities),
                                        location => Location
                                    },
                                    validate_handler_clauses(
                                        Rest,
                                        Inventory,
                                        sets:add_element(
                                            Effect,
                                            HandledEffects
                                        ),
                                        [HandlerInfo | Handlers]
                                    );
                                _ ->
                                    resolution_error(
                                        missing_handler_operations,
                                        Handler,
                                        #{
                                            effect => Effect,
                                            missing_operations => Missing,
                                            location => Location
                                        }
                                    )
                            end;
                        {error, _} = Error ->
                            Error
                    end
            end
    end;
validate_handler_clauses(
    [Invalid | _Rest],
    _Inventory,
    _HandledEffects,
    _Handlers
) ->
    resolution_error(
        invalid_handler_clause,
        Invalid,
        #{}
    ).

validate_operation_cases(
    _Effect,
    [],
    _Inventory,
    Seen,
    Identities
) ->
    {ok, Identities, Seen};
validate_operation_cases(
    Effect,
    [{operation_case, Operation, Params, _Body, Location} = Case | Rest],
    Inventory,
    Seen,
    Identities
) ->
    case sets:is_element(Operation, Seen) of
        true ->
            resolution_error(
                duplicate_handler_operation,
                Case,
                #{
                    effect => Effect,
                    operation => Operation,
                    location => Location
                }
            );
        false ->
            case lookup_operation(Effect, Operation, Inventory) of
                error ->
                    resolution_error(
                        unknown_handler_operation,
                        Case,
                        #{
                            effect => Effect,
                            operation => Operation,
                            location => Location
                        }
                    );
                {ok, OperationInfo} ->
                    Expected = maps:get(arity, OperationInfo),
                    Actual = length(Params),
                    case Expected =:= Actual of
                        true ->
                            validate_operation_cases(
                                Effect,
                                Rest,
                                Inventory,
                                sets:add_element(Operation, Seen),
                                [
                                    maps:get(identity, OperationInfo)
                                    | Identities
                                ]
                            );
                        false ->
                            resolution_error(
                                handler_arity_mismatch,
                                Case,
                                #{
                                    effect => Effect,
                                    operation => Operation,
                                    expected_arity => Expected,
                                    actual_arity => Actual,
                                    declaration_location =>
                                        maps:get(location, OperationInfo),
                                    location => Location
                                }
                            )
                    end
            end
    end;
validate_operation_cases(
    Effect,
    [Invalid | _Rest],
    _Inventory,
    _Seen,
    _Identities
) ->
    resolution_error(
        invalid_handler_operation,
        Invalid,
        #{effect => Effect}
    ).

resolution_error(Reason, SourceTerm, Extra) ->
    Context = catena_backend_error:context(
        effect_resolution,
        effect_operation,
        SourceTerm,
        Extra
    ),
    {error, {effect_resolution_error, Reason, Context}}.

transform_arity([{transform_clause, Patterns, _Guards, _Body, _Location} | _]) ->
    length(Patterns).

clauses_require_runtime(Clauses) ->
    lists:any(
        fun({transform_clause, _Patterns, Guards, Body, _Location}) ->
            term_requires_runtime(Guards) orelse
                term_requires_runtime(Body)
        end,
        Clauses
    ).

term_requires_runtime({perform_expr, _, _, _, _}) ->
    true;
term_requires_runtime({handle_expr, _, _, _}) ->
    true;
term_requires_runtime({try_with_expr, _, _, _}) ->
    true;
term_requires_runtime(Term) when is_tuple(Term) ->
    term_requires_runtime(tuple_to_list(Term));
term_requires_runtime(Terms) when is_list(Terms) ->
    lists:any(fun term_requires_runtime/1, Terms);
term_requires_runtime(_) ->
    false.

clauses_local_calls(Clauses) ->
    lists:append([
        term_local_calls(Guards) ++ term_local_calls(Body)
        || {transform_clause, _Patterns, Guards, Body, _Location} <- Clauses
    ]).

term_local_calls({app, Function, _Arguments, _Location} = Application) ->
    Root = application_root(Function),
    Direct = case Root of
        {var, Name, _} when is_atom(Name) -> [Name];
        _ -> []
    end,
    Direct ++ term_local_calls(tl(tuple_to_list(Application)));
term_local_calls(Term) when is_tuple(Term) ->
    term_local_calls(tuple_to_list(Term));
term_local_calls(Terms) when is_list(Terms) ->
    lists:append([term_local_calls(Term) || Term <- Terms]);
term_local_calls(_) ->
    [].

application_root({app, Function, _Arguments, _Location}) ->
    application_root(Function);
application_root(Function) ->
    Function.

close_effectful_calls(TransformInfo, Effectful) ->
    Expanded = maps:fold(
        fun(Name, Info, Acc) ->
            Calls = maps:get(calls, Info),
            case lists:any(
                fun(Called) -> sets:is_element(Called, Acc) end,
                Calls
            ) of
                true -> sets:add_element(Name, Acc);
                false -> Acc
            end
        end,
        Effectful,
        TransformInfo
    ),
    case sets:size(Expanded) =:= sets:size(Effectful) of
        true -> Expanded;
        false -> close_effectful_calls(TransformInfo, Expanded)
    end.
