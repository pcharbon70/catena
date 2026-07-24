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
    lookup_effect/2,
    lookup_operation/3,
    binding_name/2
]).

-define(INVENTORY_VERSION, 1).

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
    uses := [map()]
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
                uses => []
            },
            case collect_terms(Declarations, Inventory0, []) of
                {ok, Uses} ->
                    {ok, Inventory0#{uses := lists:reverse(Uses)}};
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
    uses := Uses
}) ->
    is_map(Effects) andalso is_map(Operations) andalso is_list(Uses);
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

resolution_error(Reason, SourceTerm, Extra) ->
    Context = catena_backend_error:context(
        effect_resolution,
        effect_operation,
        SourceTerm,
        Extra
    ),
    {error, {effect_resolution_error, Reason, Context}}.
