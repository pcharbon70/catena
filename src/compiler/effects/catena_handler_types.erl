-module(catena_handler_types).

%% Handler type definitions with full type signatures.
%% Handlers have explicit types specifying their operations, input/output types,
%% and effect rows for type-safe effect handling.

-export([
    %% Handler type constructors
    handler_type/0,
    handler_type/1,
    handler_type/3,
    %% Operation signature constructors
    operation_sig/0,
    operation_sig/2,
    operation_sig/3,
    %% Handler type builders
    with_operations/2,
    with_input/2,
    with_output/2,
    with_effects/2,
    %% Handler type predicates
    is_handler_type/1,
    is_operation_sig/1,
    %% Handler type validation
    is_valid_handler_type/1,
    is_valid_operation_sig/1,
    %% Handler type composition
    compose_handler_types/2,
    %% Pretty printing
    format_handler_type/1,
    format_operation_sig/1
]).

-export_type([
    handler_type/0,
    operation_sig/0,
    operation_map/0,
    handler_constraints/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Operation signature: parameter types, result type, effect row.
-type operation_sig() :: #{
    kind => operation_sig,
    params => [type_ref()],
    result => type_ref(),
    effects => catena_row_types:effect_row()
}.

%% @doc Type reference - simplified to avoid circular dependencies.
-type type_ref() :: atom() | tuple() | map().

%% @doc Map of operation names to their signatures.
-type operation_map() :: #{atom() => operation_sig()}.

%% @doc Handler type constraints.
-type handler_constraints() :: #{
    total => boolean(),
    deep => boolean()
}.

%% @doc Handler type with full signature.
-type handler_type() :: #{
    kind => handler_type,
    name => atom() | undefined,
    operations => operation_map(),
    input => type_ref(),
    output => type_ref(),
    effects => catena_row_types:effect_row(),
    constraints => handler_constraints()
}.

%%%---------------------------------------------------------------------
%%% Handler Type Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty handler type.
-spec handler_type() -> handler_type().
handler_type() ->
    #{
        kind => handler_type,
        name => undefined,
        operations => #{},
        input => catena_types:t_var("A"),
        output => catena_types:t_var("B"),
        effects => catena_row_types:empty_row(),
        constraints => #{total => true, deep => false}
    }.

%% @doc Create a handler type with a name.
-spec handler_type(atom()) -> handler_type().
handler_type(Name) when is_atom(Name) ->
    (handler_type())#{name => Name}.

%% @doc Create a handler type with name and input/output types.
-spec handler_type(atom(), type_ref(), type_ref()) -> handler_type().
handler_type(Name, Input, Output) when is_atom(Name) ->
    (handler_type())#{
        name => Name,
        input => Input,
        output => Output
    }.

%%%---------------------------------------------------------------------
%%% Operation Signature Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty operation signature.
-spec operation_sig() -> operation_sig().
operation_sig() ->
    #{
        kind => operation_sig,
        params => [],
        result => catena_types:t_var("T"),
        effects => catena_row_types:empty_row()
    }.

%% @doc Create an operation signature with params and result.
-spec operation_sig([type_ref()], type_ref()) -> operation_sig().
operation_sig(Params, Result) when is_list(Params) ->
    #{
        kind => operation_sig,
        params => Params,
        result => Result,
        effects => catena_row_types:empty_row()
    }.

%% @doc Create an operation signature with params, result, and effects.
-spec operation_sig([type_ref()], type_ref(), catena_row_types:effect_row()) -> operation_sig().
operation_sig(Params, Result, Effects) when is_list(Params) ->
    #{
        kind => operation_sig,
        params => Params,
        result => Result,
        effects => Effects
    }.

%%%---------------------------------------------------------------------
%%% Handler Type Builders
%%%---------------------------------------------------------------------

%% @doc Add operations to a handler type.
-spec with_operations(handler_type(), operation_map()) -> handler_type().
with_operations(Handler, Ops) when is_map(Ops) ->
    Handler#{operations => Ops}.

%% @doc Set the input type of a handler.
-spec with_input(handler_type(), type_ref()) -> handler_type().
with_input(Handler, Input) ->
    Handler#{input => Input}.

%% @doc Set the output type of a handler.
-spec with_output(handler_type(), type_ref()) -> handler_type().
with_output(Handler, Output) ->
    Handler#{output => Output}.

%% @doc Set the effects of a handler.
-spec with_effects(handler_type(), catena_row_types:effect_row()) -> handler_type().
with_effects(Handler, Effects) ->
    Handler#{effects => Effects}.

%%%---------------------------------------------------------------------
%%% Handler Type Predicates
%%%---------------------------------------------------------------------

%% @doc Check if a term is a handler type.
-spec is_handler_type(term()) -> boolean().
is_handler_type(#{kind := handler_type, operations := Ops, input := _, output := _}) when is_map(Ops) ->
    true;
is_handler_type(_) ->
    false.

%% @doc Check if a term is an operation signature.
-spec is_operation_sig(term()) -> boolean().
is_operation_sig(#{kind := operation_sig, params := _, result := _}) ->
    true;
is_operation_sig(_) ->
    false.

%%%---------------------------------------------------------------------
%%% Handler Type Validation
%%%---------------------------------------------------------------------

%% @doc Validate a handler type.
-spec is_valid_handler_type(handler_type()) -> boolean().
is_valid_handler_type(#{kind := handler_type, operations := Ops, input := In, output := Out}) ->
    maps:is_kind(Ops, map) andalso
    maps:size(Ops) > 0 andalso
    is_valid_type(In) andalso
    is_valid_type(Out) andalso
    maps:fold(fun(_, Sig, Acc) -> Acc andalso is_valid_operation_sig(Sig) end, true, Ops).

%% @doc Validate an operation signature.
-spec is_valid_operation_sig(operation_sig()) -> boolean().
is_valid_operation_sig(#{kind := operation_sig, params := Params, result := Result, effects := Effects}) ->
    is_list(Params) andalso
    lists:all(fun is_valid_type/1, Params) andalso
    is_valid_type(Result) andalso
    catena_row_types:is_valid_row(Effects).

%%%---------------------------------------------------------------------
%%% Handler Type Composition
%%%---------------------------------------------------------------------

%% @doc Compose two handler types.
%% The output of the first must match the input of the second.
-spec compose_handler_types(handler_type(), handler_type()) -> {ok, handler_type()} | {error, term()}.
compose_handler_types(#{output := Out1} = H1, #{input := In2} = H2) ->
    case types_equal(Out1, In2) of
        true ->
            CombinedOps = maps:merge(
                maps:get(operations, H1, #{}),
                maps:get(operations, H2, #{})
            ),
            CombinedEffects = catena_row_operations:effect_union_rows(
                maps:get(effects, H1, catena_row_types:empty_row()),
                maps:get(effects, H2, catena_row_types:empty_row())
            ),
            {ok, #{
                kind => handler_type,
                name => undefined,
                operations => CombinedOps,
                input => maps:get(input, H1),
                output => maps:get(output, H2),
                effects => CombinedEffects,
                constraints => #{total => true, deep => false}
            }};
        false ->
            {error, {type_mismatch, Out1, In2}}
    end.

%%%---------------------------------------------------------------------
%%% Pretty Printing
%%%---------------------------------------------------------------------

%% @doc Format a handler type for display.
-spec format_handler_type(handler_type()) -> binary().
format_handler_type(#{name := Name, operations := Ops, input := In, output := Out, effects := Effects}) ->
    NamePart = case Name of
        undefined -> <<"Handler">>;
        _ -> list_to_binary(atom_to_list(Name))
    end,
    InputPart = format_type(In),
    OutputPart = format_type(Out),
    EffectsPart = format_effects(Effects),
    OpsCount = maps:size(Ops),
    OpsCountBin = integer_to_binary(OpsCount),
    <<NamePart/binary, " (", InputPart/binary, " -> ", OutputPart/binary, " / ", EffectsPart/binary, ") [",
      OpsCountBin/binary, " ops]">>.

%% @doc Format an operation signature for display.
-spec format_operation_sig(operation_sig()) -> binary().
format_operation_sig(#{params := Params, result := Result, effects := Effects}) ->
    ParamsPart = format_params(Params),
    ResultPart = format_type(Result),
    EffectsPart = format_effects(Effects),
    <<ParamsPart/binary, " -> ", ResultPart/binary, " / ", EffectsPart/binary>>.

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

%% @doc Check if a term is a valid type.
-spec is_valid_type(term()) -> boolean().
is_valid_type(Type) when is_map(Type); is_atom(Type); is_tuple(Type) ->
    true;
is_valid_type(_) ->
    false.

%% @doc Check if two types are equal (simplified).
-spec types_equal(term(), term()) -> boolean().
types_equal(T1, T2) when is_map(T1), is_map(T2) ->
    maps:size(T1) =:= maps:size(T2) andalso
    lists:all(fun({K, V}) ->
        case maps:find(K, T2) of
            {ok, V2} -> types_equal(V, V2);
            error -> false
        end
    end, maps:to_list(T1));
types_equal(T1, T2) when is_tuple(T1), is_tuple(T2) ->
    tuple_size(T1) =:= tuple_size(T2) andalso
    lists:all(fun({I1, I2}) -> types_equal(I1, I2) end,
        lists:zip(tuple_to_list(T1), tuple_to_list(T2)));
types_equal(A, B) when is_atom(A); is_number(A); is_binary(A) ->
    A =:= B;
types_equal(_, _) ->
    false.

%% @doc Format a type for display.
-spec format_type(type_ref()) -> binary().
format_type(Type) when is_atom(Type) ->
    list_to_binary(atom_to_list(Type));
format_type(Type) when is_map(Type) ->
    case maps:get(kind, Type, undefined) of
        undefined -> <<"{}">>;
        Kind -> <<(list_to_binary(atom_to_list(Kind)))/binary, "{}">>
    end;
format_type(Type) when is_tuple(Type) ->
    Elements = tuple_to_list(Type),
    Formatted = [format_type(E) || E <- Elements],
    list_to_binary(["(" | [lists:join(<<", ">>, Formatted), ")"]]);
format_type(_) ->
    <<"?">>.

%% @doc Format effects for display.
-spec format_effects(catena_row_types:effect_row()) -> binary().
format_effects(Effects) ->
    List = catena_row_types:row_to_list(Effects),
    Formatted = [format_effect(E) || E <- List],
    list_to_binary(["{" | [lists:join(<<", ">>, Formatted), "}"]]).

%% @doc Format a single effect.
-spec format_effect(atom() | map()) -> binary().
format_effect(Effect) when is_atom(Effect) ->
    list_to_binary(atom_to_list(Effect));
format_effect(Effect) when is_map(Effect) ->
    <<"ρ">>.

%% @doc Format parameter list.
-spec format_params([type_ref()]) -> binary().
format_params([]) ->
    <<"()">>;
format_params(Params) ->
    Formatted = [format_type(P) || P <- Params],
    list_to_binary(["(" | [lists:join(<<", ">>, Formatted), ")"]]).
