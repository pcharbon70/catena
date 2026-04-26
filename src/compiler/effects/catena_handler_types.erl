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
    handler_constraints/0,
    type_ref/0
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
    deep => boolean(),
    pure => boolean()
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
        input => {type_var, input},
        output => {type_var, output},
        effects => catena_row_types:empty_row(),
        constraints => default_constraints()
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
        result => {type_var, result},
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
        effects => catena_row_types:row_normalize(Effects)
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
    Handler#{effects => catena_row_types:row_normalize(Effects)}.

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
is_valid_handler_type(#{
    kind := handler_type,
    name := Name,
    operations := Ops,
    input := In,
    output := Out,
    effects := Effects,
    constraints := Constraints
}) ->
    is_valid_name(Name) andalso
    is_map(Ops) andalso
    maps:size(Ops) > 0 andalso
    maps:fold(fun(Op, Sig, Acc) -> Acc andalso is_atom(Op) andalso is_valid_operation_sig(Sig) end, true, Ops) andalso
    is_valid_type(In) andalso
    is_valid_type(Out) andalso
    catena_row_types:is_valid_row(Effects) andalso
    is_valid_constraints(Constraints);
is_valid_handler_type(_) ->
    false.

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
    case {is_handler_type(H1), is_handler_type(H2)} of
        {false, _} ->
            {error, {invalid_handler_type, H1}};
        {_, false} ->
            {error, {invalid_handler_type, H2}};
        {true, true} ->
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
                        constraints => merge_constraints(
                            maps:get(constraints, H1, default_constraints()),
                            maps:get(constraints, H2, default_constraints())
                        )
                    }};
                false ->
                    {error, {type_mismatch, Out1, In2}}
            end
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
    is_valid_type_term(Type);
is_valid_type(_) ->
    false.

%% @doc Check if two types are equal (simplified).
-spec types_equal(term(), term()) -> boolean().
types_equal({type_var, _}, _) ->
    true;
types_equal(_, {type_var, _}) ->
    true;
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
format_type({type_var, Name}) ->
    <<"'",
      (list_to_binary(atom_to_list(Name)))/binary>>;
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

-spec default_constraints() -> handler_constraints().
default_constraints() ->
    #{total => true, deep => false, pure => false}.

-spec is_valid_name(atom() | undefined) -> boolean().
is_valid_name(undefined) ->
    true;
is_valid_name(Name) ->
    is_atom(Name).

-spec is_valid_constraints(map()) -> boolean().
is_valid_constraints(#{total := Total, deep := Deep, pure := Pure}) ->
    is_boolean(Total) andalso is_boolean(Deep) andalso is_boolean(Pure);
is_valid_constraints(_) ->
    false.

-spec merge_constraints(handler_constraints(), handler_constraints()) -> handler_constraints().
merge_constraints(Left, Right) ->
    #{
        total => maps:get(total, Left, true) andalso maps:get(total, Right, true),
        deep => maps:get(deep, Left, false) orelse maps:get(deep, Right, false),
        pure => maps:get(pure, Left, false) andalso maps:get(pure, Right, false)
    }.

-spec is_valid_type_term(type_ref()) -> boolean().
is_valid_type_term({type_var, Name}) ->
    is_atom(Name);
is_valid_type_term(Type) when is_atom(Type) ->
    true;
is_valid_type_term(Type) when is_tuple(Type) ->
    lists:all(fun is_valid_type/1, tuple_to_list(Type));
is_valid_type_term(#{kind := effect_row} = Row) ->
    catena_row_types:is_valid_row(Row);
is_valid_type_term(#{kind := row_var}) ->
    true;
is_valid_type_term(#{kind := _} = TypeMap) ->
    lists:all(
        fun({_Key, Value}) -> is_valid_type(Value) end,
        maps:to_list(TypeMap)
    );
is_valid_type_term(TypeMap) when is_map(TypeMap) ->
    lists:all(
        fun({_Key, Value}) -> is_valid_type(Value) end,
        maps:to_list(TypeMap)
    ).
