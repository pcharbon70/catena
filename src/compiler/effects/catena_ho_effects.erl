%%%-------------------------------------------------------------------
%%% @doc Catena Higher-Order Effect Types (Phase 13.2)
%%%
%%% This module implements types for higher-order effects (operations
%%% with effectful parameters). Higher-order operations take effectful
%%% functions as arguments, enabling operations like `iterate` that can
%%% invoke effectful callbacks, or `catch` that can handle effectful
%%% error handlers.
%%%
%%% == Higher-Order Operations ==
%%%
%%% A higher-order operation has at least one parameter that is an
%%% effectful function type: `(a -> b / ε)`. The effect row `ε` represents
%%% the effects that the parameterized function may perform.
%%%
%%% Example: iterate : (Int -> (Unit -> Unit / ε) -> Unit / {Iter | ε})
%%%
%%% == Effectful Parameter Type Inference ==
%%%
%%% When inferring types for higher-order operations, we need to:
%%% 1. Track which parameters are effectful functions
%%% 2. Infer the effect row for each effectful parameter
%%% 3. Ensure effect variables are properly scoped
%%% 4. Handle nested effectful parameters
%%%
%%% == Higher-Order Effect Substitution ==
%%%
%%% Effect variables in higher-order operation types can be substituted
%%% with concrete effect sets, enabling polymorphism over effects. The
%%% substitution must be applied consistently to all effectful parameters
%%% and the operation's own effect row.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_ho_effects).

%% Higher-order operation type constructors
-export([
    ho_op_type/0,
    ho_op_type/1,
    ho_op_type/3,
    ho_op_type/4
]).

%% Effectful parameter types
-export([
    effectful_param/0,
    effectful_param/2,
    effectful_param/3,
    is_effectful_param/1,
    param_input_type/1,
    param_output_type/1,
    param_effects/1
]).

%% Higher-order operation validation
-export([
    is_ho_op/1,
    is_ho_param_type/1,
    count_effectful_params/1,
    get_effectful_params/1
]).

%% Effectful parameter type inference
-export([
    infer_param_type/2,
    infer_param_effects/2,
    infer_all_param_types/1,
    unify_param_types/2
]).

%% Higher-order effect substitution
-export([
    substitute_param_effects/2,
    substitute_ho_effects/2,
    instantiate_ho_type/2,
    generalize_ho_type/1
]).

%% Higher-order effect operations
-export([
    compose_ho_types/2,
    ho_type_eq/2,
    merge_effect_rows/2
]).

%% Pretty printing
-export([
    format_ho_type/1,
    format_effectful_param/1
]).

-export_type([
    ho_op_type/0,
    effectful_param/0,
    param_type/0,
    ho_constraints/0
]).

%%%---------------------------------------------------------------------
%%% Types
%%%---------------------------------------------------------------------

%% @doc Higher-order operation type.
-record(ho_op_type, {
    name :: atom() | undefined,
    params :: [param_type()],
    result :: term(),
    effects :: map(),
    scope_id :: non_neg_integer() | undefined
}).

-type ho_op_type() :: #ho_op_type{}.

%% @doc Effectful parameter type - a function with effects.
-record(effectful_param, {
    input :: term(),
    output :: term(),
    effects :: map()
}).

-type effectful_param() :: #effectful_param{}.

%% @doc Parameter type - either simple type or effectful function.
-type param_type() :: term() | effectful_param().

%% @doc Higher-order type constraints.
-type ho_constraints() :: #{
    effectful_params => non_neg_integer(),
    max_nesting => non_neg_integer(),
    allow_impredicative => boolean()
}.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Type Constructors
%%%---------------------------------------------------------------------

%% @doc Create an empty higher-order operation type.
-spec ho_op_type() -> ho_op_type().
ho_op_type() ->
    #ho_op_type{
        name = undefined,
        params = [],
        result = catena_types:tcon('T'),
        effects = empty_effect_row(),
        scope_id = undefined
    }.

%% @doc Create a higher-order operation type with a name.
-spec ho_op_type(atom()) -> ho_op_type().
ho_op_type(Name) when is_atom(Name) ->
    #ho_op_type{
        name = Name,
        params = [],
        result = catena_types:tcon('T'),
        effects = empty_effect_row(),
        scope_id = undefined
    }.

%% @doc Create a higher-order operation type with params and result.
-spec ho_op_type(atom(), [param_type()], term()) -> ho_op_type().
ho_op_type(Name, Params, Result) when is_atom(Name), is_list(Params) ->
    #ho_op_type{
        name = Name,
        params = Params,
        result = Result,
        effects = empty_effect_row(),
        scope_id = undefined
    }.

%% @doc Create a complete higher-order operation type.
-spec ho_op_type(atom(), [param_type()], term(), map()) -> ho_op_type().
ho_op_type(Name, Params, Result, Effects) when is_atom(Name), is_list(Params) ->
    #ho_op_type{
        name = Name,
        params = Params,
        result = Result,
        effects = Effects,
        scope_id = undefined
    }.

%%%---------------------------------------------------------------------
%%% Effectful Parameter Types
%%%---------------------------------------------------------------------

%% @doc Create an empty effectful parameter type.
-spec effectful_param() -> effectful_param().
effectful_param() ->
    #effectful_param{
        input = catena_types:tcon('A'),
        output = catena_types:tcon('B'),
        effects = empty_effect_row()
    }.

%% @doc Create an effectful parameter with input/output types.
-spec effectful_param(term(), term()) -> effectful_param().
effectful_param(Input, Output) ->
    #effectful_param{
        input = Input,
        output = Output,
        effects = empty_effect_row()
    }.

%% @doc Create a complete effectful parameter with effects.
-spec effectful_param(term(), term(), map()) -> effectful_param().
effectful_param(Input, Output, Effects) ->
    #effectful_param{
        input = Input,
        output = Output,
        effects = Effects
    }.

%% @doc Check if a parameter type is effectful.
-spec is_effectful_param(term()) -> boolean().
is_effectful_param(#effectful_param{}) -> true;
is_effectful_param(_) -> false.

%% @doc Get the input type of an effectful parameter.
-spec param_input_type(effectful_param()) -> term().
param_input_type(#effectful_param{input = Input}) -> Input.

%% @doc Get the output type of an effectful parameter.
-spec param_output_type(effectful_param()) -> term().
param_output_type(#effectful_param{output = Output}) -> Output.

%% @doc Get the effects of an effectful parameter.
-spec param_effects(effectful_param()) -> map().
param_effects(#effectful_param{effects = Effects}) -> Effects.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Validation
%%%---------------------------------------------------------------------

%% @doc Check if a term is a higher-order operation type.
-spec is_ho_op(term()) -> boolean().
is_ho_op(#ho_op_type{}) -> true;
is_ho_op(_) -> false.

%% @doc Check if a parameter type is a higher-order type.
-spec is_ho_param_type(term()) -> boolean().
is_ho_param_type(#effectful_param{}) -> true;
is_ho_param_type(_) -> false.

%% @doc Count the number of effectful parameters in an operation.
-spec count_effectful_params(ho_op_type()) -> non_neg_integer().
count_effectful_params(#ho_op_type{params = Params}) ->
    lists:foldl(fun
        (#effectful_param{}, Acc) -> Acc + 1;
        (_, Acc) -> Acc
    end, 0, Params).

%% @doc Get all effectful parameters from an operation.
-spec get_effectful_params(ho_op_type()) -> [effectful_param()].
get_effectful_params(#ho_op_type{params = Params}) ->
    lists:filter(fun is_effectful_param/1, Params).

%%%---------------------------------------------------------------------
%%% Effectful Parameter Type Inference
%%%---------------------------------------------------------------------

%% @doc Infer the type of a parameter based on context.
-spec infer_param_type(term(), ho_constraints()) -> param_type().
infer_param_type(Term, Constraints) ->
    case detect_function_type(Term) of
        {ok, {Input, Output}} ->
            infer_effectful_param(Input, Output, Constraints);
        error ->
            Term
    end.

%% @private
detect_function_type(Term) when is_function(Term) ->
    {arity, Arity} = erlang:fun_info(Term, arity),
    case Arity of
        1 -> {ok, {catena_types:tcon('A'), catena_types:tcon('B')}};
        _ -> error
    end;
detect_function_type(_) ->
    error.

%% @private
infer_effectful_param(Input, Output, Constraints) ->
    Effects = case maps:get(allow_impredicative, Constraints, false) of
        true ->
            %% Allow polymorphic effects
            #{kind => effect_row, elements => [], row_var => catena_op_signatures:fresh_row_var()};
        false ->
            empty_effect_row()
    end,
    #effectful_param{
        input = Input,
        output = Output,
        effects = Effects
    }.

%% @doc Infer the effects of a parameter based on usage.
-spec infer_param_effects(param_type(), ho_constraints()) -> map().
infer_param_effects(#effectful_param{effects = Effects}, _Constraints) ->
    Effects;
infer_param_effects(_SimpleParam, Constraints) ->
    case maps:get(allow_impredicative, Constraints, false) of
        true ->
            #{kind => effect_row, elements => [], row_var => catena_op_signatures:fresh_row_var()};
        false ->
            empty_effect_row()
    end.

%% @doc Infer types for all parameters of an operation.
-spec infer_all_param_types([term()]) -> [param_type()].
infer_all_param_types(Terms) when is_list(Terms) ->
    Constraints = #{effectful_params => 0, max_nesting => 2, allow_impredicative => false},
    [infer_param_type(T, Constraints) || T <- Terms].

%% @doc Unify two parameter types.
-spec unify_param_types(param_type(), param_type()) -> {ok, param_type()} | {error, term()}.
unify_param_types(#effectful_param{input = I1, output = O1, effects = E1},
                  #effectful_param{input = I2, output = O2, effects = E2}) ->
    case {types_match(I1, I2), types_match(O1, O2)} of
        {true, true} ->
            MergedEffects = merge_effect_rows(E1, E2),
            {ok, #effectful_param{input = I1, output = O1, effects = MergedEffects}};
        _ ->
            {error, type_mismatch}
    end;
unify_param_types(T1, T2) when T1 =:= T2 ->
    {ok, T1};
unify_param_types(T1, T2) ->
    {error, {type_mismatch, T1, T2}}.

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Substitution
%%%---------------------------------------------------------------------

%% @doc Substitute effects in an effectful parameter.
-spec substitute_param_effects(effectful_param(), map()) -> effectful_param().
substitute_param_effects(#effectful_param{effects = Effects} = Param, Substitution) ->
    Param#effectful_param{effects = apply_substitution(Effects, Substitution)}.

%% @private
apply_substitution(Effects, Substitution) ->
    maps:fold(fun(Key, Value, Acc) ->
        maps:put(Key, Value, Acc)
    end, Effects, Substitution).

%% @doc Substitute effects in a higher-order operation type.
-spec substitute_ho_effects(ho_op_type(), map()) -> ho_op_type().
substitute_ho_effects(#ho_op_type{params = Params, effects = Effects} = HO, Substitution) ->
    NewParams = [substitute_in_param(P, Substitution) || P <- Params],
    NewEffects = apply_substitution(Effects, Substitution),
    HO#ho_op_type{params = NewParams, effects = NewEffects}.

%% @private
substitute_in_param(#effectful_param{effects = ParamEffects} = Param, Substitution) ->
    Param#effectful_param{effects = apply_substitution(ParamEffects, Substitution)};
substitute_in_param(SimpleParam, _Substitution) ->
    SimpleParam.

%% @doc Instantiate a higher-order type with concrete effects.
-spec instantiate_ho_type(ho_op_type(), map()) -> ho_op_type().
instantiate_ho_type(HO, Instantiation) ->
    substitute_ho_effects(HO, Instantiation).

%% @doc Generalize a higher-order type by introducing row variables.
-spec generalize_ho_type(ho_op_type()) -> ho_op_type().
generalize_ho_type(#ho_op_type{params = Params, effects = Effects} = HO) ->
    NewParams = [generalize_param(P) || P <- Params],
    NewEffects = generalize_effects(Effects),
    HO#ho_op_type{params = NewParams, effects = NewEffects}.

%% @private
generalize_param(#effectful_param{effects = ParamEffects} = Param) ->
    Param#effectful_param{effects = generalize_effects(ParamEffects)};
generalize_param(SimpleParam) ->
    SimpleParam.

%% @private
generalize_effects(Effects) ->
    case maps:get(row_var, Effects, undefined) of
        undefined ->
            Effects#{row_var => catena_op_signatures:fresh_row_var()};
        _ ->
            Effects
    end.

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Operations
%%%---------------------------------------------------------------------

%% @doc Compose two higher-order types.
%% The result type of the first must match a parameter type of the second.
-spec compose_ho_types(ho_op_type(), ho_op_type()) -> {ok, ho_op_type()} | {error, term()}.
compose_ho_types(#ho_op_type{result = Res1, params = Params1, effects = Eff1},
                 #ho_op_type{params = Params2, effects = Eff2} = HO2) ->
    %% Simplified composition: combine parameters and effects
    %% In a full implementation, we'd check for proper type matching
    MergedEffects = merge_effect_rows(Eff1, Eff2),
    MergedParams = Params1 ++ Params2,
    {ok, HO2#ho_op_type{
        params = MergedParams,
        effects = MergedEffects
    }};
compose_ho_types(_, _) ->
    {error, invalid_ho_types}.

%% @doc Check if two higher-order types are equal.
-spec ho_type_eq(ho_op_type(), ho_op_type()) -> boolean().
ho_type_eq(#ho_op_type{params = P1, result = R1, effects = E1},
            #ho_op_type{params = P2, result = R2, effects = E2}) ->
    param_types_eq(P1, P2) andalso
    types_match(R1, R2) andalso
    effect_rows_eq(E1, E2);
ho_type_eq(_, _) ->
    false.

%% @private
param_types_eq([], []) -> true;
param_types_eq([H1 | T1], [H2 | T2]) ->
    case params_eq(H1, H2) of
        true -> param_types_eq(T1, T2);
        false -> false
    end;
param_types_eq(_, _) -> false.

%% @private
params_eq(#effectful_param{input = I1, output = O1, effects = E1},
           #effectful_param{input = I2, output = O2, effects = E2}) ->
    types_match(I1, I2) andalso types_match(O1, O2) andalso effect_rows_eq(E1, E2);
params_eq(T1, T2) when T1 =:= T2 -> true;
params_eq(_, _) -> false.

%% @private
types_match(T1, T2) when is_atom(T1), is_atom(T2) -> T1 =:= T2;
types_match(T1, T2) when is_map(T1), is_map(T2) -> maps:size(T1) =:= maps:size(T2);
types_match(_, _) -> false.

%% @private
effect_rows_eq(E1, E2) ->
    maps:get(kind, E1, undefined) =:= maps:get(kind, E2, undefined) andalso
    maps:get(elements, E1, []) =:= maps:get(elements, E2, []).

%% @doc Merge two effect rows.
-spec merge_effect_rows(map(), map()) -> map().
merge_effect_rows(#{kind := effect_row, elements := E1, row_var := RV1},
                  #{kind := effect_row, elements := E2, row_var := RV2}) ->
    MergedElements = lists:usort(E1 ++ E2),
    MergedRowVar = case {RV1, RV2} of
        {undefined, undefined} -> undefined;
        {undefined, _} -> RV2;
        {_, undefined} -> RV1;
        _ -> RV1  % Keep first if both present
    end,
    #{
        kind => effect_row,
        elements => MergedElements,
        row_var => MergedRowVar
    };
merge_effect_rows(E1, _) ->
    E1.

%%%---------------------------------------------------------------------
%%% Pretty Printing
%%%---------------------------------------------------------------------

%% @doc Format a higher-order operation type.
-spec format_ho_type(ho_op_type()) -> binary().
format_ho_type(#ho_op_type{name = Name, params = Params, result = Result, effects = Effects}) ->
    NamePart = case Name of
        undefined -> <<"ho_op">>;
        _ -> list_to_binary(atom_to_list(Name))
    end,
    ParamsPart = format_param_list(Params),
    ResultPart = format_type(Result),
    EffectsPart = format_effect_row(Effects),
    <<NamePart/binary, ": ", ParamsPart/binary, " -> ", ResultPart/binary, " / ", EffectsPart/binary>>.

%% @doc Format an effectful parameter type.
-spec format_effectful_param(effectful_param()) -> binary().
format_effectful_param(#effectful_param{input = Input, output = Output, effects = Effects}) ->
    InputPart = format_type(Input),
    OutputPart = format_type(Output),
    EffectsPart = format_effect_row(Effects),
    <<"(", InputPart/binary, " -> ", OutputPart/binary, " / ", EffectsPart/binary, ")">>.

%% @private
format_param_list(Params) ->
    Formatted = [format_param(P) || P <- Params],
    case Formatted of
        [] -> <<"()">>;
        _ -> list_to_binary(["(" | [lists:join(<<", ">>, Formatted), ")"]])
    end.

%% @private
format_param(#effectful_param{} = EP) ->
    format_effectful_param(EP);
format_param(Simple) ->
    format_type(Simple).

%% @private
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

%% @private
format_effect_row(#{kind := effect_row, elements := Es, row_var := RV}) ->
    ElementsPart = case Es of
        [] -> <<"">>;
        _ -> format_effect_list(Es)
    end,
    RowVarPart = case RV of
        undefined -> <<"">>;
        _ -> catena_op_signatures:format_row_var(RV)
    end,
    case {ElementsPart, RowVarPart} of
        {<<>>, <<>>} -> <<"{}">>;
        {<<>>, _} -> <<"{", RowVarPart/binary, "}">>;
        {_, <<>>} -> <<"{", ElementsPart/binary, "}">>;
        {_, _} -> <<"{", ElementsPart/binary, " | ", RowVarPart/binary, "}">>
    end.

%% @private
format_effect_list(Effects) ->
    Formatted = [list_to_binary(atom_to_list(E)) || E <- Effects],
    lists:foldl(fun(E, Acc) ->
        case Acc of
            <<>> -> E;
            _ -> <<Acc/binary, ", ", E/binary>>
        end
    end, <<>>, Formatted).

%% @private
empty_effect_row() ->
    #{
        kind => effect_row,
        elements => [],
        row_var => undefined
    }.
