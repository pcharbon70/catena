%%%-------------------------------------------------------------------
%%% @doc Catena Higher-Order Effect Types (Phase 13.2)
%%%
%%% Higher-order operations are operations whose parameters include
%%% effectful functions. This module models those parameters explicitly
%%% and reuses the real row-polymorphic effect rows introduced by
%%% Phase 11.
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

-type effect_row() :: catena_row_types:effect_row().
-type type_ref() :: atom() | tuple() | map() | [type_ref()].

-record(ho_op_type, {
    name :: atom() | undefined,
    params :: [param_type()],
    result :: type_ref(),
    effects :: effect_row(),
    scope_id :: non_neg_integer() | undefined
}).

-type ho_op_type() :: #ho_op_type{}.

-record(effectful_param, {
    input :: type_ref(),
    output :: type_ref(),
    effects :: effect_row()
}).

-type effectful_param() :: #effectful_param{}.
-type param_type() :: type_ref() | effectful_param().

-type ho_constraints() :: #{
    effectful_params => non_neg_integer(),
    max_nesting => non_neg_integer(),
    allow_impredicative => boolean()
}.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Type Constructors
%%%---------------------------------------------------------------------

-spec ho_op_type() -> ho_op_type().
ho_op_type() ->
    #ho_op_type{
        name = undefined,
        params = [],
        result = catena_types:tcon('T'),
        effects = catena_row_types:empty_row(),
        scope_id = undefined
    }.

-spec ho_op_type(atom()) -> ho_op_type().
ho_op_type(Name) when is_atom(Name) ->
    (ho_op_type())#ho_op_type{name = Name}.

-spec ho_op_type(atom(), [param_type()], type_ref()) -> ho_op_type().
ho_op_type(Name, Params, Result) when is_atom(Name), is_list(Params) ->
    (ho_op_type(Name))#ho_op_type{
        params = Params,
        result = Result
    }.

-spec ho_op_type(atom(), [param_type()], type_ref(), effect_row()) -> ho_op_type().
ho_op_type(Name, Params, Result, Effects) when is_atom(Name), is_list(Params) ->
    (ho_op_type(Name, Params, Result))#ho_op_type{
        effects = normalize_effect_row(Effects)
    }.

%%%---------------------------------------------------------------------
%%% Effectful Parameter Types
%%%---------------------------------------------------------------------

-spec effectful_param() -> effectful_param().
effectful_param() ->
    #effectful_param{
        input = catena_types:tcon('A'),
        output = catena_types:tcon('B'),
        effects = catena_row_types:empty_row()
    }.

-spec effectful_param(type_ref(), type_ref()) -> effectful_param().
effectful_param(Input, Output) ->
    #effectful_param{
        input = Input,
        output = Output,
        effects = catena_row_types:empty_row()
    }.

-spec effectful_param(type_ref(), type_ref(), effect_row()) -> effectful_param().
effectful_param(Input, Output, Effects) ->
    #effectful_param{
        input = Input,
        output = Output,
        effects = normalize_effect_row(Effects)
    }.

-spec is_effectful_param(term()) -> boolean().
is_effectful_param(#effectful_param{}) ->
    true;
is_effectful_param(_) ->
    false.

-spec param_input_type(effectful_param()) -> type_ref().
param_input_type(#effectful_param{input = Input}) ->
    Input.

-spec param_output_type(effectful_param()) -> type_ref().
param_output_type(#effectful_param{output = Output}) ->
    Output.

-spec param_effects(effectful_param()) -> effect_row().
param_effects(#effectful_param{effects = Effects}) ->
    Effects.

%%%---------------------------------------------------------------------
%%% Higher-Order Operation Validation
%%%---------------------------------------------------------------------

-spec is_ho_op(term()) -> boolean().
is_ho_op(#ho_op_type{}) ->
    true;
is_ho_op(_) ->
    false.

-spec is_ho_param_type(term()) -> boolean().
is_ho_param_type(Term) ->
    is_effectful_param(Term).

-spec count_effectful_params(ho_op_type()) -> non_neg_integer().
count_effectful_params(#ho_op_type{params = Params}) ->
    length([Param || Param <- Params, is_effectful_param(Param)]).

-spec get_effectful_params(ho_op_type()) -> [effectful_param()].
get_effectful_params(#ho_op_type{params = Params}) ->
    [Param || Param <- Params, is_effectful_param(Param)].

%%%---------------------------------------------------------------------
%%% Effectful Parameter Type Inference
%%%---------------------------------------------------------------------

-spec infer_param_type(term(), ho_constraints()) -> param_type().
infer_param_type(#effectful_param{} = Param, _Constraints) ->
    Param;
infer_param_type({effectful_fun, Input, Output}, Constraints) ->
    effectful_param(Input, Output, inferred_effects(Constraints));
infer_param_type({effectful_fun, Input, Output, Effects}, _Constraints) ->
    effectful_param(Input, Output, Effects);
infer_param_type(Term, Constraints) when is_function(Term) ->
    infer_function_param(Term, Constraints);
infer_param_type(Term, _Constraints) ->
    Term.

-spec infer_param_effects(param_type(), ho_constraints()) -> effect_row().
infer_param_effects(#effectful_param{effects = Effects}, _Constraints) ->
    normalize_effect_row(Effects);
infer_param_effects({effectful_fun, _, _, Effects}, _Constraints) ->
    normalize_effect_row(Effects);
infer_param_effects(_SimpleParam, Constraints) ->
    inferred_effects(Constraints).

-spec infer_all_param_types([term()]) -> [param_type()].
infer_all_param_types(Terms) when is_list(Terms) ->
    Constraints = #{
        effectful_params => 0,
        max_nesting => 2,
        allow_impredicative => false
    },
    [infer_param_type(Term, Constraints) || Term <- Terms].

-spec unify_param_types(param_type(), param_type()) -> {ok, param_type()} | {error, term()}.
unify_param_types(#effectful_param{} = Param1, #effectful_param{} = Param2) ->
    unify_effectful_params(Param1, Param2);
unify_param_types(Term, Term) ->
    {ok, Term};
unify_param_types(Term1, Term2) ->
    {error, {type_mismatch, Term1, Term2}}.

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Substitution
%%%---------------------------------------------------------------------

-spec substitute_param_effects(effectful_param(), effect_row()) -> effectful_param().
substitute_param_effects(#effectful_param{effects = Effects} = Param, Substitution) ->
    Param#effectful_param{effects = instantiate_row(Effects, Substitution)}.

-spec substitute_ho_effects(ho_op_type(), effect_row()) -> ho_op_type().
substitute_ho_effects(#ho_op_type{params = Params, effects = Effects} = HO, Substitution) ->
    HO#ho_op_type{
        params = [substitute_param(Param, Substitution) || Param <- Params],
        effects = instantiate_row(Effects, Substitution)
    }.

-spec instantiate_ho_type(ho_op_type(), effect_row()) -> ho_op_type().
instantiate_ho_type(HO, Instantiation) ->
    substitute_ho_effects(HO, Instantiation).

-spec generalize_ho_type(ho_op_type()) -> ho_op_type().
generalize_ho_type(#ho_op_type{params = Params, effects = Effects} = HO) ->
    HO#ho_op_type{
        params = [generalize_param(Param) || Param <- Params],
        effects = generalize_row(Effects)
    }.

%%%---------------------------------------------------------------------
%%% Higher-Order Effect Operations
%%%---------------------------------------------------------------------

-spec compose_ho_types(ho_op_type(), ho_op_type()) -> {ok, ho_op_type()} | {error, term()}.
compose_ho_types(#ho_op_type{result = Result1, effects = Effects1},
                 #ho_op_type{name = Name2, params = Params2, result = Result2, effects = Effects2}) ->
    case Params2 of
        [] ->
            {ok, #ho_op_type{
                name = Name2,
                params = [],
                result = Result2,
                effects = merge_effect_rows(Effects1, Effects2),
                scope_id = undefined
            }};
        [First | Rest] ->
            case first_param_matches(Result1, First) of
                true ->
                    {ok, #ho_op_type{
                        name = Name2,
                        params = Rest,
                        result = Result2,
                        effects = merge_effect_rows(Effects1, Effects2),
                        scope_id = undefined
                    }};
                false ->
                    {error, {type_mismatch, Result1, First}}
            end
    end.

-spec ho_type_eq(ho_op_type(), ho_op_type()) -> boolean().
ho_type_eq(#ho_op_type{params = Params1, result = Result1, effects = Effects1},
           #ho_op_type{params = Params2, result = Result2, effects = Effects2}) ->
    param_types_eq(Params1, Params2) andalso
    types_equal(Result1, Result2) andalso
    effect_rows_equal(Effects1, Effects2);
ho_type_eq(_, _) ->
    false.

-spec merge_effect_rows(effect_row(), effect_row()) -> effect_row().
merge_effect_rows(Row1, Row2) ->
    catena_row_types:row_union(normalize_effect_row(Row1), normalize_effect_row(Row2)).

%%%---------------------------------------------------------------------
%%% Pretty Printing
%%%---------------------------------------------------------------------

-spec format_ho_type(ho_op_type()) -> binary().
format_ho_type(#ho_op_type{name = Name, params = Params, result = Result, effects = Effects}) ->
    NamePart =
        case Name of
            undefined -> <<"ho_op">>;
            _ -> list_to_binary(atom_to_list(Name))
        end,
    ParamsPart = format_param_list(Params),
    ResultPart = format_type(Result),
    EffectsPart = format_effect_row(Effects),
    <<NamePart/binary, ": ", ParamsPart/binary, " -> ", ResultPart/binary, " / ", EffectsPart/binary>>.

-spec format_effectful_param(effectful_param()) -> binary().
format_effectful_param(#effectful_param{input = Input, output = Output, effects = Effects}) ->
    InputPart = format_type(Input),
    OutputPart = format_type(Output),
    EffectsPart = format_effect_row(Effects),
    <<"(", InputPart/binary, " -> ", OutputPart/binary, " / ", EffectsPart/binary, ")">>.

%%%---------------------------------------------------------------------
%%% Internal Helpers
%%%---------------------------------------------------------------------

-spec normalize_effect_row(effect_row()) -> effect_row().
normalize_effect_row(Row) ->
    catena_row_types:row_normalize(Row).

-spec inferred_effects(ho_constraints()) -> effect_row().
inferred_effects(Constraints) ->
    case maps:get(allow_impredicative, Constraints, false) of
        true ->
            catena_row_types:effect_row([], catena_op_signatures:fresh_row_var());
        false ->
            catena_row_types:empty_row()
    end.

-spec infer_function_param(function(), ho_constraints()) -> param_type().
infer_function_param(Fun, Constraints) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 1} ->
            effectful_param(
                {type_var, fun_input},
                {type_var, fun_output},
                inferred_effects(Constraints)
            );
        _ ->
            {function, opaque}
    end.

-spec unify_effectful_params(effectful_param(), effectful_param()) ->
    {ok, effectful_param()} | {error, term()}.
unify_effectful_params(#effectful_param{input = Input1, output = Output1, effects = Effects1},
                       #effectful_param{input = Input2, output = Output2, effects = Effects2}) ->
    case types_equal(Input1, Input2) andalso types_equal(Output1, Output2) of
        true ->
            {ok, #effectful_param{
                input = Input1,
                output = Output1,
                effects = merge_effect_rows(Effects1, Effects2)
            }};
        false ->
            {error, {type_mismatch, {Input1, Output1}, {Input2, Output2}}}
    end.

-spec substitute_param(param_type(), effect_row()) -> param_type().
substitute_param(#effectful_param{} = Param, Substitution) ->
    substitute_param_effects(Param, Substitution);
substitute_param(SimpleParam, _Substitution) ->
    SimpleParam.

-spec instantiate_row(effect_row(), effect_row()) -> effect_row().
instantiate_row(Effects, Substitution) ->
    NormalizedEffects = normalize_effect_row(Effects),
    NormalizedSubstitution = normalize_effect_row(Substitution),
    case maps:get(row_var, NormalizedEffects, undefined) of
        undefined ->
            catena_row_types:row_union(NormalizedEffects, NormalizedSubstitution);
        RowVar ->
            catena_op_signatures:sig_effects(
                catena_op_signatures:substitute_row_var(
                    catena_op_signatures:op_sig([], unit, NormalizedEffects),
                    RowVar,
                    NormalizedSubstitution
                )
            )
    end.

-spec generalize_param(param_type()) -> param_type().
generalize_param(#effectful_param{effects = Effects} = Param) ->
    Param#effectful_param{effects = generalize_row(Effects)};
generalize_param(SimpleParam) ->
    SimpleParam.

-spec generalize_row(effect_row()) -> effect_row().
generalize_row(Row) ->
    Normalized = normalize_effect_row(Row),
    case maps:get(row_var, Normalized, undefined) of
        undefined ->
            Normalized#{row_var => catena_op_signatures:fresh_row_var()};
        _ ->
            Normalized
    end.

-spec first_param_matches(type_ref(), param_type()) -> boolean().
first_param_matches(Result, #effectful_param{input = Input}) ->
    types_equal(Result, Input);
first_param_matches(Result, Param) ->
    types_equal(Result, Param).

-spec param_types_eq([param_type()], [param_type()]) -> boolean().
param_types_eq([], []) ->
    true;
param_types_eq([Param1 | Rest1], [Param2 | Rest2]) ->
    params_eq(Param1, Param2) andalso param_types_eq(Rest1, Rest2);
param_types_eq(_, _) ->
    false.

-spec params_eq(param_type(), param_type()) -> boolean().
params_eq(#effectful_param{} = Param1, #effectful_param{} = Param2) ->
    case unify_effectful_params(Param1, Param2) of
        {ok, _} -> true;
        _ -> false
    end;
params_eq(Param1, Param2) ->
    types_equal(Param1, Param2).

-spec types_equal(type_ref(), type_ref()) -> boolean().
types_equal(Type1, Type2) when is_map(Type1), is_map(Type2) ->
    maps:size(Type1) =:= maps:size(Type2) andalso
    lists:all(
        fun({Key, Value1}) ->
            case maps:find(Key, Type2) of
                {ok, Value2} -> types_equal(Value1, Value2);
                error -> false
            end
        end,
        maps:to_list(Type1)
    );
types_equal(Type1, Type2) when is_tuple(Type1), is_tuple(Type2) ->
    tuple_size(Type1) =:= tuple_size(Type2) andalso
    lists:all(
        fun({Value1, Value2}) -> types_equal(Value1, Value2) end,
        lists:zip(tuple_to_list(Type1), tuple_to_list(Type2))
    );
types_equal(Type1, Type2) when is_list(Type1), is_list(Type2) ->
    length(Type1) =:= length(Type2) andalso
    lists:all(
        fun({Value1, Value2}) -> types_equal(Value1, Value2) end,
        lists:zip(Type1, Type2)
    );
types_equal(Type1, Type2) ->
    Type1 =:= Type2.

-spec effect_rows_equal(effect_row(), effect_row()) -> boolean().
effect_rows_equal(Row1, Row2) ->
    catena_op_signatures:sig_eq(
        catena_op_signatures:op_sig([], unit, normalize_effect_row(Row1)),
        catena_op_signatures:op_sig([], unit, normalize_effect_row(Row2))
    ).

-spec format_param_list([param_type()]) -> binary().
format_param_list([]) ->
    <<"()">>;
format_param_list(Params) ->
    iolist_to_binary(["(", lists:join(<<", ">>, [format_param(Param) || Param <- Params]), ")"]).

-spec format_param(param_type()) -> binary().
format_param(#effectful_param{} = Param) ->
    format_effectful_param(Param);
format_param(Param) ->
    format_type(Param).

-spec format_type(type_ref()) -> binary().
format_type(Type) when is_atom(Type) ->
    list_to_binary(atom_to_list(Type));
format_type(Type) when is_map(Type) ->
    case maps:get(kind, Type, undefined) of
        effect_row -> format_effect_row(Type);
        row_var -> catena_op_signatures:format_row_var(Type);
        undefined -> <<"{}">>;
        Kind -> <<(list_to_binary(atom_to_list(Kind)))/binary, "{}">>
    end;
format_type(Type) when is_tuple(Type) ->
    iolist_to_binary(["(", lists:join(<<", ">>, [format_type(Element) || Element <- tuple_to_list(Type)]), ")"]);
format_type(Type) when is_list(Type) ->
    iolist_to_binary(["[", lists:join(<<", ">>, [format_type(Element) || Element <- Type]), "]"]);
format_type(_) ->
    <<"?">>.

-spec format_effect_row(effect_row()) -> binary().
format_effect_row(Row) ->
    Normalized = normalize_effect_row(Row),
    Elements = maps:get(elements, Normalized, []),
    RowVar = maps:get(row_var, Normalized, undefined),
    ElementsPart =
        case Elements of
            [] -> <<"">>;
            _ -> iolist_to_binary(lists:join(<<", ">>, [list_to_binary(atom_to_list(Element)) || Element <- Elements]))
        end,
    RowVarPart =
        case RowVar of
            undefined -> <<"">>;
            _ -> catena_op_signatures:format_row_var(RowVar)
        end,
    case {ElementsPart, RowVarPart} of
        {<<>>, <<>>} -> <<"{}">>;
        {<<>>, _} -> <<"{", RowVarPart/binary, "}">>;
        {_, <<>>} -> <<"{", ElementsPart/binary, "}">>;
        {_, _} -> <<"{", ElementsPart/binary, " | ", RowVarPart/binary, "}">>
    end.
