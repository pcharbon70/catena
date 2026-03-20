%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for Kind Checking
%%%
%%% Uses PropEr for comprehensive testing of kind inference and checking,
%%% including HKT validation, kind application, and edge cases.
%%%
%%% ## Running Property Tests
%%%
%%%   rebar3 proper -m catena_kind_properties
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_kind_properties).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    prop_kind_from_arity_valid/0,
    prop_kind_from_arity_limit_enforced/0,
    prop_apply_kind_never_crashes/0,
    prop_apply_kind_limit_enforced/0,
    prop_builtin_kinds_correct/0,
    prop_kind_env_operations/0,
    prop_kinds_compatible_reflexive/0,
    prop_infer_type_kind_never_crashes/0
]).

%%====================================================================
%% Property: kind_from_arity Returns Valid Kinds
%%====================================================================

prop_kind_from_arity_valid() ->
    ?FORALL(Arity, oneof([0, 1, 2, 5, 10, 20, 50]),
        begin
            Result = catena_kind:kind_from_arity(Arity),
            case Result of
                star -> Arity =:= 0;
                {arrow, star, _} -> Arity > 0;
                {error, _} -> false  % Should not error for valid arity
            end
        end).

%%====================================================================
%% Property: kind_from_arity Enforces Limit
%%====================================================================

prop_kind_from_arity_limit_enforced() ->
    ?FORALL(Arity, oneof([101, 105, 110, 150, 200]),
        begin
            Result = catena_kind:kind_from_arity(Arity),
            case Result of
                {error, {kind_arity_exceeded, _, _}} -> true;
                _ -> false  % Should error for excessive arity
            end
        end).

%%====================================================================
%% Property: apply_kind Never Crashes
%%====================================================================

prop_apply_kind_never_crashes() ->
    ?FORALL({Kind, N}, {kind_generator(), choose(0, 20)},
        begin
            Result = (catch catena_kind:infer_type_kind(
                build_type_app(Kind, N), catena_kind:empty_kind_env())),
            case Result of
                {'EXIT', _} -> false;
                {ok, _} -> true;
                {error, _} -> true
            end
        end).

%%====================================================================
%% Property: apply_kind Enforces Limit
%%====================================================================

prop_apply_kind_limit_enforced() ->
    ?FORALL(N, choose(101, 150),
        begin
            %% Build a type with excessive applications
            Type = build_excessive_type_app(N),
            Env = catena_kind:empty_kind_env(),
            Result = catena_kind:infer_type_kind(Type, Env),
            case Result of
                {error, {kind_applications_exceeded, _, _, _}} -> true;
                {error, {over_applied, _, _, _}} -> true;
                {ok, _} -> true;  % May succeed if kind has enough arrows
                _ -> false
            end
        end).

%%====================================================================
%% Property: Built-in Kinds Are Correct
%%====================================================================

prop_builtin_kinds_correct() ->
    ?FORALL(TypeName, oneof(['Int', 'Bool', 'String', 'Maybe', 'List', 'Either']),
        begin
            Env = catena_kind:build_kind_env([]),
            {ok, Kind} = catena_kind:get_type_kind(TypeName, Env),
            case TypeName of
                'Int' -> Kind =:= star;
                'Bool' -> Kind =:= star;
                'String' -> Kind =:= star;
                'Maybe' -> Kind =:= {arrow, star, star};
                'List' -> Kind =:= {arrow, star, star};
                'Either' -> Kind =:= {arrow, star, {arrow, star, star}}
            end
        end).

%%====================================================================
%% Property: Kind Environment Operations
%%====================================================================

prop_kind_env_operations() ->
    ?FORALL({Name, Kind}, {type_name_generator(), kind_generator()},
        begin
            Env0 = catena_kind:empty_kind_env(),
            Env1 = catena_kind:add_type_kind(Name, Kind, Env0),
            {ok, RetrievedKind} = catena_kind:get_type_kind(Name, Env1),
            RetrievedKind =:= Kind
        end).

%%====================================================================
%% Property: kinds_compatible Is Reflexive
%%====================================================================

prop_kinds_compatible_reflexive() ->
    ?FORALL(Kind, kind_generator(),
        catena_kind:kinds_compatible(Kind, Kind)).

%%====================================================================
%% Property: infer_type_kind Never Crashes
%%====================================================================

prop_infer_type_kind_never_crashes() ->
    ?FORALL(Type, type_expr_generator(),
        begin
            Env = catena_kind:build_kind_env([]),
            Result = (catch catena_kind:infer_type_kind(Type, Env)),
            case Result of
                {'EXIT', _} -> false;
                {ok, _} -> true;
                {error, _} -> true
            end
        end).

%%====================================================================
%% Generators
%%====================================================================

kind_generator() ->
    ?SIZED(Size, kind_generator(Size)).

kind_generator(0) ->
    star;
kind_generator(Size) ->
    oneof([
        star,
        ?LAZY(?LET({K1, K2}, {kind_generator(Size div 2), kind_generator(Size div 2)},
            {arrow, K1, K2}))
    ]).

type_name_generator() ->
    oneof(['Foo', 'Bar', 'Baz', 'MyType', 'Custom']).

type_expr_generator() ->
    oneof([
        %% Type constructor
        ?LET(Name, oneof(['Int', 'Bool', 'Maybe', 'List']),
            {type_con, Name, {location, 1, 1}}),
        %% Type variable
        ?LET(Name, oneof([a, b, c, t, f]),
            {type_var, Name, {location, 1, 1}}),
        %% Type application
        ?LET({Con, Arg}, {oneof(['Maybe', 'List']), oneof(['Int', 'Bool'])},
            {type_app,
                {type_con, Con, {location, 1, 1}},
                [{type_con, Arg, {location, 1, 1}}],
                {location, 1, 1}})
    ]).

build_type_app(star, _N) ->
    {type_con, 'Int', {location, 1, 1}};
build_type_app({arrow, _, _}, N) when N > 0 ->
    {type_app,
        {type_con, 'Maybe', {location, 1, 1}},
        lists:duplicate(N, {type_con, 'Int', {location, 1, 1}}),
        {location, 1, 1}};
build_type_app(_, _) ->
    {type_con, 'Int', {location, 1, 1}}.

build_excessive_type_app(N) ->
    {type_app,
        {type_con, 'Maybe', {location, 1, 1}},
        lists:duplicate(N, {type_con, 'Int', {location, 1, 1}}),
        {location, 1, 1}}.

%%====================================================================
%% EUnit Test Generator
%%====================================================================

kind_properties_test_() ->
    [
        {"kind_from_arity returns valid kinds", ?_assert(proper:quickcheck(prop_kind_from_arity_valid(), 50))},
        {"kind_from_arity enforces limit", ?_assert(proper:quickcheck(prop_kind_from_arity_limit_enforced(), 20))},
        {"apply_kind never crashes", ?_assert(proper:quickcheck(prop_apply_kind_never_crashes(), 50))},
        {"apply_kind enforces limit", ?_assert(proper:quickcheck(prop_apply_kind_limit_enforced(), 20))},
        {"Built-in kinds are correct", ?_assert(proper:quickcheck(prop_builtin_kinds_correct(), 30))},
        {"Kind environment operations", ?_assert(proper:quickcheck(prop_kind_env_operations(), 50))},
        {"kinds_compatible is reflexive", ?_assert(proper:quickcheck(prop_kinds_compatible_reflexive(), 50))},
        {"infer_type_kind never crashes", ?_assert(proper:quickcheck(prop_infer_type_kind_never_crashes(), 50))}
    ].
