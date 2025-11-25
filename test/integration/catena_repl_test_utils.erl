%% @doc REPL Test Utilities for Integration Tests (Phase 2.4)
%%
%% This module provides helper functions for setting up REPL states
%% for integration testing.
-module(catena_repl_test_utils).

-export([
    setup_state/0,
    setup_state_no_prelude/0,
    init_with_prelude/0
]).

%% @doc Create a REPL state with prelude loaded.
%% This is the most common setup for integration tests.
-spec setup_state() -> tuple().
setup_state() ->
    {Env, RuntimeBindings} = init_with_prelude(),
    {repl_state, Env, #{}, RuntimeBindings, [], "catena> ", ""}.

%% @doc Create a REPL state without prelude (faster for basic tests).
-spec setup_state_no_prelude() -> tuple().
setup_state_no_prelude() ->
    Env = case catena_compile:build_type_env([]) of
        {ok, E} -> E;
        _ -> catena_type_env:empty()
    end,
    {repl_state, Env, #{}, #{}, [], "catena> ", ""}.

%% @doc Initialize prelude and return {TypeEnv, RuntimeBindings}.
%% This mimics catena_repl:init_with_prelude/1 but is accessible for tests.
-spec init_with_prelude() -> {map(), map()}.
init_with_prelude() ->
    BaseEnv = case catena_compile:build_type_env([]) of
        {ok, E} -> E;
        _ -> catena_type_env:empty()
    end,

    %% Get prelude bindings from catena_prelude
    PreludeBindings = catena_prelude:prelude_bindings(),

    %% Build runtime bindings and type environment
    {RuntimeBindings, TypeEnv} = maps:fold(
        fun(Name, {Fun, Arity, TypeSig}, {RunAcc, EnvAcc}) ->
            %% Add to runtime bindings
            NewRunAcc = maps:put(Name, {Fun, Arity}, RunAcc),
            %% Add to type environment - create a scheme from the type
            Scheme = type_sig_to_scheme(TypeSig),
            NewEnvAcc = catena_type_env:extend(EnvAcc, Name, Scheme),
            {NewRunAcc, NewEnvAcc}
        end,
        {#{}, BaseEnv},
        PreludeBindings
    ),

    {TypeEnv, RuntimeBindings}.

%% Convert internal type signature to type scheme
type_sig_to_scheme({forall, Vars, Type}) ->
    FreeVars = sets:from_list([{tvar, V} || V <- Vars]),
    {scheme, FreeVars, convert_type_sig(Type)};
type_sig_to_scheme(Type) ->
    {scheme, sets:new(), convert_type_sig(Type)}.

%% Convert simplified type signature to internal type representation
convert_type_sig({tfun, Arg, Ret}) ->
    {tfun, convert_type_sig(Arg), convert_type_sig(Ret)};
convert_type_sig({tvar, V}) ->
    {tvar, V};
convert_type_sig({tcon, C}) ->
    {tcon, C};
convert_type_sig({tapp, Con, Arg}) ->
    {tapp, {tcon, Con}, convert_type_sig(Arg)};
convert_type_sig({tapp2, Con, Arg1, Arg2}) ->
    {tapp, {tapp, {tcon, Con}, convert_type_sig(Arg1)}, convert_type_sig(Arg2)};
convert_type_sig(Other) ->
    Other.
