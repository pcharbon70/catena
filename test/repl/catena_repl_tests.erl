%% @doc Tests for the Catena REPL
-module(catena_repl_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Input Parsing Tests
%%====================================================================

is_command_test_() ->
    [
        ?_assert(catena_repl:is_command(":quit")),
        ?_assert(catena_repl:is_command(":type x")),
        ?_assert(catena_repl:is_command(":load file.catena")),
        ?_assertNot(catena_repl:is_command("transform id x = x")),
        ?_assertNot(catena_repl:is_command("42")),
        ?_assertNot(catena_repl:is_command(""))
    ].

is_complete_test_() ->
    [
        ?_assert(catena_repl:is_complete("")),
        ?_assert(catena_repl:is_complete("42")),
        ?_assert(catena_repl:is_complete("transform id x = x")),
        ?_assert(catena_repl:is_complete("(1 + 2)")),
        ?_assert(catena_repl:is_complete("{x: 1, y: 2}")),
        ?_assert(catena_repl:is_complete("[1, 2, 3]")),
        %% Incomplete inputs
        ?_assertNot(catena_repl:is_complete("(1 + 2")),
        ?_assertNot(catena_repl:is_complete("{x: 1")),
        ?_assertNot(catena_repl:is_complete("[1, 2,")),
        ?_assertNot(catena_repl:is_complete("transform id x =\\"))
    ].

parse_input_test_() ->
    [
        ?_assertMatch({ok, _, _}, catena_repl:parse_input("42")),
        ?_assertMatch({ok, _, _}, catena_repl:parse_input("transform id x = x")),
        ?_assertMatch({error, _, _}, catena_repl:parse_input("@#$%"))
    ].

%%====================================================================
%% Evaluation Tests
%%====================================================================

eval_test_() ->
    {setup,
     fun setup_state/0,
     fun(_) -> ok end,
     fun(State) ->
         [
             {"Evaluate integer literal",
              fun() ->
                  {ok, {value, _, Type}, _} = catena_repl:eval("42", State),
                  ?assertEqual({tcon, int}, Type)
              end},

             {"Evaluate variable reference",
              fun() ->
                  {ok, {value, _, _Type}, _} = catena_repl:eval("x", State)
              end},

             {"Define transform",
              fun() ->
                  Result = catena_repl:eval("transform id x = x", State),
                  ?assertMatch({ok, {defined, id, _}, _}, Result)
              end},

             {"Type command",
              fun() ->
                  Result = catena_repl:eval(":type 42", State),
                  ?assertMatch({ok, {type, _}, _}, Result)
              end},

             {"Help command",
              fun() ->
                  Result = catena_repl:eval(":help", State),
                  ?assertMatch({ok, {help, _}, _}, Result)
              end},

             {"Unknown command",
              fun() ->
                  Result = catena_repl:eval(":unknown", State),
                  ?assertMatch({error, {unknown_command, "unknown"}, _}, Result)
              end},

             {"Clear command",
              fun() ->
                  Result = catena_repl:eval(":clear", State),
                  ?assertMatch({ok, cleared, _}, Result)
              end}
         ]
     end}.

%%====================================================================
%% Environment Tests
%%====================================================================

env_persistence_test_() ->
    {setup,
     fun setup_state/0,
     fun(_) -> ok end,
     fun(State) ->
         [
             {"Definitions persist across evaluations",
              fun() ->
                  %% Define a function
                  {ok, {defined, const, _}, State2} =
                      catena_repl:eval("transform const x y = x", State),
                  %% Check it's in the environment
                  Env = catena_repl:get_env(State2),
                  ?assertMatch({ok, _}, catena_type_env:lookup(Env, const))
              end}
         ]
     end}.

%%====================================================================
%% Command Tests
%%====================================================================

load_command_test_() ->
    {setup,
     fun setup_state/0,
     fun(_) -> ok end,
     fun(State) ->
         [
             {"Load existing file",
              fun() ->
                  Result = catena_repl:eval(":load examples/simple/identity.catena", State),
                  ?assertMatch({ok, {loaded, _, _}, _}, Result)
              end},

             {"Load missing file",
              fun() ->
                  Result = catena_repl:eval(":load nonexistent.catena", State),
                  ?assertMatch({error, {load_error, _, _}, _}, Result)
              end},

             {"Load without filename",
              fun() ->
                  Result = catena_repl:eval(":load", State),
                  ?assertMatch({error, {missing_argument, _}, _}, Result)
              end}
         ]
     end}.

browse_command_test_() ->
    {setup,
     fun setup_state/0,
     fun(_) -> ok end,
     fun(State) ->
         [
             {"Browse empty bindings",
              fun() ->
                  Result = catena_repl:eval(":browse", State),
                  ?assertMatch({ok, {bindings, []}, _}, Result)
              end},

             {"Browse after definition",
              fun() ->
                  {ok, _, State2} = catena_repl:eval("transform id x = x", State),
                  Result = catena_repl:eval(":browse", State2),
                  ?assertMatch({ok, {bindings, [{id, _}]}, _}, Result)
              end}
         ]
     end}.

type_command_test_() ->
    {setup,
     fun setup_state/0,
     fun(_) -> ok end,
     fun(State) ->
         [
             {"Type of integer",
              fun() ->
                  {ok, {type, TypeStr}, _} = catena_repl:eval(":type 42", State),
                  ?assertEqual("int", TypeStr)
              end},

             {"Type without expression",
              fun() ->
                  Result = catena_repl:eval(":type", State),
                  ?assertMatch({error, {missing_argument, _}, _}, Result)
              end}
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup,
     fun setup_state/0,
     fun(_) -> ok end,
     fun(State) ->
         [
             {"Parse error doesn't crash",
              fun() ->
                  Result = catena_repl:eval("transform = invalid", State),
                  ?assertMatch({error, _, _}, Result)
              end},

             {"Type error doesn't crash",
              fun() ->
                  %% This might produce a type error depending on environment
                  Result = catena_repl:eval("undefined_var", State),
                  %% Should return either ok or error, not crash
                  ?assert(is_tuple(Result))
              end}
         ]
     end}.

%%====================================================================
%% Test Helpers
%%====================================================================

setup_state() ->
    %% Create a minimal REPL state for testing
    Env = case catena_compile:build_type_env([]) of
        {ok, E} -> E;
        _ -> catena_type_env:empty()
    end,
    %% Add 'x' to environment for testing
    XScheme = catena_type_scheme:mono({tcon, int}),
    Env2 = catena_type_env:extend(Env, x, XScheme),
    %% Record: {repl_state, env, bindings, runtime_bindings, history, prompt, continuation}
    {repl_state, Env2, #{}, #{}, [], "catena> ", ""}.
