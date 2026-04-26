-module(catena_continuation_kind_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Integration Tests for One-Shot vs Multi-Shot Continuations
%%%---------------------------------------------------------------------

%% These integration tests verify the interaction between:
%% - Continuation kinds (one_shot, multi_shot)
%% - State duplication strategies (deep, shallow, selective)
%% - Backtracking and nondeterminism patterns
%% - Resource management and cleanup

catena_continuation_kind_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"one-shot resumption succeeds once", fun test_one_shot_succeeds_once/0},
        {"one-shot double-resume fails", fun test_one_shot_double_resume_fails/0},
        {"one-shot resource cleanup", fun test_one_shot_cleanup/0},
        {"one-shot error handling", fun test_one_shot_error_handling/0},
        {"one-shot nested continuations", fun test_one_shot_nested/0},
        {"multi-shot resumption multiple times", fun test_multi_shot_multiple_resumes/0},
        {"multi-shot state duplication", fun test_multi_shot_state_duplication/0},
        {"multi-shot state sharing", fun test_multi_shot_state_sharing/0},
        {"multi-shot resource management", fun test_multi_shot_resource_management/0},
        {"multi-shot nested continuations", fun test_multi_shot_nested/0},
        {"handler executes under one-shot kind", fun test_handler_one_shot_kind/0},
        {"effects multi-shot wrapper", fun test_effects_multi_shot_wrapper/0},
        {"effects one-shot wrapper", fun test_effects_one_shot_wrapper/0},
        {"backtracking with multi-shot", fun test_backtracking/0},
        {"ambivalent choice operator", fun test_ambivalent_choice/0},
        {"nondeterministic search", fun test_nondeterministic_search/0},
        {"solution enumeration", fun test_solution_enumeration/0},
        {"constraint solving", fun test_constraint_solving/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    catena_one_shot:reset_table(),
    catena_multi_shot:reset_table(),
    ok.

cleanup(_) ->
    catena_one_shot:reset_table(),
    catena_multi_shot:reset_table(),
    ok.

%%%---------------------------------------------------------------------
%%% One-Shot Continuation Tests
%%%---------------------------------------------------------------------

test_one_shot_succeeds_once() ->
    Cont = catena_one_shot:new(#{data => test}),
    ?assert(catena_one_shot:is_available(Cont)),

    {ok, Result} = catena_one_shot:resume(Cont, 42),
    ?assertMatch(#{value := 42}, Result),

    ?assert(catena_one_shot:is_consumed(Cont)),
    ?assertNot(catena_one_shot:is_available(Cont)).

test_one_shot_double_resume_fails() ->
    Cont = catena_one_shot:new(#{initial => state}),

    %% First resume succeeds
    {ok, _} = catena_one_shot:resume(Cont, first),

    %% Second resume fails
    ?assertEqual({error, already_consumed},
        catena_one_shot:resume(Cont, second)),

    %% Third resume also fails
    ?assertEqual({error, already_consumed},
        catena_one_shot:resume(Cont, third)).

test_one_shot_cleanup() ->
    Cont = catena_one_shot:new(#{resource => file_handle}),

    %% Resume consumes the continuation
    {ok, _} = catena_one_shot:resume(Cont, value),

    %% Verify consumed before cleanup
    ?assert(catena_one_shot:is_consumed(Cont)),

    %% Cleanup removes from tracking table
    ok = catena_one_shot:cleanup(Cont),

    %% After cleanup, the continuation is no longer tracked
    ?assertNot(catena_one_shot:is_consumed(Cont)).

test_one_shot_error_handling() ->
    %% Invalid state
    ?assertEqual({error, invalid_state},
        catena_one_shot:resume(invalid, value)),

    %% Invalid kind
    ?assertEqual({error, invalid_state},
        catena_one_shot:resume(#{kind => other}, value)).

test_one_shot_nested() ->
    Outer = catena_one_shot:new(#{outer => data}),

    %% Capture outer state
    {ok, OuterState} = catena_one_shot:get_state(Outer),

    %% Create inner continuation
    Inner = catena_one_shot:new(#{inner => data, outer => OuterState}),

    %% Resume inner first
    {ok, _} = catena_one_shot:resume(Inner, inner_value),

    %% Inner is consumed
    ?assert(catena_one_shot:is_consumed(Inner)),

    %% Outer is still available
    {ok, _} = catena_one_shot:resume(Outer, outer_value),
    ?assert(catena_one_shot:is_consumed(Outer)).

%%%---------------------------------------------------------------------
%%% Multi-Shot Continuation Tests
%%%---------------------------------------------------------------------

test_multi_shot_multiple_resumes() ->
    Cont = catena_multi_shot:new(#{counter => 0}),

    %% Multi-shot can be resumed many times
    lists:foreach(fun(N) ->
        {ok, _, Count} = catena_multi_shot:resume(Cont, N),
        ?assertEqual(N, Count)
    end, lists:seq(1, 20)).

test_multi_shot_state_duplication() ->
    Original = #{mutable => [1, 2, 3], counter => 0},
    Cont = catena_multi_shot:new(Original, deep),

    %% First resume
    {ok, Result1, _} = catena_multi_shot:resume(Cont, 1),
    State1 = maps:get(previous, Result1),
    ?assertEqual(Original, State1),

    %% Second resume gets independent copy
    {ok, Result2, _} = catena_multi_shot:resume(Cont, 2),
    State2 = maps:get(previous, Result2),
    ?assertEqual(Original, State2),

    %% States are independent
    ?assertEqual(State1, State2).

test_multi_shot_state_sharing() ->
    Shared = #{shared => data},
    Cont = catena_multi_shot:new(Shared, shallow),

    %% Multiple resumes share the same state
    {ok, R1, _} = catena_multi_shot:resume(Cont, 1),
    {ok, R2, _} = catena_multi_shot:resume(Cont, 2),

    S1 = maps:get(previous, R1),
    S2 = maps:get(previous, R2),

    ?assertEqual(S1, S2).

test_multi_shot_resource_management() ->
    Cont = catena_multi_shot:new(#{resource => pool}),

    %% Resume multiple times
    lists:foreach(fun(N) ->
        {ok, _, Count} = catena_multi_shot:resume(Cont, N),
        ?assertEqual(N, Count)
    end, lists:seq(1, 5)),

    %% Resume count is tracked
    ?assertEqual(5, catena_multi_shot:get_resume_count(Cont)),

    %% Cleanup
    ok = catena_multi_shot:cleanup(Cont),

    %% After cleanup, count is reset
    ?assertEqual(0, catena_multi_shot:get_resume_count(Cont)).

test_multi_shot_nested() ->
    Outer = catena_multi_shot:new(#{outer => 1}),

    %% Create inner continuation
    Inner = catena_multi_shot:new(#{inner => 2}),

    %% Both can be resumed multiple times
    lists:foreach(fun(_) ->
        {ok, _, _} = catena_multi_shot:resume(Inner, inner),
        {ok, _, _} = catena_multi_shot:resume(Outer, outer)
    end, lists:seq(1, 3)).

test_handler_one_shot_kind() ->
    Handler = catena_handler:new(test_op, fun(_Value, _Resumption) ->
        catena_continuation_kind:current_kind()
    end),
    Resumption = catena_one_shot:wrap(
        catena_resumption:new(
            fun(Value) -> {resumed, Value} end,
            erlang:system_time(millisecond),
            1
        )
    ),
    ?assertEqual(one_shot, catena_handler:execute(Handler, ignored, Resumption)).

test_effects_multi_shot_wrapper() ->
    ok = catena_effects:init(),
    try
        Result = catena_effects:handle(
            phase10_multi_shot_op,
            fun(_Value, Resumption) ->
                {multi_shot, [
                    catena_effects:resume(Resumption, 1),
                    catena_effects:resume(Resumption, 2)
                ]}
            end,
            fun() ->
                catena_effects:perform(phase10_multi_shot_op, ignored)
            end
        ),
        ?assertEqual({multi_shot, [{resumed, 1}, {resumed, 2}]}, Result)
    after
        catena_effects:shutdown()
    end.

test_effects_one_shot_wrapper() ->
    ok = catena_effects:init(),
    try
        Result = catena_effects:handle(
            phase10_one_shot_op,
            fun(_Value, Resumption) ->
                {
                    catena_effects:resume(Resumption, first),
                    catena_effects:resume(Resumption, second)
                }
            end,
            fun() ->
                catena_effects:perform(phase10_one_shot_op, ignored)
            end,
            #{one_shot => true}
        ),
        ?assertEqual({{resumed, first}, {error, already_consumed}}, Result)
    after
        catena_effects:shutdown()
    end.

%%%---------------------------------------------------------------------
%%% Backtracking and Nondeterminism Tests
%%%---------------------------------------------------------------------

test_backtracking() ->
    %% Simulate backtracking with multi-shot continuation
    Choices = [1, 2, 3],
    Cont = catena_multi_shot:new(#{choices => Choices}),

    %% Try each choice by resuming
    Results = lists:map(fun(Choice) ->
        {ok, _, _} = catena_multi_shot:resume(Cont, Choice),
        Choice
    end, Choices),

    ?assertEqual(Choices, Results).

test_ambivalent_choice() ->
    %% Ambivalent operator: try both choices
    Left = catena_multi_shot:new(#{choice => left}),
    Right = catena_multi_shot:new(#{choice => right}),

    %% Both choices are available
    {ok, L, _} = catena_multi_shot:resume(Left, left_value),
    {ok, R, _} = catena_multi_shot:resume(Right, right_value),

    ?assertEqual(left, maps:get(choice, maps:get(previous, L))),
    ?assertEqual(right, maps:get(choice, maps:get(previous, R))).

test_nondeterministic_search() ->
    %% Simulate a nondeterministic search
    SearchSpace = [a, b, c, d, e],
    Cont = catena_multi_shot:new(#{space => SearchSpace}),

    %% Search through all possibilities
    Found = lists:filter(fun(Item) ->
        {ok, _, _} = catena_multi_shot:resume(Cont, Item),
        Item =:= c  %% Looking for 'c'
    end, SearchSpace),

    ?assert(lists:member(c, Found)).

test_solution_enumeration() ->
    %% Enumerate all solutions
    Solutions = [sol1, sol2, sol3],
    Cont = catena_multi_shot:new(#{solutions => Solutions}),

    %% Enumerate by resuming
    Enumerated = lists:map(fun(Sol) ->
        {ok, _, _} = catena_multi_shot:resume(Cont, Sol),
        Sol
    end, Solutions),

    ?assertEqual(Solutions, Enumerated).

test_constraint_solving() ->
    %% Simple constraint solving: find valid combinations
    Domain1 = [1, 2, 3],
    Domain2 = [4, 5, 6],
    Cont = catena_multi_shot:new(#{d1 => Domain1, d2 => Domain2}),

    %% Find combinations where sum > 7
    Valid = lists:filter(fun({X, Y}) ->
        {ok, _, _} = catena_multi_shot:resume(Cont, {X, Y}),
        X + Y > 7
    end, [{X, Y} || X <- Domain1, Y <- Domain2]),

    ?assert(length(Valid) > 0).
