-module(catena_multi_shot_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_multi_shot_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"constructors", fun test_constructors/0},
        {"multi-shot capture", fun test_capture/0},
        {"multi-shot resumption wrapping", fun test_wrap/0},
        {"multi-shot resume with deep copy", fun test_resume_deep_copy/0},
        {"multi-shot resume with shallow copy", fun test_resume_shallow_copy/0},
        {"multi-shot resume with selective copy", fun test_resume_selective_copy/0},
        {"resume counting", fun test_resume_counting/0},
        {"multiple resumes", fun test_multiple_resumes/0},
        {"state inspection", fun test_state_inspection/0},
        {"always available", fun test_always_available/0},
        {"error handling", fun test_error_handling/0},
        {"algebraic laws", fun test_algebraic_laws/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    catena_multi_shot:reset_table(),
    ok.

cleanup(_) ->
    catena_multi_shot:reset_table(),
    ok.

%%%---------------------------------------------------------------------
%%% Constructor Tests
%%%---------------------------------------------------------------------

test_constructors() ->
    Empty = catena_multi_shot:new(),
    WithData = catena_multi_shot:new(#{test => data}),
    WithStrategy = catena_multi_shot:new(#{test => data}, shallow),

    ?assertEqual(multi_shot, catena_multi_shot:kind()),
    ?assert(catena_multi_shot:is_available(Empty)),
    ?assertEqual(0, catena_multi_shot:get_resume_count(Empty)),
    ?assertEqual({ok, #{test => data}}, catena_multi_shot:get_state(WithData)),
    ?assertMatch({ok, _}, catena_multi_shot:get_state(WithStrategy)).

%%%---------------------------------------------------------------------
%%% Capture Tests
%%%---------------------------------------------------------------------

test_capture() ->
    {ok, Cont} = catena_multi_shot:capture(),
    ?assert(catena_multi_shot:is_available(Cont)),
    ?assertEqual(0, catena_multi_shot:get_resume_count(Cont)),

    %% Capture with metadata
    {ok, Cont2} = catena_multi_shot:capture(#{source => test}),
    ?assert(catena_multi_shot:is_available(Cont2)),

    %% Capture with strategy
    {ok, Cont3} = catena_multi_shot:capture(#{}, shallow),
    ?assert(catena_multi_shot:is_available(Cont3)).

test_wrap() ->
    Resumption = catena_resumption:new(
        fun(Value) -> {wrapped, Value} end,
        erlang:system_time(millisecond),
        1
    ),
    Cont = catena_multi_shot:wrap(Resumption, shallow),

    {ok, Result1, Count1} = catena_multi_shot:resume(Cont, first),
    ?assertEqual(1, Count1),
    ?assertEqual({wrapped, first}, maps:get(resumed, Result1)),

    {ok, Result2, Count2} = catena_multi_shot:resume(Cont, second),
    ?assertEqual(2, Count2),
    ?assertEqual({wrapped, second}, maps:get(resumed, Result2)).

%%%---------------------------------------------------------------------
%%% Deep Copy Tests
%%%---------------------------------------------------------------------

test_resume_deep_copy() ->
    Cont = catena_multi_shot:new(#{mutable => [1, 2, 3]}, deep),
    {ok, Result1, Count1} = catena_multi_shot:resume(Cont, 42),
    ?assertEqual(1, Count1),
    ?assertMatch(#{value := 42}, Result1),

    %% Second resume should have independent copy
    {ok, Result2, Count2} = catena_multi_shot:resume(Cont, 43),
    ?assertEqual(2, Count2),
    ?assertMatch(#{value := 43}, Result2).

%%%---------------------------------------------------------------------
%%% Shallow Copy Tests
%%%---------------------------------------------------------------------

test_resume_shallow_copy() ->
    Cont = catena_multi_shot:new(#{shared => data}, shallow),
    {ok, _Result1, Count1} = catena_multi_shot:resume(Cont, 1),
    ?assertEqual(1, Count1),

    %% Second resume shares the same state
    {ok, Result2, Count2} = catena_multi_shot:resume(Cont, 2),
    ?assertEqual(2, Count2),
    ?assertMatch(#{value := 2}, Result2).

%%%---------------------------------------------------------------------
%%% Selective Copy Tests
%%%---------------------------------------------------------------------

test_resume_selective_copy() ->
    Cont = catena_multi_shot:new(#{small => 1}, selective),
    {ok, _Result1, Count1} = catena_multi_shot:resume(Cont, 1),
    ?assertEqual(1, Count1),

    %% Small maps are shared
    {ok, _Result2, Count2} = catena_multi_shot:resume(Cont, 2),
    ?assertEqual(2, Count2).

%%%---------------------------------------------------------------------
%%% Resume Counting Tests
%%%---------------------------------------------------------------------

test_resume_counting() ->
    Cont = catena_multi_shot:new(#{data => test}),

    ?assertEqual(0, catena_multi_shot:get_resume_count(Cont)),

    catena_multi_shot:resume(Cont, 1),
    ?assertEqual(1, catena_multi_shot:get_resume_count(Cont)),

    catena_multi_shot:resume(Cont, 2),
    ?assertEqual(2, catena_multi_shot:get_resume_count(Cont)),

    catena_multi_shot:resume(Cont, 3),
    ?assertEqual(3, catena_multi_shot:get_resume_count(Cont)).

%%%---------------------------------------------------------------------
%%% Multiple Resumes Tests
%%%---------------------------------------------------------------------

test_multiple_resumes() ->
    Cont = catena_multi_shot:new(#{counter => 0}),

    %% Multi-shot continuations can be resumed many times
    lists:foreach(fun(N) ->
        {ok, _, Count} = catena_multi_shot:resume(Cont, N),
        ?assertEqual(N, Count)
    end, lists:seq(1, 10)).

%%%---------------------------------------------------------------------
%%% State Inspection Tests
%%%---------------------------------------------------------------------

test_state_inspection() ->
    Data = #{key => value, nested => #{inner => data}},
    Cont = catena_multi_shot:new(Data),

    {ok, Retrieved} = catena_multi_shot:get_state(Cont),
    ?assertEqual(Data, Retrieved).

%%%---------------------------------------------------------------------
%%% Always Available Tests
%%%---------------------------------------------------------------------

test_always_available() ->
    Cont = catena_multi_shot:new(),

    %% Multi-shot continuations are always available
    ?assert(catena_multi_shot:is_available(Cont)),

    catena_multi_shot:resume(Cont, 1),
    ?assert(catena_multi_shot:is_available(Cont)),

    catena_multi_shot:resume(Cont, 2),
    ?assert(catena_multi_shot:is_available(Cont)),

    catena_multi_shot:resume(Cont, 3),
    ?assert(catena_multi_shot:is_available(Cont)).

%%%---------------------------------------------------------------------
%%% Error Handling Tests
%%%---------------------------------------------------------------------

test_error_handling() ->
    %% Invalid state
    ?assertMatch({error, invalid_state}, catena_multi_shot:resume(invalid, value)),

    %% Invalid kind
    ?assertMatch({error, invalid_state}, catena_multi_shot:resume(#{kind => other}, value)).

%%%---------------------------------------------------------------------
%%% Algebraic Laws Tests
%%%---------------------------------------------------------------------

test_algebraic_laws() ->
    Cont = catena_multi_shot:new(),

    %% Multi-shot continuations are associative
    ?assert(catena_multi_shot:associative(Cont, Cont)),

    %% Multi-shot continuations are commutative
    ?assert(catena_multi_shot:commutative(Cont, Cont)),

    %% Empty continuation acts as identity
    Empty = catena_multi_shot:new(),
    ?assert(catena_multi_shot:identity(Empty, Cont)),

    %% Multi-shot is idempotent
    ?assert(catena_multi_shot:idempotent(Cont)).
