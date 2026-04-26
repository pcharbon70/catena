-module(catena_one_shot_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_one_shot_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"constructors", fun test_constructors/0},
        {"one-shot capture", fun test_capture/0},
        {"one-shot resumption wrapping", fun test_wrap/0},
        {"one-shot resume", fun test_resume/0},
        {"consumption tracking", fun test_consumption_tracking/0},
        {"double-resume prevention", fun test_double_resume/0},
        {"state inspection", fun test_state_inspection/0},
        {"try_resume returns new continuation", fun test_try_resume/0},
        {"error handling", fun test_error_handling/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    catena_one_shot:reset_table(),
    ok.

cleanup(_) ->
    catena_one_shot:reset_table(),
    ok.

%%%---------------------------------------------------------------------
%%% Constructor Tests
%%%---------------------------------------------------------------------

test_constructors() ->
    Empty = catena_one_shot:new(),
    WithData = catena_one_shot:new(#{test => data}),

    ?assertEqual({ok, #{}}, catena_one_shot:get_state(Empty)),
    ?assertEqual({ok, #{test => data}}, catena_one_shot:get_state(WithData)),
    ?assertEqual(one_shot, catena_one_shot:kind()),
    ?assert(catena_one_shot:is_available(Empty)),
    ?assertNot(catena_one_shot:is_consumed(Empty)).

%%%---------------------------------------------------------------------
%%% Capture Tests
%%%---------------------------------------------------------------------

test_capture() ->
    {ok, Cont} = catena_one_shot:capture(),
    ?assert(catena_one_shot:is_available(Cont)),
    ?assertNot(catena_one_shot:is_consumed(Cont)),

    %% Capture with metadata
    {ok, Cont2} = catena_one_shot:capture(#{source => test}),
    ?assert(catena_one_shot:is_available(Cont2)).

test_wrap() ->
    Resumption = catena_resumption:new(
        fun(Value) -> {wrapped, Value} end,
        erlang:system_time(millisecond),
        1
    ),
    Cont = catena_one_shot:wrap(Resumption),

    ?assert(catena_one_shot:is_available(Cont)),
    {ok, {wrapped, resumed}} = catena_one_shot:resume(Cont, resumed),
    ?assert(catena_one_shot:is_consumed(Cont)),
    ?assertEqual({error, already_consumed}, catena_one_shot:resume(Cont, again)).

%%%---------------------------------------------------------------------
%%% Resume Tests
%%%---------------------------------------------------------------------

test_resume() ->
    Cont = catena_one_shot:new(#{initial => state}),
    ?assert(catena_one_shot:is_available(Cont)),

    %% First resume succeeds
    {ok, Result} = catena_one_shot:resume(Cont, 42),
    ?assertMatch(#{value := 42}, Result),

    %% Continuation is now consumed
    ?assert(catena_one_shot:is_consumed(Cont)).

%%%---------------------------------------------------------------------
%%% Consumption Tracking Tests
%%%---------------------------------------------------------------------

test_consumption_tracking() ->
    Cont = catena_one_shot:new(),

    %% Initially available
    ?assert(catena_one_shot:is_available(Cont)),
    ?assertNot(catena_one_shot:is_consumed(Cont)),

    %% After resume, consumed
    {ok, _} = catena_one_shot:resume(Cont, value),
    ?assertNot(catena_one_shot:is_available(Cont)),
    ?assert(catena_one_shot:is_consumed(Cont)).

%%%---------------------------------------------------------------------
%%% Double-Resume Prevention Tests
%%%---------------------------------------------------------------------

test_double_resume() ->
    Cont = catena_one_shot:new(),

    %% First resume succeeds
    ?assertMatch({ok, _}, catena_one_shot:resume(Cont, first)),

    %% Second resume fails
    ?assertEqual({error, already_consumed}, catena_one_shot:resume(Cont, second)).

%%%---------------------------------------------------------------------
%%% State Inspection Tests
%%%---------------------------------------------------------------------

test_state_inspection() ->
    Data = #{key => value, counter => 1},
    Cont = catena_one_shot:new(Data),

    {ok, Retrieved} = catena_one_shot:get_state(Cont),
    ?assertEqual(Data, Retrieved).

%%%---------------------------------------------------------------------
%%% Try Resume Tests
%%%---------------------------------------------------------------------

test_try_resume() ->
    Cont = catena_one_shot:new(#{data => test}),

    %% First try_resume succeeds and returns new consumed continuation
    {{ok, Result1}, Cont1} = catena_one_shot:try_resume(Cont, 1),
    ?assertMatch(#{value := 1}, Result1),
    ?assert(catena_one_shot:is_consumed(Cont1)),
    ?assertNot(catena_one_shot:is_available(Cont1)),

    %% Second try_resume on consumed continuation fails
    {{error, already_consumed}, Cont2} = catena_one_shot:try_resume(Cont1, 2),
    ?assertEqual(Cont1, Cont2).

%%%---------------------------------------------------------------------
%%% Error Handling Tests
%%%---------------------------------------------------------------------

test_error_handling() ->
    %% Invalid state
    ?assertEqual({error, invalid_state}, catena_one_shot:resume(invalid, value)),

    %% Invalid kind
    ?assertEqual({error, invalid_state}, catena_one_shot:resume(#{kind => other}, value)).

%%%---------------------------------------------------------------------
%%% Algebraic Laws Tests
%%%---------------------------------------------------------------------

test_algebraic_laws() ->
    Cont = catena_one_shot:new(),

    %% One-shot continuations are not associative
    ?assertNot(catena_one_shot:associative(Cont, Cont)),

    %% One-shot continuations are not commutative
    ?assertNot(catena_one_shot:commutative(Cont, Cont)),

    %% No identity element
    ?assertNot(catena_one_shot:identity(Cont, Cont)),

    %% Not idempotent
    ?assertNot(catena_one_shot:idempotent(Cont)).

algebraic_laws_test() ->
    test_algebraic_laws().
