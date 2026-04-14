%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_resumption (Phase 7.1)
%%%
%%% Tests for the resumption type and continuation capture:
%%% - Resumption type definition and constructors
%%% - Continuation capture mechanism
%%% - Resumption execution
%%% - Resumption inspection and validation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Resumption Type and Constructors Tests
%%%=============================================================================

resumption_new_test() ->
    Fun = fun(X) -> X + 1 end,
    CapturedAt = 1000,
    StackDepth = 5,
    Resumption = catena_resumption:new(Fun, CapturedAt, StackDepth),
    ?assert(catena_resumption:is_resumption(Resumption)).

resumption_new_with_metadata_test() ->
    Fun = fun(X) -> X + 1 end,
    CapturedAt = 1000,
    StackDepth = 5,
    Metadata = #{key => value},
    Resumption = catena_resumption:new(Fun, CapturedAt, StackDepth, Metadata),
    ?assert(catena_resumption:is_resumption(Resumption)),
    ?assertEqual(Metadata, catena_resumption:metadata(Resumption)).

resumption_is_resumption_test() ->
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0),
    ?assert(catena_resumption:is_resumption(Resumption)),
    ?assertNot(catena_resumption:is_resumption(not_a_resumption)),
    ?assertNot(catena_resumption:is_resumption({resumption, partial})).

resumption_cont_of_test() ->
    Cont = fun(X) -> X + 1 end,
    Resumption = catena_resumption:new(Cont, 0, 0),
    ?assertEqual(Cont, catena_resumption:cont_of(Resumption)).

%%%=============================================================================
%%% Continuation Capture Tests
%%%=============================================================================

capture_continuation_test() ->
    Resumption = catena_resumption:capture_continuation(),
    ?assert(catena_resumption:is_resumption(Resumption)),
    ?assert(is_integer(catena_resumption:captured_at(Resumption))),
    ?assert(is_integer(catena_resumption:stack_depth(Resumption))).

capture_with_metadata_test() ->
    Metadata = #{operation => get, handler => state},
    Resumption = catena_resumption:capture_with_metadata(Metadata),
    ?assert(catena_resumption:is_resumption(Resumption)),
    ?assertEqual(Metadata, catena_resumption:metadata(Resumption)).

stack_depth_test() ->
    Resumption = catena_resumption:capture_continuation(),
    StackDepth = catena_resumption:stack_depth(Resumption),
    ?assert(StackDepth >= 0).

%%%=============================================================================
%%% Resumption Execution Tests
%%%=============================================================================

resume_simple_test() ->
    Fun = fun(X) -> X * 2 end,
    Resumption = catena_resumption:new(Fun, 0, 0),
    ?assertEqual(20, catena_resumption:resume(Resumption, 10)).

resume_with_state_test() ->
    Fun = fun({State, Value}) -> {State + 1, Value * 2} end,
    Resumption = catena_resumption:new(Fun, 0, 0),
    ?assertEqual({6, 20}, catena_resumption:resume(Resumption, {5, 10})).

resume_error_test() ->
    Fun = fun(_) -> error(intentional) end,
    Resumption = catena_resumption:new(Fun, 0, 0),
    Result = catena_resumption:resume(Resumption, test),
    ?assertMatch({error, error, intentional, _}, Result).

resume_with_timeout_success_test() ->
    Fun = fun(X) -> X * 2 end,
    Resumption = catena_resumption:new(Fun, 0, 0),
    ?assertEqual(20, catena_resumption:resume_with_timeout(Resumption, 10, 1000)).

resume_with_timeout_timeout_test() ->
    Fun = fun(_) -> timer:sleep(200), result end,
    Resumption = catena_resumption:new(Fun, 0, 0),
    Result = catena_resumption:resume_with_timeout(Resumption, test, 50),
    ?assertMatch({timeout, test}, Result).

%%%=============================================================================
%%% Resumption Inspection Tests
%%%=============================================================================

captured_at_test() ->
    Before = erlang:system_time(millisecond),
    Resumption = catena_resumption:capture_continuation(),
    After = erlang:system_time(millisecond),
    CapturedAt = catena_resumption:captured_at(Resumption),
    ?assert(CapturedAt >= Before andalso CapturedAt =< After).

stack_depth_non_negative_test() ->
    Resumption = catena_resumption:capture_continuation(),
    StackDepth = catena_resumption:stack_depth(Resumption),
    ?assert(StackDepth >= 0).

metadata_default_test() ->
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0),
    ?assertEqual(#{}, catena_resumption:metadata(Resumption)).

metadata_custom_test() ->
    Metadata = #{operation => get, handler => state, depth => 3},
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0, Metadata),
    ?assertEqual(Metadata, catena_resumption:metadata(Resumption)).

update_metadata_test() ->
    Resumption = catena_resumption:new(fun(X) -> X end, 0, 0),
    NewMetadata = #{updated => true},
    Updated = catena_resumption:update_metadata(Resumption, NewMetadata),
    ?assertEqual(NewMetadata, catena_resumption:metadata(Updated)),
    % Original resumption unchanged (immutable)
    ?assertEqual(#{}, catena_resumption:metadata(Resumption)).

%%%=============================================================================
%%% Resumption Validation Tests
%%%=============================================================================

validate_valid_resumption_test() ->
    Fun = fun(X) -> X end,
    Resumption = catena_resumption:new(Fun, 0, 0, #{}),
    ?assertEqual(ok, catena_resumption:validate(Resumption)).

validate_invalid_arity_test() ->
    % Test that zero-arity functions are rejected as continuations
    InvalidFun = fun() -> ok end,
    ?assertNot(catena_resumption:is_valid_continuation(InvalidFun)).

is_valid_continuation_test() ->
    ValidFun = fun(X) -> X end,
    InvalidFun = fun() -> ok end,
    TwoArgFun = fun(X, Y) -> X + Y end,
    NotAFun = not_a_function,
    ?assert(catena_resumption:is_valid_continuation(ValidFun)),
    ?assertNot(catena_resumption:is_valid_continuation(InvalidFun)),
    ?assertNot(catena_resumption:is_valid_continuation(TwoArgFun)),
    ?assertNot(catena_resumption:is_valid_continuation(NotAFun)).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

capture_and_resume_test() ->
    % Capture a continuation and resume it with a value
    Resumption = catena_resumption:capture_continuation(),
    Value = 42,
    Result = catena_resumption:resume(Resumption, Value),
    % In this simplified implementation, result is {resumed, Value}
    ?assertMatch({resumed, 42}, Result).

metadata_through_capture_test() ->
    % Metadata should be preserved through capture and resume
    Metadata = #{test => true, phase => "7.1"},
    Resumption = catena_resumption:capture_with_metadata(Metadata),
    RetrievedMetadata = catena_resumption:metadata(Resumption),
    ?assertEqual(Metadata, RetrievedMetadata).

multiple_resumptions_test() ->
    % Test that we can create and execute multiple resumptions
    Fun1 = fun(X) -> X * 2 end,
    Fun2 = fun(X) -> X + 10 end,
    Resumption1 = catena_resumption:new(Fun1, 0, 0),
    Resumption2 = catena_resumption:new(Fun2, 0, 0),
    ?assertEqual(20, catena_resumption:resume(Resumption1, 10)),
    ?assertEqual(20, catena_resumption:resume(Resumption2, 10)).
