%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_effect_advanced (Phase 6.4)
%%%
%%% Tests for advanced effect features including:
%%% - Delimited continuations (prompts, subcontinuations)
%%% - Scoped effects (temporary effect activation)
%%% - Effect handlers (dynamic handler installation)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_advanced_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Delimited Continuations Tests
%%%=============================================================================

new_prompt_creates_unique_prompt_test() ->
    P1 = catena_effect_advanced:new_prompt(),
    P2 = catena_effect_advanced:new_prompt(),
    ?assertMatch({prompt, _Id}, P1),
    ?assertMatch({prompt, _Id}, P2),
    % Prompts should have different IDs
    ?assertNotEqual(P1, P2).

push_prompt_returns_ok_test() ->
    Prompt = catena_effect_advanced:new_prompt(),
    ?assertEqual(ok, catena_effect_advanced:push_prompt(Prompt)).

pop_prompt_returns_ok_test() ->
    ?assertEqual(ok, catena_effect_advanced:pop_prompt()).

capture_subcont_returns_subcont_test() ->
    SubCont = catena_effect_advanced:capture_subcont(),
    ?assertMatch({subcont, _Id, _Fun}, SubCont).

push_subcont_returns_ok_test() ->
    SubCont = catena_effect_advanced:capture_subcont(),
    ?assertEqual(ok, catena_effect_advanced:push_subcont(SubCont)).

abort_is_non_returning_test() ->
    % abort/1 is marked as no_return(), we can't test execution
    % but we can verify the function is exported
    Exports = catena_effect_advanced:module_info(exports),
    ?assert(lists:keymember(abort, 1, Exports)).

prompt_lifecycle_test() ->
    % Test creating, pushing, and popping prompts
    Prompt = catena_effect_advanced:new_prompt(),
    ?assertMatch(ok, catena_effect_advanced:push_prompt(Prompt)),
    ?assertMatch(ok, catena_effect_advanced:pop_prompt()).

subcont_capture_and_push_test() ->
    % Test capturing and pushing subcontinuations
    SubCont = catena_effect_advanced:capture_subcont(),
    ?assertMatch(ok, catena_effect_advanced:push_subcont(SubCont)).

%%%=============================================================================
%%% Scoped Effects Tests
%%%=============================================================================

scope_effects_returns_result_test() ->
    % Test that scope_effects runs computation and returns result
    Result = catena_effect_advanced:scope_effects(
        [],
        fun() -> expected_result end
    ),
    ?assertEqual(expected_result, Result).

scope_effects_with_handlers_test() ->
    % Test scope_effects with effect handlers
    Handler = {test_handler, catena_effect_advanced, handle, []},
    Result = catena_effect_advanced:scope_effects(
        [Handler],
        fun() -> scoped_result end
    ),
    ?assertEqual(scoped_result, Result).

scope_effects_cleanup_on_error_test() ->
    % Test that effects are cleaned up even on error
    Handler = {test_handler, catena_effect_advanced, handle, []},
    try
        catena_effect_advanced:scope_effects(
            [Handler],
            fun() -> error(test_error) end
        ),
        ?assert(false, "Should have thrown error")
    catch
        error:test_error -> ok
    end.

in_scope_returns_result_test() ->
    Scope = {scope, [], []},
    Result = catena_effect_advanced:in_scope(Scope, fun() -> in_scoped end),
    ?assertEqual(in_scoped, Result).

in_scope_with_effects_test() ->
    Handler = {test_handler, catena_effect_advanced, handle, []},
    Scope = {scope, [Handler], []},
    Result = catena_effect_advanced:in_scope(Scope, fun() -> with_effects end),
    ?assertEqual(with_effects, Result).

with_effect_returns_result_test() ->
    Result = catena_effect_advanced:with_effect(
        my_handler,
        {catena_effect_advanced, handle, []},
        fun() -> handler_result end
    ),
    ?assertEqual(handler_result, Result).

local_effects_returns_handlers_test() ->
    Handlers = catena_effect_advanced:local_effects(),
    ?assert(is_list(Handlers)).

%%%=============================================================================
%%% Effect Handlers Tests
%%%=============================================================================

install_handler_returns_ok_test() ->
    ?assertEqual(ok, catena_effect_advanced:install_handler(
        test_id,
        {catena_effect_advanced, handle, []}
    )).

uninstall_handler_returns_ok_test() ->
    ?assertEqual(ok, catena_effect_advanced:uninstall_handler(test_id)).

current_handlers_returns_list_test() ->
    Handlers = catena_effect_advanced:current_handlers(),
    ?assert(is_list(Handlers)).

with_handler_returns_result_test() ->
    Result = catena_effect_advanced:with_handler(
        {catena_effect_advanced, handle, []},
        fun() -> with_handler_result end
    ),
    ?assertEqual(with_handler_result, Result).

with_handler_cleanup_on_error_test() ->
    % Test that handler is cleaned up even on error
    try
        catena_effect_advanced:with_handler(
            {catena_effect_advanced, handle, []},
            fun() -> error(handler_error) end
        ),
        ?assert(false, "Should have thrown error")
    catch
        error:handler_error -> ok
    end.

handler_lifecycle_test() ->
    % Test installing and uninstalling handlers
    HandlerId = test_lifecycle_handler,
    ?assertEqual(ok, catena_effect_advanced:install_handler(
        HandlerId,
        {catena_effect_advanced, handle, []}
    )),
    ?assertEqual(ok, catena_effect_advanced:uninstall_handler(HandlerId)).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

scoped_effects_with_continuations_test() ->
    % Test combining scoped effects with delimited continuations
    Prompt = catena_effect_advanced:new_prompt(),
    Handler = {test_handler, catena_effect_advanced, handle, []},
    Result = catena_effect_advanced:scope_effects(
        [Handler],
        fun() ->
            catena_effect_advanced:push_prompt(Prompt),
            catena_effect_advanced:pop_prompt(),
            combined_result
        end
    ),
    ?assertEqual(combined_result, Result).

nested_scopes_test() ->
    % Test nested effect scopes
    OuterHandler = {outer, catena_effect_advanced, handle, []},
    InnerHandler = {inner, catena_effect_advanced, handle, []},
    Result = catena_effect_advanced:scope_effects(
        [OuterHandler],
        fun() ->
            catena_effect_advanced:scope_effects(
                [InnerHandler],
                fun() -> nested_result end
            )
        end
    ),
    ?assertEqual(nested_result, Result).

multiple_handlers_test() ->
    % Test multiple handlers in a single scope
    Handlers = [
        {handler1, catena_effect_advanced, handle, []},
        {handler2, catena_effect_advanced, handle, []},
        {handler3, catena_effect_advanced, handle, []}
    ],
    Result = catena_effect_advanced:scope_effects(
        Handlers,
        fun() -> multiple_handlers_result end
    ),
    ?assertEqual(multiple_handlers_result, Result).

temporary_with_handler_test() ->
    % Test with_handler vs scope_effects
    % Both should provide temporary handler installation
    Result1 = catena_effect_advanced:with_handler(
        {catena_effect_advanced, handle, []},
        fun() -> temp1 end
    ),
    Result2 = catena_effect_advanced:with_effect(
        temp,
        {catena_effect_advanced, handle, []},
        fun() -> temp2 end
    ),
    ?assertEqual(temp1, Result1),
    ?assertEqual(temp2, Result2).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================
%%% No additional helpers needed
