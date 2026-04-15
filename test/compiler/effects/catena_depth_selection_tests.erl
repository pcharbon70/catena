%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_depth_selection (Phase 9.4)
%%%
%%% Tests for handler depth selection:
%%% - Depth selection API
%%% - Depth conversion
%%% - Mixed depth handlers
%%% - Depth introspection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_depth_selection_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Depth Selection API Tests
%%%=============================================================================

with_deep_handler_test() ->
    % Test that with_deep_handler creates deep handler
    HandlerFun = fun(_Op, _Args) -> ok end,
    Result = catena_depth_selection:with_deep_handler(test, HandlerFun, fun() ->
        Context = get(deep_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0)
    end),
    ?assertEqual(ok, Result).

with_shallow_handler_test() ->
    % Test that with_shallow_handler creates shallow handler
    HandlerFun = fun(_Op, _Args) -> ok end,
    Result = catena_depth_selection:with_shallow_handler(test, HandlerFun, fun() ->
        Context = get(shallow_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0)
    end),
    ?assertEqual(ok, Result).

with_handler_deep_test() ->
    % Test with_handler for deep
    HandlerFun = fun(_Op, _Args) -> ok end,
    Result = catena_depth_selection:with_handler(test, deep, HandlerFun, fun() ->
        Context = get(deep_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0)
    end),
    ?assertEqual(ok, Result).

with_handler_shallow_test() ->
    % Test with_handler for shallow
    HandlerFun = fun(_Op, _Args) -> ok end,
    Result = catena_depth_selection:with_handler(test, shallow, HandlerFun, fun() ->
        Context = get(shallow_context),
        ?assert(length(maps:get(handlers, Context, [])) > 0)
    end),
    ?assertEqual(ok, Result).

select_depth_default_test() ->
    % Test that select_depth returns deep by default
    ?assertEqual(deep, catena_depth_selection:select_depth([])).

select_depth_deep_test() ->
    % Test that select_depth respects deep option
    ?assertEqual(deep, catena_depth_selection:select_depth([{depth, deep}])).

select_depth_shallow_test() ->
    % Test that select_depth respects shallow option
    ?assertEqual(shallow, catena_depth_selection:select_depth([{depth, shallow}])).

%%%=============================================================================
%%% Depth Conversion Tests
%%%=============================================================================

to_deep_from_deep_test() ->
    % Test converting deep handler to deep
    Handler = #{type => deep, effect => test, handler => fun() -> ok end, depth => deep},
    Result = catena_depth_selection:to_deep(Handler),
    ?assertEqual(deep, maps:get(type, Result)).

to_deep_from_shallow_test() ->
    % Test converting shallow handler to deep
    Handler = #{type => shallow, effect => test, handler => fun() -> ok end, depth => shallow},
    Result = catena_depth_selection:to_deep(Handler),
    ?assertEqual(deep, maps:get(type, Result)),
    ?assertEqual(deep, maps:get(depth, Result)).

to_shallow_from_shallow_test() ->
    % Test converting shallow handler to shallow
    Handler = #{type => shallow, effect => test, handler => fun() -> ok end, depth => shallow},
    Result = catena_depth_selection:to_shallow(Handler),
    ?assertEqual(shallow, maps:get(type, Result)).

to_shallow_from_deep_test() ->
    % Test converting deep handler to shallow
    Handler = #{type => deep, effect => test, handler => fun() -> ok end, depth => deep},
    Result = catena_depth_selection:to_shallow(Handler),
    ?assertEqual(shallow, maps:get(type, Result)),
    ?assertEqual(shallow, maps:get(depth, Result)).

convert_depth_to_deep_test() ->
    % Test convert_depth to deep
    Handler = #{type => shallow, effect => test, handler => fun() -> ok end},
    Result = catena_depth_selection:convert_depth(Handler, deep),
    ?assertEqual(deep, maps:get(type, Result)).

convert_depth_to_shallow_test() ->
    % Test convert_depth to shallow
    Handler = #{type => deep, effect => test, handler => fun() -> ok end},
    Result = catena_depth_selection:convert_depth(Handler, shallow),
    ?assertEqual(shallow, maps:get(type, Result)).

can_convert_test() ->
    % Test that all handlers can convert
    DeepHandler = #{type => deep, effect => test},
    ShallowHandler = #{type => shallow, effect => test},
    ?assert(catena_depth_selection:can_convert(DeepHandler, deep)),
    ?assert(catena_depth_selection:can_convert(DeepHandler, shallow)),
    ?assert(catena_depth_selection:can_convert(ShallowHandler, deep)),
    ?assert(catena_depth_selection:can_convert(ShallowHandler, shallow)).

%%%=============================================================================
%%% Mixed Depth Handlers Tests
%%%=============================================================================

mixed_handler_scope_test() ->
    % Test creating mixed handler scope
    Handlers = [
        #{type => deep, effect => test1, handler => fun() -> ok end},
        #{type => shallow, effect => test2, handler => fun() -> ok end}
    ],
    Scope = catena_depth_selection:mixed_handler_scope(Handlers, []),
    ?assert(is_map(Scope)),
    ?assertEqual(Handlers, maps:get(handlers, Scope)),
    ?assertEqual(deep_first, maps:get(precedence, Scope)).

depth_precedence_deep_shallow_test() ->
    % Test precedence between deep and shallow
    Deep = #{type => deep, effect => test, handler => fun() -> ok end},
    Shallow = #{type => shallow, effect => test, handler => fun() -> ok end},
    Result = catena_depth_selection:depth_precedence(Deep, Shallow),
    ?assertMatch({deep, Deep}, Result).

depth_precedence_shallow_deep_test() ->
    % Test precedence between shallow and deep
    Shallow = #{type => shallow, effect => test, handler => fun() -> ok end},
    Deep = #{type => deep, effect => test, handler => fun() -> ok end},
    Result = catena_depth_selection:depth_precedence(Shallow, Deep),
    ?assertMatch({deep, Deep}, Result).

depth_precedence_same_type_test() ->
    % Test precedence between same type
    H1 = #{type => deep, effect => test, handler => fun() -> ok end},
    H2 = #{type => deep, effect => test, handler => fun() -> ok end},
    Result = catena_depth_selection:depth_precedence(H1, H2),
    ?assertEqual({equal, both}, Result).

resolve_handler_conflict_no_handler_test() ->
    % Test resolving with no handlers
    Operation = {test, op, []},
    Result = catena_depth_selection:resolve_handler_conflict(Operation, [], []),
    ?assertEqual({unhandled, no_handler}, Result).

resolve_handler_conflict_single_handler_test() ->
    % Test resolving with single handler
    Operation = {test, op, []},
    Handler = #{effect => test, handler => fun() -> ok end, type => deep},
    Result = catena_depth_selection:resolve_handler_conflict(Operation, [Handler], []),
    ?assertMatch({ok, Handler}, Result).

resolve_handler_conflict_multiple_handlers_test() ->
    % Test resolving with multiple matching handlers
    Operation = {test, op, []},
    H1 = #{effect => test, handler => fun() -> h1 end, type => deep},
    H2 = #{effect => test, handler => fun() -> h2 end, type => shallow},
    Result = catena_depth_selection:resolve_handler_conflict(Operation, [H1, H2], []),
    ?assertMatch({ok, _}, Result).

resolve_handler_conflict_deep_first_test() ->
    % Test deep_first precedence
    Operation = {test, op, []},
    H1 = #{effect => test, handler => fun() -> deep_h end, type => deep},
    H2 = #{effect => test, handler => fun() -> shallow_h end, type => shallow},
    Result = catena_depth_selection:resolve_handler_conflict(Operation, [H1, H2], [{precedence, deep_first}]),
    ?assertMatch({ok, H1}, Result).

resolve_handler_conflict_shallow_first_test() ->
    % Test shallow_first precedence
    Operation = {test, op, []},
    H1 = #{effect => test, handler => fun() -> deep_h end, type => deep},
    H2 = #{effect => test, handler => fun() -> shallow_h end, type => shallow},
    Result = catena_depth_selection:resolve_handler_conflict(Operation, [H1, H2], [{precedence, shallow_first}]),
    ?assertMatch({ok, H2}, Result).

%%%=============================================================================
%%% Depth Introspection Tests
%%%=============================================================================

handler_depth_deep_test() ->
    % Test getting depth of deep handler
    Handler = #{type => deep, effect => test, handler => fun() -> ok end, depth => deep},
    ?assertEqual(deep, catena_depth_selection:handler_depth(Handler)).

handler_depth_shallow_test() ->
    % Test getting depth of shallow handler
    Handler = #{type => shallow, effect => test, handler => fun() -> ok end, depth => shallow},
    ?assertEqual(shallow, catena_depth_selection:handler_depth(Handler)).

effective_depth_test() ->
    % Test effective depth is same as handler depth
    Handler = #{type => deep, effect => test},
    ?assertEqual(deep, catena_depth_selection:effective_depth(Handler, #{})).

available_depths_test() ->
    % Test that both depths are available
    Handler = #{type => deep, effect => test},
    Depths = catena_depth_selection:available_depths(Handler),
    ?assert(lists:member(deep, Depths)),
    ?assert(lists:member(shallow, Depths)).

%%%=============================================================================
%%% Utility Functions Tests
%%%=============================================================================

is_deep_handler_true_test() ->
    % Test checking if handler is deep
    Handler = #{type => deep, effect => test},
    ?assert(catena_depth_selection:is_deep_handler(Handler)).

is_deep_handler_false_test() ->
    % Test checking if shallow handler is not deep
    Handler = #{type => shallow, effect => test},
    ?assertNot(catena_depth_selection:is_deep_handler(Handler)).

is_shallow_handler_true_test() ->
    % Test checking if handler is shallow
    Handler = #{type => shallow, effect => test},
    ?assert(catena_depth_selection:is_shallow_handler(Handler)).

is_shallow_handler_false_test() ->
    % Test checking if deep handler is not shallow
    Handler = #{type => deep, effect => test},
    ?assertNot(catena_depth_selection:is_shallow_handler(Handler)).

handler_spec_deep_test() ->
    % Test creating handler spec with deep depth
    HandlerFun = fun() -> ok end,
    Spec = catena_depth_selection:handler_spec(test, deep, HandlerFun),
    ?assertEqual(deep, maps:get(depth, Spec)),
    ?assertEqual(deep, maps:get(type, Spec)).

handler_spec_shallow_test() ->
    % Test creating handler spec with shallow depth
    HandlerFun = fun() -> ok end,
    Spec = catena_depth_selection:handler_spec(test, shallow, HandlerFun),
    ?assertEqual(shallow, maps:get(depth, Spec)),
    ?assertEqual(shallow, maps:get(type, Spec)).

%%%=============================================================================
%%% Round-trip Conversion Tests
%%%=============================================================================

round_trip_deep_to_shallow_test() ->
    % Test deep -> shallow -> deep round-trip
    Handler = #{type => deep, effect => test, handler => fun() -> ok end},
    Shallow = catena_depth_selection:to_shallow(Handler),
    DeepAgain = catena_depth_selection:to_deep(Shallow),
    ?assertEqual(maps:get(type, DeepAgain), deep).

round_trip_shallow_to_deep_test() ->
    % Test shallow -> deep -> shallow round-trip
    Handler = #{type => shallow, effect => test, handler => fun() -> ok end},
    Deep = catena_depth_selection:to_deep(Handler),
    ShallowAgain = catena_depth_selection:to_shallow(Deep),
    ?assertEqual(maps:get(type, ShallowAgain), shallow).
