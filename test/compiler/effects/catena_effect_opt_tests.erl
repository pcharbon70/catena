%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_effect_opt (Phase 6.3)
%%%
%%% Tests for effect optimization passes including:
%%% - Effect fusion (combining operations of same type)
%%% - Effect inlining (replacing abstractions with direct impls)
%%% - Static effect resolution (compile-time handler resolution)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_opt_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Effect Fusion Tests
%%%=============================================================================

fuse_state_put_operations_test() ->
    % Two sequential State.put operations should fuse to one
    Program = [
        {effect, {state, {put, 1}}},
        {effect, {state, {put, 2}}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    ?assertMatch([{effect, {state, {put, 2}}}], Fused).

fuse_state_get_and_put_test() ->
    % State.get followed by State.put should fuse to get_and_put
    Program = [
        {effect, {state, {get}}},
        {effect, {state, {put, 42}}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    ?assertMatch([{effect, {state, {get_and_put, _}}}], Fused).

fuse_writer_tell_operations_test() ->
    % Two sequential Writer.tell operations should fuse
    Program = [
        {effect, {writer, {tell, "hello"}}},
        {effect, {writer, {tell, " world"}}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    ?assertMatch([{effect, {writer, {tell, "hello world"}}}], Fused).

fuse_writer_tell_binary_test() ->
    % Binary Writer.tell operations should fuse
    Program = [
        {effect, {writer, {tell, <<"hello">>}}},
        {effect, {writer, {tell, <<" world">>}}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    ?assertMatch([{effect, {writer, {tell, <<"hello world">>}}}], Fused).

fuse_writer_tell_lists_test() ->
    % List Writer.tell operations should concatenate
    Program = [
        {effect, {writer, {tell, [1, 2]}}},
        {effect, {writer, {tell, [3, 4]}}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    ?assertMatch([{effect, {writer, {tell, [1, 2, 3, 4]}}}], Fused).

fuse_no_change_for_different_effects_test() ->
    % Different effect types should not fuse
    Program = [
        {effect, {state, {put, 1}}},
        {effect, {writer, {tell, "log"}}}
    ],
    {no_change, _} = catena_effect_opt:fuse_effects(Program).

fuse_no_change_for_non_fusable_ops_test() ->
    % State.get followed by State.get should not fuse
    Program = [
        {effect, {state, {get}}},
        {effect, {state, {get}}}
    ],
    {no_change, _} = catena_effect_opt:fuse_effects(Program).

can_fuse_put_put_test() ->
    ?assert(catena_effect_opt:can_fuse({put, 1}, {put, 2})).

can_fuse_tell_tell_test() ->
    ?assert(catena_effect_opt:can_fuse({tell, "a"}, {tell, "b"})).

can_fuse_get_put_test() ->
    ?assert(catena_effect_opt:can_fuse({get}, {put, 1})).

cannot_fuse_get_get_test() ->
    ?assertNot(catena_effect_opt:can_fuse({get}, {get})).

cannot_fuse_different_ops_test() ->
    ?assertNot(catena_effect_opt:can_fuse({put, 1}, {get})).

fuse_state_effects_multiple_puts_test() ->
    Ops = [{put, 1}, {put, 2}, {put, 3}],
    Fused = catena_effect_opt:fuse_state_effects(Ops),
    ?assertMatch([{put, 3}], Fused).

fuse_writer_effects_multiple_tells_test() ->
    Ops = [{tell, "a"}, {tell, "b"}, {tell, "c"}],
    Fused = catena_effect_opt:fuse_writer_effects(Ops),
    ?assertMatch([{tell, "abc"}], Fused).

%%%=============================================================================
%%% Effect Inlining Tests
%%%=============================================================================

inline_state_get_test() ->
    Ops = [{get}],
    Inlined = catena_effect_opt:inline_state_handler(Ops),
    ?assertMatch([{inline_var, state}], Inlined).

inline_state_put_test() ->
    Ops = [{put, 42}],
    Inlined = catena_effect_opt:inline_state_handler(Ops),
    ?assertMatch([{inline_assign, state, {put, 42}}], Inlined).

inline_state_modify_test() ->
    Fun = fun(X) -> X + 1 end,
    Ops = [{modify, Fun}],
    Inlined = catena_effect_opt:inline_state_handler(Ops),
    ?assertMatch([{inline_modify, state, Fun}], Inlined).

inline_reader_ask_test() ->
    Ops = [ask],
    Inlined = catena_effect_opt:inline_reader_handler(Ops),
    ?assertMatch([{inline_var, env}], Inlined).

inline_reader_local_test() ->
    Fun = fun() -> ok end,
    Ops = [{local, Fun}],
    Inlined = catena_effect_opt:inline_reader_handler(Ops),
    ?assertMatch([{inline_local, env, {local, Fun}}], Inlined).

inline_reader_ask_local_test() ->
    Fun = fun(E) -> E end,
    Ops = [{ask_local, Fun}],
    Inlined = catena_effect_opt:inline_reader_handler(Ops),
    ?assertMatch([{inline_apply, env, Fun}], Inlined).

should_inline_small_handler_test() ->
    ?assert(catena_effect_opt:should_inline({small_handler, 3})).

should_inline_primitive_handler_test() ->
    ?assert(catena_effect_opt:should_inline({primitive, 2})).

should_not_inline_large_handler_test() ->
    ?assertNot(catena_effect_opt:should_inline({large_handler, 10})).

inline_handlers_program_test() ->
    Program = [
        {effect, {state, {get}}},
        {effect, {state, {put, 1}}}
    ],
    {optimized, Inlined} = catena_effect_opt:inline_handlers(Program),
    ?assertMatch({effect, {state, {inline_var, state}}}, lists:nth(1, Inlined)).

inline_handlers_no_change_test() ->
    Program = [value, 42],
    {no_change, _} = catena_effect_opt:inline_handlers(Program).

%%%=============================================================================
%%% Static Effect Resolution Tests
%%%=============================================================================

is_static_builtin_handler_test() ->
    ?assert(catena_effect_opt:is_static_handler({builtin, state})).

is_static_primitive_handler_test() ->
    ?assert(catena_effect_opt:is_static_handler({primitive, reader})).

is_not_static_user_handler_test() ->
    ?assertNot(catena_effect_opt:is_static_handler({user, custom})).

is_not_static_atom_test() ->
    ?assertNot(catena_effect_opt:is_static_handler(custom_handler)).

resolve_known_handler_test() ->
    ?assertMatch({ok, state_handler}, catena_effect_opt:resolve_handler(state_handler, #{})).

resolve_unknown_handler_test() ->
    ?assertEqual(dynamic, catena_effect_opt:resolve_handler(unknown_handler, #{})).

resolve_static_effects_builtin_test() ->
    Program = [
        {effect, {run_with, {effect, {state, counter}}, fun() -> ok end}}
    ],
    {optimized, Resolved} = catena_effect_opt:resolve_static_effects(Program),
    ?assertMatch({effect, {run_with_static, state}}, lists:nth(1, Resolved)).

resolve_static_effects_reader_test() ->
    Program = [
        {effect, {run_with, {effect, {reader, config}}, fun() -> ok end}}
    ],
    {optimized, Resolved} = catena_effect_opt:resolve_static_effects(Program),
    ?assertMatch({effect, {run_with_static, reader}}, lists:nth(1, Resolved)).

resolve_static_effects_no_change_test() ->
    Program = [value, 42],
    {no_change, _} = catena_effect_opt:resolve_static_effects(Program).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

fuse_and_inline_integration_test() ->
    % Test fusion followed by inlining
    Program = [
        {effect, {state, {put, 1}}},
        {effect, {state, {put, 2}}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    {optimized, Inlined} = catena_effect_opt:inline_handlers(Fused),
    ?assertMatch([{effect, {state, {inline_assign, state, {put, 2}}}}], Inlined).

full_optimization_pipeline_test() ->
    % Test all three optimization passes
    Program = [
        {effect, {writer, {tell, "log1"}}},
        {effect, {writer, {tell, "log2"}}},
        {effect, {run_with, {effect, {state, s}}, fun() -> ok end}}
    ],
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    % After fusing, we should have 2 elements (writer fused, run_with unchanged)
    ?assertEqual(2, length(Fused)),
    % The first element should be the fused writer tell
    ?assertMatch({effect, {writer, {tell, "log1log2"}}}, lists:nth(1, Fused)).
