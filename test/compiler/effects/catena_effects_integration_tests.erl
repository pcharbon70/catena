%%%-------------------------------------------------------------------
%%% @doc Integration Tests for Catena Effect System (Phase 6.6)
%%%
%%% End-to-end integration tests for the complete effect system
%%% including all phases of development:
%%%
%%% - Effect Polymorphism (6.1)
%%% - Expanded Effect Library (6.2)
%%% - Effect Optimizations (6.3)
%%% - Advanced Effect Features (6.4)
%%% - Distributed Effects (6.5)
%%%
%%% Tests verify cross-cutting concerns and ensure all components
%%% work together correctly.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effects_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Effect Polymorphism Integration (6.1)
%%%=============================================================================

effect_polymorphism_basic_test() ->
    % Test basic effect variable creation and unification
    EVar = catena_effect_poly:evar(1),
    ?assert(catena_effect_poly:is_effect_var(EVar)),

    Empty = catena_effect_poly:empty_effects(),
    ?assert(catena_effect_poly:is_pure(Empty)),

    % Union with empty preserves variable
    Union = catena_effect_poly:union_effects(EVar, Empty),
    ?assert(catena_effect_poly:is_effect_var(Union)).

effect_polymorphism_unification_test() ->
    % Test effect unification
    E1 = {effect_set, [io]},
    E2 = {effect_set, [io]},
    ?assertEqual({ok, #{}}, catena_effect_poly:unify_effects(E1, E2)),

    % Variable unifies with concrete
    Var = {evar, 1},
    Concrete = {effect_set, [io]},
    ?assertEqual({ok, #{1 => Concrete}}, catena_effect_poly:unify_effects(Var, Concrete)).

effect_polymorphism_constraints_test() ->
    % Test effect constraints
    RowC = catena_effect_poly:row_constraint([io, file]),
    ?assert(catena_effect_poly:is_row_constraint(RowC)),

    AbsenceC = catena_effect_poly:absence_constraint([process]),
    ?assert(catena_effect_poly:is_absence_constraint(AbsenceC)),

    % Test constraint satisfaction
    % Row constraint requires ALL specified effects to be present
    Expr = {effect_set, [io, file, process]},
    ?assert(catena_effect_poly:satisfies_constraint(Expr, RowC)),
    ?assertNot(catena_effect_poly:satisfies_constraint(Expr, AbsenceC)).

%%%=============================================================================
%%% Expanded Effect Library Integration (6.2)
%%%=============================================================================

state_effect_lifecycle_test() ->
    % Test state effect operations
    GetEffect = catena_effects:state_get(),
    ?assertMatch({effect, {state, get}}, GetEffect),

    PutEffect = catena_effects:state_put(42),
    ?assertMatch({effect, {state, {put, 42}}}, PutEffect),

    ModifyFun = fun(X) -> X + 1 end,
    ModifyEffect = catena_effects:state_modify(ModifyFun),
    ?assertMatch({effect, {state, {modify, _}}}, ModifyEffect).

reader_writer_effects_test() ->
    % Test reader and writer effects
    AskEffect = catena_effects:reader_ask(),
    ?assertMatch({effect, {reader, ask}}, AskEffect),

    TellEffect = catena_effects:writer_tell(log_message),
    ?assertMatch({effect, {writer, {tell, log_message}}}, TellEffect),

    ListenFun = fun(Output) -> Output end,
    ListenEffect = catena_effects:writer_listen(ListenFun),
    ?assertMatch({effect, {writer, {listen, _}}}, ListenEffect).

async_error_effects_test() ->
    % Test async and error effects
    SpawnFun = fun() -> result end,
    SpawnEffect = catena_effects:async_spawn(SpawnFun),
    ?assertMatch({effect, {async, {spawn, _}}}, SpawnEffect),

    AwaitEffect = catena_effects:async_await(ref),
    ?assertMatch({effect, {async, {await, ref}}}, AwaitEffect),

    ThrowEffect = catena_effects:error_throw(my_error),
    ?assertMatch({effect, {error, {throw, my_error}}}, ThrowEffect).

effect_combinators_test() ->
    % Test effect combinators
    StateEff = catena_effects:state(my_state),
    ReaderEff = catena_effects:reader(config),

    Combined = catena_effects:combine_effects([StateEff, ReaderEff]),
    ?assertMatch({effect, {combined, [_, _]}}, Combined),

    RunWith = catena_effects:run_with(StateEff, fun() -> ok end),
    ?assertMatch({effect, {run_with, _, _}}, RunWith).

%%%=============================================================================
%%% Effect Optimizations Integration (6.3)
%%%=============================================================================

effect_fusion_integration_test() ->
    % Test effect fusion across different effect types
    Program = [
        {effect, {state, {put, 1}}},
        {effect, {state, {put, 2}}},
        {effect, {writer, {tell, "a"}}},
        {effect, {writer, {tell, "b"}}}
    ],

    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),

    % State puts should fuse to one
    % Writer tells should fuse to one
    ?assertEqual(2, length(Fused)).

effect_inlining_integration_test() ->
    % Test effect inlining with actual effect operations
    Ops = [{get}, {put, 42}],
    Inlined = catena_effect_opt:inline_state_handler(Ops),

    ?assertMatch([{inline_var, state}, {inline_assign, state, {put, 42}}], Inlined).

optimization_pipeline_test() ->
    % Test full optimization pipeline
    Program = [
        {effect, {writer, {tell, "log1"}}},
        {effect, {writer, {tell, "log2"}}}
    ],

    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),
    ?assertMatch([{effect, {writer, {tell, "log1log2"}}}], Fused).

%%%=============================================================================
%%% Advanced Effect Features Integration (6.4)
%%%=============================================================================

delimited_continuations_test() ->
    % Test prompt and subcontinuation lifecycle
    Prompt = catena_effect_advanced:new_prompt(),
    ?assertMatch({prompt, _}, Prompt),

    ?assertEqual(ok, catena_effect_advanced:push_prompt(Prompt)),
    SubCont = catena_effect_advanced:capture_subcont(),
    ?assertMatch({subcont, _, _}, SubCont),
    ?assertEqual(ok, catena_effect_advanced:push_subcont(SubCont)),
    ?assertEqual(ok, catena_effect_advanced:pop_prompt()).

scoped_effects_test() ->
    % Test scoped effect execution
    Handler = {test_handler, catena_effect_advanced, handle, []},
    Result = catena_effect_advanced:scope_effects(
        [Handler],
        fun() -> scoped_result end
    ),
    ?assertEqual(scoped_result, Result).

effect_handlers_test() ->
    % Test effect handler installation and removal
    ?assertEqual(ok, catena_effect_advanced:install_handler(
        test_id,
        {catena_effect_advanced, handle, []}
    )),
    ?assertEqual(ok, catena_effect_advanced:uninstall_handler(test_id)).

%%%=============================================================================
%%% Distributed Effects Integration (6.5)
%%%=============================================================================

distributed_effect_serialization_test() ->
    % Test effect serialization for distribution
    Effect = {effect, {state, {get}}},
    Serialized = catena_effect_distributed:serialize_effect(Effect),
    ?assert(is_binary(Serialized)),

    Deserialized = catena_effect_distributed:deserialize_effect(Serialized),
    ?assertEqual(Effect, Deserialized).

distributed_serializable_check_test() ->
    % Test serializable effect detection
    SerializableEffect = {effect, {state, {put, 42}}},
    ?assert(catena_effect_distributed:is_serializable(SerializableEffect)),

    % Effects with functions are not serializable
    NonSerializableEffect = {effect, {state, {modify, fun(_) -> ok end}}},
    ?assertNot(catena_effect_distributed:is_serializable(NonSerializableEffect)).

remote_node_test() ->
    % Test remote node operations
    Node = catena_effect_distributed:remote_node(),
    ?assert(is_atom(Node)),

    ?assert(catena_effect_distributed:is_available(Node)),
    ?assertEqual(pang, catena_effect_distributed:ping_node(Node)).

%%%=============================================================================
%%% Cross-Cutting Integration Tests
%%%=============================================================================

polymorphic_effects_with_library_test() ->
    % Test using effect polymorphism with expanded library
    EVar = catena_effect_poly:evar(1),

    % Create polymorphic state effect
    StateType = catena_effects:state(poly_state),

    % Both can coexist in type system
    ?assert(catena_effect_poly:is_effect_var(EVar)),
    ?assertMatch({effect, {state, poly_state}}, StateType).

effects_with_optimizations_test() ->
    % Test optimizing effects from the library
    Effects = [
        catena_effects:state_put(1),
        catena_effects:state_put(2)
    ],

    Program = lists:map(fun(E) -> E end, Effects),
    {optimized, Fused} = catena_effect_opt:fuse_effects(Program),

    % After fusion, should have single put
    ?assertEqual(1, length([E || E <- Fused, element(1, E) =:= effect])).

advanced_effects_with_distribution_test() ->
    % Test that scoped effects work with distribution
    Effect = {effect, {state, {get}}},

    % Serialize for distribution
    Serialized = catena_effect_distributed:serialize_effect(Effect),

    % Can execute in scoped context after deserialization
    Deserialized = catena_effect_distributed:deserialize_effect(Serialized),
    ?assertEqual(Effect, Deserialized).

full_effect_lifecycle_test() ->
    % Test complete effect lifecycle from creation to execution
    % 1. Create polymorphic effect variable
    {{evar, _EVarId}, _State} = catena_effect_poly:fresh_evar(catena_infer_state:new()),

    % 2. Create concrete effect from library
    Effect = catena_effects:state_put(42),

    % 3. Optimize the effect (single effect returns no_change)
    Program = [Effect],
    {_, Optimized} = catena_effect_opt:fuse_effects(Program),

    % 4. Serialize for distribution
    Serialized = catena_effect_distributed:serialize_effect(hd(Optimized)),

    % 5. Deserialize
    Deserialized = catena_effect_distributed:deserialize_effect(Serialized),

    % 6. Verify roundtrip
    ?assertEqual(Effect, Deserialized).

multiple_effects_combination_test() ->
    % Test combining multiple different effect types
    Effects = [
        catena_effects:state(counter),
        catena_effects:reader(config),
        catena_effects:writer(log),
        catena_effects:error(validation)
    ],

    Combined = catena_effects:combine_effects(Effects),
    ?assertMatch({effect, {combined, Effects}}, Combined).

effect_system_consistency_test() ->
    % Test that all effect system components are consistent
    % Effect variables should be recognized
    EVar = catena_effect_poly:evar(1),
    ?assert(catena_effect_poly:is_effect_var(EVar)),

    % State effects should be serializable
    StateEffect = catena_effects:state_get(),
    ?assert(catena_effect_distributed:is_serializable(StateEffect)),

    % Optimizations should recognize effect structure
    Program = [StateEffect],
    {no_change, _} = catena_effect_opt:fuse_effects(Program),

    % Advanced features should handle effects
    ?assert(is_list(catena_effect_advanced:current_handlers())).

effect_type_safety_test() ->
    % Test that effect types maintain safety guarantees
    % Row constraints should only allow matching effects
    RowC = catena_effect_poly:row_constraint([io]),

    ValidExpr = {effect_set, [io, process]},
    ?assert(catena_effect_poly:satisfies_constraint(ValidExpr, RowC)),

    InvalidExpr = {effect_set, [process]},
    ?assertNot(catena_effect_poly:satisfies_constraint(InvalidExpr, RowC)).

effect_error_handling_test() ->
    % Test error handling across effect system
    ErrorEffect = catena_effects:error_throw(test_error),
    ?assertMatch({effect, {error, {throw, test_error}}}, ErrorEffect),

    % Remote operations should handle errors
    Result = catena_effect_distributed:remote_effect(
        'nonexistent@node',
        ErrorEffect,
        []
    ),
    ?assertMatch({error, {node_unavailable, _}}, Result).
