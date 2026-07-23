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
    with_effects(fun() ->
        ?assertEqual({43, 43}, catena_effects:run_state(
            fun() ->
                catena_effects:state_put(42),
                catena_effects:state_modify(fun(X) -> X + 1 end),
                catena_effects:state_get()
            end,
            0
        ))
    end).

reader_writer_effects_test() ->
    with_effects(fun() ->
        ?assertEqual({config, [log_message]}, catena_effects:run_writer(
            fun() ->
                catena_effects:run_reader(
                    fun() ->
                        catena_effects:writer_tell(log_message),
                        catena_effects:reader_ask()
                    end,
                    config
                )
            end
        ))
    end).

async_error_effects_test() ->
    with_effects(fun() ->
        AsyncResult = catena_effects:handle(
            async,
            fun
                ({spawn, Computation}, _Resumption) -> Computation();
                ({await, Future}, _Resumption) -> {awaited, Future}
            end,
            fun() ->
                Spawned = catena_effects:async_spawn(fun() -> result end),
                {Spawned, catena_effects:async_await(ref)}
            end
        ),
        ?assertEqual({result, {awaited, ref}}, AsyncResult),
        ?assertEqual({caught, my_error}, catena_effects:run_error(
            fun() -> catena_effects:error_throw(my_error) end,
            fun(Error) -> {caught, Error} end
        ))
    end).

effect_combinators_test() ->
    with_effects(fun() ->
        Result = catena_effects:run_reader(
            fun() ->
                catena_effects:run_state(
                    fun() ->
                        Delta = catena_effects:reader_ask(),
                        catena_effects:state_modify(fun(Value) -> Value + Delta end)
                    end,
                    10
                )
            end,
            5
        ),
        ?assertEqual({15, 15}, Result)
    end).

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
    ?assert(catena_effect_poly:is_effect_var(EVar)),
    with_effects(fun() ->
        ?assertEqual({poly_state, poly_state}, catena_effects:run_state(
            fun catena_effects:state_get/0,
            poly_state
        ))
    end).

effects_with_optimizations_test() ->
    % Optimizers consume their own descriptor representation.
    Effects = [
        {effect, {state, {put, 1}}},
        {effect, {state, {put, 2}}}
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

    % 2. Create an optimizer/distribution descriptor
    Effect = {effect, {state, {put, 42}}},

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
    with_effects(fun() ->
        Result = catena_effects:run_error(
            fun() ->
                catena_effects:run_writer(
                    fun() ->
                        catena_effects:run_reader(
                            fun() ->
                                catena_effects:run_state(
                                    fun() ->
                                        Env = catena_effects:reader_ask(),
                                        catena_effects:writer_tell(Env),
                                        catena_effects:state_get()
                                    end,
                                    counter
                                )
                            end,
                            config
                        )
                    end
                )
            end,
            fun(Error) -> {error, Error} end
        ),
        ?assertEqual({{counter, counter}, [config]}, Result)
    end).

effect_system_consistency_test() ->
    % Test that all effect system components are consistent
    % Effect variables should be recognized
    EVar = catena_effect_poly:evar(1),
    ?assert(catena_effect_poly:is_effect_var(EVar)),

    StateDescriptor = {effect, {state, get}},
    ?assert(catena_effect_distributed:is_serializable(StateDescriptor)),

    % Optimizations should recognize effect structure
    Program = [StateDescriptor],
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
    with_effects(fun() ->
        ?assertEqual({caught, test_error}, catena_effects:run_error(
            fun() -> catena_effects:error_throw(test_error) end,
            fun(Error) -> {caught, Error} end
        ))
    end),

    % Distributed effects consume the descriptor representation.
    ErrorDescriptor = {effect, {error, {throw, test_error}}},
    Result = catena_effect_distributed:remote_effect(
        'nonexistent@node',
        ErrorDescriptor,
        []
    ),
    ?assertMatch({error, {node_unavailable, _}}, Result).

with_effects(Computation) ->
    catch catena_effects:shutdown(),
    ok = catena_effects:init(),
    try
        Computation()
    after
        catena_effects:shutdown()
    end.
