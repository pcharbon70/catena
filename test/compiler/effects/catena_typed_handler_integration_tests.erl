-module(catena_typed_handler_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_typed_handler_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"handler type definition and checking integration", fun test_handler_type_integration/0},
        {"handler type inference integration", fun test_handler_inference_integration/0},
        {"typed handler execution integration", fun test_typed_execution_integration/0},
        {"complex typed handler scenario", fun test_complex_scenario/0},
        {"handler composition with types", fun test_handler_composition/0},
        {"effect row polymorphism in handlers", fun test_effect_row_polymorphism/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Integration Tests
%%%---------------------------------------------------------------------

test_handler_type_integration() ->
    % Test the full flow of handler type definition and checking
    IntType = int,
    Ops = #{
        get => catena_handler_types:operation_sig([], IntType),
        put => catena_handler_types:operation_sig([IntType], atom)
    },

    % Define handler type
    HandlerType = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(state), Ops),
            atom),
        IntType),

    % Check handler signature
    ?assertEqual(ok, catena_handler_check:check_handler_signature(HandlerType)),

    % Validate handler type
    ?assert(catena_handler_types:is_valid_handler_type(HandlerType)),

    % Check operation coverage
    Implemented = [get, put],
    Missing = catena_handler_check:find_missing_operations(Ops, Implemented),
    ?assertEqual([], Missing).

test_handler_inference_integration() ->
    % Test handler type inference from implementation
    Implementation = #{
        get => fun(_Value, _Resume) -> 42 end,
        put => fun(_Value, _Resume) -> ok end
    },

    % Infer handler type
    {ok, InferredType} = catena_handler_infer:infer_from_implementation(Implementation),

    % Validate inferred type
    ?assert(catena_handler_types:is_handler_type(InferredType)),
    ?assertMatch({type_var, {input, _}}, maps:get(input, InferredType)),
    ?assertEqual({type_var, output}, maps:get(output, InferredType)),

    % Infer operation signatures
    OpSigs = catena_handler_infer:infer_operation_sigs(Implementation),
    ?assertEqual(2, maps:size(OpSigs)),

    % Generalize handler type
    Generalized = catena_handler_infer:generalize_handler(InferredType),
    ?assert(catena_handler_types:is_handler_type(Generalized)).

test_typed_execution_integration() ->
    % Test typed handler execution flow
    AtomType = atom,
    Ops = #{get => catena_handler_types:operation_sig([AtomType], AtomType)},
    HandlerType = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(), Ops),
            AtomType),
        AtomType),

    % Validate before execution
    ?assertEqual(ok, catena_handler_exec:validate_before_execution(HandlerType, dummy)),

    % Check resumption type
    OpSig = maps:get(get, Ops),
    Resumption = catena_resumption:new(fun(V) -> V end, 0, 0, #{}),
    ?assertMatch({ok, _}, catena_handler_exec:enforce_resumption_type(OpSig, dummy, Resumption)),

    GoodHandler = fun(Value, _Resume) -> Value end,
    BadHandler = fun(_Value, _Resume) -> 42 end,
    ?assertMatch({ok, dummy}, catena_handler_exec:execute_typed(get, GoodHandler, HandlerType, dummy)),
    ?assertMatch({error, {return_type_mismatch, _, _}},
        catena_handler_exec:execute_typed(get, BadHandler, HandlerType, dummy)).

test_complex_scenario() ->
    % Test a complex handler with multiple operations and effects
    Effects = catena_row_types:effect_row([state, io]),

    Ops = #{
        get => catena_handler_types:operation_sig([], int),
        put => catena_handler_types:operation_sig([int], atom),
        modify => catena_handler_types:operation_sig([int], int)
    },

    HandlerType = catena_handler_types:with_effects(
        catena_handler_types:with_output(
            catena_handler_types:with_input(
                catena_handler_types:with_operations(
                    catena_handler_types:handler_type(complex), Ops),
                atom),
            int),
        Effects),

    % Validate handler
    ?assertEqual(ok, catena_handler_check:check_handler_signature(HandlerType)),

    % Check all operations
    lists:foreach(fun(Op) ->
        ?assert(maps:is_key(Op, Ops))
    end, [get, put, modify]),

    % Test effect operations
    EffectList = catena_row_types:row_to_list(Effects),
    ?assert(lists:member(state, EffectList)),
    ?assert(lists:member(io, EffectList)).

test_handler_composition() ->
    % Test handler type composition
    IntType = int,
    Ops1 = #{get => catena_handler_types:operation_sig([], IntType)},
    Handler1 = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(h1), Ops1),
            atom),
        IntType),

    Ops2 = #{put => catena_handler_types:operation_sig([IntType], atom)},
    Handler2 = catena_handler_types:with_input(
        catena_handler_types:with_operations(
            catena_handler_types:handler_type(h2), Ops2),
        IntType),

    % Compose handlers
    {ok, Composed} = catena_handler_types:compose_handler_types(Handler1, Handler2),

    % Verify composed handler has operations from both
    ComposedOps = maps:get(operations, Composed),
    ?assertEqual(2, maps:size(ComposedOps)),
    ?assert(maps:is_key(get, ComposedOps)),
    ?assert(maps:is_key(put, ComposedOps)).

test_effect_row_polymorphism() ->
    % Test effect row polymorphism in handlers
    Effects1 = catena_row_types:effect_row([state]),
    Effects2 = catena_row_types:effect_row([state, io]),

    % Create handlers with different effect rows
    Handler1 = catena_handler_types:with_effects(
        catena_handler_types:handler_type(), Effects1),

    % Check effect subsumption
    ?assert(catena_handler_exec:check_subtype(Handler1, Handler1)),
    ?assert(catena_handler_exec:effects_subsumes(Effects1, Effects2)),
    ?assertNot(catena_handler_exec:effects_subsumes(Effects2, Effects1)).
