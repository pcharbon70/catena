-module(catena_handler_infer_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_handler_infer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"handler type inference", fun test_infer_handler_type/0},
        {"operation signature inference", fun test_infer_operation_sigs/0},
        {"resumption type inference", fun test_infer_resumption_type/0},
        {"resumption parameter type inference", fun test_infer_resumption_param_type/0},
        {"resumption return type inference", fun test_infer_resumption_return_type/0},
        {"resumption effects inference", fun test_infer_resumption_effects/0},
        {"context type inference", fun test_infer_context_type/0},
        {"effect subtraction", fun test_infer_effect_subtraction/0},
        {"context validation", fun test_validate_context/0},
        {"context optimization", fun test_optimize_context/0},
        {"handler generalization", fun test_generalize_handler/0},
        {"handler instantiation", fun test_instantiate_handler/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Handler Type Inference Tests
%%%---------------------------------------------------------------------

test_infer_handler_type() ->
    % Test inferring handler type from implementation map
    Implementation = #{
        get => fun(Value, _Resume) -> Value end,
        put => fun(_Value, Resume) -> catena_resumption:resume(Resume, ok) end
    },

    {ok, HandlerType} = catena_handler_infer:infer_from_implementation(Implementation),
    ?assert(catena_handler_types:is_handler_type(HandlerType)),
    ?assert(maps:is_key(operations, HandlerType)),
    ?assert(maps:is_key(input, HandlerType)),
    ?assert(maps:is_key(output, HandlerType)),
    ?assertMatch({type_var, {input, _}}, maps:get(input, HandlerType)).

%%%---------------------------------------------------------------------
%%% Operation Signature Inference Tests
%%%---------------------------------------------------------------------

test_infer_operation_sigs() ->
    Implementation = #{
        get => fun(Value, _Resume) -> Value end,
        put => fun(_Value, Resume) -> catena_resumption:resume(Resume, ok) end
    },

    OpSigs = catena_handler_infer:infer_operation_sigs(Implementation),
    ?assertEqual(2, maps:size(OpSigs)),
    ?assert(maps:is_key(get, OpSigs)),
    ?assert(maps:is_key(put, OpSigs)).

%%%---------------------------------------------------------------------
%%% Resumption Type Inference Tests
%%%---------------------------------------------------------------------

test_infer_resumption_type() ->
    IntType = int,
    AtomType = atom,
    Effects = catena_row_types:effect_row([state]),

    OpSig = catena_handler_types:operation_sig([IntType], AtomType),
    ResumptionSig = catena_handler_infer:infer_resumption_type(OpSig, Effects),

    ?assert(catena_handler_types:is_operation_sig(ResumptionSig)),
    ?assertMatch([_], maps:get(params, ResumptionSig)),
    ?assert(maps:is_key(result, ResumptionSig)),
    ?assert(maps:is_key(effects, ResumptionSig)).

test_infer_resumption_param_type() ->
    IntType = int,
    ResultType = IntType,

    ParamType = catena_handler_infer:infer_resumption_param_type(ResultType),
    ?assertEqual(IntType, ParamType).

test_infer_resumption_return_type() ->
    IntType = int,
    EmptyEffects = catena_row_types:empty_row(),

    % Pure operation returns result type directly
    ReturnType = catena_handler_infer:infer_resumption_return_type(IntType, EmptyEffects),
    ?assertEqual(IntType, ReturnType),

    % Operation with effects returns polymorphic result
    Effects = catena_row_types:effect_row([state]),
    ReturnType2 = catena_handler_infer:infer_resumption_return_type(IntType, Effects),
    ?assertMatch({type_var, resumption_return}, ReturnType2).

test_infer_resumption_effects() ->
    OpEffects = catena_row_types:effect_row([io]),
    HandlerEffects = catena_row_types:effect_row([state]),

    Combined = catena_handler_infer:infer_resumption_effects(OpEffects, HandlerEffects),
    CombinedList = catena_row_types:row_to_list(Combined),

    ?assert(lists:member(io, CombinedList)),
    ?assert(lists:member(state, CombinedList)).

%%%---------------------------------------------------------------------
%%% Handler Context Inference Tests
%%%---------------------------------------------------------------------

test_infer_context_type() ->
    Effects = catena_row_types:effect_row([state]),
    HandlerType = catena_handler_types:with_effects(
        catena_handler_types:handler_type(), Effects),

    OuterEffects = catena_row_types:effect_row([state, io]),

    {ok, ContextHandler} = catena_handler_infer:infer_context_type(HandlerType, OuterEffects),
    ?assert(catena_handler_types:is_handler_type(ContextHandler)),
    ?assertEqual([io], catena_row_types:row_to_list(maps:get(effects, ContextHandler))).

test_infer_effect_subtraction() ->
    Effects = catena_row_types:effect_row([state, io]),
    ToSubtract = catena_row_types:effect_row([state]),

    {ok, Remaining} = catena_handler_infer:infer_effect_subtraction(ToSubtract, Effects),
    RemainingList = catena_row_types:row_to_list(Remaining),

    ?assert(lists:member(io, RemainingList)),
    ?assertNot(lists:member(state, RemainingList)),

    % Error case: subtracting non-existent effect
    Invalid = catena_row_types:effect_row([nonexistent]),
    ?assertMatch({error, _}, catena_handler_infer:infer_effect_subtraction(Invalid, Effects)).

test_validate_context() ->
    Ops = #{get => catena_handler_types:operation_sig([], int)},
    HandlerType = catena_handler_types:with_operations(
        catena_handler_types:handler_type(), Ops),

    ?assertEqual(ok, catena_handler_infer:validate_context(HandlerType)),

    % Invalid context
    InvalidHandler = #{invalid => handler},
    ?assertMatch({error, _}, catena_handler_infer:validate_context(InvalidHandler)).

test_optimize_context() ->
    EmptyEffects = catena_row_types:empty_row(),
    HandlerType = catena_handler_types:with_effects(
        catena_handler_types:handler_type(), EmptyEffects),

    Optimized = catena_handler_infer:optimize_context(HandlerType),
    Constraints = maps:get(constraints, Optimized),

    ?assert(maps:get(pure, Constraints, false)).

%%%---------------------------------------------------------------------
%%% Type Generalization Tests
%%%---------------------------------------------------------------------

test_generalize_handler() ->
    Ops = #{
        get => catena_handler_types:operation_sig([int], int)
    },
    HandlerType = catena_handler_types:with_operations(
        catena_handler_types:handler_type(), Ops),

    Generalized = catena_handler_infer:generalize_handler(HandlerType),
    ?assert(catena_handler_types:is_handler_type(Generalized)),
    ?assert(maps:is_key(operations, Generalized)).

test_instantiate_handler() ->
    Ops = #{
        get => catena_handler_types:operation_sig([int], int)
    },
    HandlerType = catena_handler_types:with_operations(
        catena_handler_types:handler_type(), Ops),

    Instantiated = catena_handler_infer:instantiate_handler(HandlerType),
    ?assert(catena_handler_types:is_handler_type(Instantiated)).
