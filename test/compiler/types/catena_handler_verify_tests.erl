%%%-------------------------------------------------------------------
%%% @doc Tests for Effect Handler Verification
%%%
%%% Tests the catena_handler_verify module which verifies algebraic
%%% effect handlers.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_verify_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Helpers
%%%===================================================================

%% Helper to create test effect declarations
make_effect(Name, Operations) ->
    {effect, Name, Operations, unknown}.

make_operation(Name, ParamTypes, ReturnType) ->
    {operation, Name, ParamTypes, ReturnType, unknown}.

make_handler_clause(Effect, Op, NumParams) ->
    Params = lists:duplicate(NumParams, pattern),
    {handler_case, Effect, Op, Params, body, unknown}.

%%%===================================================================
%%% Exhaustiveness Checking Tests
%%%===================================================================

exhaustive_handler_test() ->
    % Effect with two operations
    FileIO = make_effect('FileIO', [
        make_operation(read, [{tcon, string}], {tcon, string}),
        make_operation(write, [{tcon, string}, {tcon, string}], {tcon, unit})
    ]),

    % Handler that covers both operations
    Clauses = [
        make_handler_clause('FileIO', read, 1),
        make_handler_clause('FileIO', write, 2)
    ],

    Result = catena_handler_verify:check_exhaustiveness(FileIO, Clauses),
    ?assertEqual([], Result).

missing_operation_test() ->
    % Effect with two operations
    FileIO = make_effect('FileIO', [
        make_operation(read, [{tcon, string}], {tcon, string}),
        make_operation(write, [{tcon, string}, {tcon, string}], {tcon, unit})
    ]),

    % Handler that only covers 'read', missing 'write'
    Clauses = [
        make_handler_clause('FileIO', read, 1)
    ],

    Result = catena_handler_verify:check_exhaustiveness(FileIO, Clauses),
    ?assertEqual(1, length(Result)),
    ?assertMatch([{missing_operation, 'FileIO', write, _}], Result).

multiple_missing_operations_test() ->
    % Effect with three operations
    DB = make_effect('Database', [
        make_operation(query, [{tcon, string}], {tcon, list}),
        make_operation(insert, [{tcon, record}], {tcon, unit}),
        make_operation(delete, [{tcon, integer}], {tcon, unit})
    ]),

    % Empty handler (all operations missing)
    Clauses = [],

    Result = catena_handler_verify:check_exhaustiveness(DB, Clauses),
    ?assertEqual(3, length(Result)).

%%%===================================================================
%%% Operation Type Checking Tests
%%%===================================================================

correct_arity_test() ->
    Operations = [
        make_operation(read, [{tcon, string}], {tcon, string})
    ],

    Clause = make_handler_clause('FileIO', read, 1),

    Result = catena_handler_verify:check_operation_types(Clause, Operations, 'FileIO'),
    ?assertEqual([], Result).

arity_mismatch_test() ->
    Operations = [
        make_operation(read, [{tcon, string}], {tcon, string})
    ],

    % Handler has 2 parameters, but operation expects 1
    Clause = make_handler_clause('FileIO', read, 2),

    Result = catena_handler_verify:check_operation_types(Clause, Operations, 'FileIO'),
    ?assertEqual(1, length(Result)),
    ?assertMatch([{arity_mismatch, 'FileIO', read, 1, 2, _}], Result).

unknown_operation_test() ->
    Operations = [
        make_operation(read, [{tcon, string}], {tcon, string})
    ],

    % Handler for operation that doesn't exist
    Clause = make_handler_clause('FileIO', write, 2),

    Result = catena_handler_verify:check_operation_types(Clause, Operations, 'FileIO'),
    ?assertEqual(1, length(Result)),
    ?assertMatch([{unknown_operation, 'FileIO', write, _}], Result).

%%%===================================================================
%%% Full Handler Verification Tests
%%%===================================================================

verify_correct_handler_test() ->
    % Effect declaration
    FileIO = make_effect('FileIO', [
        make_operation(read, [{tcon, string}], {tcon, string}),
        make_operation(write, [{tcon, string}, {tcon, string}], {tcon, unit})
    ]),

    % Correct handler
    Clauses = [
        make_handler_clause('FileIO', read, 1),
        make_handler_clause('FileIO', write, 2)
    ],

    Result = catena_handler_verify:verify_handler(FileIO, Clauses),
    ?assertEqual({ok, verified}, Result).

verify_incomplete_handler_test() ->
    % Effect declaration
    FileIO = make_effect('FileIO', [
        make_operation(read, [{tcon, string}], {tcon, string}),
        make_operation(write, [{tcon, string}, {tcon, string}], {tcon, unit})
    ]),

    % Incomplete handler (missing write)
    Clauses = [
        make_handler_clause('FileIO', read, 1)
    ],

    Result = catena_handler_verify:verify_handler(FileIO, Clauses),
    ?assertMatch({error, [_]}, Result).

verify_handler_with_wrong_arity_test() ->
    % Effect declaration
    FileIO = make_effect('FileIO', [
        make_operation(read, [{tcon, string}], {tcon, string})
    ]),

    % Handler with wrong arity
    Clauses = [
        make_handler_clause('FileIO', read, 2)  % Expected 1, got 2
    ],

    Result = catena_handler_verify:verify_handler(FileIO, Clauses),
    ?assertMatch({error, [_]}, Result),

    {error, [Error]} = Result,
    ?assertMatch({arity_mismatch, _, _, _, _, _}, Error).

%%%===================================================================
%%% Effect Resolution Tests
%%%===================================================================

resolve_single_effect_test() ->
    % Start with FileIO and Process effects
    Effects = {effect_set, ['FileIO', 'Process']},

    % Handle FileIO
    Result = catena_handler_verify:resolve_handled_effects(Effects, ['FileIO']),

    % Should have only Process remaining
    ?assertEqual({effect_set, ['Process']}, Result).

resolve_multiple_effects_test() ->
    % Start with three effects
    Effects = {effect_set, ['FileIO', 'Network', 'Process']},

    % Handle FileIO and Network
    Result = catena_handler_verify:resolve_handled_effects(Effects,
                                                          ['FileIO', 'Network']),

    % Should have only Process remaining
    ?assertEqual({effect_set, ['Process']}, Result).

resolve_all_effects_test() ->
    % Start with effects
    Effects = {effect_set, ['FileIO', 'Process']},

    % Handle all effects
    Result = catena_handler_verify:resolve_handled_effects(Effects,
                                                          ['FileIO', 'Process']),

    % Should be empty (pure)
    ?assertEqual({effect_set, []}, Result).

resolve_no_effects_test() ->
    % Start with effects
    Effects = {effect_set, ['FileIO', 'Process']},

    % Handle none
    Result = catena_handler_verify:resolve_handled_effects(Effects, []),

    % Should remain unchanged
    ?assertEqual({effect_set, ['FileIO', 'Process']}, Result).

%%%===================================================================
%%% Error Formatting Tests
%%%===================================================================

format_missing_operation_error_test() ->
    Error = {missing_operation, 'FileIO', read, {file, "test.cat", 10, 5}},
    Msg = catena_handler_verify:format_handler_error(Error),

    ?assert(string:str(Msg, "Missing handler") > 0),
    ?assert(string:str(Msg, "FileIO") > 0),
    ?assert(string:str(Msg, "read") > 0).

format_arity_mismatch_error_test() ->
    Error = {arity_mismatch, 'FileIO', read, 1, 2, unknown},
    Msg = catena_handler_verify:format_handler_error(Error),

    ?assert(string:str(Msg, "Arity mismatch") > 0),
    ?assert(string:str(Msg, "Expected 1") > 0),
    ?assert(string:str(Msg, "got 2") > 0).

format_unknown_operation_error_test() ->
    Error = {unknown_operation, 'FileIO', unknown_op, unknown},
    Msg = catena_handler_verify:format_handler_error(Error),

    ?assert(string:str(Msg, "Unknown operation") > 0),
    ?assert(string:str(Msg, "FileIO") > 0).

%%%===================================================================
%%% Complex Scenarios
%%%===================================================================

multiple_effects_scenario_test() ->
    % Simulate handling multiple effects
    FileIO = make_effect('FileIO', [
        make_operation(read, [{tcon, string}], {tcon, string})
    ]),

    Network = make_effect('Network', [
        make_operation(request, [{tcon, url}], {tcon, response})
    ]),

    % Handlers for both effects
    FileIO_Clauses = [make_handler_clause('FileIO', read, 1)],
    Network_Clauses = [make_handler_clause('Network', request, 1)],

    Result1 = catena_handler_verify:verify_handler(FileIO, FileIO_Clauses),
    Result2 = catena_handler_verify:verify_handler(Network, Network_Clauses),

    ?assertEqual({ok, verified}, Result1),
    ?assertEqual({ok, verified}, Result2).

nested_effect_resolution_test() ->
    % Test effect resolution in nested handlers
    % Outer handler resolves FileIO
    % Inner handler resolves Network
    % Result should have neither

    StartEffects = {effect_set, ['FileIO', 'Network', 'Process']},

    AfterOuter = catena_handler_verify:resolve_handled_effects(StartEffects,
                                                              ['FileIO']),
    ?assertEqual({effect_set, ['Network', 'Process']}, AfterOuter),

    AfterInner = catena_handler_verify:resolve_handled_effects(AfterOuter,
                                                              ['Network']),
    ?assertEqual({effect_set, ['Process']}, AfterInner).
