-module(catena_handler_check_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_handler_check_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"handler type checking", fun test_check_handler_type/0},
        {"operation coverage checking", fun test_operation_coverage/0},
        {"missing operations detection", fun test_missing_operations/0},
        {"redundant operations detection", fun test_redundant_operations/0},
        {"coverage report generation", fun test_coverage_report/0},
        {"return type checking", fun test_return_type_checking/0},
        {"resumption type checking", fun test_resumption_type_checking/0},
        {"handler effects checking", fun test_handler_effects_checking/0},
        {"handler signature checking", fun test_handler_signature_checking/0},
        {"error formatting", fun test_error_formatting/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Handler Type Checking Tests
%%%---------------------------------------------------------------------

test_check_handler_type() ->
    % Create a valid handler type
    IntType = int,
    Ops = #{
        get => catena_handler_types:operation_sig([], IntType),
        put => catena_handler_types:operation_sig([IntType], atom)
    },
    HandlerType = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(test), Ops),
            atom),
        atom),

    % Check signature (doesn't require module)
    ?assertEqual(ok, catena_handler_check:check_handler_signature(HandlerType)),

    % Invalid handler type
    InvalidHandler = #{invalid => handler},
    ?assertMatch({error, {invalid_handler, _}},
        catena_handler_check:check_handler_type(InvalidHandler, dummy)).

%%%---------------------------------------------------------------------
%%% Operation Coverage Tests
%%%---------------------------------------------------------------------

test_operation_coverage() ->
    % Test find_missing and find_redundant directly
    ExpectedOps = #{get => #{}, put => {}, send => {}},

    % Test with complete implementation
    CompleteOps = [get, put, send],
    Missing = catena_handler_check:find_missing_operations(ExpectedOps, CompleteOps),
    ?assertEqual([], Missing),

    % Test with incomplete implementation
    PartialOps = [get, send],
    Missing2 = catena_handler_check:find_missing_operations(ExpectedOps, PartialOps),
    ?assertEqual([put], Missing2).

test_missing_operations() ->
    ExpectedOps = #{get => #{}, put => {}, send => {}},
    Implemented = [get, send],  % missing 'put'

    Missing = catena_handler_check:find_missing_operations(ExpectedOps, Implemented),
    ?assertEqual([put], Missing),

    % Complete implementation
    Complete = [get, put, send],
    ?assertEqual([], catena_handler_check:find_missing_operations(ExpectedOps, Complete)).

test_redundant_operations() ->
    ExpectedOps = #{get => #{}, put => {}},
    Implemented = [get, put, send, receive_msg],  % 'send' and 'receive_msg' are redundant

    Redundant = catena_handler_check:find_redundant_operations(ExpectedOps, Implemented),
    ?assert(lists:member(send, Redundant)),
    ?assert(lists:member(receive_msg, Redundant)),

    % No redundant operations
    Exact = [get, put],
    ?assertEqual([], catena_handler_check:find_redundant_operations(ExpectedOps, Exact)).

%%%---------------------------------------------------------------------
%%% Coverage Report Tests
%%%---------------------------------------------------------------------

test_coverage_report() ->
    % Simulate coverage report by testing the helper functions
    Ops = #{get => #{}, put => {}, send => {}},
    Implemented = [get, send],  % 2 out of 3

    Missing = catena_handler_check:find_missing_operations(Ops, Implemented),
    Redundant = catena_handler_check:find_redundant_operations(Ops, Implemented),

    ?assertEqual(1, length(Missing)),
    ?assertEqual([put], Missing),
    ?assertEqual(0, length(Redundant)).

%%%---------------------------------------------------------------------
%%% Return Type Checking Tests
%%%---------------------------------------------------------------------

test_return_type_checking() ->
    IntType = int,

    % Matching types
    ?assertEqual(ok, catena_handler_check:check_return_type(IntType, 42)),

    % Type mismatch
    ?assertMatch({error, {return_type_mismatch, _, _}},
        catena_handler_check:check_return_type(IntType, atom)),

    % Binary type
    ?assertEqual(ok, catena_handler_check:check_return_type(binary, <<"hello">>)),

    % Atom type
    ?assertEqual(ok, catena_handler_check:check_return_type(atom, ok)).

%%%---------------------------------------------------------------------
%%% Resumption Type Checking Tests
%%%---------------------------------------------------------------------

test_resumption_type_checking() ->
    IntType = int,
    AtomType = atom,

    % Valid resumption: int param -> atom result
    OpSig = catena_handler_types:operation_sig([IntType], AtomType),
    ?assertEqual(ok, catena_handler_check:check_resumption_type(OpSig, 42, ok)),

    % Parameter type mismatch
    ?assertMatch({error, _},
        catena_handler_check:check_resumption_type(OpSig, atom, ok)),

    % Return type mismatch
    ?assertMatch({error, {return_type_mismatch, _, _}},
        catena_handler_check:check_resumption_type(OpSig, 42, 42)).

%%%---------------------------------------------------------------------
%%% Handler Effects Checking Tests
%%%---------------------------------------------------------------------

test_handler_effects_checking() ->
    Effects = catena_row_types:effect_row([state, io]),
    HandlerType = catena_handler_types:with_effects(
        catena_handler_types:handler_type(), Effects),

    % Matching effects
    SameEffects = catena_row_types:effect_row([state, io]),
    ?assertEqual(ok, catena_handler_check:check_handler_effects(HandlerType, SameEffects)),

    % Different effects (order shouldn't matter)
    Reordered = catena_row_types:effect_row([io, state]),
    ?assertEqual(ok, catena_handler_check:check_handler_effects(HandlerType, Reordered)),

    % Mismatched effects
    Different = catena_row_types:effect_row([state, reader]),
    ?assertMatch({error, {effect_mismatch, _, _}},
        catena_handler_check:check_handler_effects(HandlerType, Different)).

%%%---------------------------------------------------------------------
%%% Handler Signature Checking Tests
%%%---------------------------------------------------------------------

test_handler_signature_checking() ->
    IntType = int,
    Ops = #{get => catena_handler_types:operation_sig([], IntType)},
    Effects = catena_row_types:effect_row([state]),

    HandlerType = catena_handler_types:with_effects(
        catena_handler_types:with_output(
            catena_handler_types:with_input(
                catena_handler_types:with_operations(
                    catena_handler_types:handler_type(test), Ops),
                IntType),
            IntType),
        Effects),

    ?assertEqual(ok, catena_handler_check:check_handler_signature(HandlerType)),

    % Handler without operations should fail
    EmptyHandler = catena_handler_types:handler_type(),
    ?assertNotEqual(ok, catena_handler_check:check_handler_signature(EmptyHandler)).

%%%---------------------------------------------------------------------
%%% Error Formatting Tests
%%%---------------------------------------------------------------------

test_error_formatting() ->
    % Missing operations error
    MissingError = {missing_operations, [get, put]},
    MissingFormatted = catena_handler_check:format_check_error(MissingError),
    ?assertNotEqual(nomatch, binary:match(MissingFormatted, <<"Missing operations">>)),

    % Redundant operations error
    RedundantError = {redundant_operations, [send]},
    RedundantFormatted = catena_handler_check:format_check_error(RedundantError),
    ?assertNotEqual(nomatch, binary:match(RedundantFormatted, <<"Redundant operations">>)),

    % Coverage error
    CoverageError = {coverage_issues, [get], [send]},
    CoverageFormatted = catena_handler_check:format_check_error(CoverageError),
    ?assertNotEqual(nomatch, binary:match(CoverageFormatted, <<"Coverage issues">>)),

    % Return type mismatch
    TypeError = {return_type_mismatch, int, atom},
    TypeFormatted = catena_handler_check:format_return_type_error(TypeError),
    ?assertNotEqual(nomatch, binary:match(TypeFormatted, <<"return type mismatch">>)),

    % Coverage error formatting
    CoverageError2 = {missing_operations, [put]},
    CoverageFormatted2 = catena_handler_check:format_coverage_error(CoverageError2),
    ?assertNotEqual(nomatch, binary:match(CoverageFormatted2, <<"Handler is missing">>)).

%%%---------------------------------------------------------------------
%%% Mock Handler Module (simulated)
%%%---------------------------------------------------------------------

% These would normally be real modules, but for testing we simulate
% the behavior through the test functions above.
