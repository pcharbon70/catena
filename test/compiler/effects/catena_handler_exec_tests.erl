-module(catena_handler_exec_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_handler_exec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"typed handler execution", fun test_execute_typed/0},
        {"execution with checks", fun test_execute_with_checks/0},
        {"pre-execution validation", fun test_validate_before_execution/0},
        {"resumption type enforcement", fun test_enforce_resumption_type/0},
        {"resumption parameter check", fun test_check_resumption_param/0},
        {"resumption return check", fun test_check_resumption_return/0},
        {"safe resumption call", fun test_safe_resumption_call/0},
        {"handler type coercion", fun test_coerce_handler/0},
        {"subtype checking", fun test_check_subtype/0},
        {"compatible checking", fun test_check_compatible/0},
        {"safe type casting", fun test_safe_cast/0},
        {"type error handling", fun test_handle_type_error/0},
        {"error formatting", fun test_format_type_error/0},
        {"error recovery", fun test_recover_from_error/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Typed Handler Invocation Tests
%%%---------------------------------------------------------------------

test_execute_typed() ->
    IntType = int,
    AtomType = atom,
    Ops = #{get => catena_handler_types:operation_sig([AtomType], IntType)},
    HandlerType = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(), Ops),
            AtomType),
        IntType),

    % Create a mock handler function
    HandlerFn = fun(_Val, _Resume) -> 42 end,

    % Test execution with valid operation
    Result = catena_handler_exec:execute_typed(get, HandlerFn, HandlerType, input),
    ?assertMatch({ok, _}, Result),

    BadHandlerFn = fun(_Val, _Resume) -> wrong end,
    ResultBad = catena_handler_exec:execute_typed(get, BadHandlerFn, HandlerType, input),
    ?assertMatch({error, {return_type_mismatch, _, _}}, ResultBad),

    % Test with unknown operation
    Result2 = catena_handler_exec:execute_typed(unknown, HandlerFn, HandlerType, input),
    ?assertMatch({error, {unknown_operation, _}}, Result2).

%%%---------------------------------------------------------------------
%%% Execution with Checks Tests
%%%---------------------------------------------------------------------

test_execute_with_checks() ->
    % This test simulates execution with checks
    % Since we can't easily create a real module, we test the structure
    Result = catena_handler_exec:execute_with_checks(dummy_module, dummy_fn, value),
    ?assertMatch({error, {execution_error, _, _, _}}, Result).

%%%---------------------------------------------------------------------
%%% Pre-execution Validation Tests
%%%---------------------------------------------------------------------

test_validate_before_execution() ->
    IntType = int,
    HandlerType = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:handler_type(), IntType),
        IntType),

    % Valid input type
    ?assertEqual(ok, catena_handler_exec:validate_before_execution(HandlerType, 42)),

    % Invalid input type
    ?assertMatch({error, {input_type_mismatch, _, _}},
        catena_handler_exec:validate_before_execution(HandlerType, atom)).

%%%---------------------------------------------------------------------
%%% Resumption Type Enforcement Tests
%%%---------------------------------------------------------------------

test_enforce_resumption_type() ->
    IntType = int,
    OpSig = catena_handler_types:operation_sig([IntType], IntType),

    % Create a mock resumption
    Resumption = catena_resumption:new(fun(V) -> V end, 0, 0, #{}),

    % Valid resumption call
    Result = catena_handler_exec:enforce_resumption_type(OpSig, 42, Resumption),
    ?assertMatch({ok, _}, Result).

test_check_resumption_param() ->
    IntType = int,

    % Valid parameter type
    ?assertEqual(ok, catena_handler_exec:check_resumption_param([IntType], 42)),

    % Invalid parameter type
    ?assertMatch({error, {param_type_mismatch, _, _}},
        catena_handler_exec:check_resumption_param([IntType], atom)),

    % Invalid parameter count
    ?assertMatch({error, invalid_param_count},
        catena_handler_exec:check_resumption_param([IntType, int], 42)).

test_check_resumption_return() ->
    IntType = int,

    % Valid return type
    ?assertEqual({ok, 42}, catena_handler_exec:check_resumption_return(IntType, 42)),

    % Invalid return type
    ?assertMatch({error, {return_type_mismatch, _, _}},
        catena_handler_exec:check_resumption_return(IntType, atom)).

test_safe_resumption_call() ->
    IntType = int,
    OpSig = catena_handler_types:operation_sig([IntType], IntType),
    Resumption = catena_resumption:new(fun(V) -> V end, 0, 0, #{}),

    % Safe call with correct types
    Result = catena_handler_exec:safe_resumption_call(OpSig, 42, Resumption),
    ?assertMatch({ok, _}, Result).

%%%---------------------------------------------------------------------
%%% Handler Type Coercion Tests
%%%---------------------------------------------------------------------

test_coerce_handler() ->
    IntType = int,
    Ops = #{get => catena_handler_types:operation_sig([], IntType)},
    HandlerType = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(), Ops),
            IntType),
        IntType),

    % Self-coercion should succeed
    Result = catena_handler_exec:coerce_handler(HandlerType, HandlerType),
    ?assertMatch({ok, _}, Result).

test_check_subtype() ->
    IntType = int,
    AtomType = atom,

    % Same handler is subtype of itself
    Ops1 = #{get => catena_handler_types:operation_sig([], IntType)},
    Handler1 = catena_handler_types:with_effects(
        catena_handler_types:with_output(
            catena_handler_types:with_input(
                catena_handler_types:with_operations(
                    catena_handler_types:handler_type(), Ops1),
                IntType),
            IntType),
        catena_row_types:empty_row()),

    ?assert(catena_handler_exec:check_subtype(Handler1, Handler1)),

    % Different input/output types - not subtype
    Ops2 = #{get => catena_handler_types:operation_sig([], AtomType)},
    Handler2 = catena_handler_types:with_effects(
        catena_handler_types:with_output(
            catena_handler_types:with_input(
                catena_handler_types:with_operations(
                    catena_handler_types:handler_type(), Ops2),
                AtomType),
            AtomType),
        catena_row_types:empty_row()),

    ?assertNot(catena_handler_exec:check_subtype(Handler1, Handler2)).

test_check_compatible() ->
    IntType = int,
    Ops1 = #{get => catena_handler_types:operation_sig([], IntType)},
    Handler1 = catena_handler_types:with_output(
        catena_handler_types:with_input(
            catena_handler_types:with_operations(
                catena_handler_types:handler_type(), Ops1),
            IntType),
        IntType),

    % Same handler is compatible
    Result = catena_handler_exec:check_compatible(Handler1, Handler1),
    ?assertMatch({ok, _}, Result).

test_safe_cast() ->
    % Valid cast from int to int
    ?assertMatch({ok, 42}, catena_handler_exec:safe_cast(42, int, int)),

    % Invalid cast (value doesn't match source type)
    ?assertMatch({error, {source_type_mismatch, _, _}},
        catena_handler_exec:safe_cast(atom, int, int)).

%%%---------------------------------------------------------------------
%%% Type Error Handling Tests
%%%---------------------------------------------------------------------

test_handle_type_error() ->
    Error = {return_type_mismatch, int, atom},
    Result = catena_handler_exec:handle_type_error(Error, context),
    ?assertMatch({error, {type_error, _, _}}, Result).

test_format_type_error() ->
    Error = {return_type_mismatch, int, atom},
    Formatted = catena_handler_exec:format_type_error(Error),
    ?assertNotEqual(nomatch, binary:match(Formatted, <<"Return type mismatch">>)),

    Error2 = {operation_type_mismatch, get, int, atom},
    Formatted2 = catena_handler_exec:format_type_error(Error2),
    ?assertNotEqual(nomatch, binary:match(Formatted2, <<"Operation">>)),
    ?assertNotEqual(nomatch, binary:match(Formatted2, <<"type mismatch">>)).

test_recover_from_error() ->
    Error = {return_type_mismatch, int, atom},
    Result = catena_handler_exec:recover_from_error(Error, context),
    ?assertEqual({ok, atom}, Result),

    % Non-recoverable error
    Error2 = {operation_type_mismatch, get, int, atom},
    Result2 = catena_handler_exec:recover_from_error(Error2, context),
    ?assertMatch({error, _}, Result2).
