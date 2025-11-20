%%%-------------------------------------------------------------------
%%% @doc Tests for Effect-Specific Error Messages (Task 1.2.5)
%%%
%%% Tests the formatting and construction of effect-specific error
%%% messages including unhandled effects, handler mismatches,
%%% annotation mismatches, and effect propagation chains.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_error_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Helper to create a test location
test_location() ->
    catena_location:new(10, 5).

test_location_span() ->
    catena_location:new(10, 5, 12, 20).

%%====================================================================
%% 1.2.5.1: Unhandled Effect Error Tests
%%====================================================================

unhandled_effect_test_() ->
    [
     ?_test(test_unhandled_effect_constructor()),
     ?_test(test_unhandled_effect_formatting()),
     ?_test(test_unhandled_effect_with_location())
    ].

test_unhandled_effect_constructor() ->
    Error = catena_type_error:unhandled_effect(io, process_file, test_location()),
    ?assertMatch({unhandled_effect, io, process_file, _}, Error).

test_unhandled_effect_formatting() ->
    Error = catena_type_error:unhandled_effect(io, process_file, test_location()),
    Msg = catena_type_error:format_error(Error),

    %% Should mention the effect name
    ?assert(string:find(Msg, "io") =/= nomatch),
    %% Should mention the function name
    ?assert(string:find(Msg, "process_file") =/= nomatch),
    %% Should include location
    ?assert(string:find(Msg, "10:5") =/= nomatch),
    %% Should suggest fix
    ?assert(string:find(Msg, "/ {io}") =/= nomatch orelse
            string:find(Msg, "try/with") =/= nomatch).

test_unhandled_effect_with_location() ->
    Location = test_location_span(),
    Error = catena_type_error:unhandled_effect(file_io, read_config, Location),
    Msg = catena_type_error:format_error(Error),

    ?assert(string:find(Msg, "file_io") =/= nomatch),
    ?assert(string:find(Msg, "read_config") =/= nomatch).

%%====================================================================
%% 1.2.5.2: Handler Mismatch Error Tests
%%====================================================================

handler_mismatch_test_() ->
    [
     ?_test(test_handler_missing_operation_constructor()),
     ?_test(test_handler_missing_operation_formatting()),
     ?_test(test_handler_arity_mismatch_constructor()),
     ?_test(test_handler_arity_mismatch_formatting())
    ].

test_handler_missing_operation_constructor() ->
    Error = catena_type_error:handler_missing_operation('FileIO', read, test_location()),
    ?assertMatch({handler_missing_operation, 'FileIO', read, _}, Error).

test_handler_missing_operation_formatting() ->
    Error = catena_type_error:handler_missing_operation('FileIO', read, test_location()),
    Msg = catena_type_error:format_error(Error),

    %% Should mention effect and operation
    ?assert(string:find(Msg, "FileIO") =/= nomatch),
    ?assert(string:find(Msg, "read") =/= nomatch),
    %% Should include location
    ?assert(string:find(Msg, "10:5") =/= nomatch),
    %% Should suggest adding handler case
    ?assert(string:find(Msg, "read(args)") =/= nomatch).

test_handler_arity_mismatch_constructor() ->
    Error = catena_type_error:handler_arity_mismatch('Console', print, 1, 2, test_location()),
    ?assertMatch({handler_arity_mismatch, 'Console', print, 1, 2, _}, Error).

test_handler_arity_mismatch_formatting() ->
    Error = catena_type_error:handler_arity_mismatch('Console', print, 1, 2, test_location()),
    Msg = catena_type_error:format_error(Error),

    %% Should mention effect and operation
    ?assert(string:find(Msg, "Console") =/= nomatch),
    ?assert(string:find(Msg, "print") =/= nomatch),
    %% Should show expected and actual arities
    ?assert(string:find(Msg, "1") =/= nomatch),
    ?assert(string:find(Msg, "2") =/= nomatch),
    %% Should include location
    ?assert(string:find(Msg, "10:5") =/= nomatch).

%%====================================================================
%% 1.2.5.3: Effect Annotation Mismatch Error Tests
%%====================================================================

effect_annotation_test_() ->
    [
     ?_test(test_effect_annotation_mismatch_constructor()),
     ?_test(test_effect_annotation_mismatch_formatting()),
     ?_test(test_effect_annotation_mismatch_with_extra()),
     ?_test(test_effect_annotation_mismatch_with_missing())
    ].

test_effect_annotation_mismatch_constructor() ->
    Declared = catena_types:singleton_effect(io),
    Inferred = catena_types:normalize_effects([io, file]),
    Error = catena_type_error:effect_annotation_mismatch(my_func, Declared, Inferred),
    ?assertMatch({effect_annotation_mismatch, my_func, _, _}, Error).

test_effect_annotation_mismatch_formatting() ->
    Declared = catena_types:singleton_effect(io),
    Inferred = catena_types:normalize_effects([io, file]),
    Error = catena_type_error:effect_annotation_mismatch(my_func, Declared, Inferred),
    Msg = catena_type_error:format_error(Error),

    %% Should mention function name
    ?assert(string:find(Msg, "my_func") =/= nomatch),
    %% Should show declared effects
    ?assert(string:find(Msg, "io") =/= nomatch),
    %% Should show inferred effects
    ?assert(string:find(Msg, "file") =/= nomatch).

test_effect_annotation_mismatch_with_extra() ->
    %% Declared more effects than used
    Declared = catena_types:normalize_effects([io, file, state]),
    Inferred = catena_types:singleton_effect(io),
    Error = catena_type_error:effect_annotation_mismatch(over_declared, Declared, Inferred),
    Msg = catena_type_error:format_error(Error),

    ?assert(string:find(Msg, "over_declared") =/= nomatch),
    %% Should mention unused effects
    ?assert(string:find(Msg, "file") =/= nomatch orelse
            string:find(Msg, "state") =/= nomatch).

test_effect_annotation_mismatch_with_missing() ->
    %% Declared fewer effects than used
    Declared = catena_types:singleton_effect(io),
    Inferred = catena_types:normalize_effects([io, async]),
    Error = catena_type_error:effect_annotation_mismatch(under_declared, Declared, Inferred),
    Msg = catena_type_error:format_error(Error),

    ?assert(string:find(Msg, "under_declared") =/= nomatch),
    %% Should mention missing effects
    ?assert(string:find(Msg, "async") =/= nomatch).

%%====================================================================
%% 1.2.5.4: Effect Context Chain Error Tests
%%====================================================================

effect_context_chain_test_() ->
    [
     ?_test(test_effect_context_chain_constructor()),
     ?_test(test_effect_context_chain_formatting()),
     ?_test(test_effect_context_chain_empty()),
     ?_test(test_effect_context_chain_multiple())
    ].

test_effect_context_chain_constructor() ->
    Chain = [{perform_io, test_location()}],
    Error = catena_type_error:effect_context_chain(io, Chain),
    ?assertMatch({effect_context_chain, io, _}, Error).

test_effect_context_chain_formatting() ->
    Chain = [{perform_io, catena_location:new(5, 1)}],
    Error = catena_type_error:effect_context_chain(io, Chain),
    Msg = catena_type_error:format_error(Error),

    %% Should mention effect name
    ?assert(string:find(Msg, "io") =/= nomatch),
    %% Should show function in chain
    ?assert(string:find(Msg, "perform_io") =/= nomatch),
    %% Should show location
    ?assert(string:find(Msg, "5:1") =/= nomatch).

test_effect_context_chain_empty() ->
    Error = catena_type_error:effect_context_chain(state, []),
    Msg = catena_type_error:format_error(Error),

    %% Should mention effect name even with empty chain
    ?assert(string:find(Msg, "state") =/= nomatch).

test_effect_context_chain_multiple() ->
    Chain = [
        {read_file, catena_location:new(10, 1)},
        {process_data, catena_location:new(20, 1)},
        {main, catena_location:new(30, 1)}
    ],
    Error = catena_type_error:effect_context_chain(file_io, Chain),
    Msg = catena_type_error:format_error(Error),

    %% Should show all functions in chain
    ?assert(string:find(Msg, "read_file") =/= nomatch),
    ?assert(string:find(Msg, "process_data") =/= nomatch),
    ?assert(string:find(Msg, "main") =/= nomatch),
    %% Should show all locations
    ?assert(string:find(Msg, "10:1") =/= nomatch),
    ?assert(string:find(Msg, "20:1") =/= nomatch),
    ?assert(string:find(Msg, "30:1") =/= nomatch).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
     ?_test(test_error_with_format_error_with_location()),
     ?_test(test_all_effect_errors_are_strings())
    ].

test_error_with_format_error_with_location() ->
    Location = test_location(),
    Error = catena_type_error:unhandled_effect(io, test_func, Location),
    Msg = catena_type_error:format_error_with_location(Location, Error),

    %% Should have location prefix
    ?assert(string:find(Msg, "10:5:") =/= nomatch).

test_all_effect_errors_are_strings() ->
    Loc = test_location(),
    Errors = [
        catena_type_error:unhandled_effect(io, func1, Loc),
        catena_type_error:handler_missing_operation('E', op, Loc),
        catena_type_error:handler_arity_mismatch('E', op, 1, 2, Loc),
        catena_type_error:effect_annotation_mismatch(func2,
            catena_types:singleton_effect(io),
            catena_types:singleton_effect(file)),
        catena_type_error:effect_context_chain(io, [{func3, Loc}])
    ],

    lists:foreach(fun(Error) ->
        Msg = catena_type_error:format_error(Error),
        ?assert(is_list(Msg)),
        ?assert(length(Msg) > 0)
    end, Errors).
