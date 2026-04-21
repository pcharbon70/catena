%%%-------------------------------------------------------------------
%%% @doc Tests for dedicated effect/runtime code generation helpers.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_codegen_tests).

-include_lib("eunit/include/eunit.hrl").

new_state() ->
    catena_codegen_utils:new_state().

loc() ->
    {location, 1, 1}.

translate_perform_test_() ->
    [
        ?_test(test_translate_perform_emits_runtime_call())
    ].

test_translate_perform_emits_runtime_call() ->
    Expr = {perform_expr, 'IO', print, [
        {literal, string, <<"hello">>, loc()}
    ], loc()},
    {Core, _State1} = catena_effect_codegen:translate_perform(Expr, new_state()),
    ?assertEqual(call, cerl:type(Core)),
    ?assertEqual(catena_effect_runtime, cerl:atom_val(cerl:call_module(Core))),
    ?assertEqual(perform, cerl:atom_val(cerl:call_name(Core))),
    Args = cerl:call_args(Core),
    ?assertEqual(4, length(Args)),
    ?assertEqual('__catena_ctx__', cerl:var_name(hd(Args))).

translate_handle_test_() ->
    [
        ?_test(test_translate_handle_expr_emits_with_handlers_call())
    ].

test_translate_handle_expr_emits_with_handlers_call() ->
    Expr = {handle_expr,
        {perform_expr, 'IO', read, [], loc()},
        [{handler_clause, 'IO', [
            {operation_case, read, [], {literal, string, <<"ok">>, loc()}, loc()}
        ], loc()}],
        loc()},
    {Core, _State1} = catena_effect_codegen:translate_handle(Expr, new_state()),
    ?assertEqual(call, cerl:type(Core)),
    ?assertEqual(catena_effect_runtime, cerl:atom_val(cerl:call_module(Core))),
    ?assertEqual(with_handlers, cerl:atom_val(cerl:call_name(Core))),
    [CtxVar, _Handlers, BodyFun] = cerl:call_args(Core),
    ?assertEqual('__catena_ctx__', cerl:var_name(CtxVar)),
    ?assertEqual('fun', cerl:type(BodyFun)),
    ?assertEqual(1, cerl:fun_arity(BodyFun)).

runtime_bootstrap_test_() ->
    [
        ?_test(test_with_runtime_call_targets_effect_system()),
        ?_test(test_runtime_init_and_shutdown_calls_target_effect_system())
    ].

test_with_runtime_call_targets_effect_system() ->
    Wrapped = catena_effect_codegen:with_runtime_call(cerl:c_var(result)),
    ?assertEqual(call, cerl:type(Wrapped)),
    ?assertEqual(catena_effect_system, cerl:atom_val(cerl:call_module(Wrapped))),
    ?assertEqual(with_runtime, cerl:atom_val(cerl:call_name(Wrapped))),
    [BodyFun] = cerl:call_args(Wrapped),
    ?assertEqual('fun', cerl:type(BodyFun)),
    ?assertEqual(1, cerl:fun_arity(BodyFun)).

test_runtime_init_and_shutdown_calls_target_effect_system() ->
    Init = catena_effect_codegen:runtime_init_call(),
    Stop = catena_effect_codegen:runtime_shutdown_call(),
    ?assertEqual(call, cerl:type(Init)),
    ?assertEqual(catena_effect_system, cerl:atom_val(cerl:call_module(Init))),
    ?assertEqual(start_runtime, cerl:atom_val(cerl:call_name(Init))),
    ?assertEqual(call, cerl:type(Stop)),
    ?assertEqual(catena_effect_system, cerl:atom_val(cerl:call_module(Stop))),
    ?assertEqual(stop_runtime, cerl:atom_val(cerl:call_name(Stop))).
