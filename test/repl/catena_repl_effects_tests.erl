%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_repl_effects
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_effects_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Setup
%%%=============================================================================

setup() ->
    %% Create ETS table for state effect tests
    ets:new(repl_state, [named_table, public, set]),
    ok.

cleanup(_) ->
    %% Clean up ETS table
    catch ets:delete(repl_state),
    ok.

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

%% Simple options map
default_options() ->
    #{verbose => false, timeout => 5000}.

verbose_options() ->
    #{verbose => true, timeout => 5000}.

%%%=============================================================================
%%% Default Handler Management Tests
%%%=============================================================================

with_default_handlers_test() ->
    Opts = default_options(),
    Ctx = catena_repl_effects:with_default_handlers(Opts),
    ?assertMatch(#{handlers := #{}, parent := _}, Ctx),
    Handlers = maps:get(handlers, Ctx),
    ?assert(maps:is_key(io, Handlers)),
    ?assert(maps:is_key(process, Handlers)),
    ?assert(maps:is_key(error, Handlers)),
    ?assert(maps:is_key(state, Handlers)).

%%%=============================================================================
%%% Effect Evaluation Tests
%%%=============================================================================

eval_literal_test() ->
    AST = {literal, 42, int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 42}, Result).

eval_binop_addition_test() ->
    AST = {binop, '+',
            {literal, 20, int, {1, 1}},
            {literal, 22, int, {1, 1}},
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 42}, Result).

eval_binop_equality_test() ->
    AST = {binop, '==',
            {literal, 42, int, {1, 1}},
            {literal, 42, int, {1, 1}},
            {tcon, bool}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, true}, Result).

eval_unop_negation_test() ->
    AST = {unop, '-',
            {literal, 42, int, {1, 1}},
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, -42}, Result).

eval_unop_not_test() ->
    AST = {unop, 'not',
            {literal, true, bool, {1, 1}},
            bool, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, false}, Result).

eval_if_true_test() ->
    AST = {if_expr,
            [{if_clause, {literal, true, bool, {1, 1}}, {literal, 42, int, {1, 1}}, {1, 1}}],
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 42}, Result).

eval_if_false_test() ->
    AST = {if_expr,
            [{if_clause, {literal, false, bool, {1, 1}}, {literal, 1, int, {1, 1}}, {1, 1}},
             {if_clause, {literal, true, bool, {1, 1}}, {literal, 2, int, {1, 1}}, {1, 1}}],
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 2}, Result).

eval_case_match_test() ->
    AST = {case_expr,
            {literal, 42, int, {1, 1}},
            [{match_clause, {literal, 42, int, {1, 1}}, undefined, {literal, matched, atom, {1, 1}}, {1, 1}}],
            atom, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, matched}, Result).

eval_case_no_match_test() ->
    AST = {case_expr,
            {literal, 99, int, {1, 1}},
            [{match_clause, {literal, 42, int, {1, 1}}, undefined, {literal, matched, atom, {1, 1}}, {1, 1}}],
            atom, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({error, no_matching_clause}, Result).

%%%=============================================================================
%%% Builtin Function Tests
%%%=============================================================================

eval_builtin_identity_test() ->
    AST = {apply_expr, {atom, identity, 1},
            [{literal, 42, int, {1, 1}}],
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 42}, Result).

eval_builtin_const_test() ->
    AST = {apply_expr, {atom, const, 2},
            [{literal, 42, int, {1, 1}},
             {literal, 99, int, {1, 1}}],
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 42}, Result).

eval_builtin_head_test() ->
    AST = {apply_expr, {atom, head, 1},
            [{literal, [1, 2, 3], {tcon, list, [{tvar, a}]}, {1, 1}}],
            {tvar, a}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 1}, Result).

eval_builtin_tail_test() ->
    AST = {apply_expr, {atom, tail, 1},
            [{literal, [1, 2, 3], {tcon, list, [{tvar, a}]}, {1, 1}}],
            {tcon, list, [{tvar, a}]}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, [2, 3]}, Result).

eval_builtin_length_test() ->
    AST = {apply_expr, {atom, length, 1},
            [{literal, [1, 2, 3, 4, 5], {tcon, list, [{tvar, a}]}, {1, 1}}],
            {tcon, natural}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, 5}, Result).

eval_builtin_reverse_test() ->
    AST = {apply_expr, {atom, reverse, 1},
            [{literal, [1, 2, 3], {tcon, list, [{tvar, a}]}, {1, 1}}],
            {tcon, list, [{tvar, a}]}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, [3, 2, 1]}, Result).

%%%=============================================================================
%%% Effect Operation Tests
%%%=============================================================================

eval_io_print_test() ->
    AST = {perform_expr, io, print,
            [{literal, "Hello, World!", text, {1, 1}}],
            {tcon, unit}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({ok, unit}, Result).

eval_state_put_get_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         fun() ->
             %% First put a value - state persists in ETS table
             PutAST = {perform_expr, state, put,
                        [{literal, 42, int, {1, 1}}],
                        {tcon, unit}, {1, 1}},
             Opts = default_options(),
             Ctx0 = catena_repl_effects:with_default_handlers(Opts),
             {ok, unit} = catena_repl_effects:eval_with_effects(PutAST, Ctx0, Opts),
             %% Then get it back
             GetAST = {perform_expr, state, get,
                        [],
                        int, {1, 1}},
             {ok, Result} = catena_repl_effects:eval_with_effects(GetAST, Ctx0, Opts),
             ?assertEqual(42, Result)
         end
     end}.

eval_error_raise_test() ->
    AST = {perform_expr, error, raise,
            [{literal, "test error", text, {1, 1}}],
            {tcon, {tcon, result, [{tvar, a}]}, [text]}, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({error, {effect_operation_failed, error, raise, "test error"}}, Result).

%%%=============================================================================
%%% Error Handling Tests
%%%=============================================================================

eval_unbound_variable_test() ->
    AST = {var, undefined_variable, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({error, {unbound_variable, _}}, Result).

eval_division_by_zero_test() ->
    AST = {binop, '/',
            {literal, 42, int, {1, 1}},
            {literal, 0, int, {1, 1}},
            int, {1, 1}},
    Result = catena_repl_effects:eval_with_effects(AST, default_options()),
    ?assertMatch({error, {unsupported_operation, '/'}}, Result).

%%%=============================================================================
%%% Verbose Mode Tests
%%%=============================================================================

is_verbose_default_test() ->
    ?assertEqual(false, catena_repl_effects:is_verbose()),
    catena_repl_effects:set_verbose(true),
    ?assertEqual(true, catena_repl_effects:is_verbose()),
    catena_repl_effects:set_verbose(false).

%%%=============================================================================
%%% Error Formatting Tests
%%%=============================================================================

format_effect_operation_failed_test() ->
    Error = {effect_operation_failed, io, print, "test reason"},
    Result = catena_repl_effects:format_effect_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "Effect operation failed") > 0).

format_effect_timeout_test() ->
    Error = {effect_timeout, io, read_line},
    Result = catena_repl_effects:format_effect_error(Error),
    ?assert(is_list(Result)),
    ResultString = lists:flatten(Result),
    ?assert(string:str(ResultString, "timed out") > 0).

%%%=============================================================================
%%% Pattern Matching Tests
%%%=============================================================================

match_pattern_literal_true_test() ->
    Pattern = {literal, 42, int, {1, 1}},
    Result = catena_repl_effects:match_pattern(42, Pattern),
    ?assertMatch({ok, true}, Result).

match_pattern_literal_false_test() ->
    Pattern = {literal, 42, int, {1, 1}},
    Result = catena_repl_effects:match_pattern(99, Pattern),
    ?assertMatch({ok, false}, Result).

match_pattern_var_test() ->
    Pattern = {var, x, {1, 1}},
    Result = catena_repl_effects:match_pattern(any_value, Pattern),
    ?assertMatch({ok, true}, Result).

match_pattern_nil_true_test() ->
    Pattern = {nil_pattern, {1, 1}},
    Result = catena_repl_effects:match_pattern([], Pattern),
    ?assertMatch({ok, true}, Result).

match_pattern_nil_false_test() ->
    Pattern = {nil_pattern, {1, 1}},
    Result = catena_repl_effects:match_pattern([1, 2, 3], Pattern),
    ?assertMatch({ok, false}, Result).

match_pattern_cons_true_test() ->
    Pattern = {cons_pattern, {var, h, {1, 1}}, {var, t, {1, 1}}, {1, 1}},
    Result = catena_repl_effects:match_pattern([1, 2, 3], Pattern),
    ?assertMatch({ok, true}, Result).

match_pattern_cons_false_test() ->
    Pattern = {cons_pattern, {var, h, {1, 1}}, {var, t, {1, 1}}, {1, 1}},
    Result = catena_repl_effects:match_pattern([], Pattern),
    ?assertMatch({ok, false}, Result).
