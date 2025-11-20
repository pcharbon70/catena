%%%-------------------------------------------------------------------
%%% @doc Tests for Expression Translation (Task 1.3.1)
%%%
%%% Tests translation of Catena AST expressions to Core Erlang.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_expr_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

new_state() ->
    catena_codegen_utils:new_state().

loc() ->
    {location, 1, 1}.

%%====================================================================
%% Literal Translation Tests (1.3.1.4)
%%====================================================================

literal_translation_test_() ->
    [
        ?_test(test_translate_integer()),
        ?_test(test_translate_float()),
        ?_test(test_translate_string()),
        ?_test(test_translate_atom()),
        ?_test(test_translate_bool())
    ].

test_translate_integer() ->
    State = new_state(),
    Expr = {literal, integer, 42, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(42, cerl:int_val(Core)).

test_translate_float() ->
    State = new_state(),
    Expr = {literal, float, 3.14, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(3.14, cerl:float_val(Core)).

test_translate_string() ->
    State = new_state(),
    Expr = {literal, string, <<"hello">>, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual("hello", cerl:string_val(Core)).

test_translate_atom() ->
    State = new_state(),
    Expr = {literal, atom, ok, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(ok, cerl:atom_val(Core)).

test_translate_bool() ->
    State = new_state(),
    TrueExpr = {literal, bool, true, loc()},
    FalseExpr = {literal, bool, false, loc()},
    {CoreTrue, _} = catena_codegen_expr:translate_expr(TrueExpr, State),
    {CoreFalse, _} = catena_codegen_expr:translate_expr(FalseExpr, State),
    ?assertEqual(true, cerl:atom_val(CoreTrue)),
    ?assertEqual(false, cerl:atom_val(CoreFalse)).

%%====================================================================
%% Variable Translation Tests
%%====================================================================

variable_translation_test_() ->
    [
        ?_test(test_translate_variable())
    ].

test_translate_variable() ->
    State = new_state(),
    Expr = {var, x, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(x, cerl:var_name(Core)).

%%====================================================================
%% Function Application Tests (1.3.1.1)
%%====================================================================

function_application_test_() ->
    [
        ?_test(test_translate_simple_app()),
        ?_test(test_translate_multi_arg_app()),
        ?_test(test_translate_nested_app())
    ].

test_translate_simple_app() ->
    State = new_state(),
    %% f(x)
    Expr = {app, {var, f, loc()}, [{var, x, loc()}], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    %% Should be an apply node
    ?assertEqual(apply, cerl:type(Core)).

test_translate_multi_arg_app() ->
    State = new_state(),
    %% f(x, y, z)
    Expr = {app, {var, f, loc()}, [
        {var, x, loc()},
        {var, y, loc()},
        {var, z, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(apply, cerl:type(Core)),
    ?assertEqual(3, length(cerl:apply_args(Core))).

test_translate_nested_app() ->
    State = new_state(),
    %% f(g(x))
    Inner = {app, {var, g, loc()}, [{var, x, loc()}], loc()},
    Expr = {app, {var, f, loc()}, [Inner], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(apply, cerl:type(Core)),
    %% First argument should also be an apply
    [Arg] = cerl:apply_args(Core),
    ?assertEqual(apply, cerl:type(Arg)).

%%====================================================================
%% Let Binding Tests (1.3.1.2)
%%====================================================================

let_binding_test_() ->
    [
        ?_test(test_translate_simple_let()),
        ?_test(test_translate_multi_let())
    ].

test_translate_simple_let() ->
    State = new_state(),
    %% let x = 42 in x
    Expr = {let_expr, [
        {{var, x, loc()}, {literal, integer, 42, loc()}}
    ], {var, x, loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual('let', cerl:type(Core)).

test_translate_multi_let() ->
    State = new_state(),
    %% let x = 1, y = 2 in x + y
    Expr = {let_expr, [
        {{var, x, loc()}, {literal, integer, 1, loc()}},
        {{var, y, loc()}, {literal, integer, 2, loc()}}
    ], {binary_op, '+', {var, x, loc()}, {var, y, loc()}, loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual('let', cerl:type(Core)).

%%====================================================================
%% Composition Operator Tests (1.3.1.3)
%%====================================================================

composition_operator_test_() ->
    [
        ?_test(test_translate_pipe()),
        ?_test(test_translate_chained_pipe())
    ].

test_translate_pipe() ->
    State = new_state(),
    %% x |> f becomes f(x)
    Expr = {binary_op, '|>', {var, x, loc()}, {var, f, loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(apply, cerl:type(Core)),
    %% Check it's f applied to x
    [Arg] = cerl:apply_args(Core),
    ?assertEqual(x, cerl:var_name(Arg)).

test_translate_chained_pipe() ->
    State = new_state(),
    %% x |> f |> g becomes g(f(x))
    Inner = {binary_op, '|>', {var, x, loc()}, {var, f, loc()}, loc()},
    Expr = {binary_op, '|>', Inner, {var, g, loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(apply, cerl:type(Core)).

%%====================================================================
%% Binary Operator Tests
%%====================================================================

binary_operator_test_() ->
    [
        ?_test(test_translate_arithmetic()),
        ?_test(test_translate_comparison()),
        ?_test(test_translate_equality()),
        ?_test(test_translate_list_cons())
    ].

test_translate_arithmetic() ->
    State = new_state(),
    %% 1 + 2
    Expr = {binary_op, '+',
        {literal, integer, 1, loc()},
        {literal, integer, 2, loc()},
        loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(call, cerl:type(Core)),
    %% Should call erlang:'+'
    Module = cerl:call_module(Core),
    Name = cerl:call_name(Core),
    ?assertEqual(erlang, cerl:atom_val(Module)),
    ?assertEqual('+', cerl:atom_val(Name)).

test_translate_comparison() ->
    State = new_state(),
    %% x > y
    Expr = {binary_op, '>', {var, x, loc()}, {var, y, loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(call, cerl:type(Core)),
    Name = cerl:call_name(Core),
    ?assertEqual('>', cerl:atom_val(Name)).

test_translate_equality() ->
    State = new_state(),
    %% x === y becomes erlang:'=:='(x, y)
    Expr = {binary_op, '===', {var, x, loc()}, {var, y, loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual(call, cerl:type(Core)),
    Name = cerl:call_name(Core),
    ?assertEqual('=:=', cerl:atom_val(Name)).

test_translate_list_cons() ->
    State = new_state(),
    %% 1 :: [2, 3]
    Expr = {binary_op, '::',
        {literal, integer, 1, loc()},
        {list_expr, [{literal, integer, 2, loc()}, {literal, integer, 3, loc()}], loc()},
        loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assert(cerl:is_c_cons(Core)).

%%====================================================================
%% Lambda Translation Tests
%%====================================================================

lambda_translation_test_() ->
    [
        ?_test(test_translate_lambda())
    ].

test_translate_lambda() ->
    State = new_state(),
    %% \x -> x + 1
    Expr = {lambda, [{var, x, loc()}],
        {binary_op, '+', {var, x, loc()}, {literal, integer, 1, loc()}, loc()},
        loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual('fun', cerl:type(Core)),
    ?assertEqual(1, cerl:fun_arity(Core)).

%%====================================================================
%% If Expression Tests
%%====================================================================

if_expression_test_() ->
    [
        ?_test(test_translate_if())
    ].

test_translate_if() ->
    State = new_state(),
    %% if true then 1 else 0
    Expr = {if_expr,
        {literal, bool, true, loc()},
        {literal, integer, 1, loc()},
        {literal, integer, 0, loc()},
        loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assertEqual('case', cerl:type(Core)).

%%====================================================================
%% List and Tuple Tests
%%====================================================================

list_tuple_test_() ->
    [
        ?_test(test_translate_list()),
        ?_test(test_translate_tuple()),
        ?_test(test_translate_empty_list())
    ].

test_translate_list() ->
    State = new_state(),
    %% [1, 2, 3]
    Expr = {list_expr, [
        {literal, integer, 1, loc()},
        {literal, integer, 2, loc()},
        {literal, integer, 3, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assert(cerl:is_c_list(Core)).

test_translate_tuple() ->
    State = new_state(),
    %% (1, "hello", true)
    Expr = {tuple_expr, [
        {literal, integer, 1, loc()},
        {literal, string, <<"hello">>, loc()},
        {literal, bool, true, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assert(cerl:is_c_tuple(Core)),
    ?assertEqual(3, cerl:tuple_arity(Core)).

test_translate_empty_list() ->
    State = new_state(),
    Expr = {list_expr, [], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assert(cerl:is_c_nil(Core)).

%%====================================================================
%% Effect Operation Tests (1.3.1.5)
%%====================================================================

effect_operation_test_() ->
    [
        ?_test(test_translate_perform()),
        ?_test(test_translate_try_with())
    ].

test_translate_perform() ->
    State = new_state(),
    %% perform IO.print("hello")
    Expr = {perform_expr, 'IO', print, [
        {literal, string, <<"hello">>, loc()}
    ], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    %% Should be a call to catena_effect_runtime:perform
    ?assertEqual(call, cerl:type(Core)),
    Module = cerl:call_module(Core),
    ?assertEqual(catena_effect_runtime, cerl:atom_val(Module)).

test_translate_try_with() ->
    State = new_state(),
    %% try perform IO.read() with IO { read() -> "test" }
    Expr = {try_with_expr,
        {perform_expr, 'IO', read, [], loc()},
        [{handler_clause, 'IO', [
            {operation_case, read, [], {literal, string, <<"test">>, loc()}, loc()}
        ], loc()}],
        loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    %% Should be a call to catena_effect_runtime:with_handlers
    ?assertEqual(call, cerl:type(Core)),
    Module = cerl:call_module(Core),
    ?assertEqual(catena_effect_runtime, cerl:atom_val(Module)).

%%====================================================================
%% Constructor Translation Tests
%%====================================================================

constructor_test_() ->
    [
        ?_test(test_translate_constructor())
    ].

test_translate_constructor() ->
    State = new_state(),
    %% Some(42)
    Expr = {constructor, 'Some', [{literal, integer, 42, loc()}], loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    ?assert(cerl:is_c_tuple(Core)),
    %% First element should be the constructor name
    [Tag | _] = cerl:tuple_es(Core),
    ?assertEqual('Some', cerl:atom_val(Tag)).

%%====================================================================
%% Utilities Tests
%%====================================================================

utils_test_() ->
    [
        ?_test(test_fresh_var()),
        ?_test(test_fresh_vars())
    ].

test_fresh_var() ->
    State = catena_codegen_utils:new_state(),
    {Var1, State1} = catena_codegen_utils:fresh_var(State),
    {Var2, _State2} = catena_codegen_utils:fresh_var(State1),
    ?assertEqual('_@c0', cerl:var_name(Var1)),
    ?assertEqual('_@c1', cerl:var_name(Var2)).

test_fresh_vars() ->
    State = catena_codegen_utils:new_state(),
    {Vars, _State1} = catena_codegen_utils:fresh_vars(3, State),
    ?assertEqual(3, length(Vars)),
    Names = [cerl:var_name(V) || V <- Vars],
    ?assertEqual(['_@c0', '_@c1', '_@c2'], Names).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_complex_expression())
    ].

test_complex_expression() ->
    State = new_state(),
    %% let x = 1 in x + 2 |> f
    Expr = {let_expr, [
        {{var, x, loc()}, {literal, integer, 1, loc()}}
    ], {binary_op, '|>',
        {binary_op, '+', {var, x, loc()}, {literal, integer, 2, loc()}, loc()},
        {var, f, loc()},
        loc()}, loc()},
    {Core, _State1} = catena_codegen_expr:translate_expr(Expr, State),
    %% Should be a let expression
    ?assertEqual('let', cerl:type(Core)).
