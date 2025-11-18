%%%-------------------------------------------------------------------
%%% @doc Tests for Algorithm W Orchestrator (catena_infer)
%%%
%%% Tests the top-level API that orchestrates all type inference
%%% components into a complete workflow.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_infer_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

teardown(_) ->
    ok.

%%====================================================================
%% Basic Expression Inference Tests
%%====================================================================

basic_inference_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(test_literal_inference()),
      ?_test(test_variable_inference()),
      ?_test(test_lambda_inference()),
      ?_test(test_application_inference()),
      ?_test(test_let_binding()),
      ?_test(test_if_expression())
     ]}.

test_literal_inference() ->
    % Test: 42 : Int
    Expr = {lit, {int, 42}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(Expr)),
    
    % Test: 3.14 : Float
    Expr2 = {lit, {float, 3.14}},
    ?assertEqual({ok, catena_types:tcon(float)}, catena_infer:infer_expr(Expr2)),
    
    % Test: true : Bool
    Expr3 = {lit, {bool, true}},
    ?assertEqual({ok, catena_types:tcon(bool)}, catena_infer:infer_expr(Expr3)),
    
    % Test: "hello" : String
    Expr4 = {lit, {string, "hello"}},
    ?assertEqual({ok, catena_types:tcon(string)}, catena_infer:infer_expr(Expr4)).

test_variable_inference() ->
    % Test: x in empty environment -> error
    Expr = {var, x},
    ?assertMatch({error, [_Error]}, catena_infer:infer_expr(Expr)),

    % Test: x in environment with x : Int -> Int
    Env = catena_type_env:empty(),
    Scheme = catena_type_scheme:mono(catena_types:tcon(int)),
    Env1 = catena_type_env:extend(Env, x, Scheme),
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(Expr, Env1)).

test_lambda_inference() ->
    % Test: λx. x : α₁ -> α₁ (identity function)
    Expr = {lam, x, {var, x}},
    ?assertMatch(
        {ok, {tfun, {tvar, _SameTypeVar}, {tvar, _SameTypeVar}, _Effects}},
        catena_infer:infer_expr(Expr)
    ),
    
    % Test: λx. 42 : α₁ -> Int (constant function)
    Expr2 = {lam, x, {lit, {int, 42}}},
    ?assertMatch(
        {ok, {tfun, {tvar, _Alpha2}, {tcon, int}, _Effects}},
        catena_infer:infer_expr(Expr2)
    ).

test_application_inference() ->
    % Test: (λx. x) 42 : Int
    Lam = {lam, x, {var, x}},
    App = {app, Lam, {lit, {int, 42}}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(App)),
    
    % Test: (λx. 42) "hello" : Int
    Lam2 = {lam, x, {lit, {int, 42}}},
    App2 = {app, Lam2, {lit, {string, "hello"}}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(App2)).

test_let_binding() ->
    % Test: let id = λx. x in id 42 : Int
    Lam = {lam, x, {var, x}},
    Let = {'let', id, Lam, {app, {var, id}, {lit, {int, 42}}}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(Let)),
    
    % Test: let x = 42 in x : Int
    Let2 = {'let', x, {lit, {int, 42}}, {var, x}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(Let2)).

test_if_expression() ->
    % Test: if true then 42 else 99 : Int
    If = {'if', {lit, {bool, true}}, {lit, {int, 42}}, {lit, {int, 99}}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(If)),
    
    % Test: if true then 1 else 2.0 : error (type mismatch)
    If2 = {'if', {lit, {bool, true}}, {lit, {int, 1}}, {lit, {float, 2.0}}},
    ?assertMatch({error, [_Error]}, catena_infer:infer_expr(If2)).

%%====================================================================
%% Type Polymorphism Tests
%%====================================================================

polymorphism_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(let_polymorphism()),
      ?_test(letrec_polymorphism()),
      ?_test(function_reuse())
     ]}.

let_polymorphism() ->
    % Test: let id = λx. x in (id 42, id true) : (Int, Bool)
    Lam = {lam, x, {var, x}},
    Let = {'let', id, Lam, {tuple, [
        {app, {var, id}, {lit, {int, 42}}},
        {app, {var, id}, {lit, {bool, true}}}
    ]}},
    
    ?assertMatch(
        {ok, {ttuple, [{tcon, int}, {tcon, bool}]}},
        catena_infer:infer_expr(Let)
    ).

letrec_polymorphism() ->
    % Test: letrec id = λx. x in id (id 42) : Int
    Lam = {lam, x, {var, x}},
    Letrec = {'letrec', id, Lam, {app, {var, id}, {app, {var, id}, {lit, {int, 42}}}}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(Letrec)).

function_reuse() ->
    % Test: let f = λx. x in let g = λy. y in f (g 42) : Int
    F = {lam, x, {var, x}},
    G = {lam, y, {var, y}},
    Let = {'let', f, F, {'let', g, G, {app, {var, f}, {app, {var, g}, {lit, {int, 42}}}}}},
    ?assertEqual({ok, catena_types:tcon(int)}, catena_infer:infer_expr(Let)).

%%====================================================================
%% Program Type Checking Tests
%%====================================================================

program_checking_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(simple_program()),
      ?_test(compose_program()),
      ?_test(program_with_error())
     ]}.

simple_program() ->
    % Program: let id = λx. x; let const = λy. 42
    Program = [
        {id, {lam, x, {var, x}}},
        {const, {lam, y, {lit, {int, 42}}}}
    ],
    
    ?assertMatch(
        {ok, #{id := _, const := _}},
        catena_infer:check_program(Program)
    ).

compose_program() ->
    % Program demonstrates function composition
    Compose = {lam, f, {lam, g, {lam, x, 
        {app, {var, f}, {app, {var, g}, {var, x}}}
    }}},
    
    Id = {lam, x, {var, x}},
    Const = {lam, y, {lam, x, {var, y}}},
    
    Program = [
        {compose, Compose},
        {id, Id},
        {const, Const}
    ],
    
    ?assertMatch(
        {ok, #{compose := _, id := _, const := _}},
        catena_infer:check_program(Program)
    ).

program_with_error() ->
    % Program with unbound variable
    BadExpr = {app, {var, unbound}, {lit, {int, 42}}},
    Program = [
        {bad_fun, BadExpr}
    ],
    
    ?assertMatch(
        {error, [_Error]},
        catena_infer:check_program(Program)
    ).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(unbound_variable_error()),
      ?_test(type_mismatch_error()),
      ?_test(arity_mismatch_error()),
      ?_test(error_accumulation())
     ]}.

unbound_variable_error() ->
    % Test: unbound variable
    Expr = {var, nonexistent},
    {error, Errors} = catena_infer:infer_expr(Expr),
    ?assert(length(Errors) > 0),
    ?assert(is_list(catena_type_error:format_error(hd(Errors)))).

type_mismatch_error() ->
    % Test: applying Int as function
    Expr = {app, {lit, {int, 42}}, {lit, {int, 99}}},
    ?assertMatch(
        {error, [_Error]},
        catena_infer:infer_expr(Expr)
    ).

arity_mismatch_error() ->
    % Test: applying function to wrong number of args
    Fun = {lam, x, {var, x}},
    WrongApp = {app, {app, Fun, {lit, {int, 42}}}, {lit, {int, 99}}},
    ?assertMatch(
        {error, [_Error]},
        catena_infer:infer_expr(WrongApp)
    ).

error_accumulation() ->
    % Test: error in expression (currently only first error is reported)
    BadLet = {'let', bad_var, {app, {var, unbound}, {lit, {int, 42}}}, {var, unbound2}},
    ?assertMatch(
        {error, [_Error | _]},  % At least one error
        catena_infer:infer_expr(BadLet)
    ).

%%====================================================================
%% Integration Workflow Tests
%%====================================================================

integration_workflow_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(complete_algorithm_w_workflow()),
      ?_test(test_pipeline_composition()),
      ?_test(nested_expressions())
     ]}.

complete_algorithm_w_workflow() ->
    % Complete workflow: inference -> unification -> generalization ->instantiation
    
    % Step 1: Identity function inference
    IdExpr = {lam, x, {var, x}},
    catena_infer:infer_expr(IdExpr),
    
    % Step 2: Use polymorphically
    Let = {'let', id, IdExpr, {tuple, [
        {app, {var, id}, {lit, {int, 42}}},
        {app, {var, id}, {lit, {bool, true}}}
    ]}},
    
    catena_infer:infer_expr(Let),
    
    % Step 3: Verify result
    ExpectedType = {ttuple, [{tcon, int}, {tcon, bool}]},
    ?assertMatch(
        {ok, ExpectedType},
        catena_infer:infer_expr(Let)
    ).

test_pipeline_composition() ->
    % Test function composition pipeline
    % f ∘ g = λx. f (g x)
    Comp = {lam, f, {lam, g, {lam, x, 
        {app, {var, f}, {app, {var, g}, {var, x}}}
    }}},
    
    % Use composition with actual functions
    Add1 = {lam, x, {tuple, [{var, x}, {lit, {int, 1}}]}},
    Head = {lam, pair, {tuple, [{var, pair}, {lit, {int, 2}}]}},

    Composed = {app, {app, Comp, Head}, Add1},
    ?assertMatch(
        {ok, {tfun, {tvar, _Input}, _, _Effects}},  % Output may be concrete or tvar
        catena_infer:infer_expr(Composed)
    ).

nested_expressions() ->
    % Deep nesting with multiple let-bindings
    Inner = {'let', y, {lit, {int, 1}}, {tuple, [{var, x}, {var, y}]}},
    _Middle = {'let', x, {lit, {int, 2}}, Inner},
    Outer = {'let', z, {lit, {int, 3}}, {tuple, [{var, x}, {var, y}, {var, z}]}},
    
    % This should fail because y is not in scope in Outer
    ?assertMatch(
        {error, [_Error]},
        catena_infer:infer_expr(Outer)
    ).

%%====================================================================
%% Convenience Function Tests
%%====================================================================

convenience_functions_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [
      ?_test(infer_lambda_helper()),
      ?_test(check_type_helper()),
      ?_test(format_errors_helper())
     ]}.

infer_lambda_helper() ->
    % Test the infer_lambda convenience function
    ?assertMatch(
        {ok, {tfun, {tcon, int}, {tcon, bool}, _Effects}},
        catena_infer:infer_lambda(x, catena_types:tcon(int), {lit, {bool, true}})
    ).

check_type_helper() ->
    % Test type checking function
    IntExpr = {lit, {int, 42}},
    BoolExpr = {lit, {bool, true}},
    
    ?assertEqual(
        true,
        catena_infer:check_type(IntExpr, catena_types:tcon(int))
    ),
    
    ?assertEqual(
        false,
        catena_infer:check_type(BoolExpr, catena_types:tcon(int))
    ).

format_errors_helper() ->
    % Test error formatting
    Expr = {var, nonexistent},
    {error, Errors} = catena_infer:infer_expr(Expr),
    
    Formatted = catena_infer:format_errors(Errors),
    ?assert(is_list(Formatted)),
    ?assert(length(Formatted) > 0).