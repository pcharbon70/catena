%%%-------------------------------------------------------------------
%%% @doc Standard Library Validation Tests
%%%
%%% Tests for Phase 1.5 - Standard Library Validation.
%%% This module validates that the type inference system can handle
%%% the patterns used in the standard library.
%%%
%%% Currently validates:
%%% - Binary operator type inference
%%% - List literal type inference
%%% - Match expression type inference
%%% - Basic function application
%%%
%%% Future work (documented as skipped tests):
%%% - Full trait instance resolution
%%% - Higher-kinded type validation
%%% - Do-notation desugaring
%%% - Operator desugaring to trait methods
%%% @end
%%%-------------------------------------------------------------------
-module(catena_stdlib_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    catena_type_env:empty().

state() ->
    catena_infer_state:new().

%%%===================================================================
%%% 1.5.1 Basic Expression Inference Tests
%%%===================================================================

binary_op_inference_test_() ->
    {"Binary operator type inference",
     {setup, fun setup/0,
      fun(Env) ->
          [
           {"Addition: Int + Int = Int",
            fun() ->
                Expr = {binary_op, plus, {lit, {int, 1}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, int}, Type)
            end},

           {"Subtraction: Int - Int = Int",
            fun() ->
                Expr = {binary_op, minus, {lit, {int, 5}}, {lit, {int, 3}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, int}, Type)
            end},

           {"Multiplication: Int * Int = Int",
            fun() ->
                Expr = {binary_op, star, {lit, {int, 4}}, {lit, {int, 5}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, int}, Type)
            end},

           {"Division: Int / Int = Int",
            fun() ->
                Expr = {binary_op, slash, {lit, {int, 10}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, int}, Type)
            end},

           {"Less than: Int < Int = Bool",
            fun() ->
                Expr = {binary_op, lt, {lit, {int, 1}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Greater than: Int > Int = Bool",
            fun() ->
                Expr = {binary_op, gt, {lit, {int, 3}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Less or equal: Int <= Int = Bool",
            fun() ->
                Expr = {binary_op, lte, {lit, {int, 2}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Greater or equal: Int >= Int = Bool",
            fun() ->
                Expr = {binary_op, gte, {lit, {int, 3}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Equality: Int == Int = Bool",
            fun() ->
                Expr = {binary_op, eq, {lit, {int, 1}}, {lit, {int, 1}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Inequality: Int /= Int = Bool",
            fun() ->
                Expr = {binary_op, neq, {lit, {int, 1}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Boolean and: Bool and Bool = Bool",
            fun() ->
                Expr = {binary_op, 'and', {lit, {bool, true}}, {lit, {bool, false}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Boolean or: Bool or Bool = Bool",
            fun() ->
                Expr = {binary_op, 'or', {lit, {bool, true}}, {lit, {bool, false}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Setoid equality: a === a = Bool",
            fun() ->
                Expr = {binary_op, setoid_eq, {lit, {int, 1}}, {lit, {int, 1}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end},

           {"Setoid inequality: a !== a = Bool",
            fun() ->
                Expr = {binary_op, setoid_neq, {lit, {int, 1}}, {lit, {int, 2}}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end}
          ]
      end}}.

pipe_operator_test_() ->
    {"Pipe operator type inference",
     {setup, fun setup/0,
      fun(Env0) ->
          %% Add function f : Int -> Bool to environment
          FType = {tfun, {tcon, int}, {tcon, bool}, {effect_set, []}},
          FScheme = {mono, FType},
          Env = catena_type_env:extend(Env0, f, FScheme),

          [
           {"Pipe: x |> f = f(x)",
            fun() ->
                Expr = {binary_op, pipe_right, {lit, {int, 1}}, {var, f}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, bool}, Type)
            end}
          ]
      end}}.

list_inference_test_() ->
    {"List literal type inference",
     {setup, fun setup/0,
      fun(Env) ->
          [
           {"Empty list: [] : List a",
            fun() ->
                Expr = {list, [], {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertMatch({tapp, {tcon, list}, [_]}, Type)
            end},

           {"Integer list: [1, 2, 3] : List Int",
            fun() ->
                Expr = {list, [{lit, {int, 1}}, {lit, {int, 2}}, {lit, {int, 3}}], {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tapp, {tcon, list}, [{tcon, int}]}, Type)
            end},

           {"String list: [\"a\", \"b\"] : List String",
            fun() ->
                Expr = {list, [{lit, {string, "a"}}, {lit, {string, "b"}}], {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tapp, {tcon, list}, [{tcon, string}]}, Type)
            end},

           {"Cons: 1 :: [2, 3] : List Int",
            fun() ->
                Tail = {list, [{lit, {int, 2}}, {lit, {int, 3}}], {loc, 1, 1}},
                Expr = {cons, {lit, {int, 1}}, Tail, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tapp, {tcon, list}, [{tcon, int}]}, Type)
            end}
          ]
      end}}.

list_append_test_() ->
    {"List append operator inference",
     {setup, fun setup/0,
      fun(Env0) ->
          %% Create list type for environment
          ListIntType = {tapp, {tcon, list}, [{tcon, int}]},
          XsScheme = {mono, ListIntType},
          YsScheme = {mono, ListIntType},
          Env = catena_type_env:extend(
                  catena_type_env:extend(Env0, xs, XsScheme),
                  ys, YsScheme),

          [
           {"List append: xs ++ ys : List Int",
            fun() ->
                Expr = {binary_op, plus_plus, {var, xs}, {var, ys}, {loc, 1, 1}},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual(ListIntType, Type)
            end}
          ]
      end}}.

%%%===================================================================
%%% 1.5.1 Match Expression Tests
%%%===================================================================

match_expression_test_() ->
    {"Match expression type inference",
     {setup, fun setup/0,
      fun(Env0) ->
          %% Add x : Int to environment
          IntScheme = catena_type_scheme:mono({tcon, int}),
          Env = catena_type_env:extend(Env0, x, IntScheme),

          [
           {"Simple match with literal patterns",
            fun() ->
                %% match x of | 0 -> "zero" | _ -> "other" end
                %% Pattern AST uses {plit, Lit} and {pwild}
                Clauses = [
                    {{plit, {int, 0}}, {lit, {string, "zero"}}},
                    {{pwild}, {lit, {string, "other"}}}
                ],
                Expr = {match, {var, x}, Clauses},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, string}, Type)
            end},

           {"Match with variable binding",
            fun() ->
                %% match x of | n -> n end
                %% Pattern AST uses {pvar, Name}
                Clauses = [
                    {{pvar, n}, {var, n}}
                ],
                Expr = {match, {var, x}, Clauses},
                {Type, _State} = catena_infer_expr:infer(Expr, Env, state()),
                ?assertEqual({tcon, int}, Type)
            end}
          ]
      end}}.

%%%===================================================================
%%% 1.5.2 Trait Instance Resolution Tests (Placeholder)
%%%===================================================================

%% TODO: These tests require full trait instance resolution
%% which is not yet implemented. Uncomment when trait system is complete.

%% trait_instance_resolution_test_() ->
%%     {"Trait instance resolution",
%%      [
%%       {"Resolve Mapper instance for Maybe",
%%        fun() ->
%%            %% map f (Some x) should resolve Mapper Maybe instance
%%            ?assert(false) %% Not yet implemented
%%        end},
%%
%%       {"Resolve Mapper instance for List",
%%        fun() ->
%%            %% map f [1, 2, 3] should resolve Mapper List instance
%%            ?assert(false) %% Not yet implemented
%%        end}
%%      ]}.

%%%===================================================================
%%% 1.5.5 Do-Notation Tests (Placeholder)
%%%===================================================================

%% TODO: These tests require do-notation desugaring
%% which is not yet implemented. Uncomment when desugar pass is complete.

%% do_notation_desugaring_test_() ->
%%     {"Do-notation desugaring",
%%      [
%%       {"Desugar bind: x <- ma; rest",
%%        fun() ->
%%            %% do { x <- ma; pure x } => ma >>= (\x -> pure x)
%%            ?assert(false) %% Not yet implemented
%%        end}
%%      ]}.

%%%===================================================================
%%% 1.5.7 Operator Desugaring Tests (Placeholder)
%%%===================================================================

%% TODO: These tests require operator to trait method desugaring
%% which is not yet implemented. Uncomment when desugar pass is complete.

%% operator_desugaring_test_() ->
%%     {"Operator desugaring to trait methods",
%%      [
%%       {"Desugar <$> to Mapper.map",
%%        fun() ->
%%            %% f <$> x => map f x
%%            ?assert(false) %% Not yet implemented
%%        end},
%%
%%       {"Desugar >>= to Chainable.chain",
%%        fun() ->
%%            %% ma >>= f => chain f ma
%%            ?assert(false) %% Not yet implemented
%%        end},
%%
%%       {"Desugar <> to Combiner.combine",
%%        fun() ->
%%            %% a <> b => combine a b
%%            ?assert(false) %% Not yet implemented
%%        end}
%%      ]}.

%%%===================================================================
%%% Type Error Tests
%%%===================================================================

type_error_test_() ->
    {"Type error detection",
     {setup, fun setup/0,
      fun(Env) ->
          [
           {"Type mismatch in binary op: Int + Bool should fail",
            fun() ->
                Expr = {binary_op, plus, {lit, {int, 1}}, {lit, {bool, true}}, {loc, 1, 1}},
                Result = catena_infer_expr:infer(Expr, Env, state()),
                ?assertMatch({error, _, _}, Result)
            end},

           {"Type mismatch in boolean op: and with Int should fail",
            fun() ->
                Expr = {binary_op, 'and', {lit, {int, 1}}, {lit, {bool, true}}, {loc, 1, 1}},
                Result = catena_infer_expr:infer(Expr, Env, state()),
                ?assertMatch({error, _, _}, Result)
            end},

           {"Heterogeneous list should fail",
            fun() ->
                Expr = {list, [{lit, {int, 1}}, {lit, {string, "a"}}], {loc, 1, 1}},
                Result = catena_infer_expr:infer(Expr, Env, state()),
                ?assertMatch({error, _, _}, Result)
            end}
          ]
      end}}.

%%%===================================================================
%%% Integration with Parser Tests
%%%===================================================================

%% These tests verify that parser-generated AST can be type-checked
%% Note: The parser expects full programs, not just expressions.
%% For expression-level testing, we construct AST directly.
