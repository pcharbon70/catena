%%%-------------------------------------------------------------------
%%% @doc Standard Library Integration Tests
%%%
%%% Tests for Phase 1.5 integration test requirements:
%%% - map (fn x -> x + 1) [1, 2, 3] produces [2, 3, 4]
%%% - Some 5 >>= (fn x -> Some (x + 1)) produces Some 6
%%% - [1, 2] <> [3, 4] produces [1, 2, 3, 4]
%%% - do-notation with Maybe
%%%
%%% These tests verify the Erlang prelude implementation works correctly,
%%% and that Catena code using imported constructors compiles properly.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_stdlib_integration_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Erlang Prelude Runtime Tests
%%
%% These test the catena_prelude.erl implementation directly.
%% This verifies the runtime semantics before full Catena compilation.
%%====================================================================

prelude_runtime_test_() ->
    {"Erlang Prelude Runtime Tests",
     [
      {"map (fn x -> x + 1) [1, 2, 3] produces [2, 3, 4]",
       fun() ->
           Result = catena_prelude:map(fun(X) -> X + 1 end, [1, 2, 3]),
           ?assertEqual([2, 3, 4], Result)
       end},

      {"Some 5 >>= (fn x -> Some (x + 1)) produces Some 6",
       fun() ->
           %% In Erlang prelude: Some = {some, _}, None = none
           Some5 = {some, 5},
           F = fun(X) -> {some, X + 1} end,
           Result = catena_prelude:bind(Some5, F),
           ?assertEqual({some, 6}, Result)
       end},

      {"None >>= (fn x -> Some (x + 1)) produces None",
       fun() ->
           F = fun(X) -> {some, X + 1} end,
           Result = catena_prelude:bind(none, F),
           ?assertEqual(none, Result)
       end},

      {"[1, 2] <> [3, 4] produces [1, 2, 3, 4]",
       fun() ->
           Result = catena_prelude:append([1, 2], [3, 4]),
           ?assertEqual([1, 2, 3, 4], Result)
       end},

      {"chain (fn x -> Some (x * 2)) (Some 21) produces Some 42",
       fun() ->
           F = fun(X) -> {some, X * 2} end,
           Result = catena_prelude:chain(F, {some, 21}),
           ?assertEqual({some, 42}, Result)
       end},

      {"Result bind: Ok 5 >>= (fn x -> Ok (x + 1)) produces Ok 6",
       fun() ->
           Ok5 = {ok, 5},
           F = fun(X) -> {ok, X + 1} end,
           Result = catena_prelude:bind(Ok5, F),
           ?assertEqual({ok, 6}, Result)
       end},

      {"Result bind: Err e >>= f produces Err e",
       fun() ->
           ErrE = {err, "error"},
           F = fun(X) -> {ok, X + 1} end,
           Result = catena_prelude:bind(ErrE, F),
           ?assertEqual({err, "error"}, Result)
       end},

      {"List bind (flatMap): [1, 2] >>= (fn x -> [x, x*2]) produces [1, 2, 2, 4]",
       fun() ->
           F = fun(X) -> [X, X * 2] end,
           Result = catena_prelude:bind([1, 2], F),
           ?assertEqual([1, 2, 2, 4], Result)
       end},

      {"compose: (double . addOne) 5 = 12",
       fun() ->
           AddOne = fun(X) -> X + 1 end,
           Double = fun(X) -> X * 2 end,
           Composed = catena_prelude:compose(Double, AddOne),
           ?assertEqual(12, Composed(5))
       end},

      {"filter: filter even [1,2,3,4,5] = [2,4]",
       fun() ->
           IsEven = fun(X) -> X rem 2 == 0 end,
           Result = catena_prelude:filter(IsEven, [1, 2, 3, 4, 5]),
           ?assertEqual([2, 4], Result)
       end},

      {"fold: fold (+) 0 [1,2,3,4,5] = 15",
       fun() ->
           Add = fun(X, Acc) -> X + Acc end,
           Result = catena_prelude:fold(Add, 0, [1, 2, 3, 4, 5]),
           ?assertEqual(15, Result)
       end}
     ]}.

%%====================================================================
%% Do-Notation Simulation Tests
%%
%% Tests that simulate what do-notation would desugar to.
%%====================================================================

do_notation_simulation_test_() ->
    {"Do-Notation Simulation Tests",
     [
      {"do { x <- Some 5; y <- Some 3; pure (x + y) } = Some 8",
       fun() ->
           %% Desugars to:
           %% chain (fn x -> chain (fn y -> pure (x + y)) (Some 3)) (Some 5)
           Result = catena_prelude:chain(
               fun(X) ->
                   catena_prelude:chain(
                       fun(Y) ->
                           catena_prelude:pure(X + Y)
                       end,
                       {some, 3}
                   )
               end,
               {some, 5}
           ),
           ?assertEqual({some, 8}, Result)
       end},

      {"do { x <- None; pure x } = None",
       fun() ->
           Result = catena_prelude:chain(
               fun(X) -> catena_prelude:pure(X) end,
               none
           ),
           ?assertEqual(none, Result)
       end},

      {"do { x <- [1,2]; y <- [3,4]; pure (x,y) } = [(1,3),(1,4),(2,3),(2,4)]",
       fun() ->
           Result = catena_prelude:bind([1, 2],
               fun(X) ->
                   catena_prelude:bind([3, 4],
                       fun(Y) -> [{X, Y}] end
                   )
               end
           ),
           ?assertEqual([{1, 3}, {1, 4}, {2, 3}, {2, 4}], Result)
       end}
     ]}.

%%====================================================================
%% Catena Import and Constructor Tests
%%
%% Tests that Catena code using imported constructors compiles correctly.
%%====================================================================

import_constructor_test_() ->
    {"Catena Import and Constructor Tests",
     [
      {"Import Prelude and use Some constructor",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform wrap x = Some x",
           {ok, _} = catena_compile:compile_string(Source),
           ok
       end},

      {"Import Prelude and use None constructor",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform nothing = None",
           {ok, _} = catena_compile:compile_string(Source),
           ok
       end},

      {"Import Prelude and use Either constructors",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform left x = Left x\ntransform right x = Right x",
           {ok, _} = catena_compile:compile_string(Source),
           ok
       end},

      {"Import Prelude and use Result constructors",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform ok x = Ok x\ntransform err x = Err x",
           {ok, _} = catena_compile:compile_string(Source),
           ok
       end},

      {"Import Prelude and use Ordering constructors",
       fun() ->
           Source = "module Test\nimport Prelude\ntransform lt = LT\ntransform eq = EQ\ntransform gt = GT",
           {ok, _} = catena_compile:compile_string(Source),
           ok
       end}
     ]}.

%%====================================================================
%% Operator Desugaring Verification Tests
%%
%% Tests that operator desugaring produces correct AST.
%%====================================================================

operator_desugaring_test_() ->
    {"Operator Desugaring Verification",
     [
      {"<> operator desugars to combine",
       fun() ->
           Source = "module Test\ntransform concat a b = a <> b",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, AST} = catena_parser:parse(Tokens),
           %% Desugar (returns term directly, not {ok, term})
           Desugared = catena_desugar:desugar(AST),
           %% Should have desugared <> to combine call
           {module, _, _, _, [Decl], _} = Desugared,
           {transform_decl, concat, _, Clauses, _} = Decl,
           [{transform_clause, _, _, Body, _}] = Clauses,
           %% Body is {app, {var, combine, Loc}, [A, B], Loc}
           ?assertMatch({app, {var, combine, _}, [_, _], _}, Body)
       end},

      {"=== operator desugars to equals",
       fun() ->
           Source = "module Test\ntransform eq a b = a === b",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, AST} = catena_parser:parse(Tokens),
           Desugared = catena_desugar:desugar(AST),
           {module, _, _, _, [Decl], _} = Desugared,
           {transform_decl, eq, _, Clauses, _} = Decl,
           [{transform_clause, _, _, Body, _}] = Clauses,
           ?assertMatch({app, {var, equals, _}, [_, _], _}, Body)
       end},

      {">=> (kleisli) operator desugars to kleisli call",
       fun() ->
           Source = "module Test\ntransform compose f g = f >=> g",
           {ok, Tokens, _} = catena_lexer:string(Source),
           {ok, AST} = catena_parser:parse(Tokens),
           Desugared = catena_desugar:desugar(AST),
           {module, _, _, _, [Decl], _} = Desugared,
           {transform_decl, compose, _, Clauses, _} = Decl,
           [{transform_clause, _, _, Body, _}] = Clauses,
           ?assertMatch({app, {var, kleisli, _}, [_, _], _}, Body)
       end}
     ]}.

%%====================================================================
%% Kleisli Composition Tests
%%
%% Tests for effectful Kleisli composition using the prelude.
%%====================================================================

kleisli_composition_test_() ->
    {"Kleisli Composition Tests",
     [
      {"Kleisli composition of Maybe functions",
       fun() ->
           %% f >=> g = \x -> f x >>= g
           %% safeSqrt :: Int -> Maybe Int (returns None for negative)
           %% safeRecip :: Int -> Maybe Int (returns None for 0)
           SafeSqrt = fun(X) when X >= 0 -> {some, trunc(math:sqrt(X))};
                         (_) -> none
                      end,
           SafeRecip = fun(0) -> none;
                          (X) -> {some, 1.0 / X}
                       end,

           %% Kleisli compose: safeSqrt >=> safeRecip
           KleisliCompose = fun(F, G) ->
               fun(X) -> catena_prelude:bind(F(X), G) end
           end,

           Composed = KleisliCompose(SafeSqrt, SafeRecip),

           %% sqrt(4) = 2, recip(2) = 0.5
           ?assertEqual({some, 0.5}, Composed(4)),
           %% sqrt(-1) = None
           ?assertEqual(none, Composed(-1)),
           %% sqrt(0) = 0, recip(0) = None
           ?assertEqual(none, Composed(0))
       end}
     ]}.
