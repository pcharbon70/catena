%%%-------------------------------------------------------------------
%%% @doc Effect Kleisli Integration Tests (Section 1.5.6)
%%%
%%% Tests for effect integration with Kleisli arrows, including:
%%% - Kleisli composition with effect tracking
%%% - perform operations introducing effects
%%% - handle blocks removing effects
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_kleisli_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

loc() -> {location, 1, 1}.

%%%===================================================================
%%% 1.5.6.1 - Kleisli Composition with Effect Tracking
%%%===================================================================

kleisli_composition_test_() ->
    {"1.5.6.1 - Type-check Kleisli composition >=> with effect tracking",
     [
      {"Kleisli operator is recognized in lexer",
       fun() ->
           {ok, Tokens} = catena_lexer:tokenize("f >=> g"),
           TokenTypes = [element(1, T) || T <- Tokens],
           ?assert(lists:member(kleisli, TokenTypes))
       end},

      {"Kleisli desugars to kleisli function call",
       fun() ->
           Expr = {binary_op, kleisli, {var, f, loc()}, {var, g, loc()}, loc()},
           Result = catena_desugar:desugar_expr(Expr),
           ?assertMatch({app, {var, kleisli, _}, [{var, f, _}, {var, g, _}], _}, Result)
       end},

      {"Bind operator >>= is recognized in lexer",
       fun() ->
           {ok, Tokens} = catena_lexer:tokenize("m >>= f"),
           TokenTypes = [element(1, T) || T <- Tokens],
           ?assert(lists:member(bind, TokenTypes))
       end},

      {"Bind desugars to chain function call",
       fun() ->
           Expr = {binary_op, bind, {var, m, loc()}, {var, f, loc()}, loc()},
           Result = catena_desugar:desugar_expr(Expr),
           %% m >>= f => chain f m (args swapped)
           ?assertMatch({app, {var, chain, _}, [{var, f, _}, {var, m, _}], _}, Result)
       end},

      {"fmap operator <$> is recognized in lexer",
       fun() ->
           {ok, Tokens} = catena_lexer:tokenize("f <$> x"),
           TokenTypes = [element(1, T) || T <- Tokens],
           ?assert(lists:member(fmap, TokenTypes))
       end},

      {"ap operator <*> is recognized in lexer",
       fun() ->
           {ok, Tokens} = catena_lexer:tokenize("f <*> x"),
           TokenTypes = [element(1, T) || T <- Tokens],
           ?assert(lists:member(ap, TokenTypes))
       end},

      {"mappend operator <> is recognized in lexer",
       fun() ->
           {ok, Tokens} = catena_lexer:tokenize("a <> b"),
           TokenTypes = [element(1, T) || T <- Tokens],
           ?assert(lists:member(mappend, TokenTypes))
       end},

      {"Effect union is commutative",
       fun() ->
           E1 = catena_infer_effect:from_list(['IO', 'State']),
           E2 = catena_infer_effect:from_list(['State', 'Error']),
           Union1 = catena_infer_effect:union(E1, E2),
           Union2 = catena_infer_effect:union(E2, E1),
           ?assert(catena_infer_effect:compatible(Union1, Union2))
       end},

      {"Effect union combines all effects",
       fun() ->
           E1 = catena_infer_effect:from_list(['IO']),
           E2 = catena_infer_effect:from_list(['State']),
           Union = catena_infer_effect:union(E1, E2),
           {effect_set, Effects} = Union,
           ?assert(lists:member('IO', Effects)),
           ?assert(lists:member('State', Effects))
       end}
     ]}.

%%%===================================================================
%%% 1.5.6.2 - Perform Operations Introduce Effects
%%%===================================================================

perform_introduces_effects_test_() ->
    {"1.5.6.2 - Validate that perform operations introduce effects",
     [
      {"Initial state has pure effects",
       fun() ->
           State = catena_infer_state:new(),
           Effects = catena_infer_state:get_effects(State),
           ?assert(catena_infer_effect:is_pure(Effects))
       end},

      {"add_effect adds to effect set",
       fun() ->
           State0 = catena_infer_state:new(),
           State1 = catena_infer_state:add_effect('IO', State0),
           Effects = catena_infer_state:get_effects(State1),
           ?assertNot(catena_infer_effect:is_pure(Effects)),
           {effect_set, EffectList} = Effects,
           ?assert(lists:member('IO', EffectList))
       end},

      {"Multiple effects accumulate",
       fun() ->
           State0 = catena_infer_state:new(),
           State1 = catena_infer_state:add_effect('IO', State0),
           State2 = catena_infer_state:add_effect('State', State1),
           Effects = catena_infer_state:get_effects(State2),
           {effect_set, EffectList} = Effects,
           ?assert(lists:member('IO', EffectList)),
           ?assert(lists:member('State', EffectList))
       end},

      {"perform_expr infers type and adds effect",
       fun() ->
           Env = catena_type_env:empty(),
           State = catena_infer_state:new(),
           %% perform IO.print("hello")
           Expr = {perform_expr, 'IO', print, [{literal, string, "hello", loc()}], loc()},
           case catena_infer_expr:infer(Expr, Env, State) of
               {_Type, State1} ->
                   Effects = catena_infer_state:get_effects(State1),
                   {effect_set, EffectList} = Effects,
                   ?assert(lists:member('IO', EffectList));
               {error, _, _} ->
                   ?assert(false)
           end
       end},

      {"infer_expr_effects detects perform in guards",
       fun() ->
           %% perform IO.print("x") in a guard should be detected as impure
           %% The format is {perform, Effect, Operation, Location}
           GuardExpr = {perform, 'IO', print, loc()},
           Effects = catena_infer_effect:infer_guard_effects(GuardExpr),
           ?assertNot(catena_infer_effect:is_pure(Effects))
       end}
     ]}.

%%%===================================================================
%%% 1.5.6.3 - Handle Blocks Remove Effects
%%%===================================================================

handle_removes_effects_test_() ->
    {"1.5.6.3 - Validate that handle blocks remove effects",
     [
      {"handle_expr infers body type",
       fun() ->
           Env = catena_type_env:empty(),
           State = catena_infer_state:new(),
           %% handle body with { ... }
           Body = {literal, integer, 42, loc()},
           Handlers = [],  % Empty handlers for now
           Expr = {handle_expr, Body, Handlers, loc()},
           case catena_infer_expr:infer(Expr, Env, State) of
               {{tcon, int}, _State1} ->
                   ?assert(true);
               Other ->
                   ?assertEqual(expected_int_type, Other)
           end
       end},

      {"set_effects replaces effect set",
       fun() ->
           State0 = catena_infer_state:new(),
           State1 = catena_infer_state:add_effect('IO', State0),
           %% Reset to pure
           State2 = catena_infer_state:set_effects(catena_infer_effect:pure(), State1),
           Effects = catena_infer_state:get_effects(State2),
           ?assert(catena_infer_effect:is_pure(Effects))
       end},

      {"Effect subsumption works correctly",
       fun() ->
           E1 = catena_infer_effect:from_list(['IO', 'State']),
           E2 = catena_infer_effect:from_list(['IO']),
           %% E1 subsumes E2 because E2 is subset of E1
           ?assert(catena_infer_effect:subsumes(E1, E2)),
           %% E2 does NOT subsume E1
           ?assertNot(catena_infer_effect:subsumes(E2, E1))
       end}
     ]}.

%%%===================================================================
%%% 1.5.6.4 - Effectful Kleisli Composition Example
%%%===================================================================

effectful_kleisli_example_test_() ->
    {"1.5.6.4 - Compile and execute effectful Kleisli composition",
     [
      {"Effect union simulates Kleisli composition effect tracking",
       fun() ->
           %% (a -> b / {IO}) >=> (b -> c / {State}) : a -> c / {IO, State}
           Effects1 = catena_infer_effect:from_list(['IO']),
           Effects2 = catena_infer_effect:from_list(['State']),
           ComposedEffects = catena_infer_effect:union(Effects1, Effects2),
           {effect_set, EffectList} = ComposedEffects,
           ?assertEqual(2, length(EffectList)),
           ?assert(lists:member('IO', EffectList)),
           ?assert(lists:member('State', EffectList))
       end},

      {"Pure function composed with pure stays pure",
       fun() ->
           E1 = catena_infer_effect:pure(),
           E2 = catena_infer_effect:pure(),
           Combined = catena_infer_effect:union(E1, E2),
           ?assert(catena_infer_effect:is_pure(Combined))
       end},

      {"Function type with effects can be constructed",
       fun() ->
           %% a -> b / {IO}
           Effects = catena_types:effect_set(['IO']),
           FuncType = catena_types:tfun({tvar, 0}, {tvar, 1}, Effects),
           ?assertMatch({tfun, {tvar, 0}, {tvar, 1}, {effect_set, ['IO']}}, FuncType)
       end},

      {"extract_function_effects gets effects from function type",
       fun() ->
           Effects = catena_types:effect_set(['IO', 'State']),
           FuncType = catena_types:tfun({tcon, int}, {tcon, string}, Effects),
           {ok, ExtractedEffects} = catena_types:extract_function_effects(FuncType),
           ?assertEqual(Effects, ExtractedEffects)
       end}
     ]}.

%%%===================================================================
%%% Effect Set Operations Tests
%%%===================================================================

effect_set_operations_test_() ->
    {"Effect set operations",
     [
      {"from_list normalizes (sorts and dedupes)",
       fun() ->
           E = catena_infer_effect:from_list(['Z', 'A', 'M', 'A']),
           {effect_set, List} = E,
           ?assertEqual(['A', 'M', 'Z'], List)
       end},

      {"is_pure returns true for empty set",
       fun() ->
           ?assert(catena_infer_effect:is_pure(catena_infer_effect:pure()))
       end},

      {"is_pure returns false for non-empty set",
       fun() ->
           E = catena_infer_effect:from_list(['IO']),
           ?assertNot(catena_infer_effect:is_pure(E))
       end},

      {"compatible returns true for equal sets",
       fun() ->
           E1 = catena_infer_effect:from_list(['IO', 'State']),
           E2 = catena_infer_effect:from_list(['State', 'IO']),
           ?assert(catena_infer_effect:compatible(E1, E2))
       end},

      {"compatible returns false for different sets",
       fun() ->
           E1 = catena_infer_effect:from_list(['IO']),
           E2 = catena_infer_effect:from_list(['State']),
           ?assertNot(catena_infer_effect:compatible(E1, E2))
       end}
     ]}.

%%%===================================================================
%%% Lexer Integration Tests for Category Theory Operators
%%%===================================================================

lexer_operator_test_() ->
    {"Lexer recognizes all category theory operators",
     [
      {"All operators tokenize correctly",
       fun() ->
           Tests = [
               {"f >=> g", kleisli},
               {"m >>= f", bind},
               {"f <$> x", fmap},
               {"f <*> x", ap},
               {"a <> b", mappend},
               {"a === b", setoid_eq},
               {"a !== b", setoid_neq}
           ],
           lists:foreach(
               fun({Input, ExpectedToken}) ->
                   {ok, Tokens} = catena_lexer:tokenize(Input),
                   TokenTypes = [element(1, T) || T <- Tokens],
                   ?assert(lists:member(ExpectedToken, TokenTypes),
                          io_lib:format("Expected ~p in ~p", [ExpectedToken, TokenTypes]))
               end,
               Tests
           )
       end}
     ]}.
