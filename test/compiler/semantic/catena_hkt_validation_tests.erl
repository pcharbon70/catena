%%%-------------------------------------------------------------------
%%% @doc Higher-Kinded Type Validation Tests (Section 1.5.3)
%%%
%%% These tests validate that higher-kinded types work correctly for
%%% traits like Mapper and Pipeline.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_hkt_validation_tests).

-include_lib("eunit/include/eunit.hrl").

%% Import common test helpers
-import(catena_test_helpers, [loc/0]).

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Build kind environment with builtins
kind_env() ->
    catena_kind:build_kind_env([]).

%%%===================================================================
%%% 1.5.3.1 - Kind Checking for Traits
%%%===================================================================

trait_kind_checking_test_() ->
    {"1.5.3.1 - Validate kind checking for trait Mapper f",
     [
      {"Mapper trait parameter f has kind Type -> Type",
       fun() ->
           %% trait Mapper f where
           %%   map : (a -> b) -> f a -> f b
           Trait = {trait_decl, 'Mapper', [f], [],
               [{trait_sig, map,
                   {type_fun,
                       {type_fun, {type_var, a, loc()}, {type_var, b, loc()}, loc()},
                       {type_fun,
                           {type_app, {type_var, f, loc()}, [{type_var, a, loc()}], loc()},
                           {type_app, {type_var, f, loc()}, [{type_var, b, loc()}], loc()},
                           loc()},
                       loc()},
                   loc()}],
               loc()},
           {ok, Kinds} = catena_kind:check_trait_kind(Trait),
           ?assertEqual([{f, {arrow, star, star}}], Kinds)
       end},

      {"Chainable trait parameter m has kind Type -> Type",
       fun() ->
           %% trait Chainable m where
           %%   chain : (a -> m b) -> m a -> m b
           Trait = {trait_decl, 'Chainable', [m], [],
               [{trait_sig, chain,
                   {type_fun,
                       {type_fun, {type_var, a, loc()},
                           {type_app, {type_var, m, loc()}, [{type_var, b, loc()}], loc()},
                           loc()},
                       {type_fun,
                           {type_app, {type_var, m, loc()}, [{type_var, a, loc()}], loc()},
                           {type_app, {type_var, m, loc()}, [{type_var, b, loc()}], loc()},
                           loc()},
                       loc()},
                   loc()}],
               loc()},
           {ok, Kinds} = catena_kind:check_trait_kind(Trait),
           ?assertEqual([{m, {arrow, star, star}}], Kinds)
       end},

      {"Comparable trait parameter a has kind Type (not HKT)",
       fun() ->
           %% trait Comparable a where
           %%   equals : a -> a -> Bool
           Trait = {trait_decl, 'Comparable', [a], [],
               [{trait_sig, equals,
                   {type_fun, {type_var, a, loc()},
                       {type_fun, {type_var, a, loc()},
                           {type_con, 'Bool', loc()}, loc()},
                       loc()},
                   loc()}],
               loc()},
           {ok, Kinds} = catena_kind:check_trait_kind(Trait),
           ?assertEqual([{a, star}], Kinds)
       end},

      {"Trait with multiple type params gets correct kinds",
       fun() ->
           %% trait Bifunctor f where
           %%   bimap : (a -> c) -> (b -> d) -> f a b -> f c d
           Trait = {trait_decl, 'Bifunctor', [f], [],
               [{trait_sig, bimap,
                   {type_fun, {type_var, ignored, loc()},
                       {type_app, {type_var, f, loc()},
                           [{type_var, a, loc()}, {type_var, b, loc()}], loc()},
                       loc()},
                   loc()}],
               loc()},
           {ok, Kinds} = catena_kind:check_trait_kind(Trait),
           %% f is applied to two arguments, so f : Type -> Type (from first application)
           ?assertEqual([{f, {arrow, star, star}}], Kinds)
       end}
     ]}.

%%%===================================================================
%%% 1.5.3.2 - Kind Inference for Instance Declarations
%%%===================================================================

instance_kind_inference_test_() ->
    {"1.5.3.2 - Validate kind inference for instance declarations",
     [
      {"instance Mapper Maybe - Maybe has kind Type -> Type",
       fun() ->
           Env = kind_env(),
           %% Maybe : Type -> Type
           {ok, Kind} = catena_kind:infer_type_kind({type_con, 'Maybe', loc()}, Env),
           ?assertEqual({arrow, star, star}, Kind)
       end},

      {"instance Mapper List - List has kind Type -> Type",
       fun() ->
           Env = kind_env(),
           {ok, Kind} = catena_kind:infer_type_kind({type_con, 'List', loc()}, Env),
           ?assertEqual({arrow, star, star}, Kind)
       end},

      {"Mapper instance type matches expected kind",
       fun() ->
           Env = kind_env(),
           ExpectedKind = {arrow, star, star},  % Mapper f requires f : Type -> Type
           TypeArg = {type_con, 'Maybe', loc()},
           Result = catena_kind:check_instance_kind(TypeArg, ExpectedKind, Env),
           ?assertMatch({ok, {arrow, star, star}}, Result)
       end},

      {"Comparable instance for Int matches expected kind",
       fun() ->
           Env = kind_env(),
           ExpectedKind = star,  % Comparable a requires a : Type
           TypeArg = {type_con, 'Int', loc()},
           Result = catena_kind:check_instance_kind(TypeArg, ExpectedKind, Env),
           ?assertMatch({ok, star}, Result)
       end},

      {"Validate instance kind for Maybe Int (fully applied)",
       fun() ->
           Env = kind_env(),
           %% Maybe Int : Type (fully applied)
           TypeExpr = {type_app, {type_con, 'Maybe', loc()}, [{type_con, 'Int', loc()}], loc()},
           {ok, Kind} = catena_kind:infer_type_kind(TypeExpr, Env),
           ?assertEqual(star, Kind)
       end}
     ]}.

%%%===================================================================
%%% 1.5.3.3 - Partially Applied Type Constructors
%%%===================================================================

partial_application_test_() ->
    {"1.5.3.3 - Validate partially applied type constructors",
     [
      {"Either has kind Type -> Type -> Type",
       fun() ->
           Env = kind_env(),
           {ok, Kind} = catena_kind:infer_type_kind({type_con, 'Either', loc()}, Env),
           ?assertEqual({arrow, star, {arrow, star, star}}, Kind)
       end},

      {"Either String has kind Type -> Type (partial application)",
       fun() ->
           Env = kind_env(),
           %% Either String : Type -> Type
           TypeExpr = {type_app, {type_con, 'Either', loc()}, [{type_con, 'String', loc()}], loc()},
           {ok, Kind} = catena_kind:infer_type_kind(TypeExpr, Env),
           ?assertEqual({arrow, star, star}, Kind)
       end},

      {"Either String Int has kind Type (fully applied)",
       fun() ->
           Env = kind_env(),
           %% Either String Int : Type
           TypeExpr = {type_app, {type_con, 'Either', loc()},
               [{type_con, 'String', loc()}, {type_con, 'Int', loc()}], loc()},
           {ok, Kind} = catena_kind:infer_type_kind(TypeExpr, Env),
           ?assertEqual(star, Kind)
       end},

      {"instance Mapper (Either e) - partial application valid",
       fun() ->
           Env = kind_env(),
           ExpectedKind = {arrow, star, star},  % Mapper f requires f : Type -> Type
           %% Either e where e is a type variable
           TypeArg = {type_app, {type_con, 'Either', loc()}, [{type_var, e, loc()}], loc()},
           Result = catena_kind:check_instance_kind(TypeArg, ExpectedKind, Env),
           ?assertMatch({ok, {arrow, star, star}}, Result)
       end},

      {"Partial application preserves remaining arrows",
       fun() ->
           %% If we had a type F : Type -> Type -> Type -> Type
           %% Then F Int : Type -> Type -> Type
           %% And F Int Bool : Type -> Type
           %% And F Int Bool Char : Type
           Env0 = kind_env(),
           %% Add a 3-arity type constructor
           Kind3 = {arrow, star, {arrow, star, {arrow, star, star}}},
           Env = catena_kind:add_type_kind('Triple', Kind3, Env0),

           %% Triple : Type -> Type -> Type -> Type
           {ok, K0} = catena_kind:infer_type_kind({type_con, 'Triple', loc()}, Env),
           ?assertEqual(Kind3, K0),

           %% Triple Int : Type -> Type -> Type
           {ok, K1} = catena_kind:infer_type_kind(
               {type_app, {type_con, 'Triple', loc()}, [{type_con, 'Int', loc()}], loc()}, Env),
           ?assertEqual({arrow, star, {arrow, star, star}}, K1),

           %% Triple Int Bool : Type -> Type
           {ok, K2} = catena_kind:infer_type_kind(
               {type_app, {type_con, 'Triple', loc()},
                   [{type_con, 'Int', loc()}, {type_con, 'Bool', loc()}], loc()}, Env),
           ?assertEqual({arrow, star, star}, K2)
       end}
     ]}.

%%%===================================================================
%%% 1.5.3.4 - Kind Error Reporting
%%%===================================================================

kind_error_reporting_test_() ->
    {"1.5.3.4 - Report kind errors clearly when HKT constraints are violated",
     [
      {"Over-application of type constructor returns error",
       fun() ->
           Env = kind_env(),
           %% Maybe Int Int - Maybe only takes one argument
           TypeExpr = {type_app, {type_con, 'Maybe', loc()},
               [{type_con, 'Int', loc()}, {type_con, 'Bool', loc()}], loc()},
           Result = catena_kind:infer_type_kind(TypeExpr, Env),
           ?assertMatch({error, {over_applied, star, 1, _}}, Result)
       end},

      {"Kind mismatch detected for wrong instance type",
       fun() ->
           Env = kind_env(),
           ExpectedKind = {arrow, star, star},  % Mapper f requires f : Type -> Type
           TypeArg = {type_con, 'Int', loc()},  % Int : Type, not Type -> Type
           Result = catena_kind:check_instance_kind(TypeArg, ExpectedKind, Env),
           ?assertMatch({error, {kind_mismatch, {arrow, star, star}, star}}, Result)
       end},

      {"Kind mismatch: using Type where Type -> Type expected",
       fun() ->
           Env = kind_env(),
           ExpectedKind = {arrow, star, star},
           TypeArg = {type_con, 'Bool', loc()},  % Bool : Type
           Result = catena_kind:check_instance_kind(TypeArg, ExpectedKind, Env),
           ?assertMatch({error, {kind_mismatch, _, _}}, Result)
       end},

      {"Kind mismatch: using Type -> Type where Type expected",
       fun() ->
           Env = kind_env(),
           ExpectedKind = star,  % Comparable a requires a : Type
           TypeArg = {type_con, 'Maybe', loc()},  % Maybe : Type -> Type
           Result = catena_kind:check_instance_kind(TypeArg, ExpectedKind, Env),
           ?assertMatch({error, {kind_mismatch, star, {arrow, star, star}}}, Result)
       end},

      {"format_kind produces readable output",
       fun() ->
           ?assertEqual("Type", catena_kind:format_kind(star)),
           ?assertEqual("Type -> Type", catena_kind:format_kind({arrow, star, star})),
           ?assertEqual("Type -> Type -> Type",
               catena_kind:format_kind({arrow, star, {arrow, star, star}}))
       end}
     ]}.

%%%===================================================================
%%% HKT Validation Integration Tests
%%%===================================================================

hkt_validation_integration_test_() ->
    {"HKT validation integration tests",
     [
      {"validate_hkt passes for correct declarations",
       fun() ->
           Env = kind_env(),
           %% A trait and matching instance
           Declarations = [
               {trait_decl, 'MyMapper', [f], [],
                   [{trait_sig, mymap,
                       {type_fun, {type_var, a, loc()},
                           {type_app, {type_var, f, loc()}, [{type_var, a, loc()}], loc()},
                           loc()},
                       loc()}],
                   loc()},
               {instance_decl, 'MyMapper', [{type_con, 'Maybe', loc()}], [], [], loc()}
           ],
           Result = catena_kind:validate_hkt(Declarations, Env),
           ?assertMatch({ok, _}, Result)
       end},

      {"validate_hkt detects kind mismatch in instance",
       fun() ->
           Env = kind_env(),
           %% A trait expecting Type -> Type but instance uses Type
           Declarations = [
               {trait_decl, 'BadMapper', [f], [],
                   [{trait_sig, badmap,
                       {type_fun, {type_var, a, loc()},
                           {type_app, {type_var, f, loc()}, [{type_var, a, loc()}], loc()},
                           loc()},
                       loc()}],
                   loc()},
               %% Int : Type, but BadMapper expects Type -> Type
               {instance_decl, 'BadMapper', [{type_con, 'Int', loc()}], [], [], loc()}
           ],
           Result = catena_kind:validate_hkt(Declarations, Env),
           ?assertMatch({error, _}, Result)
       end},

      {"kinds_compatible correctly compares kinds",
       fun() ->
           ?assert(catena_kind:kinds_compatible(star, star)),
           ?assert(catena_kind:kinds_compatible({arrow, star, star}, {arrow, star, star})),
           ?assertNot(catena_kind:kinds_compatible(star, {arrow, star, star})),
           ?assertNot(catena_kind:kinds_compatible({arrow, star, star}, star))
       end}
     ]}.

%%%===================================================================
%%% Internal Type Representation Tests
%%%===================================================================

internal_type_kind_test_() ->
    {"Kind inference for internal type representations",
     [
      {"tcon infers correct kind",
       fun() ->
           Env = kind_env(),
           {ok, Kind} = catena_kind:infer_type_kind({tcon, 'Maybe'}, Env),
           ?assertEqual({arrow, star, star}, Kind)
       end},

      {"tapp with tcon infers correct kind",
       fun() ->
           Env = kind_env(),
           %% {tapp, {tcon, 'Maybe'}, [{tcon, int}]} : Type
           TypeExpr = {tapp, {tcon, 'Maybe'}, [{tcon, 'Int'}]},
           {ok, Kind} = catena_kind:infer_type_kind(TypeExpr, Env),
           ?assertEqual(star, Kind)
       end},

      {"Partial tapp infers correct kind",
       fun() ->
           Env = kind_env(),
           %% {tapp, {tcon, 'Either'}, [{tcon, 'String'}]} : Type -> Type
           TypeExpr = {tapp, {tcon, 'Either'}, [{tcon, 'String'}]},
           {ok, Kind} = catena_kind:infer_type_kind(TypeExpr, Env),
           ?assertEqual({arrow, star, star}, Kind)
       end}
     ]}.
