%%%-------------------------------------------------------------------
%%% @doc Tests for Trait Method Type Inference (Task 1.2.6.3)
%%%
%%% Tests validation of trait method signatures and instance method
%%% implementations.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_methods_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Simple trait with one method
simple_trait() ->
    {'Eq', [a], [], [{equals, {fun_type, {tcon, a}, {fun_type, {tcon, a}, {tcon, bool}, {effect_set, []}}, {effect_set, []}}}], loc}.

%% Functor-like trait with higher-kinded type
functor_trait() ->
    {'Functor', [f], [], [
        {fmap, {fun_type,
            {fun_type, {tcon, a}, {tcon, b}, {effect_set, []}},
            {fun_type, {tapp, {tcon, f}, [{tcon, a}]}, {tapp, {tcon, f}, [{tcon, b}]}, {effect_set, []}},
            {effect_set, []}}}
    ], loc}.

%% Trait with multiple methods
multi_method_trait() ->
    {'Show', [a], [], [
        {show, {fun_type, {tcon, a}, {tcon, string}, {effect_set, []}}},
        {show_list, {fun_type, {tapp, {tcon, list}, [{tcon, a}]}, {tcon, string}, {effect_set, []}}}
    ], loc}.

%% Valid instance for Eq Int
valid_eq_int_instance() ->
    {instance, 'Eq', [{tcon, int}], [
        {equals, 2, {fun_type, {tcon, int}, {fun_type, {tcon, int}, {tcon, bool}, {effect_set, []}}, {effect_set, []}}}
    ], loc}.

%% Valid instance for Functor List
valid_functor_list_instance() ->
    {instance, 'Functor', [{tcon, list}], [
        {fmap, 2, {fun_type,
            {fun_type, {tcon, a}, {tcon, b}, {effect_set, []}},
            {fun_type, {tapp, {tcon, list}, [{tcon, a}]}, {tapp, {tcon, list}, [{tcon, b}]}, {effect_set, []}},
            {effect_set, []}}}
    ], loc}.

%% Instance with missing method
missing_method_instance() ->
    {instance, 'Show', [{tcon, int}], [
        {show, 1, {fun_type, {tcon, int}, {tcon, string}, {effect_set, []}}}
        %% Missing show_list
    ], loc}.

%% Instance with extra method
extra_method_instance() ->
    {instance, 'Eq', [{tcon, int}], [
        {equals, 2, {fun_type, {tcon, int}, {fun_type, {tcon, int}, {tcon, bool}, {effect_set, []}}, {effect_set, []}}},
        {not_in_trait, 1, {fun_type, {tcon, int}, {tcon, bool}, {effect_set, []}}}
    ], loc}.

%% Instance with wrong arity
wrong_arity_instance() ->
    {instance, 'Eq', [{tcon, int}], [
        %% equals should take 2 args (arity 2), but we give it arity 1
        {equals, 1, {fun_type, {tcon, int}, {tcon, bool}, {effect_set, []}}}
    ], loc}.

%%====================================================================
%% Trait Method Validation Tests
%%====================================================================

check_trait_methods_test_() ->
    [
        ?_test(test_valid_trait_methods()),
        ?_test(test_valid_multi_method_trait())
    ].

test_valid_trait_methods() ->
    Result = catena_trait_methods:check_trait_methods(simple_trait()),
    ?assertEqual(ok, Result).

test_valid_multi_method_trait() ->
    Result = catena_trait_methods:check_trait_methods(multi_method_trait()),
    ?assertEqual(ok, Result).

%%====================================================================
%% Instance Method Validation Tests
%%====================================================================

check_instance_methods_test_() ->
    [
        ?_test(test_valid_instance_methods()),
        ?_test(test_missing_method_detection()),
        ?_test(test_extra_method_detection()),
        ?_test(test_arity_mismatch_detection())
    ].

test_valid_instance_methods() ->
    Instance = valid_eq_int_instance(),
    Trait = simple_trait(),
    Result = catena_trait_methods:check_instance_methods(Instance, Trait, #{}),
    ?assertEqual(ok, Result).

test_missing_method_detection() ->
    Instance = missing_method_instance(),
    Trait = multi_method_trait(),
    Result = catena_trait_methods:check_instance_methods(Instance, Trait, #{}),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    HasMissing = lists:any(
        fun({missing_method, 'Show', show_list}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasMissing).

test_extra_method_detection() ->
    Instance = extra_method_instance(),
    Trait = simple_trait(),
    Result = catena_trait_methods:check_instance_methods(Instance, Trait, #{}),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    HasExtra = lists:any(
        fun({extra_method, 'Eq', not_in_trait}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasExtra).

test_arity_mismatch_detection() ->
    Instance = wrong_arity_instance(),
    Trait = simple_trait(),
    Result = catena_trait_methods:check_instance_methods(Instance, Trait, #{}),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    HasArityMismatch = lists:any(
        fun({arity_mismatch, 'Eq', equals, _, _}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasArityMismatch).

%%====================================================================
%% Method Signature Substitution Tests
%%====================================================================

substitute_method_type_test_() ->
    [
        ?_test(test_substitute_simple_type()),
        ?_test(test_substitute_functor_type())
    ].

test_substitute_simple_type() ->
    %% a -> Bool becomes Int -> Bool with [a -> Int]
    MethodType = {fun_type, {tcon, a}, {tcon, bool}, {effect_set, []}},
    Result = catena_trait_methods:substitute_method_type(MethodType, [a], [{tcon, int}]),
    Expected = {fun_type, {tcon, int}, {tcon, bool}, {effect_set, []}},
    ?assertEqual(Expected, Result).

test_substitute_functor_type() ->
    %% f a -> f b becomes List a -> List b with [f -> List]
    MethodType = {fun_type, {tapp, {tcon, f}, [{tcon, a}]}, {tapp, {tcon, f}, [{tcon, b}]}, {effect_set, []}},
    Result = catena_trait_methods:substitute_method_type(MethodType, [f], [{tcon, list}]),
    Expected = {fun_type, {tapp, {tcon, list}, [{tcon, a}]}, {tapp, {tcon, list}, [{tcon, b}]}, {effect_set, []}},
    ?assertEqual(Expected, Result).

%%====================================================================
%% Get Method Signature Tests
%%====================================================================

get_method_signature_test_() ->
    [
        ?_test(test_get_existing_method()),
        ?_test(test_get_nonexistent_method())
    ].

test_get_existing_method() ->
    Methods = [
        {foo, {tcon, int}},
        {bar, {tcon, bool}}
    ],
    Result = catena_trait_methods:get_method_signature(foo, Methods),
    ?assertEqual({ok, {tcon, int}}, Result).

test_get_nonexistent_method() ->
    Methods = [{foo, {tcon, int}}],
    Result = catena_trait_methods:get_method_signature(missing, Methods),
    ?assertEqual({error, not_found}, Result).

%%====================================================================
%% Error Formatting Tests
%%====================================================================

error_formatting_test_() ->
    [
        ?_test(test_format_missing_method_error()),
        ?_test(test_format_extra_method_error()),
        ?_test(test_format_arity_mismatch_error()),
        ?_test(test_format_type_mismatch_error())
    ].

test_format_missing_method_error() ->
    Error = {missing_method, 'Functor', fmap},
    Msg = catena_trait_methods:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "Functor") =/= nomatch),
    ?assert(string:find(Msg, "fmap") =/= nomatch),
    ?assert(string:find(Msg, "Missing") =/= nomatch).

test_format_extra_method_error() ->
    Error = {extra_method, 'Eq', unknown_method},
    Msg = catena_trait_methods:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "Eq") =/= nomatch),
    ?assert(string:find(Msg, "unknown_method") =/= nomatch),
    ?assert(string:find(Msg, "Extra") =/= nomatch).

test_format_arity_mismatch_error() ->
    Error = {arity_mismatch, 'Show', show, 1, 2},
    Msg = catena_trait_methods:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "Show") =/= nomatch),
    ?assert(string:find(Msg, "show") =/= nomatch),
    ?assert(string:find(Msg, "1") =/= nomatch),
    ?assert(string:find(Msg, "2") =/= nomatch).

test_format_type_mismatch_error() ->
    Expected = {tcon, int},
    Actual = {tcon, bool},
    Error = {type_mismatch, 'Eq', equals, Expected, Actual},
    Msg = catena_trait_methods:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "Eq") =/= nomatch),
    ?assert(string:find(Msg, "equals") =/= nomatch),
    %% Should contain type representations
    ?assert(length(Msg) > 50).
