-module(catena_continuation_kind_tests).
-include_lib("eunit/include/eunit.hrl").

%%%---------------------------------------------------------------------
%%% Test Suite Configuration
%%%---------------------------------------------------------------------

catena_continuation_kind_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"type constructors", fun test_type_constructors/0},
        {"type predicates", fun test_type_predicates/0},
        {"current kind tracking", fun test_current_kind_tracking/0},
        {"type validation", fun test_type_validation/0},
        {"conversion", fun test_conversion/0},
        {"semantics", fun test_semantics/0},
        {"use cases", fun test_use_cases/0},
        {"algebraic laws", fun test_algebraic_laws/0}
     ]}.

%%%---------------------------------------------------------------------
%%% Setup and Cleanup
%%%---------------------------------------------------------------------

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%---------------------------------------------------------------------
%%% Type Constructors Tests
%%%---------------------------------------------------------------------

test_type_constructors() ->
    OneShot = catena_continuation_kind:one_shot(),
    MultiShot = catena_continuation_kind:multi_shot(),
    ?assertEqual(one_shot, OneShot),
    ?assertEqual(multi_shot, MultiShot).

%%%---------------------------------------------------------------------
%%% Type Predicates Tests
%%%---------------------------------------------------------------------

test_type_predicates() ->
    OneShot = catena_continuation_kind:one_shot(),
    MultiShot = catena_continuation_kind:multi_shot(),
    ?assert(catena_continuation_kind:is_one_shot(OneShot)),
    ?assertNot(catena_continuation_kind:is_one_shot(MultiShot)),
    ?assert(catena_continuation_kind:is_multi_shot(MultiShot)),
    ?assertNot(catena_continuation_kind:is_multi_shot(OneShot)).

test_current_kind_tracking() ->
    ?assertEqual(multi_shot, catena_continuation_kind:default_kind()),
    ?assertEqual(multi_shot, catena_continuation_kind:current_kind()),
    ?assert(catena_continuation_kind:is_multi_shot()),
    ?assertNot(catena_continuation_kind:is_one_shot()),

    Result = catena_continuation_kind:with_kind(one_shot, fun() ->
        ?assertEqual(one_shot, catena_continuation_kind:current_kind()),
        ?assert(catena_continuation_kind:is_one_shot()),
        ?assertNot(catena_continuation_kind:is_multi_shot()),
        ?assertEqual(multi_shot, catena_continuation_kind:compose(one_shot, multi_shot)),
        ok
    end),

    ?assertEqual(ok, Result),
    ?assertEqual(multi_shot, catena_continuation_kind:current_kind()).

%%%---------------------------------------------------------------------
%%% Type Validation Tests
%%%---------------------------------------------------------------------

test_type_validation() ->
    ?assert(catena_continuation_kind:is_valid_kind(one_shot)),
    ?assert(catena_continuation_kind:is_valid_kind(multi_shot)),
    ?assertNot(catena_continuation_kind:is_valid_kind(undefined)),
    ?assertNot(catena_continuation_kind:is_valid_kind(other)),
    ?assertNot(catena_continuation_kind:is_valid_kind("one_shot")),
    ?assertNot(catena_continuation_kind:is_valid_kind(<<"one_shot">>)).

%%%---------------------------------------------------------------------
%%% Conversion Tests
%%%---------------------------------------------------------------------

test_conversion() ->
    OneShot = catena_continuation_kind:one_shot(),
    MultiShot = catena_continuation_kind:multi_shot(),

    %% to_atom
    ?assertEqual(one_shot, catena_continuation_kind:to_atom(OneShot)),
    ?assertEqual(multi_shot, catena_continuation_kind:to_atom(MultiShot)),

    %% from_atom
    ?assertEqual({ok, one_shot}, catena_continuation_kind:from_atom(one_shot)),
    ?assertEqual({ok, multi_shot}, catena_continuation_kind:from_atom(multi_shot)),
    ?assertEqual({error, {invalid_kind, other}}, catena_continuation_kind:from_atom(other)).

%%%---------------------------------------------------------------------
%%% Semantics Tests
%%%---------------------------------------------------------------------

test_semantics() ->
    OneShotSemantics = catena_continuation_kind:semantics(one_shot),
    MultiShotSemantics = catena_continuation_kind:semantics(multi_shot),

    %% One-shot semantics
    ?assertEqual(linear, maps:get(resumption, OneShotSemantics)),
    ?assertEqual(single, maps:get(consumption, OneShotSemantics)),
    ?assertEqual(original, maps:get(state, OneShotSemantics)),
    ?assertEqual(immediate, maps:get(cleanup, OneShotSemantics)),
    ?assertEqual(exactly_once, maps:get(guarantee, OneShotSemantics)),

    %% Multi-shot semantics
    ?assertEqual(branching, maps:get(resumption, MultiShotSemantics)),
    ?assertEqual(none, maps:get(consumption, MultiShotSemantics)),
    ?assertEqual(copied, maps:get(state, MultiShotSemantics)),
    ?assertEqual(reference_counted, maps:get(cleanup, MultiShotSemantics)),
    ?assertEqual(zero_or_more, maps:get(guarantee, MultiShotSemantics)).

%%%---------------------------------------------------------------------
%%% Use Cases Tests
%%%---------------------------------------------------------------------

test_use_cases() ->
    OneShotUseCases = catena_continuation_kind:use_cases(one_shot),
    MultiShotUseCases = catena_continuation_kind:use_cases(multi_shot),

    %% One-shot use cases
    ?assert(is_list(OneShotUseCases)),
    ?assert(length(OneShotUseCases) > 0),
    ?assert(lists:any(fun(U) -> binary:match(U, <<"Resource">>) =/= nomatch end, OneShotUseCases)),

    %% Multi-shot use cases
    ?assert(is_list(MultiShotUseCases)),
    ?assert(length(MultiShotUseCases) > 0),
    ?assert(lists:any(fun(U) -> binary:match(U, <<"Backtracking">>) =/= nomatch end, MultiShotUseCases)).

%%%---------------------------------------------------------------------
%%% Algebraic Laws Tests
%%%---------------------------------------------------------------------

test_algebraic_laws() ->
    %% Associativity
    ?assert(catena_continuation_kind:associative(one_shot, one_shot)),
    ?assert(catena_continuation_kind:associative(multi_shot, one_shot)),
    ?assert(catena_continuation_kind:associative(one_shot, multi_shot)),
    ?assert(catena_continuation_kind:associative(multi_shot, multi_shot)),

    %% Commutativity
    ?assert(catena_continuation_kind:commutative(one_shot, one_shot)),
    ?assert(catena_continuation_kind:commutative(multi_shot, multi_shot)),
    ?assertNot(catena_continuation_kind:commutative(one_shot, multi_shot)),

    %% Identity
    ?assert(catena_continuation_kind:identity(one_shot, one_shot)),
    ?assertNot(catena_continuation_kind:identity(multi_shot, multi_shot)),

    %% Idempotence
    ?assert(catena_continuation_kind:idempotent(multi_shot)),
    ?assertNot(catena_continuation_kind:idempotent(one_shot)).
