%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_handler_depth (Phase 9.1)
%%%
%%% Tests for handler depth semantics:
%%% - Depth type and constructors
%%% - Depth validation and conversion
%%% - Depth semantics documentation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_handler_depth_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Depth Type and Constructors Tests
%%%=============================================================================

depth_returns_deep_test() ->
    ?assertEqual(deep, catena_handler_depth:depth()).

deep_constructor_test() ->
    ?assertEqual(deep, catena_handler_depth:deep()).

shallow_constructor_test() ->
    ?assertEqual(shallow, catena_handler_depth:shallow()).

is_deep_true_test() ->
    ?assert(catena_handler_depth:is_deep(deep)).

is_deep_false_test() ->
    ?assertNot(catena_handler_depth:is_deep(shallow)).

is_shallow_true_test() ->
    ?assert(catena_handler_depth:is_shallow(shallow)).

is_shallow_false_test() ->
    ?assertNot(catena_handler_depth:is_shallow(deep)).

%%%=============================================================================
%%% Depth Validation Tests
%%%=============================================================================

validate_deep_test() ->
    ?assertEqual({ok, deep}, catena_handler_depth:validate_depth(deep)).

validate_shallow_test() ->
    ?assertEqual({ok, shallow}, catena_handler_depth:validate_depth(shallow)).

validate_invalid_test() ->
    ?assertMatch({error, {invalid_depth, _}}, catena_handler_depth:validate_depth(other)).

validate_atom_test() ->
    ?assertMatch({error, {invalid_depth, other}}, catena_handler_depth:validate_depth(other)).

validate_number_test() ->
    ?assertMatch({error, {invalid_depth, _}}, catena_handler_depth:validate_depth(42)).

normalize_default_test() ->
    ?assertEqual({ok, deep}, catena_handler_depth:normalize_depth(default)).

normalize_undefined_test() ->
    ?assertEqual({ok, deep}, catena_handler_depth:normalize_depth(undefined)).

normalize_map_test() ->
    ?assertEqual({ok, shallow}, catena_handler_depth:normalize_depth(#{depth => shallow})).

normalize_proplist_test() ->
    ?assertEqual({ok, deep}, catena_handler_depth:normalize_depth([{depth, deep}])).

%%%=============================================================================
%%% Depth Conversion Tests
%%%=============================================================================

to_deep_from_deep_test() ->
    ?assertEqual(deep, catena_handler_depth:to_deep(deep)).

to_deep_from_shallow_test() ->
    ?assertEqual(deep, catena_handler_depth:to_deep(shallow)).

to_shallow_from_shallow_test() ->
    ?assertEqual(shallow, catena_handler_depth:to_shallow(shallow)).

to_shallow_from_deep_test() ->
    ?assertEqual(shallow, catena_handler_depth:to_shallow(deep)).

invert_deep_test() ->
    ?assertEqual(shallow, catena_handler_depth:invert_depth(deep)).

invert_shallow_test() ->
    ?assertEqual(deep, catena_handler_depth:invert_depth(shallow)).

double_invert_test() ->
    Deep = deep,
    ?assertEqual(Deep, catena_handler_depth:invert_depth(catena_handler_depth:invert_depth(Deep))).

%%%=============================================================================
%% Depth Semantics Documentation Tests
%%%=============================================================================

depth_description_deep_test() ->
    Desc = catena_handler_depth:depth_description(deep),
    ?assert(is_list(Desc)),
    ?assert(string:length(Desc) > 0),
    ?assert(string:str(Desc, "nested") > 0).

depth_description_shallow_test() ->
    Desc = catena_handler_depth:depth_description(shallow),
    ?assert(is_list(Desc)),
    ?assert(string:length(Desc) > 0),
    ?assert(string:str(Desc, "direct") > 0).

scope_behavior_deep_test() ->
    Behavior = catena_handler_depth:scope_behavior(deep),
    ?assert(is_list(Behavior)),
    ?assert(string:str(Behavior, "nesting") > 0).

scope_behavior_shallow_test() ->
    Behavior = catena_handler_depth:scope_behavior(shallow),
    ?assert(is_list(Behavior)),
    ?assert(string:str(Behavior, "directly") > 0).

performance_implications_deep_test() ->
    Perf = catena_handler_depth:performance_implications(deep),
    ?assert(is_list(Perf)),
    ?assert(string:str(Perf, "O(n)") > 0).

performance_implications_shallow_test() ->
    Perf = catena_handler_depth:performance_implications(shallow),
    ?assert(is_list(Perf)),
    ?assert(string:str(Perf, "O(1)") > 0).

use_cases_deep_test() ->
    Cases = catena_handler_depth:use_cases(deep),
    ?assert(is_list(Cases)),
    ?assert(length(Cases) > 0),
    ?assert(lists:any(fun(Case) ->
        string:str(Case, "State") > 0 orelse
        string:str(Case, "Error") > 0
    end, Cases)).

use_cases_shallow_test() ->
    Cases = catena_handler_depth:use_cases(shallow),
    ?assert(is_list(Cases)),
    ?assert(length(Cases) > 0),
    ?assert(lists:any(fun(Case) ->
        string:str(Case, "scoped") > 0 orelse
        string:str(Case, "boundaries") > 0
    end, Cases)).

%%%=============================================================================
%%% Round-trip Conversion Tests
%%%=============================================================================

round_trip_deep_test() ->
    Deep = deep,
    Shallow = catena_handler_depth:to_shallow(Deep),
    ?assertEqual(Deep, catena_handler_depth:to_deep(Shallow)).

round_trip_shallow_test() ->
    Shallow = shallow,
    Deep = catena_handler_depth:to_deep(Shallow),
    ?assertEqual(Shallow, catena_handler_depth:to_shallow(Deep)).

round_trip_invert_test() ->
    Deep = deep,
    ?assertEqual(Deep,
        catena_handler_depth:invert_depth(catena_handler_depth:invert_depth(Deep))).

%%%=============================================================================
%%% Property-style Tests
%%%=============================================================================

to_deep_preserves_semantics_test() ->
    % to_deep should always return deep regardless of input
    ?assertEqual(deep, catena_handler_depth:to_deep(deep)),
    ?assertEqual(deep, catena_handler_depth:to_deep(shallow)).

to_shallow_preserves_semantics_test() ->
    % to_shallow should always return shallow regardless of input
    ?assertEqual(shallow, catena_handler_depth:to_shallow(deep)),
    ?assertEqual(shallow, catena_handler_depth:to_shallow(shallow)).

invert_is_involutive_test() ->
    % invert is its own inverse
    Depths = [deep, shallow],
    lists:foreach(fun(D) ->
        ?assertEqual(D, catena_handler_depth:invert_depth(catena_handler_depth:invert_depth(D)))
    end, Depths).

validate_accepts_valid_depths_test() ->
    ValidDepths = [deep, shallow],
    lists:foreach(fun(D) ->
        ?assertEqual({ok, D}, catena_handler_depth:validate_depth(D))
    end, ValidDepths).

handles_nested_deep_test() ->
    ?assert(catena_handler_depth:handles_nested(deep)).

handles_nested_shallow_test() ->
    ?assertNot(catena_handler_depth:handles_nested(shallow)).

handles_at_depth_deep_nested_test() ->
    ?assert(catena_handler_depth:handles_at_depth(deep, 1, 3)).

handles_at_depth_deep_same_scope_test() ->
    ?assert(catena_handler_depth:handles_at_depth(deep, 2, 2)).

handles_at_depth_shallow_same_scope_test() ->
    ?assert(catena_handler_depth:handles_at_depth(shallow, 2, 2)).

handles_at_depth_shallow_nested_test() ->
    ?assertNot(catena_handler_depth:handles_at_depth(shallow, 1, 2)).

lookup_strategy_deep_test() ->
    ?assertEqual(traverse_nested_scopes, catena_handler_depth:lookup_strategy(deep)).

lookup_strategy_shallow_test() ->
    ?assertEqual(current_scope_only, catena_handler_depth:lookup_strategy(shallow)).
