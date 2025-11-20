%%%-------------------------------------------------------------------
%%% @doc Tests for Trait Hierarchy Checking (Task 1.2.6.1)
%%%
%%% Tests the validation of trait hierarchies including cycle detection,
%%% supertrait resolution, and topological sorting.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_trait_hierarchy_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Helper to create a trait definition
trait_def(Name, Extends) ->
    {Name, Extends, {location, 1, 1}}.

%% Simple valid hierarchy: A <- B <- C (linear chain)
simple_hierarchy() ->
    #{
        'A' => trait_def('A', []),
        'B' => trait_def('B', ['A']),
        'C' => trait_def('C', ['B'])
    }.

%% Diamond hierarchy: D <- B,C <- A (multiple inheritance)
diamond_hierarchy() ->
    #{
        'A' => trait_def('A', []),
        'B' => trait_def('B', ['A']),
        'C' => trait_def('C', ['A']),
        'D' => trait_def('D', ['B', 'C'])
    }.

%% Hierarchy with cycle: A <- B <- C <- A
cycle_hierarchy() ->
    #{
        'A' => trait_def('A', ['C']),
        'B' => trait_def('B', ['A']),
        'C' => trait_def('C', ['B'])
    }.

%% Self-extending trait
self_extends_hierarchy() ->
    #{
        'A' => trait_def('A', ['A'])
    }.

%% Unknown supertrait
unknown_super_hierarchy() ->
    #{
        'A' => trait_def('A', ['Unknown'])
    }.

%%====================================================================
%% Basic Validation Tests
%%====================================================================

check_hierarchy_test_() ->
    [
        ?_test(test_valid_simple_hierarchy()),
        ?_test(test_valid_diamond_hierarchy()),
        ?_test(test_empty_hierarchy()),
        ?_test(test_single_trait_no_extends())
    ].

test_valid_simple_hierarchy() ->
    Result = catena_trait_hierarchy:check_hierarchy(simple_hierarchy()),
    ?assertEqual({ok, valid}, Result).

test_valid_diamond_hierarchy() ->
    Result = catena_trait_hierarchy:check_hierarchy(diamond_hierarchy()),
    ?assertEqual({ok, valid}, Result).

test_empty_hierarchy() ->
    Result = catena_trait_hierarchy:check_hierarchy(#{}),
    ?assertEqual({ok, valid}, Result).

test_single_trait_no_extends() ->
    Defs = #{'Foo' => trait_def('Foo', [])},
    Result = catena_trait_hierarchy:check_hierarchy(Defs),
    ?assertEqual({ok, valid}, Result).

%%====================================================================
%% Cycle Detection Tests
%%====================================================================

cycle_detection_test_() ->
    [
        ?_test(test_detect_direct_cycle()),
        ?_test(test_detect_indirect_cycle()),
        ?_test(test_detect_self_extends()),
        ?_test(test_multiple_cycles())
    ].

test_detect_direct_cycle() ->
    %% A extends B, B extends A
    Defs = #{
        'A' => trait_def('A', ['B']),
        'B' => trait_def('B', ['A'])
    },
    Result = catena_trait_hierarchy:check_hierarchy(Defs),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    ?assert(length(Errors) >= 1),
    %% Should have cycle_detected error
    HasCycle = lists:any(
        fun({cycle_detected, _}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasCycle).

test_detect_indirect_cycle() ->
    Result = catena_trait_hierarchy:check_hierarchy(cycle_hierarchy()),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    HasCycle = lists:any(
        fun({cycle_detected, _}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasCycle).

test_detect_self_extends() ->
    Result = catena_trait_hierarchy:check_hierarchy(self_extends_hierarchy()),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    HasSelfExtends = lists:any(
        fun({self_extends, 'A'}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasSelfExtends).

test_multiple_cycles() ->
    %% Two separate cycles: A<->B and C<->D
    Defs = #{
        'A' => trait_def('A', ['B']),
        'B' => trait_def('B', ['A']),
        'C' => trait_def('C', ['D']),
        'D' => trait_def('D', ['C'])
    },
    Result = catena_trait_hierarchy:check_hierarchy(Defs),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    %% Should detect at least one cycle
    ?assert(length(Errors) >= 1).

%%====================================================================
%% Unknown Supertrait Tests
%%====================================================================

unknown_supertrait_test_() ->
    [
        ?_test(test_detect_unknown_supertrait()),
        ?_test(test_builtin_traits_are_valid())
    ].

test_detect_unknown_supertrait() ->
    Result = catena_trait_hierarchy:check_hierarchy(unknown_super_hierarchy()),
    ?assertMatch({error, _}, Result),
    {error, Errors} = Result,
    HasUnknown = lists:any(
        fun({unknown_supertrait, 'A', 'Unknown'}) -> true; (_) -> false end,
        Errors
    ),
    ?assert(HasUnknown).

test_builtin_traits_are_valid() ->
    %% Should allow extending built-in traits without defining them
    Defs = #{
        'MyOrd' => trait_def('MyOrd', ['Eq', 'Ord']),
        'MyFunctor' => trait_def('MyFunctor', ['Functor'])
    },
    Result = catena_trait_hierarchy:check_hierarchy(Defs),
    ?assertEqual({ok, valid}, Result).

%%====================================================================
%% Supertrait Query Tests
%%====================================================================

get_extends_test_() ->
    [
        ?_test(test_get_immediate_extends()),
        ?_test(test_get_extends_not_found()),
        ?_test(test_get_extends_empty())
    ].

test_get_immediate_extends() ->
    Defs = diamond_hierarchy(),
    ?assertEqual({ok, ['B', 'C']}, catena_trait_hierarchy:get_extends('D', Defs)),
    ?assertEqual({ok, ['A']}, catena_trait_hierarchy:get_extends('B', Defs)),
    ?assertEqual({ok, []}, catena_trait_hierarchy:get_extends('A', Defs)).

test_get_extends_not_found() ->
    Defs = simple_hierarchy(),
    ?assertEqual({error, not_found}, catena_trait_hierarchy:get_extends('Z', Defs)).

test_get_extends_empty() ->
    Defs = #{'Solo' => trait_def('Solo', [])},
    ?assertEqual({ok, []}, catena_trait_hierarchy:get_extends('Solo', Defs)).

%%====================================================================
%% All Supertraits Tests
%%====================================================================

get_all_supertraits_test_() ->
    [
        ?_test(test_get_all_supertraits_simple()),
        ?_test(test_get_all_supertraits_diamond()),
        ?_test(test_get_all_supertraits_cycle_error())
    ].

test_get_all_supertraits_simple() ->
    Defs = simple_hierarchy(),
    %% C extends B extends A
    {ok, Supers} = catena_trait_hierarchy:get_all_supertraits('C', Defs),
    %% Should include B and A
    ?assert(lists:member('B', Supers)),
    ?assert(lists:member('A', Supers)),
    %% B before A (immediate first)
    BIdx = find_index('B', Supers),
    AIdx = find_index('A', Supers),
    ?assert(BIdx < AIdx).

test_get_all_supertraits_diamond() ->
    Defs = diamond_hierarchy(),
    %% D extends B,C which both extend A
    {ok, Supers} = catena_trait_hierarchy:get_all_supertraits('D', Defs),
    %% Should include B, C, and A
    ?assert(lists:member('B', Supers)),
    ?assert(lists:member('C', Supers)),
    ?assert(lists:member('A', Supers)).

test_get_all_supertraits_cycle_error() ->
    Defs = cycle_hierarchy(),
    Result = catena_trait_hierarchy:get_all_supertraits('A', Defs),
    ?assertEqual({error, cycle}, Result).

%%====================================================================
%% Topological Sort Tests
%%====================================================================

topological_sort_test_() ->
    [
        ?_test(test_topo_sort_simple()),
        ?_test(test_topo_sort_diamond()),
        ?_test(test_topo_sort_empty()),
        ?_test(test_topo_sort_cycle_error())
    ].

test_topo_sort_simple() ->
    Defs = simple_hierarchy(),
    {ok, Sorted} = catena_trait_hierarchy:topological_sort(Defs),
    %% A should come before B, B before C
    AIdx = find_index('A', Sorted),
    BIdx = find_index('B', Sorted),
    CIdx = find_index('C', Sorted),
    ?assert(AIdx < BIdx),
    ?assert(BIdx < CIdx).

test_topo_sort_diamond() ->
    Defs = diamond_hierarchy(),
    {ok, Sorted} = catena_trait_hierarchy:topological_sort(Defs),
    %% A should come before B and C, both before D
    AIdx = find_index('A', Sorted),
    BIdx = find_index('B', Sorted),
    CIdx = find_index('C', Sorted),
    DIdx = find_index('D', Sorted),
    ?assert(AIdx < BIdx),
    ?assert(AIdx < CIdx),
    ?assert(BIdx < DIdx),
    ?assert(CIdx < DIdx).

test_topo_sort_empty() ->
    {ok, Sorted} = catena_trait_hierarchy:topological_sort(#{}),
    ?assertEqual([], Sorted).

test_topo_sort_cycle_error() ->
    Defs = cycle_hierarchy(),
    Result = catena_trait_hierarchy:topological_sort(Defs),
    ?assertMatch({error, {cycle_detected, _}}, Result).

%%====================================================================
%% Error Formatting Tests
%%====================================================================

error_formatting_test_() ->
    [
        ?_test(test_format_cycle_error()),
        ?_test(test_format_unknown_supertrait_error()),
        ?_test(test_format_self_extends_error())
    ].

test_format_cycle_error() ->
    Error = {cycle_detected, ['A', 'B', 'C', 'A']},
    Msg = catena_trait_hierarchy:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "A") =/= nomatch),
    ?assert(string:find(Msg, "B") =/= nomatch),
    ?assert(string:find(Msg, "C") =/= nomatch),
    ?assert(string:find(Msg, "Circular") =/= nomatch orelse
            string:find(Msg, "cycle") =/= nomatch).

test_format_unknown_supertrait_error() ->
    Error = {unknown_supertrait, 'MyTrait', 'UnknownTrait'},
    Msg = catena_trait_hierarchy:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "MyTrait") =/= nomatch),
    ?assert(string:find(Msg, "UnknownTrait") =/= nomatch).

test_format_self_extends_error() ->
    Error = {self_extends, 'BadTrait'},
    Msg = catena_trait_hierarchy:format_error(Error),
    ?assert(is_list(Msg)),
    ?assert(string:find(Msg, "BadTrait") =/= nomatch),
    ?assert(string:find(Msg, "itself") =/= nomatch).

%%====================================================================
%% Helper Functions
%%====================================================================

find_index(Elem, List) ->
    find_index(Elem, List, 1).

find_index(_Elem, [], _N) ->
    not_found;
find_index(Elem, [Elem | _], N) ->
    N;
find_index(Elem, [_ | Rest], N) ->
    find_index(Elem, Rest, N + 1).
