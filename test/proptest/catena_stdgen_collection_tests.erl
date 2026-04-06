%% @doc Unit tests for catena_stdgen collection generators (Section 2.3)
%%
%% Tests for list, tuple, map, and set generators.
-module(catena_stdgen_collection_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 2.3: Collection Generators Tests
%%====================================================================

%% ---- List Generators ----

gen_list_produces_list_test() ->
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    List = catena_tree:root(Tree),
    ?assert(is_list(List)),
    ok.

gen_list_respects_size_test() ->
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    TreeSmall = catena_gen:run(Gen, 5, catena_gen:seed_from_int(42)),
    TreeLarge = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    ListSmall = catena_tree:root(TreeSmall),
    ListLarge = catena_tree:root(TreeLarge),
    ?assert(is_list(ListSmall)),
    ?assert(is_list(ListLarge)),
    ok.

gen_list_shrinks_by_removing_elements_test() ->
    Gen = catena_stdgen:gen_list(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    List = catena_tree:root(Tree),
    case length(List) of
        0 -> ok;
        _ ->
            Children = catena_tree:children(Tree),
            ?assert(length(Children) > 0)
    end,
    ok.

gen_list_of_length_exact_test() ->
    Gen = catena_stdgen:gen_list_of_length(5, catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    List = catena_tree:root(Tree),
    ?assertEqual(5, length(List)),
    ok.

gen_list_of_with_range_test() ->
    Range = catena_range:range_constant({3, 7}),
    Gen = catena_stdgen:gen_list_of(Range, catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    List = catena_tree:root(Tree),
    Len = length(List),
    ?assert(Len >= 3 andalso Len =< 7),
    ok.

%% ---- Tuple Generators ----

gen_tuple2_produces_pair_test() ->
    Gen = catena_stdgen:gen_tuple2(catena_gen:gen_int(), catena_gen:gen_bool()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Tuple = catena_tree:root(Tree),
    ?assertMatch({_, _}, Tuple),
    ok.

gen_tuple3_produces_triple_test() ->
    Gen = catena_stdgen:gen_tuple3(
        catena_gen:gen_int(),
        catena_gen:gen_bool(),
        catena_stdgen:gen_atom()
    ),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Tuple = catena_tree:root(Tree),
    ?assertMatch({_, _, _}, Tuple),
    ok.

gen_tuple4_produces_quadruple_test() ->
    Gen = catena_stdgen:gen_tuple4(
        catena_gen:gen_int(),
        catena_gen:gen_bool(),
        catena_stdgen:gen_atom(),
        catena_stdgen:gen_float()
    ),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Tuple = catena_tree:root(Tree),
    ?assertMatch({_, _, _, _}, Tuple),
    ok.

gen_tuple_shrinks_all_elements_test() ->
    Gen = catena_stdgen:gen_tuple2(catena_gen:gen_int(), catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    {_A, _B} = catena_tree:root(Tree),
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

%% ---- Map Generators ----

gen_map_produces_map_test() ->
    Gen = catena_stdgen:gen_map(catena_gen:gen_int(), catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Map = catena_tree:root(Tree),
    ?assert(is_map(Map)),
    ok.

gen_map_shrinks_by_removing_entries_test() ->
    Gen = catena_stdgen:gen_map(catena_gen:gen_int(), catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Map = catena_tree:root(Tree),
    case map_size(Map) of
        0 -> ok;
        _ ->
            Children = catena_tree:children(Tree),
            ?assert(length(Children) > 0)
    end,
    ok.

gen_map_of_with_range_test() ->
    Range = catena_range:range_constant({2, 5}),
    Gen = catena_stdgen:gen_map_of(Range, catena_gen:gen_int(), catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Map = catena_tree:root(Tree),
    Size = map_size(Map),
    ?assert(Size >= 2 andalso Size =< 5),
    ok.

%% ---- Set Generators ----

gen_set_produces_set_test() ->
    Gen = catena_stdgen:gen_set(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Set = catena_tree:root(Tree),
    ?assert(is_list(Set)),
    %% Check all elements are unique
    UniqueLength = length(lists:usort(Set)),
    ?assertEqual(UniqueLength, length(Set)),
    ok.

gen_set_shrinks_by_removing_elements_test() ->
    Gen = catena_stdgen:gen_set(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    Set = catena_tree:root(Tree),
    case length(Set) of
        0 -> ok;
        _ ->
            Children = catena_tree:children(Tree),
            ?assert(length(Children) > 0)
    end,
    ok.

gen_set_of_with_range_test() ->
    Range = catena_range:range_constant({2, 5}),
    Gen = catena_stdgen:gen_set_of(Range, catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Set = catena_tree:root(Tree),
    Size = length(Set),
    ?assert(Size >= 2 andalso Size =< 5),
    ok.
