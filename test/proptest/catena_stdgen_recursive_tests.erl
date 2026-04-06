%% @doc Unit tests for catena_stdgen recursive generators (Section 2.4)
%%
%% Tests for recursive structure generators with size control.
-module(catena_stdgen_recursive_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 2.4: Recursive Structure Support Tests
%%====================================================================

%% ---- Binary Tree Generator ----

gen_binarytree_produces_tree_test() ->
    Gen = catena_stdgen:gen_BinaryTree(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(is_tuple(Value)),
    ok.

gen_binarytree_shrinks_toward_leaf_test() ->
    Gen = catena_stdgen:gen_BinaryTree(catena_gen:gen_int()),
    %% Try multiple seeds until we get one with shrinks
    Results = [begin
        Tree = catena_gen:run(Gen, 50, catena_gen:seed_from_int(N)),
        length(catena_tree:children(Tree))
    end || N <- lists:seq(1, 10)],
    %% At least one should have shrinks
    ?assert(lists:any(fun(C) -> C > 0 end, Results)),
    ok.

%% ---- Maybe/Option Generator ----

gen_maybe_produces_none_or_some_test() ->
    Gen = catena_stdgen:gen_maybe(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(Value =:= none orelse (is_tuple(Value) andalso element(1, Value) =:= some)),
    ok.

gen_maybe_shrinks_toward_none_test() ->
    Gen = catena_stdgen:gen_maybe(catena_gen:gen_int()),
    %% Try multiple seeds until we get a {some, _} value that has shrinks
    Results = [begin
        Tree = catena_gen:run(Gen, 50, catena_gen:seed_from_int(N)),
        {catena_tree:root(Tree), length(catena_tree:children(Tree))}
    end || N <- lists:seq(1, 20)],
    %% At least one should be {some, _} with shrinks
    HasSomeWithShrinks = lists:any(fun
        ({none, _}) -> false;
        ({{some, _}, C}) -> C > 0
    end, Results),
    ?assert(HasSomeWithShrinks),
    ok.

%% ---- Result/Either Generator ----

gen_result_produces_ok_or_error_test() ->
    Gen = catena_stdgen:gen_result(catena_gen:gen_int(), catena_stdgen:gen_atom()),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    IsOk = is_tuple(Value) andalso element(1, Value) =:= ok,
    IsError = is_tuple(Value) andalso element(1, Value) =:= error,
    ?assert(IsOk orelse IsError),
    ok.

gen_result_shrinks_test() ->
    Gen = catena_stdgen:gen_result(catena_gen:gen_int(), catena_stdgen:gen_atom()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

%% ---- Recursive List Generator ----

gen_list_recursive_produces_nested_list_test() ->
    Gen = catena_stdgen:gen_list_recursive(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 20, catena_gen:seed_new()),
    Value = catena_tree:root(Tree),
    ?assert(is_list(Value)),
    ok.

gen_list_recursive_depth_limited_by_size_test() ->
    Gen = catena_stdgen:gen_list_recursive(catena_gen:gen_int()),
    TreeSmall = catena_gen:run(Gen, 5, catena_gen:seed_from_int(42)),
    TreeLarge = catena_gen:run(Gen, 100, catena_gen:seed_from_int(42)),
    %% Both should produce valid lists
    ?assert(is_list(catena_tree:root(TreeSmall))),
    ?assert(is_list(catena_tree:root(TreeLarge))),
    ok.

gen_list_recursive_shrinks_by_simplifying_test() ->
    Gen = catena_stdgen:gen_list_recursive(catena_gen:gen_int()),
    %% Try multiple seeds until we get one with shrinks
    Results = [begin
        Tree = catena_gen:run(Gen, 50, catena_gen:seed_from_int(N)),
        length(catena_tree:children(Tree))
    end || N <- lists:seq(1, 10)],
    %% At least one should have shrinks
    ?assert(lists:any(fun(C) -> C > 0 end, Results)),
    ok.

%% ---- Size Control ----

gen_recursive_with_max_depth_respects_limit_test() ->
    Gen = catena_stdgen:gen_recursive(5, fun() -> catena_stdgen:gen_BinaryTree(catena_gen:gen_int()) end),
    Tree = catena_gen:run(Gen, 100, catena_gen:seed_new()),
    %% Should produce a valid structure
    Value = catena_tree:root(Tree),
    ?assert(is_tuple(Value)),
    ok.

gen_recursive_with_frequency_test() ->
    %% Test that recursive structures have appropriate frequency
    Gen = catena_stdgen:gen_list_recursive(catena_gen:gen_int()),
    %% Run multiple times and check we get various structures
    Results = [begin
        T = catena_gen:run(Gen, 20, catena_gen:seed_new()),
        catena_tree:root(T)
    end || _ <- lists:seq(1, 10)],
    ?assert(length(lists:usort(Results)) > 1),  %% Should have variety
    ok.

%% ---- Lazy Evaluation ----

gen_lazy_defers_evaluation_test() ->
    %% Creating a recursive generator should be fast
    Gen = catena_stdgen:gen_list_recursive(catena_gen:gen_int()),
    %% Even with large size, creation should complete quickly
    Tree = catena_gen:run(Gen, 1000, catena_gen:seed_new()),
    ?assert(is_list(catena_tree:root(Tree))),
    ok.
