%% @doc Unit tests for catena_stdgen function generators (Section 2.5)
%%
%% Tests for function generators with deterministic behavior.
-module(catena_stdgen_function_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 2.5: Function Generators Tests
%%====================================================================

%% ---- Simple Function Generators ----

gen_function_arity0_produces_function_test() ->
    Gen = catena_stdgen:gen_function0(catena_gen:gen_int()),
    Fun = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Fun),
    ?assert(is_function(Result, 0)),
    ok.

gen_function_arity0_is_deterministic_test() ->
    Gen = catena_stdgen:gen_function0(catena_gen:gen_int()),
    Fun = catena_gen:run(Gen, 10, catena_gen:seed_from_int(42)),
    Result = catena_tree:root(Fun),
    %% Calling multiple times should give same result
    ?assertEqual(Result(), Result()),
    ok.

gen_function_arity1_produces_function_test() ->
    Gen = catena_stdgen:gen_function1(catena_gen:gen_int()),
    Fun = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Fun),
    ?assert(is_function(Result, 1)),
    ok.

gen_function_arity1_is_deterministic_test() ->
    Gen = catena_stdgen:gen_function1(catena_gen:gen_int()),
    Fun = catena_gen:run(Gen, 10, catena_gen:seed_from_int(42)),
    Result = catena_tree:root(Fun),
    %% Calling with same input should give same result
    ?assertEqual(Result(5), Result(5)),
    ok.

gen_function_arity2_produces_function_test() ->
    Gen = catena_stdgen:gen_function2(catena_gen:gen_int()),
    Fun = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Fun),
    ?assert(is_function(Result, 2)),
    ok.

%% ---- Constant Function ----

gen_constant_function_returns_constant_test() ->
    Fun = catena_stdgen:gen_constant_function(42),
    ?assertEqual(42, Fun()),
    ok.

gen_constant_function_any_arity_test() ->
    Fun = catena_stdgen:gen_constant_function(constant_value),
    ?assertEqual(constant_value, Fun()),
    ok.

%% ---- Identity Function ----

gen_identity_function_test() ->
    Fun = catena_stdgen:gen_identity_function(),
    ?assertEqual(42, Fun(42)),
    ?assertEqual("hello", Fun("hello")),
    ?assertEqual([1,2,3], Fun([1,2,3])),
    ok.

%% ---- Projection Functions ----

gen_projection_first_test() ->
    Fun = catena_stdgen:gen_projection_first(),
    ?assertEqual(1, Fun({1, 2})),
    ?assertEqual(a, Fun({a, b})),
    ok.

gen_projection_second_test() ->
    Fun = catena_stdgen:gen_projection_second(),
    ?assertEqual(2, Fun({1, 2})),
    ?assertEqual(b, Fun({a, b})),
    ok.

%% ---- Function Shrinking ----

gen_function0_shrinks_test() ->
    Gen = catena_stdgen:gen_function0(catena_gen:gen_int()),
    Tree = catena_gen:run(Gen, 50, catena_gen:seed_new()),
    %% Should have shrinks
    Children = catena_tree:children(Tree),
    ?assert(length(Children) > 0),
    ok.

%% ---- Function Application ----

gen_function_apply_works_test() ->
    Gen = catena_stdgen:gen_function1(catena_gen:gen_int()),
    Fun = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Result = catena_tree:root(Fun),
    %% Should be able to apply the function
    Output = Result(10),
    ?assert(is_integer(Output)),
    ok.

%% ---- K-Combinator ----

gen_k_combinator_test() ->
    %% K-combinator: const x y = x
    Fun = catena_stdgen:gen_k_combinator(42),
    ?assertEqual(42, Fun(ignored)),
    ok.
