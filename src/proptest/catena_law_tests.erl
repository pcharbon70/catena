%% @doc Law Testing Framework - Law Test Generation (Section 4.4)
%%
%% This module implements function-based law test generation.
%% Since Catena's macro system is not yet implemented, this provides
%% a function-based alternative to the `derive_law_tests` macro.
%%
%% == Function-Based API ==
%%
%% Instead of `@derive_law_tests for: [functor, monad]`, use:
%%
%% ```
%% catena_law_tests:test_suite(my_type, [
%%   {generator, gen_my_type()},
%%   {traits, [functor, monad]}
%% ])
%% ```
%%
%% @see catena_discipline for discipline definitions
%% @see catena_trait_laws for trait law definitions
-module(catena_law_tests).

-include("catena_laws.hrl").
-include("catena_property.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API Exports
%%====================================================================

-export([
    test_suite/2,
    test_suite/1,
    run_law_tests/2,
    law_tests_for/2,
    eunit_test_generator/1
]).

%%====================================================================
%% Types
%%====================================================================

-type test_config() :: #{
    generator => catena_gen:generator(_),
    traits => [catena_discipline:trait()],
    adapter => catena_discipline:adapter(),
    eq_fn => catena_laws:eq_fn(),
    options => map()
}.

-type test_result() :: #{
    total := non_neg_integer(),
    passed := non_neg_integer(),
    failed := non_neg_integer(),
    results := [{binary(), {passed | failed, catena_property:property_result()}}]
}.

%%====================================================================
%% Section 4.4.1: Test Suite Generation
%%====================================================================

%% @doc Generate a test suite for a type with given configuration.
%%
%% Takes a type name (as atom or binary) and a configuration map
%% with generator and traits. Returns a list of test functions
%% suitable for EUnit.
%%
%% == Example ==
%%
%% ```
%% %% Define your test module
%% %% test/my_type_law_tests.erl
%% -module(my_type_law_tests).
%% -include_lib("eunit/include/eunit.hrl").
%%
%% generator() -> gen_my_type().
%%
%% adapter() -> #{
%%     map => fun my_type:map/2,
%%     pure => fun my_type:pure/1,
%%     ...
%% }.
%%
%% %% Generate tests
%% catena_law_tests_test_() ->
%%     catena_law_tests:eunit_test_generator(#{
%%         module => ?MODULE,
%%         generator => fun generator/0,
%%         adapter => adapter(),
%%         traits => [functor, monad]
%%     }).
%% ```
-spec test_suite(atom() | binary(), test_config()) -> [{binary(), fun(() -> any())}].
test_suite(TypeName, Config) when is_atom(TypeName) ->
    test_suite(atom_to_binary(TypeName), Config);
test_suite(TypeName, Config) when is_binary(TypeName) ->
    Generator0 = maps:get(generator, Config),
    Adapter = maps:get(adapter, Config),
    Traits = maps:get(traits, Config),
    EqFn = maps:get(eq_fn, Config, catena_laws:eq_structural()),
    Options = maps:get(options, Config, #{}),

    %% Get the discipline for all specified traits
    Discipline = catena_discipline:all_laws_for(Traits),

    %% Call the generator to get the actual generator
    Generator = case Generator0 of
        Fun when is_function(Fun, 0) -> Fun();
        _ -> Generator0
    end,

    %% Create params with the correct generator
    ExtraGens = #{
        adapter => Adapter,
        fun_gen => maps:get(fun_gen, Options, catena_discipline:default_fun_gen()),
        kleisli_gen => maps:get(kleisli_gen, Options, catena_discipline:default_kleisli_gen())
    },

    Params = #law_params{
        generator = Generator,
        eq_fn = EqFn,
        extra_gens = ExtraGens
    },

    %% Get the laws from the discipline and apply them with correct params
    Laws = Discipline#discipline.laws,

    lists:map(fun(Law) ->
        LawName = catena_laws:law_name(Law),
        TestName = <<TypeName/binary, "_", LawName/binary>>,
        TestFun = fun() ->
            %% Apply the law with our params (which has the correct generator)
            Property = catena_laws:apply_law(Law, Params),
            NumTests = maps:get(num_tests, Options, 100),
            case catena_runner:run_property(Property, #{num_tests => NumTests}) of
                {passed, _} -> ok;
                {failed, Result} ->
                    erlang:error({law_violation, TestName, Result})
            end
        end,
        {TestName, TestFun}
    end, Laws).

%% @doc Generate a test suite with minimal configuration.
%%
%% Uses default options and infers adapter from module if possible.
%%
-spec test_suite(test_config()) -> [{binary(), fun(() -> any())}].
test_suite(Config) ->
    Module = maps:get(module, Config, ?MODULE),
    test_suite(Module, Config).

%%====================================================================
%% Section 4.4.2: Test Execution
%%====================================================================

%% @doc Run law tests for a type with given configuration.
%%
%% Returns a summary map with counts and detailed results.
%%
-spec run_law_tests(atom() | binary(), test_config()) -> test_result().
run_law_tests(TypeName, Config) ->
    Tests = test_suite(TypeName, Config),
    Results = lists:map(fun({Name, TestFun}) ->
        case catch TestFun() of
            ok -> {Name, {passed, #property_result{kind = success}}};
            {law_violation, Name, Result} -> {Name, {failed, Result}};
            {'EXIT', _} -> {Name, {failed, #property_result{kind = error}}};
            _ -> {Name, {failed, #property_result{kind = error}}}
        end
    end, Tests),

    Passed = length([R || {_, {passed, _}} = R <- Results]),
    Failed = length(Results) - Passed,

    #{
        total => length(Results),
        passed => Passed,
        failed => Failed,
        results => Results
    }.

%% @doc Create law tests for use with EUnit test generator.
%%
%% Returns a list of test generators that EUnit will execute.
%%
-spec eunit_test_generator(test_config()) -> [{binary(), fun(() -> any())}].
eunit_test_generator(Config) ->
    Module = maps:get(module, Config),
    Adapter = maps:get(adapter, Config),
    Traits = maps:get(traits, Config),

    FullName = case Module of
        ?MODULE -> <<"law_tests">>;
        _ -> list_to_binary(atom_to_list(Module) ++ "_laws")
    end,

    FullConfig = Config#{
        adapter => Adapter,
        traits => Traits
    },

    test_suite(FullName, FullConfig).

%% @doc Alias for test_suite with a simpler name.
%%
-spec law_tests_for(atom() | binary(), test_config()) -> [{binary(), fun(() -> any())}].
law_tests_for(TypeName, Config) ->
    test_suite(TypeName, Config).

%%====================================================================
%% Unit Tests - Section 4.4
%%====================================================================

%%--------------------------------------------------------------------
%% Section 4.4.1: Test Suite Generation Tests
%%--------------------------------------------------------------------

test_suite_returns_tests_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        pure => fun(X) -> [X] end,
        ap => fun(Fs, Xs) -> [F(X) || F <- Fs, X <- Xs] end,
        bind => fun(Xs, F) -> lists:flatmap(F, Xs) end,
        combine => fun(A, B) -> A ++ B end,
        empty => fun() -> [] end,
        equals => fun(A, B) -> A =:= B end,
        lte => fun(A, B) -> A =< B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor]
    },
    Tests = test_suite(<<"test_type">>, Config),
    ?assert(length(Tests) >= 2),
    ok.

test_suite_with_atom_name_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        pure => fun(X) -> [X] end,
        equals => fun(A, B) -> A =:= B end,
        lte => fun(A, B) -> A =< B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor]
    },
    Tests = test_suite(test_type, Config),
    ?assert(length(Tests) >= 2),
    ok.

%%--------------------------------------------------------------------
%% Section 4.4.2: Test Execution Tests
%%--------------------------------------------------------------------

run_law_tests_passing_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        pure => fun(X) -> [X] end,
        equals => fun(A, B) -> A =:= B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor],
        options => #{num_tests => 10}
    },
    Result = run_law_tests(<<"list">>, Config),
    ?assertEqual(0, maps:get(failed, Result)),
    ?assert(maps:get(passed, Result) >= 2),
    ok.

run_law_tests_returns_summary_test() ->
    Adapter = #{
        map => fun(F, X) -> lists:map(F, X) end,
        pure => fun(X) -> [X] end,
        equals => fun(A, B) -> A =:= B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => Adapter,
        traits => [functor],
        options => #{num_tests => 5}
    },
    Result = run_law_tests(<<"test">>, Config),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(total, Result)),
    ?assert(maps:is_key(passed, Result)),
    ?assert(maps:is_key(failed, Result)),
    ?assert(maps:is_key(results, Result)),
    ok.

%%--------------------------------------------------------------------
%% Section 4.4.3: Error Reporting Tests
%%--------------------------------------------------------------------

law_violation_reported_test() ->
    %% Create a broken adapter that violates functor identity
    BadAdapter = #{
        map => fun(_F, X) -> [X ++ [1]] end,  %% Always adds 1 - violates identity!
        pure => fun(X) -> [X] end,
        equals => fun(A, B) -> A =:= B end
    },
    Config = #{
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => BadAdapter,
        traits => [functor],
        options => #{num_tests => 5}
    },
    Result = run_law_tests(<<"bad_list">>, Config),
    %% Should have at least one failure
    ?assert(maps:get(failed, Result) > 0),
    ok.
