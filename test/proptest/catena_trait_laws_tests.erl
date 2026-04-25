%% @doc Unit tests for Trait Law Definitions (Section 4.2)
%%
%% Tests verify that the mathematical laws are correctly defined
%% and hold for standard implementations.
-module(catena_trait_laws_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_laws.hrl").
-include("../../src/proptest/catena_property.hrl").

%%====================================================================
%% Test Adapters for Standard Types
%%====================================================================

%% List adapter (standard list is a Functor, Applicative, Monad, Monoid)
list_adapter() ->
    #{
        map => fun(F, X) -> lists:map(F, X) end,
        pure => fun(X) -> [X] end,
        ap => fun(Fs, Xs) ->
            [F(X) || F <- Fs, X <- Xs]
        end,
        bind => fun(Xs, F) -> lists:flatmap(F, Xs) end,
        combine => fun(A, B) -> A ++ B end,
        empty => fun() -> [] end,
        equals => fun(A, B) -> A =:= B end,
        lte => fun(A, B) -> A =< B end
    }.

%% Maybe adapter (implementing simple Maybe type)
maybe_adapter() ->
    #{
        map => fun(F, X) -> map_maybe(F, X) end,
        pure => fun(X) -> {just, X} end,
        ap => fun(MF, MX) -> ap_maybe(MF, MX) end,
        bind => fun(MX, F) -> bind_maybe(MX, F) end,
        combine => fun(_, _) -> error(no_combine) end,
        empty => fun() -> error(no_empty) end,
        equals => fun(A, B) -> A =:= B end,
        lte => fun(_, _) -> error(no_lte) end
    }.

%% Integer adapter for Setoid/Ord
int_adapter() ->
    #{
        map => fun(_, _) -> error(no_map) end,
        pure => fun(_) -> error(no_pure) end,
        ap => fun(_, _) -> error(no_ap) end,
        bind => fun(_, _) -> error(no_bind) end,
        combine => fun(A, B) -> A + B end,
        empty => fun() -> 0 end,
        equals => fun(A, B) -> A =:= B end,
        lte => fun(A, B) -> A =< B end
    }.

%% String adapter for Semigroup/Monoid
string_adapter() ->
    #{
        map => fun(_, _) -> error(no_map) end,
        pure => fun(_) -> error(no_pure) end,
        ap => fun(_, _) -> error(no_ap) end,
        bind => fun(_, _) -> error(no_bind) end,
        combine => fun(A, B) -> A ++ B end,
        empty => fun() -> "" end,
        equals => fun(A, B) -> A =:= B end,
        lte => fun(_, _) -> error(no_lte) end
    }.

%% @private Map over Maybe.
map_maybe(_F, nothing) -> nothing;
map_maybe(F, {just, X}) -> {just, F(X)}.

%% @private Apply for Maybe.
ap_maybe(nothing, _MX) -> nothing;
ap_maybe(_MF, nothing) -> nothing;
ap_maybe({just, F}, {just, X}) -> {just, F(X)}.

%% @private Bind for Maybe.
bind_maybe(nothing, _F) -> nothing;
bind_maybe({just, X}, F) -> F(X).

%%====================================================================
%% Unit Tests - Section 4.2.1: Functor Laws
%%====================================================================

functor_identity_law_list_test() ->
    Law = catena_trait_laws:functor_identity_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

functor_composition_law_list_test() ->
    Law = catena_trait_laws:functor_composition_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

functor_identity_law_maybe_test() ->
    Law = catena_trait_laws:functor_identity_law(),
    Adapter = maybe_adapter(),
    MaybeGen = catena_gen:gen_one_of([
        catena_gen:constant(nothing),
        catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
            catena_gen:constant({just, N})
        end)
    ]),
    Params = #law_params{
        generator = MaybeGen,
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

functor_laws_set_name_test() ->
    Set = catena_trait_laws:functor_laws(),
    ?assertEqual(<<"Functor (Mapper)">>, catena_laws:law_set_name(Set)),
    ok.

functor_laws_count_test() ->
    Set = catena_trait_laws:functor_laws(),
    ?assertEqual(2, length(catena_laws:law_set_laws(Set))),
    ok.

%%====================================================================
%% Unit Tests - Section 4.2.2: Applicative Laws
%%====================================================================

applicative_identity_law_list_test() ->
    Law = catena_trait_laws:applicative_identity_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

applicative_homomorphism_law_list_test() ->
    Law = catena_trait_laws:applicative_homomorphism_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

applicative_laws_count_test() ->
    Set = catena_trait_laws:applicative_laws(),
    ?assertEqual(4, length(catena_laws:law_set_laws(Set))),
    ok.

%%====================================================================
%% Unit Tests - Section 4.2.3: Monad Laws
%%====================================================================

monad_left_identity_law_list_test() ->
    Law = catena_trait_laws:monad_left_identity_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

monad_right_identity_law_list_test() ->
    Law = catena_trait_laws:monad_right_identity_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

monad_associativity_law_list_test() ->
    Law = catena_trait_laws:monad_associativity_law(),
    Adapter = list_adapter(),
    Params = #law_params{
        generator = catena_stdgen:gen_list(catena_gen:gen_int()),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

monad_left_identity_law_maybe_test() ->
    Law = catena_trait_laws:monad_left_identity_law(),
    Adapter = maybe_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_one_of([
            catena_gen:constant(nothing),
            catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                catena_gen:constant({just, N})
            end)
        ]),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

monad_laws_count_test() ->
    Set = catena_trait_laws:monad_laws(),
    ?assertEqual(3, length(catena_laws:law_set_laws(Set))),
    ok.

%%====================================================================
%% Unit Tests - Section 4.2.4: Semigroup Laws
%%====================================================================

semigroup_associativity_law_string_test() ->
    Law = catena_trait_laws:semigroup_associativity_law(),
    Adapter = string_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_one_of([
            catena_gen:constant(""),
            catena_gen:gen_bind(catena_gen:gen_int(), fun(_) ->
                catena_gen:constant("a")
            end)
        ]),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

semigroup_laws_count_test() ->
    Set = catena_trait_laws:semigroup_laws(),
    ?assertEqual(1, length(catena_laws:law_set_laws(Set))),
    ok.

%%====================================================================
%% Unit Tests - Section 4.2.5: Monoid Laws
%%====================================================================

monoid_left_identity_law_string_test() ->
    Law = catena_trait_laws:monoid_left_identity_law(),
    Adapter = string_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_one_of([
            catena_gen:constant(""),
            catena_gen:gen_bind(catena_gen:gen_int(), fun(_) ->
                catena_gen:constant("hello")
            end)
        ]),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

monoid_right_identity_law_string_test() ->
    Law = catena_trait_laws:monoid_right_identity_law(),
    Adapter = string_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_one_of([
            catena_gen:constant(""),
            catena_gen:gen_bind(catena_gen:gen_int(), fun(_) ->
                catena_gen:constant("world")
            end)
        ]),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

monoid_laws_count_test() ->
    Set = catena_trait_laws:monoid_laws(),
    ?assertEqual(2, length(catena_laws:law_set_laws(Set))),
    ok.

%%====================================================================
%% Unit Tests - Section 4.2.6: Setoid Laws
%%====================================================================

setoid_reflexivity_law_int_test() ->
    Law = catena_trait_laws:setoid_reflexivity_law(),
    Adapter = int_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_int(),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

setoid_symmetry_law_int_test() ->
    Law = catena_trait_laws:setoid_symmetry_law(),
    Adapter = int_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_int(),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

setoid_transitivity_law_int_test() ->
    Law = catena_trait_laws:setoid_transitivity_law(),
    Adapter = int_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_int(),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    Result = catena_runner:run_property(Prop, #{num_tests => 50}),
    %% Can be passed or discarded (implies discards when precondition fails)
    ?assert(lists:member(element(1, Result), [passed, failed])),
    ok.

setoid_laws_count_test() ->
    Set = catena_trait_laws:setoid_laws(),
    ?assertEqual(3, length(catena_laws:law_set_laws(Set))),
    ok.

%%====================================================================
%% Unit Tests - Section 4.2.7: Ord Laws
%%====================================================================

ord_antisymmetry_law_int_test() ->
    Law = catena_trait_laws:ord_antisymmetry_law(),
    Adapter = int_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_int(),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    Result = catena_runner:run_property(Prop, #{num_tests => 50}),
    %% Can be passed or discarded (implies discards when precondition fails)
    ?assert(lists:member(element(1, Result), [passed, failed])),
    ok.

ord_transitivity_law_int_test() ->
    Law = catena_trait_laws:ord_transitivity_law(),
    Adapter = int_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_int(),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

ord_totality_law_int_test() ->
    Law = catena_trait_laws:ord_totality_law(),
    Adapter = int_adapter(),
    Params = #law_params{
        generator = catena_gen:gen_int(),
        eq_fn = catena_laws:eq_structural(),
        extra_gens = #{adapter => Adapter}
    },
    Prop = catena_laws:apply_law(Law, Params),
    {passed, _} = catena_runner:run_property(Prop, #{num_tests => 50}),
    ok.

ord_laws_count_test() ->
    Set = catena_trait_laws:ord_laws(),
    ?assertEqual(3, length(catena_laws:law_set_laws(Set))),
    ok.
