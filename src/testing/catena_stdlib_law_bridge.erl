%% @doc Bridge from promoted stdlib law vocabulary into the internal
%% discipline-based property-testing framework.
-module(catena_stdlib_law_bridge).

-include("../proptest/catena_laws.hrl").
-include("../proptest/catena_property.hrl").

-export([
    known_instances/0,
    supported_traits/1,
    normalize_instance/1,
    normalize_trait/1,
    normalize_traits/1,
    run_trait_suite/2,
    run_trait_suite/3
]).

-export_type([
    instance_id/0,
    trait_id/0,
    bridge_result/0,
    bridge_law_result/0
]).

-type instance_id() :: 'maybe' | 'either' | 'list' | 'int'.
-type trait_id() :: functor | applicative | monad | semigroup | monoid | setoid | ord.

-type bridge_law_result() :: #{
    instance := instance_id(),
    instance_label := binary(),
    trait := trait_id(),
    trait_label := binary(),
    law := binary(),
    stdlib_law := atom(),
    test_name := binary(),
    status := passed | failed,
    property_result := catena_property:property_result()
}.

-type bridge_result() :: #{
    instance := instance_id(),
    instance_label := binary(),
    requested_traits := [trait_id()],
    traits := [trait_id()],
    total := non_neg_integer(),
    passed := non_neg_integer(),
    failed := non_neg_integer(),
    results := [bridge_law_result()]
}.

%%====================================================================
%% Public API
%%====================================================================

-spec known_instances() -> [instance_id()].
known_instances() ->
    ['maybe', 'either', 'list', 'int'].

-spec supported_traits(atom() | binary() | string()) ->
    {ok, [trait_id()]} | {error, term()}.
supported_traits(InstanceValue) ->
    case normalize_instance(InstanceValue) of
        {ok, InstanceId} ->
            {ok, maps:get(traits, instance_spec(InstanceId))};
        {error, _} = Error ->
            Error
    end.

-spec normalize_instance(atom() | binary() | string()) ->
    {ok, instance_id()} | {error, term()}.
normalize_instance(Value) ->
    case normalize_text(Value) of
        <<"maybe">> -> {ok, 'maybe'};
        <<"either">> -> {ok, 'either'};
        <<"list">> -> {ok, 'list'};
        <<"int">> -> {ok, 'int'};
        <<"integer">> -> {ok, 'int'};
        <<"intordered">> -> {ok, 'int'};
        <<"orderedint">> -> {ok, 'int'};
        <<"intorder">> -> {ok, 'int'};
        Other -> {error, {unsupported_instance, Other}}
    end.

-spec normalize_trait(atom() | binary() | string()) ->
    {ok, trait_id()} | {error, term()}.
normalize_trait(Value) ->
    case normalize_text(Value) of
        <<"functor">> -> {ok, functor};
        <<"mapper">> -> {ok, functor};
        <<"applicative">> -> {ok, applicative};
        <<"applicator">> -> {ok, applicative};
        <<"monad">> -> {ok, monad};
        <<"pipeline">> -> {ok, monad};
        <<"semigroup">> -> {ok, semigroup};
        <<"combiner">> -> {ok, semigroup};
        <<"monoid">> -> {ok, monoid};
        <<"accumulator">> -> {ok, monoid};
        <<"setoid">> -> {ok, setoid};
        <<"comparable">> -> {ok, setoid};
        <<"ord">> -> {ok, ord};
        <<"orderable">> -> {ok, ord};
        Other -> {error, {unsupported_trait, Other}}
    end.

-spec normalize_traits(term()) -> {ok, [trait_id()]} | {error, term()}.
normalize_traits(Traits) when is_list(Traits) ->
    normalize_trait_list(Traits, []);
normalize_traits(Trait) ->
    normalize_traits([Trait]).

-spec run_trait_suite(term(), term()) -> bridge_result() | {error, term()}.
run_trait_suite(InstanceValue, TraitsValue) ->
    run_trait_suite(InstanceValue, TraitsValue, #{}).

-spec run_trait_suite(term(), term(), map()) -> bridge_result() | {error, term()}.
run_trait_suite(InstanceValue, TraitsValue, Opts) ->
    case normalize_instance(InstanceValue) of
        {ok, InstanceId} ->
            case normalize_traits(TraitsValue) of
                {ok, RequestedTraits} ->
                    run_known_suite(InstanceId, RequestedTraits, Opts);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Known Instance Execution
%%====================================================================

run_known_suite(InstanceId, RequestedTraits, Opts) ->
    Spec = instance_spec(InstanceId),
    Supported = maps:get(traits, Spec),
    Unsupported = [Trait || Trait <- RequestedTraits, not lists:member(Trait, Supported)],
    case Unsupported of
        [] ->
            Generator = generator_value(maps:get(generator, Spec)),
            Adapter = maps:get(adapter, Spec),
            ExpandedTraits = expand_traits(RequestedTraits),
            ExtraGens = maybe_add_kleisli_generator(
                Adapter,
                #{
                    adapter => Adapter,
                    fun_gen => maps:get(fun_gen, Opts, catena_discipline:default_fun_gen())
                },
                Opts
            ),
            Params = #law_params{
                generator = Generator,
                eq_fn = maps:get(equals, Adapter, catena_laws:eq_structural()),
                extra_gens = ExtraGens
            },
            Results = [
                run_law(InstanceId, maps:get(label, Spec), LawMeta, Params, Opts)
             || LawMeta <- laws_for_traits(ExpandedTraits)
            ],
            summarize_suite(InstanceId, maps:get(label, Spec), RequestedTraits, ExpandedTraits, Results);
        _ ->
            {error, {unsupported_traits, InstanceId, Unsupported}}
    end.

generator_value(GenFun) when is_function(GenFun, 0) ->
    GenFun();
generator_value(Generator) ->
    Generator.

default_kleisli_generator(Adapter) ->
    Pure = maps:get(pure, Adapter),
    catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
        catena_gen:constant(fun(X) -> Pure(X + N) end)
    end).

maybe_add_kleisli_generator(Adapter, ExtraGens, Opts) ->
    case maps:is_key(pure, Adapter) of
        true ->
            KleisliGen = case maps:get(kleisli_gen, Opts, undefined) of
                undefined -> default_kleisli_generator(Adapter);
                Value -> Value
            end,
            maps:put(
                kleisli_gen,
                KleisliGen,
                ExtraGens
            );
        false ->
            ExtraGens
    end.

run_law(InstanceId, InstanceLabel, LawMeta, Params, Opts) ->
    Trait = maps:get(trait, LawMeta),
    Law = maps:get(law_record, LawMeta),
    LawName = catena_laws:law_name(Law),
    StdlibLaw = maps:get(stdlib_law, LawMeta),
    TestName = build_test_name(InstanceLabel, Trait, LawName),
    PropertyResult = try
        Property = catena_laws:apply_law(Law, Params),
        case catena_runner:run_property(Property, runner_opts(Opts)) of
            {passed, Result} -> Result;
            {failed, Result} -> Result
        end
    catch
        Class:Reason:Stacktrace ->
            error_property_result(runner_seed(Opts), {Class, Reason, Stacktrace})
    end,
    #{
        instance => InstanceId,
        instance_label => InstanceLabel,
        trait => Trait,
        trait_label => trait_label(Trait),
        law => LawName,
        stdlib_law => StdlibLaw,
        test_name => TestName,
        status => result_status(PropertyResult),
        property_result => PropertyResult
    }.

runner_opts(Opts) ->
    Base = #{
        num_tests => maps:get(num_tests, Opts, 100),
        max_shrinks => maps:get(max_shrinks, Opts, 1000)
    },
    with_optional_opt(timeout, Opts,
        with_optional_opt(parallel, Opts,
            with_seed_opt(Opts, Base)
        )
    ).

with_optional_opt(Key, Opts, Acc) ->
    case maps:get(Key, Opts, undefined) of
        undefined -> Acc;
        Value -> maps:put(Key, Value, Acc)
    end.

with_seed_opt(Opts, Acc) ->
    case runner_seed(Opts) of
        undefined -> Acc;
        Seed -> maps:put(seed, Seed, Acc)
    end.

runner_seed(Opts) ->
    case maps:get(seed, Opts, undefined) of
        undefined ->
            undefined;
        Seed when is_integer(Seed) ->
            catena_gen:seed_from_int(Seed);
        Seed ->
            Seed
    end.

result_status(#property_result{kind = success}) ->
    passed;
result_status(_) ->
    failed.

error_property_result(Seed, Error) ->
    #property_result{
        kind = error,
        tests_run = 0,
        tests_discarded = 0,
        shrinks_attempted = 0,
        seed = Seed,
        original_counterexample = Error,
        shrunk_counterexample = Error,
        shrink_history = [],
        labels = [],
        output = <<>>
    }.

summarize_suite(InstanceId, InstanceLabel, RequestedTraits, ExpandedTraits, Results) ->
    Passed = length([R || #{status := passed} = R <- Results]),
    Failed = length(Results) - Passed,
    #{
        instance => InstanceId,
        instance_label => InstanceLabel,
        requested_traits => RequestedTraits,
        traits => ExpandedTraits,
        total => length(Results),
        passed => Passed,
        failed => Failed,
        results => Results
    }.

%%====================================================================
%% Law Metadata
%%====================================================================

expand_traits(Traits) ->
    ordered_unique(lists:append([trait_closure(Trait) || Trait <- Traits])).

trait_closure(functor) -> [functor];
trait_closure(applicative) -> [functor, applicative];
trait_closure(monad) -> [functor, applicative, monad];
trait_closure(semigroup) -> [semigroup];
trait_closure(monoid) -> [semigroup, monoid];
trait_closure(setoid) -> [setoid];
trait_closure(ord) -> [setoid, ord].

laws_for_traits(Traits) ->
    lists:append([laws_for_trait(Trait) || Trait <- Traits]).

laws_for_trait(functor) ->
    [
        law_meta(functor, mapperIdentityLaw, catena_trait_laws:functor_identity_law()),
        law_meta(functor, mapperCompositionLaw, catena_trait_laws:functor_composition_law())
    ];
laws_for_trait(applicative) ->
    [
        law_meta(applicative, applicativeIdentityLaw, catena_trait_laws:applicative_identity_law()),
        law_meta(applicative, applicativeCompositionLaw, catena_trait_laws:applicative_composition_law()),
        law_meta(applicative, applicativeHomomorphismLaw, catena_trait_laws:applicative_homomorphism_law()),
        law_meta(applicative, applicativeInterchangeLaw, catena_trait_laws:applicative_interchange_law())
    ];
laws_for_trait(monad) ->
    [
        law_meta(monad, pipelineLeftIdentityLaw, catena_trait_laws:monad_left_identity_law()),
        law_meta(monad, pipelineRightIdentityLaw, catena_trait_laws:monad_right_identity_law()),
        law_meta(monad, pipelineAssociativityLaw, catena_trait_laws:monad_associativity_law())
    ];
laws_for_trait(semigroup) ->
    [
        law_meta(semigroup, combinerAssociativityLaw, catena_trait_laws:semigroup_associativity_law())
    ];
laws_for_trait(monoid) ->
    [
        law_meta(monoid, accumulatorLeftIdentityLaw, catena_trait_laws:monoid_left_identity_law()),
        law_meta(monoid, accumulatorRightIdentityLaw, catena_trait_laws:monoid_right_identity_law())
    ];
laws_for_trait(setoid) ->
    [
        law_meta(setoid, comparableReflexivityLaw, catena_trait_laws:setoid_reflexivity_law()),
        law_meta(setoid, comparableSymmetryLaw, catena_trait_laws:setoid_symmetry_law()),
        law_meta(setoid, comparableTransitivityLaw, catena_trait_laws:setoid_transitivity_law())
    ];
laws_for_trait(ord) ->
    [
        law_meta(ord, orderableAntisymmetryLaw, catena_trait_laws:ord_antisymmetry_law()),
        law_meta(ord, orderableTransitivityLaw, catena_trait_laws:ord_transitivity_law()),
        law_meta(ord, orderableTotalityLaw, catena_trait_laws:ord_totality_law())
    ].

law_meta(Trait, StdlibLaw, LawRecord) ->
    #{
        trait => Trait,
        stdlib_law => StdlibLaw,
        law_record => LawRecord
    }.

build_test_name(InstanceLabel, Trait, LawName) ->
    TraitLabel = lower_binary(trait_label(Trait)),
    <<(lower_binary(InstanceLabel))/binary, ".", TraitLabel/binary, ".", LawName/binary>>.

trait_label(functor) -> <<"Mapper">>;
trait_label(applicative) -> <<"Applicative">>;
trait_label(monad) -> <<"Pipeline">>;
trait_label(semigroup) -> <<"Combiner">>;
trait_label(monoid) -> <<"Accumulator">>;
trait_label(setoid) -> <<"Comparable">>;
trait_label(ord) -> <<"Orderable">>.

%%====================================================================
%% Known Specs
%%====================================================================

instance_spec('maybe') ->
    #{
        label => <<"Maybe">>,
        traits => [functor, applicative, monad, setoid],
        generator => fun() -> catena_stdgen:gen_maybe(catena_gen:gen_int()) end,
        adapter => #{
            map => fun(_F, none) -> none;
                      (F, {some, X}) -> {some, F(X)}
                   end,
            pure => fun(X) -> {some, X} end,
            ap => fun(_MF, none) -> none;
                     (none, _MX) -> none;
                     ({some, F}, {some, X}) -> {some, F(X)}
                  end,
            bind => fun(none, _F) -> none;
                       ({some, X}, F) -> F(X)
                    end,
            equals => fun(A, B) -> A =:= B end
        }
    };
instance_spec('either') ->
    #{
        label => <<"Either">>,
        traits => [functor, applicative, monad, setoid],
        generator => fun gen_either_int/0,
        adapter => #{
            map => fun(_F, {left, Error}) -> {left, Error};
                      (F, {right, X}) -> {right, F(X)}
                   end,
            pure => fun(X) -> {right, X} end,
            ap => fun(_MF, {left, Error}) -> {left, Error};
                     ({left, Error}, _MX) -> {left, Error};
                     ({right, F}, {right, X}) -> {right, F(X)}
                  end,
            bind => fun({left, Error}, _F) -> {left, Error};
                       ({right, X}, F) -> F(X)
                    end,
            equals => fun(A, B) -> A =:= B end
        }
    };
instance_spec('list') ->
    #{
        label => <<"List">>,
        traits => [functor, applicative, monad, semigroup, monoid, setoid],
        generator => fun() -> catena_stdgen:gen_list(catena_gen:gen_int()) end,
        adapter => #{
            map => fun(F, Xs) -> lists:map(F, Xs) end,
            pure => fun(X) -> [X] end,
            ap => fun(Fs, Xs) -> [F(X) || F <- Fs, X <- Xs] end,
            bind => fun(Xs, F) -> lists:flatmap(F, Xs) end,
            combine => fun(A, B) -> A ++ B end,
            empty => fun() -> [] end,
            equals => fun(A, B) -> A =:= B end
        }
    };
instance_spec('int') ->
    #{
        label => <<"Int">>,
        traits => [setoid, ord],
        %% Keep the ordered Int bridge on a small range so implication-heavy
        %% laws (equality/transitivity/antisymmetry) see enough matching cases.
        generator => fun() -> catena_gen:gen_int_range(-1, 1) end,
        adapter => #{
            equals => fun(A, B) -> A =:= B end,
            lte => fun(A, B) -> A =< B end
        }
    }.

gen_either_int() ->
    catena_gen:gen_one_of([
        catena_gen:gen_map(
            fun(E) -> {left, E} end,
            catena_gen:gen_int()
        ),
        catena_gen:gen_map(
            fun(V) -> {right, V} end,
            catena_gen:gen_int()
        )
    ]).

%%====================================================================
%% Helpers
%%====================================================================

normalize_text(Value) when is_atom(Value) ->
    normalize_text(atom_to_binary(Value));
normalize_text(Value) when is_list(Value) ->
    normalize_text(unicode:characters_to_binary(Value));
normalize_text(Value) when is_binary(Value) ->
    Lower = lower_binary(Value),
    binary:replace(
        binary:replace(
            binary:replace(Lower, <<" ">>, <<>>, [global]),
            <<"_">>,
            <<>>,
            [global]
        ),
        <<"-">>,
        <<>>,
        [global]
    ).

normalize_trait_list([], Acc) ->
    {ok, lists:reverse(Acc)};
normalize_trait_list([Trait | Rest], Acc) ->
    case normalize_trait(Trait) of
        {ok, TraitId} ->
            normalize_trait_list(Rest, [TraitId | Acc]);
        {error, _} = Error ->
            Error
    end.

ordered_unique(Items) ->
    lists:foldl(
        fun(Item, Acc) ->
            case lists:member(Item, Acc) of
                true -> Acc;
                false -> Acc ++ [Item]
            end
        end,
        [],
        Items
    ).

lower_binary(Value) when is_binary(Value) ->
    unicode:characters_to_binary([lower_char(Char) || Char <- binary_to_list(Value)]).

lower_char(Char) when Char >= $A, Char =< $Z ->
    Char + 32;
lower_char(Char) ->
    Char.
