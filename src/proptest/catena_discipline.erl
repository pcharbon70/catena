%% @doc Law Testing Framework - Discipline Framework (Section 4.3)
%%
%% This module implements the discipline framework that packages laws
%% into reusable test suites for traits.
%%
%% == Discipline Definition ==
%%
%% A discipline defines what laws apply to a trait, how to parameterize
%% them, and what auxiliary generators are needed (e.g., function generators
%% for composition laws).
%%
%% @see catena_laws for law specification architecture
%% @see catena_trait_laws for trait law definitions
-module(catena_discipline).

-include("catena_laws.hrl").
-include("catena_property.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Type exports
-export_type([
    discipline/0
]).

%%====================================================================
%% Section 4.3.1: Discipline Type Definition
%%====================================================================

%% API exports
-export([
    discipline/3,
    discipline/2,
    discipline_name/1,
    discipline_laws/2,
    discipline_traits/1,
    discipline_auxiliary/1
]).

%%====================================================================
%% Section 4.3.2: Standard Disciplines
%%====================================================================

%% API exports
-export([
    functor_discipline/0,
    applicative_discipline/0,
    monad_discipline/0,
    semigroup_discipline/0,
    monoid_discipline/0,
    setoid_discipline/0,
    ord_discipline/0
]).

%%====================================================================
%% Section 4.3.3: Auxiliary Generators
%%====================================================================

%% API exports
-export([
    default_fun_gen/0,
    default_kleisli_gen/0,
    with_auxiliary/2
]).

%%====================================================================
%% Section 4.3.4: Discipline Composition
%%====================================================================

%% API exports
-export([
    discipline_combine/2,
    all_laws_for/1
]).

%%====================================================================
%% Types
%%====================================================================

-type discipline() :: #discipline{}.
-type trait() :: functor | applicative | monad | semigroup | monoid | setoid | ord.
-type adapter() :: #{
    map => fun(),
    pure => fun(),
    ap => fun(),
    bind => fun(),
    combine => fun(),
    empty => fun(),
    equals => fun(),
    lte => fun()
}.

%%====================================================================
%% Section 4.3.1: Discipline Type Definition
%%====================================================================

%% @doc Create a new discipline for a trait.
%%
%% A discipline packages the laws that apply to a trait along with
%% the required traits and auxiliary generators.
%%
-spec discipline(atom(), [catena_laws:law()], [trait()]) -> discipline().
discipline(TraitName, Laws, Requires) ->
    Auxiliary = default_auxiliary(TraitName),
    #discipline{
        trait_name = TraitName,
        laws = Laws,
        requires = Requires,
        auxiliary = Auxiliary
    }.

%% @doc Create a discipline without required traits.
-spec discipline(atom(), [catena_laws:law()]) -> discipline().
discipline(TraitName, Laws) ->
    discipline(TraitName, Laws, []).

%% @doc Get the name of a discipline.
-spec discipline_name(discipline()) -> atom().
discipline_name(#discipline{trait_name = Name}) ->
    Name.

%% @doc Get the laws from a discipline with parameters applied.
%%
%% Takes an adapter map containing the trait implementation functions
%% and returns the laws ready for testing.
%%
-spec discipline_laws(discipline(), adapter()) -> [catena_property:property()].
discipline_laws(#discipline{laws = Laws}, Adapter) ->
    ExtraGens = #{
        adapter => Adapter,
        fun_gen => default_fun_gen(),
        kleisli_gen => default_kleisli_gen()
    },
    Params = #law_params{
        generator = undefined,  %% Will be set per-test
        eq_fn = maps:get(equals, Adapter, fun(A, B) -> A =:= B end),
        extra_gens = ExtraGens
    },
    lists:map(fun(Law) -> catena_laws:apply_law(Law, Params) end, Laws).

%% @doc Get the list of required traits for a discipline.
-spec discipline_traits(discipline()) -> [trait()].
discipline_traits(#discipline{requires = Requires}) ->
    Requires.

%% @doc Get the auxiliary generators for a discipline.
-spec discipline_auxiliary(discipline()) -> #{atom() => catena_gen:generator(_)}.
discipline_auxiliary(#discipline{auxiliary = Auxiliary}) ->
    Auxiliary.

%%====================================================================
%% Section 4.3.2: Standard Disciplines
%%====================================================================

%% @doc Get the standard Functor discipline.
%%
%% The Functor discipline requires no other traits and includes
%% identity and composition laws.
%%
-spec functor_discipline() -> discipline().
functor_discipline() ->
    Laws = catena_laws:law_set_laws(catena_trait_laws:functor_laws()),
    discipline(functor, Laws, []).

%% @doc Get the standard Applicative discipline.
%%
%% The Applicative discipline requires Functor and includes
%% identity, composition, homomorphism, and interchange laws.
%%
-spec applicative_discipline() -> discipline().
applicative_discipline() ->
    Laws = catena_laws:law_set_laws(catena_trait_laws:applicative_laws()),
    %% Applicative implies Functor, so we include Functor laws
    AllLaws = (catena_laws:law_set_laws(catena_trait_laws:functor_laws())) ++ Laws,
    discipline(applicative, AllLaws, [functor]).

%% @doc Get the standard Monad discipline.
%%
%% The Monad discipline requires Applicative (and thus Functor)
%% and includes left identity, right identity, and associativity laws.
%%
-spec monad_discipline() -> discipline().
monad_discipline() ->
    %% Monad includes all Applicative and Functor laws
    FunctorLaws = catena_laws:law_set_laws(catena_trait_laws:functor_laws()),
    ApplicativeLaws = catena_laws:law_set_laws(catena_trait_laws:applicative_laws()),
    MonadLaws = catena_laws:law_set_laws(catena_trait_laws:monad_laws()),
    AllLaws = FunctorLaws ++ ApplicativeLaws ++ MonadLaws,
    discipline(monad, AllLaws, [applicative, functor]).

%% @doc Get the standard Semigroup discipline.
%%
%% The Semigroup discipline requires no other traits and includes
%% the associativity law.
%%
-spec semigroup_discipline() -> discipline().
semigroup_discipline() ->
    Laws = catena_laws:law_set_laws(catena_trait_laws:semigroup_laws()),
    discipline(semigroup, Laws, []).

%% @doc Get the standard Monoid discipline.
%%
%% The Monoid discipline requires Semigroup and includes
%% left identity, right identity, and associativity laws.
%%
-spec monoid_discipline() -> discipline().
monoid_discipline() ->
    %% Monoid includes Semigroup laws
    SemigroupLaws = catena_laws:law_set_laws(catena_trait_laws:semigroup_laws()),
    MonoidLaws = catena_laws:law_set_laws(catena_trait_laws:monoid_laws()),
    AllLaws = SemigroupLaws ++ MonoidLaws,
    discipline(monoid, AllLaws, [semigroup]).

%% @doc Get the standard Setoid discipline.
%%
%% The Setoid discipline requires no other traits and includes
%% reflexivity, symmetry, and transitivity laws.
%%
-spec setoid_discipline() -> discipline().
setoid_discipline() ->
    Laws = catena_laws:law_set_laws(catena_trait_laws:setoid_laws()),
    discipline(setoid, Laws, []).

%% @doc Get the standard Ord discipline.
%%
%% The Ord discipline requires Setoid and includes
%% antisymmetry, transitivity, and totality laws.
%%
-spec ord_discipline() -> discipline().
ord_discipline() ->
    %% Ord includes Setoid laws
    SetoidLaws = catena_laws:law_set_laws(catena_trait_laws:setoid_laws()),
    OrdLaws = catena_laws:law_set_laws(catena_trait_laws:ord_laws()),
    AllLaws = SetoidLaws ++ OrdLaws,
    discipline(ord, AllLaws, [setoid]).

%%====================================================================
%% Section 4.3.3: Auxiliary Generators
%%====================================================================

%% @doc Get the default function generator for composition laws.
%%
%% Generates simple arithmetic functions for testing.
%%
-spec default_fun_gen() -> catena_gen:generator(fun((_A) -> _B)).
default_fun_gen() ->
    catena_gen:gen_one_of([
        catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
            catena_gen:constant(fun(X) -> X + N end)
        end),
        catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
            catena_gen:constant(fun(X) -> X * N end)
        end),
        catena_gen:constant(fun(X) -> X end),
        catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
            catena_gen:constant(fun(_X) -> N end)
        end)
    ]).

%% @doc Get the default Kleisli arrow generator for monad laws.
%%
%% Generates functions that return values wrapped in pure.
%%
-spec default_kleisli_gen() -> catena_gen:generator(fun((_A) -> _B)).
default_kleisli_gen() ->
    catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
        catena_gen:constant(fun(X) -> X + N end)
    end).

%% @doc Add custom auxiliary generators to a discipline.
%%
%% Allows overriding default auxiliary generators with custom ones.
%%
-spec with_auxiliary(discipline(), #{atom() => catena_gen:generator(_)}) -> discipline().
with_auxiliary(Discipline, Auxiliary) ->
    Discipline#discipline{auxiliary = Auxiliary}.

%%====================================================================
%% Section 4.3.4: Discipline Composition
%%====================================================================

%% @doc Combine two disciplines into one.
%%
%% Removes duplicate laws when disciplines overlap.
%%
-spec discipline_combine(discipline(), discipline()) -> discipline().
discipline_combine(#discipline{laws = Laws1, requires = Req1, auxiliary = Aux1},
                    #discipline{laws = Laws2, requires = Req2, auxiliary = Aux2}) ->
    %% Remove duplicate laws by name
    AllLaws = Laws1 ++ Laws2,
    UniqueLaws = remove_duplicate_laws(AllLaws),
    CombinedName = combined_trait_name(Req1 ++ Req2),
    #discipline{
        trait_name = CombinedName,
        laws = UniqueLaws,
        requires = lists:usort(Req1 ++ Req2),
        auxiliary = maps:merge(Aux1, Aux2)
    }.

%% @doc Collect all laws for a list of traits.
%%
%% Returns a combined discipline with all laws from the specified traits.
%%
-spec all_laws_for([trait()]) -> discipline().
all_laws_for(Traits) ->
    Disciplines = [discipline_for_trait(T) || T <- Traits],
    lists:foldl(fun discipline_combine/2, hd(Disciplines), tl(Disciplines)).

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Get default auxiliary generators for a trait.
-spec default_auxiliary(trait()) -> #{atom() => catena_gen:generator(_)}.
default_auxiliary(functor) ->
    #{};
default_auxiliary(applicative) ->
    #{};
default_auxiliary(monad) ->
    #{};
default_auxiliary(semigroup) ->
    #{};
default_auxiliary(monoid) ->
    #{};
default_auxiliary(setoid) ->
    #{};
default_auxiliary(ord) ->
    #{};
default_auxiliary(_) ->
    #{}.

%% @private Get the discipline for a single trait.
-spec discipline_for_trait(trait()) -> discipline().
discipline_for_trait(functor) -> functor_discipline();
discipline_for_trait(applicative) -> applicative_discipline();
discipline_for_trait(monad) -> monad_discipline();
discipline_for_trait(semigroup) -> semigroup_discipline();
discipline_for_trait(monoid) -> monoid_discipline();
discipline_for_trait(setoid) -> setoid_discipline();
discipline_for_trait(ord) -> ord_discipline().

%% @private Remove duplicate laws by name.
-spec remove_duplicate_laws([catena_laws:law()]) -> [catena_laws:law()].
remove_duplicate_laws(Laws) ->
    Names = [catena_laws:law_name(L) || L <- Laws],
    UniqueNames = lists:usort(Names),
    [get_law_by_name(Laws, Name) || Name <- UniqueNames].

%% @private Get a law by its name from a list.
-spec get_law_by_name([catena_laws:law()], binary()) -> catena_laws:law().
get_law_by_name([Law | Rest], Name) ->
    case catena_laws:law_name(Law) of
        Name -> Law;
        _ -> get_law_by_name(Rest, Name)
    end;
get_law_by_name([], Name) ->
    error({law_not_found, Name}).

%% @private Create a combined trait name.
-spec combined_trait_name([atom()]) -> atom().
combined_trait_name(Traits) ->
    TraitStrings = [atom_to_list(T) || T <- lists:usort(Traits)],
    CombinedName = string:join(TraitStrings, "_"),
    list_to_existing_atom(CombinedName).

%% @private Convert string to existing atom, or create new one.
-spec list_to_existing_atom(string()) -> atom().
list_to_existing_atom(String) ->
    try
        binary_to_atom(unicode:characters_to_binary(String), utf8)
    catch
        error:badarg ->
            list_to_atom(String)
    end.

%%====================================================================
%% Unit Tests - Section 4.3
%%====================================================================

%%--------------------------------------------------------------------
%% Section 4.3.1: Discipline Type Definition Tests
%%--------------------------------------------------------------------

discipline_creation_test() ->
    Law1 = catena_laws:law(<<"l1">>, <<"d1">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Disc = discipline(test_trait, [Law1]),
    ?assertEqual(test_trait, discipline_name(Disc)),
    ?assertEqual(1, length(Disc#discipline.laws)),
    ok.

discipline_with_requires_test() ->
    Law1 = catena_laws:law(<<"l1">>, <<"d1">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Disc = discipline(test_trait, [Law1], [dependency]),
    ?assertEqual([dependency], discipline_traits(Disc)),
    ok.

discipline_laws_applies_params_test() ->
    Adapter = #{map => fun(F, X) -> lists:map(F, X) end, equals => fun(A, B) -> A =:= B end},
    Disc = functor_discipline(),
    Properties = discipline_laws(Disc, Adapter),
    ?assert(length(Properties) >= 2),
    ok.

%%--------------------------------------------------------------------
%% Section 4.3.2: Standard Disciplines Tests
%%--------------------------------------------------------------------

functor_discipline_has_laws_test() ->
    Disc = functor_discipline(),
    ?assertEqual(functor, discipline_name(Disc)),
    ?assertEqual([], discipline_traits(Disc)),
    ?assert(length(Disc#discipline.laws) >= 2),
    ok.

applicative_discipline_requires_functor_test() ->
    Disc = applicative_discipline(),
    ?assertEqual(applicative, discipline_name(Disc)),
    ?assert(lists:member(functor, discipline_traits(Disc))),
    ok.

monad_discipline_requires_applicative_test() ->
    Disc = monad_discipline(),
    ?assertEqual(monad, discipline_name(Disc)),
    ?assert(lists:member(applicative, discipline_traits(Disc))),
    ?assert(lists:member(functor, discipline_traits(Disc))),
    ok.

monoid_discipline_requires_semigroup_test() ->
    Disc = monoid_discipline(),
    ?assertEqual(monoid, discipline_name(Disc)),
    ?assert(lists:member(semigroup, discipline_traits(Disc))),
    ok.

ord_discipline_requires_setoid_test() ->
    Disc = ord_discipline(),
    ?assertEqual(ord, discipline_name(Disc)),
    ?assert(lists:member(setoid, discipline_traits(Disc))),
    ok.

%%--------------------------------------------------------------------
%% Section 4.3.3: Auxiliary Generators Tests
%%--------------------------------------------------------------------

default_fun_gen_generates_functions_test() ->
    Gen = default_fun_gen(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Fun = catena_tree:root(Tree),
    ?assert(is_function(Fun, 1)),
    Result = Fun(5),
    ?assert(is_integer(Result)),
    ok.

default_kleisli_gen_generates_functions_test() ->
    Gen = default_kleisli_gen(),
    Tree = catena_gen:run(Gen, 10, catena_gen:seed_new()),
    Fun = catena_tree:root(Tree),
    ?assert(is_function(Fun, 1)),
    Result = Fun(5),
    ?assert(is_integer(Result)),
    ok.

with_auxiliary_overrides_defaults_test() ->
    Disc = functor_discipline(),
    CustomGen = catena_gen:constant(fun(X) -> X end),
    UpdatedDisc = with_auxiliary(Disc, #{fun_gen => CustomGen}),
    ?assertEqual(CustomGen, maps:get(fun_gen, discipline_auxiliary(UpdatedDisc))),
    ok.

%%--------------------------------------------------------------------
%% Section 4.3.4: Discipline Composition Tests
%%--------------------------------------------------------------------

discipline_combine_removes_duplicates_test() ->
    Law1 = catena_laws:law(<<"l1">>, <<"d1">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law2 = catena_laws:law(<<"l2">>, <<"d2">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Disc1 = discipline(t1, [Law1, Law2]),
    Disc2 = discipline(t2, [Law1]),  %% Duplicate l1
    Combined = discipline_combine(Disc1, Disc2),
    %% Should have 2 unique laws, not 3
    ?assertEqual(2, length(Combined#discipline.laws)),
    ok.

discipline_combine_merges_requirements_test() ->
    Law1 = catena_laws:law(<<"l1">>, <<"d1">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law2 = catena_laws:law(<<"l2">>, <<"d2">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Disc1 = discipline(t1, [Law1], [r1]),
    Disc2 = discipline(t2, [Law2], [r2]),
    Combined = discipline_combine(Disc1, Disc2),
    ?assertEqual([r1, r2], lists:sort(discipline_traits(Combined))),
    ok.

all_laws_for_multiple_traits_test() ->
    Disc = all_laws_for([functor, monoid]),
    ?assert(length(Disc#discipline.laws) >= 3),
    ok.
