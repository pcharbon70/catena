%% @doc Law Testing Framework - Law Specification Architecture (Section 4.1)
%%
%% This module implements the core law specification architecture for
%% verifying categorical properties of trait implementations.
%%
%% == Law Specification ==
%%
%% Laws are mathematical properties that trait instances must satisfy.
%% They are parameterized by generators and equality functions to be
%% type-agnostic.
%%
%% == Example ==
%%
%% ```
%% %% Define the identity law for Functor
%% IdentityLaw = law(
%%     "identity",
%%     "Applying identity function should not change the value",
%%     fun(Params) ->
%%         forall(Params#law_params.generator, fun(X) ->
%%             map(fun id/1, X) =:= X
%%         end)
%%     end
%% ).
%% ```
-module(catena_laws).

-include("catena_laws.hrl").
-include("catena_gen.hrl").
-include("catena_property.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Type exports
-export_type([
    law/0,
    law_params/0,
    law_set/0,
    eq_fn/0
]).

%%====================================================================
%% Section 4.1.1: Law Type Definition
%%====================================================================

%% API exports
-export([
    law/3,
    apply_law/2,
    law_name/1,
    law_description/1
]).

%%====================================================================
%% Section 4.1.2: Equality Abstraction
%%====================================================================

%% API exports
-export([
    eq_structural/0,
    eq_float/1,
    eq_ignore_order/1
]).

%%====================================================================
%% Section 4.1.3: Law Composition
%%====================================================================

%% API exports
-export([
    law_set/2,
    law_set_concat/2,
    law_set_map/2,
    law_set_filter/2,
    law_set_name/1,
    law_set_laws/1
]).

%%====================================================================
%% Types
%%====================================================================

-type law() :: #law{}.
-type law_params() :: #law_params{}.
-type law_set() :: #law_set{}.
-type eq_fn() :: fun((_, _) -> boolean()).

%%====================================================================
%% Section 4.1.1: Law Type Definition
%%====================================================================

%% @doc Create a new law from name, description, and property function.
%%
%% The property function receives a LawParams record and returns
%% a property that can be tested.
%%
%% == Example ==
%%
%% ```
%% IdentityLaw = law(
%%     "identity",
%%     "map id x == x",
%%     fun(Params) ->
%%         catena_property:forall(
%%             Params#law_params.generator,
%%             fun(X) -> my_map(fun id/1, X) =:= X end
%%         )
%%     end
%% )
%% ```
-spec law(binary() | string(), binary() | string(), fun((law_params()) -> catena_property:property())) -> law().
law(Name, Description, PropertyFn) when is_list(Name); is_list(Description) ->
    NameBin = to_binary(Name),
    DescBin = to_binary(Description),
    #law{name = NameBin, description = DescBin, property_fn = PropertyFn};
law(Name, Description, PropertyFn) when is_binary(Name), is_binary(Description) ->
    #law{name = Name, description = Description, property_fn = PropertyFn}.

%% @doc Apply a law with concrete parameters.
%%
%% Instantiates the law's property function with the given generator
%% and equality function.
%%
-spec apply_law(law(), law_params()) -> catena_property:property().
apply_law(#law{property_fn = PropertyFn}, Params) ->
    PropertyFn(Params).

%% @doc Get the name of a law.
-spec law_name(law()) -> binary().
law_name(#law{name = Name}) ->
    Name.

%% @doc Get the description of a law.
-spec law_description(law()) -> binary().
law_description(#law{description = Description}) ->
    Description.

%%====================================================================
%% Section 4.1.2: Equality Abstraction
%%====================================================================

%% @doc Structural equality using Erlang's =:=.
%%
%% This is the default equality function for most types.
-spec eq_structural() -> eq_fn().
eq_structural() ->
    fun(A, B) -> A =:= B end.

%% @doc Floating point equality with configurable epsilon.
%%
%% Use this for laws involving floating point numbers where
%% exact equality is too strict.
%%
%% == Example ==
%%
%% ```
%% Eq = eq_float(0.0001),
%% Eq(1.0, 1.00005)  % Returns true
%% ```
-spec eq_float(float()) -> eq_fn().
eq_float(Epsilon) when is_float(Epsilon); is_integer(Epsilon) ->
    fun(A, B) when is_number(A), is_number(B) ->
        abs(A - B) =< Epsilon;
       (_A, _B) ->
        false
    end.

%% @doc Equality that ignores order for collections.
%%
%% Useful for lists, sets, and other collections where order
%% doesn't affect equality.
%%
%% == Example ==
%%
%% ```
%% Eq = eq_ignore_order(fun(A, B) -> A =:= B end),
%% Eq([1, 2, 3], [3, 2, 1])  % Returns true
%% ```
-spec eq_ignore_order(fun((_, _) -> boolean())) -> eq_fn().
eq_ignore_order(BaseEq) ->
    fun(A, B) when is_list(A), is_list(B) ->
        %% Sort both lists and compare element-wise
        As = lists:sort(A),
        Bs = lists:sort(B),
        compare_lists(As, Bs, BaseEq);
       (A, B) ->
        %% For non-lists, use base equality
        BaseEq(A, B)
    end.

%% @private Compare two lists element-wise with the given equality function.
-spec compare_lists(list(), list(), fun((_, _) -> boolean())) -> boolean().
compare_lists([], [], _Eq) ->
    true;
compare_lists([A|As], [B|Bs], Eq) ->
    try Eq(A, B) of
        true -> compare_lists(As, Bs, Eq);
        false -> false
    catch
        _:_ -> false
    end;
compare_lists(_, _, _Eq) ->
    false.

%%====================================================================
%% Section 4.1.3: Law Composition
%%====================================================================

%% @doc Create a named set of laws.
%%
%% Law sets group related laws for a trait.
%%
%% == Example ==
%%
%% ```
%% FunctorLaws = law_set("Functor", [IdentityLaw, CompositionLaw])
%% ```
-spec law_set(binary() | string(), [law()]) -> law_set().
law_set(Name, Laws) when is_list(Name) ->
    law_set(unicode:characters_to_binary(Name), Laws);
law_set(Name, Laws) when is_binary(Name), is_list(Laws) ->
    #law_set{name = Name, laws = Laws}.

%% @doc Concatenate two law sets.
%%
%% Combines all laws from both sets under a new name.
%%
-spec law_set_concat(law_set(), law_set()) -> law_set().
law_set_concat(#law_set{name = Name1, laws = Laws1},
               #law_set{laws = Laws2}) ->
    CombinedName = <<Name1/binary, " + ", (integer_to_binary(length(Laws2)))/binary>>,
    #law_set{name = CombinedName, laws = Laws1 ++ Laws2}.

%% @doc Transform all laws in a set with a function.
%%
%% Useful for modifying law parameters or adding context.
%%
-spec law_set_map(fun((law()) -> law()), law_set()) -> law_set().
law_set_map(Fn, #law_set{name = Name, laws = Laws}) ->
    #law_set{name = Name, laws = [Fn(L) || L <- Laws]}.

%% @doc Filter laws in a set by a predicate.
%%
%% Useful for selecting a subset of laws to test.
%%
-spec law_set_filter(fun((law()) -> boolean()), law_set()) -> law_set().
law_set_filter(Pred, #law_set{name = Name, laws = Laws}) ->
    #law_set{name = Name, laws = [L || L <- Laws, Pred(L)]}.

%% @doc Get the name of a law set.
-spec law_set_name(law_set()) -> binary().
law_set_name(#law_set{name = Name}) ->
    Name.

%% @doc Get the list of laws from a law set.
-spec law_set_laws(law_set()) -> [law()].
law_set_laws(#law_set{laws = Laws}) ->
    Laws.

%%====================================================================
%% Internal Helpers
%%====================================================================

%% @private Convert various types to binary.
-spec to_binary(binary() | string() | atom()) -> binary().
to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) when is_list(List) -> unicode:characters_to_binary(List);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).

%%====================================================================
%% Unit Tests - Section 4.1
%%====================================================================

%%--------------------------------------------------------------------
%% Section 4.1.1: Law Type Definition Tests
%%--------------------------------------------------------------------

law_creation_test() ->
    Fn = fun(_Params) -> catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end) end,
    Law = law("test_law", "A test law", Fn),
    ?assertEqual(<<"test_law">>, Law#law.name),
    ?assertEqual(<<"A test law">>, Law#law.description),
    ?assert(is_function(Law#law.property_fn, 1)),
    ok.

law_creation_string_test() ->
    Fn = fun(_Params) -> catena_property:property("test", fun() ->
        catena_property:forall(catena_gen:gen_int(), fun(_) -> true end)
    end) end,
    Law = law("test_law", "A test law", Fn),
    ?assert(is_binary(Law#law.name)),
    ?assert(is_binary(Law#law.description)),
    ok.

apply_law_test() ->
    PropFn = fun(Params) ->
        catena_property:property("apply_test", fun() ->
            catena_property:forall(
                Params#law_params.generator,
                fun(N) -> N >= 0 end
            )
        end)
    end,
    Law = law("apply_test", "Test apply", PropFn),
    Params = #law_params{
        generator = catena_gen:gen_pos_int(),
        eq_fn = eq_structural(),
        extra_gens = #{}
    },
    Prop = apply_law(Law, Params),
    ?assert(is_record(Prop, property)),
    ok.

law_name_test() ->
    Law = law(<<"name">>, <<"desc">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    ?assertEqual(<<"name">>, law_name(Law)),
    ok.

law_description_test() ->
    Law = law(<<"name">>, <<"description">>, fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    ?assertEqual(<<"description">>, law_description(Law)),
    ok.

%%--------------------------------------------------------------------
%% Section 4.1.2: Equality Abstraction Tests
%%--------------------------------------------------------------------

eq_structural_test() ->
    Eq = eq_structural(),
    ?assert(Eq(1, 1)),
    ?assert(Eq("test", "test")),
    ?assertNot(Eq(1, 2)),
    ?assertNot(Eq("a", "b")),
    ok.

eq_float_test() ->
    Eq = eq_float(0.001),
    ?assert(Eq(1.0, 1.0)),
    ?assert(Eq(1.0, 1.0005)),
    ?assertNot(Eq(1.0, 1.01)),
    ok.

eq_ignore_order_lists_test() ->
    Eq = eq_ignore_order(fun(A, B) -> A =:= B end),
    ?assert(Eq([1, 2, 3], [3, 2, 1])),
    ?assert(Eq([1, 2], [1, 2])),
    ?assertNot(Eq([1, 2], [1, 2, 3])),
    ok.

eq_ignore_order_non_lists_test() ->
    Eq = eq_ignore_order(fun(A, B) -> A =:= B end),
    ?assert(Eq(42, 42)),
    ?assertNot(Eq(42, 43)),
    ok.

%%--------------------------------------------------------------------
%% Section 4.1.3: Law Composition Tests
%%--------------------------------------------------------------------

law_set_creation_test() ->
    Law1 = law("l1", "desc1", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law2 = law("l2", "desc2", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Set = law_set("TestSet", [Law1, Law2]),
    ?assertEqual(<<"TestSet">>, Set#law_set.name),
    ?assertEqual(2, length(Set#law_set.laws)),
    ok.

law_set_concat_test() ->
    Law1 = law("l1", "d1", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law2 = law("l2", "d2", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Set1 = law_set("S1", [Law1]),
    Set2 = law_set("S2", [Law2]),
    Combined = law_set_concat(Set1, Set2),
    ?assertEqual(2, length(Combined#law_set.laws)),
    ok.

law_set_map_test() ->
    Law1 = law("l1", "d1", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Set = law_set("S", [Law1]),
    Mapped = law_set_map(fun(L) -> L#law{name = <<(L#law.name)/binary, "_mapped">>} end, Set),
    [MappedLaw] = Mapped#law_set.laws,
    ?assertEqual(<<"l1_mapped">>, MappedLaw#law.name),
    ok.

law_set_filter_test() ->
    Law1 = law("keep1", "d1", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law2 = law("drop2", "d2", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law3 = law("keep3", "d3", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Set = law_set("S", [Law1, Law2, Law3]),
    Filtered = law_set_filter(fun(L) -> binary:match(L#law.name, <<"keep">>) =/= nomatch end, Set),
    ?assertEqual(2, length(Filtered#law_set.laws)),
    ok.

law_set_name_test() ->
    Set = law_set("MySet", []),
    ?assertEqual(<<"MySet">>, law_set_name(Set)),
    ok.

law_set_laws_test() ->
    Law1 = law("l1", "d1", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Law2 = law("l2", "d2", fun(_) -> catena_property:new(<<"t">>, catena_gen:gen_int(), fun(_) -> true end) end),
    Set = law_set("S", [Law1, Law2]),
    Laws = law_set_laws(Set),
    ?assertEqual(2, length(Laws)),
    ok.
