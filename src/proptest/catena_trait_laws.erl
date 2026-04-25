%% @doc Law Testing Framework - Trait Law Definitions (Section 4.2)
%%
%% This module defines the mathematical laws for Catena's category
%% theory traits. Each law is expressed as a property using the law
%% specification architecture.
%%
%% == Educational Purpose ==
%%
%% Law names and descriptions are designed to be educational, helping
%% developers understand the category theory behind the traits.
%%
%% @see catena_laws for law specification architecture
-module(catena_trait_laws).

-include("catena_laws.hrl").
-include("catena_gen.hrl").
-include("catena_property.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Section 4.2.1: Functor (Mapper) Laws
%%====================================================================

%% API exports
-export([
    functor_laws/0,
    functor_identity_law/0,
    functor_composition_law/0
]).

%%====================================================================
%% Section 4.2.2: Applicative (Applicator) Laws
%%====================================================================

%% API exports
-export([
    applicative_laws/0,
    applicative_identity_law/0,
    applicative_composition_law/0,
    applicative_homomorphism_law/0,
    applicative_interchange_law/0
]).

%%====================================================================
%% Section 4.2.3: Monad (Pipeline) Laws
%%====================================================================

%% API exports
-export([
    monad_laws/0,
    monad_left_identity_law/0,
    monad_right_identity_law/0,
    monad_associativity_law/0
]).

%%====================================================================
%% Section 4.2.4: Semigroup (Combiner) Laws
%%====================================================================

%% API exports
-export([
    semigroup_laws/0,
    semigroup_associativity_law/0
]).

%%====================================================================
%% Section 4.2.5: Monoid (Accumulator) Laws
%%====================================================================

%% API exports
-export([
    monoid_laws/0,
    monoid_left_identity_law/0,
    monoid_right_identity_law/0
]).

%%====================================================================
%% Section 4.2.6: Setoid (Comparable) Laws
%%====================================================================

%% API exports
-export([
    setoid_laws/0,
    setoid_reflexivity_law/0,
    setoid_symmetry_law/0,
    setoid_transitivity_law/0
]).

%%====================================================================
%% Section 4.2.7: Ord Laws
%%====================================================================

%% API exports
-export([
    ord_laws/0,
    ord_antisymmetry_law/0,
    ord_transitivity_law/0,
    ord_totality_law/0
]).

%%====================================================================
%% Types
%%====================================================================

-type trait_adapter() :: #{
    map => fun((fun((A) -> B), A) -> B),
    pure => fun((A) -> B),
    ap => fun((A, B) -> B),
    bind => fun((A, fun((A) -> B)) -> B),
    combine => fun((A, A) -> A),
    empty => fun(() -> A),
    equals => fun((A, A) -> boolean()),
    lte => fun((A, A) -> boolean())
}.

%%====================================================================
%% Section 4.2.1: Functor (Mapper) Laws
%%====================================================================

%% @doc Get the complete set of Functor laws.
%%
%% Functor laws ensure that `map` behaves consistently: mapping identity
%% does nothing, and mapping composed functions is the same as composing
%% the mapped results.
%%
-spec functor_laws() -> catena_laws:law_set().
functor_laws() ->
    catena_laws:law_set(
        "Functor (Mapper)",
        [
            functor_identity_law(),
            functor_composition_law()
        ]
    ).

%% @doc Identity law: `map id x == x`
%%
%% Mapping the identity function should not change the value.
%% This ensures that `map` doesn't introduce unexpected transformations.
%%
%% == Educational Note ==
%%
%% The identity law guarantees that `map` is structure-preserving.
%% If mapping `id` changes a value, then `map` is doing something
%% beyond just applying a function to each element.
%%
-spec functor_identity_law() -> catena_laws:law().
functor_identity_law() ->
    catena_laws:law(
        "identity",
        "map id x == x: Mapping identity should not change the value",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            MapFun = maps:get(map, Adapter),
            Eq = Params#law_params.eq_fn,

            catena_property:property(
                "functor_identity",
                fun() ->
                    catena_property:forall(
                        Params#law_params.generator,
                        fun(X) ->
                            Id = fun(A) -> A end,
                            Mapped = MapFun(Id, X),
                            Eq(Mapped, X)
                        end
                    )
                end
            )
        end
    ).

%% @doc Composition law: `map (f . g) x == map f (map g x)`
%%
%% Mapping composed functions should be the same as composing
%% the mapped results.
%%
%% == Educational Note ==
%%
%% The composition law ensures that `map` distributes over function
%% composition. This allows us to reason about sequences of maps
%% as a single map operation.
%%
-spec functor_composition_law() -> catena_laws:law().
functor_composition_law() ->
    catena_laws:law(
        "composition",
        "map (f . g) x == map f (map g x): Mapping composed functions equals composing mapped results",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            MapFun = maps:get(map, Adapter),
            Eq = Params#law_params.eq_fn,

            %% Get function generator for composition
            FunGen = maps:get(fun_gen, Params#law_params.extra_gens,

                %% Default: generate functions that add or multiply
                catena_gen:gen_one_of([
                    catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                        catena_gen:constant(fun(X) -> X + N end)
                    end),
                    catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                        catena_gen:constant(fun(X) -> X * N end)
                    end)
                ])
            ),

            catena_property:property(
                "functor_composition",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            FunGen,
                            FunGen
                        }),
                        fun(X, F, G) ->
                            Composed = fun(A) -> F(G(A)) end,
                                    MapBoth = MapFun(F, MapFun(G, X)),
                            MapComposed = MapFun(Composed, X),
                            Eq(MapComposed, MapBoth)
                        end
                    )
                end
            )
        end
    ).

%%====================================================================
%% Section 4.2.2: Applicative (Applicator) Laws
%%====================================================================

%% @doc Get the complete set of Applicative laws.
%%
%% Applicative laws ensure that `pure` and `ap` (apply) work together
%% consistently. Applicative builds on Functor by adding the ability
%% to apply functions wrapped in the context.
%%
-spec applicative_laws() -> catena_laws:law_set().
applicative_laws() ->
    catena_laws:law_set(
        "Applicative (Applicator)",
        [
            applicative_identity_law(),
            applicative_composition_law(),
            applicative_homomorphism_law(),
            applicative_interchange_law()
        ]
    ).

%% @doc Identity law: `ap (pure id) x == x`
%%
%% Applying a wrapped identity function should not change the value.
%%
-spec applicative_identity_law() -> catena_laws:law().
applicative_identity_law() ->
    catena_laws:law(
        "identity",
        "ap (pure id) x == x: Applying wrapped identity does nothing",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Ap = maps:get(ap, Adapter),
            Eq = Params#law_params.eq_fn,

            catena_property:property(
                "applicative_identity",
                fun() ->
                    catena_property:forall(
                        Params#law_params.generator,
                        fun(X) ->
                            Id = fun(A) -> A end,
                            PureId = Pure(Id),
                            Result = Ap(PureId, X),
                            Eq(Result, X)
                        end
                    )
                end
            )
        end
    ).

%% @doc Composition law: `ap (ap (ap (pure (.)) f) g) x == ap f (ap g x)`
%%
%% Composition of wrapped functions should work as expected.
%%
-spec applicative_composition_law() -> catena_laws:law().
applicative_composition_law() ->
    catena_laws:law(
        "composition",
        "ap (ap (ap (pure (.)) f) g) x == ap f (ap g x): Composition distributes over ap",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Ap = maps:get(ap, Adapter),
            Eq = Params#law_params.eq_fn,

            FunGen = maps:get(fun_gen, Params#law_params.extra_gens,
                catena_gen:gen_one_of([
                    catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                        catena_gen:constant(fun(X) -> X + N end)
                    end),
                    catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                        catena_gen:constant(fun(X) -> X * N end)
                    end)
                ])
            ),

            catena_property:property(
                "applicative_composition",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            FunGen,
                            FunGen
                        }),
                        fun(X, F, G) ->
                            Compose = fun(A) -> fun(B) -> fun(C) -> A(B(C)) end end end,
                            PureCompose = Pure(Compose),
                            ApComposeF = Ap(PureCompose, Pure(F)),
                            ApComposeFG = Ap(ApComposeF, Pure(G)),
                            Result1 = Ap(ApComposeFG, X),

                            ApG = Ap(Pure(G), X),
                            Result2 = Ap(Pure(F), ApG),

                            Eq(Result1, Result2)
                        end
                    )
                end
            )
        end
    ).

%% @doc Homomorphism law: `ap (pure f) (pure x) == pure (f x)`
%%
%% Applying a wrapped function to a wrapped value should be the same
%% as wrapping the result of applying the function.
%%
-spec applicative_homomorphism_law() -> catena_laws:law().
applicative_homomorphism_law() ->
    catena_laws:law(
        "homomorphism",
        "ap (pure f) (pure x) == pure (f x): Pure of application equals application of pures",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Ap = maps:get(ap, Adapter),
            Eq = Params#law_params.eq_fn,

            FunGen = maps:get(fun_gen, Params#law_params.extra_gens,
                catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                    catena_gen:constant(fun(X) -> X + N end)
                end)
            ),

            catena_property:property(
                "applicative_homomorphism",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({FunGen, catena_gen:gen_int()}),
                        fun(F, X) ->
                            PureF = Pure(F),
                            PureX = Pure(X),
                            Left = Ap(PureF, PureX),
                            Right = Pure(F(X)),
                            Eq(Left, Right)
                        end
                    )
                end
            )
        end
    ).

%% @doc Interchange law: `ap f (pure x) == ap (pure (fn g -> g x)) f`
%%
%% The order of function and value arguments can be interchanged.
%%
-spec applicative_interchange_law() -> catena_laws:law().
applicative_interchange_law() ->
    catena_laws:law(
        "interchange",
        "ap f (pure x) == ap (pure (fn g -> g x)) f: Function and value arguments can be interchanged",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Ap = maps:get(ap, Adapter),
            Eq = Params#law_params.eq_fn,

            FunGen = maps:get(fun_gen, Params#law_params.extra_gens,
                catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                    catena_gen:constant(fun(X) -> X + N end)
                end)
            ),

            catena_property:property(
                "applicative_interchange",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({FunGen, catena_gen:gen_int()}),
                        fun(F, X) ->
                            PureF = Pure(F),
                            PureX = Pure(X),
                            Left = Ap(PureF, PureX),

                            ApplyF = fun(G) -> G(X) end,
                            PureApplyF = Pure(ApplyF),
                            Right = Ap(PureApplyF, PureF),

                            Eq(Left, Right)
                        end
                    )
                end
            )
        end
    ).

%%====================================================================
%% Section 4.2.3: Monad (Pipeline) Laws
%%====================================================================

%% @doc Get the complete set of Monad laws.
%%
%% Monad laws ensure that `bind` (also called `>>=` or `pipe`) works
%% consistently with `pure`. Monad builds on Applicative by adding
%% the ability to sequence computations where each step depends on
%% the previous.
%%
-spec monad_laws() -> catena_laws:law_set().
monad_laws() ->
    catena_laws:law_set(
        "Monad (Pipeline)",
        [
            monad_left_identity_law(),
            monad_right_identity_law(),
            monad_associativity_law()
        ]
    ).

%% @doc Left identity law: `bind (pure a) f == f a`
%%
%% Wrapping a value then binding should be the same as just
%% applying the function.
%%
-spec monad_left_identity_law() -> catena_laws:law().
monad_left_identity_law() ->
    catena_laws:law(
        "left_identity",
        "bind (pure a) f == f a: Wrapping then binding equals applying the function",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Bind = maps:get(bind, Adapter),
            Eq = Params#law_params.eq_fn,

            %% Generate Kleisli arrows: a -> m b
            KleisliGen = maps:get(kleisli_gen, Params#law_params.extra_gens,
                catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                    catena_gen:constant(fun(X) -> Pure(X + N) end)
                end)
            ),

            catena_property:property(
                "monad_left_identity",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({catena_gen:gen_int(), KleisliGen}),
                        fun(A, F) ->
                            Left = Bind(Pure(A), F),
                            Right = F(A),
                            Eq(Left, Right)
                        end
                    )
                end
            )
        end
    ).

%% @doc Right identity law: `bind m pure == m`
%%
%% Binding the pure function should not change the value.
%%
-spec monad_right_identity_law() -> catena_laws:law().
monad_right_identity_law() ->
    catena_laws:law(
        "right_identity",
        "bind m pure == m: Binding pure does nothing",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Bind = maps:get(bind, Adapter),
            Eq = Params#law_params.eq_fn,

            catena_property:property(
                "monad_right_identity",
                fun() ->
                    catena_property:forall(
                        Params#law_params.generator,
                        fun(M) ->
                            Result = Bind(M, Pure),
                            Eq(Result, M)
                        end
                    )
                end
            )
        end
    ).

%% @doc Associativity law: `bind (bind m f) g == bind m (fn x -> bind (f x) g)`
%%
%% The order of binding should not matter for associativity.
%%
-spec monad_associativity_law() -> catena_laws:law().
monad_associativity_law() ->
    catena_laws:law(
        "associativity",
        "bind (bind m f) g == bind m (fn x -> bind (f x) g): Binding is associative",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Pure = maps:get(pure, Adapter),
            Bind = maps:get(bind, Adapter),
            Eq = Params#law_params.eq_fn,

            KleisliGen = maps:get(kleisli_gen, Params#law_params.extra_gens,
                catena_gen:gen_bind(catena_gen:gen_int(), fun(N) ->
                    catena_gen:constant(fun(X) -> Pure(X + N) end)
                end)
            ),

            catena_property:property(
                "monad_associativity",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            KleisliGen,
                            KleisliGen
                        }),
                        fun(M, F, G) ->
                            Left = Bind(Bind(M, F), G),
                            Right = Bind(M, fun(X) -> Bind(F(X), G) end),
                            Eq(Left, Right)
                        end
                    )
                end
            )
        end
    ).

%%====================================================================
%% Section 4.2.4: Semigroup (Combiner) Laws
%%====================================================================

%% @doc Get the complete set of Semigroup laws.
%%
%% Semigroup laws ensure that the `combine` operation (often written
%% as `<>`) is associative, allowing grouping without parentheses.
%%
-spec semigroup_laws() -> catena_laws:law_set().
semigroup_laws() ->
    catena_laws:law_set(
        "Semigroup (Combiner)",
        [
            semigroup_associativity_law()
        ]
    ).

%% @doc Associativity law: `combine (combine a b) c == combine a (combine b c)`
%%
%% The order of combining doesn't matter for grouping.
%%
-spec semigroup_associativity_law() -> catena_laws:law().
semigroup_associativity_law() ->
    catena_laws:law(
        "associativity",
        "combine (combine a b) c == combine a (combine b c): Combination is associative",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Combine = maps:get(combine, Adapter),
            Eq = Params#law_params.eq_fn,

            catena_property:property(
                "semigroup_associativity",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            Params#law_params.generator,
                            Params#law_params.generator
                        }),
                        fun(A, B, C) ->
                            Left = Combine(Combine(A, B), C),
                            Right = Combine(A, Combine(B, C)),
                            Eq(Left, Right)
                        end
                    )
                end
            )
        end
    ).

%%====================================================================
%% Section 4.2.5: Monoid (Accumulator) Laws
%%====================================================================

%% @doc Get the complete set of Monoid laws.
%%
%% Monoid laws ensure that there's an identity element (`empty`) that
%% when combined with any value returns that value.
%%
-spec monoid_laws() -> catena_laws:law_set().
monoid_laws() ->
    catena_laws:law_set(
        "Monoid (Accumulator)",
        [
            monoid_left_identity_law(),
            monoid_right_identity_law()
            %% Associativity inherited from Semigroup
        ]
    ).

%% @doc Left identity law: `combine empty a == a`
%%
%% Combining with empty on the left returns the value.
%%
-spec monoid_left_identity_law() -> catena_laws:law().
monoid_left_identity_law() ->
    catena_laws:law(
        "left_identity",
        "combine empty a == a: Empty is left identity",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Combine = maps:get(combine, Adapter),
            Empty = maps:get(empty, Adapter),
            Eq = Params#law_params.eq_fn,

            catena_property:property(
                "monoid_left_identity",
                fun() ->
                    catena_property:forall(
                        Params#law_params.generator,
                        fun(A) ->
                            Result = Combine(Empty(), A),
                            Eq(Result, A)
                        end
                    )
                end
            )
        end
    ).

%% @doc Right identity law: `combine a empty == a`
%%
%% Combining with empty on the right returns the value.
%%
-spec monoid_right_identity_law() -> catena_laws:law().
monoid_right_identity_law() ->
    catena_laws:law(
        "right_identity",
        "combine a empty == a: Empty is right identity",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Combine = maps:get(combine, Adapter),
            Empty = maps:get(empty, Adapter),
            Eq = Params#law_params.eq_fn,

            catena_property:property(
                "monoid_right_identity",
                fun() ->
                    catena_property:forall(
                        Params#law_params.generator,
                        fun(A) ->
                            Result = Combine(A, Empty()),
                            Eq(Result, A)
                        end
                    )
                end
            )
        end
    ).

%%====================================================================
%% Section 4.2.6: Setoid (Comparable) Laws
%%====================================================================

%% @doc Get the complete set of Setoid laws.
%%
%% Setoid laws ensure that the `equals` operation forms an equivalence
%% relation: reflexive, symmetric, and transitive.
%%
-spec setoid_laws() -> catena_laws:law_set().
setoid_laws() ->
    catena_laws:law_set(
        "Setoid (Comparable)",
        [
            setoid_reflexivity_law(),
            setoid_symmetry_law(),
            setoid_transitivity_law()
        ]
    ).

%% @doc Reflexivity law: `equals a a == true`
%%
%% Every value equals itself.
%%
-spec setoid_reflexivity_law() -> catena_laws:law().
setoid_reflexivity_law() ->
    catena_laws:law(
        "reflexivity",
        "equals a a == true: Every value equals itself",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Equals = maps:get(equals, Adapter),

            catena_property:property(
                "setoid_reflexivity",
                fun() ->
                    catena_property:forall(
                        Params#law_params.generator,
                        fun(A) ->
                            Equals(A, A)
                        end
                    )
                end
            )
        end
    ).

%% @doc Symmetry law: `equals a b == equals b a`
%%
%% Equality works both ways.
%%
-spec setoid_symmetry_law() -> catena_laws:law().
setoid_symmetry_law() ->
    catena_laws:law(
        "symmetry",
        "equals a b == equals b a: Equality is symmetric",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Equals = maps:get(equals, Adapter),

            catena_property:property(
                "setoid_symmetry",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            Params#law_params.generator
                        }),
                        fun(A, B) ->
                            Equals(A, B) =:= Equals(B, A)
                        end
                    )
                end
            )
        end
    ).

%% @doc Transitivity law: `equals a b && equals b c ==> equals a c`
%%
%% If a equals b and b equals c, then a equals c.
%%
-spec setoid_transitivity_law() -> catena_laws:law().
setoid_transitivity_law() ->
    catena_laws:law(
        "transitivity",
        "equals a b && equals b c ==> equals a c: Equality is transitive",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Equals = maps:get(equals, Adapter),

            catena_property:property(
                "setoid_transitivity",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            Params#law_params.generator,
                            Params#law_params.generator
                        }),
                        fun(A, B, C) ->
                            catena_property:implies(
                                Equals(A, B) andalso Equals(B, C),
                                fun() -> Equals(A, C) end
                            )
                        end
                    )
                end
            )
        end
    ).

%%====================================================================
%% Section 4.2.7: Ord Laws
%%====================================================================

%% @doc Get the complete set of Ord laws.
%%
%% Ord laws ensure that the `lte` (less than or equal) operation forms
%% a total order: antisymmetric, transitive, and total.
%%
-spec ord_laws() -> catena_laws:law_set().
ord_laws() ->
    catena_laws:law_set(
        "Ord (Ordered)",
        [
            ord_antisymmetry_law(),
            ord_transitivity_law(),
            ord_totality_law()
        ]
    ).

%% @doc Antisymmetry law: `lte a b && lte b a ==> equals a b`
%%
%% If a is less than or equal to b and b is less than or equal to a,
%% then a equals b.
%%
-spec ord_antisymmetry_law() -> catena_laws:law().
ord_antisymmetry_law() ->
    catena_laws:law(
        "antisymmetry",
        "lte a b && lte b a ==> equals a b: Mutual less-than-or-equal implies equality",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Lte = maps:get(lte, Adapter),
            Equals = maps:get(equals, Adapter, fun(A, B) -> A =:= B end),

            catena_property:property(
                "ord_antisymmetry",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            Params#law_params.generator
                        }),
                        fun(A, B) ->
                            catena_property:implies(
                                Lte(A, B) andalso Lte(B, A),
                                fun() -> Equals(A, B) end
                            )
                        end
                    )
                end
            )
        end
    ).

%% @doc Transitivity law: `lte a b && lte b c ==> lte a c`
%%
%% If a is less than or equal to b and b is less than or equal to c,
%% then a is less than or equal to c.
%%
-spec ord_transitivity_law() -> catena_laws:law().
ord_transitivity_law() ->
    catena_laws:law(
        "transitivity",
        "lte a b && lte b c ==> lte a c: Less-than-or-equal is transitive",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Lte = maps:get(lte, Adapter),

            catena_property:property(
                "ord_transitivity",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            Params#law_params.generator,
                            Params#law_params.generator
                        }),
                        fun(A, B, C) ->
                            catena_property:implies(
                                Lte(A, B) andalso Lte(B, C),
                                fun() -> Lte(A, C) end
                            )
                        end
                    )
                end
            )
        end
    ).

%% @doc Totality law: `lte a b || lte b a == true`
%%
%% For any two values, one is less than or equal to the other.
%%
-spec ord_totality_law() -> catena_laws:law().
ord_totality_law() ->
    catena_laws:law(
        "totality",
        "lte a b || lte b a == true: Any two values are comparable",
        fun(Params) ->
            Adapter = maps:get(adapter, Params#law_params.extra_gens),
            Lte = maps:get(lte, Adapter),

            catena_property:property(
                "ord_totality",
                fun() ->
                    catena_property:forall(
                        catena_stdgen:gen_tuple({
                            Params#law_params.generator,
                            Params#law_params.generator
                        }),
                        fun(A, B) ->
                            Lte(A, B) orelse Lte(B, A)
                        end
                    )
                end
            )
        end
    ).
