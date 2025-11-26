%%%-------------------------------------------------------------------
%%% @doc Standard Library Trait Instances
%%%
%%% This module provides trait instances for Catena's standard library
%%% traits as defined in prelude.cat. These instances enable type
%%% checking of trait method calls like `map`, `chain`, `combine`, etc.
%%%
%%% Trait Hierarchy (from prelude.cat):
%%% - Comparable: equals/notEquals
%%% - Orderable: extends Comparable, adds compare/lessThan/etc
%%% - Combiner: combine
%%% - Accumulator: extends Combiner, adds empty
%%% - Mapper: map (Functor)
%%% - Applicator: extends Mapper, adds apply/wrap (Applicative)
%%% - Chainable: extends Mapper, adds chain (Monad bind)
%%% - Pipeline: extends Applicator + Chainable (full Monad)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_stdlib_instances).

-export([
    stdlib_instance_db/0,
    add_comparable_instances/1,
    add_orderable_instances/1,
    add_combiner_instances/1,
    add_mapper_instances/1,
    add_applicator_instances/1,
    add_chainable_instances/1,
    add_pipeline_instances/1
]).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Get instance database with all stdlib instances
%%
%% Creates a complete instance database for Catena's prelude traits.
-spec stdlib_instance_db() -> catena_instance:instance_db().
stdlib_instance_db() ->
    DB0 = catena_instance:empty_instance_db(),
    DB1 = add_comparable_instances(DB0),
    DB2 = add_orderable_instances(DB1),
    DB3 = add_combiner_instances(DB2),
    DB4 = add_mapper_instances(DB3),
    DB5 = add_applicator_instances(DB4),
    DB6 = add_chainable_instances(DB5),
    DB7 = add_pipeline_instances(DB6),
    DB7.

%%%===================================================================
%%% Comparable Instances (Setoid / Eq)
%%%===================================================================

%% @doc Add Comparable instances for primitive types and type constructors
%%
%% Comparable provides:
%%   equals : a -> a -> Bool
%%   notEquals : a -> a -> Bool (derived)
-spec add_comparable_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_comparable_instances(DB0) ->
    %% Primitives
    DB1 = add_instance('Comparable', [tcon(int)], DB0),
    DB2 = add_instance('Comparable', [tcon(float)], DB1),
    DB3 = add_instance('Comparable', [tcon(bool)], DB2),
    DB4 = add_instance('Comparable', [tcon(string)], DB3),
    DB5 = add_instance('Comparable', [tcon(atom)], DB4),

    %% Type constructors (need element type to be Comparable)
    %% instance Comparable a => Comparable (Maybe a)
    DB6 = add_instance('Comparable', [tcon('Maybe')], DB5),

    %% instance Comparable a => Comparable (List a)
    DB7 = add_instance('Comparable', [tcon(list)], DB6),

    %% instance (Comparable e, Comparable a) => Comparable (Either e a)
    DB8 = add_instance('Comparable', [tcon('Either')], DB7),

    DB8.

%%%===================================================================
%%% Orderable Instances (Ord)
%%%===================================================================

%% @doc Add Orderable instances for primitive types
%%
%% Orderable extends Comparable and provides:
%%   compare : a -> a -> Ordering
%%   lessThan, greaterThan, etc.
-spec add_orderable_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_orderable_instances(DB0) ->
    %% Primitives that have natural ordering
    DB1 = add_instance('Orderable', [tcon(int)], DB0),
    DB2 = add_instance('Orderable', [tcon(float)], DB1),
    DB3 = add_instance('Orderable', [tcon(string)], DB2),
    DB3.

%%%===================================================================
%%% Combiner Instances (Semigroup)
%%%===================================================================

%% @doc Add Combiner instances
%%
%% Combiner provides:
%%   combine : a -> a -> a
-spec add_combiner_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_combiner_instances(DB0) ->
    %% String concatenation
    DB1 = add_instance('Combiner', [tcon(string)], DB0),

    %% List concatenation
    DB2 = add_instance('Combiner', [tapp(tcon(list), [tvar(0)])], DB1),

    %% Maybe - returns first Some
    DB3 = add_instance('Combiner', [tapp(tcon('Maybe'), [tvar(1)])], DB2),

    DB3.

%%%===================================================================
%%% Mapper Instances (Functor)
%%%===================================================================

%% @doc Add Mapper instances for type constructors
%%
%% Mapper provides:
%%   map : (a -> b) -> f a -> f b
%%
%% Note: Mapper is parameterized by a type constructor (kind * -> *)
-spec add_mapper_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_mapper_instances(DB0) ->
    %% instance Mapper Maybe
    DB1 = add_instance('Mapper', [tcon('Maybe')], DB0),

    %% instance Mapper List
    DB2 = add_instance('Mapper', [tcon(list)], DB1),

    %% instance Mapper (Either e) - maps over the Right value
    %% The type constructor is (Either e), partially applied
    EitherE = tapp(tcon('Either'), [tvar(0)]),
    DB3 = add_instance('Mapper', [EitherE], DB2),

    DB3.

%%%===================================================================
%%% Applicator Instances (Applicative)
%%%===================================================================

%% @doc Add Applicator instances
%%
%% Applicator extends Mapper and provides:
%%   wrap : a -> f a
%%   apply : f (a -> b) -> f a -> f b
-spec add_applicator_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_applicator_instances(DB0) ->
    %% instance Applicator Maybe
    DB1 = add_instance('Applicator', [tcon('Maybe')], DB0),

    %% instance Applicator List
    DB2 = add_instance('Applicator', [tcon(list)], DB1),

    %% instance Applicator (Either e)
    EitherE = tapp(tcon('Either'), [tvar(0)]),
    DB3 = add_instance('Applicator', [EitherE], DB2),

    DB3.

%%%===================================================================
%%% Chainable Instances (Monad bind)
%%%===================================================================

%% @doc Add Chainable instances
%%
%% Chainable extends Mapper and provides:
%%   chain : (a -> m b) -> m a -> m b
-spec add_chainable_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_chainable_instances(DB0) ->
    %% instance Chainable Maybe
    DB1 = add_instance('Chainable', [tcon('Maybe')], DB0),

    %% instance Chainable List
    DB2 = add_instance('Chainable', [tcon(list)], DB1),

    %% instance Chainable (Either e)
    EitherE = tapp(tcon('Either'), [tvar(0)]),
    DB3 = add_instance('Chainable', [EitherE], DB2),

    DB3.

%%%===================================================================
%%% Pipeline Instances (full Monad)
%%%===================================================================

%% @doc Add Pipeline instances
%%
%% Pipeline extends both Applicator and Chainable (full Monad).
%% Provides all monad operations: wrap, apply, map, chain
-spec add_pipeline_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_pipeline_instances(DB0) ->
    %% instance Pipeline Maybe
    DB1 = add_instance('Pipeline', [tcon('Maybe')], DB0),

    %% instance Pipeline List
    DB2 = add_instance('Pipeline', [tcon(list)], DB1),

    %% instance Pipeline (Either e)
    EitherE = tapp(tcon('Either'), [tvar(0)]),
    DB3 = add_instance('Pipeline', [EitherE], DB2),

    DB3.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

%% @doc Create a type constructor
-spec tcon(atom()) -> catena_types:ty().
tcon(Name) ->
    {tcon, Name}.

%% @doc Create a type variable
-spec tvar(integer()) -> catena_types:ty().
tvar(Id) ->
    {tvar, Id}.

%% @doc Create a type application
-spec tapp(catena_types:ty(), [catena_types:ty()]) -> catena_types:ty().
tapp(Con, Args) ->
    {tapp, Con, Args}.

%% @doc Helper to add an instance to the database
-spec add_instance(catena_constraint:trait_name(), [catena_types:ty()],
                   catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_instance(TraitName, TypeArgs, DB) ->
    Instance = catena_instance:make_instance(TraitName, TypeArgs),
    catena_instance:add_instance(Instance, DB).
