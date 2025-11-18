%%%-------------------------------------------------------------------
%%% @doc Built-in Trait Instances
%%%
%%% This module provides built-in trait instances for primitive types
%%% and standard type constructors. These instances are automatically
%%% available during type inference.
%%%
%%% Traits Supported:
%%% - Eq: Equality comparison (==, /=)
%%% - Ord: Ordering comparison (<, >, <=, >=)
%%% - Show: String representation
%%% - Functor: Mappable types (map)
%%% - Monad: Chainable types (bind, return)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_builtin_instances).

-export([
    builtin_instance_db/0,
    add_eq_instances/1,
    add_ord_instances/1,
    add_show_instances/1,
    add_functor_instances/1,
    add_monad_instances/1
]).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Get instance database with all built-in instances
%%
%% This creates a complete instance database containing instances for
%% all standard traits on all primitive types and common type constructors.
-spec builtin_instance_db() -> catena_instance:instance_db().
builtin_instance_db() ->
    DB0 = catena_instance:empty_instance_db(),
    DB1 = add_eq_instances(DB0),
    DB2 = add_ord_instances(DB1),
    DB3 = add_show_instances(DB2),
    DB4 = add_functor_instances(DB3),
    DB5 = add_monad_instances(DB4),
    DB5.

%%%===================================================================
%%% Eq Instances
%%%===================================================================

%% @doc Add Eq instances for primitive types
%%
%% Eq is the equality trait, supporting == and /= operations.
%% Instances are provided for: Int, Float, Bool, String, Atom
-spec add_eq_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_eq_instances(DB0) ->
    % Eq Int
    DB1 = add_instance('Eq', [catena_types:tcon(int)], DB0),

    % Eq Float
    DB2 = add_instance('Eq', [catena_types:tcon(float)], DB1),

    % Eq Bool
    DB3 = add_instance('Eq', [catena_types:tcon(bool)], DB2),

    % Eq String
    DB4 = add_instance('Eq', [catena_types:tcon(string)], DB3),

    % Eq Atom
    DB5 = add_instance('Eq', [catena_types:tcon(atom)], DB4),

    DB5.

%%%===================================================================
%%% Ord Instances
%%%===================================================================

%% @doc Add Ord instances for primitive types
%%
%% Ord is the ordering trait, supporting <, >, <=, >= operations.
%% Instances are provided for: Int, Float, String
%%
%% Note: Ord requires Eq as a superclass constraint
-spec add_ord_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_ord_instances(DB0) ->
    % Ord Int
    DB1 = add_instance('Ord', [catena_types:tcon(int)], DB0),

    % Ord Float
    DB2 = add_instance('Ord', [catena_types:tcon(float)], DB1),

    % Ord String
    DB3 = add_instance('Ord', [catena_types:tcon(string)], DB2),

    DB3.

%%%===================================================================
%%% Show Instances
%%%===================================================================

%% @doc Add Show instances for primitive types
%%
%% Show is the string representation trait.
%% Instances are provided for: Int, Float, Bool, String, Atom
-spec add_show_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_show_instances(DB0) ->
    % Show Int
    DB1 = add_instance('Show', [catena_types:tcon(int)], DB0),

    % Show Float
    DB2 = add_instance('Show', [catena_types:tcon(float)], DB1),

    % Show Bool
    DB3 = add_instance('Show', [catena_types:tcon(bool)], DB2),

    % Show String
    DB4 = add_instance('Show', [catena_types:tcon(string)], DB3),

    % Show Atom
    DB5 = add_instance('Show', [catena_types:tcon(atom)], DB4),

    DB5.

%%%===================================================================
%%% Functor Instances
%%%===================================================================

%% @doc Add Functor instances for type constructors
%%
%% Functor is the mappable trait, supporting map :: (a -> b) -> f a -> f b
%% Instances are provided for: List, Maybe, Result
-spec add_functor_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_functor_instances(DB0) ->
    % Functor List
    % instance Functor List where
    %   map :: (a -> b) -> List a -> List b
    DB1 = add_instance('Functor', [catena_types:tcon(list)], DB0),

    % Functor Maybe
    % instance Functor Maybe where
    %   map :: (a -> b) -> Maybe a -> Maybe b
    DB2 = add_instance('Functor', [catena_types:tcon('Maybe')], DB1),

    % Functor Result (parameterized by error type)
    % instance Functor (Result e) where
    %   map :: (a -> b) -> Result e a -> Result e b
    {ErrorVar, _} = catena_types:fresh_var(catena_infer_state:new()),
    ResultE = catena_types:tapp(catena_types:tcon(result), [ErrorVar]),
    DB3 = add_instance('Functor', [ResultE], DB2),

    DB3.

%%%===================================================================
%%% Monad Instances
%%%===================================================================

%% @doc Add Monad instances for type constructors
%%
%% Monad is the chainable trait, supporting:
%%   return :: a -> m a
%%   bind :: m a -> (a -> m b) -> m b
%%
%% Instances are provided for: List, Maybe, Result
%%
%% Note: Monad requires Functor as a superclass constraint
-spec add_monad_instances(catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_monad_instances(DB0) ->
    % Monad List
    % instance Monad List where
    %   return :: a -> List a
    %   bind :: List a -> (a -> List b) -> List b
    DB1 = add_instance('Monad', [catena_types:tcon(list)], DB0),

    % Monad Maybe
    % instance Monad Maybe where
    %   return :: a -> Maybe a
    %   bind :: Maybe a -> (a -> Maybe b) -> Maybe b
    DB2 = add_instance('Monad', [catena_types:tcon('Maybe')], DB1),

    % Monad Result (parameterized by error type)
    % instance Monad (Result e) where
    %   return :: a -> Result e a
    %   bind :: Result e a -> (a -> Result e b) -> Result e b
    {ErrorVar, _} = catena_types:fresh_var(catena_infer_state:new()),
    ResultE = catena_types:tapp(catena_types:tcon(result), [ErrorVar]),
    DB3 = add_instance('Monad', [ResultE], DB2),

    DB3.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

%% @doc Helper to add an instance to the database
-spec add_instance(catena_constraint:trait_name(), [catena_types:ty()],
                   catena_instance:instance_db()) ->
    catena_instance:instance_db().
add_instance(TraitName, TypeArgs, DB) ->
    Instance = catena_instance:make_instance(TraitName, TypeArgs),
    catena_instance:add_instance(Instance, DB).
