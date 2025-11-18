%%%-------------------------------------------------------------------
%%% @doc Type System Configuration and Limits
%%%
%%% This module provides configuration constants and limits for the
%%% type inference and constraint solving system. These limits protect
%%% against denial-of-service attacks and ensure bounded resource usage.
%%%
%%% Security Limits:
%%% - Constraint set size limits (prevent constraint explosion)
%%% - Instance database size limits (prevent instance pollution)
%%% - Resolution depth limits (prevent infinite recursion)
%%% - Type depth limits (prevent deeply nested types)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_type_config).

-export([
    max_constraint_set_size/0,
    max_instances_per_trait/0,
    max_resolution_depth/0,
    max_type_depth/0,
    check_constraint_set_size/1,
    check_instance_count/2,
    check_resolution_depth/1
]).

%%%===================================================================
%%% Configuration Constants
%%%===================================================================

%% @doc Maximum number of constraints in a constraint set
%%
%% Prevents DoS via constraint explosion:
%%   shape Evil a = (Eq a, Ord a, Show a, ...[1000 constraints]...) => T a
%%
%% Recommendation: 1000 constraints is more than any reasonable type needs
-spec max_constraint_set_size() -> pos_integer().
max_constraint_set_size() ->
    1000.

%% @doc Maximum number of instances per trait
%%
%% Prevents DoS via instance database pollution:
%%   instance Eq Int where ...
%%   instance Eq Float where ...
%%   ...[100000 instances]...
%%
%% Recommendation: 10000 instances per trait is extremely generous
-spec max_instances_per_trait() -> pos_integer().
max_instances_per_trait() ->
    10000.

%% @doc Maximum recursion depth for constraint resolution
%%
%% Prevents DoS via circular instance dependencies:
%%   instance Foo a => Bar a where ...
%%   instance Bar a => Foo a where ...
%%
%% Recommendation: 100 levels is more than sufficient for real code
-spec max_resolution_depth() -> pos_integer().
max_resolution_depth() ->
    100.

%% @doc Maximum type nesting depth
%%
%% Prevents DoS via deeply nested types:
%%   List (List (List (List ... (List Int) ...)))
%%
%% Recommendation: 100 levels allows reasonable composition
-spec max_type_depth() -> pos_integer().
max_type_depth() ->
    100.

%%%===================================================================
%%% Validation Functions
%%%===================================================================

%% @doc Check if constraint set size is within limits
%%
%% Returns ok if within limits, error if too large
-spec check_constraint_set_size(non_neg_integer()) ->
    ok | {error, {constraint_set_too_large, non_neg_integer(), pos_integer()}}.
check_constraint_set_size(Size) when Size =< 1000 ->
    ok;
check_constraint_set_size(Size) ->
    {error, {constraint_set_too_large, Size, max_constraint_set_size()}}.

%% @doc Check if instance count for a trait is within limits
%%
%% Returns ok if within limits, error if too many
-spec check_instance_count(atom(), non_neg_integer()) ->
    ok | {error, {too_many_instances, atom(), non_neg_integer(), pos_integer()}}.
check_instance_count(_TraitName, Count) when Count =< 10000 ->
    ok;
check_instance_count(TraitName, Count) ->
    {error, {too_many_instances, TraitName, Count, max_instances_per_trait()}}.

%% @doc Check if resolution depth is within limits
%%
%% Returns ok if within limits, error if too deep
-spec check_resolution_depth(non_neg_integer()) ->
    ok | {error, {resolution_depth_exceeded, non_neg_integer(), pos_integer()}}.
check_resolution_depth(Depth) when Depth =< 100 ->
    ok;
check_resolution_depth(Depth) ->
    {error, {resolution_depth_exceeded, Depth, max_resolution_depth()}}.
