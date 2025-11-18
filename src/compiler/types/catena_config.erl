%%%
%%% @doc Configuration Values for Type System
%%%
%%% Centralizes all configurable constants and limits for the
%%% type inference system to make them easier to tune and manage.
%%%
%%% @end
%%%
-module(catena_config).

-export([
    get_max_type_var_id/0,
    get_max_subst_depth/0,
    get_max_type_depth/0,
    get_max_substitution_size/0,
    get_max_environment_size/0
]).

%%====================================================================
%% Configuration Constants
%%====================================================================

%% Maximum type variable ID to prevent overflow attacks
-define(MAX_TYPE_VAR_ID, 1000000).

%% Maximum recursion depth for substitution operations
-define(MAX_SUBST_DEPTH, 500).

%% Maximum depth for type traversal (prevents stack overflow)
-define(MAX_TYPE_DEPTH, 100).

%% Maximum number of mappings in a substitution (prevents DoS)
-define(MAX_SUBSTITUTION_SIZE, 10000).

%% Maximum number of bindings in a type environment
-define(MAX_ENVIRONMENT_SIZE, 1000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get the maximum type variable ID
%% Used to prevent infinite type generation and overflow attacks
-spec get_max_type_var_id() -> pos_integer().
get_max_type_var_id() ->
    ?MAX_TYPE_VAR_ID.

%% @doc Get the maximum substitution depth
%% Used to prevent infinite recursion in substitution operations
-spec get_max_subst_depth() -> non_neg_integer().
get_max_subst_depth() ->
    ?MAX_SUBST_DEPTH.

%% @doc Get the maximum type depth
%% Used to prevent stack overflow in type traversal
-spec get_max_type_depth() -> non_neg_integer().
get_max_type_depth() ->
    ?MAX_TYPE_DEPTH.

%% @doc Get the maximum substitution size
%% Used to prevent DoS through complex substitutions
-spec get_max_substitution_size() -> non_neg_integer().
get_max_substitution_size() ->
    ?MAX_SUBSTITUTION_SIZE.

%% @doc Get the maximum environment size
%% Used to prevent unbounded environment growth
-spec get_max_environment_size() -> non_neg_integer().
get_max_environment_size() ->
    ?MAX_ENVIRONMENT_SIZE.