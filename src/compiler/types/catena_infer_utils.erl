%%%
%%% @doc Utility Functions for Type Inference
%%%
%%% Provides shared helper functions used across the inference
%%% modules to avoid code duplication and improve maintainability.
%%%
%%% @end
%%%
-module(catena_infer_utils).

-export([
    literal_type/1
]).

%%%===================================================================
%%% Type Definitions
%%%===================================================================

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Get the type of a literal value
%%
%% Maps literal AST nodes to their corresponding types.
%% Used by both pattern matching and expression inference.
%%
%% @param Lit The literal AST node
%% @returns The corresponding type constructor
%%
%% @example
%% ```
%% Type = catena_infer_utils:literal_type({int, 42}).
%% %% → {tcon, int}
%% ```
-spec literal_type(catena_ast:literal()) -> catena_types:type().
literal_type({int, _}) ->
    {tcon, int};
literal_type({float, _}) ->
    {tcon, float};
literal_type({bool, _}) ->
    {tcon, bool};
literal_type({string, _}) ->
    {tcon, string};
literal_type({atom, _}) ->
    {tcon, atom};
literal_type({unit}) ->
    {tcon, unit}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================