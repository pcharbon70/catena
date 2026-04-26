%%%-------------------------------------------------------------------
%%% @doc Catena Handler Depth Selection (Phase 9.4)
%%%
%%% This module provides mechanisms for selecting handler depth and
%%% converting between deep and shallow handlers.
%%%
%%% == Depth Selection ==
%%%
%%% Handlers can be created as either deep or shallow, and can be
%%% converted between modes. Selection APIs make it easy to choose
%%% the appropriate depth for a given use case.
%%%
%%% == Conversion ==
%%%
%%% Deep handlers can be converted to shallow (restricting scope) and
%%% shallow handlers can be converted to deep (expanding scope).
%%%
%%% == Mixed Depth ==
%%%
%%% Multiple handlers at different depths can coexist. Precedence rules
%%% determine which handler handles an operation when there are
%%% multiple candidates.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_depth_selection).

%% Depth selection API
-export([
    with_deep_handler/3,
    with_shallow_handler/3,
    with_handler/4,
    select_depth/1
]).

%% Depth conversion
-export([
    to_deep/1,
    to_shallow/1,
    convert_depth/2,
    can_convert/2
]).

%% Mixed depth handlers
-export([
    mixed_handler_scope/2,
    depth_precedence/2,
    depth_precedence/3,
    resolve_handler_conflict/3
]).

%% Depth introspection
-export([
    handler_depth/1,
    effective_depth/2,
    available_depths/1
]).

%% Utility functions
-export([
    is_deep_handler/1,
    is_shallow_handler/1,
    handler_spec/3
]).

%%====================================================================
%% Types
%%====================================================================

-type handler_depth() :: catena_handler_depth:handler_depth().
-type handler() :: #{
    type => deep | shallow,
    effect => atom(),
    handler => function(),
    depth => handler_depth(),
    metadata => map()
}.
-type operation() :: {atom(), atom(), [term()]}.

-export_type([
    handler/0
]).

%%====================================================================
%% Depth Selection API
%%====================================================================

%% @doc Execute a function with a deep handler.
%% Convenience wrapper around catena_deep_handler:with_deep_handler.
-spec with_deep_handler(atom(), function(), function()) -> any().
with_deep_handler(EffectName, HandlerFun, UserFun) ->
    catena_deep_handler:with_deep_handler(EffectName, HandlerFun, UserFun).

%% @doc Execute a function with a shallow handler.
%% Convenience wrapper around catena_shallow_handler:with_shallow_handler.
-spec with_shallow_handler(atom(), function(), function()) -> any().
with_shallow_handler(EffectName, HandlerFun, UserFun) ->
    catena_shallow_handler:with_shallow_handler(EffectName, HandlerFun, UserFun).

%% @doc Execute a function with a handler at specified depth.
-spec with_handler(atom(), handler_depth(), function(), function()) -> any().
with_handler(EffectName, deep, HandlerFun, UserFun) ->
    with_deep_handler(EffectName, HandlerFun, UserFun);
with_handler(EffectName, shallow, HandlerFun, UserFun) ->
    with_shallow_handler(EffectName, HandlerFun, UserFun);
with_handler(EffectName, Depth, HandlerFun, UserFun) ->
    case catena_handler_depth:normalize_depth(Depth) of
        {ok, deep} -> with_deep_handler(EffectName, HandlerFun, UserFun);
        {ok, shallow} -> with_shallow_handler(EffectName, HandlerFun, UserFun);
        {error, Reason} -> error(Reason)
    end.

%% @doc Select a depth based on configuration or defaults.
%% Returns deep if no preference specified.
-spec select_depth(proplists:proplist()) -> handler_depth().
select_depth(Options) ->
    case catena_handler_depth:normalize_depth(Options) of
        {ok, Depth} -> Depth;
        {error, _} -> deep
    end.

%%====================================================================
%% Depth Conversion
%%====================================================================

%% @doc Convert any handler to deep.
%% For deep handlers, returns unchanged. For shallow, creates deep equivalent.
-spec to_deep(handler()) -> handler().
to_deep(#{type := deep} = Handler) -> Handler;
to_deep(#{type := shallow} = Handler) ->
    Handler#{
        type => deep,
        depth => deep,
        metadata => conversion_metadata(Handler, deep)
    }.

%% @doc Convert any handler to shallow.
%% For shallow handlers, returns unchanged. For deep, creates shallow equivalent.
-spec to_shallow(handler()) -> handler().
to_shallow(#{type := shallow} = Handler) -> Handler;
to_shallow(#{type := deep} = Handler) ->
    Handler#{
        type => shallow,
        depth => shallow,
        metadata => conversion_metadata(Handler, shallow)
    }.

%% @doc Convert a handler to a specific depth.
-spec convert_depth(handler(), handler_depth()) -> handler().
convert_depth(Handler, deep) -> to_deep(Handler);
convert_depth(Handler, shallow) -> to_shallow(Handler).

%% @doc Check if a handler can be converted to a specific depth.
%% All handlers can be converted, but this function validates
%% that the conversion is semantically meaningful.
-spec can_convert(handler(), handler_depth()) -> boolean().
can_convert(#{type := CurrentType}, TargetDepth) ->
    case catena_handler_depth:validate_depth(TargetDepth) of
        {ok, _} -> CurrentType =:= TargetDepth orelse true;
        {error, _} -> false
    end.

%%====================================================================
%% Mixed Depth Handlers
%%====================================================================

%% @doc Create a scope with both deep and shallow handlers.
%% The scope determines which handlers take precedence.
-spec mixed_handler_scope([handler()], proplists:proplist()) -> map().
mixed_handler_scope(Handlers, Options) ->
    Precedence = normalize_precedence(Options),
    #{
        handlers => [normalize_handler(Handler) || Handler <- Handlers],
        precedence => Precedence,
        options => Options
    }.

%% @doc Determine precedence between handlers at different depths.
%% Returns which handler should handle an operation.
-spec depth_precedence(handler(), handler()) ->
    {deep, handler()} | {shallow, handler()} | {equal, both}.
depth_precedence(H1, H2) ->
    depth_precedence(H1, H2, deep_first).

%% @doc Determine precedence using an explicit precedence policy.
-spec depth_precedence(handler(), handler(), deep_first | shallow_first) ->
    {deep, handler()} | {shallow, handler()} | {equal, both}.
depth_precedence(#{type := deep} = H1, #{type := shallow} = _H2, deep_first) ->
    {deep, H1};
depth_precedence(#{type := deep} = _H1, #{type := shallow} = H2, shallow_first) ->
    {shallow, H2};
depth_precedence(#{type := shallow} = H1, #{type := deep} = H2, Precedence) ->
    depth_precedence(H2, H1, Precedence);
depth_precedence(#{type := Type} = _H1, #{type := Type} = _H2, _Precedence) ->
    {equal, both}.

%% @doc Resolve a conflict when multiple handlers could handle an operation.
-spec resolve_handler_conflict(operation(), [handler()], proplists:proplist()) ->
    {ok, handler()} | {unhandled, operation()}.
resolve_handler_conflict(_Operation, [], _Options) ->
    {unhandled, no_handler};
resolve_handler_conflict(Operation, Handlers, Options) ->
    % Find handlers matching the operation's effect
    {EffectName, _Op, _Args} = Operation,
    MatchingHandlers = [
        normalize_handler(H) || H <- Handlers,
        maps:get(effect, H, undefined) =:= EffectName
    ],
    case MatchingHandlers of
        [] -> {unhandled, no_matching_handler};
        [Single] -> {ok, Single};
        Multiple ->
            % Use precedence rules
            Precedence = normalize_precedence(Options),
            select_by_precedence(Multiple, Precedence)
    end.

%% @private Select handler based on precedence.
select_by_precedence(Handlers, deep_first) ->
    % Prefer deep handlers
    DeepHandlers = [H || H <- Handlers, maps:get(type, H) =:= deep],
    ShallowHandlers = [H || H <- Handlers, maps:get(type, H) =:= shallow],
    case DeepHandlers of
        [H | _] -> {ok, H};
        [] -> case ShallowHandlers of
            [H | _] -> {ok, H};
            [] -> {ok, hd(Handlers)}
        end
    end;
select_by_precedence(Handlers, shallow_first) ->
    % Prefer shallow handlers
    ShallowHandlers = [H || H <- Handlers, maps:get(type, H) =:= shallow],
    DeepHandlers = [H || H <- Handlers, maps:get(type, H) =:= deep],
    case ShallowHandlers of
        [H | _] -> {ok, H};
        [] -> case DeepHandlers of
            [H | _] -> {ok, H};
            [] -> {ok, hd(Handlers)}
        end
    end.

%%====================================================================
%% Depth Introspection
%%====================================================================

%% @doc Get the depth type of a handler.
-spec handler_depth(handler()) -> handler_depth().
handler_depth(#{type := _Type, depth := Depth}) ->
    Depth;
handler_depth(#{type := Type}) when Type =:= deep -> deep;
handler_depth(#{type := Type}) when Type =:= shallow -> shallow.

%% @doc Get the effective depth for a handler in a specific context.
%% This considers both the handler's depth and the surrounding scope.
-spec effective_depth(handler(), map()) -> handler_depth().
effective_depth(Handler, _Context) ->
    normalize_effective_depth(Handler).

%% @doc Get available depths for a handler type.
%% Returns which depths are valid for the given handler.
-spec available_depths(handler()) -> [handler_depth()].
available_depths(_Handler) -> [deep, shallow].

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Check if a handler is deep.
-spec is_deep_handler(handler()) -> boolean().
is_deep_handler(#{type := deep}) -> true;
is_deep_handler(_) -> false.

%% @doc Check if a handler is shallow.
-spec is_shallow_handler(handler()) -> boolean().
is_shallow_handler(#{type := shallow}) -> true;
is_shallow_handler(_) -> false.

%% @doc Create a handler specification with depth.
-spec handler_spec(atom(), handler_depth(), function()) -> handler().
handler_spec(Effect, Depth, HandlerFun) ->
    {ok, NormalizedDepth} = catena_handler_depth:normalize_depth(Depth),
    Type = case NormalizedDepth of
        deep -> deep;
        shallow -> shallow
    end,
    #{
        type => Type,
        effect => Effect,
        handler => HandlerFun,
        depth => NormalizedDepth,
        metadata => #{created_by => depth_selection}
    }.

%%====================================================================
%% Internal Helpers
%%====================================================================

normalize_handler(#{type := _, effect := _, handler := _} = Handler) ->
    NormalizedDepth = normalize_effective_depth(Handler),
    Handler#{
        depth => NormalizedDepth,
        metadata => maps:get(metadata, Handler, #{})
    };
normalize_handler(Handler) ->
    Handler.

normalize_effective_depth(Handler) ->
    case maps:find(depth, Handler) of
        {ok, Depth} ->
            case catena_handler_depth:normalize_depth(Depth) of
                {ok, NormalizedDepth} -> NormalizedDepth;
                {error, _} -> maps:get(type, Handler)
            end;
        error ->
            maps:get(type, Handler)
    end.

normalize_precedence(Options) ->
    case proplists:get_value(precedence, Options, deep_first) of
        shallow_first -> shallow_first;
        _ -> deep_first
    end.

conversion_metadata(Handler, TargetDepth) ->
    Existing = maps:get(metadata, Handler, #{}),
    Existing#{
        converted_from => maps:get(type, Handler),
        converted_to => TargetDepth
    }.
