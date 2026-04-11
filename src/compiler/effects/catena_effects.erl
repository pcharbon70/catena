%%%-------------------------------------------------------------------
%%% @doc Catena Expanded Effect Library (Phase 6.2)
%%%
%%% This module implements a comprehensive effect library beyond the basic
%%% IO and Process effects from Phase 2. The expanded library includes:
%%%
%%% == State Effect ==
%%%
%%% Provides mutable state without actual side effects. Operations:
%%% - get : Unit -> s / {State s}
%%% - put : s -> Unit / {State s}
%%%
%%% == Reader Effect ==
%%%
%%% Provides read-only access to environment/configuration. Operation:
%%% - ask : Unit -> r / {Reader r}
%%%
%%% == Writer Effect ==
%%%
%%% Accumulates output values (logs, traces). Operation:
%%% - tell : w -> Unit / {Writer w} where w is a monoid
%%%
%%% == Async Effect ==
%%%
%%% Handles asynchronous computation with futures/promises.
%%% Operations:
%%% - async : (() -> a) -> Future a / {Async}
%%% - await : Future a -> a / {Async}
%%%
%%% == Error Effect ==
%%%
%%% Provides typed exceptions. Operations:
%%% - throw : Error -> a / {Error Error}
%%% - catch : (Error -> a) -> a / {Error} -> a
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effects).

%% Effect definitions
-export([
    % State effect
    state_get/0,
    state_put/1,
    state_modify/1,
    state_get_and_put/1,

    % Reader effect
    reader_ask/0,
    reader_local/1,
    reader_ask_local/1,

    % Writer effect
    writer_tell/1,
    writer_listen/1,
    writer_pass/1,

    % Async effect
    async_spawn/1,
    async_await/1,
    async_yield/1,

    % Error effect
    error_throw/1,
    error_catch/2
]).

%% Effect constructors
-export([
    state/1,
    reader/1,
    writer/1,
    async/0,
    error/1
]).

%% Effect combinators
-export([
    combine_effects/1,
    run_with/2,
    handle_pure/1
]).

%%====================================================================
%% Types
%%====================================================================

-type state_type() :: atom().
-type reader_env() :: term().
-type writer_monoid() :: atom().
-type async_result() :: term().
-type error_type() :: atom().

-type effect() ::
    {state, state_type()} |
    {reader, reader_env()} |
    {writer, writer_monoid()} |
    {async} |
    {error, error_type()}.

-export_type([effect/0]).

%%====================================================================
%% State Effect
%%====================================================================

%% @doc Perform a state get operation.
%% Returns the current state value.
-spec state_get() -> term().
state_get() ->
    {effect, {state, get}}.

%% @doc Perform a state put operation.
%% Sets the state to a new value.
-spec state_put(term()) -> term().
state_put(Value) ->
    {effect, {state, {put, Value}}}.

%% @doc Modify the state using a function.
-spec state_modify(function()) -> term().
state_modify(Fun) when is_function(Fun) ->
    {effect, {state, {modify, Fun}}}.

%% @doc Get and put in one operation.
-spec state_get_and_put(function()) -> term().
state_get_and_put(Fun) when is_function(Fun) ->
    {effect, {state, {get_and_put, Fun}}}.

%% @doc Create a state effect type.
-spec state(state_type()) -> effect().
state(Type) ->
    {effect, {state, Type}}.

%%====================================================================
%% Reader Effect
%%====================================================================

%% @doc Ask for the reader environment.
-spec reader_ask() -> term().
reader_ask() ->
    {effect, {reader, ask}}.

%% @doc Run a computation with a modified local environment.
-spec reader_local(function()) -> term().
reader_local(Fun) when is_function(Fun) ->
    {effect, {reader, {local, Fun}}}.

%% @doc Ask for the environment, then run a function with it.
-spec reader_ask_local(function()) -> term().
reader_ask_local(Fun) when is_function(Fun) ->
    {effect, {reader, {ask_local, Fun}}}.

%% @doc Create a reader effect type.
-spec reader(reader_env()) -> effect().
reader(Env) ->
    {effect, {reader, Env}}.

%%====================================================================
%% Writer Effect
%%====================================================================

%% @doc Tell a value to the writer monoid.
-spec writer_tell(term()) -> term().
writer_tell(Value) ->
    {effect, {writer, {tell, Value}}}.

%% @doc Listen to output during a computation.
-spec writer_listen(function()) -> term().
writer_listen(Fun) when is_function(Fun) ->
    {effect, {writer, {listen, Fun}}}.

%% @doc Pass through without accumulating output.
-spec writer_pass(term()) -> term().
writer_pass(Value) ->
    {effect, {writer, {pass, Value}}}.

%% @doc Create a writer effect type.
-spec writer(writer_monoid()) -> effect().
writer(Monoid) ->
    {effect, {writer, Monoid}}.

%%====================================================================
%% Async Effect
%%====================================================================

%% @doc Spawn an asynchronous computation.
-spec async_spawn(function()) -> term().
async_spawn(Fun) when is_function(Fun) ->
    {effect, {async, {spawn, Fun}}}.

%% @doc Await an async computation result.
-spec async_await(term()) -> term().
async_await(Future) ->
    {effect, {async, {await, Future}}}.

%% @doc Yield control during async computation.
-spec async_yield(term()) -> term().
async_yield(Value) ->
    {effect, {async, {yield, Value}}}.

%% @doc Create an async effect type.
-spec async() -> effect().
async() ->
    {effect, {async}}.

%%====================================================================
%% Error Effect
%%====================================================================

%% @doc Throw an error.
-spec error_throw(term()) -> no_return().
error_throw(Error) ->
    {effect, {error, {throw, Error}}}.

%% @doc Catch an error, running handler on success.
-spec error_catch(function(), function()) -> term().
error_catch(ErrorFun, HandlerFun) when is_function(ErrorFun), is_function(HandlerFun) ->
    {effect, {error, {'catch', ErrorFun, HandlerFun}}}.

%% @doc Create an error effect type.
-spec error(error_type()) -> effect().
error(Type) ->
    {effect, {error, Type}}.

%%====================================================================
%% Effect Combinators
%%====================================================================

%% @doc Combine multiple effect types.
-spec combine_effects([effect()]) -> effect().
combine_effects(Effects) when is_list(Effects) ->
    {effect, {combined, Effects}}.

%% @doc Run a computation with a specific effect handler.
-spec run_with(effect(), function()) -> term().
run_with(Effect, Computation) when is_function(Computation) ->
    {effect, {run_with, Effect, Computation}}.

%% @doc Handle effects in a pure way (for testing).
-spec handle_pure(function()) -> term().
handle_pure(Computation) when is_function(Computation) ->
    {effect, {handle_pure, Computation}}.
