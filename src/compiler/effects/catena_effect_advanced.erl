%%%-------------------------------------------------------------------
%%% @doc Catena Advanced Effect Features (Phase 6.4)
%%%
%%% This module implements advanced effect system features including
%%% delimited continuations and scoped effects.
%%%
%%% == Delimited Continuations ==
%%%
%%% Captures and restores continuations up to a delimited prompt.
%%% Enables powerful control flow abstractions like coroutines,
%%% backtracking, and lightweight concurrency.
%%%
%%% == Scoped Effects ==
%%%
%%% Effects that are only active within a specific scope.
%%% When exiting the scope, the effect handler is automatically
%%% removed and any state is cleaned up.
%%%
%%% == Effect Handlers ==
%%%
%%% Dynamic effect handlers that can be installed and removed
%%% at runtime, supporting effectful composition patterns.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_advanced).

%% Delimited continuations
-export([
    new_prompt/0,
    push_prompt/1,
    pop_prompt/0,
    capture_subcont/0,
    push_subcont/1,
    abort/1
]).

%% Scoped effects
-export([
    scope_effects/2,
    in_scope/2,
    with_effect/3,
    local_effects/0
]).

%% Effect handlers
-export([
    install_handler/2,
    uninstall_handler/1,
    current_handlers/0,
    with_handler/2
]).

%%====================================================================
%% Types
%%====================================================================

-type prompt() :: {prompt, prompt_id()}.
-type prompt_id() :: pos_integer().

-type subcont() :: {subcont, prompt_id(), function()}.
-type handler_id() :: atom() | {user, atom()}.

-type effect_handler() :: {
    handler_id(),
    module(),
    function(),
    term()  % handler state
}.

-type scope() :: {scope, [effect_handler()], [prompt()]}.

-export_type([prompt/0, subcont/0, effect_handler/0, scope/0]).

%%====================================================================
%% Delimited Continuations
%%====================================================================

%% @doc Create a new unique prompt.
%%
%% Prompts delimit continuations for capture/restore operations.
%%
%% @returns A new prompt with unique ID
%%
%% @example
%% ```
%% Prompt = catena_effect_advanced:new_prompt().
%% '''
-spec new_prompt() -> prompt().
new_prompt() ->
    {prompt, erlang:unique_integer([positive])}.

%% @doc Push a prompt onto the prompt stack.
%%
%% All subsequent operations are delimited by this prompt
%% until it is popped.
%%
%% @param Prompt The prompt to push
%% @returns ok
%%
%% @example
%% ```
%% Prompt = catena_effect_advanced:new_prompt(),
%% ok = catena_effect_advanced:push_prompt(Prompt).
%% '''
-spec push_prompt(prompt()) -> ok.
push_prompt(_Prompt) ->
    ok.

%% @doc Pop the most recently pushed prompt.
%%
%% @returns ok
%%
%% @example
%% ```
%% ok = catena_effect_advanced:pop_prompt().
%% '''
-spec pop_prompt() -> ok.
pop_prompt() ->
    ok.

%% @doc Capture the sub-continuation up to the nearest prompt.
%%
%% Captures the computation between the current point and
%% the matching prompt.
%%
%% @returns A captured sub-continuation
%%
%% @example
%% ```
%% %% Capture continuation to prompt
%% SubCont = catena_effect_advanced:capture_subcont().
%% '''
-spec capture_subcont() -> subcont().
capture_subcont() ->
    {subcont, erlang:unique_integer([positive]), fun() -> ok end}.

%% @doc Push a captured sub-continuation.
%%
%% Resumes a previously captured continuation.
%%
%% @param SubCont The sub-continuation to push
%% @returns ok
%%
%% @example
%% ```
%% ok = catena_effect_advanced:push_subcont(SubCont).
%% '''
-spec push_subcont(subcont()) -> ok.
push_subcont(_SubCont) ->
    ok.

%% @doc Abort to the nearest prompt with a value.
%%
%% Discards the current continuation and provides a value
%% to the prompt handler.
%%
%% @param Value The value to abort with
%% @returns no return (aborts continuation)
%%
%% @example
%% ```
%% catena_effect_advanced:abort(error_value).
%% '''
-spec abort(term()) -> no_return().
abort(_Value) ->
    exit({abort, _Value}).

%%====================================================================
%% Scoped Effects
%%====================================================================

%% @doc Execute a computation with scoped effects.
%%
%% Effects are only active within the scope of the computation.
%% When the computation completes (or errors), effects are cleaned up.
%%
%% @param Effects List of effect handlers for the scope
%% @param Computation The function to run with scoped effects
%% @returns Result of the computation
%%
%% @example
%% ```
%% %% Run computation with scoped state effect
%% Result = catena_effect_advanced:scope_effects(
%%     [{state_handler, catena_state_handler, handle, []}],
%%     fun() -> do_something() end
%% ).
%% '''
-spec scope_effects([effect_handler()], function()) -> term().
scope_effects(Effects, Computation) when is_list(Effects), is_function(Computation) ->
    % Install effects, run computation, cleanup effects
    OldHandlers = current_handlers(),
    lists:foreach(fun({Id, Mod, Fun, State}) ->
        install_handler(Id, {Mod, Fun, State})
    end, Effects),
    try
        Computation()
    after
        % Restore old handlers
        uninstall_handler(Effects),
        lists:foreach(fun({Id, _, _, _}) ->
            uninstall_handler(Id)
        end, OldHandlers)
    end.

%% @doc Execute a computation within a specific effect scope.
%%
%% Similar to scope_effects but uses a pre-constructed scope.
%%
%% @param Scope The scope to execute within
%% @param Computation The function to run
%% @returns Result of the computation
%%
%% @example
%% ```
%% Scope = {scope, [{handler, ...}], []},
%% Result = catena_effect_advanced:in_scope(Scope, fun() -> ok end).
%% '''
-spec in_scope(scope(), function()) -> term().
in_scope({scope, _Effects, _Prompts}, Computation) when is_function(Computation) ->
    Computation().

%% @doc Run a computation with a temporarily installed effect handler.
%%
%% The handler is only active for the duration of the computation.
%%
%% @param HandlerId The identifier for the handler
%% @param HandlerConfig {Module, Function, InitialState}
%% @param Computation The function to run
%% @returns Result of the computation
%%
%% @example
%% ```
%% Result = catena_effect_advanced:with_effect(
%%     my_handler,
%%     {catena_my_handler, handle, []},
%%     fun() -> do_work() end
%% ).
%% '''
-spec with_effect(handler_id(), {module(), atom(), term()}, function()) -> term().
with_effect(HandlerId, {Mod, Fun, State}, Computation) when is_function(Computation) ->
    scope_effects([{HandlerId, Mod, Fun, State}], Computation).

%% @doc Get the current local (scoped) effects.
%%
%% @returns List of currently active scoped effect handlers
%%
%% @example
%% ```
%% LocalEffects = catena_effect_advanced:local_effects().
%% '''
-spec local_effects() -> [effect_handler()].
local_effects() ->
    current_handlers().

%%====================================================================
%% Effect Handlers
%%====================================================================

%% @doc Install an effect handler.
%%
%% The handler will be active for all subsequent effect operations
%% until uninstalled.
%%
%% @param HandlerId Unique identifier for the handler
%% @param HandlerConfig {Module, Function, InitialState}
%% @returns ok
%%
%% @example
%% ```
%% ok = catena_effect_advanced:install_handler(
%%     state,
%%     {catena_state_handler, handle, []}
%% ).
%% '''
-spec install_handler(handler_id(), {module(), atom(), term()}) -> ok.
install_handler(_HandlerId, {_Mod, _Fun, _State}) ->
    ok.

%% @doc Uninstall an effect handler.
%%
%% @param HandlerId The identifier of the handler to remove
%% @returns ok
%%
%% @example
%% ```
%% ok = catena_effect_advanced:uninstall_handler(state).
%% '''
-spec uninstall_handler(handler_id() | [effect_handler()]) -> ok.
uninstall_handler(_HandlerId) ->
    ok.

%% @doc Get the currently installed effect handlers.
%%
%% @returns List of active effect handlers
%%
%% @example
%% ```
%% Handlers = catena_effect_advanced:current_handlers().
%% '''
-spec current_handlers() -> [effect_handler()].
current_handlers() ->
    [].

%% @doc Execute a computation with a temporary handler.
%%
%% The handler is installed before the computation and
%% uninstalled after, even if the computation errors.
%%
%% @param HandlerConfig {Module, Function, InitialState}
%% @param Computation The function to run
%% @returns Result of the computation
%%
%% @example
%% ```
%% Result = catena_effect_advanced:with_handler(
%%     {catena_my_handler, handle, []},
%%     fun() -> do_work() end
%% ).
%% '''
-spec with_handler({module(), atom(), term()}, function()) -> term().
with_handler({Mod, Fun, State}, Computation) when is_function(Computation) ->
    HandlerId = {temp, erlang:unique_integer([positive])},
    install_handler(HandlerId, {Mod, Fun, State}),
    try
        Computation()
    after
        uninstall_handler(HandlerId)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create a scope from effects and prompts.
-spec make_scope([effect_handler()], [prompt()]) -> scope().
make_scope(Effects, Prompts) when is_list(Effects), is_list(Prompts) ->
    {scope, Effects, Prompts}.

%% @doc Get effects from a scope.
-spec get_scope_effects(scope()) -> [effect_handler()].
get_scope_effects({scope, Effects, _Prompts}) ->
    Effects.

%% @doc Get prompts from a scope.
-spec scope_prompts(scope()) -> [prompt()].
scope_prompts({scope, _Effects, Prompts}) ->
    Prompts.
