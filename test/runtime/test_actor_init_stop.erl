%%%-------------------------------------------------------------------
%%% @doc Test actor for init/stop scenarios in catena_actor_tests
%%% @end
%%%-------------------------------------------------------------------
-module(test_actor_init_stop).
-behaviour(catena_actor).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

init(fail) ->
    {stop, initialization_failed};
init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
