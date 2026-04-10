%%%-------------------------------------------------------------------
%%% @doc Test Worker for test_sup
%%% @end
%%%-------------------------------------------------------------------
-module(test_worker).
-behaviour(catena_gen_server).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    catena_gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(die, _From, State) ->
    {stop, crash, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
