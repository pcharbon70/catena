%%%-------------------------------------------------------------------
%%% @doc A crashable child process for testing supervisor behavior
%%%
%%% This module provides a simple child process that can be crashed
%%% for testing supervisor restart strategies.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(crashable_child).
-behavior(gen_server).

%% API
-export([start_link/0, crash/1, get_count/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

crash(Pid) ->
    gen_server:call(Pid, crash).

get_count(Pid) ->
    gen_server:call(Pid, get_count).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
    {ok, #{count => 0}}.

handle_call(crash, _From, State) ->
    exit(simulated_crash),
    {reply, ok, State};
handle_call(get_count, _From, #{count := Count} = State) ->
    {reply, Count, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
