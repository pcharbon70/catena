%%%-------------------------------------------------------------------
%%% @doc Test GenServer for catena_gen_server_tests
%%% @end
%%%-------------------------------------------------------------------
-module(test_gen_server).
-behaviour(catena_gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    value :: integer(),
    messages :: list()
}).

init(Args) ->
    InitialValue = proplists:get_value(value, Args, 0),
    {ok, #state{value = InitialValue, messages = []}}.

handle_call(get_value, _From, State) ->
    {reply, State#state.value, State};
handle_call({set_value, NewValue}, _From, State) ->
    {reply, ok, State#state{value = NewValue}};
handle_call({add, N}, _From, State) ->
    NewValue = State#state.value + N,
    {reply, NewValue, State#state{value = NewValue}};
handle_call(get_messages, _From, State) ->
    {reply, lists:reverse(State#state.messages), State};
handle_call(stop_with_reply, _From, State) ->
    {stop, normal, stopped, State};
handle_call(stop_no_reply, _From, State) ->
    {stop, normal, State};
handle_call({stop, Reason}, _From, State) ->
    {stop, Reason, ok, State}.

handle_cast({add, N}, State) ->
    {noreply, State#state{value = State#state.value + N}};
handle_cast({set_value, NewValue}, State) ->
    {noreply, State#state{value = NewValue}};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {noreply, State#state{messages = [Msg | State#state.messages]}}.

handle_info(Info, State) ->
    {noreply, State#state{messages = [Info | State#state.messages]}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
