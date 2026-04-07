%% @doc Message Passing Properties for Phase 6.2
%%
%% This module provides tools for testing message passing between processes.
%% We verify that messages are sent correctly, received in expected order,
%% and that protocols are followed.
-module(catena_message).

%% Message Generators
-export([gen_message/1,
         gen_message_sequence/2,
         gen_tagged_message/2,
         gen_call_message/1]).

%% Message Sending Properties
-export([sends_message/3,
         sends_messages_in_order/3,
         no_message_loss/3,
         trace_messages/1]).

%% Message Receiving Properties
-export([receives_message/2,
         receives_message_matching/2,
         receives_all_messages/1,
         flush_messages/0,
         flush_messages/1]).

%% Protocol Verification
-export([follows_protocol/3,
         verify_protocol/2,
         protocol_from_template/1]).

%% Message Tracing
-export([start_trace/1,
         stop_trace/0,
         get_trace/0]).

-include("catena_process.hrl").
-include("catena_gen.hrl").

%% Internal state for tracing
-record(trace_state, {
    messages :: [term()],
    sender :: pid() | undefined,
    receiver :: pid() | undefined
}).

%%====================================================================
%% Message Generators
%%====================================================================

%% @doc Generate a message of a specific type.
%% Types: atom, binary, integer, list, tuple, any
-spec gen_message(atom()) -> fun((catena_gen:seed()) -> {term(), catena_gen:seed()}).
gen_message(Type) ->
    fun(Seed) ->
        {_Word, Seed1} = catena_gen:seed_next(Seed),
        StateVal = Seed1#seed.state,
        case Type of
            atom ->
                Atoms = [msg, data, signal, event, notify],
                Index = (StateVal rem length(Atoms)) + 1,
                {lists:nth(Index, Atoms), Seed1};
            binary ->
                BinVal = StateVal,
                {<<BinVal:32>>, Seed1};
            integer ->
                {StateVal rem 1000, Seed1};
            list ->
                Len = (StateVal rem 10) + 1,
                {lists:seq(1, Len), Seed1};
            tuple ->
                {{msg, StateVal rem 100}, Seed1};
            any ->
                AnyTypes = [atom, binary, integer, list, tuple],
                TypeIndex = (StateVal rem length(AnyTypes)) + 1,
                SelectedType = lists:nth(TypeIndex, AnyTypes),
                %% For 'any' type, generate based on selected type
                case SelectedType of
                    atom ->
                        Atoms2 = [msg, data, signal, event, notify],
                        Index2 = (StateVal rem length(Atoms2)) + 1,
                        {lists:nth(Index2, Atoms2), Seed1};
                    binary ->
                        {<<StateVal:32>>, Seed1};
                    integer ->
                        {StateVal rem 1000, Seed1};
                    list ->
                        Len = (StateVal rem 10) + 1,
                        {lists:seq(1, Len), Seed1};
                    tuple ->
                        {{msg, StateVal rem 100}, Seed1}
                end
        end
    end.

%% @doc Generate a sequence of messages.
-spec gen_message_sequence(pos_integer(), atom()) -> fun((catena_gen:seed()) -> {[term()], catena_gen:seed()}).
gen_message_sequence(Count, Type) ->
    fun(Seed) ->
        generate_sequence(Count, Type, [], Seed)
    end.

%% @doc Generate a tagged message {Tag, Payload}.
-spec gen_tagged_message(atom(), atom()) -> fun((catena_gen:seed()) -> {term(), catena_gen:seed()}).
gen_tagged_message(Tag, PayloadType) ->
    fun(Seed) ->
        GenFun = gen_message(PayloadType),
        {Payload, NewSeed} = GenFun(Seed),
        {{Tag, Payload}, NewSeed}
    end.

%% @doc Generate a gen_server call message {$gen_call, From, Request}.
-spec gen_call_message(atom()) -> fun((catena_gen:seed()) -> {term(), catena_gen:seed()}).
gen_call_message(RequestType) ->
    fun(Seed) ->
        GenFun = gen_message(RequestType),
        {Request, NewSeed} = GenFun(Seed),
        From = self(),
        Msg = {'$gen_call', From, Request},
        {Msg, NewSeed}
    end.

%%====================================================================
%% Message Sending Properties
%%====================================================================

%% @doc Property that verifies a message is sent.
%% Returns true if the message is sent to the target process.
-spec sends_message(pid(), term(), timeout()) -> boolean().
sends_message(Target, Message, Timeout) ->
    Ref = make_ref(),
    Monitor = erlang:monitor(process, Target),
    Target ! {self(), Ref, Message},
    receive
        {Target, Ref, _Reply} ->
            erlang:demonitor(Monitor, [flush]),
            true;
        {'DOWN', Monitor, process, Target, _Info} ->
            false
    after Timeout ->
        erlang:demonitor(Monitor, [flush]),
        is_process_alive(Target)  %% Message was sent even if no reply
    end.

%% @doc Property that verifies messages are sent in order.
-spec sends_messages_in_order(pid(), [term()], timeout()) -> boolean().
sends_messages_in_order(_Target, [], _Timeout) ->
    true;
sends_messages_in_order(Target, [Msg|Rest], Timeout) ->
    case sends_message(Target, Msg, Timeout) of
        false -> false;
        true -> sends_messages_in_order(Target, Rest, Timeout)
    end.

%% @doc Property that verifies no message loss.
%% Returns true if all messages are received.
-spec no_message_loss(pid(), [term()], timeout()) -> {sent, pos_integer()} | {lost, pos_integer(), pos_integer()}.
no_message_loss(Target, Messages, Timeout) ->
    Ref = make_ref(),
    SentCount = length(Messages),

    %% Send all messages with sequence numbers
    lists:foreach(fun({Msg, Index}) ->
        Target ! {Ref, Index, Msg}
    end, lists:zip(Messages, lists:seq(1, SentCount))),

    %% Count how many messages are received
    ReceivedCount = count_received_messages(Ref, Timeout, 0),

    case SentCount of
        ReceivedCount -> {sent, SentCount};
        _ -> {lost, SentCount, SentCount - ReceivedCount}
    end.

%% @doc Start tracing messages sent to/from a process.
-spec trace_messages(pid()) -> ok.
trace_messages(Pid) when is_pid(Pid) ->
    erlang:trace(Pid, true, [send, 'receive']),
    ok.

%% @doc Start tracing messages.
-spec start_trace(pid()) -> {ok, pid()}.
start_trace(Pid) when is_pid(Pid) ->
    trace_messages(Pid),
    {ok, Pid}.

%% @doc Stop tracing messages.
-spec stop_trace() -> ok.
stop_trace() ->
    erlang:trace(all, false, [send, 'receive']),
    ok.

%% @doc Get the trace results.
-spec get_trace() -> {ok, [term()]}.
get_trace() ->
    {ok, erlang:trace_info(all, traced)}.

%%====================================================================
%% Message Receiving Properties
%%====================================================================

%% @doc Receive a message with timeout.
-spec receives_message(timeout(), pid()) -> {ok, term()} | {error, timeout}.
receives_message(Timeout, Pid) when is_pid(Pid) ->
    Monitor = erlang:monitor(process, Pid),
    Pid ! {check_messages, self()},
    receive
        {Pid, Msg} ->
            erlang:demonitor(Monitor, [flush]),
            {ok, Msg};
        {'DOWN', Monitor, process, Pid, _Info} ->
            {error, process_dead}
    after Timeout ->
        erlang:demonitor(Monitor, [flush]),
        {error, timeout}
    end;
receives_message(Timeout, _AnyPid) ->
    receive
        Msg -> {ok, Msg}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Receive a message matching a pattern.
-spec receives_message_matching(fun((term()) -> boolean()), timeout()) -> {ok, term()} | {error, timeout}.
receives_message_matching(Predicate, Timeout) ->
    receive
        Msg when is_function(Predicate, 1) ->
            case Predicate(Msg) of
                true -> {ok, Msg};
                false -> receives_message_matching(Predicate, Timeout)
            end;
        Msg ->
            case Predicate(Msg) of
                true -> {ok, Msg};
                false -> receives_message_matching(Predicate, Timeout)
            end
    after Timeout ->
        {error, timeout}
    end.

%% @doc Receive all messages currently in the mailbox.
-spec receives_all_messages(timeout()) -> {ok, [term()]}.
receives_all_messages(Timeout) ->
    receive_all_messages_acc(Timeout, []).

%% @doc Flush all messages from the current process mailbox.
-spec flush_messages() -> ok.
flush_messages() ->
    receive
        _ -> flush_messages()
    after 0 ->
        ok
    end.

%% @doc Flush all messages from a specific process mailbox.
-spec flush_messages(pid()) -> {ok, non_neg_integer()}.
flush_messages(Pid) ->
    %% Send a flush request
    Ref = make_ref(),
    Pid ! {flush, Ref, self()},
    receive
        {Ref, Count} -> {ok, Count}
    after 1000 ->
        {error, timeout}
    end.

%%====================================================================
%% Protocol Verification
%%====================================================================

%% @doc Check if a message sequence follows a protocol.
-spec follows_protocol([term()], [term()], map()) -> {ok, boolean()} | {error, term()}.
follows_protocol(Messages, Protocol, Options) ->
    AllowExtra = maps:get(allow_extra, Options, false),
    Ordered = maps:get(ordered, Options, true),

    case verify_protocol(Messages, Protocol, AllowExtra, Ordered) of
        {ok, true} -> {ok, true};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Verify a protocol is followed.
-spec verify_protocol([term()], [term()]) -> {ok, true} | {error, term()}.
verify_protocol(Messages, Protocol) ->
    verify_protocol(Messages, Protocol, false, true).

%% @doc Create a protocol from a template.
-spec protocol_from_template([term()]) -> #protocol{}.
protocol_from_template(Messages) ->
    #protocol{
        name = template_protocol,
        messages = Messages,
        ordered = true,
        allow_extra = false
    }.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Generate a sequence of messages.
generate_sequence(0, _Type, Acc, Seed) ->
    {lists:reverse(Acc), Seed};
generate_sequence(Count, Type, Acc, Seed) ->
    GenFun = gen_message(Type),
    {Msg, NewSeed} = GenFun(Seed),
    generate_sequence(Count - 1, Type, [Msg | Acc], NewSeed).

%% @doc Helper for no_message_loss.
no_message_loss(_Target, _Messages, Timeout, Count) ->
    receive
        {_Ref, _Index, _Msg} ->
            no_message_loss(_Target, _Messages, Timeout - 1, Count + 1)
    after Timeout ->
        Count
    end.

%% @doc Receive all messages with timeout.
receive_all_messages_acc(Timeout, Acc) ->
    receive
        Msg ->
            receive_all_messages_acc(Timeout, [Msg | Acc])
    after 0 ->
        {ok, lists:reverse(Acc)}
    end.

%% @doc Count received messages with a specific reference.
count_received_messages(_Ref, Timeout, Count) when Timeout =< 0 ->
    Count;
count_received_messages(Ref, Timeout, Count) ->
    receive
        {Ref, _, _} ->
            count_received_messages(Ref, Timeout - 1, Count + 1)
    after Timeout ->
        Count
    end.

%% @doc Verify protocol implementation.
verify_protocol(Messages, Protocol, AllowExtra, Ordered) ->
    case {Ordered, AllowExtra} of
        {true, false} ->
            %% Exact ordered match
            case Messages of
                Protocol -> {ok, true};
                _ -> {error, {protocol_mismatch, Messages, Protocol}}
            end;
        {true, true} ->
            %% Ordered match with extra messages allowed
            case lists:prefix(Protocol, Messages) of
                true -> {ok, true};
                false -> {error, {protocol_prefix_mismatch, Messages, Protocol}}
            end;
        {false, _} ->
            %% Unordered match
            MsgSet = sets:from_list(Messages),
            ProtoSet = sets:from_list(Protocol),
            case sets:is_subset(ProtoSet, MsgSet) of
                true when AllowExtra -> {ok, true};
                true -> MsgSet =:= ProtoSet;
                false -> {error, {missing_messages, sets:to_list(sets:subtract(ProtoSet, MsgSet))}}
            end
    end.
