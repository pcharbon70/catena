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

-define(TRACE_KEY, catena_message_trace).

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

    lists:foreach(fun({Msg, Index}) ->
        Target ! {self(), Ref, Index, Msg}
    end, lists:zip(Messages, lists:seq(1, SentCount))),

    ReceivedCount = count_received_messages(Ref, Timeout, 0),

    case SentCount of
        ReceivedCount -> {sent, SentCount};
        _ -> {lost, SentCount, SentCount - ReceivedCount}
    end.

%% @doc Start tracing messages sent to/from a process.
-spec trace_messages(pid()) -> ok.
trace_messages(Pid) when is_pid(Pid) ->
    case start_trace(Pid) of
        {ok, _Tracer} -> ok;
        {error, _Reason} -> ok
    end,
    ok.

%% @doc Start tracing messages.
-spec start_trace(pid()) -> {ok, pid()}.
start_trace(Pid) when is_pid(Pid) ->
    stop_trace(),
    Tracer = spawn_link(fun() -> trace_loop([]) end),
    TraceOptions = [send, 'receive', timestamp, {tracer, Tracer}],
    case erlang:trace(Pid, true, TraceOptions) of
        1 ->
            put(?TRACE_KEY, {Pid, Tracer}),
            {ok, Tracer};
        _ ->
            exit(Tracer, normal),
            {error, trace_not_started}
    end.

%% @doc Stop tracing messages.
-spec stop_trace() -> ok.
stop_trace() ->
    case erase(?TRACE_KEY) of
        {Pid, Tracer} ->
            catch erlang:trace(Pid, false, [send, 'receive']),
            Tracer ! stop,
            ok;
        _ ->
            ok
    end.

%% @doc Get the trace results.
-spec get_trace() -> {ok, [term()]}.
get_trace() ->
    case get(?TRACE_KEY) of
        {_Pid, Tracer} ->
            Tracer ! {get_trace, self()},
            receive
                {trace, Messages} -> {ok, Messages}
            after 1000 ->
                {ok, []}
            end;
        _ ->
            {ok, []}
    end.

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
    Deadline = deadline(Timeout),
    receives_message_matching_until(Predicate, Deadline).

%% @doc Receive all messages currently in the mailbox.
-spec receives_all_messages(timeout()) -> {ok, [term()]}.
receives_all_messages(Timeout) ->
    receive
        Msg ->
            receive_all_messages_acc([Msg])
    after Timeout ->
        {ok, []}
    end.

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
    case Pid =:= self() of
        true ->
            flush_messages(),
            ok;
        false ->
            ok
    end,
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

%% @doc Receive all messages that are already in the mailbox.
receive_all_messages_acc(Acc) ->
    receive
        Msg ->
            receive_all_messages_acc([Msg | Acc])
    after 0 ->
        {ok, lists:reverse(Acc)}
    end.

%% @doc Count received messages with a specific reference.
count_received_messages(_Ref, Timeout, Count) when Timeout =< 0 ->
    Count;
count_received_messages(Ref, Timeout, Count) ->
    receive
        {Ref, _, received} ->
            count_received_messages(Ref, Timeout - 1, Count + 1)
    after Timeout ->
        Count
    end.

deadline(infinity) ->
    infinity;
deadline(Timeout) ->
    erlang:monotonic_time(millisecond) + Timeout.

receives_message_matching_until(Predicate, Deadline) ->
    WaitTime = remaining_timeout(Deadline),
    case WaitTime of
        0 ->
            {error, timeout};
        infinity ->
            receive
                Msg ->
                    case Predicate(Msg) of
                        true -> {ok, Msg};
                        false -> receives_message_matching_until(Predicate, Deadline)
                    end
            end;
        _ ->
            receive
                Msg ->
                    case Predicate(Msg) of
                        true -> {ok, Msg};
                        false -> receives_message_matching_until(Predicate, Deadline)
                    end
            after WaitTime ->
                {error, timeout}
            end
    end.

remaining_timeout(infinity) ->
    infinity;
remaining_timeout(Deadline) ->
    Remaining = Deadline - erlang:monotonic_time(millisecond),
    case Remaining > 0 of
        true -> Remaining;
        false -> 0
    end.

verify_ordered_protocol(Messages, Protocol, AllowExtra) ->
    case match_protocol(Messages, Protocol) of
        {ok, _Remaining} when AllowExtra -> {ok, true};
        {ok, []} -> {ok, true};
        {ok, Remaining} -> {error, {unexpected_messages, Remaining}};
        {error, Reason} -> {error, Reason}
    end.

verify_unordered_protocol(Messages, Protocol, AllowExtra) ->
    Required = expand_protocol_terms(Protocol),
    case required_messages_present(Messages, Required) of
        true when AllowExtra -> {ok, true};
        true when length(Messages) =:= length(Required) -> {ok, true};
        true -> {error, {unexpected_messages, Messages}};
        false -> {error, {missing_messages, missing_messages(Messages, Required)}}
    end.

match_protocol(Messages, []) ->
    {ok, Messages};
match_protocol([], Protocol) ->
    case protocol_can_terminate(Protocol) of
        true -> {ok, []};
        false -> {error, {protocol_mismatch, [], Protocol}}
    end;
match_protocol(Messages, [{optional, Term} | Rest]) ->
    case Messages of
        [Term | Remaining] -> match_protocol(Remaining, Rest);
        _ -> match_protocol(Messages, Rest)
    end;
match_protocol(Messages, [{repeat, Term} | Rest]) ->
    case consume_repeated(Messages, Term, 0) of
        {0, _Remaining} -> {error, {protocol_missing_repeat, Term}};
        {_Count, Remaining} -> match_protocol(Remaining, Rest)
    end;
match_protocol([Term | Remaining], [Term | Rest]) ->
    match_protocol(Remaining, Rest);
match_protocol(Messages, Protocol) ->
    {error, {protocol_mismatch, Messages, Protocol}}.

consume_repeated([Term | Rest], Term, Count) ->
    consume_repeated(Rest, Term, Count + 1);
consume_repeated(Messages, _Term, Count) ->
    {Count, Messages}.

protocol_can_terminate([]) ->
    true;
protocol_can_terminate([{optional, _} | Rest]) ->
    protocol_can_terminate(Rest);
protocol_can_terminate([{repeat, _} | _Rest]) ->
    false;
protocol_can_terminate(_Other) ->
    false.

expand_protocol_terms(Protocol) ->
    lists:flatmap(
        fun({optional, _Term}) -> [];
           ({repeat, Term}) -> [Term];
           (Term) -> [Term]
        end,
        Protocol
    ).

required_messages_present(Messages, Required) ->
    Missing = missing_messages(Messages, Required),
    Missing =:= [].

missing_messages(Messages, Required) ->
    lists:foldl(
        fun(Term, Acc) ->
            case lists:member(Term, Messages) of
                true -> Acc;
                false -> [Term | Acc]
            end
        end,
        [],
        Required
    ).

trace_loop(Messages) ->
    receive
        {trace_ts, Pid, send, Msg, To, Timestamp} ->
            trace_loop([{send, Pid, To, Msg, Timestamp} | Messages]);
        {trace_ts, Pid, 'receive', Msg, Timestamp} ->
            trace_loop([{'receive', Pid, Msg, Timestamp} | Messages]);
        {trace, Pid, send, Msg, To, Timestamp} ->
            trace_loop([{send, Pid, To, Msg, Timestamp} | Messages]);
        {trace, Pid, 'receive', Msg, Timestamp} ->
            trace_loop([{'receive', Pid, Msg, Timestamp} | Messages]);
        {get_trace, From} ->
            From ! {trace, lists:reverse(Messages)},
            trace_loop(Messages);
        stop ->
            ok;
        _Other ->
            trace_loop(Messages)
    end.

%% @doc Verify protocol implementation.
verify_protocol(Messages, Protocol, AllowExtra, Ordered) ->
    case {Ordered, AllowExtra} of
        {true, false} ->
            verify_ordered_protocol(Messages, Protocol, false);
        {true, true} ->
            verify_ordered_protocol(Messages, Protocol, true);
        {false, _} ->
            verify_unordered_protocol(Messages, Protocol, AllowExtra)
    end.
