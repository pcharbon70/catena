%% @doc Unit Tests for Phase 6.2: Message Passing Properties
-module(catena_message_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../../src/proptest/catena_process.hrl").
-include("../../src/proptest/catena_gen.hrl").

%%====================================================================
%% Section 6.2.1: Message Generators
%%====================================================================

gen_message_atom_returns_atom_test() ->
    Gen = catena_message:gen_message(atom),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assert(is_atom(Msg)),
    ok.

gen_message_binary_returns_binary_test() ->
    Gen = catena_message:gen_message(binary),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assert(is_binary(Msg)),
    ok.

gen_message_integer_returns_integer_test() ->
    Gen = catena_message:gen_message(integer),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assert(is_integer(Msg)),
    ok.

gen_message_list_returns_list_test() ->
    Gen = catena_message:gen_message(list),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assert(is_list(Msg)),
    ok.

gen_message_tuple_returns_tuple_test() ->
    Gen = catena_message:gen_message(tuple),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assert(is_tuple(Msg)),
    ok.

gen_message_any_returns_term_test() ->
    Gen = catena_message:gen_message(any),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assertNotEqual(undefined, Msg),
    ok.

gen_message_sequence_returns_list_test() ->
    Gen = catena_message:gen_message_sequence(3, integer),
    {Msgs, _} = Gen(#seed{state = 42}),
    ?assert(is_list(Msgs)),
    ?assertEqual(3, length(Msgs)),
    ok.

gen_tagged_message_returns_tuple_test() ->
    Gen = catena_message:gen_tagged_message(tag, integer),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assertMatch({tag, _}, Msg),
    ok.

gen_call_message_returns_gen_call_format_test() ->
    Gen = catena_message:gen_call_message(atom),
    {Msg, _} = Gen(#seed{state = 42}),
    ?assertMatch({'$gen_call', _, _}, Msg),
    ?assert(is_pid(element(2, Msg))),  %% From should be a pid
    ok.

%%====================================================================
%% Section 6.2.2: Message Sending Properties
%%====================================================================

sends_message_returns_true_when_sent_test() ->
    Fun = fun() ->
        receive
            {From, Ref, _Msg} ->
                From ! {self(), Ref, ok},
                %% Stay alive for potential further messages
                receive
                    _ -> ok
                after infinity -> ok
                end
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Result = catena_message:sends_message(Pid, test_msg, 500),
    ?assert(Result),
    catena_process:stop_process(Proc),
    ok.

sends_messages_in_order_all_sent_test() ->
    Fun = fun() ->
        receive
            {From, Ref, _Msg} ->
                From ! {self(), Ref, ok},
                %% Stay alive for more messages
                receive
                    {From2, Ref2, _Msg2} ->
                        From2 ! {self(), Ref2, ok},
                        receive
                            {From3, Ref3, _Msg3} ->
                                From3 ! {self(), Ref3, ok}
                        after infinity -> ok
                        end
                after infinity -> ok
                end
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Result = catena_message:sends_messages_in_order(Pid, [msg1, msg2, msg3], 500),
    ?assert(Result),
    catena_process:stop_process(Proc),
    ok.

sends_messages_in_order_empty_test() ->
    Fun = fun() -> receive after infinity -> ok end end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Result = catena_message:sends_messages_in_order(Pid, [], 100),
    ?assert(Result),
    catena_process:stop_process(Proc),
    ok.

no_message_loss_all_received_test() ->
    Fun = fun() ->
        receive
            {From, Ref1, 1, msg1} ->
                From ! {Ref1, 1, received},
                receive
                    {From2, Ref2, 2, msg2} ->
                        From2 ! {Ref2, 2, received}
                after infinity -> ok
                end
        end
    end,

    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    ?assertEqual({sent, 2}, catena_message:no_message_loss(Pid, [msg1, msg2], 500)),
    catena_process:stop_process(Proc),
    ok.

start_trace_records_send_and_receive_test() ->
    Fun = fun() ->
        receive
            ping -> pong
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    {ok, _Tracer} = catena_message:start_trace(Pid),
    Pid ! ping,
    timer:sleep(50),
    {ok, Trace} = catena_message:get_trace(),
    ?assert(length(Trace) > 0),
    ?assertEqual(ok, catena_message:stop_trace()),
    catena_process:stop_process(Proc),
    ok.

%%====================================================================
%% Section 6.2.3: Message Receiving Properties
%%====================================================================

receives_message_gets_message_test() ->
    Fun = fun() ->
        receive
            {check_messages, From} ->
                From ! {self(), {test, message}},
                %% Stay alive
                receive
                    _ -> ok
                after infinity -> ok
                end
        after infinity -> ok
        end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    Pid = Proc#test_process.pid,
    Result = catena_message:receives_message(500, Pid),
    ?assertMatch({ok, {test, message}}, Result),
    catena_process:stop_process(Proc),
    ok.

receives_message_timeout_test() ->
    Result = catena_message:receives_message(100, self()),
    ?assertMatch({error, timeout}, Result),
    ok.

receives_message_matching_finds_match_test() ->
    Self = self(),
    Fun = fun() ->
        Self ! {important, data},
        Self ! {other, data},
        receive after infinity -> ok end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    timer:sleep(100),
    Result = catena_message:receives_message_matching(
        fun({important, _}) -> true; (_) -> false end,
        500),
    ?assertMatch({ok, {important, _}}, Result),
    catena_process:stop_process(Proc),
    ok.

receives_all_messages_gets_all_test() ->
    %% First flush any existing messages
    catena_message:flush_messages(),

    Self = self(),
    Fun = fun() ->
        Self ! msg1,
        Self ! msg2,
        Self ! msg3,
        receive after infinity -> ok end
    end,
    Proc = catena_process:spawn_test_process(Fun),
    timer:sleep(100),
    Result = catena_message:receives_all_messages(100),
    ?assertMatch({ok, [_|_]}, Result),
    {ok, Msgs} = Result,
    ?assertEqual(3, length(Msgs)),
    catena_process:stop_process(Proc),
    ok.

flush_messages_clears_mailbox_test() ->
    %% First flush any existing messages
    catena_message:flush_messages(),

    self() ! msg1,
    self() ! msg2,
    self() ! msg3,
    ?assertNotEqual({messages, []}, process_info(self(), messages)),
    catena_message:flush_messages(),
    ?assertEqual({messages, []}, process_info(self(), messages)),
    ok.

%%====================================================================
%% Section 6.2.4: Protocol Verification
%%====================================================================

protocol_from_template_creates_protocol_test() ->
    Proto = catena_message:protocol_from_template([msg1, msg2]),
    ?assertMatch(#protocol{name = template_protocol}, Proto),
    ok.

follows_protocol_exact_match_test() ->
    Messages = [msg1, msg2, msg3],
    Protocol = [msg1, msg2, msg3],
    {ok, true} = catena_message:follows_protocol(Messages, Protocol, #{}),
    ok.

follows_protocol_ordered_extra_allowed_test() ->
    Messages = [msg1, msg2, msg3, msg4],
    Protocol = [msg1, msg2, msg3],
    {ok, true} = catena_message:follows_protocol(Messages, Protocol,
        #{allow_extra => true, ordered => true}),
    ok.

follows_protocol_unordered_test() ->
    Messages = [msg3, msg1, msg2],
    Protocol = [msg1, msg2, msg3],
    {ok, true} = catena_message:follows_protocol(Messages, Protocol,
        #{allow_extra => true, ordered => false}),
    ok.

follows_protocol_mismatch_error_test() ->
    Messages = [msg1, msg3, msg2],
    Protocol = [msg1, msg2, msg3],
    Result = catena_message:follows_protocol(Messages, Protocol,
        #{allow_extra => false, ordered => true}),
    ?assertMatch({error, {protocol_mismatch, _, _}}, Result),
    ok.

follows_protocol_optional_message_test() ->
    Messages = [msg1, msg3],
    Protocol = [msg1, {optional, msg2}, msg3],
    ?assertEqual({ok, true}, catena_message:follows_protocol(Messages, Protocol, #{})),
    ok.

follows_protocol_repeated_message_test() ->
    Messages = [msg1, msg2, msg2, msg3],
    Protocol = [msg1, {repeat, msg2}, msg3],
    ?assertEqual({ok, true}, catena_message:follows_protocol(Messages, Protocol, #{})),
    ok.

verify_protocol_exact_match_test() ->
    Messages = [msg1, msg2, msg3],
    Protocol = [msg1, msg2, msg3],
    {ok, true} = catena_message:verify_protocol(Messages, Protocol),
    ok.

verify_protocol_mismatch_test() ->
    Messages = [msg1, msg2],
    Protocol = [msg1, msg2, msg3],
    Result = catena_message:verify_protocol(Messages, Protocol),
    ?assertMatch({error, _}, Result),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

message_roundtrip_test() ->
    %% Create a simple echo server
    Echo = fun() ->
        receive
            {From, Msg} ->
                From ! {echo, Msg},
                %% Loop once more for another message
                receive
                    {_From2, _Msg2} -> ok
                after infinity -> ok
                end
        after infinity -> ok
        end
    end,

    Proc = catena_process:spawn_test_process(Echo),
    Pid = Proc#test_process.pid,

    %% Send a message
    Pid ! {self(), hello},

    %% Receive the echo
    receive
        {echo, hello} -> ?assert(true)
    after 500 ->
        ?assert(false, echo_timeout)
    end,

    catena_process:stop_process(Proc),
    ok.
