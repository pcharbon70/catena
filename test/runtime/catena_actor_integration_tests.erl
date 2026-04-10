%%%-------------------------------------------------------------------
%%% @doc Integration tests for Phase 5 Actor Model
%%%
%%% This module contains integration tests that verify the interaction
%%% between different components of the actor system:
%%% - Process Registry
%%% - Publish/Subscribe
%%% - Event Broadcaster
%%% - GenServer Pattern
%%% - Supervisor Pattern
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_actor_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

cleanup_all() ->
    lists:foreach(fun(N) ->
        case whereis(N) of
            undefined -> ok;
            Pid -> exit(Pid, kill)
        end
    end, [
        test_reg, test_pubsub, test_broadcaster,
        integration_reg, integration_pubsub, integration_broadcaster,
        integration_test_sup, integration_test_sup2,
        test_server, test_supervisor
    ]).

%%%=============================================================================
%%% 5.4.1 Actor Communication Testing
%%%=============================================================================

%% Test point-to-point messaging between actor pairs
point_to_point_messaging_test() ->
    cleanup_all(),

    % Start a registry for process lookup
    {ok, Reg} = catena_registry:start_link(integration_reg),

    % Create two simple actor processes
    Actor1 = spawn(fun() -> actor_loop(#{id => actor1, count => 0}) end),
    Actor2 = spawn(fun() -> actor_loop(#{id => actor2, count => 0}) end),

    % Register actors
    ok = catena_registry:register(Reg, actor1, Actor1),
    ok = catena_registry:register(Reg, actor2, Actor2),

    % Send message from actor1 to actor2 via registry
    Actor1 ! {send_via_registry, Reg, actor2, {ping, self()}},

    % Receive pong
    receive
        {pong, actor2} -> ok
    after 500 ->
        error(did_not_receive_pong)
    end,

    % Cleanup
    catena_registry:stop(Reg),
    exit(Actor1, kill),
    exit(Actor2, kill).

%% Test broadcast messaging to multiple actors
broadcast_messaging_test() ->
    cleanup_all(),

    % Start an event broadcaster
    {ok, Broadcaster} = catena_event_broadcaster:start_link(integration_broadcaster),

    % Create multiple listener actors
    Listener1 = spawn(fun() -> listener_loop(#{id => listener1, received => 0}) end),
    Listener2 = spawn(fun() -> listener_loop(#{id => listener2, received => 0}) end),
    Listener3 = spawn(fun() -> listener_loop(#{id => listener3, received => 0}) end),

    % Add all listeners
    ok = catena_event_broadcaster:add_listener(Broadcaster, Listener1),
    ok = catena_event_broadcaster:add_listener(Broadcaster, Listener2),
    ok = catena_event_broadcaster:add_listener(Broadcaster, Listener3),

    % Broadcast an event
    ok = catena_event_broadcaster:broadcast(Broadcaster, #{type => test, data => hello}),

    timer:sleep(50),

    % Verify all listeners received the event
    ?assertEqual(1, get_count(Listener1)),
    ?assertEqual(1, get_count(Listener2)),
    ?assertEqual(1, get_count(Listener3)),

    % Broadcast another event
    ok = catena_event_broadcaster:broadcast(Broadcaster, #{type => test, data => world}),

    timer:sleep(50),

    % Verify all listeners received the second event
    ?assertEqual(2, get_count(Listener1)),
    ?assertEqual(2, get_count(Listener2)),
    ?assertEqual(2, get_count(Listener3)),

    % Cleanup
    catena_event_broadcaster:stop(Broadcaster),
    exit(Listener1, kill),
    exit(Listener2, kill),
    exit(Listener3, kill).

%% Test message chains (pipeline pattern)
message_chain_test() ->
    cleanup_all(),

    % Start a pubsub for chaining
    {ok, PubSub} = catena_pubsub:start_link(integration_pubsub),

    % Register stage processes so they can find each other
    Stage1 = spawn(fun() -> pipeline_stage(stage1, [stage2], PubSub) end),
    register(stage1, Stage1),
    Stage2 = spawn(fun() -> pipeline_stage(stage2, [stage3], PubSub) end),
    register(stage2, Stage2),
    Stage3 = spawn(fun() -> pipeline_stage(stage3, [], PubSub) end),
    register(stage3, Stage3),

    % Subscribe to completion notifications
    ok = catena_pubsub:subscribe(PubSub, self(), <<"pipeline.complete">>),

    % Start the pipeline
    Stage1 ! {process, #{data => test_input}},

    % Wait for completion
    receive
        {pubsub, <<"pipeline.complete">>, #{stage := stage3, data := FinalData}} ->
            ?assertEqual(<<"test_input_processed_stage2_processed">>, FinalData);
        Other ->
            error({unexpected_message, Other})
    after 500 ->
        error(pipeline_timeout)
    end,

    % Cleanup
    unregister(stage1),
    unregister(stage2),
    unregister(stage3),
    catena_pubsub:stop(PubSub),
    exit(Stage1, kill),
    exit(Stage2, kill),
    exit(Stage3, kill).

%% Test error handling when messaging dead processes
error_handling_dead_process_test() ->
    cleanup_all(),

    % Start a registry
    {ok, Reg} = catena_registry:start_link(integration_reg),

    % Create and immediately kill an actor
    Actor = spawn(fun() -> actor_loop(#{}) end),
    exit(Actor, kill),
    timer:sleep(10),

    % Verify actor is dead
    ?assertNot(is_process_alive(Actor)),

    % Try to register the dead actor (succeeds but will be auto-cleaned)
    Result = catena_registry:register(Reg, dead_actor, Actor),
    % Registry allows registering any Pid - monitoring handles cleanup
    ?assertEqual(ok, Result),

    % Wait for DOWN message to clean up
    timer:sleep(50),

    % Verify dead actor was cleaned up
    ?assertEqual({error, not_found}, catena_registry:find(Reg, dead_actor)),

    % Try to send message to dead actor (should not crash sender)
    Actor ! {message, test},
    timer:sleep(10),

    % Verify sender is still alive
    ?assert(is_process_alive(self())),

    % Cleanup
    catena_registry:stop(Reg).

%%%=============================================================================
%%% 5.4.2 Fault Tolerance Testing
%%%=============================================================================

%% Test communication pattern fault tolerance
supervisor_with_communication_test() ->
    cleanup_all(),

    % Start a supervisor
    {ok, Sup} = catena_supervisor:start_link(integration_test_sup, []),

    % Start a pubsub for fault-tolerant communication
    {ok, PubSub} = catena_pubsub:start_link(integration_pubsub),

    % The supervisor's child (test_worker) should be running
    Children = catena_supervisor:which_children(Sup),
    ?assertEqual(1, length(Children)),

    % Verify pubsub is independent and working
    ok = catena_pubsub:publish(PubSub, <<"test.topic">>, #{data => test}),
    ?assert(is_process_alive(Sup)),
    ?assert(is_process_alive(PubSub)),

    % Cleanup
    catena_supervisor:stop(Sup),
    catena_pubsub:stop(PubSub).

%% Test communication pattern fault tolerance
pubsub_auto_cleanup_test() ->
    cleanup_all(),

    % Start pubsub
    {ok, PubSub} = catena_pubsub:start_link(integration_pubsub),

    % Create a subscriber
    Subscriber = spawn(fun() -> subscriber_loop_simple() end),
    ok = catena_pubsub:subscribe(PubSub, Subscriber, <<"test.topic">>),

    % Verify subscription
    ?assertEqual(1, length(catena_pubsub:list_subscribers(PubSub, <<"test.topic">>))),

    % Kill the subscriber
    exit(Subscriber, kill),
    timer:sleep(50),

    % Verify automatic cleanup
    ?assertEqual(0, length(catena_pubsub:list_subscribers(PubSub, <<"test.topic">>))),

    % Cleanup
    catena_pubsub:stop(PubSub).

%%%=============================================================================
%%% Combined Pattern Tests
%%%=============================================================================

%% Test registry + pubsub integration
registry_pubsub_integration_test() ->
    cleanup_all(),

    % Start both services
    {ok, Reg} = catena_registry:start_link(integration_reg),
    {ok, PubSub} = catena_pubsub:start_link(integration_pubsub),

    % Create and register an actor
    Actor = spawn(fun() -> pubsub_actor_loop(Reg, PubSub) end),
    ok = catena_registry:register(Reg, pubsub_actor, Actor),

    % Subscribe to notifications
    ok = catena_pubsub:subscribe(PubSub, self(), <<"actor.events">>),

    % Clear any messages in mailbox
    receive
        _ -> ok
    after 0 ->
        ok
    end,

    % Send command to actor
    Actor ! {do_work, test_data},

    % Receive notification via pubsub
    receive
        {pubsub, <<"actor.events">>, #{event := work_done, result := {ok, test_data}}} -> ok;
        Other -> error({unexpected_message, Other})
    after 500 ->
        error(did_not_receive_notification)
    end,

    % Cleanup
    catena_registry:stop(Reg),
    catena_pubsub:stop(PubSub),
    exit(Actor, kill).

%% Test event broadcaster + registry integration
broadcaster_registry_integration_test() ->
    cleanup_all(),

    % Start both services
    {ok, Reg} = catena_registry:start_link(integration_reg),
    {ok, Broadcaster} = catena_event_broadcaster:start_link(integration_broadcaster),

    % Create and register multiple listeners
    Listener1 = spawn(fun() -> counting_listener_loop() end),
    Listener2 = spawn(fun() -> counting_listener_loop() end),

    ok = catena_registry:register(Reg, listener1, Listener1),
    ok = catena_registry:register(Reg, listener2, Listener2),

    % Add listeners from registry
    {ok, Listener1} = catena_registry:find(Reg, listener1),
    {ok, Listener2} = catena_registry:find(Reg, listener2),

    ok = catena_event_broadcaster:add_listener(Broadcaster, Listener1),
    ok = catena_event_broadcaster:add_listener(Broadcaster, Listener2),

    % Broadcast events
    ok = catena_event_broadcaster:broadcast(Broadcaster, #{event => test1}),
    ok = catena_event_broadcaster:broadcast(Broadcaster, #{event => test2}),

    timer:sleep(50),

    % Verify both listeners received events
    ?assertEqual(2, get_count(Listener1)),
    ?assertEqual(2, get_count(Listener2)),

    % Cleanup
    catena_registry:stop(Reg),
    catena_event_broadcaster:stop(Broadcaster),
    exit(Listener1, kill),
    exit(Listener2, kill).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Simple actor loop for testing
actor_loop(State) ->
    receive
        {send_via_registry, Reg, Name, Message} ->
            case catena_registry:find(Reg, Name) of
                {ok, Pid} -> Pid ! Message;
                _ -> ok
            end,
            actor_loop(State);
        {ping, From} ->
            From ! {pong, maps:get(id, State)},
            actor_loop(State);
        {get_state, From} ->
            From ! {state, State},
            actor_loop(State);
        stop ->
            ok;
        _ ->
            actor_loop(State)
    end.

%% Listener loop that counts events
listener_loop(State) ->
    receive
        {event, _Event} ->
            Count = maps:get(received, State, 0),
            listener_loop(maps:put(received, Count + 1, State));
        {get_count, From} ->
            From ! {count, maps:get(received, State, 0)},
            listener_loop(State);
        stop ->
            ok;
        _ ->
            listener_loop(State)
    end.

%% Get count from listener process
get_count(Pid) ->
    Pid ! {get_count, self()},
    receive
        {count, Count} -> Count
    after 100 -> 0
    end.

%% Pipeline stage for message chain testing
pipeline_stage(StageId, NextStages, PubSub) ->
    receive
        {process, Data} ->
            % Process the data
            ProcessedData = process_data(StageId, Data),

            % Send to next stages or publish completion
            case NextStages of
                [] ->
                    % Final stage - publish completion
                    catena_pubsub:publish(PubSub, <<"pipeline.complete">>,
                        #{stage => StageId, data => ProcessedData});
                _ ->
                    % Send to next stages
                    lists:foreach(fun(StageName) ->
                        case whereis(StageName) of
                            undefined -> ok;
                            Pid -> Pid ! {process, ProcessedData}
                        end
                    end, NextStages)
            end,
            pipeline_stage(StageId, NextStages, PubSub);
        stop ->
            ok;
        _ ->
            pipeline_stage(StageId, NextStages, PubSub)
    end.

%% Process data through pipeline stage
process_data(stage1, #{data := Data}) ->
    iolist_to_binary([atom_to_list(Data), "_processed"]);
process_data(stage2, Data) when is_binary(Data) ->
    <<Data/binary, "_stage2">>;
process_data(stage3, Data) when is_binary(Data) ->
    <<Data/binary, "_processed">>;
process_data(_, Data) ->
    Data.

%% Subscriber loop for pubsub
subscriber_loop_simple() ->
    receive
        {pubsub, _Topic, _Message} ->
            subscriber_loop_simple();
        stop ->
            ok;
        _ ->
            subscriber_loop_simple()
    end.

%% Actor that uses pubsub for notifications
pubsub_actor_loop(Reg, PubSub) ->
    receive
        {do_work, Data} ->
            % Do work
            Result = do_work(Data),
            % Publish notification
            catena_pubsub:publish(PubSub, <<"actor.events">>,
                #{event => work_done, result => Result}),
            pubsub_actor_loop(Reg, PubSub);
        stop ->
            ok;
        _ ->
            pubsub_actor_loop(Reg, PubSub)
    end.

%% Do some work
do_work(Data) ->
    {ok, Data}.

%% Counting listener loop
counting_listener_loop() ->
    receive
        {event, _Event} ->
            E = case get(events) of undefined -> 0; V -> V end,
            put(events, E + 1),
            counting_listener_loop();
        {get_count, From} ->
            Count = case get(events) of undefined -> 0; V -> V end,
            From ! {count, Count},
            counting_listener_loop();
        stop ->
            ok;
        _ ->
            counting_listener_loop()
    end.
