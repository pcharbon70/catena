%%%-------------------------------------------------------------------
%%% @doc Catena Publish/Subscribe (Phase 5.3)
%%%
%%% This module implements a publish/subscribe pattern for process
%%% communication. Topics can be hierarchical (using dot notation),
%%% and processes can subscribe to specific topics or wildcards.
%%%
%%% == Topic Patterns ==
%%%
%%% Topics use dot notation for hierarchy (e.g., "stocks.price AAPL").
%%% Wildcards are supported:
%%% - "*" matches one level (e.g., "stocks.*.AAPL")
%%% - ">" matches all levels below (e.g., "stocks.>")
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Start a pub/sub service
%%% {ok, Pid} = catena_pubsub:start_link(my_events).
%%%
%%% %% Subscribe to a topic
%%% ok = catena_pubsub:subscribe(my_events, self(), "stocks.price").
%%%
%%% %% Publish a message
%%% ok = catena_pubsub:publish(my_events, "stocks.price", #{price => 150.25}).
%%%
%%% %% Subscriber receives:
%%% %% {pubsub, "stocks.price", #{price => 150.25}}
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_pubsub).

%% API
-export([
    start_link/1,
    start_link/2,
    stop/1,
    subscribe/3,
    subscribe/4,
    unsubscribe/3,
    publish/3,
    publish/4,
    list_subscribers/2,
    list_topics/1
]).

%%====================================================================
%% Types
%%====================================================================

-type pubsub_name() :: atom() | pid().
-type pubsub() :: pubsub_name() | {via, module(), term()}.
-type topic() :: binary() | string() | atom().
-type subscriber() :: pid() | {pid(), reference()}.

-export_type([pubsub_name/0, pubsub/0, topic/0, subscriber/0]).

-record(subscription, {
    topic :: topic(),
    subscriber :: subscriber(),
    monitor :: reference(),
    options :: map()
}).

-record(state, {
    name :: term(),
    subscriptions :: [#subscription{}],
    by_subscriber :: #{subscriber => [topic()]},
    by_topic :: #{topic() => [subscriber()]}
}).

%%====================================================================
%% API - Start Functions
%%====================================================================

%% @doc Start a pub/sub service with a name.
-spec start_link(pubsub_name()) -> {ok, pid()} | {error, term()}.
start_link({local, Name}) when is_atom(Name) ->
    start_link(Name, []);
start_link(Name) ->
    start_link(Name, []).

%% @doc Start a pub/sub service with a name and options.
-spec start_link(pubsub_name(), list()) -> {ok, pid()} | {error, term()}.
start_link({local, Name}, Options) when is_atom(Name) ->
    case do_start_link(Name, Options) of
        {ok, Pid} ->
            case erlang:register(Name, Pid) of
                true -> {ok, Pid};
                false -> exit(Pid, kill), {error, {already_registered, Name}}
            end;
        Error ->
            Error
    end;
start_link(Name, Options) ->
    do_start_link(Name, Options).

do_start_link(Name, Options) ->
    Parent = self(),
    Ref = make_ref(),
    InitFun = fun() ->
        try
            State = #state{
                name = Name,
                subscriptions = [],
                by_subscriber = #{},
                by_topic = #{}
            },
            Parent ! {Ref, {ok, self()}},
            pubsub_loop(State)
        catch
            Class:ErrorReason:Stacktrace ->
                Parent ! {Ref, {error, {Class, ErrorReason, Stacktrace}}}
        end
    end,
    Pid = erlang:spawn(InitFun),
    MRef = erlang:monitor(process, Pid),
    receive
        {Ref, {ok, Pid}} ->
            erlang:demonitor(MRef, [flush]),
            {ok, Pid};
        {Ref, {error, Reason}} ->
            {error, Reason}
    after 5000 ->
        erlang:demonitor(MRef, [flush]),
        {error, init_timeout}
    end.

%%====================================================================
%% API - Pub/Sub Operations
%%====================================================================

%% @doc Stop the pub/sub service.
-spec stop(pubsub()) -> ok.
stop(PubSub) when is_pid(PubSub); is_atom(PubSub) ->
    Pid = get_pid(PubSub),
    MRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _Info} -> ok
    after 5000 ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end.

%% @doc Subscribe to a topic.
-spec subscribe(pubsub(), pid(), topic()) -> ok | {error, term()}.
subscribe(PubSub, SubscriberPid, Topic) ->
    subscribe(PubSub, SubscriberPid, Topic, #{}).

%% @doc Subscribe to a topic with options.
-spec subscribe(pubsub(), pid(), topic(), map()) -> ok | {error, term()}.
subscribe(PubSub, SubscriberPid, Topic, Options) when is_pid(SubscriberPid) ->
    call(PubSub, {subscribe, SubscriberPid, Topic, Options}).

%% @doc Unsubscribe from a topic.
-spec unsubscribe(pubsub(), pid(), topic()) -> ok.
unsubscribe(PubSub, SubscriberPid, Topic) ->
    call(PubSub, {unsubscribe, SubscriberPid, Topic}).

%% @doc Publish a message to a topic.
-spec publish(pubsub(), topic(), term()) -> ok.
publish(PubSub, Topic, Message) ->
    publish(PubSub, Topic, Message, #{}).

%% @doc Publish a message to a topic with options.
-spec publish(pubsub(), topic(), term(), map()) -> ok.
publish(PubSub, Topic, Message, Options) ->
    call(PubSub, {publish, Topic, Message, Options}),
    ok.

%% @doc List all subscribers for a topic.
-spec list_subscribers(pubsub(), topic()) -> [pid()].
list_subscribers(PubSub, Topic) ->
    call(PubSub, {list_subscribers, Topic}).

%% @doc List all topics with subscribers.
-spec list_topics(pubsub()) -> [topic()].
list_topics(PubSub) ->
    call(PubSub, list_topics).

%%====================================================================
%% Internal Functions
%%====================================================================

call(PubSub, Request) when is_pid(PubSub); is_atom(PubSub) ->
    Pid = get_pid(PubSub),
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, Request},
    receive
        {Ref, Reply} ->
            Reply
    after 5000 ->
        error({call_timeout, Pid, 5000})
    end.

get_pid(Pid) when is_pid(Pid) -> Pid;
get_pid(Name) when is_atom(Name) ->
    case erlang:whereis(Name) of
        undefined -> error({no_process, Name});
        Pid -> Pid
    end.

%%====================================================================
%% Pub/Sub Loop
%%====================================================================

pubsub_loop(State) ->
    receive
        {call, From, Request} ->
            {Reply, NewState} = do_handle_call(Request, From, State),
            reply(From, Reply),
            pubsub_loop(NewState);

        {'DOWN', MRef, process, Pid, _Info} ->
            NewState = handle_down(MRef, Pid, State),
            pubsub_loop(NewState);

        _Info ->
            pubsub_loop(State)
    end.

reply({To, Tag}, Reply) ->
    To ! {Tag, Reply},
    ok.

%%====================================================================
%% Handle Calls
%%====================================================================

do_handle_call({subscribe, SubscriberPid, Topic, Options}, _From, State) ->
    MRef = erlang:monitor(process, SubscriberPid),
    Subscription = #subscription{
        topic = Topic,
        subscriber = SubscriberPid,
        monitor = MRef,
        options = Options
    },
    #state{subscriptions = Subs, by_subscriber = BySub, by_topic = ByTopic} = State,

    NewSubs = [Subscription | Subs],
    NewBySub = maps:update_with(SubscriberPid, fun(Topics) -> [Topic | Topics] end, [Topic], BySub),
    NewByTopic = maps:update_with(Topic, fun(SubsList) -> [SubscriberPid | SubsList] end, [SubscriberPid], ByTopic),

    {ok, State#state{subscriptions = NewSubs, by_subscriber = NewBySub, by_topic = NewByTopic}};

do_handle_call({unsubscribe, SubscriberPid, Topic}, _From, State) ->
    #state{subscriptions = Subs, by_subscriber = BySub, by_topic = ByTopic} = State,

    % Remove matching subscriptions
    {NewSubs, RemovedMRefs} = lists:foldl(fun(
        #subscription{subscriber = Sub, topic = T, monitor = MRef} = S,
        {AccSubs, AccMRefs}) ->
        case Sub =:= SubscriberPid andalso T =:= Topic of
            true -> {AccSubs, [MRef | AccMRefs]};
            false -> {[S | AccSubs], AccMRefs}
        end
    end, {[], []}, Subs),

    % Demonitor removed subscriptions
    lists:foreach(fun(MRef) -> erlang:demonitor(MRef, [flush]) end, RemovedMRefs),

    % Update by_subscriber
    NewBySub = case maps:get(SubscriberPid, BySub, undefined) of
        undefined -> BySub;
        Topics ->
            NewTopics = lists:delete(Topic, Topics),
            case NewTopics of
                [] -> maps:remove(SubscriberPid, BySub);
                _ -> maps:put(SubscriberPid, NewTopics, BySub)
            end
    end,

    % Update by_topic
    NewByTopic = case maps:get(Topic, ByTopic, undefined) of
        undefined -> ByTopic;
        SubsList ->
            NewSubsList = lists:delete(SubscriberPid, SubsList),
            case NewSubsList of
                [] -> maps:remove(Topic, ByTopic);
                _ -> maps:put(Topic, NewSubsList, ByTopic)
            end
    end,

    {ok, State#state{subscriptions = NewSubs, by_subscriber = NewBySub, by_topic = NewByTopic}};

do_handle_call({publish, Topic, Message, Options}, _From, State) ->
    #state{subscriptions = Subs} = State,
    Subscribers = find_matching_subscribers(Topic, Subs),
    lists:foreach(fun(#subscription{subscriber = Sub, options = SubOptions}) ->
        SendFun = maps:get(send_fun, Options, fun send_message/4),
        SendFun(Sub, Topic, Message, maps:merge(SubOptions, Options))
    end, Subscribers),
    {ok, State};

do_handle_call({list_subscribers, Topic}, _From, State) ->
    #state{subscriptions = Subs} = State,
    Matching = find_matching_subscribers(Topic, Subs),
    Subscribers = [S || #subscription{subscriber = S} <- Matching],
    {Subscribers, State};

do_handle_call(list_topics, _From, State) ->
    #state{by_topic = ByTopic} = State,
    Topics = maps:keys(ByTopic),
    {Topics, State};

do_handle_call(_Request, _From, State) ->
    {{error, unknown_request}, State}.

%%====================================================================
%% Handle DOWN
%%====================================================================

handle_down(MRef, Pid, State) ->
    #state{subscriptions = Subs, by_subscriber = BySub, by_topic = ByTopic} = State,

    % Remove subscriptions for this Pid
    {NewSubs, RemovedTopics} = lists:foldl(fun(
        #subscription{subscriber = Sub, monitor = M, topic = T} = S,
        {AccSubs, AccTopics}) ->
        case Sub =:= Pid andalso M =:= MRef of
            true -> {AccSubs, [T | AccTopics]};
            false -> {[S | AccSubs], AccTopics}
        end
    end, {[], []}, Subs),

    % Update by_subscriber
    NewBySub = maps:remove(Pid, BySub),

    % Update by_topic
    NewByTopic = lists:foldl(fun(Topic, Acc) ->
        case maps:get(Topic, Acc, undefined) of
            undefined -> Acc;
            SubsList ->
                NewSubsList = lists:delete(Pid, SubsList),
                case NewSubsList of
                    [] -> maps:remove(Topic, Acc);
                    _ -> maps:put(Topic, NewSubsList, Acc)
                end
        end
    end, ByTopic, RemovedTopics),

    State#state{subscriptions = NewSubs, by_subscriber = NewBySub, by_topic = NewByTopic}.

%%====================================================================
%% Pattern Matching
%%====================================================================

%% @private Find all subscribers matching a topic.
find_matching_subscribers(Topic, Subscriptions) ->
    TopicParts = split_topic(Topic),
    [S || #subscription{topic = SubTopic} = S <- Subscriptions,
          match_topic(TopicParts, SubTopic)].

%% @private Check if a topic matches a subscription pattern.
match_topic(TopicParts, Pattern) when is_list(TopicParts) ->
    PatternParts = split_topic(Pattern),
    match_parts(TopicParts, PatternParts);
match_topic(Topic, Pattern) ->
    match_topic(split_topic(Topic), Pattern).

match_parts(TopicParts, PatternParts) ->
    do_match_parts(TopicParts, PatternParts).

do_match_parts([], []) -> true;
do_match_parts(_Topic, [<<62>>]) -> true;
do_match_parts(_TopicParts, [<<62>>|_]) -> true;
do_match_parts([_|T1], [<<"*">>|T2]) -> do_match_parts(T1, T2);
do_match_parts([H|T1], [H|T2]) -> do_match_parts(T1, T2);
do_match_parts(_, _) -> false.

%% @private Split a topic into parts.
split_topic(Topic) when is_binary(Topic) ->
    binary:split(Topic, <<".">>, [global]);
split_topic(Topic) when is_atom(Topic) ->
    split_topic(atom_to_binary(Topic));
split_topic(Topic) when is_list(Topic) ->
    case lists:member($., Topic) of
        true ->
            % String with dots - split
            lists:map(fun list_to_binary/1, string:split(Topic, ".", all));
        false ->
            % Simple string
            [list_to_binary(Topic)]
    end;
split_topic(Topic) ->
    [Topic].

%% @private Default send function.
send_message(SubscriberPid, Topic, Message, _Options) when is_pid(SubscriberPid) ->
    SubscriberPid ! {pubsub, Topic, Message}.
