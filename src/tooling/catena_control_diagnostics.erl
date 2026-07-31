%%%-------------------------------------------------------------------
%%% @doc Source-oriented views of generated and runtime control flow.
%%%
%%% This module is deliberately a lossy boundary. Generated CPS identities,
%%% continuation wrappers, runtime authorities, and VM terms are collapsed
%%% into Catena constructs before a diagnostic reaches a user or tool.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_diagnostics).

-export([
    source_frames/1,
    trace_view/2,
    failure_view/2
]).

-type source_frame() :: #{
    construct := atom(),
    transform => atom(),
    location => term(),
    control_mode => atom(),
    runtime_disposition => atom()
}.

-export_type([source_frame/0]).

%% @doc Collapse an artifact's generated control inventory to source frames.
-spec source_frames(term()) -> [source_frame()].
source_frames(Input) ->
    Origins = control_origins(Input),
    deduplicate(lists:append([frames_for_origin(Origin) || Origin <- Origins])).

%% @doc Attach matching source frames to redacted runtime trace events.
-spec trace_view([map()], term()) -> [map()].
trace_view(Events, Artifact) when is_list(Events) ->
    Frames = source_frames(Artifact),
    [
        Event#{frames => matching_frames(Event, Frames)}
        || Event <- Events,
           is_map(Event)
    ].

%% @doc Produce a stable control-failure diagnostic with source frames.
-spec failure_view(term(), term()) -> map().
failure_view(Failure, Artifact) when is_map(Failure) ->
    #{
        category => maps:get(category, Failure, unknown_control_failure),
        details => redact(maps:get(details, Failure, #{})),
        origin => source_origin(maps:get(origin, Failure, undefined)),
        frames => source_frames(Artifact)
    };
failure_view(Failure, Artifact) ->
    #{
        category => unknown_control_failure,
        details => #{reason => redact(Failure)},
        frames => source_frames(Artifact)
    }.

control_origins(#{metadata := #{origins := #{control := Origins}}})
        when is_list(Origins) ->
    Origins;
control_origins(#{origins := #{control := Origins}}) when is_list(Origins) ->
    Origins;
control_origins(#{control := Origins}) when is_list(Origins) ->
    Origins;
control_origins(Origins) when is_list(Origins) ->
    Origins;
control_origins(_Input) ->
    [].

frames_for_origin(Origin) when is_map(Origin) ->
    Operation = maps:get(operation, Origin, unknown),
    Constructs = source_constructs(Operation),
    [frame(Construct, Origin) || Construct <- Constructs];
frames_for_origin(_Origin) ->
    [].

source_constructs(perform) -> [perform];
source_constructs(make_resumption) -> [perform];
source_constructs(install_handler) -> [handler, delimiter];
source_constructs(resume) -> [resume];
source_constructs(abort) -> [delimiter];
source_constructs(bind) -> [binder];
source_constructs(bridge) -> [transform];
source_constructs(closure) -> [transform];
source_constructs(binder) -> [binder];
source_constructs(return) -> [transform];
source_constructs(direct_expr) -> [transform];
source_constructs(direct_call) -> [transform];
source_constructs(cps_call) -> [transform];
source_constructs(_Operation) -> [transform].

frame(Construct, Origin) ->
    SourceOrigin = maps:get(source_origin, Origin, undefined),
    maps:filter(
        fun(_Key, Value) -> Value =/= undefined end,
        #{
            construct => Construct,
            transform => maps:get(transform, Origin, undefined),
            location => source_location(SourceOrigin),
            control_mode => maps:get(control_mode, Origin, undefined),
            runtime_disposition => maps:get(
                runtime_disposition,
                Origin,
                undefined
            )
        }
    ).

source_location(#{location := Location}) ->
    redact(Location);
source_location(#{source := Location}) ->
    redact(Location);
source_location({location, Line, Column}) ->
    #{line => Line, column => Column};
source_location({Line, Column}) when is_integer(Line), is_integer(Column) ->
    #{line => Line, column => Column};
source_location(undefined) ->
    undefined;
source_location(Location) ->
    redact(Location).

source_origin(undefined) ->
    undefined;
source_origin(Origin) when is_map(Origin) ->
    maps:from_list([
        {Key, source_origin_value(Key, Value)}
        || {Key, Value} <- maps:to_list(Origin),
           lists:member(Key, [
               source,
               location,
               file,
               module,
               transform,
               construct,
               perform,
               handler_case,
               delimiter,
               effect,
               operation
           ])
    ]);
source_origin(Origin) ->
    source_location(Origin).

source_origin_value(Key, Value)
        when Key =:= source; Key =:= location; Key =:= perform;
             Key =:= handler_case; Key =:= delimiter ->
    source_origin(Value);
source_origin_value(_Key, Value) ->
    redact(Value).

matching_frames(Event, Frames) ->
    EventConstruct = event_construct(maps:get(event, Event, unknown)),
    Matches = [
        Frame
        || Frame <- Frames,
           frame_matches(EventConstruct, maps:get(construct, Frame))
    ],
    case Matches of
        [] -> Frames;
        _ -> Matches
    end.

event_construct(capture) -> perform;
event_construct(handler_selection) -> handler;
event_construct(resume) -> resume;
event_construct(abort) -> delimiter;
event_construct(cleanup) -> delimiter;
event_construct(Event) -> Event.

frame_matches(Construct, Construct) -> true;
frame_matches(branch, resume) -> true;
frame_matches(consumption, resume) -> true;
frame_matches(timeout, resume) -> true;
frame_matches(_Event, _Frame) -> false.

deduplicate(Frames) ->
    {_Seen, ResultRev} = lists:foldl(
        fun(Frame, {Seen, Acc}) ->
            Key = {
                maps:get(construct, Frame),
                maps:get(transform, Frame, undefined),
                maps:get(location, Frame, undefined)
            },
            case sets:is_element(Key, Seen) of
                true -> {Seen, Acc};
                false -> {sets:add_element(Key, Seen), [Frame | Acc]}
            end
        end,
        {sets:new(), []},
        Frames
    ),
    lists:reverse(ResultRev).

redact({catena_resumption, _Version, _Authority}) -> resumption;
redact(Term) when
    is_atom(Term);
    is_binary(Term);
    is_integer(Term);
    is_float(Term)
-> Term;
redact(Term) when is_list(Term) -> [redact(Item) || Item <- Term];
redact(Term) when is_tuple(Term) ->
    list_to_tuple([redact(Item) || Item <- tuple_to_list(Term)]);
redact(Term) when is_map(Term) ->
    maps:from_list([
        {redact(Key), redact(Value)}
        || {Key, Value} <- maps:to_list(Term)
    ]);
redact(Term) when is_function(Term) -> closure;
redact(Term) when is_pid(Term) -> process;
redact(Term) when is_reference(Term) -> opaque_reference;
redact(Term) when is_port(Term) -> port;
redact(_Term) -> opaque.
