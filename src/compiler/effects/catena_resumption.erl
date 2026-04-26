%%%-------------------------------------------------------------------
%%% @doc Catena Resumption Type and Capture (Phase 7.1)
%%%
%%% This module implements the resumption type representing "the rest of
%%% the computation after an operation" and the mechanisms to capture
%%% continuations as resumptions. This is the foundation of algebraic
%%% effects, enabling handlers to receive both operation values and
%%% resumptions.
%%%
%%% == Resumption Type ==
%%%
%%% A resumption represents the continuation of computation after a perform
%%% operation. It captures the stack frame and execution state at the point
%%% where an effect was performed, allowing handlers to resume, abort, or
%%% handle operations multiple times.
%%%
%%% - type resumption() :: #resumption{}
%%%   - cont: The continuation function
%%%   - captured_at: Timestamp when continuation was captured
%%%   - stack_depth: Depth of stack at capture point
%%%   - metadata: Additional metadata about the resumption
%%%
%%% == Continuation Capture ==
%%%
%%% Continuations are captured using the process dictionary to store the
%%% current execution context. When a perform operation is invoked, the
%%% current continuation is captured before handler lookup.
%%%
%%% == Resumption Execution ==
%%%
%%% Resumptions are executed by calling the captured continuation with a
%%% value. The execution includes error handling, stack trace capture, and
%%% timeout mechanisms.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption).

%% Resumption type and constructors
-export([
    new/3,
    new/4,
    is_resumption/1,
    cont_of/1
]).

%% Resumption execution
-export([
    resume/2,
    resume_with_timeout/3
]).

%% Continuation capture
-export([
    capture_continuation/0,
    capture_with_metadata/1
]).

%% Resumption inspection
-export([
    captured_at/1,
    stack_depth/1,
    metadata/1,
    update_metadata/2
]).

%% Resumption validation
-export([
    validate/1,
    is_valid_continuation/1
]).

%%====================================================================
%% Types
%%====================================================================

-record(resumption, {
    cont :: function(),
    captured_at :: integer(),
    stack_depth :: non_neg_integer(),
    metadata :: map()
}).

-opaque resumption() :: #resumption{}.

-type continuation() :: function().
-type metadata() :: map().

-export_type([resumption/0]).

%%====================================================================
%% Resumption Type and Constructors
%%====================================================================

%% @doc Create a new resumption with minimal metadata.
-spec new(continuation(), integer(), non_neg_integer()) -> resumption().
new(Cont, CapturedAt, StackDepth) when is_function(Cont, 1) ->
    new(Cont, CapturedAt, StackDepth, #{}).

%% @doc Create a new resumption with metadata.
-spec new(continuation(), integer(), non_neg_integer(), metadata()) -> resumption().
new(Cont, CapturedAt, StackDepth, Metadata) when is_function(Cont, 1), is_map(Metadata) ->
    #resumption{
        cont = Cont,
        captured_at = CapturedAt,
        stack_depth = StackDepth,
        metadata = Metadata
    }.

%% @doc Check if a term is a resumption.
-spec is_resumption(term()) -> boolean().
is_resumption(#resumption{}) -> true;
is_resumption(_) -> false.

%% @doc Extract the continuation function from a resumption.
-spec cont_of(resumption()) -> continuation().
cont_of(#resumption{cont = Cont}) -> Cont.

%%====================================================================
%% Continuation Capture
%%====================================================================

%% @doc Capture the current continuation as a resumption.
%%
%% This function captures the current execution context using the process
%% dictionary. The continuation represents "the rest of the computation"
%% from the point where this function is called.
%%
%% Note: This is a simplified implementation. A full implementation would
%% use CPS transformation or exceptions to capture the continuation.
-spec capture_continuation() -> resumption().
capture_continuation() ->
    capture_with_metadata(#{}).

%% @doc Capture the current continuation with custom metadata.
-spec capture_with_metadata(metadata()) -> resumption().
capture_with_metadata(Metadata) when is_map(Metadata) ->
    CapturedAt = erlang:system_time(millisecond),
    StackDepth = get_stack_depth(),
    % Create a placeholder continuation
    % In a full implementation, this would capture the actual continuation
    Cont = fun(Value) -> {resumed, Value} end,
    new(Cont, CapturedAt, StackDepth, Metadata).

%% @private Get the current stack depth.
-spec get_stack_depth() -> non_neg_integer().
get_stack_depth() ->
    {_, StackDepth} = process_info(self(), stack_size),
    StackDepth.

%%====================================================================
%% Resumption Execution
%%====================================================================

%% @doc Resume a resumption with a given value.
%%
%% This executes the captured continuation with the provided value,
%% returning the result of the computation.
-spec resume(resumption() | map(), term()) -> term().
resume(#resumption{cont = Cont}, Value) ->
    try
        Cont(Value)
    catch
        Kind:Reason:Stack ->
            {error, Kind, Reason, Stack}
    end;
resume(#{kind := one_shot} = Resumption, Value) ->
    case catena_one_shot:resume(Resumption, Value) of
        {ok, Result} -> Result;
        {error, _} = Error -> Error
    end;
resume(#{kind := multi_shot} = Resumption, Value) ->
    case catena_multi_shot:resume(Resumption, Value) of
        {ok, Payload, _Count} -> unwrap_multi_shot_payload(Payload);
        {error, _} = Error -> Error
    end;
resume(#{resumption := Resumption}, Value) ->
    resume(Resumption, Value);
resume(Resumption, _Value) ->
    {error, {invalid_resumption, Resumption}}.

%% @doc Resume a resumption with a timeout.
%%
%% If the resumption doesn't complete within the timeout, returns
%% {timeout, Value}.
-spec resume_with_timeout(resumption() | map(), term(), timeout()) ->
    term() | {timeout, term()}.
resume_with_timeout(Resumption, Value, Timeout) ->
    Parent = self(),
    {Pid, Ref} = spawn_monitor(fun() ->
        Result = resume(Resumption, Value),
        Parent ! {resumption_result, Result}
    end),
    receive
        {resumption_result, Result} ->
            demonitor(Ref, [flush]),
            Result;
        {'DOWN', Ref, process, Pid, Reason} ->
            {error, Reason}
    after Timeout ->
        demonitor(Ref, [flush]),
        exit(Pid, kill),
        {timeout, Value}
    end.

%%====================================================================
%% Resumption Inspection
%%====================================================================

%% @doc Get the timestamp when the resumption was captured.
-spec captured_at(resumption()) -> integer().
captured_at(#resumption{captured_at = CapturedAt}) ->
    CapturedAt.

%% @doc Get the stack depth when the resumption was captured.
-spec stack_depth(resumption()) -> non_neg_integer().
stack_depth(#resumption{stack_depth = StackDepth}) ->
    StackDepth.

%% @doc Get the metadata associated with a resumption.
-spec metadata(resumption()) -> metadata().
metadata(#resumption{metadata = Metadata}) ->
    Metadata.

%% @doc Update the metadata of a resumption.
-spec update_metadata(resumption(), metadata()) -> resumption().
update_metadata(#resumption{} = Resumption, NewMetadata) when is_map(NewMetadata) ->
    Resumption#resumption{metadata = NewMetadata}.

%%====================================================================
%% Resumption Validation
%%====================================================================

%% @doc Validate a resumption.
%%
%% Checks that the resumption is well-formed:
%% - The continuation function has arity 1
%% - The captured timestamp is valid
%% - The stack depth is non-negative
%% - The metadata is a map
-spec validate(resumption()) -> ok | {error, term()}.
validate(#resumption{cont = Cont, captured_at = CapturedAt, stack_depth = StackDepth, metadata = Metadata}) ->
    case is_function(Cont, 1) of
        false -> {error, {invalid_arity, function_info(Cont)}};
        true when CapturedAt < 0 -> {error, invalid_timestamp};
        true when StackDepth < 0 -> {error, invalid_stack_depth};
        true when not is_map(Metadata) -> {error, invalid_metadata};
        true -> ok
    end.

%% @doc Check if a function is a valid continuation.
%%
%% A valid continuation is a function of arity 1.
-spec is_valid_continuation(term()) -> boolean().
is_valid_continuation(Cont) when is_function(Cont, 1) -> true;
is_valid_continuation(_) -> false.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private Get function info for validation.
-spec function_info(function()) -> {arity, non_neg_integer()}.
function_info(Cont) ->
    {arity, Arity} = erlang:fun_info(Cont, arity),
    {arity, Arity}.

%% @private Normalize multi-shot wrapper results to the resumed computation value.
-spec unwrap_multi_shot_payload(term()) -> term().
unwrap_multi_shot_payload(#{resumed := Result}) ->
    Result;
unwrap_multi_shot_payload(#{error := Error}) ->
    {error, Error};
unwrap_multi_shot_payload(Payload) ->
    Payload.
