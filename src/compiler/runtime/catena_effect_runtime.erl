%%%-------------------------------------------------------------------
%%% @doc Effect Runtime System (Task 1.3.5)
%%%
%%% Implements process-based effect handlers leveraging BEAM's
%%% lightweight processes. Each try/with handler spawns a handler
%%% process that receives perform messages, executes handler operations,
%%% and sends results back.
%%%
%%% Uses explicit context passing instead of process dictionary for
%%% better composability across processes and proper nesting support.
%%%
%%% This module handles:
%%% - Handler process spawning (1.3.5.1)
%%% - Perform operation execution (1.3.5.2)
%%% - Effect message protocol (1.3.5.3)
%%% - Builtin IO effect handler (1.3.5.4)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_effect_runtime).

-export([
    %% Context creation
    empty_context/0,
    new_context/0,

    %% Main API
    perform/4,
    with_handlers/3,

    %% Builtin effects
    io_handler/0,
    process_handler/0
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type effect_context() :: #{
    handlers := #{atom() => pid()},
    parent := effect_context() | undefined
}.

-export_type([effect_context/0]).

%% Effect handler timeout (5 seconds)
-define(EFFECT_TIMEOUT, 5000).

%% Default maximum allowed processes to prevent DoS
%% Note: Erlang default limit is 262144, max is ~134 million
-define(DEFAULT_MAX_PROCESS_COUNT, 50000).

%% Default maximum file size for readFile (10 MB)
-define(DEFAULT_MAX_FILE_SIZE, 10485760).

%%====================================================================
%% Context Creation
%%====================================================================

%% @doc Create an empty effect context
-spec empty_context() -> effect_context().
empty_context() ->
    #{handlers => #{}, parent => undefined}.

%% @doc Create a new effect context (alias for empty_context)
-spec new_context() -> effect_context().
new_context() ->
    empty_context().

%%====================================================================
%% Main API (1.3.5.1, 1.3.5.2)
%%====================================================================

%% @doc Perform an effect operation with explicit context
%%
%% Looks up handler in context (walking parent chain if needed),
%% sends a perform message and waits for the result.
%%
%% Message protocol (1.3.5.3):
%%   Send: {perform, Effect, Operation, Args, ReplyPid}
%%   Recv: {effect_result, Value}
-spec perform(effect_context(), atom(), atom(), list()) -> term().
perform(Ctx, Effect, Operation, Args) ->
    case get_handler(Ctx, Effect) of
        undefined ->
            %% No handler registered - try builtin effects
            perform_builtin(Effect, Operation, Args);
        HandlerPid ->
            %% Send perform message to handler
            HandlerPid ! {perform, Effect, Operation, Args, self()},
            %% Wait for result
            receive
                {effect_result, Value} ->
                    Value;
                {effect_error, Reason} ->
                    erlang:error({effect_error, Effect, Operation, Reason})
            after ?EFFECT_TIMEOUT ->
                erlang:error({effect_timeout, Effect, Operation})
            end
    end.

%% @doc Execute body with effect handlers
%%
%% Spawns handler processes for each effect, creates a child context
%% with the new handlers, executes the body, and cleans up handler processes.
-spec with_handlers(effect_context(), list(), fun((effect_context()) -> T)) -> T.
with_handlers(Ctx, HandlerSpecs, BodyFun) ->
    %% Spawn handler processes and collect their PIDs
    {HandlerPids, NewHandlers} = spawn_handlers(HandlerSpecs),

    %% Create child context with new handlers merged in
    ChildCtx = #{
        handlers => maps:merge(maps:get(handlers, Ctx), NewHandlers),
        parent => Ctx
    },

    try
        %% Execute body with child context
        BodyFun(ChildCtx)
    after
        %% Cleanup handler processes
        cleanup_handlers(HandlerPids)
    end.

%%====================================================================
%% Handler Lookup
%%====================================================================

%% @doc Get handler for effect, walking parent chain if needed
-spec get_handler(effect_context(), atom()) -> pid() | undefined.
get_handler(Ctx, Effect) ->
    Handlers = maps:get(handlers, Ctx),
    case maps:get(Effect, Handlers, undefined) of
        undefined ->
            %% Not in current context, check parent
            case maps:get(parent, Ctx) of
                undefined -> undefined;
                ParentCtx -> get_handler(ParentCtx, Effect)
            end;
        HandlerPid ->
            HandlerPid
    end.

%%====================================================================
%% Handler Spawning (1.3.5.1)
%%====================================================================

%% Spawn handler processes from handler specifications
%% Returns {ListOfPids, MapOfEffectToPid}
-spec spawn_handlers(list()) -> {list({atom(), pid()}), #{atom() => pid()}}.
spawn_handlers(HandlerSpecs) ->
    lists:foldl(
        fun({Effect, Operations}, {PidList, PidMap}) ->
            Pid = spawn_link(fun() ->
                handler_loop(Effect, Operations)
            end),
            {[{Effect, Pid} | PidList], maps:put(Effect, Pid, PidMap)}
        end,
        {[], #{}},
        HandlerSpecs
    ).

%% Handler process loop
handler_loop(Effect, Operations) ->
    receive
        {perform, Effect, Operation, Args, ReplyPid} ->
            %% Find operation handler
            case lists:keyfind(Operation, 1, Operations) of
                {Operation, HandlerFun} ->
                    %% Execute handler
                    try
                        Result = apply(HandlerFun, Args),
                        ReplyPid ! {effect_result, Result}
                    catch
                        Class:Reason:_Stack ->
                            ReplyPid ! {effect_error, {Class, Reason}}
                    end;
                false ->
                    ReplyPid ! {effect_error, {unknown_operation, Operation}}
            end,
            handler_loop(Effect, Operations);

        stop ->
            ok;

        _Other ->
            handler_loop(Effect, Operations)
    end.

%% Cleanup handler processes
cleanup_handlers(HandlerPids) ->
    lists:foreach(
        fun({_Effect, Pid}) ->
            Pid ! stop
        end,
        HandlerPids
    ).

%%====================================================================
%% Builtin Effects (1.3.5.4)
%%====================================================================

%% Perform builtin effect operations
perform_builtin('IO', Operation, Args) ->
    perform_io(Operation, Args);
perform_builtin('Process', Operation, Args) ->
    perform_process(Operation, Args);
perform_builtin(Effect, Operation, _Args) ->
    erlang:error({no_handler_for_effect, Effect, Operation}).

%%====================================================================
%% IO Effect Handler (1.3.5.4)
%%====================================================================

%% @doc Get the builtin IO effect handler specification
-spec io_handler() -> {atom(), list()}.
io_handler() ->
    {'IO', [
        {print, fun io_print/1},
        {println, fun io_println/1},
        {readFile, fun io_read_file/1},
        {writeFile, fun io_write_file/2},
        {getLine, fun io_get_line/0}
    ]}.

%% Perform IO operations directly (for builtin handler)
perform_io(print, [Text]) ->
    io_print(Text);
perform_io(println, [Text]) ->
    io_println(Text);
perform_io(readFile, [Path]) ->
    io_read_file(Path);
perform_io(writeFile, [Path, Content]) ->
    io_write_file(Path, Content);
perform_io(getLine, []) ->
    io_get_line();
perform_io(Operation, _Args) ->
    erlang:error({unknown_io_operation, Operation}).

%% IO operation implementations
io_print(Text) ->
    io:format("~s", [to_string(Text)]),
    ok.

io_println(Text) ->
    io:format("~s~n", [to_string(Text)]),
    ok.

io_read_file(Path) ->
    PathStr = path_to_string(Path),
    case validate_io_path(PathStr) of
        {ok, ValidPath} ->
            %% Check file size before reading
            MaxSize = application:get_env(catena, max_file_size, ?DEFAULT_MAX_FILE_SIZE),
            case file:read_file_info(ValidPath) of
                {ok, FileInfo} ->
                    FileSize = element(2, FileInfo),  %% size field
                    case FileSize > MaxSize of
                        true ->
                            erlang:error({io_error, readFile, {file_too_large, FileSize, MaxSize}});
                        false ->
                            case file:read_file(ValidPath) of
                                {ok, Content} ->
                                    Content;
                                {error, Reason} ->
                                    erlang:error({io_error, readFile, Reason})
                            end
                    end;
                {error, Reason} ->
                    erlang:error({io_error, readFile, Reason})
            end;
        {error, Reason} ->
            erlang:error({io_error, readFile, Reason})
    end.

io_write_file(Path, Content) ->
    PathStr = path_to_string(Path),
    case validate_io_path(PathStr) of
        {ok, ValidPath} ->
            case file:write_file(ValidPath, to_string(Content)) of
                ok ->
                    ok;
                {error, Reason} ->
                    erlang:error({io_error, writeFile, Reason})
            end;
        {error, Reason} ->
            erlang:error({io_error, writeFile, Reason})
    end.

io_get_line() ->
    case io:get_line("") of
        eof ->
            <<>>;
        {error, Reason} ->
            erlang:error({io_error, getLine, Reason});
        Line ->
            %% Remove trailing newline
            list_to_binary(string:trim(Line, trailing, "\n"))
    end.

%%====================================================================
%% Process Effect Handler
%%====================================================================

%% @doc Get the builtin Process effect handler specification
-spec process_handler() -> {atom(), list()}.
process_handler() ->
    {'Process', [
        {spawn, fun process_spawn/1},
        {send, fun process_send/2},
        {self, fun process_self/0}
    ]}.

%% Perform Process operations directly (for builtin handler)
perform_process(spawn, [Fun]) ->
    process_spawn(Fun);
perform_process(send, [Pid, Msg]) ->
    process_send(Pid, Msg);
perform_process(self, []) ->
    process_self();
perform_process(Operation, _Args) ->
    erlang:error({unknown_process_operation, Operation}).

%% Process operation implementations
process_spawn(Fun) ->
    %% Check process count to prevent DoS
    MaxCount = application:get_env(catena, max_process_count, ?DEFAULT_MAX_PROCESS_COUNT),
    case erlang:system_info(process_count) of
        Count when Count >= MaxCount ->
            erlang:error({process_limit_exceeded, Count, MaxCount});
        _ ->
            spawn(fun() -> Fun() end)
    end.

process_send(Pid, Msg) ->
    Pid ! Msg,
    ok.

process_self() ->
    self().

%%====================================================================
%% Utilities
%%====================================================================

%% Validate IO path for security
%% Blocks: path traversal (..), null bytes, system paths, symlinks to restricted paths
%% Allows: absolute paths to /tmp and user directories
-spec validate_io_path(string()) -> {ok, string()} | {error, term()}.
validate_io_path(Path) ->
    NormalizedPath = filename:absname(Path),
    case is_safe_io_path(Path, NormalizedPath) of
        true ->
            %% Also validate symlink target if path is a symlink
            case resolve_symlinks(NormalizedPath, 10) of
                {ok, ResolvedPath} ->
                    case is_system_path(ResolvedPath) of
                        true -> {error, {symlink_to_restricted_path, Path, ResolvedPath}};
                        false -> {ok, NormalizedPath}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false -> {error, {path_security_error, Path}}
    end.

%% Resolve symlinks to get final target path
%% MaxDepth prevents infinite loops from circular symlinks
-spec resolve_symlinks(string(), non_neg_integer()) -> {ok, string()} | {error, term()}.
resolve_symlinks(_Path, 0) ->
    {error, symlink_loop_detected};
resolve_symlinks(Path, MaxDepth) ->
    case file:read_link_info(Path) of
        {ok, FileInfo} ->
            case element(3, FileInfo) of  %% type field
                symlink ->
                    case file:read_link(Path) of
                        {ok, Target} ->
                            %% Resolve relative symlinks
                            AbsTarget = case Target of
                                [$/ | _] -> Target;
                                _ -> filename:absname(Target, filename:dirname(Path))
                            end,
                            resolve_symlinks(AbsTarget, MaxDepth - 1);
                        {error, Reason} ->
                            {error, {symlink_read_error, Reason}}
                    end;
                _ ->
                    {ok, Path}
            end;
        {error, enoent} ->
            %% File doesn't exist yet (for writes), that's ok
            {ok, Path};
        {error, Reason} ->
            {error, {file_info_error, Reason}}
    end.

is_safe_io_path(OriginalPath, NormalizedPath) ->
    not has_path_traversal(OriginalPath) andalso
    not has_null_bytes(OriginalPath) andalso
    not is_system_path(NormalizedPath).

%% Check for path traversal sequences
has_path_traversal(Path) ->
    string:find(Path, "..") =/= nomatch.

%% Check for null bytes (used to obfuscate paths)
has_null_bytes(Path) ->
    lists:member(0, Path).

%% Check if path is a protected system directory
is_system_path(Path) ->
    SystemPaths = ["/etc", "/sys", "/proc", "/dev", "/root", "/boot", "/var/log"],
    lists:any(fun(Prefix) ->
        lists:prefix(Prefix, Path)
    end, SystemPaths).

%% Convert various types to string/binary for IO
to_string(Bin) when is_binary(Bin) -> Bin;
to_string(List) when is_list(List) -> list_to_binary(List);
to_string(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_string(Int) when is_integer(Int) -> integer_to_binary(Int);
to_string(Float) when is_float(Float) -> float_to_binary(Float);
to_string(Other) -> list_to_binary(io_lib:format("~p", [Other])).

%% Convert path to string (list) for validation
path_to_string(Bin) when is_binary(Bin) -> binary_to_list(Bin);
path_to_string(List) when is_list(List) -> List;
path_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom).
