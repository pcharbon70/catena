%%%-------------------------------------------------------------------
%%% @doc Effect Runtime System (Task 1.3.5)
%%%
%%% Implements process-based effect handlers leveraging BEAM's
%%% lightweight processes. Each try/with handler spawns a handler
%%% process that receives perform messages, executes handler operations,
%%% and sends results back.
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
    %% Main API
    perform/3,
    with_handlers/2,

    %% Handler management
    register_handler/2,
    unregister_handler/1,
    get_handler/1,

    %% Builtin effects
    io_handler/0,
    process_handler/0
]).

%% Effect handler timeout (5 seconds)
-define(EFFECT_TIMEOUT, 5000).

%% Process dictionary key for effect handlers
-define(HANDLER_KEY(Effect), {catena_effect_handler, Effect}).

%%====================================================================
%% Main API (1.3.5.1, 1.3.5.2)
%%====================================================================

%% @doc Perform an effect operation
%%
%% Sends a perform message to the registered handler for the given
%% effect and waits for the result.
%%
%% Message protocol (1.3.5.3):
%%   Send: {perform, Effect, Operation, Args, ReplyPid}
%%   Recv: {effect_result, Value}
-spec perform(atom(), atom(), list()) -> term().
perform(Effect, Operation, Args) ->
    case get_handler(Effect) of
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
%% Spawns handler processes for each effect, registers them,
%% executes the body, and cleans up handler processes.
-spec with_handlers(list(), fun(() -> term())) -> term().
with_handlers(HandlerSpecs, BodyFun) ->
    %% Spawn handler processes
    HandlerPids = spawn_handlers(HandlerSpecs),

    try
        %% Execute body
        BodyFun()
    after
        %% Cleanup handler processes
        cleanup_handlers(HandlerPids)
    end.

%%====================================================================
%% Handler Management (1.3.5.1)
%%====================================================================

%% @doc Register a handler process for an effect
-spec register_handler(atom(), pid()) -> ok.
register_handler(Effect, Pid) ->
    put(?HANDLER_KEY(Effect), Pid),
    ok.

%% @doc Unregister a handler for an effect
-spec unregister_handler(atom()) -> ok.
unregister_handler(Effect) ->
    erase(?HANDLER_KEY(Effect)),
    ok.

%% @doc Get the handler process for an effect
-spec get_handler(atom()) -> pid() | undefined.
get_handler(Effect) ->
    get(?HANDLER_KEY(Effect)).

%%====================================================================
%% Handler Spawning (1.3.5.1)
%%====================================================================

%% Spawn handler processes from handler specifications
spawn_handlers(HandlerSpecs) ->
    lists:map(
        fun({Effect, Operations}) ->
            spawn_handler(Effect, Operations)
        end,
        HandlerSpecs
    ).

%% Spawn a single handler process
spawn_handler(Effect, Operations) ->
    CallerPid = self(),
    Pid = spawn_link(fun() ->
        handler_loop(Effect, Operations, CallerPid)
    end),
    register_handler(Effect, Pid),
    {Effect, Pid}.

%% Handler process loop
handler_loop(Effect, Operations, _CallerPid) ->
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
            handler_loop(Effect, Operations, _CallerPid);

        stop ->
            ok;

        _Other ->
            handler_loop(Effect, Operations, _CallerPid)
    end.

%% Cleanup handler processes
cleanup_handlers(HandlerPids) ->
    lists:foreach(
        fun({Effect, Pid}) ->
            unregister_handler(Effect),
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
            case file:read_file(ValidPath) of
                {ok, Content} ->
                    Content;
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
    spawn(fun() -> Fun() end).

process_send(Pid, Msg) ->
    Pid ! Msg,
    ok.

process_self() ->
    self().

%%====================================================================
%% Utilities
%%====================================================================

%% Validate IO path for security
%% Blocks: path traversal (..), null bytes, system paths
%% Allows: absolute paths to /tmp and user directories
-spec validate_io_path(string()) -> {ok, string()} | {error, term()}.
validate_io_path(Path) ->
    NormalizedPath = filename:absname(Path),
    case is_safe_io_path(Path, NormalizedPath) of
        true -> {ok, NormalizedPath};
        false -> {error, {path_security_error, Path}}
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
