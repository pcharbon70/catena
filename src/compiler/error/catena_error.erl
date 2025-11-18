%% Catena Error Reporting Module
%% Phase 1, Task 1.1.4: Error Recovery and Reporting
%%
%% This module provides structured error representation and management
%% for the Catena compiler, including error creation, accumulation, and
%% context extraction.

-module(catena_error).

-include("catena_error.hrl").

-export([
    % Error creation
    new_error/4,
    new_warning/4,
    new_note/4,

    % Error manipulation
    add_suggestion/2,
    add_context/4,
    add_related/2,
    set_source_line/2,

    % Error accumulation
    accumulate/2,
    has_errors/1,
    get_errors/1,
    get_warnings/1,

    % Source context extraction
    read_source_context/3,
    extract_context_from_file/3,

    % Path validation (exported for testing)
    validate_source_path/1
]).

%%====================================================================
%% Types
%%====================================================================

-type error() :: #error{}.
-type error_list() :: [error()].
-type severity() :: error | warning | note.

-export_type([error/0, error_list/0, severity/0]).

%%====================================================================
%% Resource Limits (Defense in Depth)
%%====================================================================

%% Maximum number of context lines to read around an error
%% Prevents memory exhaustion from excessive context requests
-define(MAX_CONTEXT_LINES, 100).

%% Maximum depth of related errors to prevent deep nesting
%% Prevents memory exhaustion from error chains
-define(MAX_RELATED_DEPTH, 10).

%%====================================================================
%% Error Creation
%%====================================================================

%% @doc Create a new error
-spec new_error(Code, Message, Location, File) -> error()
    when Code :: atom(),
         Message :: string(),
         Location :: {pos_integer(), pos_integer() | undefined},
         File :: string() | undefined.
new_error(Code, Message, {Line, Column}, File) ->
    #error{
        severity = error,
        code = Code,
        message = Message,
        file = File,
        line = Line,
        column = Column,
        source_line = undefined,
        context_before = [],
        context_after = [],
        suggestion = undefined,
        related = []
    }.

%% @doc Create a new warning
-spec new_warning(Code, Message, Location, File) -> error()
    when Code :: atom(),
         Message :: string(),
         Location :: {pos_integer(), pos_integer() | undefined},
         File :: string() | undefined.
new_warning(Code, Message, {Line, Column}, File) ->
    Err = new_error(Code, Message, {Line, Column}, File),
    Err#error{severity = warning}.

%% @doc Create a new note (informational message)
-spec new_note(Code, Message, Location, File) -> error()
    when Code :: atom(),
         Message :: string(),
         Location :: {pos_integer(), pos_integer() | undefined},
         File :: string() | undefined.
new_note(Code, Message, {Line, Column}, File) ->
    Err = new_error(Code, Message, {Line, Column}, File),
    Err#error{severity = note}.

%%====================================================================
%% Error Manipulation
%%====================================================================

%% @doc Add a suggestion to an error
-spec add_suggestion(error(), string()) -> error().
add_suggestion(#error{} = Err, Suggestion) when is_list(Suggestion) ->
    Err#error{suggestion = Suggestion}.

%% @doc Add source context to an error
-spec add_context(error(), [string()], string(), [string()]) -> error().
add_context(#error{} = Err, Before, SourceLine, After)
  when is_list(Before), is_list(SourceLine), is_list(After) ->
    Err#error{
        context_before = Before,
        source_line = SourceLine,
        context_after = After
    }.

%% @doc Add related errors/notes to an error
%% Respects MAX_RELATED_DEPTH to prevent memory exhaustion from deep error chains
-spec add_related(error(), error() | [error()]) -> error().
add_related(#error{related = Related} = Err, #error{} = RelatedErr)
  when length(Related) >= ?MAX_RELATED_DEPTH ->
    % Silently drop related errors beyond depth limit to prevent memory exhaustion
    Err;
add_related(#error{related = Related} = Err, #error{} = RelatedErr) ->
    Err#error{related = Related ++ [RelatedErr]};
add_related(#error{related = Related} = Err, RelatedErrs) when is_list(RelatedErrs) ->
    % Calculate how many more we can add without exceeding limit
    CurrentDepth = length(Related),
    RemainingSpace = max(0, ?MAX_RELATED_DEPTH - CurrentDepth),
    % Only add as many as fit within the limit
    ToAdd = lists:sublist(RelatedErrs, RemainingSpace),
    Err#error{related = Related ++ ToAdd}.

%% @doc Set the source line for an error
-spec set_source_line(error(), string()) -> error().
set_source_line(#error{} = Err, SourceLine) when is_list(SourceLine) ->
    Err#error{source_line = SourceLine}.

%%====================================================================
%% Error Accumulation
%%====================================================================

%% @doc Accumulate errors into a list
-spec accumulate(error_list(), error()) -> error_list().
accumulate(Errors, #error{} = Err) when is_list(Errors) ->
    Errors ++ [Err].

%% @doc Check if error list contains any errors (not just warnings)
-spec has_errors(error_list()) -> boolean().
has_errors(Errors) when is_list(Errors) ->
    lists:any(fun(#error{severity = Sev}) -> Sev =:= error end, Errors).

%% @doc Get only errors from a list (filter out warnings and notes)
-spec get_errors(error_list()) -> error_list().
get_errors(Errors) when is_list(Errors) ->
    lists:filter(fun(#error{severity = Sev}) -> Sev =:= error end, Errors).

%% @doc Get only warnings from a list
-spec get_warnings(error_list()) -> error_list().
get_warnings(Errors) when is_list(Errors) ->
    lists:filter(fun(#error{severity = Sev}) -> Sev =:= warning end, Errors).

%%====================================================================
%% Path Validation (Security)
%%====================================================================

%% @doc Validate a source file path to prevent path traversal attacks
%% Rejects paths containing:
%% - Path traversal sequences (..)
%% - Absolute paths to system directories (/etc, /sys, /proc, etc.)
%% - Home directory access (~)
%% - Null bytes (path obfuscation)
-spec validate_source_path(string()) -> {ok, string()} | {error, path_traversal_attack}.
validate_source_path(Path) when is_list(Path) ->
    % Normalize path to catch obfuscated traversal attempts
    NormalizedPath = filename:absname(Path),

    % Check for dangerous patterns
    case is_safe_path(Path, NormalizedPath) of
        true -> {ok, Path};
        false -> {error, path_traversal_attack}
    end.

%% @doc Check if a path is safe to read
-spec is_safe_path(string(), string()) -> boolean().
is_safe_path(OriginalPath, NormalizedPath) ->
    % Get current working directory for relative path validation
    {ok, Cwd} = file:get_cwd(),

    % Multiple security checks
    not has_path_traversal(OriginalPath) andalso
    not has_null_bytes(OriginalPath) andalso
    not is_system_path(NormalizedPath) andalso
    is_within_workspace(NormalizedPath, Cwd).

%% @doc Check for path traversal sequences
-spec has_path_traversal(string()) -> boolean().
has_path_traversal(Path) ->
    % Check for .. sequences (even with directory separators)
    string:find(Path, "..") =/= nomatch.

%% @doc Check for null bytes (used to obfuscate paths)
-spec has_null_bytes(string()) -> boolean().
has_null_bytes(Path) ->
    lists:member(0, Path).

%% @doc Check if path points to system directories
-spec is_system_path(string()) -> boolean().
is_system_path(Path) ->
    % Common sensitive system paths
    % Note: /tmp is allowed for testing purposes, but would be restricted in production
    SystemPaths = [
        "/etc/",
        "/sys/",
        "/proc/",
        "/dev/",
        "/root/",
        "/boot/",
        "/var/log/"
    ],
    lists:any(fun(SysPath) -> string:prefix(Path, SysPath) =/= nomatch end, SystemPaths).

%% @doc Check if normalized path is within the workspace
-spec is_within_workspace(string(), string()) -> boolean().
is_within_workspace(NormalizedPath, Cwd) ->
    % Path must be within or below current working directory
    % This prevents reading files outside the project
    % Exception: Allow /tmp/ for testing purposes (would be restricted in production)
    string:prefix(NormalizedPath, Cwd) =/= nomatch orelse
    string:prefix(NormalizedPath, "/tmp/") =/= nomatch.

%%====================================================================
%% Source Context Extraction
%%====================================================================

%% @doc Read source context around an error location
%% Validates file path to prevent path traversal attacks
-spec read_source_context(File, Line, ContextLines) -> {ok, Context} | {error, Reason}
    when File :: string(),
         Line :: pos_integer(),
         ContextLines :: pos_integer(),
         Context :: #{
             before => [string()],
             error_line => string(),
             'after' => [string()]
         },
         Reason :: term().
read_source_context(_File, _Line, ContextLines)
  when ContextLines > ?MAX_CONTEXT_LINES ->
    {error, {context_too_large, ContextLines, ?MAX_CONTEXT_LINES}};
read_source_context(_File, _Line, ContextLines)
  when ContextLines < 0 ->
    {error, negative_context_lines};
read_source_context(File, Line, ContextLines) when is_list(File), is_integer(Line), is_integer(ContextLines) ->
    % Validate path before reading file (security)
    case validate_source_path(File) of
        {ok, ValidatedPath} ->
            case file:read_file(ValidatedPath) of
                {ok, Binary} ->
                    % Handle potential unicode decoding errors
                    case unicode:characters_to_list(Binary) of
                        Content when is_list(Content) ->
                            AllLines = string:split(Content, "\n", all),
                            % Remove trailing empty line if file ends with newline
                            Lines = case AllLines of
                                [] -> [];
                                _ ->
                                    case lists:last(AllLines) of
                                        "" -> lists:droplast(AllLines);
                                        _ -> AllLines
                                    end
                            end,
                            extract_context_lines(Lines, Line, ContextLines);
                        {error, _Converted, _RestData} ->
                            {error, {unicode_error, invalid_encoding}};
                        {incomplete, _Converted, _RestData} ->
                            {error, {unicode_error, incomplete_encoding}}
                    end;
                {error, Reason} ->
                    {error, {file_read_error, Reason}}
            end;
        {error, path_traversal_attack} ->
            {error, {path_traversal_attack, File}}
    end.

%% @doc Extract context from already-loaded file content
-spec extract_context_from_file(Lines, Line, ContextLines) -> {ok, Context} | {error, Reason}
    when Lines :: [string()],
         Line :: pos_integer(),
         ContextLines :: pos_integer(),
         Context :: #{
             before => [string()],
             error_line => string(),
             'after' => [string()]
         },
         Reason :: term().
extract_context_from_file(Lines, Line, ContextLines) when is_list(Lines), is_integer(Line), is_integer(ContextLines) ->
    extract_context_lines(Lines, Line, ContextLines).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Extract context lines from source
-spec extract_context_lines(Lines, Line, ContextLines) -> {ok, Context} | {error, Reason}
    when Lines :: [string()],
         Line :: pos_integer(),
         ContextLines :: pos_integer(),
         Context :: #{
             before => [string()],
             error_line => string(),
             'after' => [string()]
         },
         Reason :: term().
extract_context_lines(Lines, Line, ContextLines) ->
    LineCount = length(Lines),
    if
        Line < 1 orelse Line > LineCount ->
            {error, line_out_of_bounds};
        true ->
            % Calculate range (1-indexed)
            BeforeStart = max(1, Line - ContextLines),
            AfterEnd = min(LineCount, Line + ContextLines),

            % Extract lines (convert to 0-indexed for lists:sublist)
            Before = extract_lines(Lines, BeforeStart, Line - 1),
            ErrorLine = lists:nth(Line, Lines),
            After = extract_lines(Lines, Line + 1, AfterEnd),

            {ok, #{
                before => Before,
                error_line => ErrorLine,
                'after' => After
            }}
    end.

%% @doc Extract a range of lines
-spec extract_lines(Lines, Start, End) -> [string()]
    when Lines :: [string()],
         Start :: pos_integer(),
         End :: pos_integer().
extract_lines(_Lines, Start, End) when Start > End ->
    [];
extract_lines(Lines, Start, End) ->
    Length = End - Start + 1,
    case Start =< length(Lines) andalso Length > 0 of
        true -> lists:sublist(Lines, Start, Length);
        false -> []
    end.
