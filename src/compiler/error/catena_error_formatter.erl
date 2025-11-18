%% Catena Error Formatter Module
%% Phase 1, Task 1.1.4: Error Recovery and Reporting
%%
%% This module provides ANSI-colored terminal output formatting for
%% compiler errors, warnings, and notes with source code context.

-module(catena_error_formatter).

-include("catena_error.hrl").

-export([
    % Main formatting functions
    format_error/1,
    format_error_list/1,
    format_error_simple/1,

    % Color support detection
    supports_color/0,
    set_color_mode/1,

    % ANSI color functions (exported for testing)
    red/1,
    yellow/1,
    blue/1,
    cyan/1,
    bold/1,
    dim/1,
    reset/0,

    % Security functions (exported for testing)
    sanitize_ansi/1
]).

%%====================================================================
%% Security - ANSI Sanitization
%%====================================================================

%% @doc Sanitize user-controlled strings to prevent ANSI injection attacks
%% Removes all ANSI escape sequences from input text
-spec sanitize_ansi(iolist() | string() | binary()) -> string().
sanitize_ansi(Text) when is_binary(Text) ->
    sanitize_ansi(binary_to_list(Text));
sanitize_ansi(Text) when is_list(Text) ->
    % Handle both strings and iolists
    FlatText = lists:flatten(Text),
    % Remove ANSI escape sequences using regex-like pattern matching
    strip_ansi_codes(FlatText).

%% @doc Strip ANSI escape codes from a flat string
-spec strip_ansi_codes(string()) -> string().
strip_ansi_codes([]) ->
    [];
strip_ansi_codes([$\e | Rest]) ->
    % Found escape sequence, skip until we find a letter (end of ANSI code)
    strip_ansi_codes(skip_ansi_sequence(Rest));
strip_ansi_codes([C | Rest]) ->
    [C | strip_ansi_codes(Rest)].

%% @doc Skip characters until we find the end of an ANSI sequence
%% ANSI sequences end with a letter (a-zA-Z)
-spec skip_ansi_sequence(string()) -> string().
skip_ansi_sequence([]) ->
    [];
skip_ansi_sequence([C | Rest]) when (C >= $a andalso C =< $z) orelse
                                     (C >= $A andalso C =< $Z) ->
    % Found end of ANSI sequence (letter), continue from next char
    Rest;
skip_ansi_sequence([_ | Rest]) ->
    % Still in ANSI sequence, keep skipping
    skip_ansi_sequence(Rest).

%%====================================================================
%% Color Mode State
%%====================================================================

% We use process dictionary to store color mode preference
% Default: auto-detect based on terminal
-define(COLOR_MODE_KEY, catena_error_formatter_color_mode).

-type color_mode() :: always | never | auto.

%%====================================================================
%% Main Formatting Functions
%%====================================================================

%% @doc Format a single error with full context and colors
-spec format_error(#error{}) -> iolist().
format_error(#error{} = Err) ->
    [
        format_error_header(Err),
        format_location(Err),
        format_source_context(Err),
        format_suggestion(Err),
        format_related(Err)
    ].

%% @doc Format a list of errors
-spec format_error_list([#error{}]) -> iolist().
format_error_list([]) ->
    "\n\n";  % Special case for empty list
format_error_list(Errors) when is_list(Errors) ->
    Formatted = [format_error(Err) || Err <- Errors],
    Summary = format_summary(Errors),
    [lists:join("\n", Formatted), "\n", Summary].

%% @doc Format error without context (one-line)
-spec format_error_simple(#error{}) -> iolist().
format_error_simple(#error{severity = Sev, code = Code, message = Msg, file = File, line = Line, column = Col}) ->
    SevStr = severity_string(Sev),
    % Sanitize user-controlled content
    SafeMsg = sanitize_ansi(Msg),
    Location = case File of
        undefined -> io_lib:format("~p", [Line]);
        _ ->
            SafeFile = sanitize_ansi(File),
            io_lib:format("~s:~p", [SafeFile, Line])
    end,
    ColStr = case Col of
        undefined -> "";
        _ -> io_lib:format(":~p", [Col])
    end,
    [colorize_severity(Sev, SevStr), " [", atom_to_list(Code), "]: ", SafeMsg, " at ", Location, ColStr, "\n"].

%%====================================================================
%% Color Support Detection
%%====================================================================

%% @doc Check if terminal supports ANSI colors
-spec supports_color() -> boolean().
supports_color() ->
    case get_color_mode() of
        always -> true;
        never -> false;
        auto -> auto_detect_color_support()
    end.

%% @doc Set color mode (always/never/auto)
-spec set_color_mode(color_mode()) -> ok.
set_color_mode(Mode) when Mode =:= always; Mode =:= never; Mode =:= auto ->
    put(?COLOR_MODE_KEY, Mode),
    ok.

%% @doc Get current color mode
get_color_mode() ->
    case get(?COLOR_MODE_KEY) of
        undefined -> auto;  % Default to auto-detect
        Mode -> Mode
    end.

%% @doc Auto-detect color support from environment
auto_detect_color_support() ->
    % Check NO_COLOR environment variable (standard)
    case os:getenv("NO_COLOR") of
        false ->
            % NO_COLOR not set, check TERM
            case os:getenv("TERM") of
                false -> false;          % No TERM, no colors
                "dumb" -> false;         % Dumb terminal
                _ -> true                % Assume colors supported
            end;
        _ ->
            % NO_COLOR is set (any value), disable colors
            false
    end.

%%====================================================================
%% ANSI Color Functions
%%====================================================================

%% @doc Red text (for errors)
-spec red(iolist()) -> iolist().
red(Text) -> colorize("\e[31m", Text).

%% @doc Yellow text (for warnings)
-spec yellow(iolist()) -> iolist().
yellow(Text) -> colorize("\e[33m", Text).

%% @doc Blue text (for notes)
-spec blue(iolist()) -> iolist().
blue(Text) -> colorize("\e[34m", Text).

%% @doc Cyan text (for hints/help)
-spec cyan(iolist()) -> iolist().
cyan(Text) -> colorize("\e[36m", Text).

%% @doc Bold text
-spec bold(iolist()) -> iolist().
bold(Text) -> colorize("\e[1m", Text).

%% @doc Dim text (for context lines)
-spec dim(iolist()) -> iolist().
dim(Text) -> colorize("\e[2m", Text).

%% @doc Reset all formatting
-spec reset() -> string().
reset() -> "\e[0m".

%% @doc Apply ANSI code if colors supported
colorize(AnsiCode, Text) ->
    case supports_color() of
        true -> [AnsiCode, Text, reset()];
        false -> Text
    end.

%%====================================================================
%% Internal Formatting Functions
%%====================================================================

%% @doc Format error header (severity and message)
format_error_header(#error{severity = Sev, code = Code, message = Msg}) ->
    SevStr = severity_string(Sev),
    CodeStr = atom_to_list(Code),
    % Sanitize user-controlled message to prevent ANSI injection
    SafeMsg = sanitize_ansi(Msg),
    [
        colorize_severity(Sev, [bold([SevStr, "[", CodeStr, "]"])]),
        ": ",
        SafeMsg,
        "\n"
    ].

%% @doc Format file location
format_location(#error{file = undefined}) ->
    [];
format_location(#error{file = File, line = Line, column = undefined}) ->
    % Sanitize filename to prevent ANSI injection
    SafeFile = sanitize_ansi(File),
    [dim(["  --> ", SafeFile, ":", integer_to_list(Line), "\n"])];
format_location(#error{file = File, line = Line, column = Col}) ->
    % Sanitize filename to prevent ANSI injection
    SafeFile = sanitize_ansi(File),
    [dim(["  --> ", SafeFile, ":", integer_to_list(Line), ":", integer_to_list(Col), "\n"])].

%% @doc Format source code context with highlighting
format_source_context(#error{context_before = [], source_line = undefined, context_after = []}) ->
    [];
format_source_context(#error{line = Line, column = Col, context_before = Before, source_line = SourceLine, context_after = After}) ->
    [
        format_context_lines(Before, Line - length(Before)),
        format_error_line(SourceLine, Line, Col),
        format_context_lines(After, Line + 1),
        "   |\n"
    ].

%% @doc Format context lines (dimmed)
format_context_lines([], _StartLine) ->
    [];
format_context_lines(Lines, StartLine) ->
    Formatted = lists:map(
        fun({LineNum, Text}) ->
            LineNumStr = string:pad(integer_to_list(LineNum), 4, leading, $ ),
            % Sanitize context line text to prevent ANSI injection
            SafeText = sanitize_ansi(Text),
            [dim([LineNumStr, " | ", SafeText, "\n"])]
        end,
        lists:zip(lists:seq(StartLine, StartLine + length(Lines) - 1), Lines)
    ),
    ["   |\n" | Formatted].

%% @doc Format the error line with highlighting
format_error_line(undefined, _Line, _Col) ->
    [];
format_error_line(SourceLine, Line, Col) ->
    LineNumStr = string:pad(integer_to_list(Line), 4, leading, $ ),
    % Sanitize source line to prevent ANSI injection
    SafeSourceLine = sanitize_ansi(SourceLine),
    Highlight = format_highlight(SafeSourceLine, Col),
    [
        dim([LineNumStr, " | "]),
        SafeSourceLine,
        "\n",
        dim(["     | "]),
        Highlight,
        "\n"
    ].

%% @doc Format column highlight (caret or wavy underline)
format_highlight(_SourceLine, undefined) ->
    "";
format_highlight(_SourceLine, Col) when Col > 0 ->
    % Create highlighting: spaces up to column, then ^
    Spaces = lists:duplicate(Col - 1, $ ),
    [red([Spaces, "^"])];
format_highlight(_SourceLine, _Col) ->
    "".

%% @doc Format suggestion if present
format_suggestion(#error{suggestion = undefined}) ->
    [];
format_suggestion(#error{suggestion = Sugg}) ->
    % Sanitize suggestion to prevent ANSI injection
    SafeSugg = sanitize_ansi(Sugg),
    [cyan(["help: ", SafeSugg, "\n"])].

%% @doc Format related errors/notes
format_related(#error{related = []}) ->
    [];
format_related(#error{related = Related}) ->
    ["\n", [format_error_simple(R) || R <- Related]].

%% @doc Format error summary
format_summary([]) ->
    [];  % No summary for empty list
format_summary(Errors) ->
    ErrorCount = length([E || E <- Errors, E#error.severity =:= error]),
    WarnCount = length([E || E <- Errors, E#error.severity =:= warning]),
    Parts = [
        case ErrorCount of
            0 -> [];
            1 -> red(["1 error"]);
            N -> red([integer_to_list(N), " errors"])
        end,
        case {ErrorCount, WarnCount} of
            {0, _} -> [];
            {_, 0} -> [];
            _ -> ", "
        end,
        case WarnCount of
            0 -> [];
            1 -> yellow(["1 warning"]);
            N -> yellow([integer_to_list(N), " warnings"])
        end
    ],
    ["\n", lists:flatten(Parts), "\n"].

%% @doc Get severity string
severity_string(error) -> "error";
severity_string(warning) -> "warning";
severity_string(note) -> "note".

%% @doc Colorize severity
colorize_severity(error, Text) -> red(Text);
colorize_severity(warning, Text) -> yellow(Text);
colorize_severity(note, Text) -> blue(Text).
