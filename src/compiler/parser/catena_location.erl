-module(catena_location).
-export([
    new/2, new/4,
    get_line/1, get_column/1, get_end_line/1, get_end_column/1,
    from_token/1, from_token/2,
    span/2,
    format/1,
    is_location/1
]).

%%% @doc Location tracking for precise error reporting.
%%%
%%% This module provides comprehensive location tracking for source code positions,
%%% enabling precise error reporting with line and column information. It supports
%%% both single-position locations and span locations covering multiple characters
%%% or lines.
%%%
%%% == Location Formats ==
%%%
%%% <ul>
%%%   <li><b>Single position</b>: `{location, Line, Column}' - A single point in
%%%       the source code</li>
%%%   <li><b>Span</b>: `{location, StartLine, StartCol, EndLine, EndCol}' - A range
%%%       spanning from one position to another</li>
%%% </ul>
%%%
%%% == Key Features ==
%%%
%%% <ul>
%%%   <li><b>Construction</b> - Create locations from line/column pairs or lexer tokens</li>
%%%   <li><b>Accessors</b> - Extract line and column information from locations</li>
%%%   <li><b>Spanning</b> - Combine two locations into a range</li>
%%%   <li><b>Formatting</b> - Convert locations to human-readable strings</li>
%%%   <li><b>Validation</b> - Check if a term is a valid location</li>
%%% </ul>
%%%
%%% == Usage Example ==
%%%
%%% ```
%%% %% Create single position
%%% Loc1 = catena_location:new(5, 10),  %% → {location, 5, 10}
%%%
%%% %% Create span
%%% Loc2 = catena_location:new(5, 10, 5, 25),  %% → {location, 5, 10, 5, 25}
%%%
%%% %% Create from token
%%% Loc3 = catena_location:from_token({integer, 42, 123}),  %% → {location, 42, 0}
%%%
%%% %% Format for error messages
%%% Msg = catena_location:format(Loc1),  %% → "5:10"
%%%
%%% %% Combine locations
%%% Span = catena_location:span(Loc1, Loc2),  %% → {location, 5, 10, 5, 25}
%%% '''
%%%
%%% @author Catena Contributors
%%% @version 1.0.0
%%% @since 2025-11-08

-type line() :: pos_integer().
-type column() :: non_neg_integer().
-type location() :: {location, line(), column()} |
                    {location, line(), column(), line(), column()}.

-export_type([location/0, line/0, column/0]).

%%============================================================================
%% Construction Functions
%%============================================================================

%% @doc Create a single-position location.
%%
%% Creates a location representing a single point in the source code,
%% specified by line number and column offset.
%%
%% Line numbers start at 1, column numbers start at 0 (0-indexed).
%%
%% @param Line The line number (must be > 0)
%% @param Col The column number (must be >= 0)
%% @returns A location tuple `{location, Line, Col}'
%%
%% @see new/4
%% @see from_token/1
%%
%% @example
%% ```
%% %% Line 1, column 0 (first character)
%% Loc1 = new(1, 0),  %% → {location, 1, 0}
%%%
%% %% Line 5, column 10 (11th character on line 5)
%% Loc2 = new(5, 10),  %% → {location, 5, 10}
%% '''
-spec new(line(), column()) -> location().
new(Line, Col) when is_integer(Line), Line > 0, is_integer(Col), Col >= 0 ->
    {location, Line, Col}.

%% @doc Create a span location covering a range.
%%
%% Creates a location representing a range in the source code, spanning
%% from a start position to an end position. The span can be on a single
%% line or across multiple lines.
%%
%% Constraints:
%% <ul>
%%%   <li>End position must be after or equal to start position</li>
%%%   <li>Line numbers start at 1, columns start at 0</li>
%%%   <li>If on same line, EndCol must be >= StartCol</li>
%% </ul>
%%
%% @param StartLine The starting line number (must be > 0)
%% @param StartCol The starting column number (must be >= 0)
%% @param EndLine The ending line number (must be > 0)
%% @param EndCol The ending column number (must be >= 0)
%% @returns A span location tuple `{location, StartLine, StartCol, EndLine, EndCol}'
%%
%% @see new/2
%% @see span/2
%%
%% @example
%% ```
%% %% Single line span: line 5, columns 10-25
%% Loc1 = new(5, 10, 5, 25),  %% → {location, 5, 10, 5, 25}
%%%
%% %% Multi-line span: from line 3, col 5 to line 7, col 12
%% Loc2 = new(3, 5, 7, 12),  %% → {location, 3, 5, 7, 12}
%% '''
-spec new(line(), column(), line(), column()) -> location().
new(StartLine, StartCol, EndLine, EndCol)
    when is_integer(StartLine), StartLine > 0,
         is_integer(StartCol), StartCol >= 0,
         is_integer(EndLine), EndLine > 0,
         is_integer(EndCol), EndCol >= 0,
         (EndLine > StartLine) orelse (EndLine =:= StartLine andalso EndCol >= StartCol) ->
    {location, StartLine, StartCol, EndLine, EndCol}.

%% @doc Create location from a lexer token.
%%
%% Converts a Leex-generated token into a location. Leex tokens have format:
%% <ul>
%%%   <li>`{TokenName, Line}' - Simple token without value</li>
%%%   <li>`{TokenName, Line, Value}' - Token with associated value</li>
%% </ul>
%%
%% The resulting location has column set to 0 (unknown column position).
%%
%% @param Token A Leex token tuple
%% @returns A single-position location `{location, Line, 0}'
%%
%% @see from_token/2
%% @see new/2
%%
%% @example
%% ```
%% %% From simple token
%% Loc1 = from_token({plus, 5}),  %% → {location, 5, 0}
%%%
%% %% From value token
%% Loc2 = from_token({integer, 10, 42}),  %% → {location, 10, 0}
%% '''
-spec from_token(tuple()) -> location().
from_token({_TokenName, Line}) when is_integer(Line) ->
    {location, Line, 0};
from_token({_TokenName, Line, _Value}) when is_integer(Line) ->
    {location, Line, 0}.

%% @doc Create span location from token with length information.
%%
%% Converts a Leex token into a span location, using the provided or
%% inferred length to calculate the end position. For string values,
%% the length is automatically calculated from the value.
%%
%% @param Token A Leex token tuple
%% @param Length The token length in characters (ignored for string values)
%% @returns A span location `{location, Line, 0, Line, Length}'
%%
%% @see from_token/1
%% @see new/4
%%
%% @example
%% ```
%% %% Simple token with explicit length
%% Loc1 = from_token({plus, 5}, 1),  %% → {location, 5, 0, 5, 1}
%%%
%% %% String token (length inferred from value)
%% Loc2 = from_token({string, 10, "hello"}, 0),  %% → {location, 10, 0, 10, 5}
%%%
%% %% Integer token with explicit length
%% Loc3 = from_token({integer, 3, 42}, 2),  %% → {location, 3, 0, 3, 2}
%% '''
-spec from_token(tuple(), non_neg_integer()) -> location().
from_token({_TokenName, Line}, Length) when is_integer(Line), is_integer(Length) ->
    {location, Line, 0, Line, Length};
from_token({_TokenName, Line, Value}, _Length) when is_integer(Line), is_list(Value) ->
    Len = length(Value),
    {location, Line, 0, Line, Len};
from_token({_TokenName, Line, _Value}, Length) when is_integer(Line), is_integer(Length) ->
    {location, Line, 0, Line, Length}.

%% @doc Combine two locations into a span.
%%
%% Creates a new span location that encompasses both input locations,
%% from the earliest start position to the latest end position.
%%
%% Handles all combinations:
%% <ul>
%%%   <li>Two single positions → span from first to second</li>
%%%   <li>Single + span → expanded span</li>
%%%   <li>Two spans → span from earliest start to latest end</li>
%% </ul>
%%
%% The resulting span is always ordered correctly (start before end),
%% regardless of the order of the input locations.
%%
%% @param Loc1 The first location
%% @param Loc2 The second location
%% @returns A span location covering both inputs
%%
%% @see new/4
%%
%% @example
%% ```
%% %% Combine two single positions
%% Loc1 = {location, 5, 10},
%% Loc2 = {location, 5, 25},
%% Span1 = span(Loc1, Loc2),  %% → {location, 5, 10, 5, 25}
%%%
%% %% Combine spans (takes earliest start and latest end)
%% Span2 = {location, 3, 0, 5, 10},
%% Span3 = {location, 5, 15, 7, 20},
%% Span4 = span(Span2, Span3),  %% → {location, 3, 0, 7, 20}
%%%
%% %% Order doesn't matter
%% Span5 = span(Loc2, Loc1),  %% → {location, 5, 10, 5, 25} (same as Span1)
%% '''
-spec span(location(), location()) -> location().
span({location, Line1, Col1}, {location, Line2, Col2}) ->
    {StartLine, StartCol} = if Line1 < Line2 -> {Line1, Col1};
                               Line1 =:= Line2, Col1 =< Col2 -> {Line1, Col1};
                               true -> {Line2, Col2}
                            end,
    {EndLine, EndCol} = if Line2 > Line1 -> {Line2, Col2};
                           Line2 =:= Line1, Col2 >= Col1 -> {Line2, Col2};
                           true -> {Line1, Col1}
                        end,
    {location, StartLine, StartCol, EndLine, EndCol};
span({location, Line1, Col1, _, _}, {location, _, _, Line2, Col2}) ->
    {location, Line1, Col1, Line2, Col2};
span({location, Line1, Col1}, {location, _, _, Line2, Col2}) ->
    {location, Line1, Col1, Line2, Col2};
span({location, Line1, Col1, _, _}, {location, Line2, Col2}) ->
    {location, Line1, Col1, Line2, Col2}.

%%============================================================================
%% Accessor Functions
%%============================================================================

%% @doc Get the starting line number.
%%
%% Extracts the starting line number from a location, whether it's a
%% single position or a span.
%%
%% @param Location A location tuple
%% @returns The starting line number (>= 1)
%%
%% @see get_column/1
%% @see get_end_line/1
%%
%% @example
%% ```
%% Line1 = get_line({location, 5, 10}),  %% → 5
%% Line2 = get_line({location, 5, 10, 7, 20}),  %% → 5
%% '''
-spec get_line(location()) -> line().
get_line({location, Line, _Col}) -> Line;
get_line({location, Line, _Col, _EndLine, _EndCol}) -> Line.

%% @doc Get the starting column number.
%%
%% Extracts the starting column number from a location, whether it's a
%% single position or a span.
%%
%% @param Location A location tuple
%% @returns The starting column number (>= 0)
%%
%% @see get_line/1
%% @see get_end_column/1
%%
%% @example
%% ```
%% Col1 = get_column({location, 5, 10}),  %% → 10
%% Col2 = get_column({location, 5, 10, 7, 20}),  %% → 10
%% '''
-spec get_column(location()) -> column().
get_column({location, _Line, Col}) -> Col;
get_column({location, _Line, Col, _EndLine, _EndCol}) -> Col.

%% @doc Get the ending line number.
%%
%% Extracts the ending line number from a location. For single positions,
%% this returns the same as the starting line.
%%
%% @param Location A location tuple
%% @returns The ending line number (>= 1)
%%
%% @see get_line/1
%% @see get_end_column/1
%%
%% @example
%% ```
%% Line1 = get_end_line({location, 5, 10}),  %% → 5 (same as start)
%% Line2 = get_end_line({location, 5, 10, 7, 20}),  %% → 7
%% '''
-spec get_end_line(location()) -> line().
get_end_line({location, Line, _Col}) -> Line;
get_end_line({location, _Line, _Col, EndLine, _EndCol}) -> EndLine.

%% @doc Get the ending column number.
%%
%% Extracts the ending column number from a location. For single positions,
%% this returns the same as the starting column.
%%
%% @param Location A location tuple
%% @returns The ending column number (>= 0)
%%
%% @see get_column/1
%% @see get_end_line/1
%%
%% @example
%% ```
%% Col1 = get_end_column({location, 5, 10}),  %% → 10 (same as start)
%% Col2 = get_end_column({location, 5, 10, 7, 20}),  %% → 20
%% '''
-spec get_end_column(location()) -> column().
get_end_column({location, _Line, Col}) -> Col;
get_end_column({location, _Line, _Col, _EndLine, EndCol}) -> EndCol.

%%============================================================================
%% Utility Functions
%%============================================================================

%% @doc Check if a term is a valid location.
%%
%% Validates that a term has the correct structure and constraints
%% to be a valid location tuple.
%%
%% Validation checks:
%% <ul>
%%%   <li>Tuple tag must be `location'</li>
%%%   <li>Line numbers must be positive integers (> 0)</li>
%%%   <li>Column numbers must be non-negative integers (>= 0)</li>
%%%   <li>For spans, end must be after or equal to start</li>
%% </ul>
%%
%% @param Term Any Erlang term
%% @returns `true' if term is a valid location, `false' otherwise
%%
%% @example
%% ```
%% true = is_location({location, 5, 10}),
%% true = is_location({location, 5, 10, 7, 20}),
%% false = is_location({location, 0, 0}),  %% Line must be > 0
%% false = is_location({position, 5, 10}),  %% Wrong tag
%% false = is_location(not_a_location).
%% '''
-spec is_location(term()) -> boolean().
is_location({location, Line, Col})
    when is_integer(Line), Line > 0, is_integer(Col), Col >= 0 ->
    true;
is_location({location, StartLine, StartCol, EndLine, EndCol})
    when is_integer(StartLine), StartLine > 0,
         is_integer(StartCol), StartCol >= 0,
         is_integer(EndLine), EndLine > 0,
         is_integer(EndCol), EndCol >= 0 ->
    true;
is_location(_) ->
    false.

%% @doc Format location for human-readable error messages.
%%
%% Converts a location into a compact, human-readable string format
%% suitable for error messages and debugging output.
%%
%% Format patterns:
%% <ul>
%%%   <li>Column 0 (unknown): `"Line"'</li>
%%%   <li>Single position: `"Line:Col"'</li>
%%%   <li>Same-line span: `"Line:Col-EndCol"'</li>
%%%   <li>Multi-line span: `"StartLine:StartCol-EndLine:EndCol"'</li>
%% </ul>
%%
%% @param Location A location tuple
%% @returns Formatted location string
%%
%% @see catena_compiler_utils:format_location/1
%%
%% @example
%% ```
%% %% Unknown column (column 0)
%% "5" = format({location, 5, 0}),
%%%
%% %% Single position
%% "5:10" = format({location, 5, 10}),
%%%
%% %% Same-line span
%% "5:10-25" = format({location, 5, 10, 5, 25}),
%%%
%% %% Multi-line span
%% "5:10-7:20" = format({location, 5, 10, 7, 20}).
%% '''
-spec format(location()) -> string().
format({location, Line, 0}) ->
    io_lib:format("~p", [Line]);
format({location, Line, Col}) ->
    io_lib:format("~p:~p", [Line, Col]);
format({location, Line, 0, Line, _EndCol}) ->
    io_lib:format("~p", [Line]);
format({location, Line, Col, Line, EndCol}) ->
    io_lib:format("~p:~p-~p", [Line, Col, EndCol]);
format({location, StartLine, StartCol, EndLine, EndCol}) ->
    io_lib:format("~p:~p-~p:~p", [StartLine, StartCol, EndLine, EndCol]).
