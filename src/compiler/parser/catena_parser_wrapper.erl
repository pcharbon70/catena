%% Catena Parser Wrapper with Error Reporting
%% Phase 1, Task 1.1.4: Error Recovery and Reporting
%%
%% This module wraps the yecc-generated parser and provides
%% enhanced error reporting with source context and suggestions.

-module(catena_parser_wrapper).

-include("../error/catena_error.hrl").

-export([
    parse_file/1,
    parse_tokens/1,
    parse_tokens_with_file/3
]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Parse a Catena source file
-spec parse_file(string()) -> {ok, term()} | {error, [#error{}]}.
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            % Handle potential unicode decoding errors
            case unicode:characters_to_list(Binary) of
                Content when is_list(Content) ->
                    % Split content into lines and cache for source context
                    % This prevents re-reading the file for each error
                    SourceLines = prepare_source_lines(Content),
                    case catena_lexer:tokenize(Content) of
                        {ok, Tokens} ->
                            parse_tokens_with_file(Tokens, Filename, SourceLines);
                        {error, {Line, _Module, ErrorDesc}} ->
                            % Handle errors from any lexer module (catena_lexer, catena_lexer_gen, etc.)
                            Err = make_lexer_error(ErrorDesc, Line, Filename),
                            Err2 = add_source_context(Err, Filename, SourceLines),
                            {error, [Err2]}
                    end;
                {error, _Converted, _RestData} ->
                    Err = catena_error:new_error(
                        'E000_file_error',
                        "File contains invalid UTF-8 encoding",
                        {1, undefined},
                        Filename
                    ),
                    {error, [Err]};
                {incomplete, _Converted, _RestData} ->
                    Err = catena_error:new_error(
                        'E000_file_error',
                        "File contains incomplete UTF-8 encoding at end",
                        {1, undefined},
                        Filename
                    ),
                    {error, [Err]}
            end;
        {error, Reason} ->
            Err = catena_error:new_error(
                'E000_file_error',
                io_lib:format("Could not read file: ~p", [Reason]),
                {1, undefined},
                Filename
            ),
            {error, [Err]}
    end.

%% @doc Parse tokens without file context
-spec parse_tokens([term()]) -> {ok, term()} | {error, [#error{}]}.
parse_tokens(Tokens) ->
    parse_tokens_with_file(Tokens, undefined, undefined).

%% @doc Parse tokens with file context for better error messages
%% Implements manual panic-mode error recovery
%% SourceLines parameter allows caching file contents to avoid repeated I/O
-spec parse_tokens_with_file([term()], string() | undefined, [string()] | undefined) ->
    {ok, term()} | {error, [#error{}]}.
parse_tokens_with_file(Tokens, File, SourceLines) ->
    case parse_with_recovery(Tokens, File, SourceLines, []) of
        {ok, AST, []} ->
            {ok, AST};
        {ok, _AST, Errors} ->
            % Reverse errors to restore original order (accumulated in reverse)
            {error, lists:reverse(Errors)};
        {error, Errors} ->
            % Reverse errors to restore original order (accumulated in reverse)
            {error, lists:reverse(Errors)}
    end.

%% @doc Parse with manual panic-mode error recovery
%% NOTE: Errors are accumulated in REVERSE order for O(1) cons operation,
%% then reversed by the caller to restore original order. This gives O(n)
%% instead of O(n²) performance for error accumulation.
%% SourceLines parameter allows caching file contents to avoid repeated I/O
-spec parse_with_recovery([term()], string() | undefined, [string()] | undefined, [#error{}]) ->
    {ok, term(), [#error{}]} | {error, [#error{}]}.
parse_with_recovery(Tokens, File, SourceLines, AccErrors) ->
    case catena_parser:parse(Tokens) of
        {ok, AST} ->
            % Check for error_decl nodes in the AST
            ASTErrors = extract_errors_from_ast(AST, File, SourceLines),
            % Prepend AST errors in reverse to maintain order after final reverse
            AllErrors = lists:reverse(ASTErrors) ++ AccErrors,
            {ok, AST, AllErrors};
        {error, {Line, catena_parser, ErrorDesc}} ->
            % Create error for this parse failure
            Err = make_parser_error(ErrorDesc, Line, File),
            Err2 = add_source_context(Err, File, SourceLines),
            Err3 = add_parser_suggestion(Err2, ErrorDesc),

            % Try to find next synchronization point and continue
            case find_sync_point_and_continue(Tokens, Line) of
                {ok, RemainingTokens} when length(RemainingTokens) > 0 ->
                    % Continue parsing from sync point
                    % Use cons for O(1) instead of ++ for O(n)
                    parse_with_recovery(RemainingTokens, File, SourceLines, [Err3 | AccErrors]);
                _ ->
                    % No more tokens or couldn't find sync point, return all errors
                    % Use cons for O(1) instead of ++ for O(n)
                    {error, [Err3 | AccErrors]}
            end
    end.

%% @doc Find the next synchronization point (shape, flow, effect keyword) after an error
-spec find_sync_point_and_continue([term()], integer()) -> {ok, [term()]} | {error, no_sync_point}.
find_sync_point_and_continue(Tokens, ErrorLine) ->
    % Skip past the error line and find next declaration keyword
    case skip_to_next_declaration(Tokens, ErrorLine) of
        [] -> {error, no_sync_point};
        RemainingTokens -> {ok, RemainingTokens}
    end.

%% @doc Skip tokens until we find the next declaration keyword (shape, flow, effect)
%% Must skip at least one token to avoid infinite loops
-spec skip_to_next_declaration([term()], integer()) -> [term()].
skip_to_next_declaration([], _ErrorLine) ->
    [];
skip_to_next_declaration([_ | Rest], ErrorLine) ->
    % Skip at least the first token, then look for next declaration
    find_next_declaration(Rest, ErrorLine).

%% @doc Find next declaration keyword in token list
-spec find_next_declaration([term()], integer()) -> [term()].
find_next_declaration([], _ErrorLine) ->
    [];
find_next_declaration([{shape, _} = Token | Rest], _ErrorLine) ->
    [Token | Rest];
find_next_declaration([{flow, _} = Token | Rest], _ErrorLine) ->
    [Token | Rest];
find_next_declaration([{effect, _} = Token | Rest], _ErrorLine) ->
    [Token | Rest];
find_next_declaration([_ | Rest], ErrorLine) ->
    find_next_declaration(Rest, ErrorLine).

%%====================================================================
%% Error Extraction from AST
%%====================================================================

%% @doc Extract error_decl nodes from AST and convert to error records
-spec extract_errors_from_ast(term(), string() | undefined, [string()] | undefined) -> [#error{}].
extract_errors_from_ast({module, _Name, _Imports, _Exports, Declarations, _Loc}, File, SourceLines) ->
    extract_errors_from_declarations(Declarations, File, SourceLines);
extract_errors_from_ast(_, _File, _SourceLines) ->
    [].

%% @doc Extract errors from declaration list
extract_errors_from_declarations(Declarations, File, SourceLines) when is_list(Declarations) ->
    lists:filtermap(
        fun(Decl) ->
            case Decl of
                {error_decl, Message, Location} ->
                    Line = extract_line_from_location(Location),
                    Err = catena_error:new_error(
                        'E200_syntax_error',
                        Message,
                        {Line, undefined},
                        File
                    ),
                    Err2 = add_source_context(Err, File, SourceLines),
                    {true, Err2};
                _ ->
                    false
            end
        end,
        Declarations
    );
extract_errors_from_declarations(_, _File, _SourceLines) ->
    [].

%% @doc Extract line number from location tuple
extract_line_from_location({line, Line}) ->
    Line;
extract_line_from_location({location, Line, _Col}) ->
    Line;
extract_line_from_location({location, Line, _Col, _EndLine, _EndCol}) ->
    Line;
extract_line_from_location(Line) when is_integer(Line) ->
    Line;
extract_line_from_location(_) ->
    1.  % Fallback

%%====================================================================
%% Error Creation
%%====================================================================

%% @doc Create a lexer error
make_lexer_error(ErrorDesc, Line, File) ->
    Message = format_lexer_error(ErrorDesc),
    catena_error:new_error(
        'E100_lexer_error',
        Message,
        {Line, undefined},
        File
    ).

%% @doc Create a parser error
make_parser_error(ErrorDesc, Line, File) ->
    Message = format_parser_error(ErrorDesc),
    catena_error:new_error(
        'E200_syntax_error',
        Message,
        {Line, undefined},
        File
    ).

%%====================================================================
%% Error Formatting
%%====================================================================

%% @doc Format lexer error description
format_lexer_error({string, Quote, _Text}) ->
    io_lib:format("Unterminated string starting with ~s", [[Quote]]);
format_lexer_error({illegal, Text}) ->
    io_lib:format("Illegal character sequence: ~s", [Text]);
format_lexer_error(ErrorDesc) when is_list(ErrorDesc) ->
    ErrorDesc;
format_lexer_error(ErrorDesc) ->
    lists:flatten(io_lib:format("~p", [ErrorDesc])).

%% @doc Format parser error description
format_parser_error(["syntax error before: ", Token]) ->
    io_lib:format("Unexpected token: ~s", [Token]);
format_parser_error(ErrorDesc) when is_list(ErrorDesc) ->
    lists:flatten(ErrorDesc);
format_parser_error(ErrorDesc) ->
    lists:flatten(io_lib:format("~p", [ErrorDesc])).

%%====================================================================
%% Source Context Caching
%%====================================================================

%% @doc Prepare source lines for caching to avoid repeated file I/O
%% Splits content into lines the same way read_source_context does
-spec prepare_source_lines(string()) -> [string()].
prepare_source_lines(Content) ->
    AllLines = string:split(Content, "\n", all),
    % Remove trailing empty line if content ends with newline
    case AllLines of
        [] -> [];
        _ ->
            case lists:last(AllLines) of
                "" -> lists:droplast(AllLines);
                _ -> AllLines
            end
    end.

%%====================================================================
%% Source Context
%%====================================================================

%% @doc Add source context to error if file is available
%% If SourceLines is provided, use cached lines instead of re-reading file
add_source_context(Err, undefined, _SourceLines) ->
    Err;
add_source_context(#error{file = File, line = Line} = Err, File, SourceLines) when File =/= undefined ->
    % Use cached lines if available to avoid repeated file I/O
    case SourceLines of
        undefined ->
            % No cache, read from file
            case catena_error:read_source_context(File, Line, 2) of
                {ok, Context} ->
                    Before = maps:get(before, Context),
                    ErrorLine = maps:get(error_line, Context),
                    After = maps:get('after', Context),
                    catena_error:add_context(Err, Before, ErrorLine, After);
                {error, _} ->
                    Err
            end;
        Lines when is_list(Lines) ->
            % Use cached lines
            case catena_error:extract_context_from_file(Lines, Line, 2) of
                {ok, Context} ->
                    Before = maps:get(before, Context),
                    ErrorLine = maps:get(error_line, Context),
                    After = maps:get('after', Context),
                    catena_error:add_context(Err, Before, ErrorLine, After);
                {error, _} ->
                    Err
            end
    end;
add_source_context(Err, _, _SourceLines) ->
    Err.

%%====================================================================
%% Suggestions
%%====================================================================

%% @doc Add helpful suggestion based on error pattern
add_parser_suggestion(Err, ErrorDesc) ->
    Suggestion = detect_suggestion(ErrorDesc),
    case Suggestion of
        undefined -> Err;
        _ -> catena_error:add_suggestion(Err, Suggestion)
    end.

%% @doc Detect suggestion from error description
detect_suggestion(["syntax error before: ", "'end'"]) ->
    "This 'end' keyword appears unexpected. Check if there's an extra 'end' or a missing opening keyword.";
detect_suggestion(["syntax error before: ", Token]) ->
    case Token of
        "'}'" -> "Check for missing opening '{' or extra '}'";
        "']'" -> "Check for missing opening '[' or extra ']'";
        "')'" -> "Check for missing opening '(' or extra ')'";
        "'|'" -> "In shape declarations, use '|' to separate constructors";
        _ ->
            % Check for missing '=' in declarations
            case detect_missing_equals(Token) of
                undefined ->
                    % Check for missing '->' in patterns
                    case detect_missing_arrow(Token) of
                        undefined ->
                            % Check for typos in keywords
                            case detect_keyword_typo(Token) of
                                undefined ->
                                    % Check if it looks like end of file
                                    case string:find(Token, "end of input") of
                                        nomatch ->
                                            % Check for invalid operator sequences
                                            detect_invalid_operator(Token);
                                        _ ->
                                            "File ended unexpectedly. Check for missing 'end' keywords or unclosed delimiters."
                                    end;
                                Suggestion -> Suggestion
                            end;
                        Suggestion -> Suggestion
                    end;
                Suggestion -> Suggestion
            end
    end;
detect_suggestion(_) ->
    undefined.

%% @doc Detect missing '=' in shape/flow declarations
detect_missing_equals(Token) ->
    % Common tokens that would follow '=' in declarations
    case Token of
        % Constructor IDs (capitalized) or type expressions
        T when length(T) > 2 ->
            case lists:nth(1, T) of
                $' ->
                    % It's a quoted token, check if it starts with uppercase
                    case length(T) > 3 andalso lists:nth(2, T) >= $A andalso lists:nth(2, T) =< $Z of
                        true -> "Missing '=' in declaration? Expected 'shape Name = Constructor' or 'flow name = ...'.";
                        false -> undefined
                    end;
                _ -> undefined
            end;
        _ -> undefined
    end.

%% @doc Detect missing '->' in match expressions or flow clauses
detect_missing_arrow(Token) ->
    % Common patterns that suggest missing '->'
    case Token of
        % Expression starts that should come after '->'
        "'let'" -> "Missing '->' in pattern match? Expected 'pattern -> expression'.";
        "'do'" -> "Missing '->' in pattern match? Expected 'pattern -> expression'.";
        "'if'" -> "Missing '->' in pattern match? Expected 'pattern -> expression'.";
        "'case'" -> "Missing '->' in pattern match? Expected 'pattern -> expression'.";
        _ ->
            % Check for numeric literals or identifiers (likely expression starts)
            case string:find(Token, "integer") =/= nomatch orelse
                 string:find(Token, "float") =/= nomatch of
                true -> "Missing '->' in pattern match? Expected 'pattern -> expression'.";
                false -> undefined
            end
    end.

%% @doc Detect typos in keywords using edit distance
detect_keyword_typo(Token) ->
    % Remove quotes from token if present
    CleanToken = case Token of
        [$', $' | Rest] ->
            % Remove leading and trailing single quotes
            case lists:reverse(Rest) of
                [$', $' | RevRest] -> lists:reverse(RevRest);
                _ -> Token
            end;
        _ -> Token
    end,

    % List of Catena keywords to check against
    Keywords = [
        "shape", "flow", "effect", "match", "where", "let", "in", "do", "end",
        "if", "then", "else", "case", "of", "when", "module", "import", "export",
        "trait", "instance", "forall", "actor", "supervisor", "perform", "handle",
        "try", "with", "resume"
    ],

    % Find closest keyword (edit distance <= 2)
    case find_closest_keyword(string:lowercase(CleanToken), Keywords) of
        undefined -> undefined;
        ClosestKeyword ->
            io_lib:format("Did you mean '~s'? Check for typos in keywords.", [ClosestKeyword])
    end.

%% @doc Find closest keyword using edit distance
find_closest_keyword(Input, Keywords) ->
    % Calculate edit distance to each keyword
    Distances = lists:map(
        fun(Keyword) ->
            {Keyword, levenshtein_distance(Input, Keyword)}
        end,
        Keywords
    ),

    % Filter by distance <= 2 and find minimum
    CloseMatches = lists:filter(
        fun({_K, Dist}) -> Dist > 0 andalso Dist =< 2 end,
        Distances
    ),

    case CloseMatches of
        [] -> undefined;
        _ ->
            % Sort by distance and take the closest one
            Sorted = lists:sort(
                fun({_K1, D1}, {_K2, D2}) -> D1 =< D2 end,
                CloseMatches
            ),
            {Closest, _MinDist} = hd(Sorted),
            Closest
    end.

%% @doc Calculate Levenshtein distance between two strings
levenshtein_distance(S1, S2) ->
    levenshtein_distance(S1, S2, length(S1), length(S2), #{}).

levenshtein_distance([], S2, _Len1, Len2, _Cache) ->
    Len2;
levenshtein_distance(S1, [], Len1, _Len2, _Cache) ->
    Len1;
levenshtein_distance(S1, S2, Len1, Len2, Cache) ->
    Key = {S1, S2},
    case maps:get(Key, Cache, undefined) of
        undefined ->
            [H1 | T1] = S1,
            [H2 | T2] = S2,

            Cost = case H1 =:= H2 of
                true -> 0;
                false -> 1
            end,

            % Calculate three options: delete, insert, substitute
            D1 = levenshtein_distance(T1, S2, Len1 - 1, Len2, Cache) + 1,
            D2 = levenshtein_distance(S1, T2, Len1, Len2 - 1, Cache) + 1,
            D3 = levenshtein_distance(T1, T2, Len1 - 1, Len2 - 1, Cache) + Cost,

            min(D1, min(D2, D3));
        Dist ->
            Dist
    end.

%% @doc Detect invalid operator sequences
detect_invalid_operator(Token) ->
    % Check for common operator mistakes
    case Token of
        "'=='" -> "Use '=' for binding in patterns, or '==' for equality in guards.";
        "'==='" -> "Catena uses '=' for pattern matching. Did you mean '='?";
        "'!='" -> "Use '/=' for inequality in Catena, not '!='.";
        "'&&'" -> "Use 'andalso' or 'and' for boolean operations, not '&&'.";
        "'||'" -> "Use 'orelse' or 'or' for boolean operations, not '||'.";
        "'=>'" -> "Use '->' for pattern matching. '=>' is not a Catena operator.";
        _ -> undefined
    end.
