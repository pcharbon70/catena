%% Catena Lexer Definition
%% Implements Task 1.1.1: Token Recognition
%%
%% This lexer recognizes all Catena language tokens including:
%% - Keywords (type, transform, match, etc.)
%% - Operators (|>, ->, :, =, etc.)
%% - Delimiters ({, }, [, ], etc.)
%% - Literals (integers, floats, strings)
%% - Identifiers
%% - Comments (single-line and nested multi-line)

Definitions.

%% Whitespace
WHITESPACE = [\s\t\r\n]+

%% Identifiers and Keywords
LOWER = [a-z]
UPPER = [A-Z]
DIGIT = [0-9]
IDENT_CHAR = [a-zA-Z0-9_']
LOWER_IDENT = {LOWER}{IDENT_CHAR}*
UPPER_IDENT = {UPPER}{IDENT_CHAR}*

%% Numbers
INTEGER = {DIGIT}+
FLOAT = {DIGIT}+\.{DIGIT}+
EXPONENT = [eE][+-]?{DIGIT}+
SCIENTIFIC = ({INTEGER}|{FLOAT}){EXPONENT}

%% String literals - only valid escapes allowed
VALID_ESCAPE = (\\n|\\r|\\t|\\\\|\\"|\\')
STRING_CONTENT = ([^"\\]|{VALID_ESCAPE})*

%% Single-line comment
SINGLE_COMMENT = --[^\n]*

Rules.

%% Whitespace (ignored)
{WHITESPACE} : skip_token.

%% Single-line comments (ignored)
{SINGLE_COMMENT} : skip_token.

%% Multi-line comments with nesting support
\{- : {token, {comment_start, TokenLine}}.
-\} : {token, {comment_end, TokenLine}}.

%% Core Keywords (12 keywords requiring compiler support)
type : {token, {type, TokenLine}}.
transform : {token, {transform, TokenLine}}.
let : {token, {'let', TokenLine}}.
match : {token, {match, TokenLine}}.
trait : {token, {trait, TokenLine}}.
instance : {token, {instance, TokenLine}}.
effect : {token, {effect, TokenLine}}.
perform : {token, {perform, TokenLine}}.
handle : {token, {handle, TokenLine}}.
actor : {token, {actor, TokenLine}}.
process : {token, {process, TokenLine}}.
module : {token, {'module', TokenLine}}.

%% Syntax keywords (grammar support)
in : {token, {'in', TokenLine}}.
end : {token, {'end', TokenLine}}.
where : {token, {where, TokenLine}}.
case : {token, {'case', TokenLine}}.
of : {token, {'of', TokenLine}}.
when : {token, {'when', TokenLine}}.
as : {token, {as, TokenLine}}.
forall : {token, {forall, TokenLine}}.
operation : {token, {operation, TokenLine}}.

%% Module system keywords
import : {token, {'import', TokenLine}}.
export : {token, {'export', TokenLine}}.
exports : {token, {exports, TokenLine}}.
qualified : {token, {qualified, TokenLine}}.
private : {token, {private, TokenLine}}.

%% Removed keywords (moved to library/desugar):
%% - do: desugar from >>= (Pipeline bind)
%% - if/then/else: desugar to match on Bool
%% - extends: use : syntax in trait definition
%% - try/with: replaced by handle keyword
%% - supervisor: library convention

%% Two-character and three-character operators (must come before single-character)
%% Ordering is critical: longer operators must come before shorter ones
\|> : {token, {pipe_right, TokenLine}}.
-> : {token, {arrow, TokenLine}}.
=> : {token, {double_arrow, TokenLine}}.
=== : {token, {setoid_eq, TokenLine}}.
!== : {token, {setoid_neq, TokenLine}}.
== : {token, {eq, TokenLine}}.
/= : {token, {neq, TokenLine}}.
<= : {token, {lte, TokenLine}}.
>= : {token, {gte, TokenLine}}.
\|\| : {token, {'or', TokenLine}}.
&& : {token, {'and', TokenLine}}.
:: : {token, {cons, TokenLine}}.
<- : {token, {left_arrow, TokenLine}}.
\.\. : {token, {range, TokenLine}}.

%% Single-character operators
: : {token, {colon, TokenLine}}.
= : {token, {equals, TokenLine}}.
\| : {token, {pipe, TokenLine}}.
< : {token, {lt, TokenLine}}.
> : {token, {gt, TokenLine}}.
\+ : {token, {plus, TokenLine}}.
- : {token, {minus, TokenLine}}.
\* : {token, {star, TokenLine}}.
/ : {token, {slash, TokenLine}}.
\. : {token, {dot, TokenLine}}.

%% Delimiters
\{ : {token, {lbrace, TokenLine}}.
\} : {token, {rbrace, TokenLine}}.
\[ : {token, {lbracket, TokenLine}}.
\] : {token, {rbracket, TokenLine}}.
\( : {token, {lparen, TokenLine}}.
\) : {token, {rparen, TokenLine}}.
, : {token, {comma, TokenLine}}.
; : {token, {semicolon, TokenLine}}.
_ : {token, {underscore, TokenLine}}.

%% Numeric literals (scientific notation must come first)
{SCIENTIFIC} : {token, {float, TokenLine, parse_scientific(TokenChars)}}.
{FLOAT} : {token, {float, TokenLine, parse_float(TokenChars)}}.
{INTEGER} : {token, {integer, TokenLine, parse_integer(TokenChars)}}.

%% String literals
"{STRING_CONTENT}" : validate_string_literal(TokenLine, TokenChars).

%% Identifiers (uppercase and lowercase)
{UPPER_IDENT} : validate_identifier(TokenLine, TokenChars, upper_ident).
{LOWER_IDENT} : validate_identifier(TokenLine, TokenChars, lower_ident).

Erlang code.

%% Export custom functions
-export([tokenize/1]).

%% Security limits
-define(MAX_COMMENT_DEPTH, 100).

%% Public API wrapper functions

%% @doc Tokenize a source string with UTF-8 validation and comment filtering
%% Returns {ok, Tokens} or {error, ErrorInfo}
tokenize(Source) when is_binary(Source) ->
    %% Binary input: validate UTF-8 encoding
    case validate_utf8(Source) of
        ok ->
            tokenize(unicode:characters_to_list(Source));
        {error, Reason} ->
            {error, {0, catena_lexer, Reason}}
    end;
tokenize(Source) when is_list(Source) ->
    %% List input: validate Unicode code points
    case validate_unicode_string(Source) of
        ok ->
            case string(Source) of
                {ok, RawTokens, _EndLine} ->
                    filter_comments(RawTokens);
                {error, ErrorInfo} ->
                    {error, ErrorInfo};
                {error, ErrorInfo, _EndLine} ->
                    {error, ErrorInfo}
            end;
        {error, Reason} ->
            {error, {0, catena_lexer, Reason}}
    end.

%% @doc Filter out comment tokens and validate nesting
filter_comments(Tokens) ->
    filter_comments(Tokens, [], 0, 1).

%% Filter comments with depth tracking
%% Params: RemainingTokens, Accumulator, CurrentDepth, CurrentLine
filter_comments([], Acc, 0, _Line) ->
    {ok, lists:reverse(Acc)};
filter_comments([], _Acc, Depth, Line) when Depth > 0 ->
    {error, {0, catena_lexer, {unclosed_comment, Line}}};
filter_comments([{comment_start, Line} | Rest], Acc, Depth, _) when Depth >= ?MAX_COMMENT_DEPTH ->
    {error, {Line, catena_lexer, {comment_depth_exceeded, Depth + 1, ?MAX_COMMENT_DEPTH}}};
filter_comments([{comment_start, Line} | Rest], Acc, Depth, _) ->
    filter_comments(Rest, Acc, Depth + 1, Line);
filter_comments([{comment_end, Line} | Rest], Acc, 0, _) ->
    {error, {Line, catena_lexer, unmatched_comment_end}};
filter_comments([{comment_end, _} | Rest], Acc, Depth, Line) ->
    filter_comments(Rest, Acc, Depth - 1, Line);
filter_comments([Token | Rest], Acc, 0, Line) ->
    %% Outside comments: keep token
    filter_comments(Rest, [Token | Acc], 0, Line);
filter_comments([_Token | Rest], Acc, Depth, Line) ->
    %% Inside comment: skip token
    filter_comments(Rest, Acc, Depth, Line).

%% Unicode/UTF-8 validation functions

%% @doc Validate UTF-8 binary encoding
validate_utf8(Binary) when is_binary(Binary) ->
    case unicode:characters_to_list(Binary, utf8) of
        {error, _, _} ->
            {error, {invalid_utf8, "Invalid UTF-8 byte sequence"}};
        {incomplete, _, _} ->
            {error, {invalid_utf8, "Incomplete UTF-8 byte sequence"}};
        List when is_list(List) ->
            %% Successfully decoded, now validate code points
            validate_unicode_codepoints(List, 1)
    end.

%% @doc Validate Unicode string (list of code points)
validate_unicode_string(String) when is_list(String) ->
    validate_unicode_codepoints(String, 1).

%% @doc Validate individual Unicode code points
validate_unicode_codepoints([], _Pos) ->
    ok;
validate_unicode_codepoints([Char | Rest], Pos) when is_integer(Char) ->
    case validate_codepoint(Char) of
        ok ->
            validate_unicode_codepoints(Rest, Pos + 1);
        {error, Reason} ->
            {error, {invalid_unicode, Pos, Char, Reason}}
    end;
validate_unicode_codepoints([Invalid | _], Pos) ->
    {error, {invalid_character, Pos, Invalid}}.

%% @doc Validate a single Unicode code point
validate_codepoint(Char) when Char >= 0, Char =< 16#D7FF ->
    %% Valid: Basic Multilingual Plane (excluding surrogates)
    ok;
validate_codepoint(Char) when Char >= 16#E000, Char =< 16#10FFFF ->
    %% Valid: Private Use Area and Supplementary Planes
    ok;
validate_codepoint(Char) when Char >= 16#D800, Char =< 16#DFFF ->
    %% Invalid: UTF-16 surrogate pairs (not valid Unicode scalar values)
    {error, "UTF-16 surrogate code point"};
validate_codepoint(Char) when Char > 16#10FFFF ->
    %% Invalid: Beyond Unicode range
    {error, "Code point beyond valid Unicode range"};
validate_codepoint(Char) when Char < 0 ->
    %% Invalid: Negative code point
    {error, "Negative code point"};
validate_codepoint(_Char) ->
    %% Catch-all for any other invalid cases
    {error, "Invalid code point"}.

%% Helper functions for parsing literals

validate_identifier(Line, Chars, Type) ->
    %% Use a fixed maximum identifier length
    MaxLen = 255,
    ActualLen = length(Chars),
    case ActualLen > MaxLen of
        true ->
            {error, {identifier_too_long, Line, ActualLen, MaxLen}};
        false ->
            {token, {Type, Line, Chars}}
    end.

validate_string_literal(Line, Chars) ->
    %% Maximum string literal length (including quotes)
    %% This prevents memory exhaustion attacks
    MaxLen = 8192,
    ActualLen = length(Chars),
    case ActualLen > MaxLen of
        true ->
            {error, {string_too_long, Line, ActualLen, MaxLen}};
        false ->
            {token, {string, Line, parse_string_content(Chars)}}
    end.

parse_integer(Chars) ->
    list_to_integer(Chars).

parse_float(Chars) ->
    list_to_float(Chars).

parse_scientific(Chars) ->
    %% Erlang's list_to_float requires a decimal point
    %% If the input is like "1e10", convert to "1.0e10"
    case string:chr(Chars, $.) of
        0 ->
            %% No decimal point - add one before 'e' or 'E'
            case lists:splitwith(fun(C) -> C /= $e andalso C /= $E end, Chars) of
                {Base, [$e | Exp]} -> list_to_float(Base ++ ".0e" ++ Exp);
                {Base, [$E | Exp]} -> list_to_float(Base ++ ".0E" ++ Exp);
                _ -> list_to_float(Chars)
            end;
        _ ->
            %% Already has decimal point
            list_to_float(Chars)
    end.

%% Parse string content - only called for strings with valid escapes
%% (lexer pattern ensures only valid escapes reach here)
parse_string_content(Chars) ->
    %% Remove surrounding quotes
    String = lists:sublist(Chars, 2, length(Chars) - 2),
    %% Process escape sequences
    process_valid_escapes(String, []).

%% Process escape sequences - only valid ones (lexer guarantees this)
process_valid_escapes([], Acc) ->
    lists:reverse(Acc);
process_valid_escapes([$\\, $n | Rest], Acc) ->
    process_valid_escapes(Rest, [$\n | Acc]);
process_valid_escapes([$\\, $r | Rest], Acc) ->
    process_valid_escapes(Rest, [$\r | Acc]);
process_valid_escapes([$\\, $t | Rest], Acc) ->
    process_valid_escapes(Rest, [$\t | Acc]);
process_valid_escapes([$\\, $\\ | Rest], Acc) ->
    process_valid_escapes(Rest, [$\\ | Acc]);
process_valid_escapes([$\\, $\" | Rest], Acc) ->
    process_valid_escapes(Rest, [$\" | Acc]);
process_valid_escapes([$\\, $' | Rest], Acc) ->
    process_valid_escapes(Rest, [$' | Acc]);
process_valid_escapes([C | Rest], Acc) ->
    process_valid_escapes(Rest, [C | Acc]).
