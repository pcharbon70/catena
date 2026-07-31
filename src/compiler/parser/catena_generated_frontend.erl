%% @doc Maintained contracts for the generated Catena lexer and parser.
%%
%% The leex/yecc outputs remain generated and excluded from Dialyzer. All
%% maintained compiler callers cross that boundary through this module so the
%% analyzed source has one explicit, truthful contract for generated results.
-module(catena_generated_frontend).

-export([
    format_lexer_error/1,
    parse/1,
    scan/1,
    tokenize/1
]).

-type token() :: tuple().
-type tokens() :: [token()].
-type source_location() :: non_neg_integer() | {non_neg_integer(), non_neg_integer()}.
-type lexer_error() :: {source_location(), module(), term()}.
-type scan_result() ::
    {ok, tokens(), source_location()} |
    {error, lexer_error(), source_location()}.
-type tokenize_result() :: {ok, tokens()} | {error, lexer_error()}.
-type parse_result() :: {ok, term()} | {error, term()}.

-export_type([
    lexer_error/0,
    parse_result/0,
    scan_result/0,
    source_location/0,
    token/0,
    tokenize_result/0,
    tokens/0
]).

%% @doc Run the generated leex whole-string scanner.
-spec scan(string()) -> scan_result().
scan(Source) ->
    generated_call(catena_lexer, string, [Source]).

%% @doc Run Catena's generated scanner plus maintained token filtering.
-spec tokenize(unicode:chardata()) -> tokenize_result().
tokenize(Source) ->
    generated_call(catena_lexer, tokenize, [Source]).

%% @doc Parse a generated token stream with the yecc parser.
-spec parse(tokens()) -> parse_result().
parse(Tokens) ->
    generated_call(catena_parser, parse, [Tokens]).

%% @doc Format a generated lexer diagnostic.
-spec format_lexer_error(term()) -> term().
format_lexer_error(Reason) ->
    generated_call(catena_lexer, format_error, [Reason]).

generated_call(Module, Function, Arguments) ->
    erlang:apply(Module, Function, Arguments).
