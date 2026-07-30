-module(catena_resumption_lexer_tests).

-include_lib("eunit/include/eunit.hrl").

language_words_and_identifier_boundaries_test() ->
    {ok, Tokens} = catena_lexer:tokenize(
        "with resume within resumed with_value resumeLater"
    ),
    ?assertEqual(
        [
            {with, 1},
            {resume, 1},
            {lower_ident, 1, "within"},
            {lower_ident, 1, "resumed"},
            {lower_ident, 1, "with_value"},
            {lower_ident, 1, "resumeLater"}
        ],
        Tokens
    ).

comments_whitespace_and_locations_test() ->
    {ok, Tokens} = catena_lexer:tokenize(
        "with -- ignored resume\n"
        "  resume\n"
        "{- with resume -}\n"
        "within"
    ),
    ?assertEqual(
        [
            {with, 1},
            {resume, 2},
            {lower_ident, 4, "within"}
        ],
        Tokens
    ).

punctuation_boundaries_test() ->
    {ok, Tokens} = catena_lexer:tokenize("with-resume(with,resume)"),
    ?assertEqual(
        [with, minus, resume, lparen, with, comma, resume, rparen],
        token_types(Tokens)
    ).

malformed_boundary_reports_lexer_error_test() ->
    ?assertMatch({error, _}, catena_lexer:tokenize("with@resume")).

token_types(Tokens) ->
    [element(1, Token) || Token <- Tokens].
