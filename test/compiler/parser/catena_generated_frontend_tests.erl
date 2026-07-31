%% @doc Contract tests for the maintained generated-frontend boundary.
-module(catena_generated_frontend_tests).

-include_lib("eunit/include/eunit.hrl").

scan_preserves_generated_result_test() ->
    Source = "transform identity x = x\n",
    ?assertEqual(
        catena_lexer:string(Source),
        catena_generated_frontend:scan(Source)
    ).

tokenize_preserves_filtered_result_test() ->
    Source = <<"-- comment\ntransform identity x = x\n">>,
    ?assertEqual(
        catena_lexer:tokenize(Source),
        catena_generated_frontend:tokenize(Source)
    ).

parse_preserves_generated_result_test() ->
    {ok, Tokens} = catena_generated_frontend:tokenize(
        "transform identity x = x\n"
    ),
    ?assertEqual(
        catena_parser:parse(Tokens),
        catena_generated_frontend:parse(Tokens)
    ).

format_lexer_error_preserves_generated_result_test() ->
    Reason = {illegal, "@"},
    ?assertEqual(
        catena_lexer:format_error(Reason),
        catena_generated_frontend:format_lexer_error(Reason)
    ).
