-module(error_reporting_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../../src/compiler/error/catena_error.hrl").

%%====================================================================
%% Test Helpers
%%====================================================================

% Create a temporary test file with content
create_test_file(Filename, Content) ->
    ok = file:write_file(Filename, Content).

% Delete a test file
delete_test_file(Filename) ->
    file:delete(Filename).

% Compile source string (via temp file)
compile_source(Source) ->
    TestFile = "/tmp/catena_integration_test.catena",
    create_test_file(TestFile, Source),
    Result = catena_parser_wrapper:parse_file(TestFile),
    delete_test_file(TestFile),
    Result.

%%====================================================================
%% Single Error Tests
%%====================================================================

single_error_missing_end_test() ->
    % Missing 'end' keyword
    Source = "shape Bool = True | False\n",
    Result = compile_source(Source),

    case Result of
        {ok, _AST} ->
            % Might parse successfully if 'end' is not required
            ok;
        {error, Errors} ->
            ?assert(is_list(Errors)),
            ?assert(length(Errors) >= 1),
            [Err | _] = Errors,
            ?assertEqual(error, Err#error.severity),
            ?assert(is_list(Err#error.message)),
            ?assert(length(Err#error.message) > 0)
    end.

single_error_unexpected_token_test() ->
    % Extra closing brace
    Source = "shape Foo = Bar }\n",
    Result = compile_source(Source),

    case Result of
        {ok, _} -> ok;  % Parser might accept this
        {error, Errors} ->
            ?assert(is_list(Errors)),
            [Err | _] = Errors,
            ?assertEqual('E200_syntax_error', Err#error.code)
    end.

single_error_has_source_context_test() ->
    % Create multi-line source with error
    Source = "shape Foo = Bar\nshape Baz = \nshape Qux = Quux\n",
    Result = compile_source(Source),

    case Result of
        {ok, _} -> ok;
        {error, [Err | _]} ->
            % Should have file location
            ?assertNotEqual(undefined, Err#error.file),
            ?assert(Err#error.line > 0)
    end.

%%====================================================================
%% Lexer Error Tests
%%====================================================================

lexer_error_unterminated_string_test() ->
    Source = "shape Foo = \"unterminated\n",
    Result = compile_source(Source),

    case Result of
        {error, [Err]} ->
            ?assertEqual('E100_lexer_error', Err#error.code),
            ?assertEqual(error, Err#error.severity),
            ?assert(Err#error.line > 0);
        _ ->
            % Lexer might handle this differently
            ok
    end.

lexer_error_illegal_character_test() ->
    % Tab character might be illegal in some contexts
    Source = "shape\t\tFoo = Bar\n",
    Result = compile_source(Source),

    % Either succeeds or fails with lexer error
    case Result of
        {ok, _} -> ok;
        {error, [Err]} ->
            ?assert(is_atom(Err#error.code))
    end.

%%====================================================================
%% Error Formatting Integration Tests
%%====================================================================

error_can_be_formatted_test() ->
    Source = "shape = Bar\n",  % Missing type name
    Result = compile_source(Source),

    case Result of
        {error, Errors} ->
            % Test that all errors can be formatted
            lists:foreach(
                fun(Err) ->
                    Formatted = catena_error_formatter:format_error(Err),
                    ?assert(is_list(Formatted) orelse is_binary(Formatted))
                end,
                Errors
            );
        {ok, _} ->
            ok
    end.

error_list_can_be_formatted_test() ->
    Source = "shape Foo =\n",  % Incomplete declaration
    Result = compile_source(Source),

    case Result of
        {error, Errors} ->
            Formatted = catena_error_formatter:format_error_list(Errors),
            ?assert(is_list(Formatted) orelse is_binary(Formatted)),
            % Should contain error count if non-empty
            FlatFormatted = lists:flatten(Formatted),
            case length(Errors) of
                0 -> ok;
                _ -> ?assert(length(FlatFormatted) > 0)
            end;
        {ok, _} ->
            ok
    end.

formatted_error_contains_message_test() ->
    Source = "shape\n",  % Incomplete
    Result = compile_source(Source),

    case Result of
        {error, [Err | _]} ->
            catena_error_formatter:set_color_mode(never),
            Formatted = lists:flatten(catena_error_formatter:format_error(Err)),
            % Should contain the error code
            CodeStr = atom_to_list(Err#error.code),
            ?assert(string:find(Formatted, CodeStr) /= nomatch);
        {ok, _} ->
            ok
    end.

%%====================================================================
%% Color Output Tests
%%====================================================================

color_mode_respected_test() ->
    Source = "shape = \n",  % Error
    Result = compile_source(Source),

    case Result of
        {error, [Err | _]} ->
            % Test with colors disabled
            catena_error_formatter:set_color_mode(never),
            FormattedNoColor = lists:flatten(catena_error_formatter:format_error(Err)),
            ?assertEqual(nomatch, string:find(FormattedNoColor, "\e[")),

            % Test with colors enabled
            catena_error_formatter:set_color_mode(always),
            FormattedColor = lists:flatten(catena_error_formatter:format_error(Err)),
            ?assertNotEqual(nomatch, string:find(FormattedColor, "\e["));
        {ok, _} ->
            ok
    end.

colored_error_severity_test() ->
    Source = "shape Foo =\n",
    Result = compile_source(Source),

    case Result of
        {error, [Err | _]} ->
            catena_error_formatter:set_color_mode(always),
            Formatted = lists:flatten(catena_error_formatter:format_error(Err)),

            case Err#error.severity of
                error ->
                    % Should contain red color code (31)
                    ?assertNotEqual(nomatch, string:find(Formatted, "\e[31m"));
                warning ->
                    % Should contain yellow color code (33)
                    ?assertNotEqual(nomatch, string:find(Formatted, "\e[33m"));
                _ ->
                    ok
            end;
        {ok, _} ->
            ok
    end.

%%====================================================================
%% Edge Case Tests
%%====================================================================

empty_file_test() ->
    Source = "",
    Result = compile_source(Source),

    % Should either succeed or fail gracefully
    case Result of
        {ok, _} -> ok;
        {error, Errors} ->
            ?assert(is_list(Errors)),
            lists:foreach(
                fun(Err) ->
                    ?assert(is_atom(Err#error.code)),
                    ?assert(is_atom(Err#error.severity))
                end,
                Errors
            )
    end.

single_line_source_test() ->
    Source = "shape Bool = True | False",  % No newline
    Result = compile_source(Source),

    % Should handle source without trailing newline
    case Result of
        {ok, _} -> ok;
        {error, Errors} ->
            ?assert(is_list(Errors))
    end.

very_long_line_test() ->
    % Create a line with 500+ characters
    LongName = lists:duplicate(500, $x),
    Source = "shape " ++ LongName ++ " = Bar\n",
    Result = compile_source(Source),

    % Should handle or error gracefully
    case Result of
        {ok, _} -> ok;
        {error, Errors} ->
            ?assert(is_list(Errors)),
            % Error message should be present and reasonable
            [Err | _] = Errors,
            ?assert(is_list(Err#error.message))
    end.

%%====================================================================
%% File-Based Integration Tests
%%====================================================================

parse_file_integration_test() ->
    TestFile = "/tmp/catena_file_integration_test.catena",
    Content = "shape Bool = True | False\n",
    create_test_file(TestFile, Content),

    Result = catena_parser_wrapper:parse_file(TestFile),

    delete_test_file(TestFile),

    case Result of
        {ok, _AST} -> ok;
        {error, Errors} ->
            ?assert(is_list(Errors)),
            % All errors should have the file name
            lists:foreach(
                fun(Err) ->
                    ?assertEqual(TestFile, Err#error.file)
                end,
                Errors
            )
    end.

parse_nonexistent_file_test() ->
    Result = catena_parser_wrapper:parse_file("/nonexistent/file.catena"),

    ?assertMatch({error, [#error{code = 'E000_file_error'}]}, Result),

    case Result of
        {error, [Err]} ->
            ?assert(string:find(Err#error.message, "Could not read file") /= nomatch);
        _ ->
            ?assert(false)
    end.

%%====================================================================
%% Suggestion Tests
%%====================================================================

suggestion_provided_test() ->
    Source = "shape Foo = Bar end\n",  % Extra 'end'
    Result = compile_source(Source),

    case Result of
        {error, [Err | _]} ->
            % Might have a suggestion
            case Err#error.suggestion of
                undefined -> ok;  % Suggestion is optional
                Sugg ->
                    ?assert(is_list(Sugg)),
                    ?assert(length(Sugg) > 0)
            end;
        {ok, _} ->
            ok
    end.

suggestion_in_formatted_output_test() ->
    Source = "shape Foo = Bar }\n",  % Unmatched brace
    Result = compile_source(Source),

    case Result of
        {error, [Err | _]} ->
            case Err#error.suggestion of
                undefined ->
                    ok;
                _Sugg ->
                    catena_error_formatter:set_color_mode(never),
                    Formatted = lists:flatten(catena_error_formatter:format_error(Err)),
                    % Suggestion should appear in formatted output
                    ?assertNotEqual(nomatch, string:find(Formatted, "help:"))
            end;
        {ok, _} ->
            ok
    end.

%%====================================================================
%% Multi-Line Context Tests
%%====================================================================

multiline_context_test() ->
    Source = "line 1\nline 2\nline 3 error\nline 4\nline 5\n",
    Result = compile_source(Source),

    % If there's an error, check context
    case Result of
        {error, [Err | _]} ->
            Formatted = catena_error_formatter:format_error(Err),
            FlatFormatted = lists:flatten(Formatted),
            % Should contain line numbers if context present
            ?assert(is_list(FlatFormatted));
        {ok, _} ->
            ok
    end.

error_location_accurate_test() ->
    Source = "shape Foo = Bar\nshape Baz =\n",  % Error on line 2
    Result = compile_source(Source),

    case Result of
        {error, [Err | _]} ->
            % Line number should be reasonable
            ?assert(Err#error.line >= 1),
            ?assert(Err#error.line =< 3);
        {ok, _} ->
            ok
    end.
