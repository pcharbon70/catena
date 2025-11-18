-module(catena_error_formatter_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../../src/compiler/error/catena_error.hrl").

%%====================================================================
%% ANSI Color Tests
%%====================================================================

red_text_test() ->
    catena_error_formatter:set_color_mode(always),
    Result = lists:flatten(catena_error_formatter:red("error")),
    ?assertEqual("\e[31merror\e[0m", Result).

yellow_text_test() ->
    catena_error_formatter:set_color_mode(always),
    Result = lists:flatten(catena_error_formatter:yellow("warning")),
    ?assertEqual("\e[33mwarning\e[0m", Result).

blue_text_test() ->
    catena_error_formatter:set_color_mode(always),
    Result = lists:flatten(catena_error_formatter:blue("note")),
    ?assertEqual("\e[34mnote\e[0m", Result).

cyan_text_test() ->
    catena_error_formatter:set_color_mode(always),
    Result = lists:flatten(catena_error_formatter:cyan("help")),
    ?assertEqual("\e[36mhelp\e[0m", Result).

bold_text_test() ->
    catena_error_formatter:set_color_mode(always),
    Result = lists:flatten(catena_error_formatter:bold("important")),
    ?assertEqual("\e[1mimportant\e[0m", Result).

dim_text_test() ->
    catena_error_formatter:set_color_mode(always),
    Result = lists:flatten(catena_error_formatter:dim("context")),
    ?assertEqual("\e[2mcontext\e[0m", Result).

reset_test() ->
    Result = catena_error_formatter:reset(),
    ?assertEqual("\e[0m", Result).

%%====================================================================
%% Color Mode Tests
%%====================================================================

color_mode_always_test() ->
    catena_error_formatter:set_color_mode(always),
    ?assertEqual(true, catena_error_formatter:supports_color()).

color_mode_never_test() ->
    catena_error_formatter:set_color_mode(never),
    ?assertEqual(false, catena_error_formatter:supports_color()).

color_mode_never_no_ansi_test() ->
    catena_error_formatter:set_color_mode(never),
    Result = lists:flatten(catena_error_formatter:red("text")),
    ?assertEqual("text", Result).

set_color_mode_test() ->
    ?assertEqual(ok, catena_error_formatter:set_color_mode(always)),
    ?assertEqual(ok, catena_error_formatter:set_color_mode(never)),
    ?assertEqual(ok, catena_error_formatter:set_color_mode(auto)).

%%====================================================================
%% Simple Error Formatting Tests
%%====================================================================

format_error_simple_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Syntax error", {5, 10}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_simple(Err)),
    ?assert(string:find(Result, "error") /= nomatch),
    ?assert(string:find(Result, "E001") /= nomatch),
    ?assert(string:find(Result, "Syntax error") /= nomatch),
    ?assert(string:find(Result, "test.catena:5:10") /= nomatch).

format_error_simple_no_file_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {10, undefined}, undefined),
    Result = lists:flatten(catena_error_formatter:format_error_simple(Err)),
    ?assert(string:find(Result, "10") /= nomatch).

format_error_simple_warning_test() ->
    catena_error_formatter:set_color_mode(never),
    Warn = catena_error:new_warning('W001', "Unused variable", {3, 5}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_simple(Warn)),
    ?assert(string:find(Result, "warning") /= nomatch),
    ?assert(string:find(Result, "W001") /= nomatch).

format_error_simple_note_test() ->
    catena_error_formatter:set_color_mode(never),
    Note = catena_error:new_note('N001', "Consider refactoring", {7, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_simple(Note)),
    ?assert(string:find(Result, "note") /= nomatch),
    ?assert(string:find(Result, "N001") /= nomatch).

%%====================================================================
%% Full Error Formatting Tests
%%====================================================================

format_error_basic_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Syntax error", {5, 10}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "error[E001]") /= nomatch),
    ?assert(string:find(Result, "Syntax error") /= nomatch),
    ?assert(string:find(Result, "test.catena:5:10") /= nomatch).

format_error_with_context_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Missing end", {3, 1}, "test.catena"),
    Err2 = catena_error:add_context(Err, ["line 1", "line 2"], "line 3 error", ["line 4"]),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "line 1") /= nomatch),
    ?assert(string:find(Result, "line 2") /= nomatch),
    ?assert(string:find(Result, "line 3 error") /= nomatch),
    ?assert(string:find(Result, "line 4") /= nomatch).

format_error_with_suggestion_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Missing end", {3, 1}, "test.catena"),
    Err2 = catena_error:add_suggestion(Err, "Add 'end' to close match expression"),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "help:") /= nomatch),
    ?assert(string:find(Result, "Add 'end'") /= nomatch).

format_error_with_related_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Main error", {5, 1}, "test.catena"),
    Note = catena_error:new_note('N001', "Related note", {3, 1}, "test.catena"),
    Err2 = catena_error:add_related(Err, Note),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "Main error") /= nomatch),
    ?assert(string:find(Result, "Related note") /= nomatch).

format_error_no_file_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "error[E001]") /= nomatch).

format_error_no_column_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {5, undefined}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "test.catena:5") /= nomatch).

format_error_with_column_highlight_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {3, 5}, "test.catena"),
    Err2 = catena_error:add_context(Err, [], "line text", []),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "^") /= nomatch).

%%====================================================================
%% Error List Formatting Tests
%%====================================================================

format_error_list_empty_test() ->
    catena_error_formatter:set_color_mode(never),
    Result = lists:flatten(catena_error_formatter:format_error_list([])),
    ?assertEqual("\n\n", Result).

format_error_list_single_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {1, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_list([Err])),
    ?assert(string:find(Result, "error[E001]") /= nomatch),
    ?assert(string:find(Result, "1 error") /= nomatch).

format_error_list_multiple_test() ->
    catena_error_formatter:set_color_mode(never),
    Err1 = catena_error:new_error('E001', "Error 1", {1, 1}, "test.catena"),
    Err2 = catena_error:new_error('E002', "Error 2", {2, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_list([Err1, Err2])),
    ?assert(string:find(Result, "Error 1") /= nomatch),
    ?assert(string:find(Result, "Error 2") /= nomatch),
    ?assert(string:find(Result, "2 errors") /= nomatch).

format_error_list_mixed_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {1, 1}, "test.catena"),
    Warn = catena_error:new_warning('W001', "Warning", {2, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_list([Err, Warn])),
    ?assert(string:find(Result, "1 error") /= nomatch),
    ?assert(string:find(Result, "1 warning") /= nomatch).

format_error_list_warnings_only_test() ->
    catena_error_formatter:set_color_mode(never),
    Warn1 = catena_error:new_warning('W001', "Warning 1", {1, 1}, "test.catena"),
    Warn2 = catena_error:new_warning('W002', "Warning 2", {2, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error_list([Warn1, Warn2])),
    ?assert(string:find(Result, "2 warnings") /= nomatch),
    ?assert(string:find(Result, "error") == nomatch).  % Should not contain "error" word

%%====================================================================
%% Color Formatting Tests
%%====================================================================

format_error_with_colors_test() ->
    catena_error_formatter:set_color_mode(always),
    Err = catena_error:new_error('E001', "Syntax error", {5, 10}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "\e[31m") /= nomatch),  % Red for error
    ?assert(string:find(Result, "\e[0m") /= nomatch).   % Reset code

format_warning_with_colors_test() ->
    catena_error_formatter:set_color_mode(always),
    Warn = catena_error:new_warning('W001', "Warning", {3, 5}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Warn)),
    ?assert(string:find(Result, "\e[33m") /= nomatch).  % Yellow for warning

format_note_with_colors_test() ->
    catena_error_formatter:set_color_mode(always),
    Note = catena_error:new_note('N001', "Note", {7, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Note)),
    ?assert(string:find(Result, "\e[34m") /= nomatch).  % Blue for note

%%====================================================================
%% Edge Case Tests
%%====================================================================

format_error_empty_context_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {1, 1}, "test.catena"),
    Err2 = catena_error:add_context(Err, [], "", []),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    ?assert(string:find(Result, "error[E001]") /= nomatch).

format_error_long_message_test() ->
    catena_error_formatter:set_color_mode(never),
    LongMsg = lists:duplicate(200, $x),
    Err = catena_error:new_error('E001', LongMsg, {1, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(length(Result) > 200).

format_error_unicode_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error: λ → ∀", {1, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "λ") /= nomatch).

format_error_zero_column_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {3, 0}, "test.catena"),
    Err2 = catena_error:add_context(Err, [], "line text", []),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    % Should handle column 0 gracefully (no highlight)
    ?assert(is_list(Result)).

format_error_large_line_number_test() ->
    catena_error_formatter:set_color_mode(never),
    Err = catena_error:new_error('E001', "Error", {99999, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    ?assert(string:find(Result, "99999") /= nomatch).

%%====================================================================
%% Security - ANSI Injection Prevention Tests
%%====================================================================

sanitize_ansi_simple_test() ->
    Input = "normal text",
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("normal text", Result).

sanitize_ansi_red_escape_test() ->
    % Test that red ANSI code is stripped
    Input = "\e[31mred text\e[0m",
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("red text", Result).

sanitize_ansi_bold_escape_test() ->
    % Test that bold ANSI code is stripped
    Input = "\e[1mbold\e[0m text",
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("bold text", Result).

sanitize_ansi_multiple_escapes_test() ->
    % Test multiple ANSI codes in one string
    Input = "\e[31mred\e[0m and \e[33myellow\e[0m",
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("red and yellow", Result).

sanitize_ansi_malicious_clear_screen_test() ->
    % Test that clear screen command is stripped (security)
    Input = "Before\e[2JAfter",
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("BeforeAfter", Result).

sanitize_ansi_malicious_cursor_move_test() ->
    % Test that cursor movement is stripped (security)
    Input = "Text\e[5;10Hhere",
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("Texthere", Result).

sanitize_ansi_binary_input_test() ->
    % Test binary input handling
    Input = <<"text with \e[31mcolor\e[0m">>,
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("text with color", Result).

sanitize_ansi_iolist_input_test() ->
    % Test iolist input handling
    Input = ["text ", "with ", "\e[31m", "color", "\e[0m"],
    Result = catena_error_formatter:sanitize_ansi(Input),
    ?assertEqual("text with color", Result).

format_error_ansi_injection_in_message_test() ->
    % Test that malicious ANSI codes in error message are stripped
    catena_error_formatter:set_color_mode(never),
    MaliciousMsg = "Error: \e[31mInjected Red\e[0m text",
    Err = catena_error:new_error('E001', MaliciousMsg, {1, 1}, "test.catena"),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    % Should not contain ANSI escape sequences from message
    ?assertEqual(nomatch, string:find(Result, "\e[31mInjected")),
    % Should contain sanitized message
    ?assertNotEqual(nomatch, string:find(Result, "Injected Red text")).

format_error_ansi_injection_in_filename_test() ->
    % Test that malicious ANSI codes in filename are stripped
    catena_error_formatter:set_color_mode(never),
    MaliciousFile = "test\e[2J.catena",  % Clear screen in filename
    Err = catena_error:new_error('E001', "Error", {1, 1}, MaliciousFile),
    Result = lists:flatten(catena_error_formatter:format_error(Err)),
    % Should not contain clear screen code
    ?assertEqual(nomatch, string:find(Result, "\e[2J")),
    % Should contain sanitized filename
    ?assertNotEqual(nomatch, string:find(Result, "test.catena")).

format_error_ansi_injection_in_source_line_test() ->
    % Test that malicious ANSI codes in source line are stripped
    catena_error_formatter:set_color_mode(never),
    MaliciousSource = "let x = \e[5;10Hmalicious",
    Err = catena_error:new_error('E001', "Error", {1, 1}, "test.catena"),
    Err2 = catena_error:add_context(Err, [], MaliciousSource, []),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    % Should not contain cursor movement
    ?assertEqual(nomatch, string:find(Result, "\e[5;10H")),
    % Should contain sanitized source
    ?assertNotEqual(nomatch, string:find(Result, "let x = malicious")).

format_error_ansi_injection_in_suggestion_test() ->
    % Test that malicious ANSI codes in suggestion are stripped
    catena_error_formatter:set_color_mode(never),
    MaliciousSugg = "Try \e[1mbold\e[0m instead",
    Err = catena_error:new_error('E001', "Error", {1, 1}, "test.catena"),
    Err2 = catena_error:add_suggestion(Err, MaliciousSugg),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    % Should not contain bold codes from user input
    ?assertEqual(nomatch, string:find(Result, "\e[1mbold\e[0m instead")),
    % Should contain sanitized suggestion
    ?assertNotEqual(nomatch, string:find(Result, "Try bold instead")).

format_error_ansi_injection_in_context_lines_test() ->
    % Test that malicious ANSI codes in context lines are stripped
    catena_error_formatter:set_color_mode(never),
    MaliciousBefore = ["line 1", "line \e[31mred\e[0m 2"],
    Err = catena_error:new_error('E001', "Error", {3, 1}, "test.catena"),
    Err2 = catena_error:add_context(Err, MaliciousBefore, "error line", []),
    Result = lists:flatten(catena_error_formatter:format_error(Err2)),
    % Should not contain red code from context
    ?assertEqual(nomatch, string:find(Result, "\e[31mred\e[0m")),
    % Should contain sanitized context
    ?assertNotEqual(nomatch, string:find(Result, "line red 2")).
