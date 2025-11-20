-module(catena_error_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../../../src/compiler/error/catena_error.hrl").

%%====================================================================
%% Error Creation Tests
%%====================================================================

new_error_test() ->
    Err = catena_error:new_error('E001', "Syntax error", {5, 10}, "test.catena"),
    ?assertEqual(error, Err#error.severity),
    ?assertEqual('E001', Err#error.code),
    ?assertEqual("Syntax error", Err#error.message),
    ?assertEqual("test.catena", Err#error.file),
    ?assertEqual(5, Err#error.line),
    ?assertEqual(10, Err#error.column),
    ?assertEqual(undefined, Err#error.source_line),
    ?assertEqual([], Err#error.context_before),
    ?assertEqual([], Err#error.context_after),
    ?assertEqual(undefined, Err#error.suggestion),
    ?assertEqual([], Err#error.related).

new_error_no_file_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    ?assertEqual(undefined, Err#error.file).

new_error_no_column_test() ->
    Err = catena_error:new_error('E002', "Error", {10, undefined}, "test.catena"),
    ?assertEqual(undefined, Err#error.column).

new_warning_test() ->
    Warn = catena_error:new_warning('W001', "Unused variable", {3, 5}, "test.catena"),
    ?assertEqual(warning, Warn#error.severity),
    ?assertEqual('W001', Warn#error.code),
    ?assertEqual("Unused variable", Warn#error.message).

new_note_test() ->
    Note = catena_error:new_note('N001', "Consider refactoring", {7, 1}, "test.catena"),
    ?assertEqual(note, Note#error.severity),
    ?assertEqual('N001', Note#error.code).

%%====================================================================
%% Error Manipulation Tests
%%====================================================================

add_suggestion_test() ->
    Err = catena_error:new_error('E001', "Missing end", {10, 1}, undefined),
    Err2 = catena_error:add_suggestion(Err, "Add 'end' to close match expression"),
    ?assertEqual("Add 'end' to close match expression", Err2#error.suggestion).

add_suggestion_empty_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Err2 = catena_error:add_suggestion(Err, ""),
    ?assertEqual("", Err2#error.suggestion).

add_context_test() ->
    Err = catena_error:new_error('E001', "Error", {3, 1}, undefined),
    Err2 = catena_error:add_context(Err, ["line 1", "line 2"], "line 3 error", ["line 4"]),
    ?assertEqual(["line 1", "line 2"], Err2#error.context_before),
    ?assertEqual("line 3 error", Err2#error.source_line),
    ?assertEqual(["line 4"], Err2#error.context_after).

add_context_empty_before_after_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Err2 = catena_error:add_context(Err, [], "only line", []),
    ?assertEqual([], Err2#error.context_before),
    ?assertEqual("only line", Err2#error.source_line),
    ?assertEqual([], Err2#error.context_after).

add_related_single_test() ->
    Err1 = catena_error:new_error('E001', "Main error", {5, 1}, undefined),
    Err2 = catena_error:new_note('N001', "Related note", {3, 1}, undefined),
    Err3 = catena_error:add_related(Err1, Err2),
    ?assertEqual([Err2], Err3#error.related).

add_related_multiple_test() ->
    Err1 = catena_error:new_error('E001', "Main error", {5, 1}, undefined),
    Note1 = catena_error:new_note('N001', "Note 1", {3, 1}, undefined),
    Note2 = catena_error:new_note('N002', "Note 2", {7, 1}, undefined),
    Err2 = catena_error:add_related(Err1, [Note1, Note2]),
    ?assertEqual([Note1, Note2], Err2#error.related).

add_related_accumulate_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Note1 = catena_error:new_note('N001', "Note 1", {1, 1}, undefined),
    Note2 = catena_error:new_note('N002', "Note 2", {1, 1}, undefined),
    Err2 = catena_error:add_related(Err, Note1),
    Err3 = catena_error:add_related(Err2, Note2),
    ?assertEqual(2, length(Err3#error.related)).

set_source_line_test() ->
    Err = catena_error:new_error('E001', "Error", {5, 1}, undefined),
    Err2 = catena_error:set_source_line(Err, "transform foo = bar"),
    ?assertEqual("transform foo = bar", Err2#error.source_line).

%%====================================================================
%% Error Accumulation Tests
%%====================================================================

accumulate_empty_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Errors = catena_error:accumulate([], Err),
    ?assertEqual([Err], Errors).

accumulate_multiple_test() ->
    Err1 = catena_error:new_error('E001', "Error 1", {1, 1}, undefined),
    Err2 = catena_error:new_error('E002', "Error 2", {2, 1}, undefined),
    Err3 = catena_error:new_error('E003', "Error 3", {3, 1}, undefined),
    Errors1 = catena_error:accumulate([], Err1),
    Errors2 = catena_error:accumulate(Errors1, Err2),
    Errors3 = catena_error:accumulate(Errors2, Err3),
    ?assertEqual(3, length(Errors3)),
    ?assertEqual([Err1, Err2, Err3], Errors3).

has_errors_empty_test() ->
    ?assertEqual(false, catena_error:has_errors([])).

has_errors_with_errors_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    ?assertEqual(true, catena_error:has_errors([Err])).

has_errors_warnings_only_test() ->
    Warn = catena_error:new_warning('W001', "Warning", {1, 1}, undefined),
    ?assertEqual(false, catena_error:has_errors([Warn])).

has_errors_mixed_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Warn = catena_error:new_warning('W001', "Warning", {2, 1}, undefined),
    ?assertEqual(true, catena_error:has_errors([Warn, Err])).

get_errors_empty_test() ->
    ?assertEqual([], catena_error:get_errors([])).

get_errors_only_errors_test() ->
    Err1 = catena_error:new_error('E001', "Error 1", {1, 1}, undefined),
    Err2 = catena_error:new_error('E002', "Error 2", {2, 1}, undefined),
    ?assertEqual([Err1, Err2], catena_error:get_errors([Err1, Err2])).

get_errors_mixed_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Warn = catena_error:new_warning('W001', "Warning", {2, 1}, undefined),
    Note = catena_error:new_note('N001', "Note", {3, 1}, undefined),
    ?assertEqual([Err], catena_error:get_errors([Err, Warn, Note])).

get_warnings_empty_test() ->
    ?assertEqual([], catena_error:get_warnings([])).

get_warnings_only_warnings_test() ->
    Warn1 = catena_error:new_warning('W001', "Warning 1", {1, 1}, undefined),
    Warn2 = catena_error:new_warning('W002', "Warning 2", {2, 1}, undefined),
    ?assertEqual([Warn1, Warn2], catena_error:get_warnings([Warn1, Warn2])).

get_warnings_mixed_test() ->
    Err = catena_error:new_error('E001', "Error", {1, 1}, undefined),
    Warn = catena_error:new_warning('W001', "Warning", {2, 1}, undefined),
    Note = catena_error:new_note('N001', "Note", {3, 1}, undefined),
    ?assertEqual([Warn], catena_error:get_warnings([Err, Warn, Note])).

%%====================================================================
%% Source Context Extraction Tests
%%====================================================================

read_source_context_test() ->
    % Create temporary test file
    TestFile = "/tmp/catena_test_source.catena",
    Content = "line 1\nline 2\nline 3\nline 4\nline 5\n",
    ok = file:write_file(TestFile, Content),

    % Read context around line 3
    {ok, Context} = catena_error:read_source_context(TestFile, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual(["line 4"], maps:get('after', Context)),

    % Cleanup
    test_helpers:delete_test_file(TestFile).

read_source_context_first_line_test() ->
    TestFile = "/tmp/catena_test_first.catena",
    Content = "line 1\nline 2\nline 3\n",
    ok = file:write_file(TestFile, Content),

    {ok, Context} = catena_error:read_source_context(TestFile, 1, 2),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("line 1", maps:get(error_line, Context)),
    ?assertEqual(["line 2", "line 3"], maps:get('after', Context)),

    test_helpers:delete_test_file(TestFile).

read_source_context_last_line_test() ->
    TestFile = "/tmp/catena_test_last.catena",
    Content = "line 1\nline 2\nline 3\n",
    ok = file:write_file(TestFile, Content),

    {ok, Context} = catena_error:read_source_context(TestFile, 3, 2),
    ?assertEqual(["line 1", "line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)),

    test_helpers:delete_test_file(TestFile).

read_source_context_zero_context_test() ->
    TestFile = "/tmp/catena_test_zero.catena",
    Content = "line 1\nline 2\nline 3\n",
    ok = file:write_file(TestFile, Content),

    {ok, Context} = catena_error:read_source_context(TestFile, 2, 0),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("line 2", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)),

    test_helpers:delete_test_file(TestFile).

read_source_context_file_not_found_test() ->
    {error, {file_read_error, enoent}} = catena_error:read_source_context("/tmp/catena_nonexistent_file.catena", 1, 1).

read_source_context_line_out_of_bounds_test() ->
    TestFile = "/tmp/catena_test_bounds.catena",
    Content = "line 1\nline 2\n",
    ok = file:write_file(TestFile, Content),

    {error, line_out_of_bounds} = catena_error:read_source_context(TestFile, 10, 1),
    {error, line_out_of_bounds} = catena_error:read_source_context(TestFile, 0, 1),

    test_helpers:delete_test_file(TestFile).

extract_context_from_file_test() ->
    Lines = ["line 1", "line 2", "line 3", "line 4", "line 5"],
    {ok, Context} = catena_error:extract_context_from_file(Lines, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual(["line 4"], maps:get('after', Context)).

extract_context_from_file_first_line_test() ->
    Lines = ["line 1", "line 2", "line 3"],
    {ok, Context} = catena_error:extract_context_from_file(Lines, 1, 1),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("line 1", maps:get(error_line, Context)),
    ?assertEqual(["line 2"], maps:get('after', Context)).

extract_context_from_file_last_line_test() ->
    Lines = ["line 1", "line 2", "line 3"],
    {ok, Context} = catena_error:extract_context_from_file(Lines, 3, 1),
    ?assertEqual(["line 2"], maps:get(before, Context)),
    ?assertEqual("line 3", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)).

extract_context_from_file_out_of_bounds_test() ->
    Lines = ["line 1", "line 2"],
    {error, line_out_of_bounds} = catena_error:extract_context_from_file(Lines, 10, 1),
    {error, line_out_of_bounds} = catena_error:extract_context_from_file(Lines, 0, 1).

extract_context_large_context_test() ->
    Lines = ["1", "2", "3", "4", "5", "6", "7", "8", "9", "10"],
    {ok, Context} = catena_error:extract_context_from_file(Lines, 5, 3),
    ?assertEqual(["2", "3", "4"], maps:get(before, Context)),
    ?assertEqual("5", maps:get(error_line, Context)),
    ?assertEqual(["6", "7", "8"], maps:get('after', Context)).

extract_context_single_line_file_test() ->
    Lines = ["only line"],
    {ok, Context} = catena_error:extract_context_from_file(Lines, 1, 5),
    ?assertEqual([], maps:get(before, Context)),
    ?assertEqual("only line", maps:get(error_line, Context)),
    ?assertEqual([], maps:get('after', Context)).

%%====================================================================
%% Path Validation Security Tests
%%====================================================================

%% Basic path validation
validate_path_safe_relative_test() ->
    % Safe relative path within workspace
    {ok, Cwd} = file:get_cwd(),
    TestFile = filename:join(Cwd, "test.catena"),
    ?assertMatch({ok, _}, catena_error:validate_source_path(TestFile)).

validate_path_safe_subdir_test() ->
    % Safe path in subdirectory
    {ok, Cwd} = file:get_cwd(),
    TestFile = filename:join([Cwd, "src", "test.catena"]),
    ?assertMatch({ok, _}, catena_error:validate_source_path(TestFile)).

%% Path traversal attacks
validate_path_rejects_parent_traversal_test() ->
    % Reject path with .. traversal
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("../etc/passwd")).

validate_path_rejects_deep_traversal_test() ->
    % Reject deep path traversal
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("../../../../etc/passwd")).

validate_path_rejects_mixed_traversal_test() ->
    % Reject mixed path with traversal
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("src/../../etc/passwd")).

%% System path protection
validate_path_rejects_etc_test() ->
    % Reject /etc directory access
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("/etc/passwd")).

validate_path_rejects_sys_test() ->
    % Reject /sys directory access
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("/sys/kernel/debug")).

validate_path_rejects_proc_test() ->
    % Reject /proc directory access
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("/proc/self/environ")).

validate_path_rejects_root_test() ->
    % Reject /root directory access
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("/root/.ssh/id_rsa")).

validate_path_rejects_var_log_test() ->
    % Reject /var/log directory (sensitive logs)
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path("/var/log/syslog")).

%% Null byte injection
validate_path_rejects_null_byte_test() ->
    % Reject path with null byte (path obfuscation)
    PathWithNull = "test.catena" ++ [0] ++ "/etc/passwd",
    ?assertEqual({error, path_traversal_attack},
                 catena_error:validate_source_path(PathWithNull)).

%% Integration: read_source_context with path validation
read_source_context_blocks_traversal_test() ->
    % Attempt to read /etc/passwd via read_source_context
    Result = catena_error:read_source_context("/etc/passwd", 1, 1),
    ?assertMatch({error, {path_traversal_attack, _}}, Result).

read_source_context_blocks_parent_traversal_test() ->
    % Attempt path traversal via read_source_context
    Result = catena_error:read_source_context("../../../etc/passwd", 1, 1),
    ?assertMatch({error, {path_traversal_attack, _}}, Result).

read_source_context_allows_safe_path_test() ->
    % Safe path should work (within /tmp/ which is allowed)
    test_helpers:with_temp_file("test content\n", fun(TestFile) ->
        Result = catena_error:read_source_context(TestFile, 1, 0),
        % Should either succeed or fail due to file issue, not path_traversal_attack
        case Result of
            {ok, _} -> ok;
            {error, {file_read_error, _}} -> ok;
            {error, {path_traversal_attack, _}} ->
                ?assert(false)  % Should not be a path traversal error
        end
    end).

%%====================================================================
%% Unicode Handling Tests
%%====================================================================

read_source_context_invalid_utf8_test() ->
    % Create a file with invalid UTF-8 sequence
    % Invalid UTF-8: continuation byte without start byte
    InvalidUtf8 = <<16#80, 16#81, 16#82>>,
    test_helpers:with_temp_file(InvalidUtf8, fun(TestFile) ->
        Result = catena_error:read_source_context(TestFile, 1, 1),
        % Should return unicode error
        ?assertMatch({error, {unicode_error, invalid_encoding}}, Result)
    end).

read_source_context_incomplete_utf8_test() ->
    % Create a file with incomplete UTF-8 sequence at end
    % Incomplete UTF-8: start of 2-byte sequence without continuation
    IncompleteUtf8 = <<"type Foo = Bar\n", 16#C2>>,
    test_helpers:with_temp_file(IncompleteUtf8, fun(TestFile) ->
        Result = catena_error:read_source_context(TestFile, 1, 1),
        % Should return unicode error
        ?assertMatch({error, {unicode_error, incomplete_encoding}}, Result)
    end).

read_source_context_valid_utf8_test() ->
    % Create a file with valid UTF-8 including multi-byte characters
    % Valid UTF-8 with lambda, arrow, and forall symbols (properly encoded)
    ValidUtf8String = "type λ → ∀\n",
    ValidUtf8Binary = unicode:characters_to_binary(ValidUtf8String, utf8, utf8),
    test_helpers:with_temp_file(ValidUtf8Binary, fun(TestFile) ->
        Result = catena_error:read_source_context(TestFile, 1, 0),
        % Should successfully parse (not return unicode error)
        case Result of
            {ok, Context} ->
                ErrorLine = maps:get(error_line, Context),
                % Should contain valid content (test passes if we get here)
                ?assert(is_list(ErrorLine));
            {error, {unicode_error, _}} ->
                ?assert(false);  % Should not be unicode error for valid UTF-8
            {error, _OtherError} ->
                % Might be other errors (file not found, etc.)
                ok
        end
    end).

%%====================================================================
%% Resource Limit Tests
%%====================================================================

% Test that context lines exceeding MAX_CONTEXT_LINES is rejected
read_source_context_exceeds_max_test() ->
    test_helpers:with_temp_file("type Foo = Bar\n", fun(TestFile) ->
        % Try to read 101 context lines (MAX_CONTEXT_LINES = 100)
        Result = catena_error:read_source_context(TestFile, 1, 101),
        ?assertMatch({error, {context_too_large, 101, 100}}, Result)
    end).

% Test that negative context lines is rejected
read_source_context_negative_test() ->
    test_helpers:with_temp_file("type Foo = Bar\n", fun(TestFile) ->
        % Try to read negative context lines
        Result = catena_error:read_source_context(TestFile, 1, -5),
        ?assertMatch({error, negative_context_lines}, Result)
    end).

% Test that exactly MAX_CONTEXT_LINES is allowed
read_source_context_at_max_test() ->
    test_helpers:with_temp_file("type Foo = Bar\n", fun(TestFile) ->
        % Read exactly 100 context lines (should be allowed)
        Result = catena_error:read_source_context(TestFile, 1, 100),
        ?assertMatch({ok, _}, Result)
    end).

% Test that adding related errors beyond MAX_RELATED_DEPTH is silently dropped
add_related_exceeds_max_test() ->
    BaseErr = catena_error:new_error('E001_test', "Base error", {1, 1}, "test.catena"),

    % Create 15 related errors (MAX_RELATED_DEPTH = 10)
    RelatedErrs = [
        catena_error:new_note('N001_test', "Related 1", {2, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 2", {3, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 3", {4, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 4", {5, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 5", {6, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 6", {7, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 7", {8, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 8", {9, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 9", {10, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 10", {11, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 11", {12, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 12", {13, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 13", {14, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 14", {15, 1}, "test.catena"),
        catena_error:new_note('N001_test', "Related 15", {16, 1}, "test.catena")
    ],

    % Add all 15 at once (should cap at 10)
    UpdatedErr = catena_error:add_related(BaseErr, RelatedErrs),

    % Verify only 10 were added
    #error{related = Related} = UpdatedErr,
    ?assertEqual(10, length(Related)).

% Test that adding single related errors one at a time respects limit
add_related_one_at_a_time_test() ->
    BaseErr = catena_error:new_error('E001_test', "Base error", {1, 1}, "test.catena"),

    % Add 12 related errors one at a time
    Err1 = catena_error:add_related(BaseErr, catena_error:new_note('N001_test', "R1", {2, 1}, "test.catena")),
    Err2 = catena_error:add_related(Err1, catena_error:new_note('N001_test', "R2", {3, 1}, "test.catena")),
    Err3 = catena_error:add_related(Err2, catena_error:new_note('N001_test', "R3", {4, 1}, "test.catena")),
    Err4 = catena_error:add_related(Err3, catena_error:new_note('N001_test', "R4", {5, 1}, "test.catena")),
    Err5 = catena_error:add_related(Err4, catena_error:new_note('N001_test', "R5", {6, 1}, "test.catena")),
    Err6 = catena_error:add_related(Err5, catena_error:new_note('N001_test', "R6", {7, 1}, "test.catena")),
    Err7 = catena_error:add_related(Err6, catena_error:new_note('N001_test', "R7", {8, 1}, "test.catena")),
    Err8 = catena_error:add_related(Err7, catena_error:new_note('N001_test', "R8", {9, 1}, "test.catena")),
    Err9 = catena_error:add_related(Err8, catena_error:new_note('N001_test', "R9", {10, 1}, "test.catena")),
    Err10 = catena_error:add_related(Err9, catena_error:new_note('N001_test', "R10", {11, 1}, "test.catena")),
    % These should be silently dropped
    Err11 = catena_error:add_related(Err10, catena_error:new_note('N001_test', "R11", {12, 1}, "test.catena")),
    Err12 = catena_error:add_related(Err11, catena_error:new_note('N001_test', "R12", {13, 1}, "test.catena")),

    % Verify only 10 were added
    #error{related = Related} = Err12,
    ?assertEqual(10, length(Related)).
