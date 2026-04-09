%%%-------------------------------------------------------------------
%%% @doc Unit tests for catena_repl_history
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_history_tests).
-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup and Cleanup
%%%=============================================================================

%% Temp file generator for testing
temp_file() ->
    "/tmp/test_cat_history_" ++ integer_to_list(erlang:unique_integer()).

cleanup_file(File) ->
    file:delete(File),
    ok.

%%%=============================================================================
%%% History File Tests
%%%=============================================================================

history_file_test() ->
    File = catena_repl_history:history_file(),
    ?assert(is_list(File)),
    ?assert(string:str(File, ".cat_history") > 0).

%%%=============================================================================
%%% History Creation Tests
%%%=============================================================================

new_test() ->
    History = catena_repl_history:new(),
    ?assertEqual([], maps:get(entries, History)),
    ?assertEqual(1000, maps:get(max_size, History)).

new_with_size_test() ->
    History = catena_repl_history:new(100),
    ?assertEqual(100, maps:get(max_size, History)).

%%%=============================================================================
%%% Add Entry Tests
%%%=============================================================================

add_single_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    ?assertEqual(["1 + 1"], maps:get(entries, History1)).

add_multiple_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    History2 = catena_repl_history:add("2 + 2", History1),
    ?assertEqual(["2 + 2", "1 + 1"], maps:get(entries, History2)).

add_empty_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("", History0),
    ?assertEqual([], maps:get(entries, History1)).

add_whitespace_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("   ", History0),
    ?assertEqual([], maps:get(entries, History1)).

add_duplicate_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    History2 = catena_repl_history:add("1 + 1", History1),
    %% Consecutive duplicate should not be added
    ?assertEqual(["1 + 1"], maps:get(entries, History2)).

add_nonconsecutive_duplicate_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    History2 = catena_repl_history:add("2 + 2", History1),
    History3 = catena_repl_history:add("1 + 1", History2),
    %% Non-consecutive duplicate should be added
    ?assertEqual(["1 + 1", "2 + 2", "1 + 1"], maps:get(entries, History3)).

max_size_test() ->
    History0 = catena_repl_history:new(3),
    History1 = catena_repl_history:add("1", History0),
    History2 = catena_repl_history:add("2", History1),
    History3 = catena_repl_history:add("3", History2),
    History4 = catena_repl_history:add("4", History3),
    ?assertEqual(["4", "3", "2"], maps:get(entries, History4)).

%%%=============================================================================
%%% Get Entry Tests
%%%=============================================================================

get_first_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    ?assertEqual({ok, "1 + 1"}, catena_repl_history:get(1, History1)).

get_second_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    History2 = catena_repl_history:add("2 + 2", History1),
    ?assertEqual({ok, "2 + 2"}, catena_repl_history:get(1, History2)),
    ?assertEqual({ok, "1 + 1"}, catena_repl_history:get(2, History2)).

get_out_of_bounds_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1 + 1", History0),
    ?assertEqual({error, out_of_bounds}, catena_repl_history:get(2, History1)).

get_invalid_index_test() ->
    History = catena_repl_history:new(),
    ?assertEqual({error, out_of_bounds}, catena_repl_history:get(0, History)),
    ?assertEqual({error, out_of_bounds}, catena_repl_history:get(-1, History)).

%%%=============================================================================
%%% Get All Tests
%%%=============================================================================

get_all_empty_test() ->
    History = catena_repl_history:new(),
    ?assertEqual([], catena_repl_history:get_all(History)).

get_all_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("1", History0),
    History2 = catena_repl_history:add("2", History1),
    History3 = catena_repl_history:add("3", History2),
    ?assertEqual(["3", "2", "1"], catena_repl_history:get_all(History3)).

%%%=============================================================================
%%% Search Tests
%%%=============================================================================

search_prefix_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add(":load foo", History0),
    History2 = catena_repl_history:add(":type bar", History1),
    History3 = catena_repl_history:add("1 + 1", History2),
    Results = catena_repl_history:search(":", History3),
    ?assertEqual(2, length(Results)).

search_substring_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add("map function", History0),
    History2 = catena_repl_history:add("filter data", History1),
    History3 = catena_repl_history:add("fold list", History2),
    Results = catena_repl_history:search({substring, "map"}, History3),
    ?assertEqual(1, length(Results)),
    ?assertEqual("map function", hd(Results)).

search_mode_test() ->
    History0 = catena_repl_history:new(),
    History1 = catena_repl_history:add(":type x", History0),
    History2 = catena_repl_history:add(":load y", History1),
    History3 = catena_repl_history:add("x + y", History2),
    PrefixResults = catena_repl_history:search({prefix, ":"}, History3),
    ?assertEqual(2, length(PrefixResults)),
    SubstringResults = catena_repl_history:search({substring, "x"}, History3),
    ?assertEqual(2, length(SubstringResults)).

%%%=============================================================================
%%% Persistence Tests
%%%=============================================================================

save_load_test() ->
    TempFile = temp_file(),
    try
        %% Create history with custom file
        History0 = catena_repl_history:new(),
        History1 = History0#{file => TempFile},
        History2 = catena_repl_history:add("cmd1", History1),
        History3 = catena_repl_history:add("cmd2", History2),
        History4 = catena_repl_history:add("cmd3", History3),
        %% Save
        ok = catena_repl_history:save(History3, History4),
        %% Load manually
        {ok, Binary} = file:read_file(TempFile),
        Content = binary_to_list(Binary),
        ?assert(string:str(Content, "cmd1") > 0),
        ?assert(string:str(Content, "cmd2") > 0),
        ?assert(string:str(Content, "cmd3") > 0)
    after
        cleanup_file(TempFile)
    end.

clear_test() ->
    TempFile = temp_file(),
    try
        %% Create history file
        History0 = catena_repl_history:new(),
        History1 = History0#{file => TempFile},
        History2 = catena_repl_history:add("cmd", History1),
        ok = catena_repl_history:save(History0, History2),
        %% Verify file exists
        ?assertMatch({ok, _}, file:read_file(TempFile)),
        %% Clear file (not the module function - just verify delete works)
        ok = file:delete(TempFile),
        ?assertEqual({error, enoent}, file:read_file(TempFile))
    after
        cleanup_file(TempFile)
    end.
