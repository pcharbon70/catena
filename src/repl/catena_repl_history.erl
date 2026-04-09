%%%-------------------------------------------------------------------
%%% @doc Catena REPL History Management
%%%
%%% This module provides command history persistence and management
%%% for the Catena REPL. History is stored in a `.cat_history` file
%%% in the user's home directory.
%%%
%%% == Features ==
%%%
%%% <ul>
%%%   <li><b>History Persistence</b> - Commands are saved to disk and
%%%       restored on REPL startup</li>
%%%   <li><b>Deduplication</b> - Consecutive duplicates are not stored</li>
%%%   <li><b>Size Limiting</b> - History file is trimmed to a maximum size</li>
%%%   <li><b>Search</b> - Search history by prefix or substring</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_history).

%% API
-export([
    new/0,
    new/1,
    add/2,
    get/2,
    get_all/1,
    search/2,
    save/2,
    load/0,
    clear/0,
    history_file/0
]).

%% Types
-type history() :: #{
    entries => [string()],
    max_size => pos_integer(),
    file => file:filename()
}.
-type index() :: pos_integer().
-type search_mode() :: prefix | substring.

%%%=============================================================================
%%% History File Location
%%%=============================================================================

%% @doc Get the path to the history file.
%% Returns ~/.cat_history by default.
-spec history_file() -> file:filename().
history_file() ->
    case init:get_argument(home) of
        {ok, [[Home]]} ->
            filename:join(Home, ".cat_history");
        _ ->
            %% Fallback to environment variable
            case os:getenv("HOME") of
                false ->
                    %% Last resort - current directory
                    ".cat_history";
                Home ->
                    filename:join(Home, ".cat_history")
            end
    end.

%%%=============================================================================
%%% History Creation
%%%=============================================================================

%% @doc Create a new history state.
-spec new() -> history().
new() ->
    #{
        entries => [],
        max_size => 1000,
        file => history_file()
    }.

%% @doc Create a new history state with custom max size.
-spec new(pos_integer()) -> history().
new(MaxSize) ->
    (new())#{max_size => MaxSize}.

%%%=============================================================================
%%% History Manipulation
%%%=============================================================================

%% @doc Add a command to history.
%% Returns updated history. Deduplicates consecutive entries.
-spec add(string(), history()) -> history().
add(Command, History) ->
    Trimmed = string:trim(Command, trailing),
    case Trimmed of
        "" ->
            History;
        _ ->
            Entries = maps:get(entries, History),
            MaxSize = maps:get(max_size, History),
            %% Skip if same as last entry
            case Entries of
                [Trimmed | _] ->
                    History;
                _ ->
                    NewEntries = lists:sublist([Trimmed | Entries], MaxSize),
                    History#{entries => NewEntries}
            end
    end.

%% @doc Get a specific history entry by index.
%% Index 1 is the most recent entry.
-spec get(index(), history()) -> {ok, string()} | {error, out_of_bounds}.
get(Index, History) when is_integer(Index), Index > 0 ->
    Entries = maps:get(entries, History),
    case Index =< length(Entries) of
        true ->
            {ok, lists:nth(Index, Entries)};
        false ->
            {error, out_of_bounds}
    end;
get(_Index, _History) ->
    {error, out_of_bounds}.

%% @doc Get all history entries (most recent first).
-spec get_all(history()) -> [string()].
get_all(History) ->
    maps:get(entries, History).

%% @doc Search history entries.
%% For prefix mode, returns entries starting with the search term.
%% For substring mode, returns entries containing the search term.
-spec search(string(), history()) -> [string()];
          ({search_mode(), string()}, history()) -> [string()].
search({Mode, Term}, History) ->
    search(Term, History, Mode);
search(Term, History) ->
    search(Term, History, prefix).

search(Term, History, Mode) ->
    Entries = maps:get(entries, History),
    lists:filter(fun(Entry) ->
        case Mode of
            prefix -> string:prefix(Entry, Term) =/= nomatch;
            substring -> string:str(Entry, Term) > 0
        end
    end, Entries).

%%%=============================================================================
%%% History Persistence
%%%=============================================================================

%% @doc Save history to file.
-spec save(history(), history()) -> ok | {error, term()}.
save(_OldHistory, NewHistory) ->
    File = maps:get(file, NewHistory),
    Entries = maps:get(entries, NewHistory),
    Content = string:join(lists:reverse(Entries), "\n") ++ "\n",
    file:write_file(File, Content).

%% @doc Load history from file.
%% Returns an empty history if file doesn't exist.
-spec load() -> history().
load() ->
    File = history_file(),
    History = new(),
    case file:read_file(File) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            Lines = string:split(Content, "\n", all),
            %% Remove empty lines and reverse (most recent first)
            Entries = lists:reverse(lists:filter(fun(L) -> L =/= "" end, Lines)),
            History#{entries => lists:sublist(Entries, maps:get(max_size, History))};
        {error, enoent} ->
            %% File doesn't exist - return empty history
            History;
        {error, Reason} ->
            io:format(standard_error, "Warning: Failed to load history: ~p~n", [Reason]),
            History
    end.

%% @doc Clear the history file.
-spec clear() -> ok | {error, term()}.
clear() ->
    File = history_file(),
    case file:delete(File) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> {error, Reason}
    end.
