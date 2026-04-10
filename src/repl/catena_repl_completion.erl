%%%-------------------------------------------------------------------
%%% @doc Catena REPL Tab Completion
%%%
%%% This module provides tab completion for the Catena REPL, including
%%% keywords, built-in functions, and user-defined identifiers.
%%%
%%% == Features ==
%%%
%%% <ul>
%%%   <li><b>Keyword Completion</b> - Complete Catena keywords (type, transform, etc.)</li>
%%%   <li><b>Command Completion</b> - Complete REPL commands (:type, :load, etc.)</li>
%%%   <li><b>Builtin Completion</b> - Complete built-in function names</li>
%%%   <li><b>Identifier Completion</b> - Complete user-defined names</li>
%%%   <li><b>Module Completion</b> - Complete module names with dot notation</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_completion).

%% API
-export([
    complete/3,
    complete_input/2,
    format_completions/1
]).

%% Internal exports for testing
-export([
    is_prefix/2
]).

%% Types
-type completion_context() :: #{
    bindings => map(),           % User-defined bindings
    runtime_bindings => map(),   % Runtime bindings from prelude
    env => map()                 % Type environment
}.
-type completion_result() :: {
    string(),         % The prefix that matched
    [string()],       % Possible completions
    string()          % Longest common prefix of completions
}.

%%%=============================================================================
%%% Completion Keywords
%%%=============================================================================

%% @doc List of Catena keywords for completion.
-define(KEYWORDS, [
    "type", "transform", "effect", "trait", "instance",
    "import", "export", "module", "where", "derives",
    "match", "with", "end", "let", "in", "if", "then", "else",
    "true", "false"
]).

%% @doc List of REPL commands for completion.
-define(COMMANDS, [
    ":type", ":t", ":load", ":l", ":browse", ":b",
    ":env", ":clear", ":prelude", ":p", ":quit", ":q",
    ":help", ":h", ":history"
]).

%% @doc List of builtin functions for completion.
-define(BUILTINS, [
    "identity", "const", "compose", "flip",
    "head", "tail", "length", "reverse", "take", "drop",
    "map", "filter", "fold", "foldRight", "append",
    "fmap", "pure", "bind", "chain", "join",
    "fromMaybe", "maybe", "isJust", "isNothing",
    "fromResult", "isOk", "isErr"
]).

%% @doc List of common type names for completion.
-define(TYPES, [
    "Bool", "True", "False",
    "Maybe", "Some", "None",
    "Result", "Ok", "Error",
    "List", "Nil", "Cons",
    "Natural", "Integer", "Float", "Text", "Atom",
    "Unit", "unit"
]).

%%%=============================================================================
%%% Public API
%%%=============================================================================

%% @doc Complete the input at the cursor position.
%% Returns {matched, completions, common_prefix} or 'no_match'.
-spec complete(string(), non_neg_integer(), completion_context()) ->
    completion_result() | no_match.
complete(Input, CursorPos, Context) ->
    %% Get the prefix before cursor
    Prefix = case CursorPos > length(Input) of
        true -> Input;
        false -> string:substr(Input, 1, CursorPos)
    end,
    complete_input(Prefix, Context).

%% @doc Complete the input based on its prefix.
%% Detects whether we're completing a command, keyword, or identifier.
-spec complete_input(string(), completion_context()) ->
    completion_result() | no_match.
complete_input("", _Context) ->
    %% Empty input - suggest commands and keywords
    Completions = ?COMMANDS ++ ?KEYWORDS,
    {matched, "", Completions, ""};
complete_input(":" ++ Rest, Context) ->
    %% Command completion
    complete_command(Rest, Context);
complete_input(Input, Context) ->
    %% Determine completion context
    case string:split(Input, " ", trailing) of
        [_, LastWord] ->
            %% Last word - might be an identifier
            complete_identifier(Input, LastWord, Context);
        [SingleWord] ->
            %% Single word - could be keyword, type, or identifier
            complete_any(SingleWord, Context)
    end.

%% @doc Format completions for display.
-spec format_completions([string()]) -> iolist().
format_completions([]) ->
    "No completions";
format_completions(Completions) ->
    %% Group by category for display
    {Commands, Keywords, Builtins, Types, Ids} = categorize_completions(Completions),
    Parts = [
        format_category("Commands", Commands),
        format_category("Keywords", Keywords),
        format_category("Builtins", Builtins),
        format_category("Types", Types),
        format_category("Identifiers", Ids)
    ],
    lists:filter(fun(P) -> P =/= "" end, Parts).

%%%=============================================================================
%%% Command Completion
%%%=============================================================================

complete_command("", _Context) ->
    Completions = ?COMMANDS,
    {matched, ":", Completions, common_prefix(Completions)};
complete_command(Prefix, _Context) ->
    FullPrefix = ":" ++ Prefix,
    Completions = [C || C <- ?COMMANDS, is_prefix(FullPrefix, C)],
    case Completions of
        [] -> no_match;
        _ -> {matched, FullPrefix, Completions, common_prefix(Completions)}
    end.

%%%=============================================================================
%%% Identifier/Keyword Completion
%%%=============================================================================

complete_any(Input, Context) ->
    %% Collect all possible completions
    AllCompletions = collect_completions(Context),
    Matching = [C || C <- AllCompletions, is_prefix(Input, C)],
    case Matching of
        [] -> no_match;
        _ -> {matched, Input, Matching, common_prefix(Matching)}
    end.

complete_identifier(FullInput, LastWord, Context) ->
    %% Complete just the last word
    AllCompletions = collect_completions(Context),
    Matching = [C || C <- AllCompletions, is_prefix(LastWord, C)],
    case Matching of
        [] -> no_match;
        _ ->
            %% Replace last word with completions
            PrefixLen = length(FullInput) - length(LastWord),
            Prefix = string:substr(FullInput, 1, PrefixLen),
            {matched, LastWord, Matching, Prefix ++ common_prefix(Matching)}
    end.

collect_completions(Context) ->
    %% Get all possible completions from context
    UserBindings = maps:get(bindings, Context, #{}),
    RuntimeBindings = maps:get(runtime_bindings, Context, #{}),
    EnvBindings = case maps:get(env, Context, undefined) of
        undefined -> [];
        Env -> [to_string(K) || K <- maps:keys(Env)]
    end,
    %% Combine all sources
    lists:usort(lists:flatten([
        ?KEYWORDS,
        ?BUILTINS,
        ?TYPES,
        [to_string(K) || K <- maps:keys(UserBindings)],
        [to_string(K) || K <- maps:keys(RuntimeBindings)],
        EnvBindings
    ])).

%% Convert atom or string to string
to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(S) when is_list(S) -> S;
to_string(_) -> "".

%% Check if Prefix is a prefix of String
is_prefix(Prefix, String) when is_list(Prefix), is_list(String) ->
    case string:prefix(String, Prefix) of
        nomatch -> false;
        _ -> true
    end;
is_prefix(_, _) ->
    false.

%%%=============================================================================
%%% Module/Qualified Name Completion
%%%=============================================================================

%% @doc Complete a qualified name (module.name).
%% This is a placeholder for future module-aware completion.
-spec complete_qualified(string(), completion_context()) ->
    completion_result() | no_match.
complete_qualified(Input, _Context) ->
    case string:split(Input, ".") of
        [ModulePrefix, ""] ->
            %% User typed "module." - suggest completions from that module
            %% For now, just return a placeholder
            {matched, Input, [], Input};
        [ModulePrefix, NamePrefix] ->
            %% User typed "module.name" - complete both parts
            {matched, Input, [], Input};
        _ ->
            no_match
    end.

%%%=============================================================================
%%% Helpers
%%%=============================================================================

%% @doc Find the longest common prefix of a list of strings.
-spec common_prefix([string()]) -> string().
common_prefix([]) ->
    "";
common_prefix([S]) ->
    S;
common_prefix(Strings) ->
    common_prefix(Strings, 1, "").

common_prefix(_Strings, Pos, _Acc) when Pos > length(hd(_Strings)) ->
    "";
common_prefix(Strings, Pos, Acc) ->
    First = hd(Strings),
    case lists:all(fun(S) ->
        length(S) >= Pos andalso lists:nth(Pos, S) =:= lists:nth(Pos, First)
    end, Strings) of
        true ->
            common_prefix(Strings, Pos + 1, Acc ++ [lists:nth(Pos, First)]);
        false ->
            Acc
    end.

%% @doc Categorize completions for pretty display.
-spec categorize_completions([string()]) -> {[string()], [string()], [string()], [string()], [string()]}.
categorize_completions(Completions) ->
    Commands = [C || C <- Completions, lists:prefix(":", C)],
    Keywords = [C || C <- Completions, lists:member(C, ?KEYWORDS)],
    Builtins = [C || C <- Completions, lists:member(C, ?BUILTINS)],
    Types = [C || C <- Completions, lists:member(C, ?TYPES)],
    Ids = Completions -- Commands -- Keywords -- Builtins -- Types,
    {Commands, Keywords, Builtins, Types, Ids}.

%% @doc Format a category of completions.
-spec format_category(string(), [string()]) -> iolist().
format_category(_Title, []) ->
    "";
format_category(Title, Items) ->
    [
        "\n  ", Title, ":",
        format_items(Items)
    ].

%% @doc Format items in columns.
-spec format_items([string()]) -> iolist().
format_items(Items) ->
    %% Simple column formatting - 4 items per row
    format_items(Items, 0).

format_items([], _N) ->
    "";
format_items([Item], _N) ->
    ["\n    ", Item];
format_items([Item | Rest], N) when N >= 3 ->
    ["\n    ", Item | format_items(Rest, 0)];
format_items([Item | Rest], N) ->
    ["    ", Item | format_items(Rest, N + 1)].
