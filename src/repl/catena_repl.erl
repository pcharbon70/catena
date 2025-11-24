%% @doc Catena Interactive REPL
%%
%% Provides a Read-Eval-Print Loop for interactive Catena development.
%% Supports expression evaluation, definition accumulation, and
%% introspection commands.
%%
%% @end
-module(catena_repl).

-export([
    start/0,
    start/1,
    stop/0,
    eval/2,
    get_env/1
]).

%% Internal exports for testing
-export([
    parse_input/1,
    is_command/1,
    is_complete/1
]).

-record(repl_state, {
    env :: map(),              % Type environment
    bindings :: map(),         % Value bindings (name -> typed AST)
    history :: [string()],     % Command history
    prompt :: string(),        % Current prompt
    continuation :: string()   % Accumulated incomplete input
}).

-define(VERSION, "0.1.0").
-define(PROMPT, "catena> ").
-define(CONT_PROMPT, "      | ").

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the REPL with default options.
-spec start() -> ok.
start() ->
    start(#{}).

%% @doc Start the REPL with options.
%% Options:
%%   - quiet: boolean() - suppress banner
%%   - env: map() - initial type environment
-spec start(map()) -> ok.
start(Opts) ->
    case maps:get(quiet, Opts, false) of
        false -> print_banner();
        true -> ok
    end,
    InitEnv = maps:get(env, Opts, init_env()),
    State = #repl_state{
        env = InitEnv,
        bindings = #{},
        history = [],
        prompt = ?PROMPT,
        continuation = ""
    },
    loop(State).

%% @doc Stop the REPL (called from :quit command).
-spec stop() -> no_return().
stop() ->
    io:format("Goodbye!~n"),
    halt(0).

%% @doc Evaluate an input string in the given state.
%% Returns {Result, NewState} or {error, Reason, State}.
-spec eval(string(), #repl_state{}) ->
    {ok, term(), #repl_state{}} | {error, term(), #repl_state{}}.
eval(Input, State) ->
    case is_command(Input) of
        true ->
            handle_command(Input, State);
        false ->
            eval_expression(Input, State)
    end.

%% @doc Get the current type environment from state.
-spec get_env(#repl_state{}) -> map().
get_env(#repl_state{env = Env}) ->
    Env.

%%====================================================================
%% Main Loop
%%====================================================================

loop(State) ->
    Prompt = case State#repl_state.continuation of
        "" -> State#repl_state.prompt;
        _ -> ?CONT_PROMPT
    end,
    case io:get_line(Prompt) of
        eof ->
            io:format("~n"),
            stop();
        {error, Reason} ->
            io:format("Error reading input: ~p~n", [Reason]),
            loop(State);
        Line ->
            Input = State#repl_state.continuation ++ string:trim(Line, trailing, "\n"),
            case is_complete(Input) of
                true ->
                    NewState = State#repl_state{continuation = ""},
                    case Input of
                        "" ->
                            loop(NewState);
                        _ ->
                            case eval(Input, NewState) of
                                {ok, Result, State2} ->
                                    print_result(Result),
                                    State3 = add_history(Input, State2),
                                    loop(State3);
                                {error, Error, State2} ->
                                    print_error(Error),
                                    State3 = add_history(Input, State2),
                                    loop(State3);
                                quit ->
                                    stop()
                            end
                    end;
                false ->
                    %% Incomplete input - continue reading
                    loop(State#repl_state{continuation = Input ++ "\n"})
            end
    end.

%%====================================================================
%% Input Parsing
%%====================================================================

%% @doc Check if input is a REPL command (starts with :).
-spec is_command(string()) -> boolean().
is_command(":" ++ _) -> true;
is_command(_) -> false.

%% @doc Check if input is syntactically complete.
%% Returns false if input has unclosed brackets/braces or ends with operators.
-spec is_complete(string()) -> boolean().
is_complete("") -> true;
is_complete(Input) ->
    %% Simple heuristic: check balanced delimiters
    Trimmed = string:trim(Input),
    case Trimmed of
        "" -> true;
        _ ->
            BalancedParens = count_char(Trimmed, $() =:= count_char(Trimmed, $)),
            BalancedBraces = count_char(Trimmed, ${) =:= count_char(Trimmed, $}),
            BalancedBrackets = count_char(Trimmed, $[) =:= count_char(Trimmed, $]),
            %% Check if ends with continuation indicator
            NotContinued = not lists:member(lists:last(Trimmed), [$\\, $,]),
            BalancedParens andalso BalancedBraces andalso BalancedBrackets andalso NotContinued
    end.

%% @doc Parse input into tokens.
-spec parse_input(string()) -> {ok, [term()]} | {error, term()}.
parse_input(Input) ->
    catena_lexer:string(Input ++ "\n").

count_char(String, Char) ->
    length([C || C <- String, C =:= Char]).

%%====================================================================
%% Command Handling
%%====================================================================

handle_command(Input, State) ->
    %% Strip leading : and split into command and args
    ":" ++ Rest = Input,
    {Cmd, Args} = split_command(Rest),
    case string:lowercase(Cmd) of
        "quit" -> quit;
        "q" -> quit;
        "help" -> {ok, help_text(), State};
        "h" -> {ok, help_text(), State};
        "type" -> cmd_type(Args, State);
        "t" -> cmd_type(Args, State);
        "load" -> cmd_load(Args, State);
        "l" -> cmd_load(Args, State);
        "browse" -> cmd_browse(Args, State);
        "b" -> cmd_browse(Args, State);
        "env" -> cmd_env(State);
        "clear" -> cmd_clear(State);
        _ -> {error, {unknown_command, Cmd}, State}
    end.

split_command(Input) ->
    Trimmed = string:trim(Input),
    case string:split(Trimmed, " ", leading) of
        [Cmd] -> {Cmd, ""};
        [Cmd, Args] -> {Cmd, string:trim(Args)}
    end.

%% :type - show type without evaluation
cmd_type("", State) ->
    {error, {missing_argument, ":type requires an expression"}, State};
cmd_type(Expr, State) ->
    case compile_expr(Expr, State) of
        {ok, _AST, Type} ->
            TypeStr = catena_type_pp:pp_type(Type),
            {ok, {type, TypeStr}, State};
        {error, Reason} ->
            {error, Reason, State}
    end.

%% :load - load a file
cmd_load("", State) ->
    {error, {missing_argument, ":load requires a filename"}, State};
cmd_load(Filename, State) ->
    case catena_compile:compile_file(Filename) of
        {ok, {typed_module, _Name, Decls, Env}} ->
            %% Merge environment
            NewEnv = merge_env(State#repl_state.env, Env),
            %% Extract bindings from declarations
            NewBindings = extract_bindings(Decls, State#repl_state.bindings),
            NewState = State#repl_state{env = NewEnv, bindings = NewBindings},
            {ok, {loaded, Filename, length(Decls)}, NewState};
        {error, Reason} ->
            {error, {load_error, Filename, Reason}, State}
    end.

%% :browse - show bindings
cmd_browse("", State) ->
    %% Show all current bindings
    Bindings = maps:to_list(State#repl_state.bindings),
    {ok, {bindings, Bindings}, State};
cmd_browse(_Module, State) ->
    %% TODO: Browse specific module
    {error, {not_implemented, "module browsing"}, State}.

%% :env - show type environment (debug)
cmd_env(State) ->
    {ok, {env, State#repl_state.env}, State}.

%% :clear - clear bindings
cmd_clear(State) ->
    NewState = State#repl_state{bindings = #{}, env = init_env()},
    {ok, cleared, NewState}.

help_text() ->
    {help, [
        {":type <expr>", "Show the type of an expression"},
        {":load <file>", "Load a Catena source file"},
        {":browse", "Show all current bindings"},
        {":clear", "Clear all bindings"},
        {":quit", "Exit the REPL"},
        {":help", "Show this help message"}
    ]}.

%%====================================================================
%% Expression Evaluation
%%====================================================================

eval_expression(Input, State) ->
    case compile_input(Input, State) of
        {ok, {expr, AST, Type}} ->
            %% For now, return typed AST (no runtime evaluation yet)
            {ok, {value, AST, Type}, State};
        {ok, {decl, Name, AST, Type}} ->
            %% Add definition to bindings
            NewBindings = maps:put(Name, {AST, Type}, State#repl_state.bindings),
            %% Add to type environment
            Scheme = catena_type_scheme:generalize(Type, sets:new()),
            NewEnv = catena_type_env:extend(State#repl_state.env, Name, Scheme),
            NewState = State#repl_state{bindings = NewBindings, env = NewEnv},
            {ok, {defined, Name, Type}, NewState};
        {error, Reason} ->
            {error, Reason, State}
    end.

%% Compile input - determine if it's an expression or declaration
compile_input(Input, State) ->
    %% Try to parse as a module (handles transform declarations)
    Source = Input ++ "\n",
    case catena_lexer:string(Source) of
        {ok, Tokens, _} ->
            case catena_parser:parse(Tokens) of
                {ok, {module, _, _, _, Decls, _}} ->
                    compile_decls(Decls, State);
                {error, _ParseError} ->
                    %% Try wrapping as expression
                    compile_as_expr(Input, State)
            end;
        {error, LexError, _} ->
            {error, {lex_error, LexError}}
    end.

compile_decls([], _State) ->
    {error, empty_input};
compile_decls([{transform_decl, Name, _TypeSig, Clauses, _Loc}], State) ->
    %% Single transform - compile it
    case Clauses of
        [{transform_clause, _Patterns, _Guards, Body, _CLoc}] ->
            case compile_expr_ast(Body, State) of
                {ok, _AST, Type} ->
                    {ok, {decl, Name, Body, Type}};
                Error ->
                    Error
            end;
        _ ->
            {error, {unsupported, "multiple clauses"}}
    end;
compile_decls([{type_decl, Name, _Params, _Constructors, _Derives, _Loc}], _State) ->
    %% Type declaration - just acknowledge it for now
    {ok, {decl, Name, {type_decl, Name}, {tcon, Name}}};
compile_decls(_Decls, _State) ->
    {error, {unsupported, "multiple declarations"}}.

compile_as_expr(Input, State) ->
    %% Wrap input in a dummy transform to parse as expression
    Wrapped = "transform replexpr = " ++ Input ++ "\n",
    case catena_lexer:string(Wrapped) of
        {ok, Tokens, _} ->
            case catena_parser:parse(Tokens) of
                {ok, {module, _, _, _, [{transform_decl, _, _, [{transform_clause, [], _, Body, _}], _}], _}} ->
                    case compile_expr_ast(Body, State) of
                        {ok, AST, Type} ->
                            {ok, {expr, AST, Type}};
                        Error ->
                            Error
                    end;
                {error, ParseError} ->
                    {error, {parse_error, ParseError}}
            end;
        {error, LexError, _} ->
            {error, {lex_error, LexError}}
    end.

compile_expr(Input, State) ->
    case compile_as_expr(Input, State) of
        {ok, {expr, AST, Type}} -> {ok, AST, Type};
        Error -> Error
    end.

compile_expr_ast(AST, State) ->
    %% Convert parser AST to inference AST and type check
    InferAST = convert_to_infer_ast(AST),
    case catena_infer:infer_expr(InferAST, State#repl_state.env) of
        {ok, Type} ->
            {ok, AST, Type};
        {error, Errors} ->
            {error, {type_error, Errors}}
    end.

%% Convert parser AST to inference AST format
convert_to_infer_ast({var, Name, _Loc}) ->
    {var, Name};
convert_to_infer_ast({literal, Value, integer, _Loc}) ->
    {lit, {int, Value}};
convert_to_infer_ast({literal, Value, float, _Loc}) ->
    {lit, {float, Value}};
convert_to_infer_ast({literal, Value, string, _Loc}) ->
    {lit, {string, Value}};
convert_to_infer_ast({literal, Value, boolean, _Loc}) ->
    {lit, {bool, Value}};
convert_to_infer_ast({literal, Value, atom, _Loc}) ->
    {lit, {atom, Value}};
convert_to_infer_ast({literal, {int, Value}, _Loc}) ->
    {lit, {int, Value}};
convert_to_infer_ast({literal, {float, Value}, _Loc}) ->
    {lit, {float, Value}};
convert_to_infer_ast({literal, {string, Value}, _Loc}) ->
    {lit, {string, Value}};
convert_to_infer_ast({literal, {bool, Value}, _Loc}) ->
    {lit, {bool, Value}};
convert_to_infer_ast({lambda, Params, Body, _Loc}) ->
    convert_lambda(Params, Body);
convert_to_infer_ast({app, Func, Args, _Loc}) ->
    convert_app(Func, Args);
convert_to_infer_ast({let_expr, Bindings, Body, _Loc}) ->
    convert_let(Bindings, Body);
convert_to_infer_ast({if_expr, Cond, Then, Else, _Loc}) ->
    {'if', convert_to_infer_ast(Cond),
           convert_to_infer_ast(Then),
           convert_to_infer_ast(Else)};
convert_to_infer_ast({tuple_expr, Elems, _Loc}) ->
    {tuple, [convert_to_infer_ast(E) || E <- Elems]};
convert_to_infer_ast({list_expr, Elems, _Loc}) ->
    convert_list(Elems);
convert_to_infer_ast(Other) ->
    %% Fallback - return as-is for unsupported nodes
    Other.

convert_lambda([], Body) ->
    convert_to_infer_ast(Body);
convert_lambda([{pat_var, Name, _Loc} | Rest], Body) ->
    {lam, Name, convert_lambda(Rest, Body)};
convert_lambda([_ | Rest], Body) ->
    %% Complex pattern - use fresh name
    FreshName = list_to_atom("_p" ++ integer_to_list(erlang:unique_integer([positive]))),
    {lam, FreshName, convert_lambda(Rest, Body)}.

convert_app(Func, []) ->
    convert_to_infer_ast(Func);
convert_app(Func, [Arg]) ->
    {app, convert_to_infer_ast(Func), convert_to_infer_ast(Arg)};
convert_app(Func, [Arg | Rest]) ->
    convert_app({app, convert_to_infer_ast(Func), convert_to_infer_ast(Arg)}, Rest).

convert_let([{pat_var, Name, _}, Value], Body) ->
    {'let', Name, convert_to_infer_ast(Value), convert_to_infer_ast(Body)};
convert_let([_Pattern, Value], Body) ->
    FreshName = list_to_atom("_l" ++ integer_to_list(erlang:unique_integer([positive]))),
    {'let', FreshName, convert_to_infer_ast(Value), convert_to_infer_ast(Body)};
convert_let(_, Body) ->
    convert_to_infer_ast(Body).

convert_list([]) ->
    {var, '[]'};
convert_list([H | T]) ->
    {app, {app, {var, '::'}, convert_to_infer_ast(H)}, convert_list(T)}.

%%====================================================================
%% Environment Management
%%====================================================================

init_env() ->
    %% Start with built-in types and operators
    Env0 = catena_type_env:empty(),
    %% Add built-in operators from catena_compile
    {ok, Env} = catena_compile:build_type_env([]),
    Env.

merge_env(Env1, Env2) ->
    %% Simple merge - Env2 takes precedence
    %% TODO: proper environment merging with conflict detection
    maps:merge(Env1, Env2).

extract_bindings([], Bindings) ->
    Bindings;
extract_bindings([{typed_transform, Name, Type, _Clauses, _Loc} | Rest], Bindings) ->
    extract_bindings(Rest, maps:put(Name, {typed, Type}, Bindings));
extract_bindings([_ | Rest], Bindings) ->
    extract_bindings(Rest, Bindings).

add_history(Input, State) ->
    History = [Input | State#repl_state.history],
    State#repl_state{history = History}.

%%====================================================================
%% Output
%%====================================================================

print_banner() ->
    io:format("~n"),
    io:format("  Catena REPL v~s~n", [?VERSION]),
    io:format("  Type :help for available commands~n"),
    io:format("~n").

print_result({value, _AST, Type}) ->
    TypeStr = catena_type_pp:pp_type(Type),
    io:format("~s~s~s~n", [color(green), TypeStr, color(reset)]);
print_result({defined, Name, Type}) ->
    TypeStr = catena_type_pp:pp_type(Type),
    io:format("~s~p~s : ~s~s~s~n",
              [color(cyan), Name, color(reset),
               color(blue), TypeStr, color(reset)]);
print_result({type, TypeStr}) ->
    io:format("~s~s~s~n", [color(blue), TypeStr, color(reset)]);
print_result({loaded, Filename, Count}) ->
    io:format("Loaded ~s (~p definitions)~n", [Filename, Count]);
print_result({bindings, []}) ->
    io:format("No bindings defined~n");
print_result({bindings, Bindings}) ->
    io:format("Current bindings:~n"),
    lists:foreach(fun({Name, {_AST, Type}}) ->
        TypeStr = catena_type_pp:pp_type(Type),
        io:format("  ~s~p~s : ~s~n", [color(cyan), Name, color(reset), TypeStr]);
    ({Name, {typed, Type}}) ->
        TypeStr = catena_type_pp:pp_type(Type),
        io:format("  ~s~p~s : ~s~n", [color(cyan), Name, color(reset), TypeStr])
    end, Bindings);
print_result({env, Env}) ->
    io:format("Type environment: ~p~n", [Env]);
print_result(cleared) ->
    io:format("Environment cleared~n");
print_result({help, Commands}) ->
    io:format("~nAvailable commands:~n"),
    lists:foreach(fun({Cmd, Desc}) ->
        io:format("  ~s~-15s~s ~s~n", [color(yellow), Cmd, color(reset), Desc])
    end, Commands),
    io:format("~n").

print_error({unknown_command, Cmd}) ->
    io:format("~sUnknown command: ~s~s~n", [color(red), Cmd, color(reset)]),
    io:format("Type :help for available commands~n");
print_error({missing_argument, Msg}) ->
    io:format("~s~s~s~n", [color(red), Msg, color(reset)]);
print_error({parse_error, {Line, _Mod, Msg}}) ->
    io:format("~sParse error at line ~p: ~s~s~n",
              [color(red), Line, lists:flatten(Msg), color(reset)]);
print_error({lex_error, Error}) ->
    io:format("~sLexer error: ~p~s~n", [color(red), Error, color(reset)]);
print_error({type_error, Errors}) ->
    io:format("~sType error: ~p~s~n", [color(red), Errors, color(reset)]);
print_error({load_error, Filename, Reason}) ->
    io:format("~sFailed to load ~s: ~p~s~n",
              [color(red), Filename, Reason, color(reset)]);
print_error({not_implemented, Feature}) ->
    io:format("~sNot implemented: ~s~s~n", [color(yellow), Feature, color(reset)]);
print_error(Error) ->
    io:format("~sError: ~p~s~n", [color(red), Error, color(reset)]).

%% ANSI color codes
color(reset) -> "\e[0m";
color(red) -> "\e[31m";
color(green) -> "\e[32m";
color(yellow) -> "\e[33m";
color(blue) -> "\e[34m";
color(cyan) -> "\e[36m".
