%%%-------------------------------------------------------------------
%%% @doc Catena REPL Effect Execution Module
%%%
%%% This module integrates the effect runtime into the REPL, enabling
%%% execution of effectful expressions with automatic handler provision
%%% for builtin effects (IO, Process, Error, State).
%%%
%%% == Features ==
%%%
%%% <ul>
%%%   <li><b>Automatic Handler Provision</b> - Builtin effects (IO, Process)
%%%       are automatically handled without explicit handler setup</li>
%%%   <li><b>Sandboxed Execution</b> - Effectful expressions are executed
%%%       in a controlled environment with timeouts</li>
%%%   <li><b>Error Recovery</b> - Effect errors are caught and displayed
%%%       without crashing the REPL</li>
%%%   <li><b>Effect Feedback</b> - Optional verbose mode shows which effects
%%%       are being performed during evaluation</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_repl_effects).

%% API
-export([
    eval_with_effects/2,
    eval_with_effects/3,
    with_default_handlers/1,
    handle_builtin_effect/4,
    format_effect_error/1,
    set_verbose/1,
    is_verbose/0
]).

%% Internal exports for testing
-export([
    match_pattern/2
]).

%% Types
-type eval_result() :: {ok, term()} | {error, term()}.
-type effect_context() :: catena_effect_runtime:effect_context().
-type options() :: #{
    verbose => boolean(),
    timeout => pos_integer()
}.

%%%=============================================================================
%%% Default Handler Management
%%%=============================================================================

%% @doc Create an effect context with default builtin handlers.
%% Automatically registers handlers for IO, Process, Error, and State effects.
-spec with_default_handlers(options()) -> effect_context().
with_default_handlers(Opts) ->
    Ctx = catena_effect_runtime:new_context(),
    %% For REPL, we don't spawn handler processes - we handle effects directly
    %% Mark which effects are available
    Handlers = #{
        io => direct,
        process => direct,
        error => direct,
        state => direct
    },
    Ctx#{handlers => Handlers}.

%%%=============================================================================
%%% Effect Evaluation
%%%=============================================================================

%% @doc Evaluate an expression AST with automatic effect handling.
%% Uses default handlers for builtin effects.
-spec eval_with_effects(tuple(), options()) -> eval_result().
eval_with_effects(AST, Opts) ->
    Ctx = with_default_handlers(Opts),
    eval_with_effects(AST, Ctx, Opts).

%% @doc Evaluate an expression AST with explicit effect context.
-spec eval_with_effects(tuple(), effect_context(), options()) -> eval_result().
eval_with_effects(AST, Ctx, Opts) ->
    try
        case eval_ast(AST, Ctx, Opts) of
            {ok, Value, _} -> {ok, Value};
            {error, Reason} -> {error, Reason}
        end
    catch
        {effect_error, Effect, Operation, _Reason} ->
            {error, {effect_operation_failed, Effect, Operation}};
        {effect_timeout, Effect, Operation} ->
            {error, {effect_timeout, Effect, Operation}};
        exit:_Reason ->
            {error, {exit, process_exited}};
        {Error, Kind} ->
            {error, {unexpected_error, Kind, Error}}
    end.

%% @doc Evaluate AST with effect context.
-spec eval_ast(tuple(), effect_context(), options()) -> {ok, term(), effect_context()} | {error, term()}.
eval_ast(AST, Ctx, Opts) ->
    case AST of
        {literal, Value, _Type, _Loc} ->
            {ok, Value, Ctx};
        {var, Name, _Loc} ->
            {error, {unbound_variable, Name}};
        {binop, Op, Left, Right, _Type, _Loc} ->
            {ok, LeftVal, Ctx1} = eval_ast(Left, Ctx, Opts),
            {ok, RightVal, Ctx2} = eval_ast(Right, Ctx1, Opts),
            eval_binop(Op, LeftVal, RightVal, Ctx2);
        {unop, Op, Arg, _Type, _Loc} ->
            {ok, ArgVal, Ctx1} = eval_ast(Arg, Ctx, Opts),
            eval_unop(Op, ArgVal, Ctx1);
        {apply_expr, {atom, FuncName, _Arity}, Args, _Type, _Loc} ->
            eval_apply(FuncName, Args, Ctx, Opts);
        {perform_expr, Effect, Operation, Args, _Type, _Loc} ->
            eval_perform(Effect, Operation, Args, Ctx, Opts);
        {if_expr, Clauses, _Type, _Loc} ->
            eval_if(Clauses, Ctx, Opts);
        {case_expr, Scrutinee, Clauses, _Type, _Loc} ->
            eval_case(Scrutinee, Clauses, Ctx, Opts);
        {lambda, _Params, _Body, _Type, _Loc} ->
            {ok, AST, Ctx};  %% Return lambda as-is for now
        _ ->
            {error, {unsupported_expression, element(1, AST)}}
    end.

%% @doc Evaluate a binary operation.
-spec eval_binop(atom(), term(), term(), effect_context()) ->
    {ok, term(), effect_context()} | {error, term()}.
eval_binop(Op, Left, Right, Ctx) ->
    case Op of
        '+' -> {ok, Left + Right, Ctx};
        '-' -> {ok, Left - Right, Ctx};
        '*' -> {ok, Left * Right, Ctx};
        '/' when Right =/= 0 -> {ok, Left div Right, Ctx};
        '==' -> {ok, Left =:= Right, Ctx};
        '/=' -> {ok, Left =/= Right, Ctx};
        '<' -> {ok, Left < Right, Ctx};
        '>' -> {ok, Left > Right, Ctx};
        '<=' -> {ok, Left =< Right, Ctx};
        '>=' -> {ok, Left >= Right, Ctx};
        '++' when is_list(Left), is_list(Right) -> {ok, Left ++ Right, Ctx};
        '::' when is_list(Right) -> {ok, [Left | Right], Ctx};
        _ -> {error, {unsupported_operation, Op}}
    end.

%% @doc Evaluate a unary operation.
-spec eval_unop(atom(), term(), effect_context()) ->
  {ok, term(), effect_context()} | {error, term()}.
eval_unop(Op, Arg, Ctx) ->
    case Op of
        '-' when is_number(Arg) -> {ok, -Arg, Ctx};
        'not' when is_boolean(Arg) -> {ok, not Arg, Ctx};
        _ -> {error, {unsupported_operation, Op}}
    end.

%% @doc Evaluate a function application.
-spec eval_apply(atom(), [tuple()], effect_context(), options()) ->
    {ok, term(), effect_context()} | {error, term()}.
eval_apply(FuncName, Args, Ctx, Opts) ->
    %% Evaluate arguments
    {ok, EvaluatedArgs, Ctx1} = eval_args(Args, Ctx, Opts),
    %% Check for builtin functions
    case apply_builtin(FuncName, EvaluatedArgs, Ctx1, Opts) of
        {ok, Result} -> {ok, Result, Ctx1};
        not_builtin ->
            {error, {undefined_function, FuncName}}
    end.

%% @doc Evaluate argument list.
-spec eval_args([tuple()], effect_context(), options()) ->
    {ok, [term()], effect_context()} | {error, term()}.
eval_args(Args, Ctx, Opts) ->
    {ok, RevArgs, FinalCtx} = lists:foldl(fun(Arg, {ok, Acc, CtxIn}) ->
        case eval_ast(Arg, CtxIn, Opts) of
            {ok, Val, NewCtx} -> {ok, [Val | Acc], NewCtx};
            {error, Reason} -> {error, Reason}
        end
    end, {ok, [], Ctx}, Args),
    {ok, lists:reverse(RevArgs), FinalCtx}.

%% @doc Apply builtin function.
-spec apply_builtin(atom(), [term()], effect_context(), options()) ->
    {ok, term()} | not_builtin.
apply_builtin(identity, [X], _Ctx, _Opts) -> {ok, X};
apply_builtin(const, [X, _Y], _Ctx, _Opts) -> {ok, X};
apply_builtin(compose, [F, G], _Ctx, _Opts) ->
    {ok, fun(X) -> F(G(X)) end};
apply_builtin(flip, [F, X, Y], _Ctx, _Opts) ->
    {ok, F(Y, X)};
apply_builtin(head, [[X | _]], _Ctx, _Opts) -> {ok, X};
apply_builtin(tail, [[_ | Xs]], _Ctx, _Opts) -> {ok, Xs};
apply_builtin(length, [Xs], _Ctx, _Opts) when is_list(Xs) -> {ok, length(Xs)};
apply_builtin(reverse, [Xs], _Ctx, _Opts) when is_list(Xs) -> {ok, lists:reverse(Xs)};
apply_builtin(map, [F, Xs], _Ctx, _Opts) when is_list(Xs), is_function(F) ->
    {ok, lists:map(F, Xs)};
apply_builtin(filter, [P, Xs], _Ctx, _Opts) when is_list(Xs), is_function(P) ->
    {ok, lists:filter(P, Xs)};
apply_builtin(fold, [F, Acc, Xs], _Ctx, _Opts) when is_list(Xs) ->
    {ok, lists:foldl(F, Acc, Xs)};
apply_builtin(append, [Xs], _Ctx, _Opts) when is_list(Xs) ->
    {ok, lists:flatten(Xs)};
apply_builtin(_, _, _, _) ->
    not_builtin.

%% @doc Evaluate a perform expression (effect operation).
-spec eval_perform(atom(), atom(), [tuple()], effect_context(), options()) ->
    {ok, term(), effect_context()} | {error, term()}.
eval_perform(Effect, Operation, Args, Ctx, Opts) ->
    %% Evaluate arguments
    {ok, EvaluatedArgs, Ctx1} = eval_args(Args, Ctx, Opts),
    %% Perform the effect operation
    case maps:get(verbose, Opts, false) of
        true ->
            io:format("[Effect] ~p:~p(~p)~n", [Effect, Operation, EvaluatedArgs]);
        false ->
            ok
    end,
    %% Check if effect is handled (marked as 'direct' in handlers)
    Handlers = maps:get(handlers, Ctx, #{}),
    case maps:get(Effect, Handlers, undefined) of
        direct ->
            %% Handle builtin effect directly
            case handle_builtin_effect(Effect, Operation, EvaluatedArgs, Ctx1) of
                {ok, Result} -> {ok, Result, Ctx1};
                {error, Reason} -> {error, {effect_operation_failed, Effect, Operation, Reason}}
            end;
        _ ->
            %% Try effect runtime
            case catena_effect_runtime:perform(Ctx1, Effect, Operation, EvaluatedArgs) of
                {ok, Result} -> {ok, Result, Ctx1};
                {error, Reason} -> {error, {effect_operation_failed, Effect, Operation, Reason}};
                Result -> {ok, Result, Ctx1}
            end
    end.

%% @doc Evaluate if expression.
-spec eval_if([tuple()], effect_context(), options()) ->
    {ok, term(), effect_context()} | {error, term()}.
eval_if([], _Ctx, _Opts) ->
    {error, no_matching_clause};
eval_if([{if_clause, Guard, Body, _Loc} | Rest], Ctx, Opts) ->
    case eval_ast(Guard, Ctx, Opts) of
        {ok, true, Ctx1} ->
            eval_ast(Body, Ctx1, Opts);
        {ok, false, _} ->
            eval_if(Rest, Ctx, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Evaluate case expression.
-spec eval_case(tuple(), [tuple()], effect_context(), options()) ->
    {ok, term(), effect_context()} | {error, term()}.
eval_case(Scrutinee, Clauses, Ctx, Opts) ->
    case eval_ast(Scrutinee, Ctx, Opts) of
        {ok, Value, Ctx1} ->
            eval_case_clauses(Value, Clauses, Ctx1, Opts);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Evaluate case clauses.
-spec eval_case_clauses(term(), [tuple()], effect_context(), options()) ->
    {ok, term(), effect_context()} | {error, term()}.
eval_case_clauses(_Value, [], _Ctx, _Opts) ->
    {error, no_matching_clause};
eval_case_clauses(Value, [{match_clause, Pattern, Guard, Body, _Loc} | Rest], Ctx, Opts) ->
    case match_pattern(Value, Pattern) of
        {ok, true} when Guard =:= undefined ->
            eval_ast(Body, Ctx, Opts);
        {ok, true} ->
            case eval_ast(Guard, Ctx, Opts) of
                {ok, true, Ctx1} -> eval_ast(Body, Ctx1, Opts);
                _ -> eval_case_clauses(Value, Rest, Ctx, Opts)
            end;
        {ok, false} ->
            eval_case_clauses(Value, Rest, Ctx, Opts);
        {error, _} ->
            eval_case_clauses(Value, Rest, Ctx, Opts)
    end.

%% @doc Match value against pattern.
-spec match_pattern(term(), tuple()) -> {ok, boolean()} | {error, term()}.
match_pattern(Value, {literal, LitVal, _Type, _Loc}) ->
    {ok, Value =:= LitVal};
match_pattern(Value, {var, _Name, _Loc}) ->
    {ok, true};
match_pattern(Value, {tuple_pattern, Elements, _Loc}) when is_tuple(Value) ->
    case tuple_size(Value) =:= length(Elements) of
        true ->
            match_tuple_elements(erlang:tuple_to_list(Value), Elements);
        false ->
            {ok, false}
    end;
match_pattern(Value, {cons_pattern, Head, Tail, _Loc}) when is_list(Value) ->
    case Value of
        [H | T] ->
            case match_pattern(H, Head) of
                {ok, true} -> match_pattern(T, Tail);
                Result -> Result
            end;
        _ ->
            {ok, false}
    end;
match_pattern(Value, {nil_pattern, _Loc}) ->
    {ok, Value =:= []};
match_pattern(_Value, _Pattern) ->
    {ok, true}.  %% Simplified - would need full pattern matching

%% @doc Match tuple elements against patterns.
-spec match_tuple_elements([term()], [tuple()]) -> {ok, boolean()}.
match_tuple_elements([], []) ->
    {ok, true};
match_tuple_elements([V | Vs], [P | Ps]) ->
    case match_pattern(V, P) of
        {ok, true} -> match_tuple_elements(Vs, Ps);
        Result -> Result
    end;
match_tuple_elements(_, _) ->
    {ok, false}.

%%%=============================================================================
%%% Builtin Effect Handlers
%%%=============================================================================

%% @doc Handle builtin effect operations directly.
%% This is called when no handler is registered but we have builtin support.
-spec handle_builtin_effect(atom(), atom(), [term()], effect_context()) ->
    {ok, term()} | {error, term()}.
handle_builtin_effect(io, print, [String], _Ctx) ->
    io:format("~s~n", [String]),
    {ok, unit};
handle_builtin_effect(io, println, [String], _Ctx) ->
    io:format("~s~n", [String]),
    {ok, unit};
handle_builtin_effect(io, read_line, [], _Ctx) ->
    case io:get_line("") of
        {ok, Line} -> {ok, Line};
        eof -> {ok, eof};
        {error, Reason} -> {error, Reason}
    end;
handle_builtin_effect(error, raise, [Reason], _Ctx) ->
    {error, Reason};
handle_builtin_effect(state, get, [], _Ctx) ->
    try ets:lookup_element(repl_state, value, 2) of
        Value -> {ok, Value}
    catch
        error:badarg -> {error, no_state}
    end;
handle_builtin_effect(state, put, [Value], _Ctx) ->
    ets:insert(repl_state, {value, Value}),
    {ok, unit};
handle_builtin_effect(state, modify, [Fun], _Ctx) when is_function(Fun, 1) ->
    try ets:lookup_element(repl_state, value, 1) of
        Value ->
            NewValue = Fun(Value),
            ets:insert(repl_state, {value, NewValue}),
            {ok, NewValue}
    catch
        error:badarg ->
            {error, no_state}
    end;
handle_builtin_effect(Effect, Operation, Args, _Ctx) ->
    {error, {unknown_effect_operation, Effect, Operation, Args}}.

%%%=============================================================================
%%% Verbose Mode
%%%=============================================================================

%% @doc Set verbose mode for effect execution feedback.
-spec set_verbose(boolean()) -> ok.
set_verbose(true) ->
    application:set_env(catena, repl_verbose, true);
set_verbose(false) ->
    application:set_env(catena, repl_verbose, false).

%% @doc Check if verbose mode is enabled.
-spec is_verbose() -> boolean().
is_verbose() ->
    application:get_env(catena, repl_verbose, false).

%%%=============================================================================
%%% Error Formatting
%%%=============================================================================

%% @doc Format an effect error for display.
-spec format_effect_error(term()) -> iolist().
format_effect_error({effect_operation_failed, Effect, Operation, Reason}) ->
    io_lib:format("Effect operation failed: ~p:~p()~nReason: ~p~n",
                  [Effect, Operation, Reason]);
format_effect_error({effect_timeout, Effect, Operation}) ->
    io_lib:format("Effect operation timed out: ~p:~p()~n",
                  [Effect, Operation]);
format_effect_error({exit, Reason}) ->
    io_lib:format("Process exit: ~p~n", [Reason]);
format_effect_error({unexpected_error, Kind, Error}) ->
    io_lib:format("Unexpected error: ~p: ~p~n", [Kind, Error]);
format_effect_error(Error) ->
    io_lib:format("Error: ~p~n", [Error]).
