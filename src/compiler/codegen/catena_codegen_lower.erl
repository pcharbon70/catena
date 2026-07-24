%%%-------------------------------------------------------------------
%%% @doc Canonical frontend AST lowering for Core Erlang generation.
%%%
%%% The parser and semantic passes use declaration and expression shapes that
%%% predate the original code-generation proof of concept. This module is the
%%% explicit boundary between those canonical frontend shapes and the smaller
%%% backend AST consumed by `catena_codegen_*`.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_lower).

-export([
    lower_module/1,
    lower_decl/1,
    lower_expr/1,
    lower_pattern/1,
    lower_operator/1
]).

%% @doc Lower a parser/semantic module into the backend module AST.
-spec lower_module(term()) -> term().
lower_module({module, Name, Exports, Imports, Declarations, Location}) ->
    {module,
        Name,
        Exports,
        Imports,
        [lower_decl(Declaration) || Declaration <- Declarations],
        Location};
lower_module(Other) ->
    Other.

%% @doc Lower a declaration while preserving declarations erased by codegen.
-spec lower_decl(term()) -> term().
lower_decl({transform_decl, _Name, _Type, [], _Location}) ->
    %% Signature-only declarations do not have a runtime definition.
    erased;
lower_decl({transform_decl, Name, _Type, Clauses, Location}) ->
    lower_transform(Name, Clauses, Location);
lower_decl({transform, Name, Params, Body, Location}) ->
    {transform,
        Name,
        [lower_pattern(Param) || Param <- Params],
        lower_expr(Body),
        Location};
lower_decl({transform_typed, Name, Type, Params, Body, Location}) ->
    {transform_typed,
        Name,
        Type,
        [lower_pattern(Param) || Param <- Params],
        lower_expr(Body),
        Location};
lower_decl(Other) ->
    Other.

lower_transform(Name, Clauses, Location) ->
    Arity = clause_arity(Name, Clauses),
    ParamNames = [backend_param_name(Index) || Index <- lists:seq(1, Arity)],
    Params = [{pat_var, ParamName, Location} || ParamName <- ParamNames],
    Body = lower_transform_body(Clauses, ParamNames, Location),
    {transform, Name, Params, Body, Location}.

clause_arity(Name, [{transform_clause, Patterns, _Guards, _Body, _Location} | Rest]) ->
    Arity = length(Patterns),
    case lists:all(
        fun
            ({transform_clause, OtherPatterns, _, _, _}) ->
                length(OtherPatterns) =:= Arity;
            (_) ->
                false
        end,
        Rest
    ) of
        true ->
            Arity;
        false ->
            error({inconsistent_transform_arity, Name})
    end.

backend_param_name(Index) ->
    list_to_atom("$catena_arg_" ++ integer_to_list(Index)).

lower_transform_body(
    [{transform_clause, [], undefined, Body, _ClauseLocation}],
    [],
    _Location
) ->
    lower_expr(Body);
lower_transform_body(Clauses, ParamNames, Location) ->
    Scrutinee = transform_scrutinee(ParamNames, Location),
    MatchClauses = [
        lower_transform_clause(Clause, length(ParamNames), Location)
        || Clause <- Clauses
    ],
    {match_expr, Scrutinee, MatchClauses, Location}.

transform_scrutinee([], Location) ->
    {literal, bool, true, Location};
transform_scrutinee([Name], Location) ->
    {var, Name, Location};
transform_scrutinee(Names, Location) ->
    {tuple_expr, [{var, Name, Location} || Name <- Names], Location}.

lower_transform_clause(
    {transform_clause, Patterns, Guards, Body, ClauseLocation},
    Arity,
    _TransformLocation
) ->
    LoweredPatterns = [lower_pattern(Pattern) || Pattern <- Patterns],
    MatchPattern = case Arity of
        0 ->
            {pat_literal, true, bool, ClauseLocation};
        1 ->
            hd(LoweredPatterns);
        _ ->
            {pat_tuple, LoweredPatterns, ClauseLocation}
    end,
    {clause,
        [MatchPattern],
        lower_guards(Guards),
        lower_expr(Body)}.

lower_guards(undefined) ->
    [];
lower_guards(Guards) when is_list(Guards) ->
    [lower_expr(Guard) || Guard <- Guards];
lower_guards(Guard) ->
    [lower_expr(Guard)].

%% @doc Lower parser expressions and retain already-lowered backend forms.
-spec lower_expr(term()) -> term().
lower_expr({literal, Type, Value, Location})
  when Type =:= integer; Type =:= float; Type =:= string;
       Type =:= atom; Type =:= char; Type =:= bool ->
    {literal, Type, Value, Location};
lower_expr({literal, Value, Type, Location})
  when Type =:= integer; Type =:= float; Type =:= string;
       Type =:= atom; Type =:= char; Type =:= bool ->
    {literal, Type, Value, Location};
lower_expr({var, true, Location}) ->
    {literal, bool, true, Location};
lower_expr({var, false, Location}) ->
    {literal, bool, false, Location};
lower_expr({var, Name, Location}) ->
    case is_constructor_name(Name) of
        true -> {constructor, Name, [], Location};
        false -> {var, Name, Location}
    end;
lower_expr({app, Function, Arguments, Location}) ->
    LoweredFunction = lower_expr(Function),
    LoweredArguments = [lower_expr(Argument) || Argument <- Arguments],
    case LoweredFunction of
        {constructor, Name, ExistingArguments, ConstructorLocation} ->
            {constructor,
                Name,
                ExistingArguments ++ LoweredArguments,
                ConstructorLocation};
        _ ->
            {app, LoweredFunction, LoweredArguments, Location}
    end;
lower_expr({let_expr, [Pattern, Value], Body, Location})
  when is_tuple(Pattern) ->
    {let_expr,
        [{lower_pattern(Pattern), lower_expr(Value)}],
        lower_expr(Body),
        Location};
lower_expr({let_expr, Bindings, Body, Location}) when is_list(Bindings) ->
    {let_expr,
        [
            {lower_pattern(Pattern), lower_expr(Value)}
            || {Pattern, Value} <- Bindings
        ],
        lower_expr(Body),
        Location};
lower_expr({binary_op, Operator, Left, Right, Location}) ->
    {binary_op,
        lower_operator(Operator),
        lower_expr(Left),
        lower_expr(Right),
        Location};
lower_expr({cons_expr, Head, Tail, Location}) ->
    {binary_op, '::', lower_expr(Head), lower_expr(Tail), Location};
lower_expr({unary_op, Operator, Operand, Location}) ->
    {unary_op, lower_operator(Operator), lower_expr(Operand), Location};
lower_expr({lambda, Params, Body, Location}) ->
    {lambda,
        [lower_pattern(Param) || Param <- Params],
        lower_expr(Body),
        Location};
lower_expr({if_expr, Condition, Then, Else, Location}) ->
    {if_expr,
        lower_expr(Condition),
        lower_expr(Then),
        lower_expr(Else),
        Location};
lower_expr({list_expr, Elements, Location}) ->
    {list_expr, [lower_expr(Element) || Element <- Elements], Location};
lower_expr({tuple_expr, Elements, Location}) ->
    {tuple_expr, [lower_expr(Element) || Element <- Elements], Location};
lower_expr({record_expr, Fields, _Base, Location}) ->
    {record_expr,
        [{Field, lower_expr(Value)} || {Field, Value} <- Fields],
        Location};
lower_expr({record_expr, Fields, Location}) ->
    {record_expr,
        [{Field, lower_expr(Value)} || {Field, Value} <- Fields],
        Location};
lower_expr({record_access, Record, Field, Location}) ->
    {record_access, lower_expr(Record), Field, Location};
lower_expr({match_expr, undefined, Clauses, Location}) ->
    ParamName = backend_param_name(1),
    Param = {pat_var, ParamName, Location},
    Match = {match_expr,
        {var, ParamName, Location},
        [lower_match_clause(Clause) || Clause <- Clauses],
        Location},
    {lambda, [Param], Match, Location};
lower_expr({match_expr, Scrutinee, Clauses, Location}) ->
    {match_expr,
        lower_expr(Scrutinee),
        [lower_match_clause(Clause) || Clause <- Clauses],
        Location};
lower_expr({perform_expr, Effect, Operation, Arguments, Location}) ->
    {perform_expr,
        Effect,
        Operation,
        [lower_expr(Argument) || Argument <- Arguments],
        Location};
lower_expr({handle_expr, Body, Handlers, Location}) ->
    {handle_expr,
        lower_expr(Body),
        [lower_handler(Handler) || Handler <- Handlers],
        Location};
lower_expr({try_with_expr, Body, Handlers, Location}) ->
    {try_with_expr,
        lower_expr(Body),
        [lower_handler(Handler) || Handler <- Handlers],
        Location};
lower_expr({constructor, Name, Arguments, Location}) ->
    {constructor,
        Name,
        [lower_expr(Argument) || Argument <- Arguments],
        Location};
lower_expr(Other) ->
    Other.

lower_match_clause({match_clause, Pattern, Guards, Body, _Location}) ->
    {clause,
        [lower_pattern(Pattern)],
        lower_guards(Guards),
        lower_expr(Body)};
lower_match_clause({clause, Patterns, Guards, Body}) ->
    {clause,
        [lower_pattern(Pattern) || Pattern <- Patterns],
        lower_guards(Guards),
        lower_expr(Body)};
lower_match_clause(Other) ->
    Other.

lower_handler({handler_clause, Effect, Operations, Location}) ->
    {handler_clause,
        Effect,
        [lower_operation(Operation) || Operation <- Operations],
        Location};
lower_handler(Other) ->
    Other.

lower_operation({operation_case, Name, Params, Body, Location}) ->
    {operation_case,
        Name,
        [lower_pattern(Param) || Param <- Params],
        lower_expr(Body),
        Location};
lower_operation(Other) ->
    Other.

%% @doc Recursively normalize parser patterns for backend consumption.
-spec lower_pattern(term()) -> term().
lower_pattern({pat_var, true, Location}) ->
    {pat_literal, true, bool, Location};
lower_pattern({pat_var, false, Location}) ->
    {pat_literal, false, bool, Location};
lower_pattern({pat_constructor, Name, Arguments, Location}) ->
    {pat_constructor,
        Name,
        [lower_pattern(Argument) || Argument <- Arguments],
        Location};
lower_pattern({pat_list, Elements, Location}) ->
    {pat_list, [lower_pattern(Element) || Element <- Elements], Location};
lower_pattern({pat_cons, Head, Tail, Location}) ->
    {pat_cons, lower_pattern(Head), lower_pattern(Tail), Location};
lower_pattern({pat_tuple, Elements, Location}) ->
    {pat_tuple, [lower_pattern(Element) || Element <- Elements], Location};
lower_pattern({pat_as, Name, Pattern, Location}) ->
    {pat_as, Name, lower_pattern(Pattern), Location};
lower_pattern({pat_or, Alternatives, Location}) ->
    {pat_or,
        [lower_pattern(Alternative) || Alternative <- Alternatives],
        Location};
lower_pattern({pat_record, Fields, Location}) ->
    {pat_record,
        [{Field, lower_pattern(Pattern)} || {Field, Pattern} <- Fields],
        Location};
lower_pattern(Other) ->
    Other.

%% @doc Map parser operator names to their Core Erlang operation atoms.
-spec lower_operator(atom()) -> atom().
lower_operator(pipe_right) -> '|>';
lower_operator(plus) -> '+';
lower_operator(minus) -> '-';
lower_operator(star) -> '*';
lower_operator(slash) -> '/';
lower_operator(eq) -> '==';
lower_operator(neq) -> '/=';
lower_operator(setoid_eq) -> '===';
lower_operator(setoid_neq) -> '!==';
lower_operator(lt) -> '<';
lower_operator(gt) -> '>';
lower_operator(lte) -> '=<';
lower_operator(gte) -> '>=';
lower_operator(plus_plus) -> '++';
lower_operator(Other) -> Other.

is_constructor_name(Name) when is_atom(Name) ->
    case atom_to_list(Name) of
        [First | _] when First >= $A, First =< $Z -> true;
        _ -> false
    end;
is_constructor_name(_) ->
    false.
