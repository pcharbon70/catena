%%%-------------------------------------------------------------------
%%% @doc Expression Translation to Core Erlang (Task 1.3.1)
%%%
%%% Translates Catena AST expressions to Core Erlang expressions.
%%% This module handles:
%%% - Literals (numbers, strings, atoms)
%%% - Variables
%%% - Function applications
%%% - Let bindings
%%% - Composition operators (|>)
%%% - Binary operations
%%% - Effect operations (perform, try/with)
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_expr).

-export([
    %% Main translation
    translate_expr/2,
    translate_exprs/2,

    %% Specific translations
    translate_literal/2,
    translate_var/2,
    translate_app/2,
    translate_let/2,
    translate_binary_op/2,
    translate_lambda/2,
    translate_if/2,
    translate_list/2,
    translate_tuple/2,

    %% Effect translations
    translate_perform/2,
    translate_try_with/2
]).

-include_lib("compiler/src/core_parse.hrl").

%%====================================================================
%% Main Translation Functions
%%====================================================================

%% @doc Translate a Catena expression to Core Erlang
-spec translate_expr(term(), catena_codegen_utils:codegen_state()) ->
    {cerl:cerl(), catena_codegen_utils:codegen_state()}.

%% Literals
translate_expr({literal, Type, Value, Loc}, State) ->
    translate_literal({literal, Type, Value, Loc}, State);

%% Variables
translate_expr({var, Name, _Loc}, State) ->
    translate_var({var, Name, _Loc}, State);

%% Function application
translate_expr({app, Func, Args, Loc}, State) ->
    translate_app({app, Func, Args, Loc}, State);

%% Let binding
translate_expr({let_expr, Bindings, Body, Loc}, State) ->
    translate_let({let_expr, Bindings, Body, Loc}, State);

%% Binary operators (includes |>)
translate_expr({binary_op, Op, Left, Right, Loc}, State) ->
    translate_binary_op({binary_op, Op, Left, Right, Loc}, State);

%% Lambda expressions
translate_expr({lambda, Params, Body, Loc}, State) ->
    translate_lambda({lambda, Params, Body, Loc}, State);

%% If expressions
translate_expr({if_expr, Cond, Then, Else, Loc}, State) ->
    translate_if({if_expr, Cond, Then, Else, Loc}, State);

%% List literals
translate_expr({list_expr, Elements, Loc}, State) ->
    translate_list({list_expr, Elements, Loc}, State);

%% Tuple literals
translate_expr({tuple_expr, Elements, Loc}, State) ->
    translate_tuple({tuple_expr, Elements, Loc}, State);

%% Perform expression (effect invocation)
translate_expr({perform_expr, Effect, Operation, Args, Loc}, State) ->
    translate_perform({perform_expr, Effect, Operation, Args, Loc}, State);

%% Try/with expression (effect handling)
translate_expr({try_with_expr, Body, Handlers, Loc}, State) ->
    translate_try_with({try_with_expr, Body, Handlers, Loc}, State);

%% Unary operators
translate_expr({unary_op, Op, Operand, Loc}, State) ->
    translate_unary_op({unary_op, Op, Operand, Loc}, State);

%% Record access
translate_expr({record_access, Record, Field, Loc}, State) ->
    translate_record_access({record_access, Record, Field, Loc}, State);

%% Constructor application (ADT constructors)
translate_expr({constructor, Name, Args, Loc}, State) ->
    translate_constructor({constructor, Name, Args, Loc}, State);

%% Unknown expression type
translate_expr(Unknown, State) ->
    %% For now, return a placeholder with an error annotation
    Error = cerl:c_tuple([
        cerl:c_atom(error),
        cerl:c_atom(unknown_expression),
        cerl:c_literal(Unknown)
    ]),
    {Error, State}.

%% @doc Translate multiple expressions
-spec translate_exprs([term()], catena_codegen_utils:codegen_state()) ->
    {[cerl:cerl()], catena_codegen_utils:codegen_state()}.
translate_exprs([], State) ->
    {[], State};
translate_exprs([Expr | Rest], State) ->
    {CoreExpr, State1} = translate_expr(Expr, State),
    {CoreRest, State2} = translate_exprs(Rest, State1),
    {[CoreExpr | CoreRest], State2}.

%%====================================================================
%% Literal Translation (1.3.1.4)
%%====================================================================

%% @doc Translate literals to Core Erlang
translate_literal({literal, integer, Value, _Loc}, State) ->
    {cerl:c_int(Value), State};

translate_literal({literal, float, Value, _Loc}, State) ->
    {cerl:c_float(Value), State};

translate_literal({literal, string, Value, _Loc}, State) when is_binary(Value) ->
    {cerl:c_string(binary_to_list(Value)), State};

translate_literal({literal, string, Value, _Loc}, State) when is_list(Value) ->
    {cerl:c_string(Value), State};

translate_literal({literal, atom, Value, _Loc}, State) ->
    {cerl:c_atom(Value), State};

translate_literal({literal, char, Value, _Loc}, State) ->
    {cerl:c_int(Value), State};

translate_literal({literal, bool, true, _Loc}, State) ->
    {cerl:c_atom(true), State};

translate_literal({literal, bool, false, _Loc}, State) ->
    {cerl:c_atom(false), State}.

%%====================================================================
%% Variable Translation
%%====================================================================

%% @doc Translate variables to Core Erlang
translate_var({var, Name, _Loc}, State) ->
    {cerl:c_var(Name), State}.

%%====================================================================
%% Function Application Translation (1.3.1.1)
%%====================================================================

%% @doc Translate function application to Core Erlang
%%
%% Function calls are translated to either:
%% - cerl:c_apply for local function calls
%% - cerl:c_call for module-qualified calls
translate_app({app, Func, Args, _Loc}, State) ->
    %% Translate arguments first
    {CoreArgs, State1} = translate_exprs(Args, State),

    %% Translate the function expression
    case Func of
        %% Module-qualified call: Module.function(args)
        {module_call, Module, FuncName, _} ->
            ModAtom = cerl:c_atom(Module),
            FuncAtom = cerl:c_atom(FuncName),
            {cerl:c_call(ModAtom, FuncAtom, CoreArgs), State1};

        %% Direct function reference by name
        {var, FuncName, _} ->
            %% Local function application
            FuncVar = cerl:c_var(FuncName),
            {cerl:c_apply(FuncVar, CoreArgs), State1};

        %% Lambda or other expression as function
        _ ->
            {CoreFunc, State2} = translate_expr(Func, State1),
            {cerl:c_apply(CoreFunc, CoreArgs), State2}
    end.

%%====================================================================
%% Let Binding Translation (1.3.1.2)
%%====================================================================

%% @doc Translate let bindings to Core Erlang
%%
%% let x = expr1, y = expr2 in body
%% becomes nested Core Erlang let expressions
translate_let({let_expr, Bindings, Body, _Loc}, State) ->
    %% Translate body first (for nested scoping)
    {CoreBody, State1} = translate_expr(Body, State),

    %% Fold bindings right-to-left to create nested lets
    {FinalExpr, FinalState} = lists:foldr(
        fun({Pattern, BindExpr}, {Acc, St}) ->
            {CoreBindExpr, St1} = translate_expr(BindExpr, St),
            CoreVar = pattern_to_var(Pattern),
            Let = cerl:c_let([CoreVar], CoreBindExpr, Acc),
            {Let, St1}
        end,
        {CoreBody, State1},
        Bindings
    ),
    {FinalExpr, FinalState}.

%% Convert a pattern to a Core Erlang variable
%% For simple variable patterns; complex patterns need pattern compilation
pattern_to_var({var, Name, _Loc}) ->
    cerl:c_var(Name);
pattern_to_var({wildcard, _Loc}) ->
    cerl:c_var('_');
pattern_to_var(_Complex) ->
    %% Complex patterns need pattern matching - placeholder for now
    cerl:c_var('_').

%%====================================================================
%% Binary Operator Translation (1.3.1.3)
%%====================================================================

%% @doc Translate binary operators to Core Erlang
%%
%% The composition operator |> is translated to function application.
%% Arithmetic and comparison operators become BIF calls.
translate_binary_op({binary_op, '|>', Left, Right, _Loc}, State) ->
    %% a |> f becomes f(a)
    %% a |> f(x) becomes f(a, x)
    {CoreLeft, State1} = translate_expr(Left, State),
    case Right of
        {app, Func, Args, AppLoc} ->
            %% f(x) |> g(y) becomes g(f(x), y)
            translate_app({app, Func, [Left | Args], AppLoc}, State);
        {var, _, _} ->
            %% a |> f becomes f(a)
            {CoreRight, State2} = translate_expr(Right, State1),
            {cerl:c_apply(CoreRight, [CoreLeft]), State2};
        _ ->
            %% General case: treat right as a function
            {CoreRight, State2} = translate_expr(Right, State1),
            {cerl:c_apply(CoreRight, [CoreLeft]), State2}
    end;

%% Arithmetic operators
translate_binary_op({binary_op, Op, Left, Right, _Loc}, State)
  when Op =:= '+'; Op =:= '-'; Op =:= '*'; Op =:= '/';
       Op =:= 'div'; Op =:= 'rem' ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom(Op),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2};

%% Comparison operators
translate_binary_op({binary_op, Op, Left, Right, _Loc}, State)
  when Op =:= '=='; Op =:= '/='; Op =:= '<'; Op =:= '>';
       Op =:= '=<'; Op =:= '>='; Op =:= '=:='; Op =:= '=/=' ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom(Op),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2};

%% Catena equality operators (=== and !==)
translate_binary_op({binary_op, '===', Left, Right, _Loc}, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('=:='),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2};

translate_binary_op({binary_op, '!==', Left, Right, _Loc}, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('=/='),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2};

%% Boolean operators
translate_binary_op({binary_op, Op, Left, Right, _Loc}, State)
  when Op =:= 'and'; Op =:= 'or'; Op =:= 'andalso'; Op =:= 'orelse' ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom(Op),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2};

%% List append (<>)
translate_binary_op({binary_op, '<>', Left, Right, _Loc}, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('++'),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2};

%% List cons (::)
translate_binary_op({binary_op, '::', Left, Right, _Loc}, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    {cerl:c_cons(CoreLeft, CoreRight), State2};

%% Default: treat as BIF call
translate_binary_op({binary_op, Op, Left, Right, _Loc}, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom(Op),
        [CoreLeft, CoreRight]
    ),
    {BifCall, State2}.

%%====================================================================
%% Lambda Translation
%%====================================================================

%% @doc Translate lambda expressions to Core Erlang functions
translate_lambda({lambda, Params, Body, _Loc}, State) ->
    %% Create parameter variables
    ParamVars = [cerl:c_var(param_name(P)) || P <- Params],

    %% Translate body
    {CoreBody, State1} = translate_expr(Body, State),

    %% Create Core Erlang fun
    Fun = cerl:c_fun(ParamVars, CoreBody),
    {Fun, State1}.

param_name({var, Name, _}) -> Name;
param_name({typed_var, Name, _, _}) -> Name;
param_name(_) -> '_'.

%%====================================================================
%% If Expression Translation
%%====================================================================

%% @doc Translate if expressions to Core Erlang case
translate_if({if_expr, Cond, Then, Else, _Loc}, State) ->
    {CoreCond, State1} = translate_expr(Cond, State),
    {CoreThen, State2} = translate_expr(Then, State1),
    {CoreElse, State3} = translate_expr(Else, State2),

    %% Translate to case expression
    TrueClause = cerl:c_clause([cerl:c_atom(true)], CoreThen),
    FalseClause = cerl:c_clause([cerl:c_atom(false)], CoreElse),

    Case = cerl:c_case(CoreCond, [TrueClause, FalseClause]),
    {Case, State3}.

%%====================================================================
%% List and Tuple Translation
%%====================================================================

%% @doc Translate list literals
translate_list({list_expr, Elements, _Loc}, State) ->
    {CoreElements, State1} = translate_exprs(Elements, State),
    %% Build list from elements
    List = lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), CoreElements),
    {List, State1}.

%% @doc Translate tuple literals
translate_tuple({tuple_expr, Elements, _Loc}, State) ->
    {CoreElements, State1} = translate_exprs(Elements, State),
    {cerl:c_tuple(CoreElements), State1}.

%%====================================================================
%% Effect Operation Translation (1.3.1.5)
%%====================================================================

%% @doc Translate perform expression to process-based message passing
%%
%% perform Effect.operation(args) becomes:
%% 1. Get handler process for Effect from process dictionary
%% 2. Send message {perform, Effect, operation, [args], self()}
%% 3. Receive result
translate_perform({perform_expr, Effect, Operation, Args, _Loc}, State) ->
    {CoreArgs, State1} = translate_exprs(Args, State),

    %% Build the effect invocation
    %% This creates: effect_runtime:perform(Effect, Operation, Args)
    PerformCall = cerl:c_call(
        cerl:c_atom(catena_effect_runtime),
        cerl:c_atom(perform),
        [
            cerl:c_atom(Effect),
            cerl:c_atom(Operation),
            build_list(CoreArgs)
        ]
    ),
    {PerformCall, State1}.

%% @doc Translate try/with expression to process-based handler
%%
%% try body with handlers becomes:
%% 1. Spawn handler processes for each effect
%% 2. Execute body with handler routing
%% 3. Cleanup handler processes
translate_try_with({try_with_expr, Body, Handlers, _Loc}, State) ->
    {CoreBody, State1} = translate_expr(Body, State),

    %% Translate handlers to handler specifications
    {HandlerSpecs, State2} = translate_handlers(Handlers, State1),

    %% Wrap body with handler setup/teardown
    %% This creates: effect_runtime:with_handlers(HandlerSpecs, fun() -> Body end)
    BodyFun = cerl:c_fun([], CoreBody),
    WithHandlers = cerl:c_call(
        cerl:c_atom(catena_effect_runtime),
        cerl:c_atom(with_handlers),
        [HandlerSpecs, BodyFun]
    ),
    {WithHandlers, State2}.

%% Translate handler clauses to handler specifications
translate_handlers(Handlers, State) ->
    {CoreHandlers, FinalState} = lists:mapfoldl(
        fun(Handler, St) ->
            translate_handler(Handler, St)
        end,
        State,
        Handlers
    ),
    {build_list(CoreHandlers), FinalState}.

translate_handler({handler_clause, Effect, Operations, _Loc}, State) ->
    %% Translate each operation case
    {OpCases, State1} = lists:mapfoldl(
        fun({operation_case, OpName, Params, Body, _OpLoc}, St) ->
            %% Create parameter variables
            ParamVars = [cerl:c_var(param_name(P)) || P <- Params],
            %% Translate body
            {CoreBody, St1} = translate_expr(Body, St),
            %% Create handler function
            HandlerFun = cerl:c_fun(ParamVars, CoreBody),
            %% Create tuple {operation_name, handler_fun}
            OpSpec = cerl:c_tuple([cerl:c_atom(OpName), HandlerFun]),
            {OpSpec, St1}
        end,
        State,
        Operations
    ),

    %% Create handler spec tuple {Effect, [{op1, fun}, {op2, fun}, ...]}
    HandlerSpec = cerl:c_tuple([
        cerl:c_atom(Effect),
        build_list(OpCases)
    ]),
    {HandlerSpec, State1}.

%%====================================================================
%% Unary Operator Translation
%%====================================================================

translate_unary_op({unary_op, '-', Operand, _Loc}, State) ->
    {CoreOperand, State1} = translate_expr(Operand, State),
    Neg = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('-'),
        [cerl:c_int(0), CoreOperand]
    ),
    {Neg, State1};

translate_unary_op({unary_op, 'not', Operand, _Loc}, State) ->
    {CoreOperand, State1} = translate_expr(Operand, State),
    Not = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('not'),
        [CoreOperand]
    ),
    {Not, State1};

translate_unary_op({unary_op, Op, Operand, _Loc}, State) ->
    {CoreOperand, State1} = translate_expr(Operand, State),
    UnaryCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom(Op),
        [CoreOperand]
    ),
    {UnaryCall, State1}.

%%====================================================================
%% Record Access Translation
%%====================================================================

translate_record_access({record_access, Record, Field, _Loc}, State) ->
    {CoreRecord, State1} = translate_expr(Record, State),
    %% Record access translates to maps:get or element/2 depending on representation
    %% For now, use maps:get assuming records are maps
    Access = cerl:c_call(
        cerl:c_atom(maps),
        cerl:c_atom(get),
        [cerl:c_atom(Field), CoreRecord]
    ),
    {Access, State1}.

%%====================================================================
%% Constructor Translation
%%====================================================================

translate_constructor({constructor, Name, Args, _Loc}, State) ->
    {CoreArgs, State1} = translate_exprs(Args, State),
    %% Constructors translate to tagged tuples: {Name, Arg1, Arg2, ...}
    Constructor = cerl:c_tuple([cerl:c_atom(Name) | CoreArgs]),
    {Constructor, State1}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Build a Core Erlang list from elements
build_list(Elements) ->
    lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), Elements).
