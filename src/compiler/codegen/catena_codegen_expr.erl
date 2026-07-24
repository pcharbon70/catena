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
    translate_record/2,

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

%% A qualified import is resolved before typing and carries its executable
%% module/function identity through to code generation.
translate_expr({imported_ref, Entry, _Loc}, State) ->
    translate_callable_value(Entry, State);

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

%% Record literals
translate_expr({record_expr, Fields, Loc}, State) ->
    translate_record({record_expr, Fields, Loc}, State);

%% Match expressions
translate_expr({match_expr, Scrutinee, Clauses, _Loc}, State) ->
    {CoreScrutinee, State1} = translate_expr(Scrutinee, State),
    catena_codegen_pattern:compile_match(
        CoreScrutinee,
        Clauses,
        State1,
        #{optimize => true, warn_incomplete => true}
    );

%% Perform expression (effect invocation)
translate_expr({perform_expr, Effect, Operation, Args, Loc}, State) ->
    translate_perform({perform_expr, Effect, Operation, Args, Loc}, State);

%% Handle expression (parser AST)
translate_expr({handle_expr, Body, Handlers, Loc}, State) ->
    catena_effect_codegen:translate_handle({handle_expr, Body, Handlers, Loc}, State);

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
translate_expr(Unknown, _State) ->
    unsupported(expression_translation, expression, Unknown).

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
    {cerl:c_atom(false), State};

translate_literal({literal, Type, Value, Loc} = Literal, _State) ->
    unsupported(
        expression_translation,
        literal,
        Literal,
        #{literal_type => Type, literal_value => Value, location => Loc}
    ).

%%====================================================================
%% Variable Translation
%%====================================================================

%% @doc Translate variables to Core Erlang
translate_var({var, Name, _Loc} = Variable, State) ->
    case catena_codegen_utils:is_bound(Name, State) orelse
        not catena_codegen_utils:resolution_enabled(State)
    of
        true ->
            {cerl:c_var(Name), State};
        false ->
            case catena_codegen_utils:resolve_value(Name, Variable, State) of
                {ok, Callable} ->
                    translate_callable_value(Callable, State);
                {error, CallableDiagnostic} ->
                    case resolve_trait_value(Name, Variable, State) of
                        {ok, Candidates, Arity} ->
                            translate_trait_callable_value(
                                Name,
                                Arity,
                                Candidates,
                                State
                            );
                        {error, _} ->
                            throw(CallableDiagnostic)
                    end
            end
    end.

%%====================================================================
%% Function Application Translation (1.3.1.1)
%%====================================================================

%% @doc Translate function application to Core Erlang
%%
%% Function calls are translated to either:
%% - cerl:c_apply for local function calls
%% - cerl:c_call for module-qualified calls
translate_app({app, _Func, _Args, _Loc} = Application, State) ->
    case direct_local_application(Application, State) of
        {direct, FuncName, Args, Loc} ->
            {CoreArgs, State1} = translate_exprs(Args, State),
            translate_named_app(
                FuncName,
                CoreArgs,
                Loc,
                Application,
                State1
            );
        closure ->
            translate_closure_application(Application, State)
    end.

translate_closure_application(
    {app, Func, Args, Loc} = Application,
    State
) ->
    %% Translate arguments first
    {CoreArgs, State1} = translate_exprs(Args, State),

    %% Translate the function expression
    case Func of
        {imported_ref, Entry, _} ->
            ensure_imported_arity(Entry, length(CoreArgs), Application),
            {remote_call(Entry, CoreArgs), State1};

        %% Module-qualified call: Module.function(args)
        {module_call, Module, FuncName, _} ->
            ModAtom = cerl:c_atom(Module),
            FuncAtom = cerl:c_atom(FuncName),
            {cerl:c_call(ModAtom, FuncAtom, CoreArgs), State1};

        %% Direct function reference by name
        {var, FuncName, _} ->
            translate_named_app(
                FuncName,
                CoreArgs,
                Loc,
                Application,
                State1
            );

        %% Lambda or other expression as function
        _ ->
            {CoreFunc, State2} = translate_expr(Func, State1),
            {cerl:c_apply(CoreFunc, CoreArgs), State2}
    end.

direct_local_application(Application, State) ->
    case catena_codegen_utils:resolution_enabled(State) of
        false ->
            closure;
        true ->
            {Root, Arguments, Location} =
                application_spine(Application),
            case Root of
                {var, Name, _}
                  when is_atom(Name) ->
                    case catena_codegen_utils:is_bound(Name, State) of
                        true -> closure;
                        false -> {direct, Name, Arguments, Location}
                    end;
                _ ->
                    closure
            end
    end.

application_spine({app, Function, Arguments, Location}) ->
    case Function of
        {app, _, [], _} ->
            {Function, Arguments, Location};
        {app, _, _, _} ->
            {Root, EarlierArguments, _EarlierLocation} =
                application_spine(Function),
            {Root, EarlierArguments ++ Arguments, Location};
        _ ->
            {Function, Arguments, Location}
    end.

translate_named_app(FuncName, CoreArgs, _Loc, _Application, State)
  when not is_atom(FuncName) ->
    {cerl:c_apply(cerl:c_var(FuncName), CoreArgs), State};
translate_named_app(FuncName, CoreArgs, _Loc, _Application, State) ->
    case catena_codegen_utils:is_bound(FuncName, State) of
        true ->
            {cerl:c_apply(cerl:c_var(FuncName), CoreArgs), State};
        false ->
            case catena_codegen_utils:resolution_enabled(State) of
                false ->
                    {cerl:c_apply(cerl:c_var(FuncName), CoreArgs), State};
                true ->
                    case catena_codegen_utils:resolve_transform(
                        FuncName,
                        length(CoreArgs),
                        _Application,
                        State
                    ) of
                        {ok, Callable} ->
                            Arity = maps:get(arity, Callable),
                            case maps:get(imported, Callable, false) of
                                true ->
                                    {remote_call(Callable, CoreArgs), State};
                                false ->
                                    {Target, RuntimeArgs} =
                                        local_transform_target(
                                            FuncName,
                                            Arity,
                                            CoreArgs,
                                            State
                                        ),
                                    {cerl:c_apply(Target, RuntimeArgs), State}
                            end;
                        {error, CallableDiagnostic} ->
                            case catena_codegen_utils:
                                resolve_trait_method(
                                    FuncName,
                                    length(CoreArgs),
                                    _Application,
                                    State
                                )
                            of
                                {ok, Candidates} ->
                                    {
                                        trait_dispatch_call(
                                            Candidates,
                                            FuncName,
                                            CoreArgs
                                        ),
                                        State
                                    };
                                {error, _} ->
                                    throw(CallableDiagnostic)
                            end
                    end
            end
    end.

%%====================================================================
%% Let Binding Translation (1.3.1.2)
%%====================================================================

%% @doc Translate let bindings to Core Erlang
%%
%% let x = expr1, y = expr2 in body
%% becomes nested Core Erlang let expressions
translate_let({let_expr, Bindings, Body, _Loc}, State) ->
    translate_let_bindings(Bindings, Body, State).

translate_let_bindings([], Body, State) ->
    translate_expr(Body, State);
translate_let_bindings([{Pattern, BindExpr} | Rest], Body, State) ->
    CoreVar = pattern_to_var(Pattern),
    BindingName = cerl:var_name(CoreVar),
    {CoreBindExpr, State1} = translate_expr(BindExpr, State),
    {CoreBody, State2} = catena_codegen_utils:with_bindings(
        [BindingName],
        fun(ScopedState) ->
            translate_let_bindings(Rest, Body, ScopedState)
        end,
        State1
    ),
    {cerl:c_let([CoreVar], CoreBindExpr, CoreBody), State2}.

%% Convert a pattern to a Core Erlang variable
%% For simple variable patterns; complex patterns need pattern compilation
pattern_to_var({var, Name, _Loc}) ->
    cerl:c_var(Name);
pattern_to_var({pat_var, Name, _Loc}) ->
    cerl:c_var(Name);
pattern_to_var({wildcard, _Loc}) ->
    cerl:c_var('_');
pattern_to_var({pat_wildcard, _Loc}) ->
    cerl:c_var('_');
pattern_to_var(Complex) ->
    unsupported(let_binding_translation, binding_pattern, Complex).

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
    case Right of
        {app, Func, Args, AppLoc} ->
            %% f(x) |> g(y) becomes g(f(x), y)
            translate_app({app, Func, [Left | Args], AppLoc}, State);
        {var, _, RightLoc} ->
            %% a |> f becomes f(a)
            translate_app({app, Right, [Left], RightLoc}, State);
        _ ->
            %% General case: treat right as a function
            {CoreLeft, State1} = translate_expr(Left, State),
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
    translate_list_append(Left, Right, State);

%% Parser-native list append (++)
translate_binary_op({binary_op, '++', Left, Right, _Loc}, State) ->
    translate_list_append(Left, Right, State);

translate_binary_op({binary_op, '::', Left, Right, _Loc}, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    {cerl:c_cons(CoreLeft, CoreRight), State2};

%% Every accepted operator requires an explicit lowering.
translate_binary_op({binary_op, Op, Left, Right, Loc} = Expr, _State) ->
    unsupported(
        operator_translation,
        operator,
        Expr,
        #{operator => Op, operands => [Left, Right], location => Loc}
    ).

translate_list_append(Left, Right, State) ->
    {CoreLeft, State1} = translate_expr(Left, State),
    {CoreRight, State2} = translate_expr(Right, State1),
    BifCall = cerl:c_call(
        cerl:c_atom(erlang),
        cerl:c_atom('++'),
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

    %% Translate body with lambda parameters in lexical value scope.
    ParamNames = [cerl:var_name(ParamVar) || ParamVar <- ParamVars],
    {CoreBody, State1} = catena_codegen_utils:with_bindings(
        ParamNames,
        fun(ScopedState) ->
            translate_expr(Body, ScopedState)
        end,
        State
    ),

    %% Create Core Erlang fun
    Fun = cerl:c_fun(ParamVars, CoreBody),
    {Fun, State1}.

param_name({var, Name, _}) -> Name;
param_name({pat_var, Name, _}) -> Name;
param_name({typed_var, Name, _, _}) -> Name;
param_name({pat_typed_var, Name, _, _}) -> Name;
param_name(Pattern) ->
    unsupported(lambda_translation, parameter_pattern, Pattern).

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

%% @doc Translate record literals to Core Erlang maps.
translate_record({record_expr, Fields, _Loc}, State) ->
    {CorePairs, State1} = lists:mapfoldl(
        fun({Field, Value}, CurrentState) ->
            {CoreValue, NextState} = translate_expr(Value, CurrentState),
            {cerl:c_map_pair(cerl:c_atom(Field), CoreValue), NextState}
        end,
        State,
        Fields
    ),
    {cerl:c_map(CorePairs), State1}.

%%====================================================================
%% Effect Operation Translation (1.3.1.5)
%%====================================================================

%% @doc Translate perform expression to process-based message passing
%%
%% perform Effect.operation(args) becomes:
%% catena_effect_runtime:perform(Ctx, Effect, Operation, Args)
%% where Ctx is the current effect context (passed through __catena_ctx__)
translate_perform({perform_expr, Effect, Operation, Args, _Loc}, State) ->
    catena_effect_codegen:translate_perform(
        {perform_expr, Effect, Operation, Args, _Loc},
        State
    ).

%% @doc Translate try/with expression to process-based handler
%%
%% try body with handlers becomes:
%% catena_effect_runtime:with_handlers(Ctx, HandlerSpecs, fun(NewCtx) -> Body end)
%% where Ctx is the current effect context and NewCtx is the child context
translate_try_with({try_with_expr, Body, Handlers, _Loc}, State) ->
    catena_effect_codegen:translate_try_with(
        {try_with_expr, Body, Handlers, _Loc},
        State
    ).

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

translate_unary_op({unary_op, Op, Operand, Loc} = Expr, _State) ->
    unsupported(
        operator_translation,
        unary_operator,
        Expr,
        #{operator => Op, operand => Operand, location => Loc}
    ).

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

translate_constructor({constructor, Name, Args, _Loc} = Constructor, State) ->
    case catena_codegen_utils:resolution_enabled(State) of
        true ->
            case catena_codegen_utils:resolve_constructor(
                Name,
                length(Args),
                Constructor,
                State
            ) of
                {ok, _Callable} ->
                    translate_tagged_constructor(Name, Args, State);
                {error, Diagnostic} ->
                    throw(Diagnostic)
            end;
        false ->
            translate_tagged_constructor(Name, Args, State)
    end.

translate_tagged_constructor(Name, Args, State) ->
    {CoreArgs, State1} = translate_exprs(Args, State),
    {cerl:c_tuple([cerl:c_atom(Name) | CoreArgs]), State1}.

translate_callable_value(
    #{kind := transform, name := Name, arity := Arity} = Callable,
    State
) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(Arity, State),
    Body = case maps:get(imported, Callable, false) of
        true ->
            remote_call(Callable, Arguments);
        false ->
            {Target, RuntimeArguments} = local_transform_target(
                Name,
                Arity,
                Arguments,
                State1
            ),
            cerl:c_apply(Target, RuntimeArguments)
    end,
    {cerl:c_fun(Arguments, Body), State1};
translate_callable_value(
    #{kind := constructor, name := Name, arity := Arity},
    State
) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(Arity, State),
    Body = cerl:c_tuple([cerl:c_atom(Name) | Arguments]),
    case Arity of
        0 -> {Body, State1};
        _ -> {cerl:c_fun(Arguments, Body), State1}
    end.

resolve_trait_value(Name, SourceTerm, State) ->
    case catena_codegen_utils:resolve_trait_value(
        Name,
        SourceTerm,
        State
    ) of
        {ok, Arity, Candidates} ->
            {ok, Candidates, Arity};
        {error, _} = Error ->
            Error
    end.

translate_trait_callable_value(Name, Arity, Candidates, State) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(Arity, State),
    Body = trait_dispatch_call(Candidates, Name, Arguments),
    {cerl:c_fun(Arguments, Body), State1}.

trait_dispatch_call(Candidates, Method, Arguments) ->
    cerl:c_call(
        cerl:c_atom(catena_trait_runtime),
        cerl:c_atom(invoke),
        [
            cerl:abstract(Candidates),
            cerl:c_atom(Method),
            core_list(Arguments)
        ]
    ).

core_list(Elements) ->
    lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), Elements).

remote_call(Entry, Arguments) ->
    cerl:c_call(
        cerl:c_atom(maps:get(runtime_module, Entry)),
        cerl:c_atom(maps:get(name, Entry)),
        Arguments
    ).

ensure_imported_arity(Entry, Actual, SourceTerm) ->
    Expected = maps:get(arity, Entry),
    case Expected =:= Actual of
        true ->
            ok;
        false ->
            Context = catena_backend_error:context(
                call_resolution,
                imported_call,
                SourceTerm,
                #{
                    source_module => maps:get(source_module, Entry),
                    runtime_module => maps:get(runtime_module, Entry)
                }
            ),
            throw(catena_backend_error:arity_mismatch(
                maps:get(name, Entry),
                Expected,
                Actual,
                Context
            ))
    end.

local_transform_target(Name, Arity, Arguments, State) ->
    case
        catena_codegen_utils:is_effectful_transform(Name, State) andalso
            catena_codegen_utils:has_runtime_context(State)
    of
        true ->
            Context = catena_codegen_utils:runtime_context(State),
            {
                cerl:c_fname(
                    catena_codegen_utils:effect_entry_name(Name),
                    Arity + 1
                ),
                [Context | Arguments]
            };
        false ->
            {cerl:c_fname(Name, Arity), Arguments}
    end.

unsupported(Stage, Construct, SourceTerm) ->
    unsupported(Stage, Construct, SourceTerm, #{}).

unsupported(Stage, Construct, SourceTerm, Extra) ->
    Context =
        catena_backend_error:context(
            Stage,
            Construct,
            SourceTerm,
            Extra
        ),
    throw(
        catena_backend_error:unsupported_backend_construct(
            Construct,
            Context
        )
    ).
