%%%-------------------------------------------------------------------
%%% @doc Core Erlang lowering for validated selective-CPS control IR.
%%%
%%% Resumable transforms receive private `(Args..., Context, Continuation)`
%%% entries while direct transforms receive `(Args..., Context)` entries.
%%% Public wrappers retain source arity and establish the runtime context and
%%% final continuation exactly once.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_control_codegen).

-export([
    generate/1,
    private_name/2,
    control_abi_version/0
]).

-define(CONTROL_ABI_VERSION, 1).

-spec control_abi_version() -> pos_integer().
control_abi_version() -> ?CONTROL_ABI_VERSION.

-spec private_name(direct | cps, atom()) -> atom().
private_name(Mode, Name) ->
    list_to_atom(
        "$catena_" ++ atom_to_list(Mode) ++ "$" ++ atom_to_list(Name)
    ).

-spec generate(catena_compilation_unit:t()) ->
    {ok, cerl:cerl()} | {error, term()}.
generate(Unit) ->
    try
        ok = validate_unit(Unit),
        IR = catena_compilation_unit:control_ir(Unit),
        Module = catena_compilation_unit:runtime_module(Unit),
        Options = catena_compilation_unit:options(Unit),
        CodegenOptions = maps:get(codegen_opts, Options, #{}),
        State0 = catena_codegen_utils:new_state(#{
            module_name => Module,
            source_file => maps:get(file, CodegenOptions, "nofile"),
            callables => catena_compilation_unit:callables(Unit),
            import_resolution =>
                catena_compilation_unit:import_resolution(Unit),
            trait_inventory =>
                catena_compilation_unit:trait_inventory(Unit),
            control_modes => catena_compilation_unit:control_modes(Unit),
            effectful_transforms =>
                catena_compilation_unit:effectful_transforms(Unit)
        }),
        {Definitions0, State1} = lists:mapfoldl(
            fun compile_transform/2,
            State0,
            catena_control_ir:transforms(IR)
        ),
        {DictionaryDefinitions, _State2, DictionaryExports} =
            catena_trait_dictionary:compile_dictionaries(
                catena_compilation_unit:trait_inventory(Unit),
                fun(Dictionary, Name, Method, DictionaryState) ->
                    compile_dictionary_method(
                        Unit,
                        Dictionary,
                        Name,
                        Method,
                        DictionaryState
                    )
                end,
                State1
            ),
        Definitions = lists:append(Definitions0) ++ DictionaryDefinitions,
        Exports = public_exports(Unit, IR) ++
            private_exports(Unit, IR) ++ DictionaryExports,
        Attributes = catena_codegen_module:generate_attributes(
            CodegenOptions#{
                runtime_dependencies =>
                    catena_compilation_unit:runtime_dependencies(Unit),
                artifact_dependencies =>
                    catena_compilation_unit:artifact_dependencies(Unit),
                control_abi_version => ?CONTROL_ABI_VERSION,
                resumption_runtime_version =>
                    catena_resumption_runtime:version(),
                handler_frame_features =>
                    catena_resumption_runtime:features()
            }
        ),
        Core0 = cerl:c_module(
            cerl:c_atom(Module),
            Exports,
            Attributes,
            Definitions
        ),
        {ok, catena_core_origin:synthetic(
            Core0,
            selective_cps_module,
            catena_compilation_unit:normalized_ast(Unit),
            State0,
            #{
                generated_identity => Module,
                control_abi_version => ?CONTROL_ABI_VERSION
            }
        )}
    catch
        throw:{backend_error, _, _} = Diagnostic ->
            {error, Diagnostic};
        error:{backend_error, _, _} = Diagnostic:_Stack ->
            {error, Diagnostic};
        Class:Reason:_Stack ->
            {error, {control_codegen_error, #{class => Class, reason => Reason}}}
    end.

validate_unit(Unit) ->
    case {
        catena_compilation_unit:is_compilation_unit(Unit),
        catena_control_ir:is_ir(catena_compilation_unit:control_ir(Unit)),
        catena_control_validate:is_report(
            catena_compilation_unit:control_validation(Unit)
        )
    } of
        {true, true, true} -> ok;
        _ -> error({invalid_compilation_unit, invalid_control_lowering_input})
    end.

compile_transform(Transform, State0) ->
    Name = maps:get(name, Transform),
    Arity = maps:get(arity, Transform),
    Mode = maps:get(control_mode, Transform),
    SourceVars = [
        cerl:c_var(generated_name("arg", Index))
        || Index <- lists:seq(1, Arity)
    ],
    ContextVar = cerl:c_var(generated_name("context", Arity)),
    ContinuationVar = cerl:c_var(generated_name("continuation", Arity)),
    BoundNames = [cerl:var_name(Var) || Var <- SourceVars],
    {PrivateBody, State1} = catena_codegen_utils:with_function_scope(
        Name,
        BoundNames,
        fun(FunctionState) ->
            catena_codegen_utils:with_runtime_context(
                ContextVar,
                fun(RuntimeState) ->
                    compile_transform_clauses(
                        Transform,
                        SourceVars,
                        ContextVar,
                        case Mode of
                            direct -> none;
                            resumable -> ContinuationVar
                        end,
                        RuntimeState
                    )
                end,
                FunctionState
            )
        end,
        State0
    ),
    PrivateMode = case Mode of direct -> direct; resumable -> cps end,
    PrivateName = private_name(PrivateMode, Name),
    PrivateParams = case Mode of
        direct -> SourceVars ++ [ContextVar];
        resumable -> SourceVars ++ [ContextVar, ContinuationVar]
    end,
    PrivateIdentity = {PrivateName, length(PrivateParams)},
    PrivateConstruct = case
        Mode =:= resumable orelse
            catena_codegen_utils:is_effectful_transform(Name, State0)
    of
        true -> effect_runtime_entry;
        false -> selective_cps_private_entry
    end,
    PrivateDefinition = {
        catena_core_origin:synthetic(
            cerl:c_fname(PrivateName, length(PrivateParams)),
            PrivateConstruct,
            maps:get(origin, Transform),
            State0,
            #{transform => Name, generated_identity => PrivateIdentity}
        ),
        catena_core_origin:synthetic(
            cerl:c_fun(PrivateParams, PrivateBody),
            PrivateConstruct,
            maps:get(origin, Transform),
            State0,
            #{transform => Name, generated_identity => PrivateIdentity}
        )
    },
    {WrapperDefinition, State2} = compile_public_wrapper(
        Transform,
        SourceVars,
        State1
    ),
    {[WrapperDefinition, PrivateDefinition], State2}.

compile_public_wrapper(Transform, SourceVars, State0) ->
    Name = maps:get(name, Transform),
    Arity = maps:get(arity, Transform),
    Mode = maps:get(control_mode, Transform),
    {ContextVar, State1} = catena_codegen_utils:fresh_var(State0),
    EmptyContext = runtime_call(catena_effect_runtime, empty_context, []),
    PrivateMode = case Mode of direct -> direct; resumable -> cps end,
    PrivateTarget = cerl:c_fname(private_name(PrivateMode, Name),
        Arity + case Mode of direct -> 1; resumable -> 2 end),
    {Call, State2} = case Mode of
        direct ->
            {cerl:c_apply(PrivateTarget, SourceVars ++ [ContextVar]), State1};
        resumable ->
            {ValueVar, S1} = catena_codegen_utils:fresh_var(State1),
            {FinalContextVar, S2} = catena_codegen_utils:fresh_var(S1),
            FinalContinuation = cerl:c_fun(
                [ValueVar, FinalContextVar],
                ValueVar
            ),
            {
                cerl:c_apply(
                    PrivateTarget,
                    SourceVars ++ [ContextVar, FinalContinuation]
                ),
                S2
            }
    end,
    WrapperBody = cerl:c_let([ContextVar], EmptyContext, Call),
    Identity = {Name, Arity},
    Definition = {
        catena_core_origin:user(
            cerl:c_fname(Name, Arity),
            transform_name,
            maps:get(origin, Transform),
            State0,
            #{transform => Name, generated_identity => Identity}
        ),
        catena_core_origin:synthetic(
            cerl:c_fun(SourceVars, WrapperBody),
            selective_cps_public_wrapper,
            maps:get(origin, Transform),
            State0,
            #{transform => Name, generated_identity => Identity}
        )
    },
    {Definition, State2}.

compile_transform_clauses(Transform, SourceVars, Context, Continuation, State0) ->
    Arity = maps:get(arity, Transform),
    Scrutinee = case SourceVars of
        [] -> cerl:c_atom(true);
        [Only] -> Only;
        _ -> cerl:c_tuple(SourceVars)
    end,
    {Clauses, State1} = lists:mapfoldl(
        fun(Clause, CurrentState) ->
            compile_transform_clause(
                Clause,
                Arity,
                Context,
                Continuation,
                CurrentState
            )
        end,
        State0,
        maps:get(clauses, Transform)
    ),
    CompleteClauses = case SourceVars of
        [] -> Clauses ++ [zero_arity_failure_clause()];
        _ -> Clauses
    end,
    {cerl:c_case(Scrutinee, CompleteClauses), State1}.

zero_arity_failure_clause() ->
    cerl:c_clause(
        [cerl:c_atom(false)],
        cerl:c_atom(true),
        runtime_call(erlang, error, [cerl:c_atom(function_clause)])
    ).

compile_transform_clause(Clause, Arity, Context, Continuation, State0) ->
    Patterns = maps:get(patterns, Clause),
    Pattern = case Arity of
        0 -> cerl:c_atom(true);
        1 -> hd(Patterns);
        _ -> {pat_tuple, Patterns, maps:get(origin, Clause)}
    end,
    {CorePattern, State1} = case Arity of
        0 -> {Pattern, State0};
        _ -> catena_codegen_pattern:compile_pattern(Pattern, State0)
    end,
    Bindings = pattern_bindings(Pattern),
    {{CoreGuard, CoreBody}, State2} = catena_codegen_utils:with_bindings(
        Bindings,
        fun(ScopedState) ->
            {Guard, GuardState} = compile_guards(
                maps:get(guards, Clause, []),
                ScopedState
            ),
            {Body, BodyState} = compile_node(
                maps:get(body, Clause),
                Context,
                Continuation,
                GuardState
            ),
            {{Guard, Body}, BodyState}
        end,
        State1
    ),
    {
        cerl:c_clause([CorePattern], CoreGuard, CoreBody),
        State2
    }.

compile_node(Node, Context, Continuation, State) ->
    case maps:get(op, Node) of
        return ->
            compile_node(
                maps:get(value, maps:get(fields, Node)),
                Context,
                Continuation,
                State
            );
        direct_expr ->
            compile_direct_expr(Node, Context, Continuation, State);
        bind ->
            compile_bind(Node, Context, Continuation, State);
        match ->
            compile_match(Node, Context, Continuation, State);
        perform ->
            compile_perform(Node, Context, Continuation, State);
        delimiter ->
            compile_delimiter(Node, Context, Continuation, State);
        install_handler ->
            compile_install_handler(Node, Context, Continuation, State);
        resume ->
            compile_resume(Node, Context, Continuation, State);
        abort ->
            compile_abort(Node, Context, State);
        direct_call ->
            compile_call(Node, Context, Continuation, State);
        cps_call ->
            compile_call(Node, Context, Continuation, State);
        bridge ->
            compile_call(Node, Context, Continuation, State);
        closure ->
            compile_closure(Node, Context, Continuation, State);
        Unsupported ->
            control_codegen_error(
                unsupported_control_node,
                Node,
                #{operation => Unsupported}
            )
    end.

compile_direct_expr(Node, Context, Continuation, State0) ->
    Source = maps:get(source, maps:get(fields, Node)),
    {Template, Embedded, State1} = extract_embedded_control(Source, State0),
    Bindings = [cerl:var_name(Variable) || {Variable, _} <- Embedded],
    catena_codegen_utils:with_bindings(
        Bindings,
        fun(ScopedState) ->
            compile_embedded_control(
                Embedded,
                Template,
                Context,
                Continuation,
                ScopedState
            )
        end,
        State1
    ).

compile_embedded_control([], Source, Context, Continuation, State0) ->
    {Value, State1} = compile_direct_value(Source, Context, State0),
    {continue(Value, Context, Continuation), State1};
compile_embedded_control(
    [{ValueVar, Node} | Rest],
    Source,
    Context,
    Continuation,
    State0
) ->
    {RestoredContext, State1} = catena_codegen_utils:fresh_var(State0),
    {RestCore, State2} = compile_embedded_control(
        Rest,
        Source,
        RestoredContext,
        Continuation,
        State1
    ),
    EmbeddedContinuation = cerl:c_fun(
        [ValueVar, RestoredContext],
        RestCore
    ),
    compile_node(Node, Context, EmbeddedContinuation, State2).

compile_direct_value({var, true, _Loc} = Source, Context, State0) ->
    translate_direct_value(Source, Context, State0);
compile_direct_value({var, false, _Loc} = Source, Context, State0) ->
    translate_direct_value(Source, Context, State0);
compile_direct_value({var, Name, _Loc} = Source, Context, State0) ->
    case catena_codegen_utils:is_bound(Name, State0) of
        true ->
            translate_direct_value(Source, Context, State0);
        false ->
            case catena_codegen_utils:resolve_value(Name, Source, State0) of
                {ok, Callable} ->
                    compile_callable_value(Callable, Context, State0);
                {error, CallableDiagnostic} ->
                    case catena_codegen_utils:resolve_trait_value(
                        Name,
                        Source,
                        State0
                    ) of
                        {ok, Arity, Candidates} ->
                            compile_trait_callable_value(
                                Name,
                                Arity,
                                Candidates,
                                State0
                            );
                        {error, _} ->
                            throw(CallableDiagnostic)
                    end
            end
    end;
compile_direct_value(Source, Context, State) ->
    translate_direct_value(Source, Context, State).

translate_direct_value(Source, Context, State0) ->
    Lowered = catena_codegen_lower:lower_expr(Source),
    catena_codegen_utils:with_runtime_context(
        Context,
        fun(ScopedState) ->
            catena_codegen_expr:translate_expr(Lowered, ScopedState)
        end,
        State0
    ).

extract_embedded_control(Term, State0) when is_map(Term) ->
    case catena_control_ir:is_node(Term) of
        true ->
            {Variable, State1} = catena_codegen_utils:fresh_var(State0),
            Origin = maps:get(origin, maps:get(metadata, Term)),
            {
                {var, cerl:var_name(Variable), Origin},
                [{Variable, Term}],
                State1
            };
        false ->
            {Term, [], State0}
    end;
extract_embedded_control(Term, State0) when is_tuple(Term) ->
    {Parts, State1} = lists:mapfoldl(
        fun extract_embedded_part/2,
        State0,
        tuple_to_list(Term)
    ),
    {
        list_to_tuple([Part || {Part, _Nodes} <- Parts]),
        lists:append([Nodes || {_Part, Nodes} <- Parts]),
        State1
    };
extract_embedded_control(Term, State0) when is_list(Term) ->
    {Parts, State1} = lists:mapfoldl(
        fun extract_embedded_part/2,
        State0,
        Term
    ),
    {
        [Part || {Part, _Nodes} <- Parts],
        lists:append([Nodes || {_Part, Nodes} <- Parts]),
        State1
    };
extract_embedded_control(Term, State) ->
    {Term, [], State}.

extract_embedded_part(Term, State0) ->
    {Template, Nodes, State1} = extract_embedded_control(Term, State0),
    {{Template, Nodes}, State1}.

compile_bind(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    Pattern = maps:get(pattern, Fields),
    {ValueVar, State1} = catena_codegen_utils:fresh_var(State0),
    {RestoredContext, State2} = catena_codegen_utils:fresh_var(State1),
    {CorePattern, State3} = catena_codegen_pattern:compile_pattern(
        Pattern,
        State2
    ),
    Bindings = pattern_bindings(Pattern),
    {Body, State4} = catena_codegen_utils:with_bindings(
        Bindings,
        fun(ScopedState) ->
            compile_node(
                maps:get(body, Fields),
                RestoredContext,
                Continuation,
                ScopedState
            )
        end,
        State3
    ),
    BindContinuation = cerl:c_fun(
        [ValueVar, RestoredContext],
        compile_bind_pattern(Pattern, CorePattern, ValueVar, Body)
    ),
    compile_node(
        maps:get(value, Fields),
        Context,
        BindContinuation,
        State4
    ).

compile_bind_pattern({pat_var, _Name, _Loc}, CorePattern, Value, Body) ->
    %% A one-clause Core case whose pattern is a fresh variable is logically a
    %% let, but OTP's boolean-case optimiser does not accept that degenerate
    %% shape in every surrounding expression. Emit the canonical Core node.
    cerl:c_let([CorePattern], Value, Body);
compile_bind_pattern({pat_wildcard, _Loc}, _CorePattern, _Value, Body) ->
    Body;
compile_bind_pattern(_Pattern, CorePattern, Value, Body) ->
    cerl:c_case(Value, [cerl:c_clause([CorePattern], Body)]).

compile_match(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    {ValueVar, State1} = catena_codegen_utils:fresh_var(State0),
    {RestoredContext, State2} = catena_codegen_utils:fresh_var(State1),
    {Clauses, State3} = lists:mapfoldl(
        fun(Clause, CurrentState) ->
            compile_match_clause(
                Clause,
                RestoredContext,
                Continuation,
                CurrentState
            )
        end,
        State2,
        maps:get(clauses, Fields)
    ),
    MatchContinuation = cerl:c_fun(
        [ValueVar, RestoredContext],
        cerl:c_case(ValueVar, Clauses)
    ),
    compile_node(
        maps:get(scrutinee, Fields),
        Context,
        MatchContinuation,
        State3
    ).

compile_match_clause(Clause, Context, Continuation, State0) ->
    Pattern = maps:get(pattern, Clause),
    {CorePattern, State1} = catena_codegen_pattern:compile_pattern(
        Pattern,
        State0
    ),
    {{Guard, Body}, State2} = catena_codegen_utils:with_bindings(
        pattern_bindings(Pattern),
        fun(ScopedState) ->
            {CoreGuard, GuardState} = compile_guards(
                maps:get(guards, Clause, []),
                ScopedState
            ),
            {CoreBody, BodyState} = compile_node(
                maps:get(body, Clause),
                Context,
                Continuation,
                GuardState
            ),
            {{CoreGuard, CoreBody}, BodyState}
        end,
        State1
    ),
    {cerl:c_clause([CorePattern], Guard, Body), State2}.

compile_perform(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    compile_values(
        maps:get(arguments, Fields),
        Context,
        [],
        fun(Arguments, CurrentContext, State1) ->
            RuntimeContinuation = ensure_continuation(Continuation, State1),
            Call = runtime_call(
                catena_effect_runtime,
                perform_cps,
                [
                    CurrentContext,
                    cerl:c_atom(maps:get(effect, Fields)),
                    cerl:c_atom(maps:get(operation, Fields)),
                    core_list(Arguments),
                    element(1, RuntimeContinuation),
                    cerl:abstract(maps:get(origin, maps:get(metadata, Node)))
                ]
            ),
            {Call, element(2, RuntimeContinuation)}
        end,
        State0
    ).

compile_delimiter(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    {DelimitedResult, State1} = compile_delimiter_body(
        maps:get(body, Fields),
        Context,
        State0
    ),
    {continue(DelimitedResult, Context, Continuation), State1}.

compile_delimiter_body(InstallNode, Context, State0) ->
    case maps:get(op, InstallNode) of
        install_handler ->
            Fields = maps:get(fields, InstallNode),
            compile_handlers(
                maps:get(handlers, Fields),
                maps:get(body, Fields),
                Context,
                State0
            );
        _ ->
            {Identity, State1} = identity_continuation(State0),
            compile_node(InstallNode, Context, Identity, State1)
    end.

compile_install_handler(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    {Result, State1} = compile_handlers(
        maps:get(handlers, Fields),
        maps:get(body, Fields),
        Context,
        State0
    ),
    {continue(Result, Context, Continuation), State1}.

compile_handlers([], BodyNode, Context, State0) ->
    {Identity, State1} = identity_continuation(State0),
    compile_node(BodyNode, Context, Identity, State1);
compile_handlers([Handler | Rest], BodyNode, Context, State0) ->
    {ChildContext, State1} = catena_codegen_utils:fresh_var(State0),
    {Inner, State2} = compile_handlers(
        Rest,
        BodyNode,
        ChildContext,
        State1
    ),
    {HandlerSpec, State3} = compile_handler(Handler, State2),
    BodyFun = cerl:c_fun([ChildContext], Inner),
    {
        runtime_call(
            catena_effect_runtime,
            with_resumable_handler,
            [Context, HandlerSpec, BodyFun]
        ),
        State3
    }.

compile_handler(Handler, State0) ->
    {Cases, State1} = lists:mapfoldl(
        fun compile_handler_operation/2,
        State0,
        maps:get(operations, Handler)
    ),
    {
        core_map([
            {effect, cerl:c_atom(maps:get(effect, Handler))},
            {cases, core_list(Cases)},
            {origin, cerl:abstract(maps:get(origin, Handler))}
        ]),
        State1
    }.

compile_handler_operation(Operation, State0) ->
    Binder = maps:get(binder, Operation),
    Patterns = maps:get(patterns, Operation),
    {ArgumentsVar, State1} = catena_codegen_utils:fresh_var(State0),
    ResumptionVar = cerl:c_var(Binder),
    {HandlerContext, State2} = catena_codegen_utils:fresh_var(State1),
    {CorePatterns, State3} = lists:mapfoldl(
        fun catena_codegen_pattern:compile_pattern/2,
        State2,
        Patterns
    ),
    ArgumentPattern = core_list(CorePatterns),
    Bindings = lists:usort(
        [Binder | lists:append([pattern_bindings(P) || P <- Patterns])]
    ),
    {Body, State4} = catena_codegen_utils:with_bindings(
        Bindings,
        fun(ScopedState) ->
            compile_node(
                maps:get(body, Operation),
                HandlerContext,
                none,
                ScopedState
            )
        end,
        State3
    ),
    HandlerFun = cerl:c_fun(
        [ArgumentsVar, ResumptionVar, HandlerContext],
        cerl:c_case(
            ArgumentsVar,
            [cerl:c_clause([ArgumentPattern], Body)]
        )
    ),
    {
        runtime_call(
            catena_effect_runtime,
            control_case,
            [
                cerl:c_atom(maps:get(operation, Operation)),
                cerl:c_int(length(Patterns)),
                HandlerFun,
                cerl:abstract(maps:get(origin, Operation))
            ]
        ),
        State4
    }.

compile_resume(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    compile_values(
        [maps:get(target, Fields), maps:get(value, Fields)],
        Context,
        [],
        fun([Target, Value], CurrentContext, State1) ->
            Result = runtime_call(
                catena_effect_runtime,
                resume,
                [Target, Value]
            ),
            {continue(Result, CurrentContext, Continuation), State1}
        end,
        State0
    ).

compile_abort(Node, Context, State) ->
    compile_node(
        maps:get(result, maps:get(fields, Node)),
        Context,
        none,
        State
    ).

compile_call(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    compile_values(
        maps:get(arguments, Fields),
        Context,
        [],
        fun(Arguments, CurrentContext, State1) ->
            compile_call_target(
                maps:get(op, Node),
                Fields,
                Arguments,
                CurrentContext,
                Continuation,
                State1
            )
        end,
        State0
    ).

compile_call_target(Operation, Fields, Arguments, Context, Continuation, State) ->
    Target = maps:get(target, Fields),
    case Target of
        {local, {Name, _Arity}} ->
            compile_local_call(
                Operation,
                Name,
                Arguments,
                Context,
                Continuation,
                State
            );
        {imported, Module, Name, _Arity} ->
            compile_imported_call(
                Operation,
                Module,
                Name,
                Arguments,
                Context,
                Continuation,
                State
            );
        _ ->
            compile_dynamic_call(
                Fields,
                Arguments,
                Context,
                Continuation,
                State
            )
    end.

compile_local_call(cps_call, Name, Arguments, Context, Continuation, State0) ->
    {RuntimeContinuation, State1} = ensure_continuation(Continuation, State0),
    Target = cerl:c_fname(private_name(cps, Name), length(Arguments) + 2),
    {
        cerl:c_apply(
            Target,
            Arguments ++ [Context, RuntimeContinuation]
        ),
        State1
    };
compile_local_call(_Operation, Name, Arguments, Context, Continuation, State) ->
    Target = cerl:c_fname(private_name(direct, Name), length(Arguments) + 1),
    Value = cerl:c_apply(Target, Arguments ++ [Context]),
    {continue(Value, Context, Continuation), State}.

compile_imported_call(cps_call, Module, Name, Arguments, Context,
        Continuation, State0) ->
    {RuntimeContinuation, State1} = ensure_continuation(Continuation, State0),
    Call = runtime_call(
        Module,
        private_name(cps, Name),
        Arguments ++ [Context, RuntimeContinuation]
    ),
    {Call, State1};
compile_imported_call(_Operation, Module, Name, Arguments, Context,
        Continuation, State) ->
    Value = runtime_call(
        Module,
        Name,
        Arguments
    ),
    {continue(Value, Context, Continuation), State}.

compile_dynamic_call(Fields, Arguments, Context, Continuation, State0) ->
    Function = maps:get(function, Fields),
    case compile_constructor_application(
        Function,
        Arguments,
        Context,
        Continuation,
        State0
    ) of
        {ok, Result} ->
            Result;
        not_constructor ->
            {CoreFunction, State1} = compile_direct_value(
                Function,
                Context,
                State0
            ),
            {RuntimeContinuation, State2} = ensure_continuation(
                Continuation,
                State1
            ),
            {
                runtime_call(
                    catena_effect_runtime,
                    apply_control,
                    [
                        CoreFunction,
                        core_list(Arguments),
                        Context,
                        RuntimeContinuation
                    ]
                ),
                State2
            }
    end.

compile_constructor_application(
    {var, Name, _Loc} = Source,
    Arguments,
    Context,
    Continuation,
    State
) ->
    case catena_codegen_utils:is_bound(Name, State) of
        true ->
            not_constructor;
        false ->
            case catena_codegen_utils:resolve_constructor(
                Name,
                length(Arguments),
                Source,
                State
            ) of
                {ok, _Callable} ->
                    Value = cerl:c_tuple([cerl:c_atom(Name) | Arguments]),
                    {ok, {continue(Value, Context, Continuation), State}};
                {error, Diagnostic} ->
                    case catena_codegen_utils:resolve_value(
                        Name,
                        Source,
                        State
                    ) of
                        {ok, #{kind := constructor}} -> throw(Diagnostic);
                        _ -> not_constructor
                    end
            end
    end;
compile_constructor_application(
    _Function,
    _Arguments,
    _Context,
    _Continuation,
    _State
) ->
    not_constructor.

compile_callable_value(
    #{kind := constructor, name := Name, arity := 0},
    _Context,
    State
) ->
    {cerl:c_tuple([cerl:c_atom(Name)]), State};
compile_callable_value(
    #{kind := constructor, name := Name, arity := Arity},
    _Context,
    State0
) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(Arity, State0),
    {
        cerl:c_fun(
            Arguments,
            cerl:c_tuple([cerl:c_atom(Name) | Arguments])
        ),
        State1
    };
compile_callable_value(
    #{kind := transform, name := Name, arity := Arity} = Callable,
    Context,
    State0
) ->
    Mode = callable_control_mode(Callable, State0),
    {Arguments, State1} = catena_codegen_utils:fresh_vars(Arity, State0),
    case Mode of
        direct ->
            Value = case maps:get(imported, Callable, false) of
                true ->
                    runtime_call(
                        callable_module(Callable),
                        Name,
                        Arguments
                    );
                false ->
                    cerl:c_apply(
                        cerl:c_fname(private_name(direct, Name), Arity + 1),
                        Arguments ++ [Context]
                    )
            end,
            {cerl:c_fun(Arguments, Value), State1};
        resumable ->
            control_closure(
                resumable,
                Arguments,
                fun(ClosureContext, Continuation) ->
                    compile_callable_target(
                        Callable,
                        Name,
                        resumable,
                        Arguments,
                        ClosureContext,
                        Continuation
                    )
                end,
                maps:get(location, Callable, {callable, Name}),
                State1
            )
    end.

compile_trait_callable_value(Name, Arity, Candidates, State0) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(Arity, State0),
    control_closure(
        resumable,
        Arguments,
        fun(Context, Continuation) ->
            runtime_call(
                catena_trait_runtime,
                invoke_control,
                [
                    cerl:abstract(Candidates),
                    cerl:c_atom(Name),
                    core_list(Arguments),
                    Context,
                    Continuation
                ]
            )
        end,
        {trait_dictionary_call, Name},
        State1
    ).

control_closure(Mode, Arguments, BodyBuilder, Origin, State0) ->
    {ArgumentList, State1} = catena_codegen_utils:fresh_var(State0),
    {Context, State2} = catena_codegen_utils:fresh_var(State1),
    {Continuation, State3} = catena_codegen_utils:fresh_var(State2),
    Body = BodyBuilder(Context, Continuation),
    Callable = cerl:c_fun(
        [ArgumentList, Context, Continuation],
        cerl:c_case(
            ArgumentList,
            [cerl:c_clause([core_list(Arguments)], Body)]
        )
    ),
    {
        runtime_call(
            catena_effect_runtime,
            control_closure,
            [cerl:c_atom(Mode), Callable, cerl:abstract(Origin)]
        ),
        State3
    }.

compile_callable_target(
    Callable,
    Name,
    direct,
    Arguments,
    Context,
    Continuation
) ->
    Value = case maps:get(imported, Callable, false) of
        true -> runtime_call(callable_module(Callable), Name, Arguments);
        false ->
            cerl:c_apply(
                cerl:c_fname(private_name(direct, Name),
                    length(Arguments) + 1),
                Arguments ++ [Context]
            )
    end,
    cerl:c_apply(Continuation, [Value, Context]);
compile_callable_target(
    Callable,
    Name,
    resumable,
    Arguments,
    Context,
    Continuation
) ->
    case maps:get(imported, Callable, false) of
        true ->
            runtime_call(
                callable_module(Callable),
                private_name(cps, Name),
                Arguments ++ [Context, Continuation]
            );
        false ->
            cerl:c_apply(
                cerl:c_fname(private_name(cps, Name),
                    length(Arguments) + 2),
                Arguments ++ [Context, Continuation]
            )
    end.

callable_control_mode(Callable, State) ->
    case maps:get(control_mode, Callable, unknown) of
        unknown ->
            case catena_codegen_utils:control_mode(
                maps:get(name, Callable),
                State
            ) of
                unknown -> resumable;
                Mode -> Mode
            end;
        Mode -> Mode
    end.

callable_module(Callable) ->
    case maps:find(runtime_module, Callable) of
        {ok, RuntimeModule} -> RuntimeModule;
        error ->
            case maps:find(source_module, Callable) of
                {ok, SourceModule} -> SourceModule;
                error -> maps:get(module, Callable)
            end
    end.

compile_closure(Node, Context, Continuation, State0) ->
    Fields = maps:get(fields, Node),
    Patterns = maps:get(parameters, Fields),
    {CorePatterns, State1} = lists:mapfoldl(
        fun catena_codegen_pattern:compile_pattern/2,
        State0,
        Patterns
    ),
    case maps:get(capability, Fields) of
        direct ->
            compile_direct_closure(
                Fields,
                Patterns,
                CorePatterns,
                Context,
                Continuation,
                State1
            );
        resumable ->
            compile_resumable_closure(
                Fields,
                Patterns,
                CorePatterns,
                Context,
                Continuation,
                maps:get(origin, maps:get(metadata, Node)),
                State1
            )
    end.

compile_direct_closure(
    Fields,
    Patterns,
    CorePatterns,
    Context,
    Continuation,
    State0
) ->
    {Arguments, State1} = catena_codegen_utils:fresh_vars(
        length(Patterns),
        State0
    ),
    {Body, State2} = catena_codegen_utils:with_bindings(
        lists:append([pattern_bindings(Pattern) || Pattern <- Patterns]),
        fun(ScopedState) ->
            compile_node(
                maps:get(body, Fields),
                Context,
                none,
                ScopedState
            )
        end,
        State1
    ),
    Closure = cerl:c_fun(
        Arguments,
        cerl:c_case(
            core_list(Arguments),
            [cerl:c_clause([core_list(CorePatterns)], Body)]
        )
    ),
    {continue(Closure, Context, Continuation), State2}.

compile_resumable_closure(
    Fields,
    Patterns,
    CorePatterns,
    Context,
    Continuation,
    Origin,
    State0
) ->
    {ArgumentList, State1} = catena_codegen_utils:fresh_var(State0),
    {ClosureContext, State2} = catena_codegen_utils:fresh_var(State1),
    {ClosureContinuation, State3} = catena_codegen_utils:fresh_var(State2),
    {Body, State4} = catena_codegen_utils:with_bindings(
        lists:append([pattern_bindings(Pattern) || Pattern <- Patterns]),
        fun(ScopedState) ->
            compile_node(
                maps:get(body, Fields),
                ClosureContext,
                ClosureContinuation,
                ScopedState
            )
        end,
        State3
    ),
    Callable = cerl:c_fun(
        [ArgumentList, ClosureContext, ClosureContinuation],
        cerl:c_case(
            ArgumentList,
            [cerl:c_clause([core_list(CorePatterns)], Body)]
        )
    ),
    Closure = runtime_call(
        catena_effect_runtime,
        control_closure,
        [cerl:c_atom(resumable), Callable, cerl:abstract(Origin)]
    ),
    {continue(Closure, Context, Continuation), State4}.

compile_dictionary_method(
    Unit,
    Dictionary,
    Name,
    Method,
    State0
) ->
    Identity = dictionary_method_identity(Dictionary, Name),
    case catena_selective_cps:lower_dictionary_closure(
        Unit,
        Identity,
        maps:get(lambda, Method)
    ) of
        {ok, ClosureNode} ->
            compile_closure(
                ClosureNode,
                runtime_call(catena_effect_runtime, empty_context, []),
                none,
                State0
            );
        {error, Reason} ->
            control_codegen_error(
                invalid_dictionary_control_lowering,
                #{
                    op => closure,
                    metadata => #{
                        origin => maps:get(location, Method, undefined),
                        transform => Identity
                    }
                },
                #{
                    trait => maps:get(trait, Dictionary),
                    method => Name,
                    reason => Reason
                }
            )
    end.

dictionary_method_identity(Dictionary, Name) ->
    list_to_atom(
        "$catena_dictionary$" ++
            atom_to_list(maps:get(trait, Dictionary)) ++
            "$" ++ atom_to_list(Name)
    ).

compile_values([], Context, Acc, Finish, State) ->
    Finish(lists:reverse(Acc), Context, State);
compile_values([Node | Rest], Context, Acc, Finish, State0) ->
    {ValueVar, State1} = catena_codegen_utils:fresh_var(State0),
    {RestoredContext, State2} = catena_codegen_utils:fresh_var(State1),
    {RestCore, State3} = compile_values(
        Rest,
        RestoredContext,
        [ValueVar | Acc],
        Finish,
        State2
    ),
    ValueContinuation = cerl:c_fun(
        [ValueVar, RestoredContext],
        RestCore
    ),
    compile_node(Node, Context, ValueContinuation, State3).

compile_guards([], State) ->
    {cerl:c_atom(true), State};
compile_guards([Guard | Rest], State0) ->
    Lowered = catena_codegen_lower:lower_expr(Guard),
    {First, State1} = catena_codegen_pattern:compile_guard(Lowered, State0),
    case Rest of
        [] -> {First, State1};
        _ ->
            {Remaining, State2} = compile_guards(Rest, State1),
            {
                runtime_call(erlang, 'andalso', [First, Remaining]),
                State2
            }
    end.

continue(Value, _Context, none) -> Value;
continue(Value, Context, Continuation) ->
    cerl:c_apply(Continuation, [Value, Context]).

ensure_continuation(none, State) ->
    identity_continuation(State);
ensure_continuation(Continuation, State) ->
    {Continuation, State}.

identity_continuation(State0) ->
    {Value, State1} = catena_codegen_utils:fresh_var(State0),
    {Context, State2} = catena_codegen_utils:fresh_var(State1),
    {cerl:c_fun([Value, Context], Value), State2}.

runtime_call(Module, Function, Arguments) ->
    cerl:c_call(
        cerl:c_atom(Module),
        cerl:c_atom(Function),
        Arguments
    ).

core_list(Values) ->
    lists:foldr(fun cerl:c_cons/2, cerl:c_nil(), Values).

core_map(Pairs) ->
    cerl:c_map([
        cerl:c_map_pair(cerl:c_atom(Key), Value)
        || {Key, Value} <- Pairs
    ]).

public_exports(Unit, IR) ->
    [
        cerl:c_fname(maps:get(name, Transform), maps:get(arity, Transform))
        || Transform <- exported_transforms(Unit, IR)
    ].

private_exports(Unit, IR) ->
    [
        cerl:c_fname(
            private_name(private_mode(Transform), maps:get(name, Transform)),
            private_arity(Transform)
        )
        || Transform <- exported_transforms(Unit, IR)
    ].

exported_transforms(Unit, IR) ->
    Transforms = catena_control_ir:transforms(IR),
    Requested = catena_compilation_unit:exports(Unit),
    ExportedNames = [
        Name
        || {export_transform, Name} <- Requested
    ],
    case Requested of
        [] -> Transforms;
        _ -> [
            Transform
            || Transform <- Transforms,
               lists:member(maps:get(name, Transform), ExportedNames)
        ]
    end.

private_mode(#{control_mode := direct}) -> direct;
private_mode(#{control_mode := resumable}) -> cps.

private_arity(#{arity := Arity, control_mode := direct}) -> Arity + 1;
private_arity(#{arity := Arity, control_mode := resumable}) -> Arity + 2.

pattern_bindings({pat_var, Name, _}) -> [Name];
pattern_bindings({pat_typed_var, Name, _, _}) -> [Name];
pattern_bindings({pat_constructor, _, Patterns, _}) ->
    lists:append([pattern_bindings(Pattern) || Pattern <- Patterns]);
pattern_bindings({pat_list, Patterns, _}) ->
    lists:append([pattern_bindings(Pattern) || Pattern <- Patterns]);
pattern_bindings({pat_cons, Head, Tail, _}) ->
    pattern_bindings(Head) ++ pattern_bindings(Tail);
pattern_bindings({pat_tuple, Patterns, _}) ->
    lists:append([pattern_bindings(Pattern) || Pattern <- Patterns]);
pattern_bindings({pat_as, Name, Pattern, _}) ->
    [Name | pattern_bindings(Pattern)];
pattern_bindings({pat_or, [Pattern | _], _}) -> pattern_bindings(Pattern);
pattern_bindings({pat_record, Fields, _}) ->
    lists:append([
        pattern_bindings(Pattern)
        || {_Field, Pattern} <- Fields
    ]);
pattern_bindings(_) -> [].

generated_name(Prefix, Index) ->
    list_to_atom("$catena_" ++ Prefix ++ "_" ++ integer_to_list(Index)).

control_codegen_error(Reason, Node, Extra) ->
    Metadata = maps:get(metadata, Node, #{}),
    Context = #{
        stage => control_core_lowering,
        construct => maps:get(op, Node, control_node),
        transform => maps:get(transform, Metadata, undefined),
        location => maps:get(origin, Metadata, undefined),
        source_term => Node
    },
    throw(catena_backend_error:invalid_control_ir(
        {Reason, Extra},
        Context
    )).
