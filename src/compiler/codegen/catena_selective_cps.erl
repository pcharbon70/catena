%%%-------------------------------------------------------------------
%%% @doc Lower typed normalized Catena AST into selective-CPS control IR.
%%%
%%% Direct expressions remain explicit direct nodes. Resumable regions carry
%%% deterministic delimiter and continuation identities, while bind/match
%%% nodes preserve source evaluation order, patterns, guards, fallthrough,
%%% and origins. This pass constructs compiler IR only; it does not provide
%%% runtime resumption execution.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_selective_cps).

-export([lower/1, lower_dictionary_closure/3]).

-spec lower(catena_compilation_unit:t()) ->
    {ok, catena_control_ir:ir()} | {error, term()}.
lower(Unit) ->
    case catena_compilation_unit:is_compilation_unit(Unit) of
        true ->
            {module, Module, _Exports, _Imports, Declarations, Origin} =
                catena_compilation_unit:normalized_ast(Unit),
            Modes = catena_compilation_unit:control_modes(Unit),
            Typed = catena_compilation_unit:typed_declarations(Unit),
            TypedByName = typed_by_name(Typed),
            State0 = #{next_delimiter => 1, next_continuation => 1},
            case lower_transforms(
                Declarations,
                Modes,
                TypedByName,
                catena_compilation_unit:import_resolution(Unit),
                catena_compilation_unit:callables(Unit),
                State0,
                []
            ) of
                {ok, Transforms, _State} ->
                    catena_control_ir:new(Module, Transforms, Origin);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {invalid_control_ir, unchecked_compilation_unit}}
    end.

%% @doc Lower an instance method through the same expression and callable
%% inventory as source transforms. Dictionary entries are conservatively
%% resumable because their concrete caller and higher-order arguments are
%% selected dynamically at runtime.
-spec lower_dictionary_closure(
    catena_compilation_unit:t(),
    atom(),
    term()
) -> {ok, catena_control_ir:node()} | {error, term()}.
lower_dictionary_closure(
    Unit,
    Identity,
    {lambda, Patterns, Body, Origin}
) ->
    Context = #{
        transform => Identity,
        mode => resumable,
        type => unknown,
        callable_type => unknown,
        effect_row => {teffectrow, [], open},
        delimiter => none,
        continuation => {continuation, Identity, 0},
        modes => catena_compilation_unit:control_modes(Unit),
        import_resolution =>
            catena_compilation_unit:import_resolution(Unit),
        callables => catena_compilation_unit:callables(Unit),
        evidence => []
    },
    State0 = #{next_delimiter => 1, next_continuation => 1},
    case lower_closure(Patterns, Body, Origin, Context, State0) of
        {ok, Node, _State} ->
            Fields = maps:get(fields, Node),
            Metadata = maps:get(metadata, Node),
            {ok, Node#{
                fields => Fields#{capability => resumable},
                metadata => Metadata#{
                    continuation_arity => 1,
                    runtime_disposition => requires_resumption_runtime
                }
            }};
        {error, _} = Error ->
            Error
    end;
lower_dictionary_closure(_Unit, _Identity, Lambda) ->
    {error, {invalid_dictionary_control_closure, Lambda}}.

lower_transforms([], _Modes, _Typed, _Imports, _Callables, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_transforms(
    [
        {transform_decl, Name, _DeclaredType, Clauses, Origin}
        | Rest
    ],
    Modes,
    Typed,
    ImportResolution,
    Callables,
    State,
    Acc
) when Clauses =/= [] ->
    {ok, ModeEntry} = catena_control_mode:lookup(Name, Modes),
    TypedDeclaration = maps:get(Name, Typed, undefined),
    Type = maps:get(type, ModeEntry),
    EffectRow = maps:get(effect_row, ModeEntry),
    Mode = maps:get(mode, ModeEntry),
    Evidence = resumption_evidence(TypedDeclaration),
    Entry = catena_control_abi:entry_shape(
        Name,
        length(hd_patterns(Clauses)),
        Mode
    ),
    Context = #{
        transform => Name,
        mode => Mode,
        type => transform_result_type(Type),
        callable_type => Type,
        effect_row => EffectRow,
        delimiter => none,
        continuation => {continuation, Name, 0},
        modes => Modes,
        import_resolution => ImportResolution,
        callables => Callables,
        evidence => Evidence
    },
    case lower_clauses(Clauses, Context, State, []) of
        {ok, LoweredClauses, State1} ->
            Transform = #{
                name => Name,
                arity => length(hd_patterns(Clauses)),
                control_mode => Mode,
                entry => Entry,
                final_continuation =>
                    catena_control_abi:final_continuation(
                        Name,
                        maps:get(type, Context),
                        Origin
                    ),
                clauses => LoweredClauses,
                type => Type,
                effect_row => EffectRow,
                origin => Origin
            },
            lower_transforms(
                Rest,
                Modes,
                Typed,
                ImportResolution,
                Callables,
                State1,
                [Transform | Acc]
            );
        {error, _} = Error ->
            Error
    end;
lower_transforms(
    [_Declaration | Rest],
    Modes,
    Typed,
    ImportResolution,
    Callables,
    State,
    Acc
) ->
    lower_transforms(
        Rest,
        Modes,
        Typed,
        ImportResolution,
        Callables,
        State,
        Acc
    ).

lower_clauses([], _Context, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_clauses(
    [
        {transform_clause, Patterns, Guards, Body, Origin}
        | Rest
    ],
    Context,
    State,
    Acc
) ->
    case lower_expr(Body, Context, State) of
        {ok, BodyIR, State1} ->
            {ok, ReturnIR} = catena_control_ir:node(
                return,
                metadata(
                    Context,
                    Origin,
                    continuation_arity(Context),
                    cps_or_direct(Context)
                ),
                #{
                    value => BodyIR,
                    continuation => maps:get(continuation, Context),
                    tail_position => true
                }
            ),
            Clause = #{
                patterns => Patterns,
                guards => preserve_guards(Guards),
                body => ReturnIR,
                failure => clause_fallthrough,
                origin => Origin
            },
            lower_clauses(
                Rest,
                Context,
                State1,
                [Clause | Acc]
            );
        {error, _} = Error ->
            Error
    end.

lower_expr(
    {let_expr, [Pattern, Value], Body, Origin},
    Context,
    State
) ->
    {Continuation, State1} = fresh_continuation(
        maps:get(transform, Context),
        State
    ),
    ValueContext = Context#{continuation => Continuation},
    case lower_expr(Value, ValueContext, State1) of
        {ok, ValueIR, State2} ->
            case lower_expr(Body, Context, State2) of
                {ok, BodyIR, State3} ->
                    make_node(
                        bind,
                        metadata(Context, Origin, 1, cps_or_direct(Context)),
                        #{
                            pattern => Pattern,
                            value => ValueIR,
                            continuation => Continuation,
                            body => BodyIR,
                            evaluation => exactly_once
                        },
                        State3
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
lower_expr(
    {let_expr, Bindings, Body, Origin},
    Context,
    State
) when is_list(Bindings) ->
    lower_bindings(Bindings, Body, Origin, Context, State);
lower_expr(
    {match_expr, Scrutinee, Clauses, Origin},
    Context,
    State
) ->
    case lower_expr(Scrutinee, Context, State) of
        {ok, ScrutineeIR, State1} ->
            case lower_match_clauses(
                Clauses,
                Context,
                State1,
                []
            ) of
                {ok, ClauseIR, State2} ->
                    make_node(
                        match,
                        metadata(
                            Context,
                            Origin,
                            continuation_arity(Context),
                            cps_or_direct(Context)
                        ),
                        #{
                            scrutinee => ScrutineeIR,
                            clauses => ClauseIR,
                            fallthrough => match_failure,
                            evaluation => source_order
                        },
                        State2
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
lower_expr(
    {handle_expr, Body, Handlers, Origin},
    Context,
    State
) ->
    lower_expr(
        {
            handle_expr,
            catena_resumption_mode:default(Origin),
            Body,
            Handlers,
            Origin
        },
        Context,
        State
    );
lower_expr(
    {handle_expr, HandlerMode, Body, Handlers, Origin},
    Context,
    State
) ->
    {Delimiter, State1} = fresh_delimiter(
        maps:get(transform, Context),
        State
    ),
    {Continuation, State2} = fresh_continuation(
        maps:get(transform, Context),
        State1
    ),
    InnerContext = Context#{
        mode => resumable,
        delimiter => Delimiter,
        continuation => Continuation,
        handler_mode => HandlerMode
    },
    case lower_expr(Body, InnerContext, State2) of
        {ok, BodyIR, State3} ->
            case lower_handlers(
                Handlers,
                InnerContext,
                State3,
                []
            ) of
                {ok, HandlerIR, State4} ->
                    {ok, Install} = catena_control_ir:node(
                        install_handler,
                        metadata(
                            InnerContext,
                            Origin,
                            1,
                            requires_resumption_runtime
                        ),
                        #{
                            delimiter => Delimiter,
                            depth =>
                                catena_resumption_mode:depth(HandlerMode),
                            kind =>
                                catena_resumption_mode:kind(HandlerMode),
                            handlers => HandlerIR,
                            body => BodyIR
                        }
                    ),
                    make_node(
                        delimiter,
                        metadata(
                            InnerContext,
                            Origin,
                            1,
                            requires_resumption_runtime
                        ),
                        #{
                            identity => Delimiter,
                            continuation => Continuation,
                            depth =>
                                catena_resumption_mode:depth(HandlerMode),
                            kind =>
                                catena_resumption_mode:kind(HandlerMode),
                            body => Install
                        },
                        State4
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
lower_expr(
    {perform_expr, Effect, Operation, Arguments, Origin},
    Context,
    State
) ->
    case lower_exprs(Arguments, Context, State, []) of
        {ok, ArgumentIR, State1} ->
            case maps:get(delimiter, Context) of
                none ->
                    make_node(
                        perform,
                        metadata(
                            Context,
                            Origin,
                            0,
                            provider_runtime
                        ),
                        #{
                            effect => Effect,
                            operation => Operation,
                            arguments => ArgumentIR,
                            suspension => false
                        },
                        State1
                    );
                Delimiter ->
                    HandlerMode = maps:get(
                        handler_mode,
                        Context,
                        catena_resumption_mode:default(Origin)
                    ),
                    {ok, Resumption} = catena_control_ir:node(
                        make_resumption,
                        metadata(
                            Context,
                            Origin,
                            1,
                            requires_resumption_runtime
                        ),
                        #{
                            kind => catena_resumption_mode:kind(HandlerMode),
                            depth => catena_resumption_mode:depth(HandlerMode),
                            delimiter => Delimiter,
                            continuation =>
                                maps:get(continuation, Context),
                            remainder => {
                                continuation_ref,
                                maps:get(continuation, Context)
                            }
                        }
                    ),
                    make_node(
                        perform,
                        metadata(
                            Context,
                            Origin,
                            1,
                            requires_resumption_runtime
                        ),
                        #{
                            effect => Effect,
                            operation => Operation,
                            arguments => ArgumentIR,
                            suspension => true,
                            resumption => Resumption
                        },
                        State1
                    )
            end;
        {error, _} = Error ->
            Error
    end;
lower_expr(
    {resume_expr, Target, Value, Origin},
    Context,
    State
) ->
    case lower_expr(Target, Context, State) of
        {ok, TargetIR, State1} ->
            case lower_expr(Value, Context, State1) of
                {ok, ValueIR, State2} ->
                    Evidence = resume_evidence(Target, Origin, Context),
                    ResumeContext = Context#{
                        type => resume_result_type(
                            maps:get(type, Evidence,
                                maps:get(type, Context))
                        )
                    },
                    make_node(
                        resume,
                        metadata(
                            ResumeContext,
                            Origin,
                            1,
                            requires_resumption_runtime
                        ),
                        #{
                            target => TargetIR,
                            value => ValueIR,
                            authority => Evidence,
                            delimiter => resume_delimiter(Context)
                        },
                        State2
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
lower_expr(
    {binary_op, pipe_right, Left, {app, Function, Arguments, _}, Origin},
    Context,
    State
) ->
    lower_expr(
        {app, Function, [Left | Arguments], Origin},
        Context,
        State
    );
lower_expr(
    {binary_op, pipe_right, Left, Function, Origin},
    Context,
    State
) ->
    lower_expr({app, Function, [Left], Origin}, Context, State);
lower_expr({app, _Function, _Arguments, _Origin} = Application, Context,
        State) ->
    {Function, Arguments, Origin} = application_spine(Application),
    case lower_exprs(Arguments, Context, State, []) of
        {ok, ArgumentIR, State1} ->
            lower_call(
                Function,
                ArgumentIR,
                Origin,
                Context,
                State1
            );
        {error, _} = Error ->
            Error
    end;
lower_expr({lambda, Patterns, Body, Origin}, Context, State) ->
    lower_closure(Patterns, Body, Origin, Context, State);
lower_expr({lam, Parameter, Body}, Context, State) ->
    lower_closure([Parameter], Body, source_location(Body), Context, State);
lower_expr(Expression, Context, State) ->
    case contains_nested_control(Expression) orelse
        (maps:get(mode, Context) =:= resumable andalso
            contains_application(Expression))
    of
        true ->
            case lower_nested_children(Expression, Context, State) of
                {ok, Lowered, State1} ->
                    make_node(
                        direct_expr,
                        metadata(
                            Context,
                            source_location(Expression),
                            0,
                            cps_or_direct(Context)
                        ),
                        #{
                            source => Lowered,
                            evaluation => source_order
                        },
                        State1
                    );
                {error, _} = Error ->
                    Error
            end;
        false ->
            make_node(
                direct_expr,
                metadata(
                    Context,
                    source_location(Expression),
                    0,
                    direct
                ),
                #{source => Expression, evaluation => source_order},
                State
            )
    end.

lower_bindings([], Body, _Origin, Context, State) ->
    lower_expr(Body, Context, State);
lower_bindings(
    [{Pattern, Value} | Rest],
    Body,
    Origin,
    Context,
    State
) ->
    NestedBody = case Rest of
        [] -> Body;
        _ -> {let_expr, Rest, Body, Origin}
    end,
    lower_expr(
        {let_expr, [Pattern, Value], NestedBody, Origin},
        Context,
        State
    ).

lower_match_clauses([], _Context, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_match_clauses(
    [
        {match_clause, Pattern, Guards, Body, Origin}
        | Rest
    ],
    Context,
    State,
    Acc
) ->
    case lower_expr(Body, Context, State) of
        {ok, BodyIR, State1} ->
            Clause = #{
                pattern => Pattern,
                guards => preserve_guards(Guards),
                body => BodyIR,
                failure => next_clause,
                origin => Origin
            },
            lower_match_clauses(
                Rest,
                Context,
                State1,
                [Clause | Acc]
            );
        {error, _} = Error ->
            Error
    end.

lower_handlers([], _Context, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_handlers(
    [
        {handler_clause, Effect, Operations, Origin}
        | Rest
    ],
    Context,
    State,
    Acc
) ->
    case lower_operations(Operations, Context, State, []) of
        {ok, OperationIR, State1} ->
            Handler = #{
                effect => Effect,
                operations => OperationIR,
                origin => Origin
            },
            lower_handlers(
                Rest,
                Context,
                State1,
                [Handler | Acc]
            );
        {error, _} = Error ->
            Error
    end.

lower_operations([], _Context, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_operations(
    [
        {
            operation_case,
            Operation,
            Patterns,
            {resumption_binder, Binder, BinderOrigin},
            Body,
            Origin
        }
        | Rest
    ],
    Context,
    State,
    Acc
) ->
    case lower_expr(Body, Context, State) of
        {ok, BodyIR0, State1} ->
            BodyIR = case contains_resume(Body) of
                true ->
                    BodyIR0;
                false ->
                    {ok, Abort} = catena_control_ir:node(
                        abort,
                        metadata(
                            Context,
                            Origin,
                            0,
                            requires_resumption_runtime
                        ),
                        #{
                            delimiter => maps:get(delimiter, Context),
                            result => BodyIR0,
                            reason => handler_returned_without_resume
                        }
                    ),
                    Abort
            end,
            OperationIR = #{
                operation => Operation,
                patterns => Patterns,
                binder => Binder,
                binder_origin => BinderOrigin,
                authority => binder_evidence(Binder, Context),
                body => BodyIR,
                origin => Origin
            },
            lower_operations(
                Rest,
                Context,
                State1,
                [OperationIR | Acc]
            );
        {error, _} = Error ->
            Error
    end.

lower_call(Function, Arguments, Origin, Context, State) ->
    Root = application_root(Function),
    CallerMode = maps:get(mode, Context),
    {Target, CalleeMode, Capability} = call_capability(
        Root,
        length(Arguments),
        Context
    ),
    Operation = case {CallerMode, CalleeMode} of
        {direct, direct} -> direct_call;
        {resumable, resumable} -> cps_call;
        {resumable, direct} -> bridge;
        {direct, resumable} -> bridge
    end,
    Fields0 = #{
        target => Target,
        function => Function,
        arguments => Arguments,
        capability => Capability,
        closure => closure_shape(
            Target,
            length(Arguments),
            CalleeMode,
            Origin
        ),
        evaluation => left_to_right
    },
    {Disposition, Fields} = case catena_control_abi:bridge(
        CallerMode,
        CalleeMode,
        missing,
        Target,
        Origin
    ) of
        {ok, none} when CalleeMode =:= resumable ->
            {requires_resumption_runtime, Fields0};
        {ok, none} ->
            {direct, Fields0};
        {ok, Bridge} ->
            {
                direct_to_cps_bridge,
                Fields0#{
                    bridge => maps:get(kind, Bridge),
                    proof => maps:get(proof, Bridge),
                    bridge_evidence => Bridge
                }
            };
        {error, {resumption_abi_mismatch, Evidence}} ->
            {
                unresolved_mode_bridge,
                Fields0#{
                    bridge => resumable_to_direct,
                    proof => missing,
                    bridge_evidence => Evidence
                }
            }
    end,
    make_node(
        Operation,
        metadata(
            Context,
            Origin,
            case CalleeMode of resumable -> 1; direct -> 0 end,
            Disposition
        ),
        Fields,
        State
    ).

lower_closure(Patterns, Body, Origin, Context, State) ->
    case lower_expr(Body, Context, State) of
        {ok, BodyIR, State1} ->
            Capability = closure_capability(BodyIR),
            make_node(
                closure,
                metadata(
                    Context,
                    Origin,
                    case Capability of direct -> 0; resumable -> 1 end,
                    case Capability of
                        direct -> direct;
                        resumable -> requires_resumption_runtime
                    end
                ),
                #{
                    parameters => Patterns,
                    body => BodyIR,
                    capability => Capability
                },
                State1
            );
        {error, _} = Error ->
            Error
    end.

closure_capability(Node) ->
    case closure_requires_cps(Node) of
        true -> resumable;
        false -> direct
    end.

closure_requires_cps(#{op := Operation, fields := Fields}) ->
    Local = case Operation of
        delimiter -> true;
        install_handler -> true;
        resume -> true;
        abort -> true;
        cps_call -> true;
        perform -> maps:get(suspension, Fields, false);
        bridge -> maps:get(capability, Fields, direct) =:= resumable;
        closure -> maps:get(capability, Fields, direct) =:= resumable;
        _ -> false
    end,
    Local orelse lists:any(
        fun closure_requires_cps/1,
        maps:values(Fields)
    );
closure_requires_cps(Terms) when is_list(Terms) ->
    lists:any(fun closure_requires_cps/1, Terms);
closure_requires_cps(Term) when is_tuple(Term) ->
    closure_requires_cps(tuple_to_list(Term));
closure_requires_cps(_Term) ->
    false.

lower_exprs([], _Context, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_exprs([Expression | Rest], Context, State, Acc) ->
    case lower_expr(Expression, Context, State) of
        {ok, ExpressionIR, State1} ->
            lower_exprs(
                Rest,
                Context,
                State1,
                [ExpressionIR | Acc]
            );
        {error, _} = Error ->
            Error
    end.

make_node(Operation, Metadata, Fields, State) ->
    case catena_control_ir:node(Operation, Metadata, Fields) of
        {ok, Node} -> {ok, Node, State};
        {error, _} = Error -> Error
    end.

metadata(Context, Origin, ContinuationArity, Disposition) ->
    #{
        value_type => maps:get(type, Context, unknown),
        effect_row => maps:get(
            effect_row,
            Context,
            {teffectrow, [], closed}
        ),
        control_mode => maps:get(mode, Context),
        delimiter => maps:get(delimiter, Context, none),
        continuation_arity => ContinuationArity,
        runtime_disposition => Disposition,
        origin => Origin,
        transform => maps:get(transform, Context, undefined)
    }.

call_capability(
    {var, Name, _Origin},
    Arity,
    Context
) ->
    Modes = maps:get(modes, Context),
    ImportResolution = maps:get(import_resolution, Context),
    case catena_control_mode:lookup(Name, Modes) of
        {ok, Entry} ->
            {
                {local, maps:get(identity, Entry)},
                maps:get(mode, Entry),
                maps:get(mode, Entry)
            };
        none ->
            case local_constructor(
                Name,
                Arity,
                maps:get(callables, Context)
            ) of
                true ->
                    {{dynamic, Name}, direct, direct};
                false ->
                    imported_or_dynamic_capability(
                        Name,
                        Arity,
                        ImportResolution
                    )
            end
    end;
call_capability(
    {imported_ref, #{kind := constructor} = Entry, _Origin},
    _Arity,
    _Context
) ->
    {
        {imported_constructor,
            maps:get(source_module, Entry, undefined),
            maps:get(name, Entry, undefined),
            maps:get(arity, Entry, undefined)},
        direct,
        direct
    };
call_capability(
    {imported_ref, Entry, _Origin},
    _Arity,
    _Context
) ->
    Mode = maps:get(control_mode, Entry, resumable),
    {
        {imported,
            maps:get(source_module, Entry, undefined),
            maps:get(name, Entry, undefined),
            maps:get(arity, Entry, undefined)},
        Mode,
        Mode
    };
call_capability(_Function, _Arity, _Context) ->
    {dynamic_callable, resumable, resumable}.

imported_or_dynamic_capability(Name, Arity, ImportResolution) ->
    case resolved_import(Name, Arity, ImportResolution) of
        {ok, #{kind := constructor} = Imported} ->
            {
                {imported_constructor,
                    maps:get(source_module, Imported),
                    maps:get(name, Imported),
                    maps:get(arity, Imported)},
                direct,
                direct
            };
                {ok, Imported} ->
                    Mode = maps:get(
                        control_mode,
                        Imported,
                        resumable
                    ),
                    {
                        {imported,
                            maps:get(source_module, Imported),
                            maps:get(name, Imported),
                            maps:get(arity, Imported)},
                        Mode,
                        Mode
                    };
                none ->
                    case imported_constructor_binding(
                        Name,
                        ImportResolution
                    ) of
                        true -> {{dynamic, Name}, direct, direct};
                        false -> {{dynamic, Name}, resumable, resumable}
                    end
    end.

local_constructor(Name, _Arity, Callables) ->
    lists:any(
        fun(Callable) ->
            maps:get(kind, Callable) =:= constructor
        end,
        catena_call_resolution:lookup(Name, Callables)
    ).

imported_constructor_binding(Name, Resolution) ->
    lists:any(
        fun(Entry) ->
            maps:get(kind, Entry) =:= constructor andalso
                maps:get(binding, Entry, undefined) =:= Name
        end,
        catena_import_resolution:entries(Resolution)
    ).

resolved_import(Name, Arity, Resolution) ->
    Matches = [
        Entry
        || Entry <- catena_import_resolution:entries(Resolution),
           lists:member(
               maps:get(kind, Entry),
               [transform, constructor]
           ),
           maps:get(binding, Entry, undefined) =:= Name,
           maps:get(arity, Entry) =:= Arity
    ],
    case Matches of
        [Entry] -> {ok, Entry};
        _ -> none
    end.

resume_delimiter(#{delimiter := none}) ->
    from_resumption_authority;
resume_delimiter(Context) ->
    maps:get(delimiter, Context).

contains_nested_control(Term) when is_tuple(Term), tuple_size(Term) > 0 ->
    case element(1, Term) of
        handle_expr -> true;
        perform_expr -> true;
        resume_expr -> true;
        _ -> contains_nested_control(tuple_to_list(Term))
    end;
contains_nested_control(Terms) when is_list(Terms) ->
    lists:any(fun contains_nested_control/1, Terms);
contains_nested_control(_) ->
    false.

contains_application(Term) when is_tuple(Term), tuple_size(Term) > 0 ->
    case element(1, Term) of
        app -> true;
        _ -> contains_application(tuple_to_list(Term))
    end;
contains_application(Terms) when is_list(Terms) ->
    lists:any(fun contains_application/1, Terms);
contains_application(_) ->
    false.

lower_nested_children(Term, Context, State) when is_tuple(Term) ->
    case is_lowerable_expression(Term) of
        true ->
            lower_expr(Term, Context, State);
        false ->
            case lower_nested_terms(
                tuple_to_list(Term),
                Context,
                State,
                []
            ) of
                {ok, Elements, State1} ->
                    {ok, list_to_tuple(Elements), State1};
                {error, _} = Error ->
                    Error
            end
    end;
lower_nested_children(Terms, Context, State) when is_list(Terms) ->
    lower_nested_terms(Terms, Context, State, []);
lower_nested_children(Term, _Context, State) ->
    {ok, Term, State}.

lower_nested_terms([], _Context, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_nested_terms([Term | Rest], Context, State, Acc) ->
    case lower_nested_children(Term, Context, State) of
        {ok, Lowered, State1} ->
            lower_nested_terms(
                Rest,
                Context,
                State1,
                [Lowered | Acc]
            );
        {error, _} = Error ->
            Error
    end.

is_lowerable_expression({binary_op, pipe_right, _, _, _}) ->
    true;
is_lowerable_expression(Term) ->
    lists:member(
        element(1, Term),
        [
            let_expr,
            match_expr,
            handle_expr,
            perform_expr,
            resume_expr,
            app,
            lambda,
            lam
        ]
    ).

closure_shape({local, {_Name, Arity}} = Target, _AppliedArity, Mode, Origin) ->
    catena_control_abi:closure_shape(
        local,
        Target,
        Arity,
        Mode,
        Origin
    );
closure_shape(
    {imported, _Module, _Name, Arity} = Target,
    _AppliedArity,
    Mode,
    Origin
) when is_integer(Arity) ->
    catena_control_abi:closure_shape(
        imported,
        Target,
        Arity,
        Mode,
        Origin
    );
closure_shape(Target, AppliedArity, Mode, Origin) ->
    Kind = case Target of
        {dynamic, Name} ->
            case catena_trait_resolve:is_trait_method(Name) of
                true -> trait_dictionary;
                false -> higher_order
            end;
        _ ->
            higher_order
    end,
    catena_control_abi:closure_shape(
        Kind,
        Target,
        AppliedArity,
        Mode,
        Origin
    ).

resumption_evidence(
    {typed_transform, _Name, _Type, _Clauses, Metadata, _Origin}
) ->
    maps:get(resumptions, Metadata, []);
resumption_evidence(_TypedDeclaration) ->
    [].

binder_evidence(Binder, Context) ->
    first_evidence(
        fun(Evidence) ->
            maps:get(kind, Evidence, undefined) =:=
                resumption_binder andalso
                maps:get(binder, Evidence, undefined) =:= Binder
        end,
        maps:get(evidence, Context),
        #{binder => Binder}
    ).

resume_evidence(Target, Origin, Context) ->
    first_evidence(
        fun(Evidence) ->
            maps:get(kind, Evidence, undefined) =:= resume andalso
                maps:get(resume_location, Evidence, undefined) =:= Origin
        end,
        maps:get(evidence, Context),
        resume_target_evidence(Target, Context)
    ).

resume_target_evidence({var, Binder, _Origin}, Context) ->
    first_evidence(
        fun(Evidence) ->
            maps:get(kind, Evidence, undefined) =:= resume andalso
                (
                    maps:get(binder, Evidence, undefined) =:= Binder orelse
                        maps:get(target, Evidence, undefined) =:= Binder
                )
        end,
        maps:get(evidence, Context),
        binder_evidence(Binder, Context)
    );
resume_target_evidence(_Target, _Context) ->
    #{}.

first_evidence(_Predicate, [], Default) ->
    Default;
first_evidence(Predicate, [Evidence | Rest], Default) ->
    case Predicate(Evidence) of
        true -> Evidence;
        false -> first_evidence(Predicate, Rest, Default)
    end.

resume_result_type(
    {tresumption, _Kind, _Input, Output, _Effects}
) ->
    Output;
resume_result_type(_Type) ->
    unknown.

transform_result_type({tfun, _Input, Output, _Effects}) ->
    transform_result_type(Output);
transform_result_type(Type) ->
    Type.

typed_by_name(TypedDeclarations) ->
    maps:from_list([
        {element(2, Declaration), Declaration}
        || Declaration <- TypedDeclarations,
           is_typed_transform(Declaration)
    ]).

is_typed_transform({typed_transform, _, _, _, _}) -> true;
is_typed_transform({typed_transform, _, _, _, _, _}) -> true;
is_typed_transform(_) -> false.

hd_patterns([
    {transform_clause, Patterns, _Guards, _Body, _Origin}
    | _
]) ->
    Patterns.

preserve_guards(undefined) -> [];
preserve_guards(Guards) -> Guards.

contains_resume({resume_expr, _, _, _}) ->
    true;
contains_resume(Term) when is_tuple(Term) ->
    contains_resume(tuple_to_list(Term));
contains_resume(Terms) when is_list(Terms) ->
    lists:any(fun contains_resume/1, Terms);
contains_resume(_) ->
    false.

application_root({app, Function, _Arguments, _Origin}) ->
    application_root(Function);
application_root(Function) ->
    Function.

application_spine({app, Function, Arguments, Origin}) ->
    case Function of
        {app, _, [], _} ->
            {Function, Arguments, Origin};
        {app, _, _, _} ->
            {Root, EarlierArguments, _EarlierOrigin} =
                application_spine(Function),
            {Root, EarlierArguments ++ Arguments, Origin};
        _ ->
            {Function, Arguments, Origin}
    end.

fresh_delimiter(Transform, #{next_delimiter := Next} = State) ->
    {
        {delimiter, Transform, Next},
        State#{next_delimiter := Next + 1}
    }.

fresh_continuation(
    Transform,
    #{next_continuation := Next} = State
) ->
    {
        {continuation, Transform, Next},
        State#{next_continuation := Next + 1}
    }.

continuation_arity(#{mode := resumable}) -> 1;
continuation_arity(_Context) -> 0.

cps_or_direct(#{mode := resumable}) ->
    requires_resumption_runtime;
cps_or_direct(_Context) ->
    direct.

source_location(Term) when is_tuple(Term), tuple_size(Term) > 1 ->
    Candidate = element(tuple_size(Term), Term),
    case is_location(Candidate) of
        true -> Candidate;
        false -> undefined
    end;
source_location(_Term) ->
    undefined.

is_location({location, _, _}) -> true;
is_location({location, _, _, _, _}) -> true;
is_location({line, _}) -> true;
is_location({synthetic, _, _}) -> true;
is_location(_) -> false.
