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

-export([lower/1]).

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

lower_transforms([], _Modes, _Typed, State, Acc) ->
    {ok, lists:reverse(Acc), State};
lower_transforms(
    [
        {transform_decl, Name, _DeclaredType, Clauses, Origin}
        | Rest
    ],
    Modes,
    Typed,
    State,
    Acc
) when Clauses =/= [] ->
    {ok, ModeEntry} = catena_control_mode:lookup(Name, Modes),
    TypedDeclaration = maps:get(Name, Typed, undefined),
    Type = maps:get(type, ModeEntry),
    EffectRow = maps:get(effect_row, ModeEntry),
    Mode = maps:get(mode, ModeEntry),
    Evidence = resumption_evidence(TypedDeclaration),
    Entry = entry_shape(Name, length(hd_patterns(Clauses)), Mode),
    Context = #{
        transform => Name,
        mode => Mode,
        type => transform_result_type(Type),
        callable_type => Type,
        effect_row => EffectRow,
        delimiter => none,
        continuation => {continuation, Name, 0},
        modes => Modes,
        evidence => Evidence
    },
    case lower_clauses(Clauses, Context, State, []) of
        {ok, LoweredClauses, State1} ->
            Transform = #{
                name => Name,
                arity => length(hd_patterns(Clauses)),
                control_mode => Mode,
                entry => Entry,
                clauses => LoweredClauses,
                type => Type,
                effect_row => EffectRow,
                origin => Origin
            },
            lower_transforms(
                Rest,
                Modes,
                Typed,
                State1,
                [Transform | Acc]
            );
        {error, _} = Error ->
            Error
    end;
lower_transforms([_Declaration | Rest], Modes, Typed, State, Acc) ->
    lower_transforms(Rest, Modes, Typed, State, Acc).

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
        continuation => Continuation
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
                            depth => deep,
                            kind => one_shot,
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
                    {ok, Resumption} = catena_control_ir:node(
                        make_resumption,
                        metadata(
                            Context,
                            Origin,
                            1,
                            requires_resumption_runtime
                        ),
                        #{
                            kind => one_shot,
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
                    Evidence = resume_evidence(Target, Context),
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
                            delimiter =>
                                maps:get(delimiter, Context)
                        },
                        State2
                    );
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
lower_expr({app, Function, Arguments, Origin}, Context, State) ->
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
    ).

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
        maps:get(modes, Context)
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
        evaluation => left_to_right
    },
    {Disposition, Fields} = case {CallerMode, CalleeMode} of
        {resumable, direct} ->
            {
                direct_to_cps_bridge,
                Fields0#{
                    bridge => direct_to_cps,
                    proof => direct_callee
                }
            };
        {direct, resumable} ->
            {
                unresolved_mode_bridge,
                Fields0#{
                    bridge => resumable_to_direct,
                    proof => missing
                }
            };
        {resumable, resumable} ->
            {requires_resumption_runtime, Fields0};
        {direct, direct} ->
            {direct, Fields0}
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
            make_node(
                closure,
                metadata(
                    Context,
                    Origin,
                    continuation_arity(Context),
                    cps_or_direct(Context)
                ),
                #{
                    parameters => Patterns,
                    body => BodyIR,
                    capability => maps:get(mode, Context)
                },
                State1
            );
        {error, _} = Error ->
            Error
    end.

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
        origin => Origin
    }.

entry_shape(Name, Arity, direct) ->
    #{
        public => {Name, Arity},
        private => {direct, Name, Arity + 1},
        context_arity => 1,
        continuation_arity => 0
    };
entry_shape(Name, Arity, resumable) ->
    #{
        public => {Name, Arity},
        private => {cps, Name, Arity + 2},
        context_arity => 1,
        continuation_arity => 1
    }.

call_capability({var, Name, _Origin}, Modes) ->
    case catena_control_mode:lookup(Name, Modes) of
        {ok, Entry} ->
            {
                {local, maps:get(identity, Entry)},
                maps:get(mode, Entry),
                maps:get(mode, Entry)
            };
        none ->
            {{dynamic, Name}, resumable, resumable}
    end;
call_capability({imported_ref, Entry, _Origin}, _Modes) ->
    Mode = maps:get(control_mode, Entry, resumable),
    {
        {imported,
            maps:get(source_module, Entry, undefined),
            maps:get(name, Entry, undefined),
            maps:get(arity, Entry, undefined)},
        Mode,
        Mode
    };
call_capability(_Function, _Modes) ->
    {dynamic_callable, resumable, resumable}.

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

resume_evidence({var, Binder, _Origin}, Context) ->
    first_evidence(
        fun(Evidence) ->
            maps:get(kind, Evidence, undefined) =:= resume andalso
                maps:get(binder, Evidence, undefined) =:= Binder
        end,
        maps:get(evidence, Context),
        binder_evidence(Binder, Context)
    );
resume_evidence(_Target, _Context) ->
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
