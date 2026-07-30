%%%-------------------------------------------------------------------
%%% @doc Semantic normalization for delimited resumptions.
%%%
%%% Parsed value handlers are translated to the explicit semantic form:
%%%
%%%   op(patterns) -> body
%%%
%%% becomes:
%%%
%%%   op(patterns) with Synthetic ->
%%%       resume(Synthetic, body)
%%%
%%% Explicit control handlers keep their user-written binder and body. The
%%% generated binder and resume node carry a synthetic origin linked to the
%%% original operation case. This pass also enforces the lexical binder rules
%%% that are independent of eventual Resumption typing.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_normalize).

-export([
    normalize/1,
    normalize_expr/1,
    first_resumption/1,
    project_legacy_value_handlers/1,
    project_legacy_value_handlers/2,
    format_error/1
]).

%% @doc Normalize a module, declaration list, or expression.
-spec normalize(term()) -> {ok, term()} | {error, term()}.
normalize(AST) ->
    State = #{
        used => sets:from_list(collect_atoms(AST)),
        next => 0
    },
    try
        {Normalized, _FinalState} = normalize_term(AST, State, []),
        {ok, Normalized}
    catch
        throw:{resumption_semantic_error, Reason} ->
            {error, Reason}
    end.

%% @doc Normalize a single expression.
-spec normalize_expr(term()) -> {ok, term()} | {error, term()}.
normalize_expr(Expr) ->
    normalize(Expr).

%% @doc Locate the first normalized resumption construct in an AST.
%%
%% This is used by typed/backend phase boundaries to fail closed until later
%% phases provide Resumption typing and selective-CPS dispositions.
-spec first_resumption(term()) -> none | {ok, map()}.
first_resumption({
    operation_case,
    _Operation,
    _Params,
    {resumption_binder, _Binder, Origin},
    _Body,
    Location
} = OperationCase) ->
    {ok, #{
        construct => operation_case,
        mode => binder_mode(Origin),
        location => Location,
        source_term => OperationCase
    }};
first_resumption({resume_expr, _Resumption, _Value, Location} = ResumeExpr) ->
    {ok, #{
        construct => resume_expr,
        mode => resumable,
        location => Location,
        source_term => ResumeExpr
    }};
first_resumption(Tuple) when is_tuple(Tuple) ->
    first_resumption_list(tuple_to_list(Tuple));
first_resumption(List) when is_list(List) ->
    first_resumption_list(List);
first_resumption(_Other) ->
    none.

first_resumption_list([]) ->
    none;
first_resumption_list([Term | Rest]) ->
    case first_resumption(Term) of
        none -> first_resumption_list(Rest);
        {ok, _} = Found -> Found
    end.

%% @doc Produce the compatibility view consumed by the pre-resumption
%% typechecker and value-handler backend.
%%
%% Only the exact synthetic shape emitted by this module is projected. This
%% keeps normalized resumptions in the semantic AST while preserving the
%% established value-handler implementation until later phases add
%% Resumption typing and selective CPS. Explicit control cases, standalone
%% resume expressions, and malformed synthetic shapes fail closed.
-spec project_legacy_value_handlers(term()) ->
    {ok, term()} | {error, term()}.
project_legacy_value_handlers(AST) ->
    project_legacy_value_handlers(AST, compatibility_projection).

-spec project_legacy_value_handlers(term(), atom()) ->
    {ok, term()} | {error, term()}.
project_legacy_value_handlers(AST, Stage) when is_atom(Stage) ->
    try
        {ok, project_legacy_term(AST, Stage)}
    catch
        throw:{resumption_projection_error, Reason} ->
            {error, Reason}
    end.

project_legacy_term(
    {
        operation_case,
        Operation,
        Params,
        {
            resumption_binder,
            Binder,
            {synthetic, value_handler_auto_resume, _SourceLocation} = Origin
        },
        {
            resume_expr,
            {var, Binder, Origin},
            ValueBody,
            Origin
        },
        Location
    },
    Stage
) ->
    {
        operation_case,
        Operation,
        project_legacy_term(Params, Stage),
        project_legacy_term(ValueBody, Stage),
        Location
    };
project_legacy_term(
    {
        operation_case,
        _Operation,
        _Params,
        {resumption_binder, _Binder, Origin},
        _Body,
        Location
    } = OperationCase,
    Stage
) ->
    Mode = binder_mode(Origin),
    projection_error(#{
        stage => Stage,
        construct => operation_case,
        mode => Mode,
        reason => projection_reason(Mode),
        location => Location,
        source_term => OperationCase
    });
project_legacy_term(
    {resume_expr, _Resumption, _Value, Location} = ResumeExpr,
    Stage
) ->
    projection_error(#{
        stage => Stage,
        construct => resume_expr,
        mode => resumable,
        location => Location,
        source_term => ResumeExpr
    });
project_legacy_term(Tuple, Stage) when is_tuple(Tuple) ->
    list_to_tuple([
        project_legacy_term(Element, Stage)
        || Element <- tuple_to_list(Tuple)
    ]);
project_legacy_term(List, Stage) when is_list(List) ->
    [project_legacy_term(Element, Stage) || Element <- List];
project_legacy_term(Other, _Stage) ->
    Other.

projection_reason(synthetic_auto_resume) ->
    malformed_synthetic_auto_resume;
projection_reason(explicit_control) ->
    resumption_typing_and_cps_deferred.

projection_error(Context) ->
    throw({resumption_projection_error,
        {missing_resumption_lowering, Context}}).

normalize_term(
    {operation_case, Operation, Params, Body, Location},
    State,
    Scope
) ->
    normalize_value_case(Operation, Params, Body, Location, State, Scope);
normalize_term(
    {operation_case, Operation, Params, none, Body, Location},
    State,
    Scope
) ->
    normalize_value_case(Operation, Params, Body, Location, State, Scope);
normalize_term(
    {
        operation_case,
        Operation,
        Params,
        {resumption_binder, Binder, Origin} = Resumption,
        Body,
        Location
    },
    State,
    Scope
) ->
    validate_explicit_case(Binder, Origin, Params),
    {NormalizedParams, State1} = normalize_list(Params, State, Scope),
    {NormalizedBody, State2} = normalize_term(
        Body,
        State1,
        [Binder | Scope]
    ),
    {
        {
            operation_case,
            Operation,
            NormalizedParams,
            Resumption,
            NormalizedBody,
            Location
        },
        State2
    };
normalize_term(
    {operation_case, _Operation, _Params, InvalidBinder, _Body, _Location},
    _State,
    _Scope
) ->
    semantic_error({invalid_resumption_binder, #{
        binder => InvalidBinder,
        reason => malformed_metadata
    }});
normalize_term({resume_expr, Resumption, Value, Location}, State, Scope) ->
    validate_resume_scope(Resumption, Scope, Location),
    {NormalizedResumption, State1} = normalize_term(
        Resumption,
        State,
        Scope
    ),
    {NormalizedValue, State2} = normalize_term(Value, State1, Scope),
    {
        {resume_expr, NormalizedResumption, NormalizedValue, Location},
        State2
    };
normalize_term(Tuple, State, Scope) when is_tuple(Tuple) ->
    {Elements, State1} = normalize_list(tuple_to_list(Tuple), State, Scope),
    {list_to_tuple(Elements), State1};
normalize_term(List, State, Scope) when is_list(List) ->
    normalize_list(List, State, Scope);
normalize_term(Other, State, _Scope) ->
    {Other, State}.

normalize_value_case(Operation, Params, Body, Location, State, Scope) ->
    {NormalizedParams, State1} = normalize_list(Params, State, Scope),
    %% The original value expression is normalized outside the synthetic
    %% binder's lexical scope: user source cannot refer to an internal name.
    {NormalizedBody, State2} = normalize_term(Body, State1, Scope),
    {Binder, State3} = fresh_binder(State2),
    Origin = {synthetic, value_handler_auto_resume, source_location(Location)},
    Resumption = {resumption_binder, Binder, Origin},
    Resume = {
        resume_expr,
        {var, Binder, Origin},
        NormalizedBody,
        Origin
    },
    {
        {
            operation_case,
            Operation,
            NormalizedParams,
            Resumption,
            Resume,
            Location
        },
        State3
    }.

normalize_list(Terms, State, Scope) ->
    lists:mapfoldl(
        fun(Term, CurrentState) ->
            normalize_term(Term, CurrentState, Scope)
        end,
        State,
        Terms
    ).

validate_explicit_case(Binder, Origin, Params)
        when is_atom(Binder), Binder =/= undefined, Binder =/= '' ->
    validate_origin(Origin, Binder),
    Bindings = lists:append([pattern_bindings(Pattern) || Pattern <- Params]),
    case first_duplicate(Bindings) of
        none ->
            case lists:member(Binder, Bindings) of
                true ->
                    semantic_error({invalid_resumption_binder, #{
                        binder => Binder,
                        reason => duplicates_operation_pattern,
                        location => Origin
                    }});
                false ->
                    ok
            end;
        {some, Duplicate} ->
            semantic_error({invalid_resumption_binder, #{
                binder => Binder,
                reason => {duplicate_operation_pattern, Duplicate},
                location => Origin
            }})
    end;
validate_explicit_case(Binder, Origin, _Params) ->
    semantic_error({invalid_resumption_binder, #{
        binder => Binder,
        reason => malformed_name,
        location => Origin
    }}).

validate_origin({synthetic, Kind, SourceLocation}, _Binder)
        when is_atom(Kind) ->
    validate_concrete_location(SourceLocation);
validate_origin(Origin, Binder) ->
    case is_concrete_location(Origin) of
        true -> ok;
        false ->
            semantic_error({invalid_resumption_binder, #{
                binder => Binder,
                reason => invalid_origin,
                location => Origin
            }})
    end.

validate_resume_scope({var, Name, _TargetLocation}, Scope, ResumeLocation)
        when is_atom(Name) ->
    case lists:member(Name, Scope) of
        true ->
            ok;
        false ->
            semantic_error({resumption_binder_scope, #{
                target => Name,
                active_binders => lists:reverse(Scope),
                location => ResumeLocation
            }})
    end;
validate_resume_scope(Target, Scope, ResumeLocation) ->
    semantic_error({resumption_binder_scope, #{
        target => Target,
        active_binders => lists:reverse(Scope),
        location => ResumeLocation
    }}).

pattern_bindings({pat_var, Name, _Location})
        when Name =/= true, Name =/= false ->
    [Name];
pattern_bindings({pat_constructor, _Name, Arguments, _Location}) ->
    lists:append([pattern_bindings(Argument) || Argument <- Arguments]);
pattern_bindings({pat_list, Elements, _Location}) ->
    lists:append([pattern_bindings(Element) || Element <- Elements]);
pattern_bindings({pat_cons, Head, Tail, _Location}) ->
    pattern_bindings(Head) ++ pattern_bindings(Tail);
pattern_bindings({pat_tuple, Elements, _Location}) ->
    lists:append([pattern_bindings(Element) || Element <- Elements]);
pattern_bindings({pat_as, Name, Pattern, _Location}) ->
    [Name | pattern_bindings(Pattern)];
pattern_bindings({pat_or, [First | _], _Location}) ->
    pattern_bindings(First);
pattern_bindings({pat_record, Fields, _Location}) ->
    lists:append([
        pattern_bindings(Pattern)
        || {_Field, Pattern} <- Fields
    ]);
pattern_bindings(_Pattern) ->
    [].

first_duplicate(Names) ->
    first_duplicate(Names, sets:new()).

first_duplicate([], _Seen) ->
    none;
first_duplicate([Name | Rest], Seen) ->
    case sets:is_element(Name, Seen) of
        true -> {some, Name};
        false -> first_duplicate(Rest, sets:add_element(Name, Seen))
    end.

fresh_binder(#{used := Used, next := Next} = State) ->
    Candidate = list_to_atom(
        "__catena_resumption_" ++ integer_to_list(Next)
    ),
    case sets:is_element(Candidate, Used) of
        true ->
            fresh_binder(State#{next := Next + 1});
        false ->
            {
                Candidate,
                State#{
                    used := sets:add_element(Candidate, Used),
                    next := Next + 1
                }
            }
    end.

collect_atoms(Atom) when is_atom(Atom) ->
    [Atom];
collect_atoms(Tuple) when is_tuple(Tuple) ->
    lists:append([collect_atoms(Element) || Element <- tuple_to_list(Tuple)]);
collect_atoms(List) when is_list(List) ->
    lists:append([collect_atoms(Element) || Element <- List]);
collect_atoms(_Other) ->
    [].

binder_mode({synthetic, value_handler_auto_resume, _SourceLocation}) ->
    synthetic_auto_resume;
binder_mode(_Origin) ->
    explicit_control.

source_location({synthetic, _Kind, SourceLocation}) ->
    SourceLocation;
source_location(Location) ->
    Location.

validate_concrete_location(Location) ->
    case is_concrete_location(Location) of
        true -> ok;
        false ->
            semantic_error({invalid_resumption_binder, #{
                reason => invalid_source_location,
                location => Location
            }})
    end.

is_concrete_location({line, Line}) when is_integer(Line), Line > 0 ->
    true;
is_concrete_location({location, Line, Column})
        when is_integer(Line), Line > 0,
             is_integer(Column), Column >= 0 ->
    true;
is_concrete_location({
    location,
    StartLine,
    StartColumn,
    EndLine,
    EndColumn
})
        when is_integer(StartLine), StartLine > 0,
             is_integer(StartColumn), StartColumn >= 0,
             is_integer(EndLine), EndLine > 0,
             is_integer(EndColumn), EndColumn >= 0 ->
    true;
is_concrete_location(_Other) ->
    false.

semantic_error(Reason) ->
    throw({resumption_semantic_error, Reason}).

%% @doc Format stable structural diagnostics for source-facing callers.
-spec format_error(term()) -> string().
format_error({invalid_resumption_binder, Details}) ->
    lists:flatten(
        io_lib:format("Invalid resumption binder: ~p", [Details])
    );
format_error({resumption_binder_scope, Details}) ->
    lists:flatten(
        io_lib:format("Resume target is outside its binder scope: ~p", [Details])
    );
format_error(Other) ->
    lists:flatten(io_lib:format("Resumption normalization error: ~p", [Other])).
