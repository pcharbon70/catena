%%%-------------------------------------------------------------------
%%% @doc Conservative static checks for first-class resumptions.
%%%
%%% Hindley-Milner inference preserves Resumption values through ordinary
%%% data and higher-order code, but it does not prove affine consumption.
%%% This pass therefore rejects only duplicate resumes that are syntactically
%%% unavoidable on one control-flow path. The runtime remains authoritative
%%% for all other one-shot consumption and lifetime checks.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_resumption_flow).

-export([
    validate_declarations/1,
    validate_one_shot_case/4,
    validate_supported_mode/2
]).

%% @doc Protect the compiler-owned type and representation vocabulary.
-spec validate_declarations([term()]) -> ok | {error, term()}.
validate_declarations(Declarations) ->
    validate_opaque_terms(Declarations).

%% @doc Reject a provable second resume of the same one-shot authority.
-spec validate_one_shot_case(
    atom(),
    catena_types:type(),
    term(),
    map()
) -> ok | {error, term()}.
validate_one_shot_case(Binder, Type, Body, Context) ->
    case validate_supported_mode(Type, Context) of
        ok ->
            Aliases = sets:from_list([Binder]),
            {Count, Sites} = usage(Body, Aliases),
            case Count > 1 of
                true ->
                    {error, {obvious_one_shot_reuse, Context#{
                        resume_count => Count,
                        resume_sites => Sites,
                        first_resume => hd(Sites),
                        duplicate_resume => lists:nth(2, Sites)
                    }}};
                false ->
                    ok
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Phase 3 admits one-shot and generic types, but no concrete multi-shot
%% construction or invocation.
-spec validate_supported_mode(catena_types:type(), map()) ->
    ok | {error, term()}.
validate_supported_mode(
    {tresumption, {tcon, 'MultiShot'}, _A, _B, Effects},
    Context
) ->
    {error, {unsupported_resumption_mode, Context#{
        requested_mode => multi_shot,
        residual_effects => Effects,
        reason => multi_shot_deferred
    }}};
validate_supported_mode(
    {tresumption, {tcon, 'OneShot'}, _A, _B, _Effects},
    _Context
) ->
    ok;
validate_supported_mode(
    {tresumption, {tkvar, resumption_kind, _}, _A, _B, _Effects},
    _Context
) ->
    ok;
validate_supported_mode(_Type, _Context) ->
    ok.

%%%===================================================================
%%% Opaque Representation Validation
%%%===================================================================

validate_opaque_terms([]) ->
    ok;
validate_opaque_terms([Term | Rest]) ->
    case validate_opaque_term(Term) of
        ok -> validate_opaque_terms(Rest);
        {error, _} = Error -> Error
    end;
validate_opaque_terms(Term) ->
    validate_opaque_term(Term).

validate_opaque_term(
    {type_decl, Name, _Params, _Constructors, _Derives, Location} = Term
) ->
    case reserved_resumption_name(Name) of
        true ->
            opaque_error(reserved_type_name, Name, Location, Term);
        false ->
            validate_tuple_children(Term)
    end;
validate_opaque_term(
    {constructor, Name, _Args, Location} = Term
) ->
    case reserved_resumption_name(Name) of
        true ->
            opaque_error(reserved_constructor_name, Name, Location, Term);
        false ->
            validate_tuple_children(Term)
    end;
validate_opaque_term(
    {pat_constructor, Name, _Args, Location} = Term
) ->
    case reserved_resumption_name(Name) of
        true ->
            opaque_error(opaque_pattern_match, Name, Location, Term);
        false ->
            validate_tuple_children(Term)
    end;
validate_opaque_term({var, Name, Location} = Term) ->
    case reserved_resumption_name(Name) of
        true ->
            opaque_error(opaque_value_construction, Name, Location, Term);
        false ->
            ok
    end;
validate_opaque_term(Term) when is_tuple(Term) ->
    validate_tuple_children(Term);
validate_opaque_term(Terms) when is_list(Terms) ->
    validate_opaque_terms(Terms);
validate_opaque_term(_Other) ->
    ok.

validate_tuple_children(Term) ->
    validate_opaque_terms(tuple_to_list(Term)).

opaque_error(Reason, Name, Location, Term) ->
    {error, {invalid_resumption_representation, #{
        reason => Reason,
        name => Name,
        location => Location,
        source_term => Term
    }}}.

reserved_resumption_name('Resumption') -> true;
reserved_resumption_name('ResumptionKind') -> true;
reserved_resumption_name('OneShot') -> true;
reserved_resumption_name('MultiShot') -> true;
reserved_resumption_name(_Name) -> false.

%%%===================================================================
%%% Conservative One-Shot Use Analysis
%%%===================================================================

usage({resume_expr, Target, Value, Location}, Aliases) ->
    {TargetCount, TargetSites} = usage(Target, Aliases),
    {ValueCount, ValueSites} = usage(Value, Aliases),
    ResumeCount = case authority_expression(Target, Aliases) of
        true -> 1;
        false -> 0
    end,
    ResumeSites = case ResumeCount of
        1 -> [Location];
        0 -> []
    end,
    {
        TargetCount + ValueCount + ResumeCount,
        TargetSites ++ ValueSites ++ ResumeSites
    };
usage({'let', Name, Value, Body}, Aliases) ->
    ValueUsage = usage(Value, Aliases),
    BodyAliases0 = sets:del_element(Name, Aliases),
    BodyAliases = case authority_expression(Value, Aliases) of
        true -> sets:add_element(Name, BodyAliases0);
        false -> BodyAliases0
    end,
    combine_sequence([ValueUsage, usage(Body, BodyAliases)]);
usage({'letrec', Name, Value, Body}, Aliases) ->
    Shadowed = sets:del_element(Name, Aliases),
    combine_sequence([
        usage(Value, Shadowed),
        usage(Body, Shadowed)
    ]);
usage({lam, _Param, _Body}, _Aliases) ->
    %% Creating a closure does not invoke it. Runtime consumption remains
    %% authoritative when invocation count is not statically known.
    {0, []};
usage({'if', Condition, Then, Else}, Aliases) ->
    combine_sequence([
        usage(Condition, Aliases),
        combine_alternatives([
            usage(Then, Aliases),
            usage(Else, Aliases)
        ])
    ]);
usage({'match', Scrutinee, Clauses, _Location}, Aliases) ->
    usage_match(Scrutinee, Clauses, Aliases);
usage({match, Scrutinee, Clauses}, Aliases) ->
    usage_match(Scrutinee, Clauses, Aliases);
usage({handle_expr, Body, Handlers, _Location}, Aliases) ->
    combine_sequence([
        usage(Body, Aliases),
        combine_alternatives([
            usage_handler(Handler, Aliases)
            || Handler <- Handlers
        ])
    ]);
usage(Term, Aliases) when is_tuple(Term) ->
    combine_sequence([
        usage(Element, Aliases)
        || Element <- tuple_to_list(Term)
    ]);
usage(Terms, Aliases) when is_list(Terms) ->
    combine_sequence([usage(Term, Aliases) || Term <- Terms]);
usage(_Other, _Aliases) ->
    {0, []}.

usage_match(Scrutinee, Clauses, Aliases) ->
    combine_sequence([
        usage(Scrutinee, Aliases),
        combine_alternatives([
            usage_match_clause(Clause, Aliases)
            || Clause <- Clauses
        ])
    ]).

usage_match_clause({_Pattern, Body}, Aliases) ->
    usage(Body, Aliases);
usage_match_clause({_Pattern, Guard, Body}, Aliases) ->
    combine_sequence([
        usage(Guard, Aliases),
        usage(Body, Aliases)
    ]);
usage_match_clause(Other, Aliases) ->
    usage(Other, Aliases).

usage_handler(
    {handler_clause, _Effect, Operations, _Location},
    Aliases
) ->
    combine_alternatives([
        usage_operation_case(Operation, Aliases)
        || Operation <- Operations
    ]);
usage_handler(Other, Aliases) ->
    usage(Other, Aliases).

usage_operation_case(
    {
        operation_case,
        _Operation,
        _Patterns,
        {resumption_binder, Binder, _Origin},
        Body,
        _Location
    },
    Aliases
) ->
    usage(Body, sets:del_element(Binder, Aliases));
usage_operation_case(
    {operation_case, _Operation, _Patterns, Body, _Location},
    Aliases
) ->
    usage(Body, Aliases);
usage_operation_case(Other, Aliases) ->
    usage(Other, Aliases).

authority_expression({var, Name}, Aliases) ->
    sets:is_element(Name, Aliases);
authority_expression({var, Name, _Location}, Aliases) ->
    sets:is_element(Name, Aliases);
authority_expression({ann, Expr, _Type}, Aliases) ->
    authority_expression(Expr, Aliases);
authority_expression(_Other, _Aliases) ->
    false.

combine_sequence(Usages) ->
    lists:foldl(
        fun({Count, Sites}, {AccCount, AccSites}) ->
            {AccCount + Count, AccSites ++ Sites}
        end,
        {0, []},
        Usages
    ).

combine_alternatives([]) ->
    {0, []};
combine_alternatives([First | Rest]) ->
    lists:foldl(
        fun({Count, _Sites} = Candidate, {BestCount, _} = Best) ->
            case Count > BestCount of
                true -> Candidate;
                false -> Best
            end
        end,
        First,
        Rest
    ).
