%% @doc Semantic analysis pass for Catena AST.
%%
%% This module performs semantic validation and transformation on the parsed AST.
%% It runs between parsing and type checking to:
%% - Group transform signatures with their implementation clauses
%% - Validate that clause names match signature names
%% - Detect duplicate definitions
%% - Desugar do-notation to explicit bind chains
%% - Prepare the AST for type inference
%%
%% @end
-module(catena_semantic).

-export([analyze/1, analyze_module/1, format_error/1]).

%% @doc Analyze a parsed AST (either a module or list of declarations).
-spec analyze(term()) -> {ok, term()} | {error, term()}.
analyze({module, Name, Exports, Imports, Declarations, Location}) ->
    case analyze_declarations(Declarations) of
        {ok, AnalyzedDecls} ->
            %% Desugar do-notation after grouping transforms
            DesugaredDecls = catena_desugar:desugar(AnalyzedDecls),
            {ok, {module, Name, Exports, Imports, DesugaredDecls, Location}};
        {error, _} = Error ->
            Error
    end;
analyze(Declarations) when is_list(Declarations) ->
    case analyze_declarations(Declarations) of
        {ok, AnalyzedDecls} ->
            %% Desugar do-notation
            {ok, catena_desugar:desugar(AnalyzedDecls)};
        {error, _} = Error ->
            Error
    end.

%% @doc Alias for analyze/1 for clarity when working with modules.
-spec analyze_module(term()) -> {ok, term()} | {error, term()}.
analyze_module(Module) ->
    analyze(Module).

%% @doc Analyze a list of declarations.
%% Groups transforms by name and validates them.
-spec analyze_declarations([term()]) -> {ok, [term()]} | {error, term()}.
analyze_declarations(Declarations) ->
    try
        {ok, group_transforms(Declarations)}
    catch
        throw:{semantic_error, Error} ->
            {error, Error}
    end.

%% @doc Group consecutive transform declarations by name.
%% Merges signatures with their implementation clauses.
-spec group_transforms([term()]) -> [term()].
group_transforms([]) ->
    [];
group_transforms([{transform_decl, Name, Type, Clauses, Loc} | Rest]) ->
    %% Collect all consecutive clauses for this transform
    {MoreClauses, Remaining} = collect_transform_clauses(Name, Rest),
    AllClauses = Clauses ++ MoreClauses,

    %% Validate the grouped transform
    validate_transform(Name, Type, AllClauses, Loc),

    %% Build the merged transform declaration
    MergedTransform = {transform_decl, Name, Type, AllClauses, Loc},
    [MergedTransform | group_transforms(Remaining)];
group_transforms([Other | Rest]) ->
    [Other | group_transforms(Rest)].

%% @doc Collect consecutive transform clauses with the same name.
%% Returns {CollectedClauses, RemainingDeclarations}.
-spec collect_transform_clauses(atom(), [term()]) -> {[term()], [term()]}.
collect_transform_clauses(Name, [{transform_decl, Name, undefined, Clauses, _Loc} | Rest]) ->
    %% Same name, no type signature - these are additional clauses
    {MoreClauses, Remaining} = collect_transform_clauses(Name, Rest),
    {Clauses ++ MoreClauses, Remaining};
collect_transform_clauses(Name, [{transform_decl, Name, _Type, _Clauses, Loc} | _Rest]) ->
    %% Same name but has a type signature - this is an error (duplicate signature)
    throw({semantic_error, {duplicate_signature, Name, Loc}});
collect_transform_clauses(_Name, Rest) ->
    %% Different name or not a transform - stop collecting
    {[], Rest}.

%% @doc Validate a grouped transform declaration.
-spec validate_transform(atom(), term(), [term()], term()) -> ok.
validate_transform(Name, Type, Clauses, Loc) ->
    %% Check that we have at least one clause if we have a signature
    case {Type, Clauses} of
        {undefined, []} ->
            throw({semantic_error, {empty_transform, Name, Loc}});
        {_Type, []} ->
            %% Signature without implementation is allowed (forward declaration)
            ok;
        _ ->
            ok
    end,

    validate_clause_arities(Name, Clauses),

    %% Validate each clause
    lists:foreach(fun(Clause) ->
        validate_clause(Name, Clause)
    end, Clauses),

    ok.

%% @doc Validate a single transform clause.
-spec validate_clause(atom(), term()) -> ok.
validate_clause(Name, {transform_clause, Patterns, Guards, Body, _Loc}) ->
    validate_patterns(Patterns),
    validate_guards(Name, Guards),
    validate_expression_patterns(Body),
    ok;
validate_clause(Name, Other) ->
    throw({semantic_error, {invalid_clause, Name, Other}}).

validate_clause_arities(_Name, []) ->
    ok;
validate_clause_arities(Name, [{transform_clause, Patterns, _, _, _} | Rest]) ->
    ExpectedArity = length(Patterns),
    case lists:dropwhile(
        fun
            ({transform_clause, OtherPatterns, _, _, _}) ->
                length(OtherPatterns) =:= ExpectedArity;
            (_) ->
                false
        end,
        Rest
    ) of
        [] ->
            ok;
        [{transform_clause, OtherPatterns, _, _, OtherLocation} | _] ->
            throw({semantic_error,
                {inconsistent_clause_arity,
                    Name,
                    ExpectedArity,
                    length(OtherPatterns),
                    OtherLocation}});
        [Other | _] ->
            throw({semantic_error, {invalid_clause, Name, Other}})
    end.

validate_patterns(Patterns) ->
    lists:foreach(fun validate_pattern/1, Patterns).

validate_pattern({pat_constructor, _Name, Arguments, _Location}) ->
    validate_patterns(Arguments);
validate_pattern({pat_list, Elements, _Location}) ->
    validate_patterns(Elements);
validate_pattern({pat_cons, Head, Tail, _Location}) ->
    validate_pattern(Head),
    validate_pattern(Tail);
validate_pattern({pat_tuple, Elements, _Location}) ->
    validate_patterns(Elements);
validate_pattern({pat_as, _Name, Pattern, _Location}) ->
    validate_pattern(Pattern);
validate_pattern({pat_record, Fields, _Location}) ->
    validate_patterns([Pattern || {_Field, Pattern} <- Fields]);
validate_pattern({pat_or, [], Location}) ->
    throw({semantic_error, {empty_or_pattern, Location}});
validate_pattern({pat_or, [First | Rest], Location}) ->
    validate_pattern(First),
    ExpectedBindings = pattern_bindings(First),
    lists:foreach(
        fun(Alternative) ->
            validate_pattern(Alternative),
            ActualBindings = pattern_bindings(Alternative),
            case ActualBindings =:= ExpectedBindings of
                true ->
                    ok;
                false ->
                    throw({semantic_error,
                        {or_pattern_binding_mismatch,
                            sets:to_list(ExpectedBindings),
                            sets:to_list(ActualBindings),
                            Location}})
            end
        end,
        Rest
    );
validate_pattern(_Pattern) ->
    ok.

pattern_bindings({pat_var, Name, _Location}) when Name =/= true, Name =/= false ->
    sets:from_list([Name]);
pattern_bindings({pat_constructor, _Name, Arguments, _Location}) ->
    union_pattern_bindings(Arguments);
pattern_bindings({pat_list, Elements, _Location}) ->
    union_pattern_bindings(Elements);
pattern_bindings({pat_cons, Head, Tail, _Location}) ->
    sets:union(pattern_bindings(Head), pattern_bindings(Tail));
pattern_bindings({pat_tuple, Elements, _Location}) ->
    union_pattern_bindings(Elements);
pattern_bindings({pat_as, Name, Pattern, _Location}) ->
    sets:add_element(Name, pattern_bindings(Pattern));
pattern_bindings({pat_or, [First | _], _Location}) ->
    pattern_bindings(First);
pattern_bindings({pat_record, Fields, _Location}) ->
    union_pattern_bindings([Pattern || {_Field, Pattern} <- Fields]);
pattern_bindings(_Pattern) ->
    sets:new().

union_pattern_bindings(Patterns) ->
    lists:foldl(
        fun(Pattern, Bindings) ->
            sets:union(Bindings, pattern_bindings(Pattern))
        end,
        sets:new(),
        Patterns
    ).

validate_guards(_Name, undefined) ->
    ok;
validate_guards(Name, Guards) when is_list(Guards) ->
    lists:foreach(fun(Guard) -> validate_guard(Name, Guard) end, Guards);
validate_guards(Name, Guard) ->
    validate_guard(Name, Guard).

validate_guard(Name, Guard) ->
    case catena_infer_effect:check_guard_purity(Guard) of
        ok ->
            validate_expression_patterns(Guard);
        {error, {impure_guard, _Guard, Effects}} ->
            throw({semantic_error,
                {impure_guard, Name, Effects, expression_location(Guard)}})
    end.

validate_expression_patterns({match_expr, Scrutinee, Clauses, _Location}) ->
    validate_expression_patterns(Scrutinee),
    lists:foreach(fun validate_match_clause/1, Clauses);
validate_expression_patterns(Term) when is_tuple(Term) ->
    validate_expression_patterns(tuple_to_list(Term));
validate_expression_patterns(Terms) when is_list(Terms) ->
    lists:foreach(fun validate_expression_patterns/1, Terms);
validate_expression_patterns(_Term) ->
    ok.

validate_match_clause({match_clause, Pattern, Guards, Body, _Location}) ->
    validate_pattern(Pattern),
    validate_guards(match, Guards),
    validate_expression_patterns(Body);
validate_match_clause(_Other) ->
    ok.

expression_location(Term) when is_tuple(Term), tuple_size(Term) > 1 ->
    Last = element(tuple_size(Term), Term),
    case Last of
        {location, _, _} -> Last;
        _ -> undefined
    end;
expression_location(_Term) ->
    undefined.

%% @doc Format a semantic error for display.
-spec format_error(term()) -> string().
format_error({duplicate_signature, Name, {location, Line, _Col}}) ->
    io_lib:format("Duplicate type signature for '~s' at line ~p", [Name, Line]);
format_error({duplicate_signature, Name, Line}) when is_integer(Line) ->
    io_lib:format("Duplicate type signature for '~s' at line ~p", [Name, Line]);
format_error({empty_transform, Name, {location, Line, _Col}}) ->
    io_lib:format("Transform '~s' at line ~p has no implementation", [Name, Line]);
format_error({empty_transform, Name, Line}) when is_integer(Line) ->
    io_lib:format("Transform '~s' at line ~p has no implementation", [Name, Line]);
format_error({invalid_clause, Name, _Clause}) ->
    io_lib:format("Invalid clause for transform '~s'", [Name]);
format_error({inconsistent_clause_arity, Name, Expected, Actual, Location}) ->
    io_lib:format(
        "Transform '~s' has inconsistent clause arity: expected ~p, got ~p at ~p",
        [Name, Expected, Actual, Location]
    );
format_error({impure_guard, Name, Effects, Location}) ->
    io_lib:format(
        "Pattern guard in '~s' must be pure; inferred ~p at ~p",
        [Name, Effects, Location]
    );
format_error({or_pattern_binding_mismatch, Expected, Actual, Location}) ->
    io_lib:format(
        "Or-pattern alternatives bind different names: expected ~p, got ~p at ~p",
        [Expected, Actual, Location]
    );
format_error(Other) ->
    io_lib:format("Semantic error: ~p", [Other]).
