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

    %% Validate each clause
    lists:foreach(fun(Clause) ->
        validate_clause(Name, Clause)
    end, Clauses),

    ok.

%% @doc Validate a single transform clause.
-spec validate_clause(atom(), term()) -> ok.
validate_clause(_Name, {transform_clause, _Patterns, _Guards, _Body, _Loc}) ->
    %% Basic validation - clause structure is correct
    ok;
validate_clause(Name, Other) ->
    throw({semantic_error, {invalid_clause, Name, Other}}).

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
format_error(Other) ->
    io_lib:format("Semantic error: ~p", [Other]).
