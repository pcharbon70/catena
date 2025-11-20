#!/usr/bin/env escript
%%% Demonstration of Enhanced Location Tracking
%%%
%%% This demo shows how the Catena parser now tracks precise locations
%%% throughout the AST for better error messages and debugging.

main([]) ->
    io:format("~n=== Enhanced Location Tracking Demo ===~n~n"),

    %% Add paths
    code:add_patha("src/compiler/parser"),
    code:add_patha("src/compiler/lexer"),

    %% Demo 1: Simple declaration with location tracking
    demo_simple_type(),

    %% Demo 2: Multi-line function with location spans
    demo_multiline_function(),

    %% Demo 3: Location-aware error messages
    demo_error_locations(),

    %% Demo 4: Extract locations from AST
    demo_ast_locations(),

    io:format("~n=== Demo Complete ===~n"),
    ok.

%%============================================================================
%% Demo Functions
%%============================================================================

demo_simple_type() ->
    io:format("1. Simple Type Declaration~n"),
    io:format("   Source: type Bool = True | False~n"),

    Source = "type Bool = True | False",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, {module, _, _, _, [TypeDecl], _}} = catena_parser:parse(Tokens),

    {type_decl, 'Bool', [], Constructors, [], Loc} = TypeDecl,
    io:format("   Type location: ~s~n", [catena_location:format(Loc)]),

    lists:foreach(fun({constructor, Name, _, ConsLoc}) ->
        io:format("     Constructor ~p at ~s~n",
                  [Name, catena_location:format(ConsLoc)])
    end, Constructors),
    io:format("~n").

demo_multiline_function() ->
    io:format("2. Multi-line Function~n"),
    Source = "transform length xs = match\n"
             "  | Nil -> 0\n"
             "  | Cons(_ rest) -> 1\n"
             "end",
    io:format("   Source:~n~s~n", [Source]),

    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, {module, _, _, _, [TransformDecl], _}} = catena_parser:parse(Tokens),

    {transform_decl, length, _Type, Clauses, TransformLoc} = TransformDecl,
    io:format("   Transform declaration at ~s~n", [catena_location:format(TransformLoc)]),

    lists:foreach(fun({transform_clause, _Patterns, _Guards, Body, ClauseLoc}) ->
        io:format("     Clause at ~s~n", [catena_location:format(ClauseLoc)]),
        case Body of
            {match_expr, MatchClauses, MatchLoc} ->
                io:format("       Match expression at ~s~n",
                          [catena_location:format(MatchLoc)]),
                lists:foreach(fun({match_clause, Pattern, _, _, McaseLoc}) ->
                    PatternStr = format_pattern(Pattern),
                    io:format("         Pattern ~s at ~s~n",
                              [PatternStr, catena_location:format(McaseLoc)])
                end, MatchClauses);
            _ ->
                ok
        end
    end, Clauses),
    io:format("~n").

demo_error_locations() ->
    io:format("3. Location-Aware Error Messages~n"),

    %% Parse error
    Source1 = "transform bad syntax",
    io:format("   Source: ~s~n", [Source1]),
    case catena_parse:parse(Source1) of
        {error, Error} ->
            ErrorMsg = lists:flatten(catena_parse:format_error(Error)),
            io:format("   Error: ~s~n", [ErrorMsg]);
        _ ->
            io:format("   (Should have been an error)~n")
    end,

    %% Lex error
    Source2 = "flow f x = {- unclosed",
    io:format("   Source: ~s~n", [Source2]),
    case catena_parse:parse(Source2) of
        {error, Error2} ->
            ErrorMsg2 = lists:flatten(catena_parse:format_error(Error2)),
            io:format("   Error: ~s~n", [ErrorMsg2]);
        _ ->
            io:format("   (Should have been an error)~n")
    end,
    io:format("~n").

demo_ast_locations() ->
    io:format("4. Extract Locations from AST~n"),
    Source = "transform add x y = x + y",
    io:format("   Source: ~s~n", [Source]),

    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, {module, _, _, _, [TransformDecl], _}} = catena_parser:parse(Tokens),

    {transform_decl, add, _Type, [Clause], TransformLoc} = TransformDecl,
    {transform_clause, Patterns, _, Body, ClauseLoc} = Clause,

    io:format("   Transform at ~s~n", [catena_location:format(TransformLoc)]),
    io:format("   Clause at ~s~n", [catena_location:format(ClauseLoc)]),

    %% Show pattern locations
    lists:foreach(fun(Pattern) ->
        {pat_var, VarName, PatLoc} = Pattern,
        io:format("     Pattern ~p at ~s~n",
                  [VarName, catena_location:format(PatLoc)])
    end, Patterns),

    %% Show body location
    case Body of
        {binary_op, plus, Left, Right, BinLoc} ->
            io:format("   Binary operation at ~s~n",
                      [catena_location:format(BinLoc)]),
            {var, LName, LLoc} = Left,
            {var, RName, RLoc} = Right,
            io:format("     Left operand ~p at ~s~n",
                      [LName, catena_location:format(LLoc)]),
            io:format("     Right operand ~p at ~s~n",
                      [RName, catena_location:format(RLoc)]);
        _ ->
            ok
    end,
    io:format("~n").

%%============================================================================
%% Helper Functions
%%============================================================================

format_pattern({pat_var, Name, _}) ->
    atom_to_list(Name);
format_pattern({pat_wildcard, _}) ->
    "_";
format_pattern({pat_constructor, Name, [], _}) ->
    atom_to_list(Name);
format_pattern({pat_constructor, Name, Args, _}) ->
    io_lib:format("~p(~p args)", [Name, length(Args)]);
format_pattern(_) ->
    "?".
