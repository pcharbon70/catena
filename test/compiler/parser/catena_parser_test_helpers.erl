%% Parser Test Helper Module
%% Shared utilities for parser test suites
%%
%% This module provides common parser test utilities to reduce code duplication
%% across parser test files. These helpers simplify common patterns like parsing
%% and extracting specific AST components.

-module(catena_parser_test_helpers).
-include_lib("eunit/include/eunit.hrl").

-export([
    % Parsing helpers
    parse_flow/1,
    parse_type_sig/1,
    parse_single_decl/1,

    % AST extraction helpers
    get_patterns/1,
    get_guards/1,
    get_body/1
]).

%%====================================================================
%% Parsing Helpers
%%====================================================================

%% @doc Parse a Catena transform declaration from source code
%% Tokenizes and parses source, extracting the single transform declaration.
%% Useful for testing transform syntax, patterns, and guards.
%%
%% Example:
%%   FlowDecl = parse_flow("transform id x = x"),
%%   Patterns = get_patterns(FlowDecl).
-spec parse_flow(string()) -> term().
parse_flow(Source) ->
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = AST,
    FlowDecl.

%% @doc Parse a Catena type signature from source code
%% Parses a type signature by adding a dummy transform body.
%% Useful for testing type expressions without full transform implementation.
%%
%% Example:
%%   TypeSig = parse_type_sig("transform f : Int -> Int"),
%%   ?assertMatch({type_fun, _, _}, TypeSig).
-spec parse_type_sig(string()) -> term().
parse_type_sig(Source) ->
    FullSource = Source ++ "\ntransform f = x",
    {ok, Tokens} = catena_lexer:tokenize(FullSource),
    {ok, AST} = catena_parser:parse(Tokens),
    {module, _, _, _, [FlowDecl], _} = AST,
    {transform_decl, _Name, TypeSig, _Clauses, _Loc} = FlowDecl,
    TypeSig.

%% @doc Parse tokens and extract the single declaration
%% Takes pre-tokenized input and extracts the single top-level declaration.
%% Useful when you need control over tokenization or want to test specific
%% token sequences.
%%
%% Example:
%%   {ok, Tokens} = catena_lexer:tokenize(Source),
%%   Decl = parse_single_decl(Tokens).
-spec parse_single_decl(list()) -> term().
parse_single_decl(Tokens) ->
    {ok, Result} = catena_parser:parse(Tokens),
    ?assertMatch({module, undefined, [], [], [_], _}, Result),
    {module, _, _, _, [Decl], _} = Result,
    Decl.

%%====================================================================
%% AST Extraction Helpers
%%====================================================================

%% @doc Extract the pattern list from a transform declaration
%% Assumes a transform with a single clause (most common in tests).
%% Returns the list of patterns from the first clause.
%%
%% Example:
%%   FlowDecl = parse_flow("transform add x y = x + y"),
%%   [P1, P2] = get_patterns(FlowDecl),
%%   ?assertMatch({pat_var, x, _}, P1).
-spec get_patterns(term()) -> list().
get_patterns(FlowDecl) ->
    {transform_decl, _Name, _Type, [Clause], _Loc} = FlowDecl,
    {transform_clause, Patterns, _Guards, _Body, _ClauseLoc} = Clause,
    Patterns.

%% @doc Extract guards from a transform declaration
%% Assumes a transform with a single clause (most common in tests).
%% Returns the guards from the first clause.
%%
%% Example:
%%   FlowDecl = parse_flow("transform positive x when x > 0 = x"),
%%   Guards = get_guards(FlowDecl),
%%   ?assertMatch([{op, '>', _, _}], Guards).
-spec get_guards(term()) -> list().
get_guards(FlowDecl) ->
    {transform_decl, _Name, _Type, [Clause], _Loc} = FlowDecl,
    {transform_clause, _Patterns, Guards, _Body, _ClauseLoc} = Clause,
    Guards.

%% @doc Extract the body expression from a transform declaration
%% Assumes a transform with a single clause (most common in tests).
%% Returns the body expression from the first clause.
%%
%% Example:
%%   FlowDecl = parse_flow("transform id x = x"),
%%   Body = get_body(FlowDecl),
%%   ?assertMatch({var, x, _}, Body).
-spec get_body(term()) -> term().
get_body(FlowDecl) ->
    {transform_decl, _Name, _Type, [Clause], _Loc} = FlowDecl,
    {transform_clause, _Patterns, _Guards, Body, _ClauseLoc} = Clause,
    Body.

%%====================================================================
%% Note: Trait-Specific Assertion Helpers
%%====================================================================
%%
%% Trait assertion helpers (assert_trait_structure, assert_instance_structure)
%% are kept in their respective test files because they:
%% - Have file-specific expectations and validation logic
%% - Use different parameter formats (strings vs atoms)
%% - Are tightly coupled to specific test scenarios
%%
%% This keeps test-specific logic in test files while sharing only
%% truly generic parsing utilities.
%%====================================================================
