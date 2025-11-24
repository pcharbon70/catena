%% @doc Standard Library Law Verification Tests
%% Tests for Section 1.5.4 of Phase 1.
%% Validates that trait law verification functions compile correctly.

-module(catena_stdlib_laws_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper to load laws module
load_laws_decls() ->
    {ok, Source} = catena_test_helpers:load_stdlib_file("laws.cat"),
    {ok, Tokens} = catena_test_helpers:tokenize_source(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),
    Decls.

%% =============================================================================
%% Section 1.5.4 - Law Verification via Test Module
%% =============================================================================

%% 1.5.4.1 Compile Mapper identity law verification
parse_mapper_identity_law_test() ->
    {ok, Source} = catena_test_helpers:load_stdlib_file("laws.cat"),
    {ok, Tokens} = catena_test_helpers:tokenize_source(Source),
    {ok, {module, 'Laws', Exports, _, Decls, _}} = catena_parser:parse(Tokens),
    %% Check that mapperIdentityLaw is exported
    ?assert(lists:member({export_transform, mapperIdentityLaw}, Exports)),
    %% Check declaration exists
    ?assertEqual(8, length(Decls)).

%% 1.5.4.2 Compile Mapper composition law verification
parse_mapper_composition_law_test() ->
    Decls = load_laws_decls(),
    %% Find the composition law transform
    CompLaws = [D || D = {transform_decl, mapperCompositionLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(CompLaws)).

%% 1.5.4.3 Compile Pipeline monad laws
parse_pipeline_laws_test() ->
    Decls = load_laws_decls(),
    %% Check all three Pipeline laws exist
    LeftId = [D || D = {transform_decl, pipelineLeftIdentityLaw, _, _, _} <- Decls],
    RightId = [D || D = {transform_decl, pipelineRightIdentityLaw, _, _, _} <- Decls],
    Assoc = [D || D = {transform_decl, pipelineAssociativityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(LeftId)),
    ?assertEqual(1, length(RightId)),
    ?assertEqual(1, length(Assoc)).

%% Test Comparable and Combiner laws
parse_comparable_combiner_laws_test() ->
    Decls = load_laws_decls(),
    Reflex = [D || D = {transform_decl, comparableReflexivityLaw, _, _, _} <- Decls],
    Symm = [D || D = {transform_decl, comparableSymmetryLaw, _, _, _} <- Decls],
    CombAssoc = [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(Reflex)),
    ?assertEqual(1, length(Symm)),
    ?assertEqual(1, length(CombAssoc)).

%% 1.5.4.4 Test law structure contains expected AST nodes
verify_law_structure_test() ->
    Decls = load_laws_decls(),
    %% Get mapperIdentityLaw
    [{transform_decl, mapperIdentityLaw, _Sig, Clauses, _Loc}] =
        [D || D = {transform_decl, mapperIdentityLaw, _, _, _} <- Decls],
    %% Should have one clause with one parameter
    ?assertEqual(1, length(Clauses)),
    [{transform_clause, Patterns, _Guards, _Body, _ClauseLoc}] = Clauses,
    ?assertEqual(1, length(Patterns)).

%% Test that all 8 laws are present
all_laws_present_test() ->
    Decls = load_laws_decls(),
    ?assertEqual(8, length(Decls)),
    %% All should be transform_decl
    TransformDecls = [D || D = {transform_decl, _, _, _, _} <- Decls],
    ?assertEqual(8, length(TransformDecls)).
