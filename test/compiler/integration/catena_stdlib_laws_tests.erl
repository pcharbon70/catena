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

%% =============================================================================
%% Section 1.5.4.4 - Type Signature Verification for Law Functions
%% =============================================================================

%% Note: Full type-checking of laws.cat requires module imports (Prelude)
%% which is not yet implemented. These tests verify the expected type
%% structure from the AST declarations instead.

%% Helper to extract parameter count from a transform declaration
get_param_count({transform_decl, _, _, Clauses, _}) ->
    case Clauses of
        [{transform_clause, Patterns, _, _, _}] -> length(Patterns);
        _ -> 0
    end.

%% 1.5.4.4a - Verify mapperIdentityLaw expected signature
%% mapperIdentityLaw : f a -> Bool (1 parameter)
verify_mapper_identity_law_signature_test() ->
    Decls = load_laws_decls(),
    [IdentityLaw] = [D || D = {transform_decl, mapperIdentityLaw, _, _, _} <- Decls],
    %% Should take 1 parameter (fa)
    ?assertEqual(1, get_param_count(IdentityLaw)).

%% 1.5.4.4b - Verify mapperCompositionLaw expected signature
%% mapperCompositionLaw : (b -> c) -> (a -> b) -> f a -> Bool (3 parameters)
verify_mapper_composition_law_signature_test() ->
    Decls = load_laws_decls(),
    [CompLaw] = [D || D = {transform_decl, mapperCompositionLaw, _, _, _} <- Decls],
    %% Should take 3 parameters (f, g, fa)
    ?assertEqual(3, get_param_count(CompLaw)).

%% 1.5.4.4c - Verify pipelineLeftIdentityLaw expected signature
%% pipelineLeftIdentityLaw : a -> (a -> m b) -> Bool (2 parameters)
verify_pipeline_left_identity_law_signature_test() ->
    Decls = load_laws_decls(),
    [LeftIdLaw] = [D || D = {transform_decl, pipelineLeftIdentityLaw, _, _, _} <- Decls],
    %% Should take 2 parameters (a, f)
    ?assertEqual(2, get_param_count(LeftIdLaw)).

%% 1.5.4.4d - Verify pipelineRightIdentityLaw expected signature
%% pipelineRightIdentityLaw : m a -> Bool (1 parameter)
verify_pipeline_right_identity_law_signature_test() ->
    Decls = load_laws_decls(),
    [RightIdLaw] = [D || D = {transform_decl, pipelineRightIdentityLaw, _, _, _} <- Decls],
    %% Should take 1 parameter (ma)
    ?assertEqual(1, get_param_count(RightIdLaw)).

%% 1.5.4.4e - Verify pipelineAssociativityLaw expected signature
%% pipelineAssociativityLaw : m a -> (a -> m b) -> (b -> m c) -> Bool (3 parameters)
verify_pipeline_associativity_law_signature_test() ->
    Decls = load_laws_decls(),
    [AssocLaw] = [D || D = {transform_decl, pipelineAssociativityLaw, _, _, _} <- Decls],
    %% Should take 3 parameters (ma, f, g)
    ?assertEqual(3, get_param_count(AssocLaw)).

%% 1.5.4.4f - Verify comparableReflexivityLaw expected signature
%% comparableReflexivityLaw : a -> Bool (1 parameter)
verify_comparable_reflexivity_law_signature_test() ->
    Decls = load_laws_decls(),
    [ReflexLaw] = [D || D = {transform_decl, comparableReflexivityLaw, _, _, _} <- Decls],
    %% Should take 1 parameter (a)
    ?assertEqual(1, get_param_count(ReflexLaw)).

%% 1.5.4.4g - Verify comparableSymmetryLaw expected signature
%% comparableSymmetryLaw : a -> a -> Bool (2 parameters)
verify_comparable_symmetry_law_signature_test() ->
    Decls = load_laws_decls(),
    [SymmLaw] = [D || D = {transform_decl, comparableSymmetryLaw, _, _, _} <- Decls],
    %% Should take 2 parameters (a, b)
    ?assertEqual(2, get_param_count(SymmLaw)).

%% 1.5.4.4h - Verify combinerAssociativityLaw expected signature
%% combinerAssociativityLaw : a -> a -> a -> Bool (3 parameters)
verify_combiner_associativity_law_signature_test() ->
    Decls = load_laws_decls(),
    [CombAssocLaw] = [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
    %% Should take 3 parameters (a, b, c)
    ?assertEqual(3, get_param_count(CombAssocLaw)).

%% =============================================================================
%% Law Arity Verification Tests
%% =============================================================================

%% Verify each law function has the correct number of parameters
law_arity_test_() ->
    {"Law functions have correct arity",
     [
      {"mapperIdentityLaw takes 1 argument (fa)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, mapperIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"mapperCompositionLaw takes 3 arguments (f, g, fa)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, mapperCompositionLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"pipelineLeftIdentityLaw takes 2 arguments (a, f)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, pipelineLeftIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"pipelineRightIdentityLaw takes 1 argument (ma)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, pipelineRightIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"pipelineAssociativityLaw takes 3 arguments (ma, f, g)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, pipelineAssociativityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"comparableReflexivityLaw takes 1 argument (a)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, comparableReflexivityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"comparableSymmetryLaw takes 2 arguments (a, b)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, comparableSymmetryLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"combinerAssociativityLaw takes 3 arguments (a, b, c)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end}
     ]}.
