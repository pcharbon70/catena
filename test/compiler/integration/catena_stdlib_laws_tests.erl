%% @doc Standard Library Law Verification Tests
%% Tests for Section 1.5.4 of Phase 1.
%% Validates that trait law verification functions compile correctly and
%% that concrete law suites execute end-to-end through the current runtime path.

-module(catena_stdlib_laws_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper to load laws module
load_laws_decls() ->
    {ok, Source} = catena_test_helpers:load_stdlib_file("laws.cat"),
    {ok, Tokens} = catena_test_helpers:tokenize_source(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),
    Decls.

load_semantic_module(ModuleName) ->
    SearchPaths = catena_module_loader:get_default_search_paths(),
    {ok, AST} = catena_module_loader:load_module(ModuleName, SearchPaths),
    {ok, SemanticModule} = catena_semantic:analyze(AST),
    SemanticModule.

analyze_source_module(Source) ->
    {ok, Tokens} = catena_test_helpers:tokenize_source(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, SemanticModule} = catena_semantic:analyze(AST),
    SemanticModule.

filter_imported_runtime_decls(Decls, BaseEnv) ->
    lists:filter(
        fun({transform_decl, Name, _TypeSig, _Clauses, _Loc}) ->
                not maps:is_key(Name, BaseEnv);
           (_Other) ->
                true
        end,
        Decls
    ).

stdlib_law_runtime_decls(BaseEnv) ->
    Modules = ['Prelude', 'Test', 'Laws'],
    lists:foldl(
        fun(ModuleName, Acc) ->
            {module, _Name, _Exports, _Imports, Decls, _ModuleLoc} =
                load_semantic_module(ModuleName),
            Acc ++ filter_imported_runtime_decls(Decls, BaseEnv)
        end,
        [],
        Modules
    ).

runtime_env_for_source(Source, BaseEnv) ->
    {module, _Name, _Exports, _Imports, LocalDecls, _Loc} = analyze_source_module(Source),
    ImportedDecls = stdlib_law_runtime_decls(BaseEnv),
    catena_test_runner:build_runtime_env(ImportedDecls ++ LocalDecls, BaseEnv).

run_named_suite(Source, SuiteName, BaseEnv) ->
    Env = runtime_env_for_source(Source, BaseEnv),
    {SuiteFun, 0, runtime} = maps:get(SuiteName, Env),
    SuiteValue = SuiteFun(),
    catena_test_runner:run_suite_value(SuiteValue).

maybe_law_env() ->
    #{
        id => {fun(X) -> X end, 1, runtime},
        map => {fun(_F, none) -> none;
                   (F, {some, X}) -> {some, F(X)}
                end, 2, runtime},
        pure => {fun(X) -> {some, X} end, 1, runtime},
        apply => {fun(_MF, none) -> none;
                     (none, _MX) -> none;
                     ({some, F}, {some, X}) -> {some, F(X)}
                  end, 2, runtime},
        chain => {fun(_F, none) -> none;
                     (F, {some, X}) -> F(X)
                  end, 2, runtime},
        equals => {fun(A, B) -> A =:= B end, 2, runtime}
    }.

broken_maybe_law_env() ->
    maps:put(pure, {fun(_X) -> none end, 1, runtime}, maybe_law_env()).

either_law_env() ->
    #{
        id => {fun(X) -> X end, 1, runtime},
        map => {fun(_F, {left, Error}) -> {left, Error};
                   (F, {right, X}) -> {right, F(X)}
                end, 2, runtime},
        pure => {fun(X) -> {right, X} end, 1, runtime},
        apply => {fun(_EF, {left, Error}) -> {left, Error};
                     ({left, Error}, _EX) -> {left, Error};
                     ({right, F}, {right, X}) -> {right, F(X)}
                  end, 2, runtime},
        chain => {fun(_F, {left, Error}) -> {left, Error};
                     (F, {right, X}) -> F(X)
                  end, 2, runtime},
        equals => {fun(A, B) -> A =:= B end, 2, runtime}
    }.

list_law_env() ->
    #{
        id => {fun(X) -> X end, 1, runtime},
        map => {fun(F, Xs) -> lists:map(F, Xs) end, 2, runtime},
        pure => {fun(X) -> [X] end, 1, runtime},
        apply => {fun(Fs, Xs) -> [F(X) || F <- Fs, X <- Xs] end, 2, runtime},
        chain => {fun(F, Xs) -> lists:flatmap(F, Xs) end, 2, runtime},
        equals => {fun(A, B) -> A =:= B end, 2, runtime},
        combine => {fun(A, B) -> A ++ B end, 2, runtime},
        empty => {value, []}
    }.

int_order_law_env() ->
    #{
        equals => {fun(A, B) -> A =:= B end, 2, runtime},
        lessOrEqual => {fun(A, B) -> A =< B end, 2, runtime}
    }.

maybe_suite_source() ->
    "module MaybeLawSuite\n"
    "export transform maybeSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform inc x = x + 1\n"
    "transform double x = x * 2\n"
    "transform wrapInc x = Some (x + 1)\n"
    "transform wrapDouble x = Some (x * 2)\n"
    "transform maybeSuite = suite \"Maybe Laws\" [\n"
    "  verify \"mapper identity\" (fn u -> mapperIdentityLaw (Some 42)),\n"
    "  verify \"mapper composition\" (fn u -> mapperCompositionLaw inc double (Some 5)),\n"
    "  verify \"pipeline left identity\" (fn u -> pipelineLeftIdentityLaw 42 wrapInc),\n"
    "  verify \"pipeline right identity\" (fn u -> pipelineRightIdentityLaw (Some 42)),\n"
    "  verify \"pipeline associativity\" (fn u -> pipelineAssociativityLaw (Some 5) wrapInc wrapDouble)\n"
    "]\n".

either_suite_source() ->
    "module EitherLawSuite\n"
    "export transform eitherSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform inc x = x + 1\n"
    "transform double x = x * 2\n"
    "transform rightInc x = Right (x + 1)\n"
    "transform rightDouble x = Right (x * 2)\n"
    "transform eitherSuite = suite \"Either Laws\" [\n"
    "  verify \"mapper identity\" (fn u -> mapperIdentityLaw (Right 42)),\n"
    "  verify \"mapper composition\" (fn u -> mapperCompositionLaw inc double (Right 5)),\n"
    "  verify \"pipeline left identity\" (fn u -> pipelineLeftIdentityLaw 42 rightInc),\n"
    "  verify \"pipeline right identity\" (fn u -> pipelineRightIdentityLaw (Right 42)),\n"
    "  verify \"pipeline associativity\" (fn u -> pipelineAssociativityLaw (Right 5) rightInc rightDouble)\n"
    "]\n".

list_suite_source() ->
    "module ListLawSuite\n"
    "export transform listSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform inc x = x + 1\n"
    "transform double x = x * 2\n"
    "transform wrapInc x = [x + 1]\n"
    "transform wrapDouble x = [x * 2, x * 3]\n"
    "transform listSuite = suite \"List Laws\" [\n"
    "  verify \"mapper identity\" (fn u -> mapperIdentityLaw [1, 2, 3]),\n"
    "  verify \"mapper composition\" (fn u -> mapperCompositionLaw inc double [1, 2, 3]),\n"
    "  verify \"pipeline left identity\" (fn u -> pipelineLeftIdentityLaw 42 wrapInc),\n"
    "  verify \"pipeline right identity\" (fn u -> pipelineRightIdentityLaw [1, 2, 3]),\n"
    "  verify \"pipeline associativity\" (fn u -> pipelineAssociativityLaw [1, 2] wrapInc wrapDouble),\n"
    "  verify \"combiner associativity\" (fn u -> combinerAssociativityLaw [1] [2] [3])\n"
    "]\n".

applicative_suite_source() ->
    "module ApplicativeLawSuite\n"
    "export transform applicativeSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform inc x = x + 1\n"
    "transform double x = x * 2\n"
    "transform applicativeSuite = suite \"Applicative Laws\" [\n"
    "  verify \"applicative identity\" (fn u -> applicativeIdentityLaw (Some 42)),\n"
    "  verify \"applicative composition\" (fn u ->\n"
    "    applicativeCompositionLaw\n"
    "      (Some inc)\n"
    "      (Some double)\n"
    "      (Some 5)),\n"
    "  verify \"applicative homomorphism\" (fn u -> applicativeHomomorphismLaw inc 41),\n"
    "  verify \"applicative interchange\" (fn u -> applicativeInterchangeLaw (Some inc) 41)\n"
    "]\n".

accumulator_suite_source() ->
    "module AccumulatorLawSuite\n"
    "export transform accumulatorSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform accumulatorSuite = suite \"Accumulator Laws\" [\n"
    "  verify \"left identity\" (fn u -> accumulatorLeftIdentityLaw [1, 2]),\n"
    "  verify \"right identity\" (fn u -> accumulatorRightIdentityLaw [1, 2])\n"
    "]\n".

orderable_suite_source() ->
    "module OrderableLawSuite\n"
    "export transform orderableSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform orderableSuite = suite \"Orderable Laws\" [\n"
    "  verify \"antisymmetry\" (fn u -> orderableAntisymmetryLaw 5 5),\n"
    "  verify \"transitivity\" (fn u -> orderableTransitivityLaw 1 2 3),\n"
    "  verify \"totality\" (fn u -> orderableTotalityLaw 4 9)\n"
    "]\n".

broken_maybe_suite_source() ->
    "module BrokenMaybeLawSuite\n"
    "export transform brokenMaybeSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform wrapInc x = Some (x + 1)\n"
    "transform brokenMaybeSuite = suite \"Broken Maybe Laws\" [\n"
    "  verify \"pipeline left identity\" (fn u -> pipelineLeftIdentityLaw 42 wrapInc)\n"
    "]\n".

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
    ?assertEqual(28, length(Decls)).

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
    Trans = [D || D = {transform_decl, comparableTransitivityLaw, _, _, _} <- Decls],
    CombAssoc = [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(Reflex)),
    ?assertEqual(1, length(Symm)),
    ?assertEqual(1, length(Trans)),
    ?assertEqual(1, length(CombAssoc)).

parse_accumulator_applicator_orderable_laws_test() ->
    Decls = load_laws_decls(),
    AccLeft = [D || D = {transform_decl, accumulatorLeftIdentityLaw, _, _, _} <- Decls],
    AccRight = [D || D = {transform_decl, accumulatorRightIdentityLaw, _, _, _} <- Decls],
    AppIdentity = [D || D = {transform_decl, applicativeIdentityLaw, _, _, _} <- Decls],
    AppComposition = [D || D = {transform_decl, applicativeCompositionLaw, _, _, _} <- Decls],
    AppHomomorphism = [D || D = {transform_decl, applicativeHomomorphismLaw, _, _, _} <- Decls],
    AppInterchange = [D || D = {transform_decl, applicativeInterchangeLaw, _, _, _} <- Decls],
    OrdAnti = [D || D = {transform_decl, orderableAntisymmetryLaw, _, _, _} <- Decls],
    OrdTrans = [D || D = {transform_decl, orderableTransitivityLaw, _, _, _} <- Decls],
    OrdTotality = [D || D = {transform_decl, orderableTotalityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(AccLeft)),
    ?assertEqual(1, length(AccRight)),
    ?assertEqual(1, length(AppIdentity)),
    ?assertEqual(1, length(AppComposition)),
    ?assertEqual(1, length(AppHomomorphism)),
    ?assertEqual(1, length(AppInterchange)),
    ?assertEqual(1, length(OrdAnti)),
    ?assertEqual(1, length(OrdTrans)),
    ?assertEqual(1, length(OrdTotality)).

parse_system_flow_laws_test() ->
    Decls = load_laws_decls(),
    LeftId = [D || D = {transform_decl, systemLeftIdentityLaw, _, _, _} <- Decls],
    RightId = [D || D = {transform_decl, systemRightIdentityLaw, _, _, _} <- Decls],
    Assoc = [D || D = {transform_decl, systemAssociativityLaw, _, _, _} <- Decls],
    FlowLiftId = [D || D = {transform_decl, flowLiftIdentityLaw, _, _, _} <- Decls],
    FlowLift = [D || D = {transform_decl, flowLiftCompositionLaw, _, _, _} <- Decls],
    FlowFirstLift = [D || D = {transform_decl, flowFirstLiftLaw, _, _, _} <- Decls],
    FlowFirstComp = [D || D = {transform_decl, flowFirstCompositionLaw, _, _, _} <- Decls],
    FlowFirstProj = [D || D = {transform_decl, flowFirstProjectionLaw, _, _, _} <- Decls],
    FlowFirstNat = [D || D = {transform_decl, flowFirstNaturalityLaw, _, _, _} <- Decls],
    FlowFirstAssoc = [D || D = {transform_decl, flowFirstAssociativityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(LeftId)),
    ?assertEqual(1, length(RightId)),
    ?assertEqual(1, length(Assoc)),
    ?assertEqual(1, length(FlowLiftId)),
    ?assertEqual(1, length(FlowLift)),
    ?assertEqual(1, length(FlowFirstLift)),
    ?assertEqual(1, length(FlowFirstComp)),
    ?assertEqual(1, length(FlowFirstProj)),
    ?assertEqual(1, length(FlowFirstNat)),
    ?assertEqual(1, length(FlowFirstAssoc)).

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

%% Test that all promoted laws are present
all_laws_present_test() ->
    Decls = load_laws_decls(),
    ?assertEqual(28, length(Decls)),
    %% All should be transform_decl
    TransformDecls = [D || D = {transform_decl, _, _, _, _} <- Decls],
    ?assertEqual(28, length(TransformDecls)).

%% =============================================================================
%% Stage 2 - Executable Concrete Law Suites
%% =============================================================================

maybe_law_suite_executes_test() ->
    Results = run_named_suite(maybe_suite_source(), maybeSuite, maybe_law_env()),
    ?assertEqual(5, maps:get(total, Results)),
    ?assertEqual(5, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

either_law_suite_executes_test() ->
    Results = run_named_suite(either_suite_source(), eitherSuite, either_law_env()),
    ?assertEqual(5, maps:get(total, Results)),
    ?assertEqual(5, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

list_law_suite_executes_test() ->
    Results = run_named_suite(list_suite_source(), listSuite, list_law_env()),
    ?assertEqual(6, maps:get(total, Results)),
    ?assertEqual(6, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

applicative_law_suite_executes_test() ->
    Results = run_named_suite(applicative_suite_source(), applicativeSuite, maybe_law_env()),
    ?assertEqual(4, maps:get(total, Results)),
    ?assertEqual(4, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

accumulator_law_suite_executes_test() ->
    Results = run_named_suite(accumulator_suite_source(), accumulatorSuite, list_law_env()),
    ?assertEqual(2, maps:get(total, Results)),
    ?assertEqual(2, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

orderable_law_suite_executes_test() ->
    Results = run_named_suite(orderable_suite_source(), orderableSuite, int_order_law_env()),
    ?assertEqual(3, maps:get(total, Results)),
    ?assertEqual(3, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

broken_fixture_reports_law_failure_test() ->
    Results = run_named_suite(
        broken_maybe_suite_source(),
        brokenMaybeSuite,
        broken_maybe_law_env()
    ),
    ?assertEqual(1, maps:get(total, Results)),
    ?assertEqual(0, maps:get(passed, Results)),
    ?assertEqual(1, maps:get(failed, Results)),
    ?assertEqual(
        [{fail, "pipeline left identity", "Law verification failed"}],
        maps:get(results, Results)
    ).

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

assert_law_arity(Decls, LawName, ExpectedArity) ->
    [LawDecl] = [D || D = {transform_decl, Name, _, _, _} <- Decls, Name =:= LawName],
    ?assertEqual(ExpectedArity, get_param_count(LawDecl)).

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

verify_additional_promoted_law_signatures_test() ->
    Decls = load_laws_decls(),
    assert_law_arity(Decls, comparableTransitivityLaw, 3),
    assert_law_arity(Decls, accumulatorLeftIdentityLaw, 1),
    assert_law_arity(Decls, accumulatorRightIdentityLaw, 1),
    assert_law_arity(Decls, applicativeIdentityLaw, 1),
    assert_law_arity(Decls, applicativeCompositionLaw, 3),
    assert_law_arity(Decls, applicativeHomomorphismLaw, 2),
    assert_law_arity(Decls, applicativeInterchangeLaw, 2),
    assert_law_arity(Decls, orderableAntisymmetryLaw, 2),
    assert_law_arity(Decls, orderableTransitivityLaw, 3),
    assert_law_arity(Decls, orderableTotalityLaw, 2).

%% 1.5.4.4h - Verify combinerAssociativityLaw expected signature
%% combinerAssociativityLaw : a -> a -> a -> Bool (3 parameters)
verify_combiner_associativity_law_signature_test() ->
    Decls = load_laws_decls(),
    [CombAssocLaw] = [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
    %% Should take 3 parameters (a, b, c)
    ?assertEqual(3, get_param_count(CombAssocLaw)).

verify_system_and_flow_law_signatures_test() ->
    Decls = load_laws_decls(),
    assert_law_arity(Decls, systemLeftIdentityLaw, 1),
    assert_law_arity(Decls, systemRightIdentityLaw, 1),
    assert_law_arity(Decls, systemAssociativityLaw, 3),
    assert_law_arity(Decls, flowLiftIdentityLaw, 0),
    assert_law_arity(Decls, flowLiftCompositionLaw, 2),
    assert_law_arity(Decls, flowFirstLiftLaw, 1),
    assert_law_arity(Decls, flowFirstCompositionLaw, 2),
    assert_law_arity(Decls, flowFirstProjectionLaw, 1),
    assert_law_arity(Decls, flowFirstNaturalityLaw, 2),
    assert_law_arity(Decls, flowFirstAssociativityLaw, 1).

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

      {"comparableTransitivityLaw takes 3 arguments (a, b, c)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, comparableTransitivityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"combinerAssociativityLaw takes 3 arguments (a, b, c)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"accumulatorLeftIdentityLaw takes 1 argument (a)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, accumulatorLeftIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"accumulatorRightIdentityLaw takes 1 argument (a)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, accumulatorRightIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"applicativeIdentityLaw takes 1 argument (fa)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, applicativeIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"applicativeCompositionLaw takes 3 arguments (ff, fg, fa)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, applicativeCompositionLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"applicativeHomomorphismLaw takes 2 arguments (f, x)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, applicativeHomomorphismLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"applicativeInterchangeLaw takes 2 arguments (ff, x)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, applicativeInterchangeLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"orderableAntisymmetryLaw takes 2 arguments (a, b)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, orderableAntisymmetryLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"orderableTransitivityLaw takes 3 arguments (a, b, c)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, orderableTransitivityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"orderableTotalityLaw takes 2 arguments (a, b)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, orderableTotalityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"systemLeftIdentityLaw takes 1 argument (f)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, systemLeftIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"systemRightIdentityLaw takes 1 argument (f)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, systemRightIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"systemAssociativityLaw takes 3 arguments (f, g, h)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, systemAssociativityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(3, length(Patterns))
       end},

      {"flowLiftIdentityLaw takes 0 arguments",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowLiftIdentityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(0, length(Patterns))
       end},

      {"flowLiftCompositionLaw takes 2 arguments (f, g)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowLiftCompositionLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"flowFirstLiftLaw takes 1 argument (f)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowFirstLiftLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"flowFirstCompositionLaw takes 2 arguments (f, g)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowFirstCompositionLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"flowFirstProjectionLaw takes 1 argument (f)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowFirstProjectionLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end},

      {"flowFirstNaturalityLaw takes 2 arguments (f, g)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowFirstNaturalityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(2, length(Patterns))
       end},

      {"flowFirstAssociativityLaw takes 1 argument (f)",
       fun() ->
           Decls = load_laws_decls(),
           [{transform_decl, _, _, Clauses, _}] =
               [D || D = {transform_decl, flowFirstAssociativityLaw, _, _, _} <- Decls],
           [{transform_clause, Patterns, _, _, _}] = Clauses,
           ?assertEqual(1, length(Patterns))
       end}
     ]}.
