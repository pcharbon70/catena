%% @doc End-to-end workflow validation for the converged property/law path.
-module(catena_workflow_validation_tests).

-include_lib("eunit/include/eunit.hrl").

legacy_property_decl_is_reproducible_through_runner_test() ->
    PropDecl = {property_decl, "less than five",
        {property_forall,
            [{n, 'Natural'}],
            {binary_op, lt, {var, n, {line, 1}}, {literal, 5, integer, {line, 1}}, {line, 1}},
            {line, 1}},
        {line, 1}},
    Opts = #{property_iterations => 25, property_seed => 41},
    Result1 = catena_test_runner:run_test(PropDecl, #{}, Opts),
    Result2 = catena_test_runner:run_test(PropDecl, #{}, Opts),
    ?assertEqual(Result1, Result2).

first_class_property_failure_formats_for_ci_test() ->
    PropertyValue = #{
        name => "phase4 workflow property",
        body => fun(_Unit) ->
            {for_all, {gen_constant_int, 1}, fun(_) -> false end}
        end,
        config => #{iterations => 2, seed => {some, 13}, labels => ["workflow"]}
    },
    Result = catena_test_runner:run_property_value(PropertyValue),
    Formatted = catena_test_runner:format_result(Result),
    ?assert(string:find(Formatted, "phase4 workflow property") =/= nomatch),
    ?assert(string:find(Formatted, "Seed") =/= nomatch),
    ?assert(string:find(Formatted, "workflow") =/= nomatch).

stdlib_law_suite_executes_through_compiler_runtime_pipeline_test() ->
    Results = run_named_suite(maybe_mapper_suite_source(), maybeMapperSuite, maybe_runtime_env()),
    ?assertEqual(2, maps:get(total, Results)),
    ?assertEqual(2, maps:get(passed, Results)),
    ?assertEqual(0, maps:get(failed, Results)).

generic_law_suite_failure_formats_actionable_output_test() ->
    SuiteValue = #{
        name => "Generic Law Workflow",
        tests => [
            #{
                name => "unsupported maybe accumulator",
                run => fun(_Unit) -> passed end,
                lawCheck => {some, #{
                    instance => <<"Maybe">>,
                    traits => [<<"Accumulator">>],
                    config => #{iterations => 10, seed => {some, 7}, labels => []}
                }}
            }
        ],
        properties => []
    },
    Results = catena_test_runner:run_suite_value(SuiteValue),
    Formatted = catena_test_runner:format_results(Results),
    ?assertEqual(1, maps:get(failed, Results)),
    ?assert(string:find(Formatted, "unsupported maybe accumulator") =/= nomatch),
    ?assert(string:find(Formatted, "unsupported_traits") =/= nomatch).

%%====================================================================
%% Runtime helper pipeline
%%====================================================================

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

maybe_runtime_env() ->
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

maybe_mapper_suite_source() ->
    "module MaybeMapperLawSuite\n"
    "export transform maybeMapperSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform inc x = x + 1\n"
    "transform double x = x * 2\n"
    "transform maybeMapperSuite = suite \"Maybe Mapper Laws\" [\n"
    "  verify \"mapper identity\" (fn u -> mapperIdentityLaw (Some 42)),\n"
    "  verify \"mapper composition\" (fn u -> mapperCompositionLaw inc double (Some 5))\n"
    "]\n".
