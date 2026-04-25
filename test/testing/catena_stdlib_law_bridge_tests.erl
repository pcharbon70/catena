%% @doc Focused tests for the stdlib-to-proptest law bridge.
-module(catena_stdlib_law_bridge_tests).

-include_lib("eunit/include/eunit.hrl").

known_instances_test() ->
    ?assertEqual(
        ['maybe', 'either', 'list', 'int'],
        catena_stdlib_law_bridge:known_instances()
    ).

supported_traits_use_catena_vocabulary_test() ->
    ?assertEqual(
        {ok, [functor, applicative, monad, setoid]},
        catena_stdlib_law_bridge:supported_traits(<<"Maybe">>)
    ),
    ?assertEqual(
        {ok, ord},
        catena_stdlib_law_bridge:normalize_trait(<<"Orderable">>)
    ).

maybe_mapper_suite_passes_test() ->
    Result = catena_stdlib_law_bridge:run_trait_suite(
        <<"Maybe">>,
        [<<"Mapper">>],
        #{num_tests => 15, seed => 7}
    ),
    ?assertEqual(2, maps:get(total, Result)),
    ?assertEqual(2, maps:get(passed, Result)),
    ?assertEqual(0, maps:get(failed, Result)),
    ?assertEqual([functor], maps:get(traits, Result)),
    ?assertEqual(
        [mapperIdentityLaw, mapperCompositionLaw],
        [maps:get(stdlib_law, LawResult) || LawResult <- maps:get(results, Result)]
    ).

either_pipeline_suite_includes_inherited_traits_test() ->
    Result = catena_stdlib_law_bridge:run_trait_suite(
        'either',
        [pipeline],
        #{num_tests => 12, seed => 19}
    ),
    ?assertEqual([functor, applicative, monad], maps:get(traits, Result)),
    ?assertEqual(9, maps:get(total, Result)),
    ?assertEqual(9, maps:get(passed, Result)),
    ?assertEqual(0, maps:get(failed, Result)).

list_accumulator_suite_passes_test() ->
    Result = catena_stdlib_law_bridge:run_trait_suite(
        'list',
        [accumulator],
        #{num_tests => 12, seed => 23}
    ),
    ?assertEqual([semigroup, monoid], maps:get(traits, Result)),
    ?assertEqual(3, maps:get(total, Result)),
    ?assertEqual(3, maps:get(passed, Result)),
    ?assertEqual(0, maps:get(failed, Result)).

int_orderable_suite_passes_test() ->
    Result = catena_stdlib_law_bridge:run_trait_suite(
        "Int",
        ["Orderable"],
        #{num_tests => 20, seed => 31}
    ),
    ?assertEqual([setoid, ord], maps:get(traits, Result)),
    ?assertEqual(6, maps:get(total, Result)),
    ?assertEqual(6, maps:get(passed, Result)),
    ?assertEqual(0, maps:get(failed, Result)).

unsupported_trait_reports_explicit_error_test() ->
    ?assertEqual(
        {error, {unsupported_traits, 'maybe', [monoid]}},
        catena_stdlib_law_bridge:run_trait_suite('maybe', [monoid], #{})
    ).

concrete_and_generic_mapper_paths_align_for_maybe_test() ->
    Concrete = run_named_suite(
        maybe_mapper_suite_source(),
        maybeMapperSuite,
        maybe_runtime_env()
    ),
    Generic = catena_stdlib_law_bridge:run_trait_suite(
        'maybe',
        [mapper],
        #{num_tests => 12, seed => 11}
    ),
    ?assertEqual(2, maps:get(total, Concrete)),
    ?assertEqual(2, maps:get(passed, Concrete)),
    ?assertEqual(2, maps:get(total, Generic)),
    ?assertEqual(2, maps:get(passed, Generic)),
    ?assertEqual(
        [mapperIdentityLaw, mapperCompositionLaw],
        [maps:get(stdlib_law, LawResult) || LawResult <- maps:get(results, Generic)]
    ).

concrete_and_generic_accumulator_paths_align_for_list_test() ->
    Concrete = run_named_suite(
        list_accumulator_suite_source(),
        listAccumulatorSuite,
        list_runtime_env()
    ),
    Generic = catena_stdlib_law_bridge:run_trait_suite(
        'list',
        [accumulator],
        #{num_tests => 12, seed => 29}
    ),
    ?assertEqual(3, maps:get(total, Concrete)),
    ?assertEqual(3, maps:get(passed, Concrete)),
    ?assertEqual(3, maps:get(total, Generic)),
    ?assertEqual(3, maps:get(passed, Generic)),
    ?assertEqual(
        [combinerAssociativityLaw, accumulatorLeftIdentityLaw, accumulatorRightIdentityLaw],
        [maps:get(stdlib_law, LawResult) || LawResult <- maps:get(results, Generic)]
    ).

%%====================================================================
%% Concrete suite helpers
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

list_runtime_env() ->
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

list_accumulator_suite_source() ->
    "module ListAccumulatorLawSuite\n"
    "export transform listAccumulatorSuite\n"
    "import Prelude\n"
    "import Test\n"
    "import Laws\n"
    "transform listAccumulatorSuite = suite \"List Accumulator Laws\" [\n"
    "  verify \"combiner associativity\" (fn u -> combinerAssociativityLaw [1] [2] [3]),\n"
    "  verify \"left identity\" (fn u -> accumulatorLeftIdentityLaw [1, 2]),\n"
    "  verify \"right identity\" (fn u -> accumulatorRightIdentityLaw [1, 2])\n"
    "]\n".
