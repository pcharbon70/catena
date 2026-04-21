%%%-------------------------------------------------------------------
%%% @doc Integration tests for Phase 14.2 compiler/type integration.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_phase14_type_integration_tests).

-include_lib("eunit/include/eunit.hrl").

loc() ->
    {location, 1, 1}.

perform_transform_gets_effectful_function_type_test() ->
    Decls = [
        {transform_decl, greet, undefined, [
            {transform_clause,
                [{pat_var, name, loc()}],
                undefined,
                {perform_expr, 'IO', print, [{var, name, loc()}], loc()},
                loc()}
        ], loc()}
    ],
    {ok, Env} = catena_compile:build_type_env(Decls),
    {ok, {typed_module, _Name, [TypedDecl], _TypedEnv}} =
        catena_compile:type_check_module({module, demo, [], [], Decls, loc()}, Env),
    {typed_transform, greet, Type, _Clauses, _Loc} = TypedDecl,
    {ok, Effects} = catena_types:extract_function_effects(Type),
    ?assertEqual({effect_set, ['IO']}, Effects).

handled_transform_becomes_pure_test() ->
    Decls = [
        {transform_decl, safe_read, undefined, [
            {transform_clause,
                [{pat_var, path, loc()}],
                undefined,
                {handle_expr,
                    {perform_expr, 'IO', read, [{var, path, loc()}], loc()},
                    [
                        {handler_clause, 'IO', [
                            {operation_case, read, [{pat_var, p, loc()}], {var, p, loc()}, loc()}
                        ], loc()}
                    ],
                    loc()},
                loc()}
        ], loc()}
    ],
    {ok, Env} = catena_compile:build_type_env(Decls),
    {ok, {typed_module, _Name, [TypedDecl], _TypedEnv}} =
        catena_compile:type_check_module({module, demo, [], [], Decls, loc()}, Env),
    {typed_transform, safe_read, Type, _Clauses, _Loc} = TypedDecl,
    {ok, Effects} = catena_types:extract_function_effects(Type),
    ?assertEqual({effect_set, []}, Effects).

declared_pure_transform_rejects_effectful_body_test() ->
    DeclaredType = {type_fun, {type_con, string, loc()}, {type_con, string, loc()}, loc()},
    Decls = [
        {transform_decl, bad, DeclaredType, [
            {transform_clause,
                [{pat_var, path, loc()}],
                undefined,
                {perform_expr, 'IO', read, [{var, path, loc()}], loc()},
                loc()}
        ], loc()}
    ],
    {ok, Env} = catena_compile:build_type_env(Decls),
    Result = catena_compile:type_check_module({module, demo, [], [], Decls, loc()}, Env),
    ?assertMatch({error, {effect_mismatch, bad, _, _}}, Result).
