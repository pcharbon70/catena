%%%-------------------------------------------------------------------
%%% @doc Tests for Type Erasure (Task 1.3.3)
%%%
%%% Tests type erasure from Catena AST for code generation.
%%% @end
%%%-------------------------------------------------------------------
-module(catena_codegen_erase_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

loc() ->
    {location, 1, 1}.

%%====================================================================
%% Type Annotation Removal Tests (1.3.3.1)
%%====================================================================

type_annotation_removal_test_() ->
    [
        ?_test(test_strip_typed_expr()),
        ?_test(test_strip_type_ascription()),
        ?_test(test_strip_typed_lambda()),
        ?_test(test_strip_typed_var_pattern()),
        ?_test(test_preserve_untyped_expr())
    ].

test_strip_typed_expr() ->
    %% {typed_expr, expr, type, loc} -> expr
    Expr = {typed_expr, {var, x, loc()}, {tcon, int}, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual({var, x, loc()}, Result).

test_strip_type_ascription() ->
    %% (x : Int) -> x
    Expr = {type_ascription, {var, x, loc()}, {tcon, int}, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual({var, x, loc()}, Result).

test_strip_typed_lambda() ->
    %% (\x : Int -> x) : Int -> Int -> \x -> x
    Expr = {typed_lambda,
        [{pat_var, x, loc()}],
        {tcon, int},
        {var, x, loc()},
        loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertMatch({lambda, [{pat_var, x, _}], {var, x, _}, _}, Result).

test_strip_typed_var_pattern() ->
    %% (x : Int) -> x
    Pattern = {pat_typed_var, x, {tcon, int}, loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual({pat_var, x, loc()}, Result).

test_preserve_untyped_expr() ->
    %% Untyped expressions preserved
    Expr = {var, x, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

%%====================================================================
%% Expression Erasure Tests
%%====================================================================

expression_erasure_test_() ->
    [
        ?_test(test_erase_literal()),
        ?_test(test_erase_variable()),
        ?_test(test_erase_app()),
        ?_test(test_erase_let()),
        ?_test(test_erase_lambda()),
        ?_test(test_erase_if()),
        ?_test(test_erase_binary_op()),
        ?_test(test_erase_list()),
        ?_test(test_erase_tuple()),
        ?_test(test_erase_constructor()),
        ?_test(test_erase_nested_typed())
    ].

test_erase_literal() ->
    Expr = {literal, integer, 42, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_variable() ->
    Expr = {var, x, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_app() ->
    Expr = {app, {var, f, loc()}, [{var, x, loc()}], loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_let() ->
    Expr = {let_expr, [
        {{pat_var, x, loc()}, {literal, integer, 1, loc()}}
    ], {var, x, loc()}, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_lambda() ->
    Expr = {lambda, [{pat_var, x, loc()}], {var, x, loc()}, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_if() ->
    Expr = {if_expr,
        {literal, bool, true, loc()},
        {literal, integer, 1, loc()},
        {literal, integer, 0, loc()},
        loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_binary_op() ->
    Expr = {binary_op, '+', {var, x, loc()}, {var, y, loc()}, loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_list() ->
    Expr = {list_expr, [{var, x, loc()}, {var, y, loc()}], loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_tuple() ->
    Expr = {tuple_expr, [{var, x, loc()}, {var, y, loc()}], loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_constructor() ->
    Expr = {constructor, 'Some', [{var, x, loc()}], loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertEqual(Expr, Result).

test_erase_nested_typed() ->
    %% Nested typed expression
    Expr = {app,
        {typed_expr, {var, f, loc()}, {fun_type, a, b, pure}, loc()},
        [{typed_expr, {var, x, loc()}, {tcon, int}, loc()}],
        loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    Expected = {app, {var, f, loc()}, [{var, x, loc()}], loc()},
    ?assertEqual(Expected, Result).

%%====================================================================
%% Pattern Erasure Tests
%%====================================================================

pattern_erasure_test_() ->
    [
        ?_test(test_erase_pat_var()),
        ?_test(test_erase_pat_wildcard()),
        ?_test(test_erase_pat_literal()),
        ?_test(test_erase_pat_constructor()),
        ?_test(test_erase_pat_list()),
        ?_test(test_erase_pat_tuple()),
        ?_test(test_erase_pat_as())
    ].

test_erase_pat_var() ->
    Pattern = {pat_var, x, loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

test_erase_pat_wildcard() ->
    Pattern = {pat_wildcard, loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

test_erase_pat_literal() ->
    Pattern = {pat_literal, 42, integer, loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

test_erase_pat_constructor() ->
    Pattern = {pat_constructor, 'Some', [{pat_var, x, loc()}], loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

test_erase_pat_list() ->
    Pattern = {pat_list, [{pat_var, x, loc()}], loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

test_erase_pat_tuple() ->
    Pattern = {pat_tuple, [{pat_var, x, loc()}, {pat_var, y, loc()}], loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

test_erase_pat_as() ->
    Pattern = {pat_as, x, {pat_constructor, 'Some', [{pat_var, y, loc()}], loc()}, loc()},
    Result = catena_codegen_erase:erase_pattern(Pattern),
    ?assertEqual(Pattern, Result).

%%====================================================================
%% Dictionary Passing Tests (1.3.3.2)
%%====================================================================

dictionary_passing_test_() ->
    [
        ?_test(test_transform_simple_trait_call()),
        ?_test(test_transform_with_constraints()),
        ?_test(test_build_dictionary()),
        ?_test(test_trait_method_erasure())
    ].

test_transform_simple_trait_call() ->
    %% Transform trait method call to dictionary lookup
    Expr = {lambda, [{pat_var, x, loc()}], {var, x, loc()}, loc()},
    Constraints = [{trait, 'Show', [{tvar, 1}], loc()}],
    Result = catena_codegen_erase:transform_trait_calls(Expr, Constraints),
    %% Should add dictionary parameter
    ?assertMatch({lambda, [{pat_var, 'Show_dict', _}, {pat_var, x, _}], _, _}, Result).

test_transform_with_constraints() ->
    %% Transform expression with multiple constraints
    Body = {var, x, loc()},
    Constraints = [
        {trait, 'Show', [{tvar, 1}], loc()},
        {trait, 'Eq', [{tvar, 1}], loc()}
    ],
    Result = catena_codegen_erase:transform_trait_calls(Body, Constraints),
    %% Should wrap in lambda with both dictionary parameters
    ?assertMatch({lambda, [{pat_var, 'Show_dict', _}, {pat_var, 'Eq_dict', _}], _, _}, Result).

test_build_dictionary() ->
    %% Build dictionary reference
    Result = catena_codegen_erase:build_dictionary('Show', [{tcon, int}]),
    ?assertMatch({var, 'Show_int_dict', _}, Result).

test_trait_method_erasure() ->
    %% Trait method call should become dictionary lookup
    Expr = {trait_method, 'Show', show, [{tvar, 1}], [{var, x, loc()}], loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    %% Should become: Show_dict.show(x)
    ?assertMatch({app, {record_access, {var, 'Show_dict', _}, show, _}, [{var, x, _}], _}, Result).

%%====================================================================
%% Polymorphism Handling Tests (1.3.3.3)
%%====================================================================

polymorphism_test_() ->
    [
        ?_test(test_monomorphize_simple()),
        ?_test(test_uniform_representation())
    ].

test_monomorphize_simple() ->
    %% Monomorphize strips types (BEAM uses uniform representation)
    Expr = {typed_expr, {var, x, loc()}, {tcon, int}, loc()},
    Result = catena_codegen_erase:monomorphize(Expr, #{}),
    ?assertEqual({var, x, loc()}, Result).

test_uniform_representation() ->
    %% All values have same runtime representation
    Expr = {app, {var, f, loc()}, [{var, x, loc()}], loc()},
    Result = catena_codegen_erase:monomorphize(Expr, #{}),
    ?assertEqual(Expr, Result).

%%====================================================================
%% Verification Tests (1.3.3.4)
%%====================================================================

verification_test_() ->
    [
        ?_test(test_verify_no_types_remain()),
        ?_test(test_verify_types_detected()),
        ?_test(test_verify_structure_preserved())
    ].

test_verify_no_types_remain() ->
    Original = {typed_expr, {var, x, loc()}, {tcon, int}, loc()},
    Erased = catena_codegen_erase:erase_expr(Original),
    Result = catena_codegen_erase:verify_erasure(Original, Erased),
    ?assertEqual(ok, Result).

test_verify_types_detected() ->
    %% Should detect if types weren't erased
    Original = {var, x, loc()},
    NotErased = {typed_expr, {var, x, loc()}, {tcon, int}, loc()},
    Result = catena_codegen_erase:verify_erasure(Original, NotErased),
    ?assertEqual({error, type_annotations_remain}, Result).

test_verify_structure_preserved() ->
    %% Verify structure preservation
    Original = {app, {var, f, loc()}, [{var, x, loc()}], loc()},
    Erased = catena_codegen_erase:erase_expr(Original),
    Result = catena_codegen_erase:verify_erasure(Original, Erased),
    ?assertEqual(ok, Result).

%%====================================================================
%% Module Erasure Tests
%%====================================================================

module_erasure_test_() ->
    [
        ?_test(test_erase_transform_decl()),
        ?_test(test_erase_typed_transform()),
        ?_test(test_erase_type_decl()),
        ?_test(test_erase_module())
    ].

test_erase_transform_decl() ->
    %% Transform declaration preserved without types
    Module = {module, test_mod, [], [
        {transform, foo, [{pat_var, x, loc()}], {var, x, loc()}, loc()}
    ], loc()},
    {module, _, _, [Decl], _} = catena_codegen_erase:erase_module(Module),
    ?assertMatch({transform, foo, [{pat_var, x, _}], {var, x, _}, _}, Decl).

test_erase_typed_transform() ->
    %% Typed transform loses type signature
    Module = {module, test_mod, [], [
        {transform_typed, foo, {fun_type, int, int, pure},
            [{pat_var, x, loc()}], {var, x, loc()}, loc()}
    ], loc()},
    {module, _, _, [Decl], _} = catena_codegen_erase:erase_module(Module),
    ?assertMatch({transform, foo, [{pat_var, x, _}], {var, x, _}, _}, Decl).

test_erase_type_decl() ->
    %% Type declarations completely erased
    %% type_decl tuple is {type_decl, Name, TypeVars, Constructors, Derives, Loc}
    Module = {module, test_mod, [], [
        {type_decl, 'Maybe', [a], [{'None', []}, {'Some', [a]}], [], loc()}
    ], loc()},
    {module, _, _, Decls, _} = catena_codegen_erase:erase_module(Module),
    ?assertEqual([erased], Decls).

test_erase_module() ->
    %% Full module erasure
    Module = {module, test_mod, [{foo, 1}], [
        {transform_typed, foo, {fun_type, int, int, pure},
            [{pat_typed_var, x, {tcon, int}, loc()}],
            {typed_expr, {var, x, loc()}, {tcon, int}, loc()},
            loc()}
    ], loc()},
    {module, Name, Exports, Decls, _} = catena_codegen_erase:erase_module(Module),
    ?assertEqual(test_mod, Name),
    ?assertEqual([{foo, 1}], Exports),
    ?assertEqual(1, length(Decls)),
    [Decl] = Decls,
    %% Verify all types stripped
    ?assertMatch({transform, foo, [{pat_var, x, _}], {var, x, _}, _}, Decl).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    [
        ?_test(test_complex_erasure()),
        ?_test(test_effect_erasure())
    ].

test_complex_erasure() ->
    %% Complex expression with multiple type annotations
    Expr = {let_expr, [
        {{pat_typed_var, x, {tcon, int}, loc()},
         {typed_expr, {literal, integer, 42, loc()}, {tcon, int}, loc()}}
    ],
    {typed_expr,
        {app, {var, f, loc()}, [{var, x, loc()}], loc()},
        {tcon, string},
        loc()},
    loc()},

    Result = catena_codegen_erase:erase_expr(Expr),

    %% All types should be stripped
    {let_expr, Bindings, Body, _} = Result,
    [{Pat, Val}] = Bindings,
    ?assertMatch({pat_var, x, _}, Pat),
    ?assertMatch({literal, integer, 42, _}, Val),
    ?assertMatch({app, {var, f, _}, [{var, x, _}], _}, Body).

test_effect_erasure() ->
    %% Effect operations don't have types to erase
    Expr = {perform_expr, 'IO', print, [
        {typed_expr, {literal, string, <<"hello">>, loc()}, {tcon, string}, loc()}
    ], loc()},
    Result = catena_codegen_erase:erase_expr(Expr),
    ?assertMatch({perform_expr, 'IO', print, [{literal, string, <<"hello">>, _}], _}, Result).
