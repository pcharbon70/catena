%%%-------------------------------------------------------------------
%%% @doc Phase 2 integration contract for resumption source forms.
%%%
%%% These tests prove the source-to-normalized-AST boundary and its
%%% fail-closed handoffs. They deliberately do not claim that explicit
%%% resumptions have types or executable selective-CPS lowering yet.
%%%-------------------------------------------------------------------
-module(catena_delimited_resumption_phase2_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/error/catena_error.hrl").

explicit_control_source_reaches_normalized_ast_test() ->
    Source =
        "transform run = handle perform State.put(1) then {\n"
        "  State { put(value) with k -> resume(k, value + 1) }\n"
        "}",
    {ok, Normalized} = analyze(Source),
    ?assertMatch(
        {
            operation_case,
            put,
            [{pat_var, value, _}],
            {resumption_binder, k, _},
            {
                resume_expr,
                {var, k, _},
                {binary_op, plus, {var, value, _}, {literal, 1, integer, _}, _},
                _
            },
            _
        },
        only_operation_case(Normalized)
    ).

value_handler_normalizes_to_single_synthetic_tail_resume_test() ->
    Source =
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() -> let answer = 41 in answer + 1 }\n"
        "}",
    {ok, Normalized} = analyze(Source),
    {
        operation_case,
        choose,
        [],
        {resumption_binder, Binder, Origin},
        {resume_expr, {var, Binder, Origin}, Value, Origin},
        CaseLocation
    } = only_operation_case(Normalized),
    ?assertMatch(
        {synthetic, value_handler_auto_resume, CaseLocation},
        Origin
    ),
    ?assertMatch(
        {
            let_expr,
            _,
            {binary_op, plus, {var, answer, _}, {literal, 1, integer, _}, _},
            _
        },
        Value
    ),
    ?assertEqual(
        1,
        count_tag(resume_expr, only_operation_case(Normalized))
    ).

parse_print_parse_and_normalize_stability_test_() ->
    Sources = [
        "handle perform Choice.choose() then { "
        "Choice { choose() with k -> resume(k, 1) } }",
        "handle perform State.put(1) then { "
        "State { put(value) with k -> resume(k, value + 1) } }",
        "handle (handle perform Inner.read() then { "
        "Inner { read() with inner -> resume(inner, 1) } }) then { "
        "Outer { wrap(value) with outer -> resume(outer, value) } }",
        "handle perform Choice.choose() then { Choice { choose() -> 1 } }"
    ],
    [
        ?_test(assert_roundtrip_and_normalization_stability(Source))
        || Source <- Sources
    ].

nested_binder_shadowing_remains_lexical_test() ->
    Source =
        "transform run = handle 1 then {\n"
        "  Outer { outer() with k ->\n"
        "    handle 2 then {\n"
        "      Inner { inner() with k -> resume(k, 3) }\n"
        "    }\n"
        "  }\n"
        "}",
    ?assertMatch({ok, _}, analyze(Source)).

keyword_boundaries_and_negative_parser_paths_test() ->
    {ok, BoundaryTokens} =
        catena_lexer:tokenize("within resumed with_value resumeLater"),
    ?assertEqual(
        [lower_ident, lower_ident, lower_ident, lower_ident],
        [element(1, Token) || Token <- BoundaryTokens]
    ),
    ?assertMatch(
        {error, _},
        parse(
            "transform run = handle 1 then { "
            "Choice { choose() with K -> 1 } }"
        )
    ),
    ?assert(has_resume_arity_error("transform run = resume(k)")).

parser_recovery_preserves_resumption_diagnostic_test() ->
    Source =
        "transform broken = resume(k)\n"
        "transform valid = 1\n",
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {error, Errors} = catena_parser_wrapper:parse_tokens(Tokens),
    ?assert(length(Errors) >= 1),
    ?assert(
        lists:any(
            fun(#error{message = Message}) ->
                string:find(
                    lists:flatten(Message),
                    "resume expects exactly two operands"
                ) =/= nomatch
            end,
            Errors
        )
    ).

invalid_scope_and_explicit_typed_boundary_fail_closed_test() ->
    ?assertMatch(
        {error, {resumption_binder_scope, #{target := k}}},
        analyze("transform run = resume(k, 1)")
    ),
    ?assertMatch(
        {error, {missing_resumption_lowering, #{
            stage := type_inference,
            construct := operation_case,
            mode := explicit_control
        }}},
        catena_compile:compile_string(
            "transform run = handle 1 then { "
            "Choice { choose() with k -> resume(k, 1) } }"
        )
    ).

normalized_explicit_control_cannot_leak_to_backend_test() ->
    {ok, Normalized} = analyze(
        "transform run = handle handled then { "
        "Choice { choose() with k -> resume(k, 1) } }"
    ),
    ?assertThrow(
        {backend_error, missing_resumption_lowering, _},
        catena_codegen_lower:lower_module(Normalized)
    ),
    ?assertThrow(
        {backend_error, missing_resumption_lowering, _},
        catena_codegen_erase:erase_expr(transform_body(Normalized))
    ).

value_handler_keeps_normalized_authority_and_executes_compatibly_test() ->
    Source =
        "module PhaseTwoValueCompatibility\n"
        "export transform run\n"
        "effect Reader\n"
        "operation read : Int\n"
        "end\n"
        "transform run ignored = handle perform Reader.read() then {\n"
        "  Reader { read() -> 42 }\n"
        "}\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    ?assertMatch(
        {
            operation_case,
            read,
            [],
            {
                resumption_binder,
                _,
                {synthetic, value_handler_auto_resume, _}
            },
            {resume_expr, _, {literal, 42, integer, _}, _},
            _
        },
        only_operation_case(
            catena_compilation_unit:normalized_ast(Unit)
        )
    ),
    with_loaded_module(Source, fun() ->
        ?assertEqual(42, 'PhaseTwoValueCompatibility':run(0))
    end).

assert_roundtrip_and_normalization_stability(SourceExpr) ->
    OriginalSource = "transform run = " ++ SourceExpr,
    OriginalParsed = parse_ok(OriginalSource),
    PrintedExpr = catena_ast_pp:pp_expr(transform_body(OriginalParsed)),
    Reparsed = parse_ok("transform run = " ++ PrintedExpr),
    ?assert(
        catena_ast_utils:equivalent(OriginalParsed, Reparsed),
        #{source => SourceExpr, printed => PrintedExpr}
    ),
    {ok, OriginalNormalized} = catena_semantic:analyze(OriginalParsed),
    {ok, ReparsedNormalized} = catena_semantic:analyze(Reparsed),
    ?assert(
        catena_ast_utils:equivalent(
            OriginalNormalized,
            ReparsedNormalized
        ),
        #{source => SourceExpr, printed => PrintedExpr}
    ).

with_loaded_module(Source, Assertion) ->
    {ok, Artifact} = catena_compile:compile_string_to_beam(Source),
    Module = maps:get(runtime_module, Artifact),
    unload(Module),
    try
        {module, Module} = code:load_binary(
            Module,
            "delimited-resumption-phase2-memory",
            maps:get(beam, Artifact)
        ),
        Assertion()
    after
        unload(Module),
        case catena_effect_system:is_initialized() of
            true -> catena_effect_system:stop_runtime();
            false -> ok
        end
    end.

unload(Module) ->
    code:purge(Module),
    code:delete(Module).

analyze(Source) ->
    case parse(Source) of
        {ok, AST} -> catena_semantic:analyze(AST);
        {error, _} = Error -> Error
    end.

parse_ok(Source) ->
    {ok, AST} = parse(Source),
    AST.

parse(Source) ->
    {ok, Tokens} = catena_lexer:tokenize(Source),
    catena_parser:parse(Tokens).

has_resume_arity_error(Source) ->
    case parse(Source) of
        {error, {_Line, catena_parser, Message}} ->
            string:find(
                lists:flatten(Message),
                "resume expects exactly two operands"
            ) =/= nomatch;
        _ ->
            false
    end.

transform_body({
    module,
    _Name,
    _Exports,
    _Imports,
    [{
        transform_decl,
        run,
        _Type,
        [{transform_clause, _Patterns, _Guard, Body, _ClauseLocation}],
        _DeclarationLocation
    }],
    _ModuleLocation
}) ->
    Body.

only_operation_case(AST) ->
    [OperationCase] = [
        Operation
        || {handler_clause, _Effect, Operations, _HandlerLocation} <-
            collect_tag(handler_clause, AST),
           Operation <- Operations
    ],
    OperationCase.

count_tag(Tag, AST) ->
    length(collect_tag(Tag, AST)).

collect_tag(Tag, Tuple) when is_tuple(Tuple) ->
    Here = case element(1, Tuple) of
        Tag -> [Tuple];
        _ -> []
    end,
    Here ++ lists:append([
        collect_tag(Tag, Element)
        || Element <- tuple_to_list(Tuple)
    ]);
collect_tag(Tag, List) when is_list(List) ->
    lists:append([collect_tag(Tag, Element) || Element <- List]);
collect_tag(_Tag, _Other) ->
    [].
