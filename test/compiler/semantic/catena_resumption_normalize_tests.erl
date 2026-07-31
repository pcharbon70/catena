-module(catena_resumption_normalize_tests).

-include_lib("eunit/include/eunit.hrl").

value_handler_normalizes_to_collision_free_tail_resume_test() ->
    Source =
        "transform run = handle perform State.get() then {\n"
        "  State { get() -> (seed + 1) }\n"
        "}",
    {ok, Analyzed} = analyze_source(Source),
    Operation = only_operation_case(Analyzed),
    ?assertMatch(
        {
            operation_case,
            get,
            [],
            {
                resumption_binder,
                _,
                {synthetic, value_handler_auto_resume, _}
            },
            {
                resume_expr,
                {
                    var,
                    _,
                    {synthetic, value_handler_auto_resume, _}
                },
                {binary_op, plus, {var, seed, _}, {literal, 1, integer, _}, _},
                {synthetic, value_handler_auto_resume, _}
            },
            _CaseLocation
        },
        Operation
    ),
    {
        operation_case,
        get,
        [],
        {resumption_binder, Binder, Origin},
        {resume_expr, {var, Binder, Origin}, OriginalBody, Origin},
        CaseLocation
    } = Operation,
    ?assertNotEqual(seed, Binder),
    ?assertEqual(CaseLocation, source_location(Origin)),
    ?assertEqual(1, count_exact(OriginalBody, Operation)).

synthetic_binder_skips_every_existing_ast_atom_test() ->
    Loc = loc(1),
    Existing = '__catena_resumption_0',
    Expr = {
        handle_expr,
        {var, Existing, Loc},
        [{
            handler_clause,
            'State',
            [{operation_case, get, [], {var, Existing, Loc}, Loc}],
            Loc
        }],
        Loc
    },
    {ok, Normalized} = catena_resumption_normalize:normalize_expr(Expr),
    {
        handle_expr,
        _,
        [{
            handler_clause,
            'State',
            [{
                operation_case,
                get,
                [],
                {resumption_binder, Binder, _},
                _,
                _
            }],
            _
        }],
        _
    } = Normalized,
    ?assertEqual('__catena_resumption_1', Binder).

explicit_control_case_preserves_binder_and_body_test() ->
    Source =
        "transform run = handle perform Choice.choose() then {\n"
        "  Choice { choose() with continuation -> "
        "resume(continuation, 1) }\n"
        "}",
    Parsed = parse_source(Source),
    ParsedOperation = only_operation_case(Parsed),
    {ok, Analyzed} = catena_semantic:analyze(Parsed),
    NormalizedOperation = only_operation_case(Analyzed),
    ?assertEqual(ParsedOperation, NormalizedOperation),
    ?assertMatch(
        {
            operation_case,
            choose,
            [],
            {resumption_binder, continuation, {location, 2, _}},
            {resume_expr, {var, continuation, _}, {literal, 1, integer, _}, _},
            _
        },
        NormalizedOperation
    ).

normalization_is_idempotent_test() ->
    Parsed = parse_source(
        "transform run = handle perform State.get() then { "
        "State { get() -> 1 } }"
    ),
    {ok, Once} = catena_resumption_normalize:normalize(Parsed),
    {ok, Twice} = catena_resumption_normalize:normalize(Once),
    ?assertEqual(Once, Twice).

handler_body_desugars_before_resumption_normalization_test() ->
    Source =
        "transform run = handle perform Choice.choose() then { "
        "Choice { choose() with k -> resume(k, mapper <$> values) } }",
    {ok, Analyzed} = analyze_source(Source),
    ?assertMatch(
        {
            operation_case,
            choose,
            [],
            {resumption_binder, k, _},
            {
                resume_expr,
                {var, k, _},
                {app, {var, map, _}, [{var, mapper, _}, {var, values, _}], _},
                _
            },
            _
        },
        only_operation_case(Analyzed)
    ).

resume_outside_operation_binder_is_rejected_test() ->
    ?assertMatch(
        {error, {resumption_binder_scope, #{
            target := k,
            active_binders := [],
            location := _
        }}},
        analyze_source("transform run = resume(k, 1)")
    ).

resume_target_must_be_an_active_binder_test() ->
    ?assertMatch(
        {error, {resumption_binder_scope, #{
            target := other,
            active_binders := [k],
            location := _
        }}},
        analyze_source(
            "transform run = handle 1 then { "
            "Choice { choose() with k -> resume(other, 1) } }"
        )
    ).

binder_cannot_duplicate_operation_pattern_test() ->
    ?assertMatch(
        {error, {invalid_resumption_binder, #{
            binder := value,
            reason := duplicates_operation_pattern,
            location := _
        }}},
        analyze_source(
            "transform run = handle 1 then { "
            "State { put(value) with value -> resume(value, 1) } }"
        )
    ).

duplicate_operation_patterns_are_rejected_for_control_cases_test() ->
    ?assertMatch(
        {error, {invalid_resumption_binder, #{
            binder := k,
            reason := {duplicate_operation_pattern, value},
            location := _
        }}},
        analyze_source(
            "transform run = handle 1 then { "
            "State { put(value, value) with k -> resume(k, value) } }"
        )
    ).

nested_resumption_binder_shadowing_is_lexical_test() ->
    Source =
        "transform run = handle 1 then {\n"
        "  Outer { outer() with k ->\n"
        "    handle 2 then {\n"
        "      Inner { inner() with k -> resume(k, 3) }\n"
        "    }\n"
        "  }\n"
        "}",
    ?assertMatch({ok, _}, analyze_source(Source)).

outer_binder_remains_available_inside_nested_handler_test() ->
    Source =
        "transform run = handle 1 then {\n"
        "  Outer { outer() with outerK ->\n"
        "    handle 2 then {\n"
        "      Inner { inner() with innerK -> resume(outerK, 3) }\n"
        "    }\n"
        "  }\n"
        "}",
    ?assertMatch({ok, _}, analyze_source(Source)).

malformed_normalized_binder_fails_closed_test() ->
    Loc = loc(1),
    ?assertMatch(
        {error, {invalid_resumption_binder, #{
            binder := forged,
            reason := malformed_metadata
        }}},
        catena_resumption_normalize:normalize_expr(
            {operation_case, get, [], forged, {literal, 1, integer, Loc}, Loc}
        )
    ).

first_resumption_reports_explicit_and_synthetic_modes_test() ->
    {ok, Explicit} = catena_resumption_normalize:normalize(
        parse_source(
            "transform run = handle 1 then { "
            "Choice { choose() with k -> resume(k, 1) } }"
        )
    ),
    ?assertMatch(
        {ok, #{construct := operation_case, mode := explicit_control}},
        catena_resumption_normalize:first_resumption(Explicit)
    ),
    {ok, Synthetic} = catena_resumption_normalize:normalize(
        parse_source(
            "transform run = handle 1 then { Choice { choose() -> 1 } }"
        )
    ),
    ?assertMatch(
        {ok, #{construct := operation_case, mode := synthetic_auto_resume}},
        catena_resumption_normalize:first_resumption(Synthetic)
    ),
    ?assertEqual(
        none,
        catena_resumption_normalize:first_resumption(
            parse_source("transform run = 1")
        )
    ).

typed_boundary_and_control_backend_accept_explicit_resumptions_test() ->
    Source =
        "module NormalizedTypedBoundary\n"
        "effect Choice\n"
        "operation choose : Int\n"
        "end\n"
        "transform run ignored = handle perform Choice.choose() then { "
        "Choice { choose() with k -> resume(k, 1) } }\n",
    ?assertMatch(
        {ok, {typed_module, _, [_, {typed_transform, run, _, _, _, _}], _}},
        catena_compile:compile_string(Source)
    ),
    ?assertMatch({ok, _}, catena_compile:compile_string_to_core(Source)).

synthetic_auto_resume_projects_to_legacy_value_handler_test() ->
    Loc = loc(1),
    ValueCase =
        {operation_case, choose, [], {literal, 1, integer, Loc}, Loc},
    {ok, Normalized} =
        catena_resumption_normalize:normalize_expr(ValueCase),
    ?assertEqual(
        {ok, ValueCase},
        catena_resumption_normalize:project_legacy_value_handlers(
            Normalized,
            type_inference
        )
    ).

malformed_synthetic_auto_resume_projection_fails_closed_test() ->
    Loc = loc(1),
    Origin = {synthetic, value_handler_auto_resume, Loc},
    Forged = {
        operation_case,
        choose,
        [],
        {resumption_binder, generated, Origin},
        {literal, 1, integer, Loc},
        Loc
    },
    ?assertMatch(
        {error, {missing_resumption_lowering, #{
            stage := backend_compatibility,
            construct := operation_case,
            mode := synthetic_auto_resume,
            reason := malformed_synthetic_auto_resume
        }}},
        catena_resumption_normalize:project_legacy_value_handlers(
            Forged,
            backend_compatibility
        )
    ).

backend_boundaries_reject_normalized_resumptions_test() ->
    {ok, Normalized} = catena_resumption_normalize:normalize_expr({
        handle_expr,
        {var, handled, loc(1)},
        [{
            handler_clause,
            'Choice',
            [{operation_case, choose, [], {var, value, loc(1)}, loc(1)}],
            loc(1)
        }],
        loc(1)
    }),
    ?assertThrow(
        {backend_error, missing_resumption_lowering, _},
        catena_codegen_lower:lower_expr(Normalized)
    ),
    ?assertThrow(
        {backend_error, missing_resumption_lowering, _},
        catena_codegen_erase:erase_expr(Normalized)
    ).

semantic_error_formatter_names_resumption_family_test() ->
    Message = catena_semantic:format_error(
        {resumption_binder_scope, #{target => k}}
    ),
    ?assert(string:find(lists:flatten(Message), "binder scope") =/= nomatch).

analyze_source(Source) ->
    catena_semantic:analyze(parse_source(Source)).

parse_source(Source) ->
    {ok, Tokens} = catena_lexer:tokenize(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    AST.

only_operation_case({
    module,
    _Name,
    _Exports,
    _Imports,
    [{
        transform_decl,
        _Transform,
        _Type,
        [{
            transform_clause,
            _Patterns,
            _Guards,
            {
                handle_expr,
                _Handled,
                [{handler_clause, _Effect, [Operation], _HandlerLocation}],
                _HandleLocation
            },
            _ClauseLocation
        }],
        _DeclarationLocation
    }],
    _ModuleLocation
}) ->
    Operation.

count_exact(Needle, Needle) ->
    1;
count_exact(Needle, Tuple) when is_tuple(Tuple) ->
    lists:sum([
        count_exact(Needle, Element)
        || Element <- tuple_to_list(Tuple)
    ]);
count_exact(Needle, List) when is_list(List) ->
    lists:sum([count_exact(Needle, Element) || Element <- List]);
count_exact(_Needle, _Other) ->
    0.

source_location({synthetic, _Kind, Location}) ->
    Location.

loc(Line) ->
    {line, Line}.
