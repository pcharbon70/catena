-module(catena_resumption_ast_tests).

-include_lib("eunit/include/eunit.hrl").
-include("src/compiler/parser/catena_ast.hrl").

smart_constructors_preserve_resumption_metadata_test() ->
    Loc = loc(4),
    Target = catena_ast:var(k, Loc),
    Value = catena_ast:literal(integer, 42, Loc),
    Resume = catena_ast:resume_expr(Target, Value, Loc),
    Case = catena_ast:operation_case(
        choose,
        [],
        {resumption_binder, k, Loc},
        Resume,
        Loc
    ),
    ?assertMatch(
        #operation_case{
            operation=choose,
            params=[],
            resumption={resumption_binder, k, Loc},
            body=#resume_expr{
                resumption=#var{name=k},
                value=#literal{value=42}
            },
            location=Loc
        },
        Case
    ),
    ?assertEqual(Loc, catena_ast:location(Resume)),
    ?assertEqual(Loc, catena_ast_utils:get_location(Resume)),
    ?assertEqual(Loc, catena_compiler_utils:extract_location(Resume)).

value_case_constructor_uses_none_metadata_test() ->
    Case = catena_ast:operation_case(
        choose,
        [],
        catena_ast:literal(integer, 1, loc(1)),
        loc(1)
    ),
    ?assertMatch(#operation_case{resumption=none}, Case).

map_expr_traverses_both_resume_operands_test() ->
    Expr = catena_ast:resume_expr(
        catena_ast:var(k, loc(1)),
        catena_ast:literal(integer, 1, loc(1)),
        loc(1)
    ),
    Increment = fun
        ({literal, Value, Type, Loc}) ->
            {literal, Value + 1, Type, Loc};
        (Node) ->
            Node
    end,
    ?assertMatch(
        #resume_expr{
            resumption=#var{name=k},
            value=#literal{value=2}
        },
        catena_ast_utils:map_expr(Increment, Expr)
    ).

map_expr_traverses_control_handler_body_test() ->
    Loc = loc(1),
    Resume = catena_ast:resume_expr(
        catena_ast:var(k, Loc),
        catena_ast:literal(integer, 1, Loc),
        Loc
    ),
    Operation = catena_ast:operation_case(
        get,
        [],
        {resumption_binder, k, Loc},
        Resume,
        Loc
    ),
    Expr = catena_ast:try_with_expr(
        catena_ast:literal(integer, 0, Loc),
        [catena_ast:handler_clause('State', [Operation], Loc)],
        Loc
    ),
    Increment = fun
        ({literal, Value, Type, NodeLoc}) ->
            {literal, Value + 1, Type, NodeLoc};
        (Node) ->
            Node
    end,
    ?assertMatch(
        #try_with_expr{
            body=#literal{value=1},
            handlers=[
                #handler_clause{
                    operations=[
                        #operation_case{
                            resumption={resumption_binder, k, Loc},
                            body=#resume_expr{value=#literal{value=2}}
                        }
                    ]
                }
            ]
        },
        catena_ast_utils:map_expr(Increment, Expr)
    ).

fold_and_walk_keep_resume_postorder_test() ->
    Expr = catena_ast:resume_expr(
        catena_ast:var(k, loc(1)),
        catena_ast:literal(integer, 1, loc(1)),
        loc(1)
    ),
    Count = catena_ast_utils:fold_expr(
        fun(_Node, Acc) -> Acc + 1 end,
        0,
        Expr
    ),
    ?assertEqual(3, Count),
    put(resumption_walk_order, []),
    ok = catena_ast_utils:walk_expr(
        fun(Node) ->
            Tags = get(resumption_walk_order),
            put(resumption_walk_order, Tags ++ [element(1, Node)]),
            ok
        end,
        Expr
    ),
    ?assertEqual([var, literal, resume_expr], get(resumption_walk_order)),
    erase(resumption_walk_order).

resume_nodes_participate_in_depth_limits_test() ->
    Base = catena_ast:var(k, loc(1)),
    Deep = lists:foldl(
        fun(_, Acc) ->
            catena_ast:resume_expr(Acc, catena_ast:literal(integer, 0, loc(1)), loc(1))
        end,
        Base,
        lists:seq(1, 1002)
    ),
    ?assertThrow(
        {error, {max_depth_exceeded, 1000}},
        catena_ast_utils:map_expr(fun(Node) -> Node end, Deep)
    ).

location_insensitive_equality_preserves_source_intent_test() ->
    Explicit1 = {
        operation_case,
        choose,
        [],
        {resumption_binder, k, loc(1)},
        {resume_expr, {var, k, loc(1)}, {literal, 1, integer, loc(1)}, loc(1)},
        loc(1)
    },
    Explicit2 = {
        operation_case,
        choose,
        [],
        {resumption_binder, k, loc(9)},
        {resume_expr, {var, k, loc(9)}, {literal, 1, integer, loc(9)}, loc(9)},
        loc(9)
    },
    ValueCase = {
        operation_case,
        choose,
        [],
        none,
        {literal, 1, integer, loc(9)},
        loc(9)
    },
    ?assert(catena_ast_utils:equivalent(Explicit1, Explicit2)),
    ?assertNot(catena_ast_utils:equivalent(Explicit1, ValueCase)).

validation_accepts_canonical_control_nodes_test() ->
    Loc = loc(1),
    Expr = #try_with_expr{
        body=#literal{value=0, type=integer, location=Loc},
        handlers=[
            #handler_clause{
                effect='Choice',
                operations=[
                    #operation_case{
                        operation=choose,
                        params=[],
                        resumption={resumption_binder, k, Loc},
                        body=#resume_expr{
                            resumption=#var{name=k, location=Loc},
                            value=#literal{value=1, type=integer, location=Loc},
                            location=Loc
                        },
                        location=Loc
                    }
                ],
                location=Loc
            }
        ],
        location=Loc
    },
    ?assertEqual(ok, catena_ast_utils:validate_ast(Expr)).

validation_rejects_malformed_resumption_metadata_test_() ->
    Body = {literal, 1, integer, loc(1)},
    [
        ?_assertEqual(
            {error, {invalid_resumption_binder, {resumption_binder, undefined, loc(1)}}},
            catena_ast_utils:validate_ast(
                {operation_case, choose, [], {resumption_binder, undefined, loc(1)}, Body, loc(1)}
            )
        ),
        ?_assertEqual(
            {error, {invalid_resumption_binder, forged}},
            catena_ast_utils:validate_ast(
                {operation_case, choose, [], forged, Body, loc(1)}
            )
        ),
        ?_assertEqual(
            {error, {invalid_location, {line, 0}}},
            catena_ast_utils:validate_ast(
                {operation_case, choose, [], {resumption_binder, k, {line, 0}}, Body, loc(1)}
            )
        )
    ].

pretty_prints_explicit_and_value_handler_forms_test() ->
    Explicit = parse_expr(
        "handle perform Choice.choose() then { "
        "Choice { choose() with k -> resume(k, 1) } }"
    ),
    ?assertEqual(
        "handle perform Choice.choose() then { "
        "Choice { choose() with k -> resume(k, 1) } }",
        catena_ast_pp:pp_expr(Explicit)
    ),
    Value = parse_expr(
        "handle perform Choice.choose() then { Choice { choose() -> 1 } }"
    ),
    ?assertEqual(
        "handle perform Choice.choose() then { Choice { choose() -> 1 } }",
        catena_ast_pp:pp_expr(Value)
    ).

pretty_prints_resume_with_stable_precedence_test() ->
    Expr = parse_expr("resume(k, 1) + 2"),
    ?assertEqual("resume(k, 1) + 2", catena_ast_pp:pp_expr(Expr)),
    ?assertEqual(
        "resume(k, 1)",
        lists:flatten(
            catena_ast_utils:format_expr(
                catena_ast:resume_expr(
                    catena_ast:var(k, loc(1)),
                    catena_ast:literal(integer, 1, loc(1)),
                    loc(1)
                )
            )
        )
    ).

parse_print_roundtrip_test_() ->
    Sources = [
        "handle (handle perform Inner.get() then { "
        "Inner { get() with inner -> resume(inner, 1) } }) then { "
        "Outer { wrap(value) with outer -> resume(outer, value) } }",
        "handle perform File.read() then {\n"
        "  File {\n"
        "    read(Some(path)) with k ->\n"
        "      let value = path in resume(k, value)\n"
        "  }\n"
        "}",
        "resume(k, 1) + 2",
        "resumed (k)",
        "handle perform Choice.choose() then {\n"
        "  -- comments are intentionally not reconstructed\n"
        "  Choice { choose() with k -> resume(k, 1) }\n"
        "}"
    ],
    [
        ?_test(assert_parse_print_roundtrip(Source))
     || Source <- Sources
    ].

assert_parse_print_roundtrip(Source) ->
    Original = parse_expr(Source),
    Printed = catena_ast_pp:pp_expr(Original),
    Reparsed = parse_expr(Printed),
    ?assert(
        catena_ast_utils:equivalent(Original, Reparsed),
        #{source => Source, printed => Printed}
    ).

parse_expr(Source) ->
    ModuleSource = "transform roundtrip = " ++ Source,
    {ok, Tokens} = catena_lexer:tokenize(ModuleSource),
    {ok,
        {module, undefined, [], [], [
            {transform_decl, roundtrip, undefined, [
                {transform_clause, [], undefined, Expr, _ClauseLoc}
            ], _DeclLoc}
        ], _ModuleLoc}} = catena_parser:parse(Tokens),
    Expr.

loc(Line) ->
    {line, Line}.
