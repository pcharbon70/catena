-module(catena_parser_resumption_tests).

-include_lib("eunit/include/eunit.hrl").

explicit_nullary_resumption_binder_test() ->
    AST = parse_ok(
        "transform run =\n"
        "  handle (perform Choice.choose()) then {\n"
        "    Choice { choose() with k -> resume(k, 1) }\n"
        "  }"
    ),
    ?assertMatch(
        {module, undefined, [], [], [
            {transform_decl, run, undefined, [
                {transform_clause, [], undefined,
                    {handle_expr, _Handled, [
                        {handler_clause, 'Choice', [
                            {operation_case, choose, [],
                                {resumption_binder, k, {location, 3, _}},
                                {resume_expr,
                                    {var, k, {location, 3, _}},
                                    {literal, 1, integer, {location, 3, _}},
                                    {location, 3, _}},
                                {location, 3, _}}
                        ], _}
                    ], _},
                    _}
            ], _}
        ], _},
        AST
    ).

explicit_parameterized_resumption_binder_test() ->
    AST = parse_ok(
        "transform run = handle (perform State.put(1)) then {\n"
        "  State { put(value) with continuation -> resume(continuation, value) }\n"
        "}"
    ),
    ?assertMatch(
        {module, undefined, [], [], [
            {transform_decl, run, undefined, [
                {transform_clause, [], undefined,
                    {handle_expr, _, [
                        {handler_clause, 'State', [
                            {operation_case, put,
                                [{pat_var, value, {location, 2, _}}],
                                {resumption_binder, continuation, {location, 2, _}},
                                {resume_expr,
                                    {var, continuation, {location, 2, _}},
                                    {var, value, {location, 2, _}},
                                    {location, 2, _}},
                                {location, 2, _}}
                        ], _}
                    ], _},
                    _}
            ], _}
        ], _},
        AST
    ).

bare_nullary_resumption_binder_test() ->
    AST = parse_ok(
        "transform run = handle (perform Choice.choose()) then {\n"
        "  Choice { choose with k -> resume(k, 1) }\n"
        "}"
    ),
    ?assertMatch(
        {module, _, _, _, [
            {transform_decl, run, _, [
                {transform_clause, _, _,
                    {handle_expr, _, [
                        {handler_clause, 'Choice', [
                            {operation_case, choose, [],
                                {resumption_binder, k, _},
                                {resume_expr, _, _, _},
                                _}
                        ], _}
                    ], _},
                    _}
            ], _}
        ], _},
        AST
    ).

value_handler_syntax_remains_compatible_test() ->
    AST = parse_ok(
        "transform run = handle (perform Choice.choose()) then {\n"
        "  Choice { choose() -> 1 }\n"
        "}"
    ),
    ?assertMatch(
        {module, _, _, _, [
            {transform_decl, run, _, [
                {transform_clause, _, _,
                    {handle_expr, _, [
                        {handler_clause, 'Choice', [
                            {operation_case, choose, [],
                                {literal, 1, integer, _},
                                _}
                        ], _}
                    ], _},
                    _}
            ], _}
        ], _},
        AST
    ).

nested_resume_expression_test() ->
    AST = parse_ok("transform run = resume(outer, resume(inner, 1))"),
    ?assertMatch(
        {module, _, _, _, [
            {transform_decl, run, _, [
                {transform_clause, _, _,
                    {resume_expr, {var, outer, _},
                        {resume_expr, {var, inner, _}, {literal, 1, integer, _}, _},
                        _},
                    _}
            ], _}
        ], _},
        AST
    ).

resume_expression_precedence_test() ->
    AST = parse_ok("transform run = resume(k, 1) + 2"),
    ?assertMatch(
        {module, _, _, _, [
            {transform_decl, run, _, [
                {transform_clause, _, _,
                    {binary_op, plus,
                        {resume_expr, {var, k, _}, {literal, 1, integer, _}, _},
                        {literal, 2, integer, _},
                        _},
                    _}
            ], _}
        ], _},
        AST
    ).

resume_arity_diagnostic_test_() ->
    [
        ?_assert(has_resume_arity_error("transform run = resume()")),
        ?_assert(has_resume_arity_error("transform run = resume(k)")),
        ?_assert(has_resume_arity_error("transform run = resume(k, 1, 2)")),
        ?_assert(has_resume_arity_error("transform run = resume(k, 1, 2, 3)"))
    ].

malformed_resumption_binder_test_() ->
    [
        ?_assertMatch({error, _}, parse("transform run = handle 1 then { E { op() with -> 1 } }")),
        ?_assertMatch({error, _}, parse("transform run = handle 1 then { E { op() with K -> 1 } }")),
        ?_assertMatch({error, _}, parse("transform run = handle 1 then { E { op() with 1 -> 1 } }")),
        ?_assertMatch({error, _}, parse("transform run = handle 1 then { E { op() with k with j -> 1 } }"))
    ].

parse_ok(Source) ->
    {ok, AST} = parse(Source),
    AST.

parse(Source) ->
    {ok, Tokens} = catena_lexer:tokenize(Source),
    catena_parser:parse(Tokens).

has_resume_arity_error(Source) ->
    case parse(Source) of
        {error, {_Line, catena_parser, Message}} ->
            string:find(lists:flatten(Message), "resume expects exactly two operands") =/= nomatch;
        _ ->
            false
    end.
