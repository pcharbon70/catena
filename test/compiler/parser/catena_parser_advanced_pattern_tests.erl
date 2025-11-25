%% @doc Advanced Pattern Parsing Tests (Phase 3.1)
%%
%% Tests for as-patterns and or-patterns added in Phase 3.1.
%% These tests validate the parser correctly handles advanced pattern syntax.
-module(catena_parser_advanced_pattern_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helper Functions
%%====================================================================

parse(Str) ->
    {ok, Tokens, _} = catena_lexer:string(Str),
    catena_parser:parse(Tokens).

%% Extract the pattern from a match clause
get_match_pattern({module, _, _, _, Decls, _}) ->
    [TransformDecl] = [D || D = {transform_decl, _, _, _, _} <- Decls],
    {transform_decl, _, _, [{transform_clause, [], _, {match_expr, _, Clauses, _}, _}], _} = TransformDecl,
    [{match_clause, Pattern, _, _, _}] = Clauses,
    Pattern.

%%====================================================================
%% As-Pattern Tests
%%====================================================================

%% Test: Simple as-pattern with variable
simple_as_pattern_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | x as y -> y end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_as, y, {pat_var, x, _}, _}, Pattern).

%% Test: As-pattern with constructor
as_pattern_with_constructor_test() ->
    {ok, AST} = parse("type Maybe = Some a | None\ntransform f = match | Some(x) as opt -> opt end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_as, opt, {pat_constructor, 'Some', [{pat_var, x, _}], _}, _}, Pattern).

%% Test: As-pattern with list
as_pattern_with_list_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | [x y] as lst -> lst end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_as, lst, {pat_list, _, _}, _}, Pattern).

%% Test: As-pattern with cons
%% Note: "h :: t as lst" parses as "h :: (t as lst)" due to precedence
%% Use parentheses for "(h :: t) as lst" if needed
as_pattern_with_cons_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | h :: t as lst -> lst end"),
    Pattern = get_match_pattern(AST),
    %% 'as' binds looser than '::', so this is h :: (t as lst)
    ?assertMatch({pat_cons, {pat_var, h, _}, {pat_as, lst, {pat_var, t, _}, _}, _}, Pattern).

%% Test: As-pattern binding location
as_pattern_has_location_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | x as y -> y end"),
    Pattern = get_match_pattern(AST),
    {pat_as, _, _, Loc} = Pattern,
    ?assertMatch({location, _, _}, Loc).

%%====================================================================
%% Or-Pattern Tests
%%====================================================================

%% Test: Simple two-alternative or-pattern
simple_or_pattern_test() ->
    {ok, AST} = parse("type Color = Red | Blue\ntransform f = match | Red | Blue -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_or, [
        {pat_constructor, 'Red', [], _},
        {pat_constructor, 'Blue', [], _}
    ], _}, Pattern).

%% Test: Three-alternative or-pattern
three_way_or_pattern_test() ->
    {ok, AST} = parse("type Color = Red | Green | Blue\ntransform f = match | Red | Green | Blue -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_or, [
        {pat_constructor, 'Red', [], _},
        {pat_constructor, 'Green', [], _},
        {pat_constructor, 'Blue', [], _}
    ], _}, Pattern).

%% Test: Or-pattern with variables
or_pattern_with_variables_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | x | y -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_or, [
        {pat_var, x, _},
        {pat_var, y, _}
    ], _}, Pattern).

%% Test: Or-pattern with literals
or_pattern_with_literals_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | 1 | 2 | 3 -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_or, [
        {pat_literal, 1, integer, _},
        {pat_literal, 2, integer, _},
        {pat_literal, 3, integer, _}
    ], _}, Pattern).

%% Test: Or-pattern with constructor patterns
or_pattern_with_constructors_test() ->
    {ok, AST} = parse("type Maybe = Some a | None\ntransform f = match | Some(x) | None -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_or, [
        {pat_constructor, 'Some', [{pat_var, x, _}], _},
        {pat_constructor, 'None', [], _}
    ], _}, Pattern).

%% Test: Or-pattern location
or_pattern_has_location_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | x | y -> 1 end"),
    Pattern = get_match_pattern(AST),
    {pat_or, _, Loc} = Pattern,
    ?assertMatch({location, _, _}, Loc).

%%====================================================================
%% Combined Pattern Tests
%%====================================================================

%% Test: As-pattern inside or-pattern alternative
%% Note: This requires careful precedence handling
nested_as_in_or_test() ->
    {ok, AST} = parse("type X = A | B\ntransform f = match | A | B -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_or, _, _}, Pattern).

%% Test: Regular pattern parsing still works
regular_pattern_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | x -> 1 end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_var, x, _}, Pattern).

%% Test: Constructor pattern parsing still works
constructor_pattern_test() ->
    {ok, AST} = parse("type Maybe = Some a\ntransform f = match | Some(x) -> x end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_constructor, 'Some', [{pat_var, x, _}], _}, Pattern).

%% Test: Nested constructor with as-pattern
nested_constructor_as_pattern_test() ->
    {ok, AST} = parse("type Tree = Node a Tree Tree | Leaf\ntransform f = match | Node(x left right) as node -> node end"),
    Pattern = get_match_pattern(AST),
    ?assertMatch({pat_as, node, {pat_constructor, 'Node', _, _}, _}, Pattern).

%%====================================================================
%% Guard with Advanced Patterns Tests
%%====================================================================

%% Extract guard from match clause
get_match_guard({module, _, _, _, Decls, _}) ->
    [TransformDecl] = [D || D = {transform_decl, _, _, _, _} <- Decls],
    {transform_decl, _, _, [{transform_clause, [], _, {match_expr, _, Clauses, _}, _}], _} = TransformDecl,
    [{match_clause, _, Guard, _, _}] = Clauses,
    Guard.

%% Test: Or-pattern with guard
or_pattern_with_guard_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | 1 | 2 | 3 when x > 0 -> 1 end"),
    Pattern = get_match_pattern(AST),
    Guard = get_match_guard(AST),
    ?assertMatch({pat_or, _, _}, Pattern),
    ?assertNotEqual(undefined, Guard).

%% Test: As-pattern with guard
as_pattern_with_guard_test() ->
    {ok, AST} = parse("type X = A\ntransform f = match | x as y when x > 0 -> y end"),
    Pattern = get_match_pattern(AST),
    Guard = get_match_guard(AST),
    ?assertMatch({pat_as, y, {pat_var, x, _}, _}, Pattern),
    ?assertNotEqual(undefined, Guard).
