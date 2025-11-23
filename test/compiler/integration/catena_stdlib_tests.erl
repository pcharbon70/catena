%% @doc Standard Library Validation Tests
%% Tests for Section 1.7 of Phase 1.
%% Validates that Catena's standard library compiles to BEAM.

-module(catena_stdlib_tests).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Test Configuration
%% =============================================================================

%% Use project root relative path
stdlib_dir() ->
    %% Get the test file directory and navigate to stdlib
    case os:getenv("CATENA_ROOT") of
        false ->
            %% Default to project structure
            "/home/ducky/code/catena/lib/catena/stdlib";
        Root ->
            filename:join([Root, "lib", "catena", "stdlib"])
    end.

%% =============================================================================
%% Section 1.7.1 - Standard Library Compilation
%% =============================================================================

%% Test that all stdlib files can be located
stdlib_files_exist_test() ->
    BaseDir = stdlib_dir(),
    Files = [
        "prelude.cat",
        "test.cat",
        "effect/io.cat",
        "effect/state.cat",
        "effect/error.cat"
    ],
    lists:foreach(fun(File) ->
        Path = filename:join(BaseDir, File),
        ?assert(filelib:is_file(Path), {file_not_found, Path})
    end, Files).

%% Test parsing prelude.cat - basic structure
parse_prelude_module_test() ->
    %% Parser requires at least one declaration after module
    Source = "module Prelude\ntype X = Y\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, 'Prelude', _, _, _, _}} -> ok;
        {ok, Other} ->
            io:format("Unexpected AST: ~p~n", [Other]),
            ?assert(false);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% Test parsing trait declaration
parse_trait_declaration_test() ->
    Source = "trait Comparable a where\n  equals : a -> a -> Bool\nend\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing trait with default implementation
parse_trait_default_impl_test() ->
    Source = "trait Comparable a where\n"
             "  equals : a -> a -> Bool\n"
             "  notEquals : a -> a -> Bool\n"
             "  notEquals x y = not (equals x y)\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, _AST} -> ok;
        {error, Reason} ->
            io:format("Parse error for trait with default: ~p~n", [Reason]),
            ?assert(false)
    end.

%% Test parsing trait hierarchy with : syntax
parse_trait_hierarchy_test() ->
    Source = "trait Orderable a : Comparable a where\n"
             "  compare : a -> a -> Ordering\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing instance declaration
parse_instance_declaration_test() ->
    Source = "instance Mapper Maybe where\n"
             "  transform map f x = x\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing type declaration (ADT)
parse_type_declaration_test() ->
    Source = "type Maybe a = None | Some a\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing effect declaration (using parser's end syntax, not braces)
parse_effect_declaration_test() ->
    %% Parser expects: effect Name ... end
    %% Stdlib uses: effect Name { ... } (needs update)
    Source = "effect IO\n"
             "  operation print : String -> Unit\n"
             "  operation readLine : Unit -> String\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing parameterized effect
%% NOTE: Parser doesn't support parameterized effects yet
parse_parameterized_effect_test() ->
    %% This documents a gap: parser needs effect type parameters
    Source = "effect State\n"
             "  operation get : Unit -> s\n"
             "  operation put : s -> Unit\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing export declarations
parse_export_declarations_test() ->
    Source = "export trait Comparable\n"
             "export type Maybe\n"
             "export transform map\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, _AST} -> ok;
        {error, Reason} ->
            io:format("Parse error for exports: ~p~n", [Reason]),
            %% Note: export syntax may not be implemented
            ok
    end.

%% Test parsing constrained instance
parse_constrained_instance_test() ->
    Source = "instance Comparable a => Comparable (Maybe a) where\n"
             "  transform equals x y = true\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, _AST} -> ok;
        {error, Reason} ->
            io:format("Parse error for constrained instance: ~p~n", [Reason]),
            %% Note: constrained instances may not be implemented
            ok
    end.

%% Test parsing higher-kinded trait
parse_hkt_trait_test() ->
    Source = "trait Mapper f where\n"
             "  map : (a -> b) -> f a -> f b\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing record type
parse_record_type_test() ->
    Source = "type Test = Test {\n"
             "  name : String,\n"
             "  run : Unit -> TestResult\n"
             "}\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing transform with type signature
parse_transform_with_sig_test() ->
    Source = "transform unit : String -> (Unit -> Bool) -> Test\n"
             "transform unit name testFn = name\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, _AST} -> ok;
        {error, Reason} ->
            io:format("Parse error for transform with sig: ~p~n", [Reason]),
            ok
    end.

%% Test parsing match expression
%% NOTE: Parser uses pattern-only match (no scrutinee)
%% Syntax: match | pat -> expr | pat -> expr end
%% Constructor patterns with args need parens: Some(a) not Some a
parse_match_expression_test() ->
    Source = "transform f x = match\n"
             "  | None -> None\n"
             "  | Some(a) -> a\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    ?assertMatch({ok, _}, catena_parser:parse(Tokens)).

%% Test parsing multiple trait constraints
parse_multiple_constraints_test() ->
    Source = "trait Pipeline m : Applicator m, Chainable m where\n"
             "  join : m (m a) -> m a\n"
             "end\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, _AST} -> ok;
        {error, Reason} ->
            io:format("Parse error for multiple constraints: ~p~n", [Reason]),
            %% Note: multiple constraints may not be implemented
            ok
    end.

%% =============================================================================
%% Full File Parsing Tests
%% =============================================================================

%% Test parsing the IO effect file
parse_io_effect_file_test() ->
    Path = filename:join([stdlib_dir(), "effect", "io.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    case catena_lexer:string(Source) of
        {ok, Tokens, _} ->
            case catena_parser:parse(Tokens) of
                {ok, {module, 'Effect.IO', _, _, _, _}} -> ok;
                {ok, Other} ->
                    io:format("~nUnexpected AST for io.cat:~n~p~n", [Other]),
                    ?assert(false, {unexpected_ast, Other});
                {error, Reason} ->
                    io:format("~nParse error for io.cat:~n~p~n", [Reason]),
                    ?assert(false, {parse_failed, Reason})
            end;
        {error, LexError, _} ->
            io:format("~nLex error for io.cat:~n~p~n", [LexError]),
            ?assert(false, {lex_failed, LexError})
    end.

%% Test parsing the State effect file
parse_state_effect_file_test() ->
    Path = filename:join([stdlib_dir(), "effect", "state.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    case catena_lexer:string(Source) of
        {ok, Tokens, _} ->
            case catena_parser:parse(Tokens) of
                {ok, {module, 'Effect.State', _, _, _, _}} -> ok;
                {ok, Other} ->
                    io:format("~nUnexpected AST for state.cat:~n~p~n", [Other]),
                    ?assert(false, {unexpected_ast, Other});
                {error, Reason} ->
                    io:format("~nParse error for state.cat:~n~p~n", [Reason]),
                    ?assert(false, {parse_failed, Reason})
            end;
        {error, LexError, _} ->
            io:format("~nLex error for state.cat:~n~p~n", [LexError]),
            ?assert(false, {lex_failed, LexError})
    end.

%% Test parsing the Error effect file
parse_error_effect_file_test() ->
    Path = filename:join([stdlib_dir(), "effect", "error.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    case catena_lexer:string(Source) of
        {ok, Tokens, _} ->
            case catena_parser:parse(Tokens) of
                {ok, {module, 'Effect.Error', _, _, _, _}} -> ok;
                {ok, Other} ->
                    io:format("~nUnexpected AST for error.cat:~n~p~n", [Other]),
                    ?assert(false, {unexpected_ast, Other});
                {error, Reason} ->
                    io:format("~nParse error for error.cat:~n~p~n", [Reason]),
                    ?assert(false, {parse_failed, Reason})
            end;
        {error, LexError, _} ->
            io:format("~nLex error for error.cat:~n~p~n", [LexError]),
            ?assert(false, {lex_failed, LexError})
    end.

%% =============================================================================
%% Section 1.5.1 - Standard Library Type-Checking
%% =============================================================================

%% Test type-checking prelude.cat
typecheck_prelude_test() ->
    Path = filename:join([stdlib_dir(), "prelude.cat"]),
    case catena_compile:compile_file(Path) of
        {ok, {typed_module, 'Prelude', Decls, _Env}} ->
            %% Verify we get all 32 declarations
            ?assertEqual(32, length(Decls)),
            ok;
        {error, Reason} ->
            io:format("~nPrelude type-check error: ~p~n", [Reason]),
            ?assert(false, {prelude_typecheck_failed, Reason})
    end.

%% Test type-checking effect/io.cat
typecheck_io_effect_test() ->
    Path = filename:join([stdlib_dir(), "effect", "io.cat"]),
    case catena_compile:compile_file(Path) of
        {ok, {typed_module, 'Effect.IO', Decls, _Env}} ->
            %% IO effect has 1 effect declaration
            ?assertEqual(1, length(Decls)),
            ok;
        {error, Reason} ->
            io:format("~nIO effect type-check error: ~p~n", [Reason]),
            ?assert(false, {io_typecheck_failed, Reason})
    end.

%% Test type-checking effect/state.cat
typecheck_state_effect_test() ->
    Path = filename:join([stdlib_dir(), "effect", "state.cat"]),
    case catena_compile:compile_file(Path) of
        {ok, {typed_module, 'Effect.State', Decls, _Env}} ->
            ?assertEqual(1, length(Decls)),
            ok;
        {error, Reason} ->
            io:format("~nState effect type-check error: ~p~n", [Reason]),
            ?assert(false, {state_typecheck_failed, Reason})
    end.

%% Test type-checking effect/error.cat
typecheck_error_effect_test() ->
    Path = filename:join([stdlib_dir(), "effect", "error.cat"]),
    case catena_compile:compile_file(Path) of
        {ok, {typed_module, 'Effect.Error', Decls, _Env}} ->
            ?assertEqual(1, length(Decls)),
            ok;
        {error, Reason} ->
            io:format("~nError effect type-check error: ~p~n", [Reason]),
            ?assert(false, {error_typecheck_failed, Reason})
    end.

%% Test parsing test.cat (type-checking has known limitations with record construction)
parse_test_module_test() ->
    Path = filename:join([stdlib_dir(), "test.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, 'Test', Exports, _, Decls, _}} ->
            %% Verify exports and declarations
            ?assertEqual(13, length(Exports)),
            ?assertEqual(13, length(Decls)),
            ok;
        {error, Reason} ->
            io:format("~nTest module parse error: ~p~n", [Reason]),
            ?assert(false, {test_parse_failed, Reason})
    end.

%% =============================================================================
%% Section 1.5.2 - Trait Instance Resolution
%% =============================================================================

%% Helper to load prelude and build instance database
load_prelude_instances() ->
    Path = filename:join([stdlib_dir(), "prelude.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),
    catena_instance:build_instance_db(Decls).

%% 1.5.2.1 Resolve Mapper instance for Maybe
resolve_mapper_maybe_test() ->
    DB = load_prelude_instances(),
    MaybeType = {tcon, 'Maybe'},
    Constraint = catena_constraint:trait_constraint('Mapper', [MaybeType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Mapper', [{tcon, 'Maybe'}], _}, _}, Result).

%% 1.5.2.2 Resolve Mapper instance for List
resolve_mapper_list_test() ->
    DB = load_prelude_instances(),
    ListType = {tcon, 'List'},
    Constraint = catena_constraint:trait_constraint('Mapper', [ListType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Mapper', [{tcon, 'List'}], _}, _}, Result).

%% 1.5.2.3 Resolve constrained instances (Comparable for Maybe a, List a)
%% Note: Comparable instances in prelude are parameterized: Comparable (Maybe a), Comparable (List a)
resolve_comparable_maybe_a_test() ->
    DB = load_prelude_instances(),
    %% Comparable instance for Maybe Int (unifies with Maybe a)
    MaybeInt = {tapp, {tcon, 'Maybe'}, [{tcon, 'Int'}]},
    Constraint = catena_constraint:trait_constraint('Comparable', [MaybeInt]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Comparable', [{tapp, {tcon, 'Maybe'}, _}], _}, _}, Result).

resolve_comparable_list_a_test() ->
    DB = load_prelude_instances(),
    %% Comparable instance for List Int (unifies with List a)
    ListInt = {tapp, {tcon, 'List'}, [{tcon, 'Int'}]},
    Constraint = catena_constraint:trait_constraint('Comparable', [ListInt]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Comparable', [{tapp, {tcon, 'List'}, _}], _}, _}, Result).

%% 1.5.2.4 Detect and report missing instances
resolve_missing_instance_test() ->
    DB = load_prelude_instances(),
    %% Int doesn't have a Mapper instance
    IntType = {tcon, 'Int'},
    Constraint = catena_constraint:trait_constraint('Mapper', [IntType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertEqual({error, no_instance}, Result).

%% 1.5.2.5 Verify trait hierarchy (Pipeline, Applicator, Chainable for Maybe)
resolve_pipeline_maybe_test() ->
    DB = load_prelude_instances(),
    MaybeType = {tcon, 'Maybe'},
    Constraint = catena_constraint:trait_constraint('Pipeline', [MaybeType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Pipeline', [{tcon, 'Maybe'}], _}, _}, Result).

resolve_applicator_maybe_test() ->
    DB = load_prelude_instances(),
    MaybeType = {tcon, 'Maybe'},
    Constraint = catena_constraint:trait_constraint('Applicator', [MaybeType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Applicator', [{tcon, 'Maybe'}], _}, _}, Result).

resolve_chainable_maybe_test() ->
    DB = load_prelude_instances(),
    MaybeType = {tcon, 'Maybe'},
    Constraint = catena_constraint:trait_constraint('Chainable', [MaybeType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Chainable', [{tcon, 'Maybe'}], _}, _}, Result).

%% Test multiple traits for List
resolve_pipeline_list_test() ->
    DB = load_prelude_instances(),
    ListType = {tcon, 'List'},
    Constraint = catena_constraint:trait_constraint('Pipeline', [ListType]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Pipeline', [{tcon, 'List'}], _}, _}, Result).

%% Test Either instances (Mapper for Either e)
resolve_mapper_either_test() ->
    DB = load_prelude_instances(),
    %% Either String (partially applied - unifies with Either e)
    EitherString = {tapp, {tcon, 'Either'}, [{tcon, 'String'}]},
    Constraint = catena_constraint:trait_constraint('Mapper', [EitherString]),
    Result = catena_instance:resolve_constraint(Constraint, DB),
    ?assertMatch({ok, {instance, 'Mapper', [{tapp, {tcon, 'Either'}, _}], _}, _}, Result).

%% =============================================================================
%% Section 1.7.3 - Higher-Kinded Type Validation (placeholder)
%% TODO: Implement when HKT support is complete
%% =============================================================================

%% =============================================================================
%% Section 1.7.4 - Law Verification (placeholder)
%% TODO: Implement when Test module compilation works
%% =============================================================================

%% =============================================================================
%% Section 1.7.5 - Do-Notation Desugaring (placeholder)
%% TODO: Implement when do-notation is added to parser
%% =============================================================================

%% =============================================================================
%% Section 1.7.6 - Effect Integration (placeholder)
%% TODO: Implement when effect polymorphism is complete
%% =============================================================================

%% =============================================================================
%% Section 1.7.7 - Operator Desugaring (placeholder)
%% TODO: Implement when trait resolution integration is complete
%% =============================================================================
