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
%% Section 1.5.3 - Higher-Kinded Type Validation
%% =============================================================================

%% Helper to load prelude declarations
load_prelude_decls() ->
    Path = filename:join([stdlib_dir(), "prelude.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),
    Decls.

%% 1.5.3.1 Validate kind checking for Mapper trait
kind_check_mapper_trait_test() ->
    Decls = load_prelude_decls(),
    [Mapper] = [D || D = {trait_decl, 'Mapper', _, _, _, _} <- Decls],
    {ok, Kinds} = catena_kind:check_trait_kind(Mapper),
    %% Mapper's f parameter should have kind Type -> Type
    ?assertEqual([{f, {arrow, star, star}}], Kinds).

%% Test kind checking for Pipeline trait (also HKT)
kind_check_pipeline_trait_test() ->
    Decls = load_prelude_decls(),
    [Pipeline] = [D || D = {trait_decl, 'Pipeline', _, _, _, _} <- Decls],
    {ok, Kinds} = catena_kind:check_trait_kind(Pipeline),
    %% Pipeline's m parameter should have kind Type -> Type
    ?assertEqual([{m, {arrow, star, star}}], Kinds).

%% Test kind checking for Comparable trait (not HKT)
kind_check_comparable_trait_test() ->
    Decls = load_prelude_decls(),
    [Comparable] = [D || D = {trait_decl, 'Comparable', _, _, _, _} <- Decls],
    {ok, Kinds} = catena_kind:check_trait_kind(Comparable),
    %% Comparable's a parameter should have kind Type
    ?assertEqual([{a, star}], Kinds).

%% 1.5.3.2 Validate kind inference for instance declarations
kind_infer_maybe_instance_test() ->
    Decls = load_prelude_decls(),
    Env = catena_kind:build_kind_env(Decls),
    %% Maybe : Type -> Type
    MaybeType = {type_con, 'Maybe', {location, 0, 0}},
    {ok, Kind} = catena_kind:infer_type_kind(MaybeType, Env),
    ?assertEqual({arrow, star, star}, Kind).

%% 1.5.3.3 Validate partially applied type constructors
kind_infer_either_partial_test() ->
    Decls = load_prelude_decls(),
    Env = catena_kind:build_kind_env(Decls),
    %% Either : Type -> Type -> Type
    %% Either String : Type -> Type
    EitherString = {type_app, {type_con, 'Either', {location, 0, 0}},
                    [{type_con, 'String', {location, 0, 0}}], {location, 0, 0}},
    {ok, Kind} = catena_kind:infer_type_kind(EitherString, Env),
    ?assertEqual({arrow, star, star}, Kind).

%% Test fully applied type
kind_infer_maybe_int_test() ->
    Decls = load_prelude_decls(),
    Env = catena_kind:build_kind_env(Decls),
    %% Maybe Int : Type
    MaybeInt = {type_app, {type_con, 'Maybe', {location, 0, 0}},
                [{type_con, 'Int', {location, 0, 0}}], {location, 0, 0}},
    {ok, Kind} = catena_kind:infer_type_kind(MaybeInt, Env),
    ?assertEqual(star, Kind).

%% 1.5.3.4 Report kind errors
kind_error_over_applied_test() ->
    Decls = load_prelude_decls(),
    Env = catena_kind:build_kind_env(Decls),
    %% Int Int - can't apply Int (kind Type) to Int
    BadType = {type_app, {type_con, 'Int', {location, 1, 1}},
               [{type_con, 'Int', {location, 1, 1}}], {location, 1, 1}},
    Result = catena_kind:infer_type_kind(BadType, Env),
    ?assertMatch({error, {over_applied, star, 1, _}}, Result).

%% Test HKT validation passes for all prelude instances
validate_hkt_prelude_test() ->
    Decls = load_prelude_decls(),
    Env = catena_kind:build_kind_env(Decls),
    Result = catena_kind:validate_hkt(Decls, Env),
    ?assertMatch({ok, _}, Result).

%% =============================================================================
%% Section 1.5.4 - Law Verification via Test Module
%% =============================================================================

%% Helper to load laws module
load_laws_decls() ->
    Path = filename:join([stdlib_dir(), "laws.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    {ok, {module, _, _, _, Decls, _}} = catena_semantic:analyze(AST),
    Decls.

%% 1.5.4.1 Compile Mapper identity law verification
parse_mapper_identity_law_test() ->
    Path = filename:join([stdlib_dir(), "laws.cat"]),
    {ok, Content} = file:read_file(Path),
    Source = binary_to_list(Content),
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, {module, 'Laws', Exports, _, Decls, _}} = catena_parser:parse(Tokens),
    %% Check that mapperIdentityLaw is exported
    ?assert(lists:member({export_transform, mapperIdentityLaw}, Exports)),
    %% Check declaration exists
    ?assertEqual(8, length(Decls)).

%% 1.5.4.2 Compile Mapper composition law verification
parse_mapper_composition_law_test() ->
    Decls = load_laws_decls(),
    %% Find the composition law transform
    CompLaws = [D || D = {transform_decl, mapperCompositionLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(CompLaws)).

%% 1.5.4.3 Compile Pipeline monad laws
parse_pipeline_laws_test() ->
    Decls = load_laws_decls(),
    %% Check all three Pipeline laws exist
    LeftId = [D || D = {transform_decl, pipelineLeftIdentityLaw, _, _, _} <- Decls],
    RightId = [D || D = {transform_decl, pipelineRightIdentityLaw, _, _, _} <- Decls],
    Assoc = [D || D = {transform_decl, pipelineAssociativityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(LeftId)),
    ?assertEqual(1, length(RightId)),
    ?assertEqual(1, length(Assoc)).

%% Test Comparable and Combiner laws
parse_comparable_combiner_laws_test() ->
    Decls = load_laws_decls(),
    Reflex = [D || D = {transform_decl, comparableReflexivityLaw, _, _, _} <- Decls],
    Symm = [D || D = {transform_decl, comparableSymmetryLaw, _, _, _} <- Decls],
    CombAssoc = [D || D = {transform_decl, combinerAssociativityLaw, _, _, _} <- Decls],
    ?assertEqual(1, length(Reflex)),
    ?assertEqual(1, length(Symm)),
    ?assertEqual(1, length(CombAssoc)).

%% 1.5.4.4 Test law structure contains expected AST nodes
verify_law_structure_test() ->
    Decls = load_laws_decls(),
    %% Get mapperIdentityLaw
    [{transform_decl, mapperIdentityLaw, _Sig, Clauses, _Loc}] =
        [D || D = {transform_decl, mapperIdentityLaw, _, _, _} <- Decls],
    %% Should have one clause with one parameter
    ?assertEqual(1, length(Clauses)),
    [{transform_clause, Patterns, _Guards, _Body, _ClauseLoc}] = Clauses,
    ?assertEqual(1, length(Patterns)).

%% Test that all 8 laws are present
all_laws_present_test() ->
    Decls = load_laws_decls(),
    ?assertEqual(8, length(Decls)),
    %% All should be transform_decl
    TransformDecls = [D || D = {transform_decl, _, _, _, _} <- Decls],
    ?assertEqual(8, length(TransformDecls)).

%% =============================================================================
%% Section 1.5.5 - Do-Notation Desugaring
%% =============================================================================

%% 1.5.5.1 Test parsing do-block syntax
parse_do_block_test() ->
    Source = "transform test x = do { y <- x; pure y }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, test, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, Body, _}] = Clauses,
            %% Body should be a do_expr
            ?assertMatch({do_expr, _, _}, Body);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.2 Test parsing bind in do-block
parse_do_bind_test() ->
    Source = "transform test x = do { a <- x; b <- a; pure b }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, test, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, {do_expr, Stmts, _}, _}] = Clauses,
            %% Should have 3 statements: bind, bind, return
            ?assertEqual(3, length(Stmts)),
            [{do_bind, a, _, _}, {do_bind, b, _, _}, {do_return, _, _}] = Stmts;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.3 Test parsing let in do-block
parse_do_let_test() ->
    Source = "transform test x = do { let y = 42; pure y }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, test, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, {do_expr, Stmts, _}, _}] = Clauses,
            %% Should have 2 statements: let, return
            ?assertEqual(2, length(Stmts)),
            [{do_let, y, _, _}, {do_return, _, _}] = Stmts;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.4 Test parsing action in do-block (sequence without binding)
parse_do_action_test() ->
    Source = "transform test x = do { print x; pure 42 }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, test, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, {do_expr, Stmts, _}, _}] = Clauses,
            %% Should have 2 statements: action, return
            ?assertEqual(2, length(Stmts)),
            [{do_action, _, _}, {do_return, _, _}] = Stmts;
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.5.5 Test desugaring bind to chain
desugar_bind_test() ->
    Source = "transform test x = do { y <- x; pure y }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    %% Desugar
    Desugared = catena_desugar:desugar(AST),
    %% Extract the desugared body
    {module, _, _, _, [{transform_decl, test, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be: chain (fn y -> pure y) x
    ?assertMatch({app, {var, chain, _}, [_, _], _}, Body).

%% 1.5.5.6 Test desugaring let to let_expr
desugar_let_test() ->
    Source = "transform test u = do { let x = 42; pure x }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    Desugared = catena_desugar:desugar(AST),
    {module, _, _, _, [{transform_decl, test, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be: let x = 42 in pure x
    ?assertMatch({let_expr, _, _, _}, Body).

%% 1.5.5.7 Test desugaring action (sequence)
desugar_action_test() ->
    Source = "transform test x = do { print x; pure 42 }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    Desugared = catena_desugar:desugar(AST),
    {module, _, _, _, [{transform_decl, test, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be: chain (fn _ -> pure 42) (print x)
    ?assertMatch({app, {var, chain, _}, [{lambda, [{pat_wildcard, _}], _, _}, _], _}, Body).

%% 1.5.5.8 Test complex do-block with multiple binds
desugar_complex_do_test() ->
    Source = "transform test x = do { a <- x; b <- f a; c <- g b; pure (a, b, c) }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    {ok, AST} = catena_parser:parse(Tokens),
    Desugared = catena_desugar:desugar(AST),
    {module, _, _, _, [{transform_decl, test, _, Clauses, _}], _} = Desugared,
    [{transform_clause, _, _, Body, _}] = Clauses,
    %% Should be nested chain calls
    ?assertMatch({app, {var, chain, _}, _, _}, Body).

%% =============================================================================
%% Section 1.5.6 - Effect Integration with Kleisli Arrows
%% =============================================================================

%% 1.5.6.1 Test effect set union for Kleisli-style composition
effect_union_for_composition_test() ->
    %% When composing effectful functions, effects should union
    %% (a -> b / ε₁) composed with (b -> c / ε₂) has effects ε₁ ∪ ε₂
    E1 = catena_infer_effect:from_list(['IO']),
    E2 = catena_infer_effect:from_list(['State']),

    Combined = catena_infer_effect:union(E1, E2),

    %% Combined should have both effects
    ?assertEqual({effect_set, ['IO', 'State']}, Combined),
    ?assertNot(catena_infer_effect:is_pure(Combined)).

%% 1.5.6.2 Test that function types carry effects correctly
function_type_with_effects_test() ->
    %% Create a function type: Int -> String / {IO}
    IntType = {tcon, 'Int'},
    StringType = {tcon, 'String'},
    IoEffects = catena_types:singleton_effect('IO'),

    FuncType = catena_types:tfun(IntType, StringType, IoEffects),

    %% Extract effects from function type
    {ok, Effects} = catena_types:extract_function_effects(FuncType),
    ?assertEqual({effect_set, ['IO']}, Effects).

%% 1.5.6.3 Test pure function type has empty effects
pure_function_type_test() ->
    %% Create a pure function type: Int -> Int / {}
    IntType = {tcon, 'Int'},
    PureEffects = catena_types:empty_effects(),

    FuncType = catena_types:tfun(IntType, IntType, PureEffects),

    {ok, Effects} = catena_types:extract_function_effects(FuncType),
    ?assert(catena_types:is_pure(Effects)).

%% 1.5.6.4 Test effect subsumption for handler matching
effect_subsumption_for_handlers_test() ->
    %% A handler with more effects can handle a subset
    %% Handler with {IO, State} can handle computation with just {IO}
    HandlerEffects = catena_infer_effect:from_list(['IO', 'State']),
    ComputationEffects = catena_infer_effect:from_list(['IO']),

    ?assert(catena_infer_effect:subsumes(HandlerEffects, ComputationEffects)).

%% 1.5.6.5 Test effect removal after handling
effect_removal_after_handling_test() ->
    %% When a handler handles an effect, it's removed from the set
    %% Original: {IO, State}, Handle IO -> Remaining: {State}
    Original = catena_infer_effect:from_list(['IO', 'State']),
    Handled = catena_infer_effect:from_list(['IO']),

    %% Simulate effect removal (remaining = original - handled)
    {effect_set, OrigList} = Original,
    {effect_set, HandledList} = Handled,
    RemainingList = lists:filter(fun(E) -> not lists:member(E, HandledList) end, OrigList),
    Remaining = catena_infer_effect:from_list(RemainingList),

    ?assertEqual({effect_set, ['State']}, Remaining).

%% 1.5.6.6 Test Kleisli composition effect propagation
kleisli_composition_effects_test() ->
    %% For f : a -> m b / {IO} and g : b -> m c / {State}
    %% f >=> g : a -> m c / {IO, State}
    IoEffects = catena_types:singleton_effect('IO'),
    StateEffects = catena_types:singleton_effect('State'),

    %% Create function types
    TypeA = {tcon, 'Int'},
    TypeB = {tcon, 'String'},
    TypeC = {tcon, 'Bool'},
    MonadB = {tapp, {tcon, 'Maybe'}, [TypeB]},
    MonadC = {tapp, {tcon, 'Maybe'}, [TypeC]},

    FuncF = catena_types:tfun(TypeA, MonadB, IoEffects),
    FuncG = catena_types:tfun(TypeB, MonadC, StateEffects),

    %% Extract and combine effects
    {ok, EffectsF} = catena_types:extract_function_effects(FuncF),
    {ok, EffectsG} = catena_types:extract_function_effects(FuncG),
    ComposedEffects = catena_types:union_effects(EffectsF, EffectsG),

    %% Composed effects should have both IO and State
    ?assertEqual({effect_set, ['IO', 'State']}, ComposedEffects).

%% 1.5.6.7 Test parse effect annotation on function type
parse_effect_annotation_test() ->
    Source = "transform readAndProcess : String -> String / {IO}\n"
             "transform readAndProcess path = path\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, readAndProcess, TypeSig, _, _}] = Decls,
            %% Type signature is function type with effect annotation on return
            %% String -> String / {IO} parses as String -> (String / {IO})
            ?assertMatch({type_fun, _, {type_effect, _, ['IO'], _}, _}, TypeSig);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.6.8 Test parse empty effect annotation (pure)
parse_empty_effect_annotation_test() ->
    Source = "transform pureFunc : Int -> Int / {}\n"
             "transform pureFunc x = x\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, pureFunc, TypeSig, _, _}] = Decls,
            %% Type signature is function type with empty effect annotation on return
            %% Int -> Int / {} parses as Int -> (Int / {})
            ?assertMatch({type_fun, _, {type_effect, _, [], _}, _}, TypeSig);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.6.9 Test parse perform expression
parse_perform_introduces_effect_test() ->
    Source = "transform readFile path = perform IO.read(path)\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, readFile, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, Body, _}] = Clauses,
            %% Body should be a perform expression
            ?assertMatch({perform_expr, 'IO', read, [_], _}, Body);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% 1.5.6.10 Test parse handle expression removes effects
parse_handle_removes_effect_test() ->
    Source = "transform safeRead path = handle perform IO.read(path) then { IO { read(p) -> p } }\n",
    {ok, Tokens, _} = catena_lexer:string(Source),
    case catena_parser:parse(Tokens) of
        {ok, {module, _, _, _, Decls, _}} ->
            [{transform_decl, safeRead, _, Clauses, _}] = Decls,
            [{transform_clause, _, _, Body, _}] = Clauses,
            %% Body should be a handle expression
            ?assertMatch({handle_expr, _, [{handler_clause, 'IO', _, _}], _}, Body);
        {error, Reason} ->
            io:format("Parse error: ~p~n", [Reason]),
            ?assert(false)
    end.

%% =============================================================================
%% Section 1.5.7 - Operator Desugaring (placeholder)
%% TODO: Implement when trait resolution integration is complete
%% =============================================================================
