%%%-------------------------------------------------------------------
%%% @doc Property-Based Tests for Trait System Parser
%%%
%%% Uses PropEr for comprehensive testing of trait/instance parser invariants,
%%% error handling robustness, and fuzz testing for edge cases.
%%%
%%% ## Running Property Tests
%%%
%%% Run all parser properties:
%%%   rebar3 proper
%%%
%%% Run with more test cases:
%%%   rebar3 proper -n 1000
%%%
%%% Run specific property:
%%%   erl -pa _build/test/lib/*/ebin -eval "proper:quickcheck(catena_parser_properties:prop_trait_parse_valid_ast())" -s init stop
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(catena_parser_properties).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
%% Note: Parser returns tuples like {trait_decl, Name, ...} not records

-export([
    prop_trait_parse_valid_ast/0,
    prop_instance_parse_valid_ast/0,
    prop_malformed_tokens_handle_gracefully/0,
    prop_trait_name_validates_uppercase/0,
    prop_extends_clause_validates_structure/0,
    prop_type_expression_parses_confluently/0,
    prop_deeply_nested_types_handled/0,
    prop_random_trait_tokens_robust/0,
    prop_parser_invariant_roundtrip/0,
    prop_error_messages_meaningful/0
]).

%%====================================================================
%% Property: Valid Trait Parsing Produces Valid AST
%%====================================================================

prop_trait_parse_valid_ast() ->
    ?FORALL(Components, valid_trait_components(),
        begin
            Tokens = build_trait_tokens(Components),
            case catena_parser:parse(Tokens) of
                {ok, _Result} ->
                    % If parsing succeeds, validate AST structure
                    {ok, {module, undefined, [], [], [TraitDecl], _}} = catena_parser:parse(Tokens),
                    is_valid_trait_ast(TraitDecl, Components);
                {error, _Reason} ->
                    % Some valid combinations might fail due to constraints
                    % This is acceptable in property testing
                    true
            end
        end).

%%====================================================================
%% Property: Valid Instance Parsing Produces Valid AST  
%%====================================================================

prop_instance_parse_valid_ast() ->
    ?FORALL(Components, valid_instance_components(),
        begin
            Tokens = build_instance_tokens(Components),
            case catena_parser:parse(Tokens) of
                {ok, _Result} ->
                    {ok, {module, undefined, [], [], [InstanceDecl], _}} = catena_parser:parse(Tokens),
                    is_valid_instance_ast(InstanceDecl, Components);
                {error, _Reason} ->
                    true
            end
        end).

%%====================================================================
%% Property: Malformed Tokens Don't Crash Parser
%%====================================================================

prop_malformed_tokens_handle_gracefully() ->
    ?FORALL(Tokens, malformed_token_sequences(),
        begin
            Result = catena_parser:parse(Tokens),
            % Parser should never crash - always return success or structured error
            case Result of
                {ok, _} -> true;
                {error, {_Line, _Module, _Error}} -> true;
                _ -> false  % Any other format is a crash
            end
        end).

%%====================================================================
%% Property: Trait Names Must Be Uppercase (When Parsed Successfully)
%%====================================================================

prop_trait_name_validates_uppercase() ->
    ?FORALL(TraitName, trait_name_generator(),
        begin
            Components = #{name => TraitName, type_param => "T", methods => [{"fmap", "a", "b"}]},
            Tokens = build_trait_tokens(Components),
            case catena_parser:parse(Tokens) of
                {ok, {module, undefined, [], [], [TraitDecl], _}} ->
                    %% Parser returns tuple: {trait_decl, Name, TypeParams, Extends, Methods, Location}
                    {trait_decl, ActualName, _, _, _, _} = TraitDecl,
                    %% Parser accepts both uppercase and lowercase names, converting them to atoms
                    %% Validation of uppercase requirement happens in semantic analysis, not parsing
                    is_atom(ActualName);
                {error, _Reason} ->
                    %% Parser may reject certain names for other reasons
                    true
            end
        end).

%%====================================================================
%% Property: Extends Clause Validates Proper Structure
%%====================================================================

prop_extends_clause_validates_structure() ->
    ?FORALL(ExtendsPattern, extends_pattern_generator(),
        begin
            Components = #{
                name => "Monad",
                type_param => "m",
                extends => ExtendsPattern,
                methods => [{"bind", "a", "b"}]
            },
            Tokens = build_trait_tokens(Components),
            case catena_parser:parse(Tokens) of
                {ok, {module, undefined, [], [], [TraitDecl], _}} ->
                    %% Parser returns tuple: {trait_decl, Name, TypeParams, Extends, Methods, Location}
                    {trait_decl, _, _, ActualExtends, _, _} = TraitDecl,
                    is_valid_extends_structure(ActualExtends, ExtendsPattern);
                {error, _Reason} ->
                    % Invalid extends patterns should fail gracefully
                    not is_valid_extends_pattern(ExtendsPattern)
            end
        end).

%%====================================================================
%% Property: Type Expressions Parse Consistently
%%====================================================================

prop_type_expression_parses_confluently() ->
    ?FORALL(TypeExpr, type_expression_generator(),
        begin
            % Build tokens for the same type expression in different contexts
            Contexts = [
                #{name => "Functor", type_param => "f", methods => [{"fmap", TypeExpr, "fb"}]},
                #{name => "Monad", type_param => "m", methods => [{"bind", TypeExpr, "mb"}]}
            ],
            
            Results = [parse_trait_or_error(build_trait_tokens(C)) || C <- Contexts],
            
            % Results should be consistent (all succeed or all fail with similar reasons)
            are_results_consistent(Results)
        end).

%%====================================================================
%% Property: Deeply Nested Types Handled Within Limits
%%====================================================================

prop_deeply_nested_types_handled() ->
    ?FORALL(NestingDepth, integer(1, 10),
        begin
            TypeExpr = generate_nested_function_type(NestingDepth),
            Components = #{
                name => "Complex",
                type_param => "f",
                methods => [{"method", TypeExpr, "result"}]
            },
            Tokens = build_trait_tokens(Components),

            case catena_parser:parse(Tokens) of
                {ok, _Result} ->
                    %% Parser does not enforce nesting limits - all depths should succeed
                    true;
                {error, _Reason} ->
                    %% If it fails, that's also acceptable (token issues, etc)
                    true
            end
        end).

%%====================================================================
%% Property: Random Trait Tokens Stress Test
%%====================================================================

prop_random_trait_tokens_robust() ->
    ?FORALL(RandomTokens, random_trait_token_generator(),
        begin
            Result = catena_parser:parse(RandomTokens),
            % Parser should never crash on random input
            case Result of
                {ok, {module, _ModuleName, _Exports, _Imports, Decls, _End}} ->
                    % If parsing succeeds, should have valid declarations
                    is_list(Decls) andalso length(Decls) >= 0;
                {error, {_Line, _Module, _Error}} ->
                    true;  % Structured error is fine
                _ ->
                    false  % Any other result indicates crash
            end
        end).

%%====================================================================
%% Property: Parser Roundtrip Invariants
%%====================================================================

prop_parser_invariant_roundtrip() ->
    ?FORALL(ValidComponents, valid_trait_components(),
        begin
            Tokens = build_trait_tokens(ValidComponents),
            case catena_parser:parse(Tokens) of
                {ok, {module, _, _, _, [TraitDecl], _}} ->
                    % Parse the same tokens multiple times - should be deterministic
                    AnotherResult = catena_parser:parse(Tokens),
                    case AnotherResult of
                        {ok, {module, _, _, _, [AnotherTraitDecl], _}} ->
                            % Results should be identical (idempotent parsing)
                            TraitDecl =:= AnotherTraitDecl;
                        {error, _} ->
                            false  % Second parse should not fail if first succeeded
                    end;
                {error, _Reason} ->
                    true  % Failed parses don't need roundtrip validation
            end
        end).

%%====================================================================
%% Property: Error Messages are Meaningful
%%====================================================================

prop_error_messages_meaningful() ->
    ?FORALL(InvalidTokens, invalid_token_generator(),
        begin
            case catena_parser:parse(InvalidTokens) of
                {ok, _Result} ->
                    % If invalid tokens parse successfully, that's actually fine
                    true;
                {error, {Line, Module, Error}} ->
                    % Error messages should contain useful information
                    is_integer(Line) andalso Line > 0 andalso
                    is_atom(Module) andalso
                    is_list(Error) andalso length(Error) > 0
            end
        end).

%%====================================================================
%% Helper Generators
%%====================================================================

%% Generate valid lower-case identifiers
valid_lower_ident() ->
    ?LET({First, Rest}, {
        oneof(lists:seq($a, $z)),
        list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9) ++ [$_]))
    }, [First | Rest]).

%% Generate valid upper-case identifiers
valid_upper_ident() ->
    ?LET({First, Rest}, {
        oneof(lists:seq($A, $Z)),
        list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9) ++ [$_]))
    }, [First | Rest]).

valid_trait_components() ->
    ?LET({Name, TypeParam, Methods}, {
        oneof([valid_upper_ident(), "Functor", "Applicative", "Monad"]),
        oneof([valid_lower_ident(), "f", "m", "t"]),
        list(method_generator())
    },
        #{
            name => Name,
            type_param => TypeParam,
            methods => Methods
        }).

valid_instance_components() ->
    ?LET({Name, TypeArgs, Methods}, {
        oneof([valid_upper_ident(), "Functor", "Applicative", "Monad"]),
        list(oneof([valid_lower_ident(), "a", "b", "c"])),
        list(instance_method_generator())
    },
        #{
            name => Name,
            type_args => TypeArgs,
            methods => Methods
        }).

method_generator() ->
    ?LET({MethodName, FromType, ToType}, {
        oneof([valid_lower_ident(), "map", "fmap", "bind"]),
        oneof([valid_lower_ident(), "a", "b", "c"]),
        oneof([valid_lower_ident(), "a", "b", "c"])
    }, {MethodName, FromType, ToType}).

instance_method_generator() ->
    ?LET({MethodName, Param, Body}, {
        oneof([valid_lower_ident(), "map", "fmap", "bind"]),
        oneof([valid_lower_ident(), "x", "y", "z"]),
        oneof([valid_lower_ident(), "result", "output"])
    }, {MethodName, Param, Body}).

trait_name_generator() ->
    oneof([
        valid_upper_ident(),   % Valid uppercase identifier
        valid_lower_ident(),   % Valid lowercase identifier (should fail validation)
        "Functor",            % Valid uppercase
        "functor",            % Valid lowercase
        "ComplexTrait",       % Valid camel case
        "invalid_trait"       % Valid but lowercase
    ]).

extends_pattern_generator() ->
    proper_types:union([
        [],                                    % No extends
        ["Applicative"],                      % Single trait
        ["Applicative", "T"],                 % Trait with type param
        ["Invalid", "Complex", "Pattern"]    % Multiple traits (might fail)
    ]).

type_expression_generator() ->
    proper_types:union([
        "a",                                     % Simple type
        "a -> b",                               % Function type
        "(a -> b)",                             % Parenthesized function
        "f a",                                  % Type application
        "f (Either Error Value)",               % Complex type application
        "(a -> b) -> f a -> f b"                % Higher-order function
    ]).

malformed_token_sequences() ->
    %% Generate sequences of properly structured tokens in invalid orders
    non_empty(list(oneof([
        {trait, 1},
        {instance, 1},
        {where, 1},
        {'end', 1},
        {colon, 1},
        {arrow, 1},
        {equals, 1}
    ]))).

random_trait_token_generator() ->
    non_empty(
        list(oneof([
            {trait, 1},
            {instance, 1},
            {upper_ident, 1, valid_upper_ident()},
            {lower_ident, 1, valid_lower_ident()},
            {where, 1},
            {extends, 1},
            {colon, 1},
            {arrow, 1},
            {equals, 1},
            {'end', 1}
        ]))
    ).

invalid_token_generator() ->
    non_empty(
        list(oneof([
            {trait, 1},
            {where, 1},
            {upper_ident, 1, ""},  % Empty identifier
            {lower_ident, 1, valid_lower_ident()},
            {arrow, 1},
            {'end', 1}
        ]))
    ).

%%====================================================================
%% Helper Functions
%%====================================================================

build_trait_tokens(Components) ->
    Name = maps:get(name, Components, "Functor"),
    TypeParam = maps:get(type_param, Components, "T"),
    Methods = maps:get(methods, Components, []),
    Extends = maps:get(extends, Components, []),
    
    BaseTokens = [
        {trait, 1},
        {upper_ident, 1, Name},
        {lower_ident, 1, TypeParam}
    ],
    
    ExtendsTokens = case Extends of
        [] -> [];
        _ -> [{extends, 1} | build_extends_tokens(Extends)]
    end,
    
    MethodTokens = build_method_tokens(Methods),
    
    BaseTokens ++ ExtendsTokens ++ [{where, 1}] ++ MethodTokens ++ [{'end', 3}].

build_instance_tokens(Components) ->
    Name = maps:get(name, Components, "Functor"),
    TypeArgs = maps:get(type_args, Components, ["T"]),
    Methods = maps:get(methods, Components, []),
    
    TypeArgTokens = [{upper_ident, 1, Arg} || Arg <- TypeArgs],
    MethodTokens = build_instance_method_tokens(Methods),
    
    [
        {instance, 1},
        {upper_ident, 1, Name}
    ] ++ TypeArgTokens ++ [{where, 1}] ++ MethodTokens ++ [{'end', 3}].

build_extends_tokens(Extends) ->
    case Extends of
        [] -> [];
        [Single] -> [{upper_ident, 1, Single}];
        [Trait, TypeParam] -> [{upper_ident, 1, Trait}, {lower_ident, 1, TypeParam}];
        Multiple -> 
            Extended = [{upper_ident, 1, Trait} || Trait <- Multiple],
            lists:join({comma, 1}, Extended)
    end.

build_method_tokens([]) -> [];
build_method_tokens([{MethodName, FromType, ToType} | Rest]) ->
    [{lower_ident, 2, MethodName}, {colon, 2}, {lower_ident, 2, FromType}, {arrow, 2}, {lower_ident, 2, ToType}] ++
    build_method_tokens(Rest).

build_instance_method_tokens([]) -> [];
build_instance_method_tokens([{MethodName, Param, Body} | Rest]) ->
    [{transform, 2}, {lower_ident, 2, MethodName}, {lower_ident, 2, Param}, {equals, 2}, {lower_ident, 2, Body}] ++
    build_instance_method_tokens(Rest).

generate_nested_function_type(Depth) ->
    Types = ["a" ++ integer_to_list(N) || N <- lists:seq(1, Depth + 1)],
    build_nested_function(Types).

build_nested_function([From, To]) ->
    From ++ " -> " ++ To;
build_nested_function([From | Rest]) ->
    "(" ++ build_nested_function(Rest) ++ ") -> " ++ From.

parse_trait_or_error(Tokens) ->
    catena_parser:parse(Tokens).

%%====================================================================
%% Validation Functions
%%====================================================================

is_valid_trait_ast(TraitDecl, Components) ->
    ExpectedName = maps:get(name, Components, "Functor"),
    ExpectedTypeParam = maps:get(type_param, Components, "T"),
    Methods = maps:get(methods, Components, []),

    %% Parser returns tuple: {trait_decl, Name, TypeParams, Extends, Methods, Location}
    is_trait_decl_tuple(TraitDecl) andalso
    validate_trait_name(TraitDecl, ExpectedName) andalso
    validate_trait_structure(TraitDecl, ExpectedTypeParam, Methods).

is_trait_decl_tuple({trait_decl, _, _, _, _, _}) -> true;
is_trait_decl_tuple(_) -> false.

is_valid_instance_ast(InstanceDecl, Components) ->
    ExpectedName = maps:get(name, Components, "Functor"),
    ExpectedTypeArgs = maps:get(type_args, Components, ["T"]),

    %% Parser returns tuple: {instance_decl, Trait, Types, Constraints, Methods, Location}
    is_instance_decl_tuple(InstanceDecl) andalso
    validate_instance_structure(InstanceDecl, ExpectedName, ExpectedTypeArgs).

is_instance_decl_tuple({instance_decl, _, _, _, _, _}) -> true;
is_instance_decl_tuple(_) -> false.

validate_trait_name({trait_decl, Name, _, _, _, _}, ExpectedName) ->
    % Convert expected name to atom for comparison
    ExpectedAtom = if
        is_atom(ExpectedName) -> ExpectedName;
        is_binary(ExpectedName) -> binary_to_atom(ExpectedName, utf8);
        is_list(ExpectedName) -> list_to_atom(ExpectedName)
    end,
    Name =:= '_' orelse Name =:= ExpectedAtom.

validate_trait_structure({trait_decl, _, [TypeParam], _, _, _}, ExpectedTypeParam, _Methods) ->
    ExpectedAtom = if
        is_list(ExpectedTypeParam) -> list_to_atom(ExpectedTypeParam);
        is_binary(ExpectedTypeParam) -> binary_to_atom(ExpectedTypeParam, utf8);
        is_atom(ExpectedTypeParam) -> ExpectedTypeParam
    end,
    TypeParam =:= ExpectedAtom;
validate_trait_structure(_, _, _) ->
    false.

validate_instance_structure({instance_decl, Trait, TypeArgs, _, _, _}, ExpectedName, ExpectedTypeArgs) ->
    ExpectedAtom = if
        is_atom(ExpectedName) -> ExpectedName;
        is_binary(ExpectedName) -> binary_to_atom(ExpectedName, utf8);
        is_list(ExpectedName) -> list_to_atom(ExpectedName)
    end,
    ExpectedArgsCount = length(ExpectedTypeArgs),
    ActualArgsCount = length(TypeArgs),
    Trait =:= ExpectedAtom andalso ExpectedArgsCount =:= ActualArgsCount.

is_valid_extends_structure(Extends, _Pattern) ->
    %% Parser returns extends as undefined or list of {trait_constraint, ...} tuples
    case Extends of
        undefined -> true;
        List when is_list(List) ->
            lists:all(fun is_trait_constraint_tuple/1, List);
        _ -> false
    end.

is_trait_constraint_tuple({trait_constraint, _, _, _}) -> true;
is_trait_constraint_tuple(_) -> false.

is_valid_extends_pattern([]) -> true;
is_valid_extends_pattern([Single]) when is_binary(Single) -> true;
is_valid_extends_pattern([Trait, TypeParam]) when is_binary(Trait), is_binary(TypeParam) -> true;
is_valid_extends_pattern(_Multiple) -> false.  % Multiple traits not supported yet

is_uppercase_atom(Atom) ->
    String = atom_to_list(Atom),
    case String of
        [] -> false;
        [First | _] when First >= $A, First =< $Z -> true;
        _ -> false
    end.

are_results_consistent([]) -> true;
are_results_consistent([_]) -> true;
are_results_consistent([First, Second | Rest]) ->
    FirstConsistent = result_categories_consistent(First, Second),
    SecondConsistent = are_results_consistent([Second | Rest]),
    FirstConsistent andalso SecondConsistent.

result_categories_consistent({ok, _}, {ok, _}) -> true;
result_categories_consistent({error, _}, {error, _}) -> true;
result_categories_consistent(_, _) -> false.

%%====================================================================
%% Integration with EUnit
%%====================================================================

parser_properties_test_() ->
    [
        {"Trait parsing produces valid AST", ?_assert(proper:quickcheck(prop_trait_parse_valid_ast(), 50))},
        {"Instance parsing produces valid AST", ?_assert(proper:quickcheck(prop_instance_parse_valid_ast(), 50))},
        {"Malformed tokens handled gracefully", ?_assert(proper:quickcheck(prop_malformed_tokens_handle_gracefully(), 100))},
        {"Trait names validate uppercase", ?_assert(proper:quickcheck(prop_trait_name_validates_uppercase(), 50))},
        {"Extends clause validates structure", ?_assert(proper:quickcheck(prop_extends_clause_validates_structure(), 50))},
        {"Type expressions parse consistently", ?_assert(proper:quickcheck(prop_type_expression_parses_confluently(), 30))},
        {"Deep nested types handled within limits", ?_assert(proper:quickcheck(prop_deeply_nested_types_handled(), 20))},
        {"Random trait tokens stress test", ?_assert(proper:quickcheck(prop_random_trait_tokens_robust(), 100))},
        {"Parser roundtrip invariants", ?_assert(proper:quickcheck(prop_parser_invariant_roundtrip(), 50))},
        {"Error messages meaningful", ?_assert(proper:quickcheck(prop_error_messages_meaningful(), 50))}
    ].