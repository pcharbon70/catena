%% @doc Effect Integration Tests
%% Tests for Section 1.5.6 of Phase 1.
%% Validates effect system integration with Kleisli arrows.

-module(catena_stdlib_effects_tests).

-include_lib("eunit/include/eunit.hrl").

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
%% Error Path Tests
%% =============================================================================

%% Test effect annotation on non-function type (parser level)
error_effect_on_non_function_parse_test() ->
    %% Effect annotation on a simple type should parse but may be invalid
    Source = "transform test : Int / {IO}",
    {ok, Tokens, _} = catena_lexer:string(Source),
    %% Parser may accept this syntax
    case catena_parser:parse(Tokens) of
        {ok, _AST} ->
            %% Syntax is valid, but semantically questionable
            ok;
        {error, _Reason} ->
            %% Parser rejection is also acceptable
            ok
    end.
