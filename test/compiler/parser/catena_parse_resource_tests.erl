-module(catena_parse_resource_tests).
-include_lib("eunit/include/eunit.hrl").

%%============================================================================
%% Test Suite for Parser Resource Limits
%%
%% Tests comprehensive resource protection including:
%% - Token count limits
%% - Parse time limits
%% - Pattern depth limits
%% - Type expression depth limits
%% - AST depth and size limits (existing)
%%============================================================================

%%----------------------------------------------------------------------------
%% Section 1: Token Count Limits
%%----------------------------------------------------------------------------

%% Test 1.1: Normal token count passes
token_count_normal_test() ->
    Source = "type Bool = True | False",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 1.2: Excessive token count fails
token_count_excessive_test() ->
    %% Set a low limit
    OldMax = application:get_env(catena, max_token_count, undefined),
    try
        application:set_env(catena, max_token_count, 5),
        %% This will generate more than 5 tokens
        Source = "type Bool = True | False",
        Result = catena_parse:parse(Source),
        ?assertMatch({error, {too_many_tokens, _, _}}, Result),
        {error, ErrorTuple} = Result,
        Msg = catena_parse:format_error(ErrorTuple),
        ?assert(is_list(Msg))
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_token_count);
            Value -> application:set_env(catena, max_token_count, Value)
        end
    end.

%% Test 1.3: Token count at boundary
token_count_boundary_test() ->
    OldMax = application:get_env(catena, max_token_count, undefined),
    try
        %% Set limit to exact token count
        application:set_env(catena, max_token_count, 6),
        Source = "type Bool = True | False",
        %% Should succeed (6 tokens exactly)
        Result = catena_parse:parse(Source),
        ?assertMatch({ok, _}, Result)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_token_count);
            Value -> application:set_env(catena, max_token_count, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 2: Parse Time Limits
%%----------------------------------------------------------------------------

%% Test 2.1: Fast parse completes
parse_time_normal_test() ->
    Source = "type Bool = True | False",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 2.2: Very short timeout triggers (simulated)
parse_time_short_timeout_test() ->
    %% Set an extremely short timeout
    OldMax = application:get_env(catena, max_parse_time, undefined),
    try
        application:set_env(catena, max_parse_time, 0),
        %% Even simple parse might exceed 0ms
        Source = "type Bool = True | False",
        Result = catena_parse:parse(Source),
        %% Should either succeed quickly or timeout
        case Result of
            {ok, _} -> ok;  % Completed in < 1ms
            {error, {parse_timeout, _, _}} -> ok  % Timed out as expected
        end
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_parse_time);
            Value -> application:set_env(catena, max_parse_time, Value)
        end
    end.

%% Test 2.3: Reasonable timeout passes
parse_time_reasonable_test() ->
    OldMax = application:get_env(catena, max_parse_time, undefined),
    try
        application:set_env(catena, max_parse_time, 5000),  % 5 seconds
        Source = "type List a = Nil | Cons a (List a)\n"
                 "transform map : (a -> b) -> List a -> List b\n"
                 "transform map f xs = match\n"
                 "  | Nil -> Nil\n"
                 "  | Cons(x rest) -> x\n"
                 "end",
        Result = catena_parse:parse(Source),
        ?assertMatch({ok, _}, Result)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_parse_time);
            Value -> application:set_env(catena, max_parse_time, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 3: Pattern Depth Limits
%%----------------------------------------------------------------------------

%% Test 3.1: Shallow pattern passes
pattern_depth_shallow_test() ->
    Source = "transform f x = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 3.2: Nested pattern passes with high limit
pattern_depth_moderate_test() ->
    %% Nested tuple pattern: ((a, b), (c, d))
    Source = "transform f ((a, b), (c, d)) = a",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 3.3: Deeply nested pattern fails with low limit
pattern_depth_excessive_test() ->
    OldMax = application:get_env(catena, max_pattern_depth, undefined),
    try
        application:set_env(catena, max_pattern_depth, 2),
        %% Create a deeply nested pattern: Some(Some(Some(x)))
        Source = "transform f Some(Some(Some(x))) = x",
        Result = catena_parse:parse(Source),
        ?assertMatch({error, {pattern_too_deep, _, _}}, Result),
        {error, ErrorTuple} = Result,
        Msg = catena_parse:format_error(ErrorTuple),
        ?assert(is_list(Msg))
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_pattern_depth);
            Value -> application:set_env(catena, max_pattern_depth, Value)
        end
    end.

%% Test 3.4: List pattern depth
pattern_depth_list_test() ->
    OldMax = application:get_env(catena, max_pattern_depth, undefined),
    try
        application:set_env(catena, max_pattern_depth, 2),
        %% Nested list pattern [[[x]]] (depth 3, exceeds limit of 2)
        Source = "transform f [[[x]]] = x",
        Result = catena_parse:parse(Source),
        ?assertMatch({error, {pattern_too_deep, _, _}}, Result)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_pattern_depth);
            Value -> application:set_env(catena, max_pattern_depth, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 4: Type Depth Limits
%%----------------------------------------------------------------------------

%% Test 4.1: Simple type passes
type_depth_simple_test() ->
    Source = "transform f : Int -> Int\ntransform f x = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 4.2: Function type chain passes
type_depth_moderate_test() ->
    Source = "transform f : Int -> Int -> Int -> Int\ntransform f x y z = x",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 4.3: Deeply nested type fails with low limit
type_depth_excessive_test() ->
    OldMax = application:get_env(catena, max_type_depth, undefined),
    try
        application:set_env(catena, max_type_depth, 3),
        %% Deeply nested type: Maybe (Maybe (Maybe (Maybe Int)))
        Source = "transform f : Maybe (Maybe (Maybe (Maybe Int))) -> Int\n"
                 "transform f x = 0",
        Result = catena_parse:parse(Source),
        ?assertMatch({error, {type_too_deep, _, _}}, Result),
        {error, ErrorTuple} = Result,
        Msg = catena_parse:format_error(ErrorTuple),
        ?assert(is_list(Msg))
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_type_depth);
            Value -> application:set_env(catena, max_type_depth, Value)
        end
    end.

%% Test 4.4: Forall quantification depth
type_depth_forall_test() ->
    OldMax = application:get_env(catena, max_type_depth, undefined),
    try
        application:set_env(catena, max_type_depth, 2),
        %% forall a . forall b . forall c . a -> b -> c
        Source = "transform f : forall a . forall b . forall c . a -> b -> c\n"
                 "transform f x y = y",
        Result = catena_parse:parse(Source),
        ?assertMatch({error, {type_too_deep, _, _}}, Result)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_type_depth);
            Value -> application:set_env(catena, max_type_depth, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 5: Configuration Tests
%%----------------------------------------------------------------------------

%% Test 5.1: Get default token count limit
get_max_token_count_test() ->
    Count = catena_parse:get_max_token_count(),
    ?assert(is_integer(Count)),
    ?assert(Count > 0).

%% Test 5.2: Get default parse time limit
get_max_parse_time_test() ->
    Time = catena_parse:get_max_parse_time(),
    ?assert(is_integer(Time)),
    ?assert(Time > 0).

%% Test 5.3: Get default pattern depth limit
get_max_pattern_depth_test() ->
    Depth = catena_parse:get_max_pattern_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

%% Test 5.4: Get default type depth limit
get_max_type_depth_test() ->
    Depth = catena_parse:get_max_type_depth(),
    ?assert(is_integer(Depth)),
    ?assert(Depth > 0).

%% Test 5.5: Override token count limit
override_token_count_test() ->
    OldMax = application:get_env(catena, max_token_count, undefined),
    try
        application:set_env(catena, max_token_count, 999),
        Count = catena_parse:get_max_token_count(),
        ?assertEqual(999, Count)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_token_count);
            Value -> application:set_env(catena, max_token_count, Value)
        end
    end.

%% Test 5.6: Override parse time limit
override_parse_time_test() ->
    OldMax = application:get_env(catena, max_parse_time, undefined),
    try
        application:set_env(catena, max_parse_time, 888),
        Time = catena_parse:get_max_parse_time(),
        ?assertEqual(888, Time)
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_parse_time);
            Value -> application:set_env(catena, max_parse_time, Value)
        end
    end.

%%----------------------------------------------------------------------------
%% Section 6: Integration Tests
%%----------------------------------------------------------------------------

%% Test 6.1: Complex valid code passes all limits
integration_complex_valid_test() ->
    Source = "type Tree a = Leaf a | Node (Tree a) a (Tree a)\n"
             "transform sum : Tree Int -> Int\n"
             "transform sum tree = match\n"
             "  | Leaf(x) -> x\n"
             "  | Node(left val right) -> (sum left) + val + (sum right)\n"
             "end",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).

%% Test 6.2: Multiple limits enforced together
integration_multiple_limits_test() ->
    %% Set multiple low limits
    OldTokens = application:get_env(catena, max_token_count, undefined),
    OldDepth = application:get_env(catena, max_ast_depth, undefined),
    try
        application:set_env(catena, max_token_count, 100),
        application:set_env(catena, max_ast_depth, 50),

        Source = "type Bool = True | False\n"
                 "transform not x = match | True -> False | False -> True end",

        Result = catena_parse:parse(Source),
        %% Should succeed (within both limits)
        ?assertMatch({ok, _}, Result)
    after
        case OldTokens of
            undefined -> application:unset_env(catena, max_token_count);
            V1 -> application:set_env(catena, max_token_count, V1)
        end,
        case OldDepth of
            undefined -> application:unset_env(catena, max_ast_depth);
            V2 -> application:set_env(catena, max_ast_depth, V2)
        end
    end.

%% Test 6.3: All limits at default pass reasonable code
integration_defaults_test() ->
    Source = "type List a = Nil | Cons a (List a)\n"
             "transform length xs = match\n"
             "  | Nil -> 0\n"
             "  | Cons(_ rest) -> 1 + length rest\n"
             "end\n"
             "transform map f xs = match\n"
             "  | Nil -> Nil\n"
             "  | Cons(x rest) -> x\n"
             "end",
    Result = catena_parse:parse(Source),
    ?assertMatch({ok, _}, Result).
