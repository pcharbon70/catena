#!/usr/bin/env escript
%%% Demonstration of Comprehensive Parser Resource Limits
%%%
%%% This demo shows all 6 resource limit categories in action.

main([]) ->
    io:format("~n=== Parser Resource Limits Demo ===~n~n"),

    %% Add paths
    code:add_patha("src/compiler/parser"),
    code:add_patha("src/compiler/lexer"),

    demo_token_limit(),
    demo_pattern_depth(),
    demo_type_depth(),
    demo_ast_limits(),
    demo_configuration(),

    io:format("~n=== Demo Complete ===~n"),
    ok.

%%============================================================================
%% Demo Functions
%%============================================================================

demo_token_limit() ->
    io:format("1. Token Count Limit~n"),
    io:format("   Prevents excessive memory usage from large token streams~n~n"),

    %% Save old limit
    OldMax = application:get_env(catena, max_token_count, undefined),

    try
        %% Set a very low limit for demonstration
        application:set_env(catena, max_token_count, 5),
        io:format("   Limit set to: 5 tokens~n"),

        Source = "type Bool = True | False",
        io:format("   Source: ~s~n", [Source]),

        case catena_parse:parse(Source) of
            {ok, _} ->
                io:format("   ✗ Should have failed (exceeded limit)~n");
            {error, Error} ->
                Msg = lists:flatten(catena_parse:format_error(Error)),
                io:format("   ✓ Caught: ~s~n", [Msg])
        end
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_token_count);
            Value -> application:set_env(catena, max_token_count, Value)
        end
    end,
    io:format("~n").

demo_pattern_depth() ->
    io:format("2. Pattern Nesting Depth Limit~n"),
    io:format("   Prevents stack overflow from deeply nested patterns~n~n"),

    OldMax = application:get_env(catena, max_pattern_depth, undefined),

    try
        application:set_env(catena, max_pattern_depth, 2),
        io:format("   Limit set to: 2 levels~n"),

        %% Create a deeply nested pattern
        Source = "transform f Some(Some(Some(x))) = x",
        io:format("   Source: ~s~n", [Source]),
        io:format("   Pattern depth: 3 (exceeds limit)~n"),

        case catena_parse:parse(Source) of
            {ok, _} ->
                io:format("   ✗ Should have failed (exceeded depth)~n");
            {error, Error} ->
                Msg = lists:flatten(catena_parse:format_error(Error)),
                io:format("   ✓ Caught: ~s~n", [Msg])
        end
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_pattern_depth);
            Value -> application:set_env(catena, max_pattern_depth, Value)
        end
    end,
    io:format("~n").

demo_type_depth() ->
    io:format("3. Type Expression Depth Limit~n"),
    io:format("   Prevents issues during type checking~n~n"),

    OldMax = application:get_env(catena, max_type_depth, undefined),

    try
        application:set_env(catena, max_type_depth, 3),
        io:format("   Limit set to: 3 levels~n"),

        %% Create a deeply nested type
        Source = "flow f : Maybe (Maybe (Maybe (Maybe Int))) -> Int\n"
                 "flow f x = 0",
        io:format("   Type: Maybe (Maybe (Maybe (Maybe Int)))~n"),
        io:format("   Type depth: 4 (exceeds limit)~n"),

        case catena_parse:parse(Source) of
            {ok, _} ->
                io:format("   ✗ Should have failed (exceeded depth)~n");
            {error, Error} ->
                Msg = lists:flatten(catena_parse:format_error(Error)),
                io:format("   ✓ Caught: ~s~n", [Msg])
        end
    after
        case OldMax of
            undefined -> application:unset_env(catena, max_type_depth);
            Value -> application:set_env(catena, max_type_depth, Value)
        end
    end,
    io:format("~n").

demo_ast_limits() ->
    io:format("4. AST Depth and Size Limits~n"),
    io:format("   Prevents stack overflow and memory exhaustion~n~n"),

    OldDepth = application:get_env(catena, max_ast_depth, undefined),
    OldNodes = application:get_env(catena, max_ast_nodes, undefined),

    try
        application:set_env(catena, max_ast_depth, 10),
        application:set_env(catena, max_ast_nodes, 20),
        io:format("   Limits set to: depth=10, nodes=20~n"),

        %% This will create a fairly complex AST
        Source = "flow f x = x + x + x + x + x + x + x + x + x + x",
        io:format("   Source: ~s~n", [Source]),

        case catena_parse:parse(Source) of
            {ok, _} ->
                io:format("   ✗ Should have failed (exceeded limit)~n");
            {error, Error} ->
                Msg = lists:flatten(catena_parse:format_error(Error)),
                io:format("   ✓ Caught: ~s~n", [Msg])
        end
    after
        case OldDepth of
            undefined -> application:unset_env(catena, max_ast_depth);
            V1 -> application:set_env(catena, max_ast_depth, V1)
        end,
        case OldNodes of
            undefined -> application:unset_env(catena, max_ast_nodes);
            V2 -> application:set_env(catena, max_ast_nodes, V2)
        end
    end,
    io:format("~n").

demo_configuration() ->
    io:format("5. Configuration and Defaults~n"),
    io:format("   All limits are configurable via application environment~n~n"),

    io:format("   Current limits:~n"),
    io:format("     Token count:    ~p tokens~n", [catena_parse:get_max_token_count()]),
    io:format("     Parse time:     ~p ms~n", [catena_parse:get_max_parse_time()]),
    io:format("     AST depth:      ~p levels~n", [catena_parse:get_max_ast_depth()]),
    io:format("     AST nodes:      ~p nodes~n", [catena_parse:get_max_ast_nodes()]),
    io:format("     Pattern depth:  ~p levels~n", [catena_parse:get_max_pattern_depth()]),
    io:format("     Type depth:     ~p levels~n", [catena_parse:get_max_type_depth()]),

    io:format("~n   Configuration example:~n"),
    io:format("     application:set_env(catena, max_token_count, 1000000).~n"),
    io:format("     application:set_env(catena, max_parse_time, 60000).~n"),
    io:format("~n").
