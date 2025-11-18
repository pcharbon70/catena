-module(catena_parser_effect_limits_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Effect Resource Limits Tests
%%====================================================================
%%
%% Tests for effect-specific resource limits and protection mechanisms.
%% These tests ensure the parser enforces reasonable bounds on effect
%% constructs to prevent resource exhaustion and DOS attacks.

%% Test effect limits getter functions
effect_limits_getters_test() ->
    %% Verify all getter functions return reasonable defaults
    ?assert(is_integer(catena_parse:get_max_effects_per_module())),
    ?assert(catena_parse:get_max_effects_per_module() > 0),
    ?assertEqual(50, catena_parse:get_max_effects_per_module()),  % Default
    
    ?assert(is_integer(catena_parse:get_max_operations_per_effect())),
    ?assert(catena_parse:get_max_operations_per_effect() > 0),
    ?assertEqual(100, catena_parse:get_max_operations_per_effect()),  % Default
    
    ?assert(is_integer(catena_parse:get_max_effects_in_annotation())),
    ?assert(catena_parse:get_max_effects_in_annotation() > 0),
    ?assertEqual(10, catena_parse:get_max_effects_in_annotation()),  % Default
    
    ?assert(is_integer(catena_parse:get_max_effect_handler_depth())),
    ?assert(catena_parse:get_max_effect_handler_depth() > 0),
    ?assertEqual(20, catena_parse:get_max_effect_handler_depth()),  % Default
    
    ?assert(is_integer(catena_parse:get_max_effect_identifier_length())),
    ?assert(catena_parse:get_max_effect_identifier_length() > 0),
    ?assertEqual(100, catena_parse:get_max_effect_identifier_length()).  % Default

%% Test error formatting for effect limit violations
effect_limits_error_formatting_test() ->
    %% Test that all new effect limit error types format correctly
    %% Focus on basic formatting and structure rather than exact string matching
    
    Error1 = {too_many_effects, 3, 2},
    Msg1 = catena_parse:format_error(Error1),
    ?assert(is_list(Msg1)),
    Msg1Str = lists:flatten(Msg1),  % Convert io_list to string
    ?assert(string:str(Msg1Str, "Too many effect") > 0),
    
    Error2 = {too_many_operations, 'BigEffect', 5, 3},
    Msg2 = catena_parse:format_error(Error2),
    ?assert(is_list(Msg2)),
    Msg2Str = lists:flatten(Msg2),
    ?assert(string:str(Msg2Str, "too many operations") > 0),
    ?assert(string:str(Msg2Str, "BigEffect") > 0),
    
    Error3 = {effect_name_too_long, 'VeryLongEffectName', 12, 10},
    Msg3 = catena_parse:format_error(Error3),
    ?assert(is_list(Msg3)),
    Msg3Str = lists:flatten(Msg3),
    ?assert(string:str(Msg3Str, "too long") > 0),
    ?assert(string:str(Msg3Str, "12") > 0),
    
    Error4 = {operation_name_too_long, 'longOperation', 8, 5},
    Msg4 = catena_parse:format_error(Error4),
    ?assert(is_list(Msg4)),
    
    Error5 = {too_many_effects_in_annotation, 4, 2},
    Msg5 = catena_parse:format_error(Error5),
    ?assert(is_list(Msg5)),
    Msg5Str = lists:flatten(Msg5),
    ?assert(string:str(Msg5Str, "too many effects") > 0).

%% Test configurable limits
effect_limits_configuration_test() ->
    %% Test that limits can be configured via application environment
    OldMaxEffects = application:get_env(catena, max_effects_per_module, undefined),
    try
        application:set_env(catena, max_effects_per_module, 25),
        ?assertEqual(25, catena_parse:get_max_effects_per_module())
    after
        case OldMaxEffects of
            undefined -> application:unset_env(catena, max_effects_per_module);
            Value -> application:set_env(catena, max_effects_per_module, Value)
        end
    end.

effect_limits_operations_config_test() ->
    %% Test operation limit configuration separately
    OldMaxOps = application:get_env(catena, max_operations_per_effect, undefined),
    try
        application:set_env(catena, max_operations_per_effect, 50),
        ?assertEqual(50, catena_parse:get_max_operations_per_effect())
    after
        case OldMaxOps of
            undefined -> application:unset_env(catena, max_operations_per_effect);
            Value -> application:set_env(catena, max_operations_per_effect, Value)
        end
    end.

%% Test validation framework basic functionality
effect_limits_validation_test() ->
    %% Test that the validation framework integrates correctly
    %% This test verifies the infrastructure is in place
    OldMaxEffects = application:get_env(catena, max_effects_per_module, undefined),
    try
        %% Set very low limit to trigger validation
        application:set_env(catena, max_effects_per_module, 1),
        
        %% This should fail validation during parsing
        Code = "effect First\n  operation op1\nend\neffect Second\n  operation op2\nend",
        
        %% The parse should fail due to our new effect validation
        Result = catena_parse:parse(Code),
        ?assertMatch({error, _}, Result),
        
        case Result of
            {error, {too_many_effects, 2, 1}} -> 
                ?assert(true);  % Expected our new error
            {error, _Other} -> 
                ?assert(false, "Expected effect limit error but got: " ++ io_lib:format("~p", [_Other]))
        end
    after
        case OldMaxEffects of
            undefined -> application:unset_env(catena, max_effects_per_module);
            Value -> application:set_env(catena, max_effects_per_module, Value)
        end
    end.