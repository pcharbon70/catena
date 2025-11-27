-module(catena_parser_regression_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Automated Regression Tests
%% These tests ensure that removed syntax features never get
%% accidentally reintroduced during development.
%%====================================================================

%%====================================================================
%% Removed Operators Registry
%% Add entries here whenever operators are removed from the language
%%====================================================================

%% @doc Registry of operators removed from the language.
%% Each entry contains:
%%   - operator: The operator syntax (string)
%%   - name: Human-readable name
%%   - removed_in: Task/version where it was removed
%%   - reason: Why it was removed
%%   - expected_behavior: How it should tokenize now
%%
%% NOTE: Most category theory operators were restored in Section 1.5.6
%% (Effect Integration with Kleisli Arrows). Only <=< remains removed.
removed_operators() ->
    [
        #{
            operator => "<=<",
            name => "Kleisli right-to-left operator",
            removed_in => "Task 1.1.7",
            reason => "Right-to-left Kleisli composition not needed - use >=> instead",
            expected_behavior => separate_tokens,
            expected_tokens => [lte, lt]
        }
    ].

%%====================================================================
%% Removed Syntax Features Registry
%% Add entries here whenever syntax features are removed
%%====================================================================

%% @doc Registry of syntax features removed from the language.
%% NOTE: Category theory operators were restored in Section 1.5.6.
%% Only <=< (right-to-left Kleisli) remains removed.
removed_syntax_features() ->
    [
        #{
            feature => "kleisli_right_to_left",
            description => "Kleisli right-to-left composition operator (<=<)",
            removed_in => "Task 1.1.7",
            reason => "Use >=> instead - right-to-left composition not needed",
            operators => ["<=<"]
        }
    ].

%%====================================================================
%% Automated Regression Tests
%%====================================================================

%% @doc Test that all removed operators stay removed.
%% This is the main automated regression test.
removed_operators_regression_test_() ->
    [test_removed_operator(Op) || Op <- removed_operators()].

%% @doc Generate a test for a single removed operator.
test_removed_operator(#{operator := OpStr, name := Name, expected_behavior := Behavior} = OpSpec) ->
    TestName = lists:flatten(io_lib:format("~s (~s) stays removed", [Name, OpStr])),
    {TestName, fun() -> verify_operator_removed(OpSpec) end}.

%% @doc Verify that a removed operator behaves as expected.
verify_operator_removed(#{operator := OpStr, expected_behavior := lexer_error, error_pattern := ErrorPattern}) ->
    %% Operator should cause lexer error
    Source = "x " ++ OpStr ++ " y",
    Result = catena_lexer:string(Source),

    ?assertMatch({error, _, _}, Result),
    {error, {_, catena_lexer, ActualError}, _} = Result,
    ?assertEqual(ErrorPattern, ActualError);

verify_operator_removed(#{operator := OpStr, expected_behavior := separate_tokens, expected_tokens := ExpectedTokens}) ->
    %% Operator should tokenize as separate tokens
    Source = "x " ++ OpStr ++ " y",
    {ok, Tokens, _} = catena_lexer:string(Source),

    %% Extract token types (ignore line numbers and values)
    TokenTypes = [extract_token_type(T) || T <- Tokens],

    %% Verify expected tokens are present
    lists:foreach(
        fun(ExpectedToken) ->
            ?assert(
                lists:member(ExpectedToken, TokenTypes),
                io_lib:format("Expected token ~p not found in ~p", [ExpectedToken, TokenTypes])
            )
        end,
        ExpectedTokens
    ),

    %% Verify it does NOT contain a single-token version
    %% (e.g., no 'bind' token, no 'concat' token, etc.)
    SingleTokenName = operator_to_single_token_name(OpStr),
    case SingleTokenName of
        undefined -> ok;
        TokenName ->
            ?assertNot(
                lists:member(TokenName, TokenTypes),
                io_lib:format("Operator ~s should not tokenize as single token ~p", [OpStr, TokenName])
            )
    end.

%% @doc Extract token type from token tuple.
extract_token_type({TokenType, _}) -> TokenType;
extract_token_type({TokenType, _, _}) -> TokenType;
extract_token_type(Token) -> Token.

%% @doc Map operator string to its old single-token name (if it had one).
%% NOTE: Most operators are now restored. Only <=< remains removed.
operator_to_single_token_name("<=<") -> kleisli_rl;
operator_to_single_token_name(_) -> undefined.

%%====================================================================
%% Comprehensive Regression Test
%%====================================================================

%% @doc Verify all removed operators in a single comprehensive test.
%% This provides a summary view of removal status.
all_removed_operators_test() ->
    Operators = removed_operators(),
    TotalCount = length(Operators),

    io:format("~n========================================~n"),
    io:format("Removed Operators Regression Test~n"),
    io:format("========================================~n"),
    io:format("Testing ~p removed operators...~n~n", [TotalCount]),

    Results = lists:map(
        fun(#{operator := Op, name := Name} = Spec) ->
            try
                verify_operator_removed(Spec),
                io:format("✓ ~s (~s) - correctly removed~n", [Name, Op]),
                {ok, Op}
            catch
                _:Reason ->
                    io:format("✗ ~s (~s) - FAILED: ~p~n", [Name, Op, Reason]),
                    {error, Op, Reason}
            end
        end,
        Operators
    ),

    Failures = [R || {error, _, _} = R <- Results],
    SuccessCount = TotalCount - length(Failures),

    io:format("~n========================================~n"),
    io:format("Results: ~p/~p operators verified~n", [SuccessCount, TotalCount]),
    io:format("========================================~n~n"),

    ?assertEqual([], Failures, "All removed operators must stay removed").

%%====================================================================
%% Feature-Level Regression Tests
%%====================================================================

%% @doc Test that all operators from removed features stay removed.
removed_features_regression_test_() ->
    [test_removed_feature(Feature) || Feature <- removed_syntax_features()].

test_removed_feature(#{feature := FeatureName, operators := OpList} = FeatureSpec) ->
    TestName = lists:flatten(io_lib:format("Feature '~s' stays removed", [FeatureName])),
    {TestName, fun() -> verify_feature_removed(FeatureSpec) end}.

verify_feature_removed(#{operators := OpList}) ->
    %% Verify each operator in the feature is removed
    RemovedOps = removed_operators(),

    lists:foreach(
        fun(OpStr) ->
            %% Find the operator spec
            OpSpec = lists:filter(
                fun(#{operator := Op}) -> Op =:= OpStr end,
                RemovedOps
            ),

            ?assertNotEqual([], OpSpec,
                io_lib:format("Operator ~s not found in removed_operators() registry", [OpStr])),

            %% Verify it's removed
            [Spec] = OpSpec,
            verify_operator_removed(Spec)
        end,
        OpList
    ).

%%====================================================================
%% Documentation Tests
%%====================================================================

%% @doc Verify that removed operators documentation is complete.
removed_operators_documentation_test() ->
    Operators = removed_operators(),

    %% Each operator must have all required fields
    lists:foreach(
        fun(Op) ->
            ?assertMatch(#{operator := _, name := _, removed_in := _, reason := _, expected_behavior := _}, Op),

            %% Verify expected_behavior is valid
            #{expected_behavior := Behavior} = Op,
            ?assert(
                lists:member(Behavior, [lexer_error, separate_tokens]),
                io_lib:format("Invalid expected_behavior: ~p", [Behavior])
            ),

            %% If lexer_error, must have error_pattern
            case Behavior of
                lexer_error ->
                    ?assertMatch(#{error_pattern := _}, Op);
                separate_tokens ->
                    ?assertMatch(#{expected_tokens := _}, Op)
            end
        end,
        Operators
    ).

%% @doc Count and report removed operators by task.
removed_operators_by_task_test() ->
    Operators = removed_operators(),

    %% Group by task
    ByTask = lists:foldl(
        fun(#{removed_in := Task} = Op, Acc) ->
            maps:update_with(Task, fun(Ops) -> [Op | Ops] end, [Op], Acc)
        end,
        #{},
        Operators
    ),

    io:format("~n========================================~n"),
    io:format("Removed Operators by Task~n"),
    io:format("========================================~n"),

    maps:foreach(
        fun(Task, Ops) ->
            io:format("~s: ~p operators~n", [Task, length(Ops)])
        end,
        ByTask
    ),

    io:format("========================================~n~n"),

    %% This test always passes - it's just for reporting
    ?assert(true).

%%====================================================================
%% Helper Tests
%%====================================================================

%% @doc Test that the registry itself is valid.
registry_validity_test() ->
    Operators = removed_operators(),
    Features = removed_syntax_features(),

    %% Registry should not be empty
    ?assertNotEqual([], Operators, "Removed operators registry should not be empty"),
    ?assertNotEqual([], Features, "Removed features registry should not be empty"),

    %% All operator strings should be unique
    OpStrings = [Op || #{operator := Op} <- Operators],
    UniqueOpStrings = lists:usort(OpStrings),
    ?assertEqual(length(OpStrings), length(UniqueOpStrings), "All operator strings must be unique"),

    %% All feature names should be unique
    FeatureNames = [Name || #{feature := Name} <- Features],
    UniqueFeatureNames = lists:usort(FeatureNames),
    ?assertEqual(length(FeatureNames), length(UniqueFeatureNames), "All feature names must be unique").

%%====================================================================
%% Instructions for Future Developers
%%====================================================================

%% When removing operators or syntax features in the future:
%%
%% 1. Add entry to removed_operators() with:
%%    - operator: The syntax string (e.g., "<$>")
%%    - name: Human-readable name
%%    - removed_in: Task ID where removed
%%    - reason: Why it was removed
%%    - expected_behavior: Either 'lexer_error' or 'separate_tokens'
%%    - error_pattern (if lexer_error) or expected_tokens (if separate_tokens)
%%
%% 2. Add entry to removed_syntax_features() if removing a feature group
%%
%% 3. Run tests to verify: rebar3 eunit --module=catena_parser_regression_tests
%%
%% 4. Tests will automatically verify all registered removals
%%
%% Example:
%%   #{
%%       operator => "@@",
%%       name => "Double-at operator",
%%       removed_in => "Task 2.3.4",
%%       reason => "Replaced with @@ syntax",
%%       expected_behavior => lexer_error,
%%       error_pattern => {illegal, "@"}
%%   }
