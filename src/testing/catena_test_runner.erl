%% @doc Catena Test Runner (Phase 2.3)
%%
%% This module provides the test execution framework for Catena's first-class
%% testing support. It handles both unit tests (defined with the `test` keyword)
%% and property tests (defined with the `property` keyword).
%%
%% Unit tests are expressions that should evaluate to `true` for passing.
%% Property tests use random generators to verify properties hold for many inputs.
%%
%% Example Catena syntax:
%%   test "addition is commutative" = 1 + 2 == 2 + 1
%%   property "list reverse is involutive" = forall xs : List. reverse (reverse xs) == xs
%%
-module(catena_test_runner).

-export([
    run_tests/1,
    run_tests/2,
    run_test/2,
    run_test/3,
    run_test_value/1,
    run_test_value/2,
    run_suite_value/1,
    run_suite_value/2,
    build_runtime_env/1,
    build_runtime_env/2,
    collect_tests/1,
    evaluate_expr/2,
    format_results/1,
    format_result/1
]).

%% @doc Default number of iterations for property tests
-define(DEFAULT_PROPERTY_ITERATIONS, 100).

%% Test result types
-type test_result() :: {pass, string()} | {fail, string(), term()}.
-type test_results() :: #{
    passed => non_neg_integer(),
    failed => non_neg_integer(),
    total => non_neg_integer(),
    results => [test_result()]
}.

-export_type([test_result/0, test_results/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run all tests in a list of declarations
%% Takes a list of parsed AST declarations and runs all test_decl nodes.
%% Returns aggregate results with pass/fail counts.
-spec run_tests([term()]) -> test_results().
run_tests(Declarations) ->
    run_tests(Declarations, #{}).

%% @doc Run all tests with custom options
%% Options:
%%   - env: runtime environment bindings (default: prelude)
%%   - verbose: print each test as it runs (default: false)
%%   - property_iterations: number of iterations for property tests (default: 100)
-spec run_tests([term()], map()) -> test_results().
run_tests(Declarations, Opts) ->
    Tests = collect_tests(Declarations),
    Env = maps:get(env, Opts, catena_prelude:prelude_bindings()),
    Verbose = maps:get(verbose, Opts, false),

    Results = lists:map(
        fun(TestDecl) ->
            Result = run_test(TestDecl, Env, Opts),
            case Verbose of
                true -> io:format("~s~n", [format_result(Result)]);
                false -> ok
            end,
            Result
        end,
        Tests
    ),

    aggregate_results(Results).

%% @doc Run a single test declaration
%% Returns {pass, Name} or {fail, Name, Reason}
-spec run_test(term(), map()) -> test_result().
run_test(TestDecl, Env) ->
    run_test(TestDecl, Env, #{}).

%% @doc Run a single test with options
-spec run_test(term(), map(), map()) -> test_result().
run_test({test_decl, Name, Body, _Loc}, Env, _Opts) ->
    run_unit_test(Name, Body, Env);
run_test({property_decl, Name, Body, _Loc}, Env, Opts) ->
    Iterations = maps:get(property_iterations, Opts, ?DEFAULT_PROPERTY_ITERATIONS),
    run_property_test(Name, Body, Env, Iterations).

%% @doc Run a first-class Catena Test value produced by stdlib `Test.verify`/`Test.unit`
-spec run_test_value(map()) -> test_result().
run_test_value(TestValue) ->
    run_test_value(TestValue, #{}).

-spec run_test_value(map(), map()) -> test_result().
run_test_value(TestValue, _Opts) when is_map(TestValue) ->
    Name = maps:get(name, TestValue, "<anonymous test>"),
    case maps:get(run, TestValue, undefined) of
        undefined ->
            {fail, Name, missing_run_function};
        Run ->
            try
                case apply_function(Run, [unit_value()]) of
                    passed -> {pass, Name};
                    true -> {pass, Name};
                    ok -> {pass, Name};
                    {failed, Message} -> {fail, Name, Message};
                    {skipped, Message} -> {fail, Name, {skipped, Message}};
                    false -> {fail, Name, {expected_true, false}};
                    Other -> {fail, Name, {unexpected_result, Other}}
                end
            catch
                throw:{assertion_failed, Details} ->
                    {fail, Name, {assertion_failed, Details}};
                error:Error:Stacktrace ->
                    {fail, Name, {error, Error, Stacktrace}};
                Class:Error ->
                    {fail, Name, {exception, Class, Error}}
            end
    end.

%% @doc Run a first-class Catena Suite value and aggregate its results.
-spec run_suite_value(map()) -> test_results().
run_suite_value(SuiteValue) ->
    run_suite_value(SuiteValue, #{}).

-spec run_suite_value(map(), map()) -> test_results().
run_suite_value(SuiteValue, Opts) when is_map(SuiteValue) ->
    Tests = maps:get(tests, SuiteValue, []),
    Results = [run_test_value(TestValue, Opts) || TestValue <- Tests],
    aggregate_results(Results).

%% @doc Build a runtime environment from Catena declarations.
%%
%% This is used by the Stage 2 law-verification path to execute stdlib
%% `Test` and `Laws` modules directly from semantic declarations.
-spec build_runtime_env([term()]) -> map().
build_runtime_env(Declarations) ->
    build_runtime_env(Declarations, #{}).

-spec build_runtime_env([term()], map()) -> map().
build_runtime_env(Declarations, BaseEnv) ->
    lists:foldl(fun add_runtime_decl_to_env/2, BaseEnv, Declarations).

%% @doc Collect all test and property declarations from a list of declarations
-spec collect_tests([term()]) -> [term()].
collect_tests(Declarations) ->
    lists:filter(
        fun({test_decl, _, _, _}) -> true;
           ({property_decl, _, _, _}) -> true;
           (_) -> false
        end,
        Declarations
    ).

%% @doc Format test results as a human-readable string
-spec format_results(test_results()) -> string().
format_results(#{passed := Passed, failed := Failed, total := Total, results := Results}) ->
    Header = io_lib:format("~nRunning ~B test(s)...~n~n", [Total]),

    ResultLines = lists:map(fun format_result/1, Results),

    Summary = case Failed of
        0 -> io_lib:format("~nResults: ~B passed (all tests passed!)~n", [Passed]);
        _ -> io_lib:format("~nResults: ~B passed, ~B failed (~B total)~n", [Passed, Failed, Total])
    end,

    lists:flatten([Header, ResultLines, Summary]).

%% @doc Format a single test result
-spec format_result(test_result()) -> string().
format_result({pass, Name}) ->
    io_lib:format("  ✓ test ~p~n", [Name]);
format_result({fail, Name, Reason}) ->
    ReasonStr = format_failure_reason(Reason),
    io_lib:format("  ✗ test ~p~n      ~s~n", [Name, ReasonStr]).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Run a unit test
-spec run_unit_test(string(), term(), map()) -> test_result().
run_unit_test(Name, Body, Env) ->
    try
        Result = evaluate_expr(Body, Env),
        case Result of
            true -> {pass, Name};
            false -> {fail, Name, {expected_true, false}};
            ok -> {pass, Name};  % assertion passed
            Other -> {fail, Name, {unexpected_result, Other}}
        end
    catch
        throw:{assertion_failed, Details} ->
            {fail, Name, {assertion_failed, Details}};
        error:Error:Stacktrace ->
            {fail, Name, {error, Error, Stacktrace}};
        Class:Error ->
            {fail, Name, {exception, Class, Error}}
    end.

%% @doc Run a property test
-spec run_property_test(string(), term(), map(), pos_integer()) -> test_result().
run_property_test(Name, {property_forall, Bindings, Expr, _Loc}, Env, Iterations) ->
    run_property_iterations(Name, Bindings, Expr, Env, Iterations, 1).

%% @doc Run property test iterations
run_property_iterations(Name, _Bindings, _Expr, _Env, MaxIter, Current) when Current > MaxIter ->
    {pass, Name};
run_property_iterations(Name, Bindings, Expr, Env, MaxIter, Current) ->
    %% Generate values for each binding
    GeneratedValues = generate_values(Bindings),

    %% Create environment with generated values
    TestEnv = maps:merge(Env, GeneratedValues),

    %% Evaluate the property expression
    try
        case evaluate_expr(Expr, TestEnv) of
            true ->
                run_property_iterations(Name, Bindings, Expr, Env, MaxIter, Current + 1);
            false ->
                {fail, Name, {counterexample, Current, GeneratedValues}};
            Other ->
                {fail, Name, {unexpected_result, Current, Other, GeneratedValues}}
        end
    catch
        Class:Error ->
            {fail, Name, {exception_in_property, Current, Class, Error, GeneratedValues}}
    end.

%% @doc Generate values for property bindings
-spec generate_values([{atom(), atom()}]) -> map().
generate_values(Bindings) ->
    lists:foldl(
        fun({VarName, GenName}, Acc) ->
            Value = catena_generators:generate(GenName),
            maps:put(VarName, wrap_generated_value(Value), Acc)
        end,
        #{},
        Bindings
    ).

%% @doc Wrap a generated value for the runtime environment
wrap_generated_value(Value) ->
    %% Runtime bindings expect {Fun, Arity, Type} tuples for functions,
    %% but for generated values we just need the value itself
    {value, Value}.

%% @doc Evaluate an expression in the given environment
%% This is a simplified evaluator for test expressions.
-spec evaluate_expr(term(), map()) -> term().
evaluate_expr({literal, Value, _Type, _Loc}, _Env) ->
    Value;

evaluate_expr({var, true, _Loc}, _Env) ->
    true;
evaluate_expr({var, false, _Loc}, _Env) ->
    false;
evaluate_expr({var, Name, _Loc}, Env) ->
    case maps:get(Name, Env, undefined) of
        undefined ->
            throw({undefined_variable, Name});
        {value, Val} ->
            Val;
        {Fun, 0, _Type} when is_function(Fun) ->
            Fun();
        {Fun, _Arity, _Type} when is_function(Fun) ->
            Fun;
        Val ->
            Val
    end;

evaluate_expr({binary_op, Op, Left, Right, _Loc}, Env) ->
    L = evaluate_expr(Left, Env),
    R = evaluate_expr(Right, Env),
    apply_binary_op(Op, L, R);

evaluate_expr({app, Fun, Args, _Loc}, Env) ->
    FunVal = evaluate_expr(Fun, Env),
    ArgVals = [evaluate_expr(Arg, Env) || Arg <- Args],
    apply_function(FunVal, ArgVals);

evaluate_expr({list_expr, Elements, _Loc}, Env) ->
    [evaluate_expr(E, Env) || E <- Elements];

evaluate_expr({tuple_expr, Elements, _Loc}, Env) ->
    list_to_tuple([evaluate_expr(E, Env) || E <- Elements]);

evaluate_expr({let_expr, [{pat_var, Name, _}, Value], Body, _Loc}, Env) ->
    Val = evaluate_expr(Value, Env),
    NewEnv = maps:put(Name, {value, Val}, Env),
    evaluate_expr(Body, NewEnv);

evaluate_expr({lambda, Params, Body, _Loc}, Env) ->
    make_lambda(Params, Body, Env);

evaluate_expr({match_expr, Scrutinee, Clauses, _Loc}, Env) ->
    Val = case Scrutinee of
        undefined -> undefined;
        _ -> evaluate_expr(Scrutinee, Env)
    end,
    evaluate_match(Val, Clauses, Env);

evaluate_expr({record_expr, Fields, _, _Loc}, Env) ->
    maps:from_list([{Name, evaluate_expr(Value, Env)} || {Name, Value} <- Fields]);

evaluate_expr({record_access, Expr, Field, _Loc}, Env) ->
    Record = evaluate_expr(Expr, Env),
    maps:get(Field, Record);

evaluate_expr({cons_expr, Head, Tail, _Loc}, Env) ->
    H = evaluate_expr(Head, Env),
    T = evaluate_expr(Tail, Env),
    [H | T];

evaluate_expr(Other, _Env) ->
    throw({unsupported_expression, Other}).

%% @doc Apply a binary operator
-spec apply_binary_op(atom(), term(), term()) -> term().
apply_binary_op(plus, L, R) -> L + R;
apply_binary_op(minus, L, R) -> L - R;
apply_binary_op(star, L, R) -> L * R;
apply_binary_op(slash, L, R) -> L / R;
apply_binary_op(eq, L, R) -> L == R;
apply_binary_op(neq, L, R) -> L /= R;
apply_binary_op(setoid_eq, L, R) -> L =:= R;
apply_binary_op(setoid_neq, L, R) -> L =/= R;
apply_binary_op(lt, L, R) -> L < R;
apply_binary_op(gt, L, R) -> L > R;
apply_binary_op(lte, L, R) -> L =< R;
apply_binary_op(gte, L, R) -> L >= R;
apply_binary_op('and', L, R) -> L andalso R;
apply_binary_op('or', L, R) -> L orelse R;
apply_binary_op(plus_plus, L, R) when is_list(L), is_list(R) -> L ++ R;
apply_binary_op(Op, _L, _R) -> throw({unsupported_operator, Op}).

%% @doc Apply a function to arguments
-spec apply_function(term(), [term()]) -> term().
apply_function({Fun, _Arity, _Type}, Args) when is_function(Fun) ->
    apply_function(Fun, Args);
apply_function(Fun, []) when is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 0} -> Fun();
        _ -> Fun
    end;
apply_function(Fun, [Arg | Rest]) when is_function(Fun) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 0} ->
            apply_function(Fun(), [Arg | Rest]);
        {arity, 1} ->
            apply_applied_result(Fun(Arg), Rest);
        {arity, N} ->
            Args = [Arg | Rest],
            case length(Args) >= N of
                true ->
                    {CallArgs, Remaining} = lists:split(N, Args),
                    apply_applied_result(erlang:apply(Fun, CallArgs), Remaining);
                false ->
                    make_partial_fun(Fun, Args)
            end
    end;
apply_function(Other, _Args) ->
    throw({not_a_function, Other}).

%% @doc Evaluate a match expression
evaluate_match(_Val, [], _Env) ->
    throw(no_matching_clause);
evaluate_match(Val, [{match_clause, Pattern, _Guard, Body, _Loc} | Rest], Env) ->
    case match_pattern(Pattern, Val, Env) of
        {ok, NewEnv} ->
            evaluate_expr(Body, NewEnv);
        nomatch ->
            evaluate_match(Val, Rest, Env)
    end.

%% @doc Try to match a pattern against a value
match_pattern({pat_wildcard, _Loc}, _Val, Env) ->
    {ok, Env};
match_pattern({pat_var, true, _Loc}, true, Env) ->
    {ok, Env};
match_pattern({pat_var, false, _Loc}, false, Env) ->
    {ok, Env};
match_pattern({pat_var, true, _Loc}, _Val, _Env) ->
    nomatch;
match_pattern({pat_var, false, _Loc}, _Val, _Env) ->
    nomatch;
match_pattern({pat_var, Name, _Loc}, Val, Env) ->
    {ok, maps:put(Name, {value, Val}, Env)};
match_pattern({pat_literal, LitVal, _Type, _Loc}, Val, Env) when LitVal =:= Val ->
    {ok, Env};
match_pattern({pat_literal, _, _, _}, _, _Env) ->
    nomatch;
match_pattern({pat_constructor, 'True', [], _Loc}, true, Env) ->
    {ok, Env};
match_pattern({pat_constructor, 'False', [], _Loc}, false, Env) ->
    {ok, Env};
match_pattern({pat_constructor, 'Some', [SubPat], _Loc}, {some, Val}, Env) ->
    match_pattern(SubPat, Val, Env);
match_pattern({pat_constructor, 'None', [], _Loc}, none, Env) ->
    {ok, Env};
match_pattern({pat_constructor, 'Ok', [SubPat], _Loc}, {ok, Val}, Env) ->
    match_pattern(SubPat, Val, Env);
match_pattern({pat_constructor, 'Err', [SubPat], _Loc}, {err, Val}, Env) ->
    match_pattern(SubPat, Val, Env);
match_pattern({pat_list, [], _Loc}, [], Env) ->
    {ok, Env};
match_pattern({pat_list, [P|Ps], Loc}, [V|Vs], Env) ->
    case match_pattern(P, V, Env) of
        {ok, Env1} -> match_pattern({pat_list, Ps, Loc}, Vs, Env1);
        nomatch -> nomatch
    end;
match_pattern({pat_cons, HeadPat, TailPat, _Loc}, [H|T], Env) ->
    case match_pattern(HeadPat, H, Env) of
        {ok, Env1} -> match_pattern(TailPat, T, Env1);
        nomatch -> nomatch
    end;
match_pattern({pat_tuple, Patterns, _Loc}, Tuple, Env) when is_tuple(Tuple) ->
    Values = tuple_to_list(Tuple),
    case length(Patterns) == length(Values) of
        true -> match_patterns(Patterns, Values, Env);
        false -> nomatch
    end;
match_pattern({pat_record, Fields, _Loc}, Record, Env) when is_map(Record) ->
    match_record_fields(Fields, Record, Env);
match_pattern(_, _, _) ->
    nomatch.

%% @doc Match multiple patterns against multiple values
match_patterns([], [], Env) ->
    {ok, Env};
match_patterns([P|Ps], [V|Vs], Env) ->
    case match_pattern(P, V, Env) of
        {ok, Env1} -> match_patterns(Ps, Vs, Env1);
        nomatch -> nomatch
    end.

%% @doc Match record pattern fields
match_record_fields([], _Record, Env) ->
    {ok, Env};
match_record_fields([{FieldName, Pattern}|Rest], Record, Env) ->
    case maps:get(FieldName, Record, undefined) of
        undefined -> nomatch;
        Value ->
            case match_pattern(Pattern, Value, Env) of
                {ok, Env1} -> match_record_fields(Rest, Record, Env1);
                nomatch -> nomatch
            end
    end.

%% @doc Format a failure reason
-spec format_failure_reason(term()) -> string().
format_failure_reason({expected_true, false}) ->
    "Expected true, got false";
format_failure_reason({assertion_failed, {expected, E, actual, A}}) ->
    io_lib:format("Assertion failed: expected ~p, actual ~p", [E, A]);
format_failure_reason({assertion_failed, Msg}) when is_list(Msg) ->
    io_lib:format("Assertion failed: ~s", [Msg]);
format_failure_reason({assertion_failed, Details}) ->
    io_lib:format("Assertion failed: ~p", [Details]);
format_failure_reason({skipped, Msg}) ->
    io_lib:format("Skipped: ~s", [Msg]);
format_failure_reason({counterexample, Iteration, Values}) ->
    ValueStrs = [io_lib:format("    ~p = ~p", [K, get_value(V)]) || {K, V} <- maps:to_list(Values)],
    io_lib:format("Property failed after ~B iterations.~n  Counterexample:~n~s",
                  [Iteration, string:join(ValueStrs, "\n")]);
format_failure_reason({error, Error, _Stacktrace}) ->
    io_lib:format("Error: ~p", [Error]);
format_failure_reason({exception, Class, Error}) ->
    io_lib:format("Exception ~p: ~p", [Class, Error]);
format_failure_reason({unexpected_result, Result}) ->
    io_lib:format("Unexpected result: ~p (expected true or false)", [Result]);
format_failure_reason(Reason) when is_list(Reason) ->
    Reason;
format_failure_reason(Other) ->
    io_lib:format("~p", [Other]).

%% @doc Extract value from wrapped form
get_value({value, V}) -> V;
get_value(V) -> V.

%%====================================================================
%% Runtime Environment Construction
%%====================================================================

add_runtime_decl_to_env({type_decl, _Name, _Params, Constructors, _Derives, _Loc}, Env) ->
    lists:foldl(fun add_constructor_binding/2, Env, Constructors);
add_runtime_decl_to_env({transform_decl, Name, _TypeSig, Clauses, _Loc}, Env) ->
    maps:put(Name, {make_transform_fun(Clauses, Env), transform_arity(Clauses), runtime}, Env);
add_runtime_decl_to_env(_Decl, Env) ->
    Env.

add_constructor_binding({constructor, Name, ArgTypes, _Loc}, Env) ->
    maps:put(Name, make_constructor_binding(Name, ArgTypes), Env).

make_constructor_binding(Name, []) ->
    runtime_nullary_constructor(Name);
make_constructor_binding(Name, ArgTypes) ->
    {make_constructor_fun(Name, ArgTypes), length(ArgTypes), runtime}.

make_constructor_fun(Name, ArgTypes) ->
    make_curried_constructor(Name, ArgTypes, []).

make_curried_constructor(Name, ArgTypes, BoundArgs) ->
    fun(Arg) ->
        NewArgs = BoundArgs ++ [Arg],
        case length(NewArgs) of
            Arity when Arity =:= length(ArgTypes) ->
                runtime_constructor(Name, NewArgs, ArgTypes);
            _ ->
                make_curried_constructor(Name, ArgTypes, NewArgs)
        end
    end.

runtime_nullary_constructor('Passed') -> passed;
runtime_nullary_constructor('None') -> none;
runtime_nullary_constructor('LT') -> lt;
runtime_nullary_constructor('EQ') -> eq;
runtime_nullary_constructor('GT') -> gt;
runtime_nullary_constructor(Name) -> normalize_constructor_tag(Name).

runtime_constructor(_Name, [RecordValue], [{type_record, _, _, _}]) ->
    RecordValue;
runtime_constructor('Some', [Value], _ArgTypes) ->
    {some, Value};
runtime_constructor('Ok', [Value], _ArgTypes) ->
    {ok, Value};
runtime_constructor('Err', [Value], _ArgTypes) ->
    {err, Value};
runtime_constructor('Left', [Value], _ArgTypes) ->
    {left, Value};
runtime_constructor('Right', [Value], _ArgTypes) ->
    {right, Value};
runtime_constructor('Failed', [Message], _ArgTypes) ->
    {failed, Message};
runtime_constructor('Skipped', [Message], _ArgTypes) ->
    {skipped, Message};
runtime_constructor(Name, [Value], _ArgTypes) ->
    {normalize_constructor_tag(Name), Value};
runtime_constructor(Name, Values, _ArgTypes) ->
    list_to_tuple([normalize_constructor_tag(Name) | Values]).

normalize_constructor_tag(Name) ->
    list_to_atom(string:lowercase(atom_to_list(Name))).

transform_arity([{transform_clause, Patterns, _Guards, _Body, _Loc} | _]) ->
    length(Patterns);
transform_arity([]) ->
    0.

make_transform_fun(Clauses, Env) ->
    Arity = transform_arity(Clauses),
    case Arity of
        0 ->
            fun() -> evaluate_transform_clauses(Clauses, [], Env) end;
        _ ->
            make_curried_transform(Clauses, Env, [], Arity)
    end.

make_curried_transform(Clauses, Env, BoundArgs, Arity) ->
    fun(Arg) ->
        NewArgs = BoundArgs ++ [Arg],
        case length(NewArgs) of
            Arity ->
                evaluate_transform_clauses(Clauses, NewArgs, Env);
            _ ->
                make_curried_transform(Clauses, Env, NewArgs, Arity)
        end
    end.

evaluate_transform_clauses([], Args, _Env) ->
    throw({no_matching_transform_clause, Args});
evaluate_transform_clauses([{transform_clause, Patterns, Guards, Body, _Loc} | Rest], Args, Env) ->
    case length(Patterns) =:= length(Args) of
        false ->
            evaluate_transform_clauses(Rest, Args, Env);
        true ->
            case match_patterns(Patterns, Args, Env) of
                {ok, Env1} ->
                    case guards_match(Guards, Env1) of
                        true -> evaluate_expr(Body, Env1);
                        false -> evaluate_transform_clauses(Rest, Args, Env)
                    end;
                nomatch ->
                    evaluate_transform_clauses(Rest, Args, Env)
            end
    end.

guards_match(undefined, _Env) ->
    true;
guards_match([], _Env) ->
    true;
guards_match(Guards, Env) ->
    lists:all(fun(Guard) -> evaluate_expr(Guard, Env) =:= true end, Guards).

aggregate_results(Results) ->
    {Passed, Failed} = lists:foldl(
        fun({pass, _}, {P, F}) -> {P + 1, F};
           ({fail, _, _}, {P, F}) -> {P, F + 1}
        end,
        {0, 0},
        Results
    ),
    #{
        passed => Passed,
        failed => Failed,
        total => length(Results),
        results => Results
    }.

unit_value() ->
    {unit}.

make_lambda([], Body, Env) ->
    fun() -> evaluate_expr(Body, Env) end;
make_lambda([Pattern | Rest], Body, Env) ->
    fun(Arg) ->
        case match_pattern(Pattern, Arg, Env) of
            {ok, Env1} ->
                case Rest of
                    [] -> evaluate_expr(Body, Env1);
                    _ -> make_lambda(Rest, Body, Env1)
                end;
            nomatch ->
                throw({lambda_pattern_match_failed, Pattern, Arg})
        end
    end.

apply_applied_result(Result, []) ->
    Result;
apply_applied_result(Result, RemainingArgs) ->
    apply_function(Result, RemainingArgs).

make_partial_fun(Fun, BoundArgs) ->
    fun(NextArg) ->
        apply_function(Fun, BoundArgs ++ [NextArg])
    end.
