%% @doc Record definitions for catena_property module

%% A property encapsulates a generator and predicate for universal quantification.
-record(property, {
    name,
    generator,
    predicate,
    config
}).

%% Configuration for property test execution.
-record(property_config, {
    num_tests,
    max_shrinks,
    seed,
    labels,
    classify_fun
}).

%% Property test result with full context.
-record(property_result, {
    kind,
    tests_run,
    tests_discarded,
    shrinks_attempted,
    seed,
    original_counterexample,
    shrunk_counterexample,
    shrink_history,
    labels,
    output
}).
