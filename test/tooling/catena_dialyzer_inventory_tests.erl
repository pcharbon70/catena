%% @doc Regression tests for deterministic Dialyzer warning inventories.
-module(catena_dialyzer_inventory_tests).

-include_lib("eunit/include/eunit.hrl").

multiline_and_line_zero_records_are_parsed_test() ->
    {ok, Warnings} = catena_dialyzer_inventory:parse(fixture(), "/workspace/catena"),
    ?assertEqual(8, length(Warnings)),
    Unknown = warning_at("src/proptest/catena_laws.erl", Warnings),
    ?assertEqual(0, maps:get(line, Unknown)),
    ?assertEqual(undefined, maps:get(column, Unknown)),
    ?assertEqual("Unknown function eunit:test/1", maps:get(headline, Unknown)),
    ?assertEqual(missing_metadata, maps:get(family, Unknown)),
    Spec = warning_at("src/compiler/types/catena_types.erl", Warnings),
    ?assertEqual(#{line => 41, column => 2}, maps:with([line, column], Spec)),
    ?assert(string:find(maps:get(diagnostic, Spec), "success typing") =/= nomatch).

paths_are_workspace_independent_test() ->
    Artifact =
        "C:\\agent\\catena\\src\\runtime\\catena_runtime.erl:12:3: "
        "Function run/0 has no local return\n",
    {ok, [Warning]} = catena_dialyzer_inventory:parse(Artifact, "/different/root"),
    ?assertEqual("src/runtime/catena_runtime.erl", maps:get(path, Warning)),
    ?assertEqual("src/runtime", maps:get(directory, Warning)),
    ?assertEqual("catena_runtime", maps:get(module, Warning)),
    ?assertEqual(runtime, maps:get(ownership, Warning)).

relative_external_and_root_paths_are_normalized_test() ->
    RelativeArtifact =
        "./src/runtime/catena_relative.erl:1:1: Function run/0 has no local return\n",
    {ok, [Relative]} = catena_dialyzer_inventory:parse(
        RelativeArtifact,
        "/different/root"
    ),
    ?assertEqual("src/runtime/catena_relative.erl", maps:get(path, Relative)),
    ExternalArtifact =
        "/opt/vendor/external.erl:2:1: Opaque diagnostic family retained for audit\n",
    {ok, [External]} = catena_dialyzer_inventory:parse(
        ExternalArtifact,
        "/different/root"
    ),
    ?assertEqual("/opt/vendor/external.erl", maps:get(path, External)),
    ?assertEqual(other, maps:get(ownership, External)),
    RootArtifact =
        "/src/tooling/catena_root.erl:3:1: Function run/0 has no local return\n",
    {ok, [RootWarning]} = catena_dialyzer_inventory:parse(RootArtifact, "/"),
    ?assertEqual("src/tooling/catena_root.erl", maps:get(path, RootWarning)).

warning_families_are_classified_test() ->
    {ok, Warnings} = catena_dialyzer_inventory:parse(fixture(), "/workspace/catena"),
    Families = lists:sort([maps:get(family, Warning) || Warning <- Warnings]),
    ?assertEqual(lists:sort([
        call_contract_mismatch,
        ignored_return,
        missing_metadata,
        no_return_control_flow,
        record_field_mismatch,
        type_specification_contract,
        unreachable_pattern_variable_guard,
        other
    ]), Families).

sorting_and_summaries_are_deterministic_test() ->
    {ok, Forward} = catena_dialyzer_inventory:parse(fixture(), "/workspace/catena"),
    ReversedArtifact = join_records(lists:reverse(records(fixture()))),
    {ok, Reverse} = catena_dialyzer_inventory:parse(ReversedArtifact, "/workspace/catena"),
    ?assertEqual(Forward, Reverse),
    Report = catena_dialyzer_inventory:summarize(Reverse),
    ?assertEqual(8, maps:get(total, Report)),
    ?assertEqual(8, maps:get(module_count, Report)),
    ?assertEqual([{compiler_types, 2}, {property_testing, 1}, {runtime, 1},
        {testing_compatibility_bridges, 1}, {tooling, 3}],
        maps:get(ownership_areas, Report)),
    Output = catena_dialyzer_inventory:format_report(Report),
    ?assert(string:find(Output, "Total warnings: 8") =/= nomatch),
    ?assert(string:find(Output, "missing_metadata: 1") =/= nomatch).

malformed_records_are_rejected_test() ->
    ?assertEqual(
        {error, [{invalid_warning_record, 1, "not a Dialyzer warning"}]},
        catena_dialyzer_inventory:parse("not a Dialyzer warning", "/workspace/catena")
    ).

default_root_and_file_apis_are_covered_test() ->
    {ok, Parsed} = catena_dialyzer_inventory:parse(fixture()),
    ?assertEqual(8, length(Parsed)),
    Path = filename:join(
        "/tmp",
        "catena_dialyzer_inventory_" ++
            integer_to_list(erlang:unique_integer([positive])) ++ ".warnings"
    ),
    ok = file:write_file(Path, fixture()),
    try
        {ok, Read} = catena_dialyzer_inventory:read(Path),
        ?assertEqual(Parsed, Read)
    after
        ok = file:delete(Path)
    end,
    ?assertMatch(
        {error, {read_failed, Path, enoent}},
        catena_dialyzer_inventory:read(Path)
    ).

warning_at(Path, Warnings) ->
    hd([Warning || Warning <- Warnings, maps:get(path, Warning) =:= Path]).

fixture() ->
    join_records([
        "/workspace/catena/src/tooling/catena_tool.erl:7:1: "
        "The call catena_runner:run([]) does not have a local return",
        "/workspace/catena/src/compiler/types/catena_types.erl:41:2: "
        "Type specification catena_types:new(term()) -> term() is a supertype\n"
        "          of the success typing catena_types:new(atom()) -> atom()",
        "/workspace/catena/src/proptest/catena_laws.erl:0: Unknown function eunit:test/1",
        "/workspace/catena/src/runtime/catena_runtime.erl:12:3: "
        "Function run/0 has no local return",
        "/workspace/catena/src/compiler/types/catena_state.erl:20:5: "
        "Record construction #state{value = undefined} violates the declared type of field value",
        "/workspace/catena/src/testing/catena_bridge.erl:50:1: "
        "Function translate/1 will never be called",
        "/workspace/catena/src/tooling/catena_report.erl:60:9: "
        "Expression produces a value of type ok, but this value is unmatched",
        "/workspace/catena/src/tooling/catena_misc.erl:70:1: "
        "Opaque diagnostic family retained for audit"
    ]).

records(Artifact) ->
    [binary_to_list(Block) || Block <- binary:split(
        iolist_to_binary(Artifact), <<"\n\n">>, [global]
    ), Block =/= <<>>].

join_records(Records) ->
    lists:join("\n\n", Records) ++ "\n".
