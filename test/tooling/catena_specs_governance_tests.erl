%% @doc Regression tests for executable specs governance.
-module(catena_specs_governance_tests).

-include_lib("eunit/include/eunit.hrl").

live_repository_passes_governance_test() ->
    {ok, Report} = catena_specs_governance:validate(repo_root()),
    ?assertEqual(35, maps:get(requirements, Report)),
    ?assertEqual(5, maps:get(requirement_families, Report)),
    ?assertEqual(10, maps:get(scenarios, Report)),
    ?assertEqual(19, maps:get(evidence_rows, Report)),
    ?assertEqual(19, maps:get(evidence_modules, Report)),
    ?assertEqual(10, maps:get(component_specs, Report)),
    ?assertEqual(4, maps:get(adrs, Report)),
    ?assert(maps:get(acceptance_criteria, Report) > 50),
    ?assert(maps:get(local_links, Report) > 50).

valid_minimal_fixture_passes_test() ->
    with_fixture(fun(Root) ->
        ?assertMatch({ok, #{
            requirements := 1,
            requirement_families := 1,
            scenarios := 1,
            evidence_rows := 1,
            evidence_modules := 1,
            acceptance_criteria := 1,
            component_specs := 1,
            adrs := 1
        }}, catena_specs_governance:validate(Root))
    end).

missing_evidence_source_is_reported_test() ->
    with_fixture(fun(Root) ->
        write_fixture_file(
            Root,
            "specs/conformance/executable_scenarios.tsv",
            "scenario_id\ttest_module\tsource_path\n"
            "SCN-001\texample_tests\ttest/missing_tests.erl\n"
        ),
        {error, Errors} = catena_specs_governance:validate(Root),
        ?assert(lists:member(
            {missing_evidence_source, "SCN-001", "test/missing_tests.erl"},
            Errors
        ))
    end).

unknown_scenario_reference_is_reported_test() ->
    with_fixture(fun(Root) ->
        write_fixture_file(
            Root,
            "specs/conformance/spec_conformance_matrix.md",
            "| `REQ-COMP-*` | `specs/contracts/compiler_contract.md` | `SCN-001`, `SCN-999` |\n"
            "| `specs/compiler/example.md` | `REQ-COMP-*` | `SCN-001` |\n"
        ),
        {error, Errors} = catena_specs_governance:validate(Root),
        ?assert(lists:member({unknown_matrix_scenario, "SCN-999"}, Errors))
    end).

broken_local_markdown_link_is_reported_test() ->
    with_fixture(fun(Root) ->
        write_fixture_file(
            Root,
            "specs/README.md",
            "# Specs\n\n[missing](compiler/missing.md)\n"
        ),
        {error, Errors} = catena_specs_governance:validate(Root),
        ?assert(lists:member(
            {broken_local_link, "specs/README.md", "compiler/missing.md"},
            Errors
        ))
    end).

unmapped_component_acceptance_criteria_are_reported_test() ->
    with_fixture(fun(Root) ->
        write_fixture_file(
            Root,
            "specs/runtime/unmapped.md",
            "# Unmapped\n\n### AC-UNMAPPED-001 Example\n"
        ),
        {error, Errors} = catena_specs_governance:validate(Root),
        ?assert(lists:member(
            {component_missing_from_matrix, "specs/runtime/unmapped.md"},
            Errors
        ))
    end).

uncataloged_adr_is_reported_test() ->
    with_fixture(fun(Root) ->
        write_fixture_file(
            Root,
            "specs/adr/ADR-0002-uncataloged.md",
            "# ADR-0002\n"
        ),
        {error, Errors} = catena_specs_governance:validate(Root),
        ?assert(lists:member(
            {adr_missing_from_catalog, "specs/adr/ADR-0002-uncataloged.md"},
            Errors
        ))
    end).

missing_requirement_family_catalog_entry_is_reported_test() ->
    with_fixture(fun(Root) ->
        write_fixture_file(
            Root,
            "specs/contracts/requirements_catalog.md",
            "# Requirements\n\n`SCN-001`\n"
        ),
        {error, Errors} = catena_specs_governance:validate(Root),
        ?assert(lists:member(
            {missing, requirement_catalog, "REQ-COMP-*"},
            Errors
        ))
    end).

formatters_produce_actionable_output_test() ->
    {ok, Report} = catena_specs_governance:validate(repo_root()),
    Success = catena_specs_governance:format_report(Report),
    Failure = catena_specs_governance:format_errors([{broken_local_link, "a.md", "b.md"}]),
    ?assert(string:find(Success, "Specs governance passed") =/= nomatch),
    ?assert(string:find(Success, "scenarios") =/= nomatch),
    ?assert(string:find(Failure, "Specs governance failed") =/= nomatch),
    ?assert(string:find(Failure, "broken_local_link") =/= nomatch).

repo_root() ->
    {ok, Cwd} = file:get_cwd(),
    Cwd.

with_fixture(TestFun) ->
    Root = filename:join(
        "/tmp",
        "catena_specs_governance_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Root),
    try
        create_valid_fixture(Root),
        TestFun(Root)
    after
        ok = file:del_dir_r(Root)
    end.

create_valid_fixture(Root) ->
    write_fixture_file(
        Root,
        "specs/contracts/compiler_contract.md",
        "# Compiler Contract\n\n- `REQ-COMP-001`: Example requirement.\n"
    ),
    write_fixture_file(
        Root,
        "specs/contracts/requirements_catalog.md",
        "# Requirements\n\n"
        "| Family | Contract | Scenario |\n"
        "| --- | --- | --- |\n"
        "| `REQ-COMP-*` | [compiler](compiler_contract.md) | `SCN-001` |\n"
    ),
    write_fixture_file(
        Root,
        "specs/conformance/scenario_catalog.md",
        "# Scenarios\n\n"
        "| ID | Name | Description |\n"
        "| --- | --- | --- |\n"
        "| `SCN-001` | Example | Example scenario. |\n"
    ),
    write_fixture_file(
        Root,
        "specs/conformance/spec_conformance_matrix.md",
        "# Matrix\n\n"
        "| `REQ-COMP-*` | `specs/contracts/compiler_contract.md` | `SCN-001` |\n"
        "| `specs/compiler/example.md` | `REQ-COMP-*` | `SCN-001` |\n"
    ),
    write_fixture_file(
        Root,
        "specs/conformance/executable_scenarios.tsv",
        "scenario_id\ttest_module\tsource_path\n"
        "SCN-001\texample_tests\ttest/example_tests.erl\n"
    ),
    write_fixture_file(
        Root,
        "specs/compiler/example.md",
        "# Example\n\n### AC-EXAMPLE-001 Example criterion\n"
    ),
    write_fixture_file(
        Root,
        "specs/adr/ADR-0001-example.md",
        "# ADR-0001\n"
    ),
    write_fixture_file(
        Root,
        "specs/adr/adr_catalog.md",
        "# ADR Catalog\n\n[ADR-0001](ADR-0001-example.md)\n"
    ),
    write_fixture_file(
        Root,
        "specs/README.md",
        "# Specs\n\n[component](compiler/example.md)\n"
    ),
    write_fixture_file(
        Root,
        "test/example_tests.erl",
        "-module(example_tests).\n"
    ).

write_fixture_file(Root, RelativePath, Content) ->
    Path = filename:join(Root, RelativePath),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, Content).
