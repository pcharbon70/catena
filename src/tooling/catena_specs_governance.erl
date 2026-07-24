%% @doc Executable governance validation for Catena's promoted specs tree.
-module(catena_specs_governance).

-export([
    validate/0,
    validate/1,
    format_report/1,
    format_errors/1
]).

-type validation_error() :: term().
-type validation_report() :: #{
    requirements := non_neg_integer(),
    requirement_families := non_neg_integer(),
    scenarios := non_neg_integer(),
    evidence_rows := non_neg_integer(),
    evidence_modules := non_neg_integer(),
    acceptance_criteria := non_neg_integer(),
    component_specs := non_neg_integer(),
    adrs := non_neg_integer(),
    local_links := non_neg_integer()
}.

-export_type([validation_error/0, validation_report/0]).

%%====================================================================
%% Public API
%%====================================================================

-spec validate() -> {ok, validation_report()} | {error, [validation_error()]}.
validate() ->
    validate(".").

-spec validate(file:filename_all()) ->
    {ok, validation_report()} | {error, [validation_error()]}.
validate(RootValue) ->
    Root = filename:absname(RootValue),
    {Requirements, RequirementFamilies, RequirementErrors} =
        validate_requirements(Root),
    {Scenarios, EvidenceRows, EvidenceModules, ScenarioErrors} =
        validate_scenarios(Root),
    {AcceptanceCriteria, ComponentSpecs, ComponentErrors} =
        validate_components(Root),
    {Adrs, AdrErrors} = validate_adrs(Root),
    {LocalLinks, LinkErrors} = validate_local_links(Root),
    MatrixPathErrors = validate_matrix_paths(Root),
    Errors = lists:sort(
        RequirementErrors ++
        ScenarioErrors ++
        ComponentErrors ++
        AdrErrors ++
        LinkErrors ++
        MatrixPathErrors
    ),
    case Errors of
        [] ->
            {ok, #{
                requirements => Requirements,
                requirement_families => RequirementFamilies,
                scenarios => Scenarios,
                evidence_rows => EvidenceRows,
                evidence_modules => EvidenceModules,
                acceptance_criteria => AcceptanceCriteria,
                component_specs => ComponentSpecs,
                adrs => Adrs,
                local_links => LocalLinks
            }};
        _ ->
            {error, Errors}
    end.

-spec format_report(validation_report()) -> string().
format_report(Report) ->
    lists:flatten(io_lib:format(
        "Specs governance passed: ~B requirements in ~B families, "
        "~B scenarios, ~B evidence rows across ~B modules, "
        "~B acceptance criteria across ~B component specs, "
        "~B ADRs, and ~B local Markdown links.",
        [
            maps:get(requirements, Report),
            maps:get(requirement_families, Report),
            maps:get(scenarios, Report),
            maps:get(evidence_rows, Report),
            maps:get(evidence_modules, Report),
            maps:get(acceptance_criteria, Report),
            maps:get(component_specs, Report),
            maps:get(adrs, Report),
            maps:get(local_links, Report)
        ]
    )).

-spec format_errors([validation_error()]) -> string().
format_errors(Errors) ->
    lists:flatten([
        "Specs governance failed:\n",
        [io_lib:format("  - ~p~n", [Error]) || Error <- Errors]
    ]).

%%====================================================================
%% Requirement Validation
%%====================================================================

validate_requirements(Root) ->
    ContractPattern = root_path(Root, "specs/contracts/*_contract.md"),
    ContractFiles = lists:sort(filelib:wildcard(ContractPattern)),
    {RequirementIds, ContractReadErrors} = lists:foldl(
        fun(File, {IdsAcc, ErrorsAcc}) ->
            case read_file(File, Root) of
                {ok, Content} ->
                    Ids = captures(Content, <<"`(REQ-[A-Z]+-[0-9]{3})`:">>),
                    EmptyErrors = case Ids of
                        [] -> [{contract_without_requirements, relative_path(File, Root)}];
                        _ -> []
                    end,
                    {IdsAcc ++ Ids, ErrorsAcc ++ EmptyErrors};
                {error, Error} ->
                    {IdsAcc, ErrorsAcc ++ [Error]}
            end
        end,
        {[], []},
        ContractFiles
    ),
    DuplicateErrors = [
        {duplicate_requirement, binary_to_list(Id)}
     || Id <- duplicates(RequirementIds)
    ],
    Families = unique([requirement_family(Id) || Id <- RequirementIds]),
    CatalogPath = root_path(Root, "specs/contracts/requirements_catalog.md"),
    MatrixPath = root_path(Root, "specs/conformance/spec_conformance_matrix.md"),
    {CatalogFamilies, CatalogErrors} = families_from_file(CatalogPath, Root),
    {MatrixFamilies, MatrixErrors} = families_from_file(MatrixPath, Root),
    FamilyErrors =
        set_alignment_errors(requirement_catalog, Families, CatalogFamilies) ++
        set_alignment_errors(requirement_matrix, Families, MatrixFamilies),
    {
        length(unique(RequirementIds)),
        length(Families),
        ContractReadErrors ++ DuplicateErrors ++ CatalogErrors ++ MatrixErrors ++ FamilyErrors
    }.

families_from_file(Path, Root) ->
    case read_file(Path, Root) of
        {ok, Content} ->
            {unique(captures(Content, <<"`(REQ-[A-Z]+-\\*)`">>)), []};
        {error, Error} ->
            {[], [Error]}
    end.

requirement_family(Id) ->
    re:replace(Id, <<"-[0-9]{3}$">>, <<"-*">>, [{return, binary}]).

%%====================================================================
%% Scenario And Evidence Validation
%%====================================================================

validate_scenarios(Root) ->
    CatalogPath = root_path(Root, "specs/conformance/scenario_catalog.md"),
    MatrixPath = root_path(Root, "specs/conformance/spec_conformance_matrix.md"),
    RequirementsPath = root_path(Root, "specs/contracts/requirements_catalog.md"),
    ManifestPath = root_path(Root, "specs/conformance/executable_scenarios.tsv"),
    {ScenarioIds, CatalogErrors} = scenario_definitions(CatalogPath, Root),
    {MatrixRefs, MatrixErrors} = scenario_references(MatrixPath, Root),
    {RequirementRefs, RequirementErrors} = scenario_references(RequirementsPath, Root),
    {EvidenceRows, ManifestErrors} = parse_manifest(ManifestPath, Root),
    EvidenceIds = unique([maps:get(scenario, Row) || Row <- EvidenceRows]),
    ReferenceErrors =
        unknown_values(unknown_matrix_scenario, MatrixRefs, ScenarioIds) ++
        unknown_values(unknown_requirement_scenario, RequirementRefs, ScenarioIds) ++
        missing_values(scenario_without_matrix_reference, ScenarioIds, MatrixRefs) ++
        unknown_values(unknown_evidence_scenario, EvidenceIds, ScenarioIds) ++
        missing_values(scenario_without_evidence, ScenarioIds, EvidenceIds),
    EvidenceErrors = lists:append([
        validate_evidence_row(Row, Root)
     || Row <- EvidenceRows
    ]),
    DuplicateRows = duplicates([
        {
            maps:get(scenario, Row),
            maps:get(module, Row),
            maps:get(source, Row)
        }
     || Row <- EvidenceRows
    ]),
    DuplicateErrors = [
        {duplicate_evidence_row, row_error_value(Row)}
     || Row <- DuplicateRows
    ],
    {
        length(unique(ScenarioIds)),
        length(EvidenceRows),
        length(unique([maps:get(module, Row) || Row <- EvidenceRows])),
        CatalogErrors ++
        MatrixErrors ++
        RequirementErrors ++
        ManifestErrors ++
        ReferenceErrors ++
        EvidenceErrors ++
        DuplicateErrors
    }.

scenario_definitions(Path, Root) ->
    case read_file(Path, Root) of
        {ok, Content} ->
            Ids = captures(Content, <<"`(SCN-[0-9]{3})`\\s*\\|">>),
            Errors = [
                {duplicate_scenario, binary_to_list(Id)}
             || Id <- duplicates(Ids)
            ],
            {unique(Ids), Errors};
        {error, Error} ->
            {[], [Error]}
    end.

scenario_references(Path, Root) ->
    case read_file(Path, Root) of
        {ok, Content} ->
            {unique(captures(Content, <<"`(SCN-[0-9]{3})`">>)), []};
        {error, Error} ->
            {[], [Error]}
    end.

parse_manifest(Path, Root) ->
    case read_file(Path, Root) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global]),
            parse_manifest_lines(Lines, 1, false, [], []);
        {error, Error} ->
            {[], [Error]}
    end.

parse_manifest_lines([], _LineNumber, HeaderSeen, Rows, Errors) ->
    HeaderErrors = case HeaderSeen of
        true -> [];
        false -> [missing_evidence_header]
    end,
    {lists:reverse(Rows), lists:reverse(Errors) ++ HeaderErrors};
parse_manifest_lines([RawLine | Rest], LineNumber, HeaderSeen, Rows, Errors) ->
    Line = string:trim(RawLine),
    case {LineNumber, Line} of
        {1, <<"scenario_id\ttest_module\tsource_path">>} ->
            parse_manifest_lines(Rest, LineNumber + 1, true, Rows, Errors);
        {1, _} ->
            parse_manifest_lines(
                Rest,
                LineNumber + 1,
                false,
                Rows,
                [{invalid_evidence_header, binary_to_list(Line)} | Errors]
            );
        {_, <<>>} ->
            parse_manifest_lines(Rest, LineNumber + 1, HeaderSeen, Rows, Errors);
        _ ->
            case binary:split(Line, <<"\t">>, [global]) of
                [Scenario, Module, Source] ->
                    Row = #{
                        scenario => Scenario,
                        module => Module,
                        source => Source,
                        line => LineNumber
                    },
                    RowErrors = manifest_field_errors(Row),
                    parse_manifest_lines(
                        Rest,
                        LineNumber + 1,
                        HeaderSeen,
                        [Row | Rows],
                        lists:reverse(RowErrors) ++ Errors
                    );
                _ ->
                    parse_manifest_lines(
                        Rest,
                        LineNumber + 1,
                        HeaderSeen,
                        Rows,
                        [{invalid_evidence_row, LineNumber, binary_to_list(Line)} | Errors]
                    )
            end
    end.

manifest_field_errors(Row) ->
    Scenario = maps:get(scenario, Row),
    Module = maps:get(module, Row),
    Source = maps:get(source, Row),
    Line = maps:get(line, Row),
    valid_pattern_error(
        Scenario,
        <<"^SCN-[0-9]{3}$">>,
        {invalid_evidence_scenario, Line, binary_to_list(Scenario)}
    ) ++
    valid_pattern_error(
        Module,
        <<"^[a-z][a-z0-9_]*$">>,
        {invalid_evidence_module, Line, binary_to_list(Module)}
    ) ++
    case Source of
        <<>> -> [{invalid_evidence_source, Line, ""}];
        _ -> []
    end.

validate_evidence_row(Row, Root) ->
    Scenario = binary_to_list(maps:get(scenario, Row)),
    Module = binary_to_list(maps:get(module, Row)),
    Source = binary_to_list(maps:get(source, Row)),
    AbsoluteSource = filename:absname(filename:join(Root, Source)),
    RootPrefix = Root ++ "/",
    case lists:prefix(RootPrefix, AbsoluteSource) of
        false ->
            [{evidence_source_outside_root, Scenario, Source}];
        true ->
            case file:read_file(AbsoluteSource) of
                {ok, Content} ->
                    Expected = iolist_to_binary(["-module(", Module, ")."]),
                    case binary:match(Content, Expected) of
                        nomatch ->
                            [{evidence_module_mismatch, Scenario, Module, Source}];
                        _ ->
                            []
                    end;
                {error, enoent} ->
                    [{missing_evidence_source, Scenario, Source}];
                {error, Reason} ->
                    [{unreadable_evidence_source, Scenario, Source, Reason}]
            end
    end.

row_error_value({Scenario, Module, Source}) ->
    {
        binary_to_list(Scenario),
        binary_to_list(Module),
        binary_to_list(Source)
    }.

%%====================================================================
%% Component And ADR Validation
%%====================================================================

validate_components(Root) ->
    MatrixPath = root_path(Root, "specs/conformance/spec_conformance_matrix.md"),
    {Matrix, MatrixErrors} = case read_file(MatrixPath, Root) of
        {ok, MatrixContent} -> {MatrixContent, []};
        {error, MatrixError} -> {<<>>, [MatrixError]}
    end,
    ComponentFiles = lists:sort(lists:append([
        filelib:wildcard(root_path(Root, "specs/" ++ Dir ++ "/*.md"))
     || Dir <- ["compiler", "runtime", "stdlib", "tooling"]
    ])),
    {AcEntries, ReadErrors} = lists:foldl(
        fun(File, {EntriesAcc, ErrorsAcc}) ->
            case read_file(File, Root) of
                {ok, FileContent} ->
                    Ids = captures(
                        FileContent,
                        <<"(?m)^### (AC-[A-Z]+-[0-9]{3})\\b">>
                    ),
                    Entry = #{
                        path => list_to_binary(relative_path(File, Root)),
                        ids => Ids
                    },
                    {[Entry | EntriesAcc], ErrorsAcc};
                {error, ReadError} ->
                    {EntriesAcc, [ReadError | ErrorsAcc]}
            end
        end,
        {[], []},
        ComponentFiles
    ),
    MappedEntries = [Entry || #{ids := Ids} = Entry <- AcEntries, Ids =/= []],
    AcIds = lists:append([maps:get(ids, Entry) || Entry <- MappedEntries]),
    DuplicateErrors = [
        {duplicate_acceptance_criterion, binary_to_list(Id)}
     || Id <- duplicates(AcIds)
    ],
    MappingErrors = lists:append([
        case binary:match(
            Matrix,
            iolist_to_binary(["`", maps:get(path, Entry), "`"])
        ) of
            nomatch ->
                [{component_missing_from_matrix, binary_to_list(maps:get(path, Entry))}];
            _ ->
                []
        end
     || Entry <- MappedEntries
    ]),
    {
        length(unique(AcIds)),
        length(MappedEntries),
        MatrixErrors ++ ReadErrors ++ DuplicateErrors ++ MappingErrors
    }.

validate_adrs(Root) ->
    AdrFiles = lists:sort(filelib:wildcard(root_path(Root, "specs/adr/ADR-*.md"))),
    CatalogPath = root_path(Root, "specs/adr/adr_catalog.md"),
    case read_file(CatalogPath, Root) of
        {ok, Catalog} ->
            Errors = lists:append([
                case binary:match(Catalog, list_to_binary(filename:basename(File))) of
                    nomatch ->
                        [{adr_missing_from_catalog, relative_path(File, Root)}];
                    _ ->
                        []
                end
             || File <- AdrFiles
            ]),
            {length(AdrFiles), Errors};
        {error, Error} ->
            {length(AdrFiles), [Error]}
    end.

validate_matrix_paths(Root) ->
    MatrixPath = root_path(Root, "specs/conformance/spec_conformance_matrix.md"),
    case read_file(MatrixPath, Root) of
        {ok, Content} ->
            Paths = unique(captures(Content, <<"`(specs/[^`]+\\.md)`">>)),
            [
                {missing_matrix_path, binary_to_list(Path)}
             || Path <- Paths,
                not filelib:is_regular(root_path(Root, binary_to_list(Path)))
            ];
        {error, Error} ->
            [Error]
    end.

%%====================================================================
%% Local Markdown Link Validation
%%====================================================================

validate_local_links(Root) ->
    MarkdownFiles = filelib:fold_files(
        root_path(Root, "specs"),
        ".*\\.md$",
        true,
        fun(File, Acc) -> [File | Acc] end,
        []
    ),
    lists:foldl(
        fun(File, {CountAcc, ErrorsAcc}) ->
            case read_file(File, Root) of
                {ok, Content} ->
                    Targets = captures(
                        Content,
                        <<"\\]\\(([^\\s)#]+\\.md)(?:#[^)]*)?\\)">>
                    ),
                    Errors = [
                        {broken_local_link,
                            relative_path(File, Root),
                            binary_to_list(Target)}
                     || Target <- Targets,
                        is_local_target(Target),
                        not filelib:is_regular(
                            filename:absname(
                                filename:join(
                                    filename:dirname(File),
                                    binary_to_list(Target)
                                )
                            )
                        )
                    ],
                    {
                        CountAcc + length([T || T <- Targets, is_local_target(T)]),
                        ErrorsAcc ++ Errors
                    };
                {error, Error} ->
                    {CountAcc, ErrorsAcc ++ [Error]}
            end
        end,
        {0, []},
        MarkdownFiles
    ).

is_local_target(<<"http://", _/binary>>) -> false;
is_local_target(<<"https://", _/binary>>) -> false;
is_local_target(<<"mailto:", _/binary>>) -> false;
is_local_target(_) -> true.

%%====================================================================
%% Helpers
%%====================================================================

read_file(Path, Root) ->
    case file:read_file(Path) of
        {ok, Content} ->
            {ok, Content};
        {error, Reason} ->
            {error, {unreadable_governance_file, relative_path(Path, Root), Reason}}
    end.

captures(Content, Pattern) ->
    case re:run(Content, Pattern, [global, {capture, [1], binary}]) of
        {match, Matches} -> [Value || [Value] <- Matches];
        nomatch -> []
    end.

valid_pattern_error(Value, Pattern, Error) ->
    case re:run(Value, Pattern, [{capture, none}]) of
        match -> [];
        nomatch -> [Error]
    end.

set_alignment_errors(Context, Expected, Actual) ->
    missing_values({missing, Context}, Expected, Actual) ++
    unknown_values({unknown, Context}, Actual, Expected).

missing_values(Tag, Expected, Actual) ->
    tagged_value_errors(Tag, [Value || Value <- unique(Expected), not lists:member(Value, Actual)]).

unknown_values(Tag, Actual, Expected) ->
    tagged_value_errors(Tag, [Value || Value <- unique(Actual), not lists:member(Value, Expected)]).

tagged_value_errors(Tag, Values) when is_atom(Tag) ->
    [{Tag, binary_to_list(Value)} || Value <- Values];
tagged_value_errors({Kind, Context}, Values) ->
    [{Kind, Context, binary_to_list(Value)} || Value <- Values].

duplicates(Values) ->
    [
        Value
     || Value <- unique(Values),
        length([Item || Item <- Values, Item =:= Value]) > 1
    ].

unique(Values) ->
    lists:usort(Values).

root_path(Root, Relative) ->
    filename:join(Root, Relative).

relative_path(Path, Root) ->
    case lists:prefix(Root ++ "/", Path) of
        true -> lists:nthtail(length(Root) + 1, Path);
        false -> Path
    end.
