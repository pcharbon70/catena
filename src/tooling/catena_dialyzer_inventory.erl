%% @doc Deterministic classification of formatted rebar3 Dialyzer artifacts.
-module(catena_dialyzer_inventory).

-export([
    parse/1,
    parse/2,
    read/1,
    read/2,
    summarize/1,
    format_report/1
]).

-type warning_family() ::
    call_contract_mismatch |
    ignored_return |
    missing_metadata |
    no_return_control_flow |
    other |
    record_field_mismatch |
    type_specification_contract |
    unreachable_pattern_variable_guard.
-type ownership_area() ::
    compiler_ast |
    compiler_codegen |
    compiler_effects |
    compiler_error |
    compiler_parser |
    compiler_root |
    compiler_runtime |
    compiler_semantic_analysis |
    compiler_types |
    compiler_validation |
    other |
    property_testing |
    repl |
    runtime |
    standard_library |
    testing_compatibility_bridges |
    tests |
    tooling.
-type warning() :: #{
    column := non_neg_integer() | undefined,
    diagnostic := string(),
    directory := string(),
    family := warning_family(),
    headline := string(),
    line := non_neg_integer(),
    module := string(),
    ownership := ownership_area(),
    path := string()
}.
-type count_key() :: atom() | string().
-type count_rows() :: [{count_key(), pos_integer()}].
-type report() :: #{
    directories := count_rows(),
    families := count_rows(),
    module_count := non_neg_integer(),
    modules := count_rows(),
    ownership_areas := count_rows(),
    total := non_neg_integer(),
    warnings := [warning()]
}.
-type parse_error() :: {invalid_warning_record, pos_integer(), string()}.

-export_type([
    ownership_area/0,
    parse_error/0,
    report/0,
    warning/0,
    warning_family/0
]).

%%====================================================================
%% Public API
%%====================================================================

-spec parse(iodata()) -> {ok, [warning()]} | {error, [parse_error(), ...]}.
parse(Content) ->
    parse(Content, ".").

-spec parse(iodata(), file:filename_all()) ->
    {ok, [warning()]} | {error, [parse_error(), ...]}.
parse(Content, RootValue) ->
    Root = normalize_separators(filename:absname(RootValue)),
    Binary = normalize_newlines(iolist_to_binary(Content)),
    Blocks = nonempty_blocks(binary:split(Binary, <<"\n\n">>, [global])),
    {Warnings, Errors} = parse_blocks(Blocks, Root, 1, [], []),
    case Errors of
        [] -> {ok, sort_warnings(Warnings)};
        _ -> {error, lists:reverse(Errors)}
    end.

-spec read(file:filename_all()) -> {ok, [warning()]} | {error, term()}.
read(Path) ->
    read(Path, ".").

-spec read(file:filename_all(), file:filename_all()) ->
    {ok, [warning()]} | {error, term()}.
read(Path, Root) ->
    case file:read_file(Path) of
        {ok, Content} -> parse(Content, Root);
        {error, Reason} -> {error, {read_failed, Path, Reason}}
    end.

-spec summarize([warning()]) -> report().
summarize(Warnings) ->
    SortedWarnings = sort_warnings(Warnings),
    Modules = count_by(module, SortedWarnings),
    #{
        total => length(SortedWarnings),
        module_count => length(Modules),
        families => count_by(family, SortedWarnings),
        ownership_areas => count_by(ownership, SortedWarnings),
        directories => count_by(directory, SortedWarnings),
        modules => Modules,
        warnings => SortedWarnings
    }.

-spec format_report(report()) -> string().
format_report(Report) ->
    lists:flatten([
        "Dialyzer warning inventory\n",
        io_lib:format("Total warnings: ~B\n", [maps:get(total, Report)]),
        io_lib:format("Affected modules: ~B\n", [maps:get(module_count, Report)]),
        format_counts("Warning families", maps:get(families, Report)),
        format_counts("Ownership areas", maps:get(ownership_areas, Report)),
        format_counts("Directories", maps:get(directories, Report)),
        format_counts("Modules", maps:get(modules, Report))
    ]).

%%====================================================================
%% Artifact Parsing
%%====================================================================

normalize_newlines(Binary) ->
    binary:replace(Binary, <<"\r\n">>, <<"\n">>, [global]).

nonempty_blocks(Blocks) ->
    [string:trim(Block) || Block <- Blocks, string:trim(Block) =/= <<>>].

parse_blocks([], _Root, _Index, Warnings, Errors) ->
    {Warnings, Errors};
parse_blocks([Block | Rest], Root, Index, Warnings, Errors) ->
    case parse_block(Block, Root) of
        {ok, Warning} ->
            parse_blocks(Rest, Root, Index + 1, [Warning | Warnings], Errors);
        error ->
            Error = {invalid_warning_record, Index, leading_text(Block)},
            parse_blocks(Rest, Root, Index + 1, Warnings, [Error | Errors])
    end.

parse_block(Block, Root) ->
    [Header | DiagnosticLines] = binary:split(Block, <<"\n">>, [global]),
    Pattern = <<"^(.+\\.erl):([0-9]+)(?::([0-9]+))?:[[:space:]]*(.*)$">>,
    case re:run(Header, Pattern, [{capture, [1, 2, 3, 4], binary}]) of
        {match, [RawPath, RawLine, RawColumn, RawHeadline]} ->
            Path = normalize_path(RawPath, Root),
            Headline = normalize_whitespace(RawHeadline),
            Diagnostic = normalize_whitespace([
                RawHeadline,
                <<" ">>,
                lists:join(<<" ">>, DiagnosticLines)
            ]),
            {ok, #{
                path => Path,
                line => binary_to_integer(RawLine),
                column => parse_column(RawColumn),
                module => filename:basename(Path, ".erl"),
                directory => filename:dirname(Path),
                headline => binary_to_list(Headline),
                diagnostic => binary_to_list(Diagnostic),
                family => classify(Diagnostic),
                ownership => ownership(Path)
            }};
        nomatch ->
            error
    end.

parse_column(<<>>) -> undefined;
parse_column(Column) -> binary_to_integer(Column).

leading_text(Block) ->
    [FirstLine | _] = binary:split(Block, <<"\n">>, [global]),
    binary_to_list(normalize_whitespace(FirstLine)).

normalize_whitespace(Value) ->
    re:replace(iolist_to_binary(Value), <<"[[:space:]]+">>, <<" ">>, [
        global,
        {return, binary}
    ]).

normalize_path(RawPath, Root) ->
    Path = strip_dot_prefix(normalize_separators(binary_to_list(RawPath))),
    RootPrefix = ensure_trailing_separator(Root),
    case lists:prefix(RootPrefix, Path) of
        true -> lists:nthtail(length(RootPrefix), Path);
        false -> portable_source_path(Path)
    end.

normalize_separators(Path) ->
    lists:flatten(string:replace(Path, "\\", "/", all)).

strip_dot_prefix("./" ++ Rest) -> Rest;
strip_dot_prefix(Path) -> Path.

ensure_trailing_separator(Root) ->
    case lists:last(Root) of
        $/ -> Root;
        _ -> Root ++ "/"
    end.

portable_source_path(Path) ->
    Pattern = <<"(?:^|/)((?:src|test)/.*)$">>,
    case re:run(Path, Pattern, [{capture, [1], list}]) of
        {match, [Relative]} -> Relative;
        nomatch -> Path
    end.

%%====================================================================
%% Classification And Reporting
%%====================================================================

classify(Diagnostic) ->
    Text = string:lowercase(binary_to_list(Diagnostic)),
    classify_text(Text).

classify_text(Text) ->
    classify_text(Text, [
        {missing_metadata, [
            "unknown function", "unknown type", "callback info", "unknown behaviour",
            "missing or unexported function"
        ]},
        {record_field_mismatch, [
            "record construction", "violates the declared type of field", "record field"
        ]},
        {ignored_return, [
            "expression produces a value", "returned value", "term is constructed, but never used"
        ]},
        {call_contract_mismatch, [
            "breaks the contract", "does not have a local return", "will never return since",
            "contract is not compatible", "the call "
        ]},
        {type_specification_contract, [
            "type specification", "invalid type specification", "the specification for",
            "specification is not equal", "the contract "
        ]},
        {no_return_control_flow, [
            "has no local return", "created fun has no local return", "only terminates with"
        ]},
        {unreachable_pattern_variable_guard, [
            "will never be called", "can never match", "will never match", "can never succeed",
            "previous clauses completely covered", "pattern can never",
            "attempt to match a term of type"
        ]}
    ]).

classify_text(_Text, []) -> other;
classify_text(Text, [{Family, Needles} | Rest]) ->
    case contains_any(Text, Needles) of
        true -> Family;
        false -> classify_text(Text, Rest)
    end.

contains_any(Text, Needles) ->
    lists:any(fun(Needle) -> string:find(Text, Needle) =/= nomatch end, Needles).

ownership(Path) ->
    ownership_for_prefix(Path, [
        {"src/compiler/effects/", compiler_effects},
        {"src/proptest/", property_testing},
        {"src/compiler/types/", compiler_types},
        {"src/testing/", testing_compatibility_bridges},
        {"src/repl/", repl},
        {"src/runtime/", runtime},
        {"src/compiler/semantic/", compiler_semantic_analysis},
        {"src/compiler/parser/", compiler_parser},
        {"src/compiler/validation/", compiler_validation},
        {"src/compiler/ast/", compiler_ast},
        {"src/compiler/runtime/", compiler_runtime},
        {"src/compiler/codegen/", compiler_codegen},
        {"src/compiler/error/", compiler_error},
        {"src/compiler/", compiler_root},
        {"src/stdlib/", standard_library},
        {"src/tooling/", tooling},
        {"test/", tests}
    ]).

ownership_for_prefix(_Path, []) -> other;
ownership_for_prefix(Path, [{Prefix, Area} | Rest]) ->
    case lists:prefix(Prefix, Path) of
        true -> Area;
        false -> ownership_for_prefix(Path, Rest)
    end.

sort_warnings(Warnings) ->
    lists:sort(fun warning_precedes/2, Warnings).

warning_precedes(Left, Right) ->
    warning_sort_key(Left) < warning_sort_key(Right).

warning_sort_key(Warning) ->
    {
        maps:get(path, Warning),
        maps:get(line, Warning),
        column_sort_value(maps:get(column, Warning)),
        maps:get(headline, Warning)
    }.

column_sort_value(undefined) -> -1;
column_sort_value(Column) -> Column.

count_by(Key, Warnings) ->
    Counts = lists:foldl(
        fun(Warning, Acc) ->
            Value = maps:get(Key, Warning),
            maps:update_with(Value, fun(Count) -> Count + 1 end, 1, Acc)
        end,
        #{},
        Warnings
    ),
    lists:sort(maps:to_list(Counts)).

format_counts(Title, Counts) ->
    [Title, ":\n", [io_lib:format("  ~s: ~B\n", [key_text(Key), Count]) ||
        {Key, Count} <- Counts]].

key_text(Key) when is_atom(Key) -> atom_to_list(Key);
key_text(Key) -> Key.
