#!/usr/bin/env bash
# Run tests with coverage reporting
# Usage: ./scripts/run_coverage.sh [MODULE]

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Catena Test Coverage Report ===${NC}\n"

# Create coverage output directory
mkdir -p _build/test/cover

# Check if specific module was requested
MODULE=""
if [ $# -gt 0 ]; then
    MODULE="--module=$1"
    echo -e "${YELLOW}Running coverage for module: $1${NC}\n"
else
    echo -e "${YELLOW}Running coverage for all test modules${NC}\n"
fi

# Compile all source modules with debug_info for coverage
echo -e "${BLUE}Compiling source modules with debug_info...${NC}"
mkdir -p _build/test
erlc +debug_info -o _build/test -pa _build/test -I src \
    src/compiler/types/catena_type_state.erl \
    src/compiler/types/catena_types.erl \
    src/compiler/types/catena_type_subst.erl \
    src/compiler/types/catena_type_scheme.erl \
    src/compiler/types/catena_type_env.erl \
    src/compiler/types/catena_type_pp.erl \
    src/compiler/types/catena_type_error.erl \
    src/compiler/catena_compiler_utils.erl

# Compile test modules
echo -e "${BLUE}Compiling test modules...${NC}"
erlc -o _build/test -pa _build/test -I src \
    test/compiler/catena_compiler_utils_tests.erl \
    test/compiler/types/catena_types_tests.erl \
    test/compiler/types/catena_type_subst_tests.erl \
    test/compiler/types/catena_type_scheme_tests.erl \
    test/compiler/types/catena_type_env_tests.erl \
    test/compiler/types/catena_type_pp_tests.erl \
    test/compiler/types/catena_type_integration_tests.erl \
    test/compiler/types/catena_type_error_tests.erl

# Run tests with coverage
echo -e "\n${BLUE}Running tests with coverage...${NC}\n"
erl -noshell -pa _build/test -eval "
    % Start cover
    cover:start(),

    % Compile modules for coverage
    Modules = [
        catena_type_state,
        catena_types,
        catena_type_subst,
        catena_type_scheme,
        catena_type_env,
        catena_type_pp,
        catena_type_error,
        catena_compiler_utils
    ],

    lists:foreach(fun(M) ->
        BeamFile = lists:flatten(io_lib:format(\"_build/test/~p.beam\", [M])),
        case cover:compile_beam(BeamFile) of
            {ok, M} ->
                io:format(\"Instrumented ~p for coverage~n\", [M]);
            {error, Reason} ->
                io:format(\"Warning: Could not compile ~p for coverage: ~p~n\", [M, Reason])
        end
    end, Modules),

    io:format(\"~n\", []),

    % Run all test suites
    TestModules = [
        catena_compiler_utils_tests,
        catena_types_tests,
        catena_type_subst_tests,
        catena_type_scheme_tests,
        catena_type_env_tests,
        catena_type_pp_tests,
        catena_type_integration_tests,
        catena_type_error_tests
    ],

    eunit:test(TestModules, [verbose]),

    % Export coverage data
    io:format(\"~n~n\"),
    io:format(\"${BLUE}=== Coverage Summary ===${NC}~n~n\", []),

    % Analyze coverage for each module
    lists:foreach(fun(M) ->
        case cover:analyse(M, coverage, line) of
            {ok, Analysis} ->
                Total = length(Analysis),
                Covered = length([X || {_, {C, _}} = X <- Analysis, C > 0]),
                Percentage = if Total > 0 -> (Covered * 100) div Total; true -> 0 end,
                Color = if
                    Percentage >= 80 -> \"${GREEN}\";
                    Percentage >= 60 -> \"${YELLOW}\";
                    true -> \"\\033[0;31m\"
                end,
                io:format(\"~s~-30s ~3B% (~B/~B lines)${NC}~n\",
                         [Color, M, Percentage, Covered, Total]);
            {error, _} ->
                io:format(\"~-30s No coverage data~n\", [M])
        end
    end, Modules),

    % Export HTML reports
    io:format(\"~n${BLUE}Exporting HTML coverage reports to _build/test/cover/${NC}~n\", []),
    lists:foreach(fun(M) ->
        OutFile = lists:flatten(io_lib:format(\"_build/test/cover/~p.html\", [M])),
        cover:analyse_to_file(M, OutFile, [html])
    end, Modules),

    % Stop cover
    cover:stop()
" -s init stop

echo -e "\n${GREEN}=== Coverage reports generated in _build/test/cover/ ===${NC}"
echo -e "${BLUE}Open _build/test/cover/*.html in your browser to view detailed coverage${NC}\n"
