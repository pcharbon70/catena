# Build And Test Tooling

## Status

Promoted status: implemented with native generated lexer/parser stages,
canonical rebar3 entry points, executable specs governance, manifest-selected
conformance, subsystem-organized coverage, and a green complete EUnit surface.

## Design Anchors

- [AGENTS.md](../../AGENTS.md)
- [Current Status](../planning/current_status.md)
- [Phase 7 Test Baseline](../planning/spec-source-reconciliation/phase-07-test-baseline.md)
- [Testing And Quality Contract](../contracts/testing_and_quality_contract.md)
- [Specs Governance And Compliance Guide](../specs-governance-and-compliance-guide.md)
- `rebar.config`
- `Makefile`
- `.github/workflows/ci.yml`
- `scripts/build.sh`
- `scripts/build_lexer.sh`
- `scripts/build_parser.sh`
- `scripts/check_specs.escript`

## Current Promoted Surface

- Rebar3 is the canonical workflow; Make targets are thin checked wrappers.
- Lexer and parser generation are part of the normal build path rather than a manual pre-step.
- Tests are organized by subsystem across compiler, runtime, integration, REPL, stdlib, testing, and property-testing areas.
- `test_legacy/proper/` is now historical/documentary only; it no longer holds runnable suites that participate in the maintained workflow.
- The repo-wide `rebar3 eunit` entry point compiles and executes the complete
  active suite and exits successfully at the published Phase 7 baseline.
- `make check-specs` validates promoted requirements, scenarios, executable
  evidence, component criteria, ADRs, paths, and local Markdown links.
- `make conformance` runs the unique EUnit module set named by
  `specs/conformance/executable_scenarios.tsv`.
- `make verify` composes specs governance and the complete active test suite.
- The read-only GitHub Actions workflow runs `make verify` for pull requests
  and pushes to `main`.

## Acceptance Criteria

### AC-TOOL-001 Generated Sources Stay Generated

The promoted build workflow must continue to regenerate lexer and parser
artifacts from their `.xrl` and `.yrl` sources through Rebar's native compiler
stages. Generated outputs are ignored build products, not hand-maintained
source files.

### AC-TOOL-002 Multiple Supported Entry Points

The repo's current supported tooling surface includes:

- `rebar3` for canonical OTP-style compile, test, coverage, and dialyzer tasks
- `make` targets as convenience wrappers around the corresponding rebar3 tasks

The wrappers must not maintain separate hand-enumerated source or test lists.

### AC-TOOL-003 Organized Test Surfaces

The test tree must remain organized by subsystem so implementation work can be validated in focused slices, including the existing families for compiler components, runtime, integration, REPL, stdlib, testing, and proptest.

### AC-TOOL-004 Default Full-Test Path Is Complete And Truthful

The following is part of the current promoted repo status:

- `rebar3 eunit` discovers, compiles, and executes the maintained suite
- its current green result and exact total are published in the Phase 7 test
  baseline
- the default path no longer depends on or compiles runnable PropEr-era test modules from `test_legacy/proper/`

This criterion prevents a partial or filtered command from being promoted as a
green default workflow.

### AC-TOOL-005 Coverage And Analysis Boundaries

The promoted tooling configuration includes the current coverage and dialyzer boundaries:

- generated parser/lexer modules are excluded where appropriate
- owned compiler/runtime/tooling code is the main coverage and analysis target
- static analysis is part of the intended workflow even if it is not run on every edit

### AC-TOOL-006 Migration Expectation

The project must keep any future rediscovered PropEr-era artifacts out of the default compile path unless they are either:

- migrated to the internal testing/property infrastructure, or
- intentionally retired

At present, the historical directory is documentation-only rather than an active quarantine of runnable suites.

### AC-TOOL-007 Specs Governance And CI Stay Executable

Promoted requirements, scenarios, component criteria, ADRs, paths, and local
Markdown links must pass `make check-specs`. Scenario evidence must remain
executable through `make conformance`, while `make verify` must remain the
single complete local and CI verification contract.
