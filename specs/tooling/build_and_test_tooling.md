# Build And Test Tooling

## Status

Promoted status: implemented with native generated lexer/parser stages,
canonical rebar3 entry points, subsystem-organized coverage, and a complete but
currently non-green active EUnit surface.

## Design Anchors

- [AGENTS.md](../../AGENTS.md)
- [Current Status](../planning/current_status.md)
- [Phase 2 Test Baseline](../planning/spec-source-reconciliation/phase-02-test-baseline.md)
- [Testing And Quality Contract](../contracts/testing_and_quality_contract.md)
- `rebar.config`
- `Makefile`
- `scripts/build.sh`
- `scripts/build_lexer.sh`
- `scripts/build_parser.sh`

## Current Promoted Surface

- Rebar3 is the canonical workflow; Make targets are thin checked wrappers.
- Lexer and parser generation are part of the normal build path rather than a manual pre-step.
- Tests are organized by subsystem across compiler, runtime, integration, REPL, stdlib, testing, and property-testing areas.
- `test_legacy/proper/` is now historical/documentary only; it no longer holds runnable suites that participate in the maintained workflow.
- The repo-wide `rebar3 eunit` entry point compiles and executes the complete
  active suite, then exits nonzero for the published reconciliation baseline.

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
- its current nonzero result and exact failure inventory are published in the
  Phase 3 test baseline
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
