# Build And Test Tooling

## Status

Promoted status: implemented with generated lexer/parser hooks, multiple test entry points, subsystem-organized coverage, and a default `rebar3 eunit` path that is no longer blocked by legacy PropEr compile contamination.

## Design Anchors

- [AGENTS.md](../../AGENTS.md)
- [Proof-of-concept Phase 2.4 summary](../../notes/summaries/phase-2.4-integration-tests.md)
- [Rose tree completion summary](../../notes/summaries/rose-tree-unit-tests-complete-2025-11-29.md)
- `rebar.config`
- `Makefile`
- `scripts/build.sh`
- `scripts/build_lexer.sh`
- `scripts/build_parser.sh`

## Current Promoted Surface

- The repo supports both `make`-based and `rebar3`-based workflows.
- Lexer and parser generation are part of the normal build path rather than a manual pre-step.
- Tests are organized by subsystem across compiler, runtime, integration, REPL, stdlib, testing, and property-testing areas.
- Legacy PropEr-oriented suites are preserved under `test_legacy/proper/` and intentionally excluded from the default compile path while the internal replacement work continues.
- The repo-wide `rebar3 eunit` entry point now reaches active suite execution without the old PropEr compile failure, but unrelated active-test failures still exist elsewhere in the suite.

## Acceptance Criteria

### AC-TOOL-001 Generated Sources Stay Generated

The promoted build workflow must continue to regenerate lexer and parser artifacts from their `.xrl` and `.yrl` sources through the checked-in build scripts or `rebar3` pre-hooks. Generated outputs are build products, not hand-maintained source files.

### AC-TOOL-002 Multiple Supported Entry Points

The repo's current supported tooling surface includes both:

- `make` targets for focused compiler/test workflows
- `rebar3` workflows for normal OTP-style compile/test/dialyzer tasks

Promoted docs should describe the scope of each entry point rather than implying they are exact substitutes.

### AC-TOOL-003 Organized Test Surfaces

The test tree must remain organized by subsystem so implementation work can be validated in focused slices, including the existing families for compiler components, runtime, integration, REPL, stdlib, testing, and proptest.

### AC-TOOL-004 Default Test Entry Point Is Unblocked

The following is part of the current promoted repo status:

- `rebar3 eunit` reaches active repo test execution without compiling the quarantined legacy PropEr suites

This criterion exists so the specs state the PropEr fix precisely rather than implying either that the old compile-path failure still exists or that the entire active suite is already green.

### AC-TOOL-005 Coverage And Analysis Boundaries

The promoted tooling configuration includes the current coverage and dialyzer boundaries:

- generated parser/lexer modules are excluded where appropriate
- owned compiler/runtime/tooling code is the main coverage and analysis target
- static analysis is part of the intended workflow even if it is not run on every edit

### AC-TOOL-006 Migration Expectation

The project must keep the historical PropEr suites quarantined from the default compile path until they are either:

- migrated to the internal testing/property infrastructure, or
- intentionally retired

This preserves a clean default workflow while keeping the migration status honest.
