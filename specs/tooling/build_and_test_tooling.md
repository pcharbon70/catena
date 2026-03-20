# Build And Test Tooling

## Status

Promoted status: implemented with generated lexer/parser hooks, multiple test entry points, subsystem-organized coverage, and one important unresolved gap in the default `rebar3 eunit` workflow.

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
- The default repo-wide `rebar3 eunit` path is not yet clean because legacy PropEr-oriented test modules still sit in the test tree while PropEr is no longer configured as a dependency.

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

### AC-TOOL-004 Known Full-Test Gap

The following is part of the current promoted repo status and must stay visible until resolved:

- `rebar3 eunit` does not complete cleanly because PropEr-oriented test modules still compile under the test tree while PropEr is absent from `rebar.config`

This criterion is intentionally explicit so the specs do not accidentally overstate repository health.

### AC-TOOL-005 Coverage And Analysis Boundaries

The promoted tooling configuration includes the current coverage and dialyzer boundaries:

- generated parser/lexer modules are excluded where appropriate
- owned compiler/runtime/tooling code is the main coverage and analysis target
- static analysis is part of the intended workflow even if it is not run on every edit

### AC-TOOL-006 Migration Expectation

Before the repository can claim a clean default test workflow again, the project must either:

- migrate the legacy PropEr-style tests to the internal testing/property infrastructure, or
- isolate/remove those tests from the default compilation path

Until then, subsystem tests and implementation summaries remain meaningful evidence, but not a substitute for a clean repo-wide `rebar3 eunit`.
