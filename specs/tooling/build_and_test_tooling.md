# Build And Test Tooling

## Status

Promoted status: implemented with generated lexer/parser hooks, multiple test entry points, subsystem-organized coverage, and a clean repo-wide `rebar3 eunit` path for the active test surface.

## Design Anchors

- [AGENTS.md](../../AGENTS.md)
- [Current Status](../planning/current_status.md)
- [Testing And Quality Contract](../contracts/testing_and_quality_contract.md)
- `rebar.config`
- `Makefile`
- `scripts/build.sh`
- `scripts/build_lexer.sh`
- `scripts/build_parser.sh`

## Current Promoted Surface

- The repo supports both `make`-based and `rebar3`-based workflows.
- Lexer and parser generation are part of the normal build path rather than a manual pre-step.
- Tests are organized by subsystem across compiler, runtime, integration, REPL, stdlib, testing, and property-testing areas.
- `test_legacy/proper/` is now historical/documentary only; it no longer holds runnable suites that participate in the maintained workflow.
- The repo-wide `rebar3 eunit` entry point completes successfully across the active suite.

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

### AC-TOOL-004 Default Full-Test Path Is Clean

The following is part of the current promoted repo status:

- `rebar3 eunit` completes successfully across the maintained suite
- the default path no longer depends on or compiles runnable PropEr-era test modules from `test_legacy/proper/`

This criterion exists so the specs record both the historical boundary and the restored green default workflow precisely.

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
