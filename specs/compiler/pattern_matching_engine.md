# Pattern Matching Engine

## Status

Promoted status: implemented across parsing, typing, static analysis, and code generation, with decision-tree compilation and exhaustiveness checking present in code and summaries.

## Design Anchors

- [Proof-of-concept Phase 3 plan](../../notes/planning/proof-of-concept/phase-03.md)
- [Phase 3.1 advanced patterns summary](../../notes/summaries/phase-3.1-advanced-patterns-summary.md)
- [Phase 3.2 decision trees summary](../../notes/summaries/phase-3.2-decision-trees-2025-11-25.md)
- [Phase 3.3 exhaustiveness summary](../../notes/summaries/phase-3.3-exhaustiveness-checking-2025-11-25.md)
- [Phase 3.4 integration summary](../../notes/summaries/phase-3.4-integration-tests-2025-11-25.md)
- `src/compiler/semantic/catena_pattern_check.erl`
- `src/compiler/codegen/catena_pattern_decision_tree.erl`
- `src/compiler/codegen/catena_codegen_pattern.erl`
- `test/compiler/parser/catena_parser_advanced_pattern_tests.erl`
- `test/compiler/types/catena_pattern_advanced_tests.erl`
- `test/compiler/codegen/catena_pattern_decision_tree_tests.erl`
- `test/compiler/integration/catena_pattern_integration_tests.erl`

## Current Promoted Surface

- Pattern matching is no longer just parser sugar; it is a multi-layer compiler feature.
- The codebase contains both static analysis for exhaustiveness/redundancy and lowering logic for efficient decision trees.
- Guard purity is part of the pattern story because effectful guards would break the intended semantics.
- Phase 3 planning checkboxes lag behind the implementation summaries and source tree, so this spec follows the reconciled current status instead of the stale checklist alone.

## Acceptance Criteria

### AC-PAT-001 End-To-End Pattern Support

The promoted pattern subsystem must cover the pattern forms that are represented in the parser, type tests, and decision-tree/codegen modules, including the implemented family of:

- variable and wildcard patterns
- literal patterns
- constructor patterns
- tuple and list patterns
- cons patterns
- as-patterns and or-patterns where supported by the current parser/tests
- record-oriented pattern forms where supported by the current AST and codegen

This spec follows the implemented and tested surface, not a purely theoretical future grammar.

### AC-PAT-002 Decision-Tree Compilation

Pattern compilation must proceed through a decision-tree representation rather than naive clause-by-clause expansion. The promoted implementation includes:

- pattern matrix construction
- heuristic column selection
- constructor specialization
- default-branch handling
- lowering into Core Erlang-oriented structures

### AC-PAT-003 Exhaustiveness And Redundancy Analysis

Static pattern analysis must remain a first-class compiler feature. The promoted system must continue to provide:

- exhaustiveness checking
- missing-pattern witness generation
- redundancy detection
- warning formatting tied to source locations

### AC-PAT-004 Guard Purity

Pattern guards are only promoted as correct when they remain pure within the current effect model. Any design or implementation change that allows effectful guards would need a new ADR and contract update before it can become canonical.

### AC-PAT-005 Reconciled Phase Status

The promoted current status for the pattern engine is:

- advanced pattern support: implemented
- decision trees: implemented
- exhaustiveness and redundancy checking: implemented
- integration coverage: implemented

This criterion exists because the Phase 3 planning document has not yet been reconciled to those later summaries.

## Out Of Scope

- runtime pattern backtracking beyond the compiled decision-tree strategy
- pattern-system extensions not represented in the current parser, type, and codegen tests
