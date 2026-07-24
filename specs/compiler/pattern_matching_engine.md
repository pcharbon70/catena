# Pattern Matching Engine

## Status

Promoted status: parser-native patterns are integrated through semantic
validation and Core Erlang lowering. Exhaustiveness/redundancy analysis and
decision-tree construction are implemented and tested as separate compiler
surfaces, not yet selected by the default source-to-Core path.

## Design Anchors

- [Current Status](../planning/current_status.md)
- [Compiler Contract](../contracts/compiler_contract.md)
- `src/compiler/semantic/catena_pattern_check.erl`
- `src/compiler/codegen/catena_pattern_decision_tree.erl`
- `src/compiler/codegen/catena_codegen_pattern.erl`
- `src/compiler/codegen/catena_codegen_lower.erl`
- `test/compiler/parser/catena_parser_advanced_pattern_tests.erl`
- `test/compiler/types/catena_pattern_advanced_tests.erl`
- `test/compiler/codegen/catena_pattern_decision_tree_tests.erl`
- `test/compiler/integration/catena_pattern_integration_tests.erl`
- `test/compiler/integration/catena_pattern_contract_tests.erl`

## Current Promoted Surface

- Pattern matching is no longer just parser sugar; it is a multi-layer compiler feature.
- Canonical transform and match clauses now lower into binding-safe Core Erlang
  case clauses.
- The codebase contains static analysis for exhaustiveness/redundancy and a
  decision-tree builder, but neither is wired into the default Core emission
  path yet.
- Guard purity is part of the pattern story because effectful guards would break the intended semantics.
- Semantic analysis also enforces transform clause arity and identical bound
  names across or-pattern alternatives.

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

### AC-PAT-002 Core Clause And Decision-Tree Boundaries

The promoted executable path must preserve canonical pattern bindings while
lowering transform and match clauses to Core Erlang cases. The separate
decision-tree surface includes:

- pattern matrix construction
- heuristic column selection
- constructor specialization
- default-branch handling
- lowering into Core Erlang-oriented structures in its focused tests

The decision-tree surface must not be described as the default compiler path
until it preserves source bindings and is selected by
`compile_string_to_core/1,2`.

### AC-PAT-003 Exhaustiveness And Redundancy Analysis

Static pattern analysis remains a separately callable compiler feature. It
must continue to provide:

- exhaustiveness checking
- missing-pattern witness generation
- redundancy detection
- warning formatting tied to source locations

The current public Core pipeline enforces guard purity, clause arity, and
or-pattern binding consistency. Automatic exhaustiveness/redundancy warning
collection at that public boundary remains follow-on work.

### AC-PAT-004 Guard Purity

Pattern guards are only promoted as correct when they remain pure within the current effect model. Any design or implementation change that allows effectful guards would need a new ADR and contract update before it can become canonical.

### AC-PAT-005 Reconciled Phase Status

The promoted current status for the pattern engine is:

- advanced parser-native pattern lowering: integrated
- guard purity, clause arity, and or-pattern bindings: compiler-enforced
- decision-tree construction: implemented and tested separately
- exhaustiveness and redundancy checking: implemented and tested separately

This criterion records the verified separation between the executable
clause-lowering path and the separately tested analysis and decision-tree
surfaces.

## Out Of Scope

- selecting decision-tree lowering as the default public compiler path
- automatic exhaustiveness/redundancy diagnostics from the public Core API
- runtime pattern backtracking beyond the current Core case strategy
- pattern-system extensions not represented in the current parser, type, and codegen tests
