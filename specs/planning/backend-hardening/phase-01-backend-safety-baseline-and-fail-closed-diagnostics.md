# Phase 1: Backend Safety Baseline And Fail-Closed Diagnostics

**Description:** This phase makes the current backend boundary reproducible,
introduces stable backend diagnostic categories, and removes the most dangerous
placeholder and wildcard fallbacks before later phases expand executable
coverage.

**Status:** Complete.

**Dependencies:** ADR-0005 and the Core Erlang and BEAM backend spec accepted.

## Section 1.1: Reproducible Backend Support Inventory

**Description:** Establish a code- and test-backed inventory that distinguishes
proven execution, lowering-only behavior, static erasure, runtime lowering,
known failure, and deferred support.

- [x] **Section 1.1 Complete**

### Task 1.1.1: Publish The Feature Support Ledger

**Description:** Create a maintained ledger for every parser-native
declaration, expression, pattern, and operator that can reach semantic
analysis.

- [x] **Task 1.1.1 Complete**

#### Subtask 1.1.1.1: Enumerate Frontend Constructs

**Description:** Derive the inventory from the generated grammar sources,
canonical semantic AST shapes, desugaring rules, and public compiler entry
points.

- [x] **Subtask 1.1.1.1 Complete**

#### Subtask 1.1.1.2: Assign Initial Support Classes

**Description:** Label each construct as proven, lowering-only, static-erased,
runtime-lowered, known-failing, or deferred and link the label to source and
test evidence.

- [x] **Subtask 1.1.1.2 Complete**

### Task 1.1.2: Capture Known Backend Failure Fixtures

**Description:** Turn the backend gaps that motivated ADR-0005 into
deterministic regression fixtures before changing their behavior.

- [x] **Task 1.1.2 Complete**

#### Subtask 1.1.2.1: Capture Named-Call Core Failure

**Description:** Add a source fixture showing that one top-level transform
calling another currently reaches Core validation as an unbound variable.

- [x] **Subtask 1.1.2.1 Complete**

#### Subtask 1.1.2.2: Capture Lossy Fallback Behavior

**Description:** Add focused fixtures for unknown-expression placeholders,
unknown-pattern wildcards, complex-binding approximation, unknown operators,
and silently ignored declarations.

- [x] **Subtask 1.1.2.2 Complete**

## Section 1.2: Structured Backend Diagnostic Taxonomy

**Description:** Define stable internal and public error forms so every
fail-closed change can report the construct, compiler stage, and source
location instead of crashing or returning a generated placeholder.

- [x] **Section 1.2 Complete**

### Task 1.2.1: Define Backend Error Types

**Description:** Add a shared backend diagnostic representation covering the
accepted categories in the backend spec.

- [x] **Task 1.2.1 Complete**

#### Subtask 1.2.1.1: Model Stable Error Categories

**Description:** Represent unsupported constructs, unresolved or ambiguous
calls, arity mismatches, invalid declaration dispositions, Core validation
failures, and BEAM compilation failures.

- [x] **Subtask 1.2.1.1 Complete**

#### Subtask 1.2.1.2: Preserve Source Context

**Description:** Carry module, transform, construct kind, original source
location, and relevant generated identity through backend error terms.

- [x] **Subtask 1.2.1.2 Complete**

### Task 1.2.2: Propagate Backend Errors Through Public Core APIs

**Description:** Make the existing source-to-Core entry points return
structured backend errors without collapsing earlier frontend error families.

- [x] **Task 1.2.2 Complete**

#### Subtask 1.2.2.1: Normalize Backend Return Contracts

**Description:** Replace throw-, crash-, and embedded-value paths with one
consistent `{ok, Artifact}` or `{error, Diagnostic}` contract at the
orchestration boundary.

- [x] **Subtask 1.2.2.1 Complete**

#### Subtask 1.2.2.2: Add Backend Diagnostic Formatting

**Description:** Format backend errors in Catena vocabulary and prefer source
names and locations over generated Core Erlang identifiers.

- [x] **Subtask 1.2.2.2 Complete**

## Section 1.3: Fail-Closed Code Generation Fallbacks

**Description:** Remove fallback behavior that can silently change program
meaning while preserving explicitly supported paths.

- [x] **Section 1.3 Complete**

### Task 1.3.1: Reject Unknown Expressions And Operators

**Description:** Ensure expression and operator translation is exhaustive for
the current supported set and rejects every other normalized form.

- [x] **Task 1.3.1 Complete**

#### Subtask 1.3.1.1: Remove Placeholder Expression Values

**Description:** Replace generated `{error, unknown_expression, ...}` tuples
with `unsupported_backend_construct` diagnostics before Core module success.

- [x] **Subtask 1.3.1.1 Complete**

#### Subtask 1.3.1.2: Remove Arbitrary Operator-To-BIF Fallback

**Description:** Require every backend operator to have an explicit lowering
or return an unsupported-operator diagnostic.

- [x] **Subtask 1.3.1.2 Complete**

### Task 1.3.2: Reject Lossy Pattern And Declaration Paths

**Description:** Stop replacing unknown patterns with wildcards or dropping
unclassified declarations during module function filtering.

- [x] **Task 1.3.2 Complete**

#### Subtask 1.3.2.1: Remove Unknown-Pattern Wildcards

**Description:** Return a source-oriented backend error whenever pattern
lowering or compilation receives an unsupported shape.

- [x] **Subtask 1.3.2.1 Complete**

#### Subtask 1.3.2.2: Reject Unclassified Declarations

**Description:** Detect declarations without an explicit provisional
disposition before module emission and reject them instead of filtering them
away.

- [x] **Subtask 1.3.2.2 Complete**

## Section 1.4: Phase 1 Integration Tests

**Description:** Verify that the original executable vertical slice remains
green while every captured lossy fallback now fails deterministically before
artifact success.

- [x] **Section 1.4 Complete**

### Task 1.4.1: Verify Supported Baseline Preservation

**Description:** Run the current source-to-Core-to-BEAM arithmetic and
constructor-pattern programs through the hardened error boundary.

- [x] **Task 1.4.1 Complete**

#### Subtask 1.4.1.1: Execute Existing Positive Fixtures

**Description:** Confirm modules, exports, primitive arithmetic, constructors,
and multi-clause constructor matches still compile, load, and return expected
values.

- [x] **Subtask 1.4.1.1 Complete**

#### Subtask 1.4.1.2: Preserve Frontend Failure Families

**Description:** Confirm lexer, parser, semantic, kind, import, type, and effect
failures still stop before code generation with their original categories.

- [x] **Subtask 1.4.1.2 Complete**

### Task 1.4.2: Verify Fail-Closed Rejection

**Description:** Exercise every fallback removed in this phase through both
focused backend tests and public source-to-Core tests where the grammar can
produce the construct.

- [x] **Task 1.4.2 Complete**

#### Subtask 1.4.2.1: Assert No Placeholder Artifacts

**Description:** Verify unknown expressions, operators, patterns, and
declarations return structured errors and do not appear in successful Core
Erlang.

- [x] **Subtask 1.4.2.1 Complete**

#### Subtask 1.4.2.2: Run Phase Completion Gates

**Description:** Run focused codegen suites, `make check-specs`,
`make conformance`, and the complete active EUnit suite and publish the ending
support ledger.

- [x] **Subtask 1.4.2.2 Complete**

### Phase Completion Evidence

- `catena_backend_hardening_phase1_tests`: 15 passing integration tests
- focused backend suites: 168 passing tests before the final integration suite
- `make check-specs`: 42 requirements, 11 scenarios, 21 evidence rows across
  20 modules, 73 acceptance criteria, and five ADRs
- `make conformance`: 426 passing tests
- `make test`: 4,873 passing tests, zero failures, and zero skipped tests
