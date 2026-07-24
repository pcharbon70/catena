# Phase 4: Exhaustive Expression, Pattern, And Data Lowering

**Description:** This phase makes the supported pure-language backend surface
exhaustive and semantics-preserving across expressions, operators, bindings,
patterns, clauses, algebraic data, lists, tuples, and records.

**Status:** Planned.

**Dependencies:** Phase 3 complete.

## Section 4.1: Exhaustive Expression And Operator Lowering

**Description:** Give every normalized pure expression and operator an explicit
lowering or a structured rejection path.

- [x] **Section 4.1 Complete**

### Task 4.1.1: Complete Pure Expression Coverage

**Description:** Reconcile semantic AST shapes with backend translation for
literals, applications, lambdas, lets, matches, lists, tuples, records, and
field access.

- [x] **Task 4.1.1 Complete**

#### Subtask 4.1.1.1: Lower Values And Bindings

**Description:** Validate literal, list, tuple, record, lambda, application,
simple-let, and nested-let behavior without placeholder nodes or lost scope.

- [x] **Subtask 4.1.1.1 Complete**

#### Subtask 4.1.1.2: Lower Matches And Field Access

**Description:** Preserve match scrutinees, clause ordering, record map access,
and missing-field runtime behavior through explicit Core forms.

- [x] **Subtask 4.1.1.2 Complete**

### Task 4.1.2: Reconcile Operator Semantics

**Description:** Align parser operators, semantic desugaring, type inference,
and backend emission so each supported operator has one documented meaning.

- [x] **Task 4.1.2 Complete**

#### Subtask 4.1.2.1: Lower Primitive Operators Explicitly

**Description:** Cover arithmetic, comparison, Boolean, pipe, list append, and
list cons operations with verified OTP targets and evaluation semantics.

- [x] **Subtask 4.1.2.1 Complete**

#### Subtask 4.1.2.2: Validate Desugared Library Operators

**Description:** Confirm category-theory and do-notation operators reach call
resolution as explicit library calls rather than leaking parser operator atoms
into arbitrary BIF emission.

- [x] **Subtask 4.1.2.2 Complete**

## Section 4.2: Lossless Pattern And Clause Compilation

**Description:** Compile every promoted parser-native pattern without wildcard
substitution while preserving bindings, guards, and source clause order.

- [x] **Section 4.2 Complete**

### Task 4.2.1: Complete Pattern Representation Coverage

**Description:** Align variable, wildcard, literal, constructor, list, cons,
tuple, record, as-pattern, and or-pattern shapes across semantic validation and
Core generation.

- [x] **Task 4.2.1 Complete**

#### Subtask 4.2.1.1: Compile Structural Patterns

**Description:** Emit exact Core patterns for constructors, lists, cons cells,
tuples, records, literals, variables, and wildcards.

- [x] **Subtask 4.2.1.1 Complete**

#### Subtask 4.2.1.2: Compile As-Patterns And Or-Patterns

**Description:** Preserve alias bindings and expand valid alternatives without
dropping nested structure or changing bound-name sets.

- [x] **Subtask 4.2.1.2 Complete**

### Task 4.2.2: Preserve Clause And Binding Semantics

**Description:** Keep transform parameters, match clauses, guards, and binding
scope correct when the backend introduces synthetic parameters or cases.

- [x] **Task 4.2.2 Complete**

#### Subtask 4.2.2.1: Compile Complex Transform Parameters

**Description:** Ensure synthetic function parameters are immediately matched
against the original patterns and never replace them with fresh unconstrained
variables.

- [x] **Subtask 4.2.2.1 Complete**

#### Subtask 4.2.2.2: Compile Guards And Pattern Bindings

**Description:** Preserve guard conjunction, guard purity, clause fallthrough,
and all variables introduced by successful patterns.

- [x] **Subtask 4.2.2.2 Complete**

## Section 4.3: Stable Data Representation And Erasure

**Description:** Apply the accepted runtime representation table consistently
and erase static type information only after all representation metadata is
fixed.

- [ ] **Section 4.3 Complete**

### Task 4.3.1: Stabilize Value Representations

**Description:** Make constructor, list, tuple, record, primitive, and closure
representations explicit and consistent across construction, access, and
pattern matching.

- [ ] **Task 4.3.1 Complete**

#### Subtask 4.3.1.1: Validate Constructor And Collection Symmetry

**Description:** Ensure each emitted value can be consumed by the corresponding
pattern form with the same tag, arity, field, and element conventions.

- [ ] **Subtask 4.3.1.1 Complete**

#### Subtask 4.3.1.2: Validate Record Map Semantics

**Description:** Confirm record construction, field access, and exact record
patterns use compatible map-key and missing-field behavior.

- [ ] **Subtask 4.3.1.2 Complete**

### Task 4.3.2: Harden Type Erasure

**Description:** Make erasure an explicit transformation over classified
declarations and supported expressions rather than a permissive identity
fallback.

- [ ] **Task 4.3.2 Complete**

#### Subtask 4.3.2.1: Erase Static Types Exhaustively

**Description:** Remove type signatures, annotations, and static declarations
only through explicit clauses that preserve required constructor and dispatch
metadata.

- [ ] **Subtask 4.3.2.1 Complete**

#### Subtask 4.3.2.2: Reject Unknown Erasure Forms

**Description:** Return an invalid-disposition or unsupported-construct error
when erasure receives a normalized form without an accepted rule.

- [ ] **Subtask 4.3.2.2 Complete**

## Section 4.4: Phase 4 Integration Tests

**Description:** Execute representative pure Catena programs for every
promoted expression, operator, pattern, and data representation and verify
negative paths fail closed.

- [ ] **Section 4.4 Complete**

### Task 4.4.1: Execute Pure Feature Matrix

**Description:** Add source-to-BEAM tests that exercise supported values,
operators, bindings, matches, records, collections, and clauses in combination.

- [ ] **Task 4.4.1 Complete**

#### Subtask 4.4.1.1: Test Expressions And Data

**Description:** Execute literals, lists, tuples, records, field access,
lambdas, lets, primitive operators, pipe, and desugared pure operators.

- [ ] **Subtask 4.4.1.1 Complete**

#### Subtask 4.4.1.2: Test Pattern Matrix

**Description:** Execute every promoted pattern form, nested combinations,
guards, aliases, alternative patterns, and multi-clause fallthrough.

- [ ] **Subtask 4.4.1.2 Complete**

### Task 4.4.2: Verify Semantic And Failure Boundaries

**Description:** Compare construction and matching results and verify all
unsupported pure forms fail before Core success.

- [ ] **Task 4.4.2 Complete**

#### Subtask 4.4.2.1: Test Representation Round Trips

**Description:** Construct values in Catena, deconstruct them through Catena
patterns or accessors, and assert the expected observable result after BEAM
execution.

- [ ] **Subtask 4.4.2.1 Complete**

#### Subtask 4.4.2.2: Run Phase Completion Gates

**Description:** Run expression, operator, pattern, erasure, Core validation,
and source-to-BEAM suites plus `make check-specs`, `make conformance`, and the
complete active EUnit suite.

- [ ] **Subtask 4.4.2.2 Complete**
