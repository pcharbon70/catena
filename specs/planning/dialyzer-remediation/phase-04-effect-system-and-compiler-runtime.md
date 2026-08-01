# Phase 4: Effect System and Compiler Runtime

**Description:** This phase clears the effect-system, validation, and compiler
runtime inventory by reconciling handler lifecycle, resumption multiplicity,
equation rewriting, row/effect contracts, and runtime execution results.

**Status:** Planned.

**Dependencies:** Phase 3 complete.

## Section 4.1: Handlers And Resumptions

**Description:** Make handler registration, lookup, scope, depth, continuation,
and one-shot/multi-shot contracts describe their real state transitions.

- [ ] **Section 4.1 Complete**

### Task 4.1.1: Reconcile Handler Lifecycle

**Description:** Align handler stacks, registration results, lookup failures,
depth tracking, and scope restoration across the effect façade.

- [ ] **Task 4.1.1 Complete**

#### Subtask 4.1.1.1: Specify Handler State

**Description:** Replace broad terms and inconsistent tuples with shared
handler, operation, context, and result types.

- [ ] **Subtask 4.1.1.1 Complete**

#### Subtask 4.1.1.2: Verify Scope Restoration

**Description:** Exercise success, missing handler, thrown handler, nested
scope, and cleanup paths before eliminating control-flow warnings.

- [ ] **Subtask 4.1.1.2 Complete**

### Task 4.1.2: Reconcile Resumption Multiplicity

**Description:** Make capture, resume, clone, consumed-state, and multiplicity
errors truthful for ordinary, one-shot, and multi-shot resumptions.

- [ ] **Task 4.1.2 Complete**

#### Subtask 4.1.2.1: Align Continuation Records

**Description:** Reconcile record fields and function types used by captured
continuations and resumption wrappers.

- [ ] **Subtask 4.1.2.1 Complete**

#### Subtask 4.1.2.2: Remove Impossible Resumption Branches

**Description:** Correct state types or remove dead clauses only after focused
multiplicity and reuse tests prove the intended behavior.

- [ ] **Subtask 4.1.2.2 Complete**

## Section 4.2: Equations And Algebraic Operations

**Description:** Reconcile equation construction, matching, unification,
rewriting strategies, proofs, and algebraic-law metadata.

- [ ] **Section 4.2 Complete**

### Task 4.2.1: Repair Equation Application

**Description:** Resolve the high-density call and no-return findings in
equation application, normalization, substitution, and rewrite strategy flow.

- [ ] **Task 4.2.1 Complete**

#### Subtask 4.2.1.1: Standardize Rewrite Results

**Description:** Use explicit changed/unchanged/error outcomes across rewrite,
normalize, and strategy APIs.

- [ ] **Subtask 4.2.1.1 Complete**

#### Subtask 4.2.1.2: Verify Termination Boundaries

**Description:** Preserve bounded normalization, failed matching, and proof
diagnostics while eliminating false no-return cascades.

- [ ] **Subtask 4.2.1.2 Complete**

### Task 4.2.2: Repair Effect Algebra Contracts

**Description:** Align unions, intersections, rows, effect operations,
equation sets, and law callbacks with the concrete values accepted by the
implementation.

- [ ] **Task 4.2.2 Complete**

#### Subtask 4.2.2.1: Reconcile Callback Signatures

**Description:** Correct callback metadata and implementation specs so
behavior validation is available to Dialyzer.

- [ ] **Subtask 4.2.2.1 Complete**

#### Subtask 4.2.2.2: Remove Unreachable Algebra Clauses

**Description:** Delete dead clauses or restore intended variants through
upstream contract fixes, backed by algebraic-law tests.

- [ ] **Subtask 4.2.2.2 Complete**

## Section 4.3: Effect Validation And Runtime

**Description:** Close the static contract across compiler validation,
generated-effect execution, explicit contexts, and built-in handlers.

- [ ] **Section 4.3 Complete**

### Task 4.3.1: Repair Validation Reports

**Description:** Align theoretical, property, conformance, and aggregate report
types with every emitted validation result.

- [ ] **Task 4.3.1 Complete**

#### Subtask 4.3.1.1: Specify Report Shapes

**Description:** Replace partial maps and broad result types with maintained
report records or precise map types.

- [ ] **Subtask 4.3.1.1 Complete**

#### Subtask 4.3.1.2: Verify Failure Reporting

**Description:** Test failing validation and missing-data paths so tightening
specifications cannot erase diagnostics.

- [ ] **Subtask 4.3.1.2 Complete**

### Task 4.3.2: Repair Compiler Runtime Calls

**Description:** Reconcile explicit-context operation dispatch, handler
processes, built-ins, timeouts, and cross-process results.

- [ ] **Task 4.3.2 Complete**

#### Subtask 4.3.2.1: Align Runtime Protocol Results

**Description:** Give requests, replies, timeouts, handler failures, and
cleanup one explicit set of result types.

- [ ] **Subtask 4.3.2.1 Complete**

#### Subtask 4.3.2.2: Run The Complete Effect Gate

**Description:** Require focused effect/type/runtime suites and Phase 14
validation to pass before declaring owned areas warning-free.

- [ ] **Subtask 4.3.2.2 Complete**

**Acceptance Criteria:**

- `src/compiler/effects`, `src/compiler/validation`, and
  `src/compiler/runtime` have no remaining Dialyzer warnings
- Handler and resumption failure paths remain tested
- Rewrite and normalization APIs terminate within their documented bounds
- Effect validation reports remain actionable
- Focused effect/type/runtime tests pass
- `make verify` remains green
- The phase publishes its exact ending warning count
