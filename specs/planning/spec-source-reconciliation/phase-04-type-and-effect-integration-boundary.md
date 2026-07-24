# Phase 4: Type and Effect Integration Boundary

**Description:** This phase reconciles Catena's higher-level algebraic-effects
orchestration with the promoted type/effect and explicit-context runtime
contracts. It replaces historical descriptor-constructor expectations with
executable handler behavior, makes resumption and handler scope semantics
coherent, connects the public type helpers to implemented inference machinery,
and publishes a deterministic Phase 4 baseline without absorbing failures
owned by later phases.

**Status:** Complete.

**Dependencies:** Phase 3 complete.

## Section 4.1: Public Effect Execution Contract

**Description:** Align the public effect library and builtin runners around
operations that execute through installed handlers, while preserving the
explicit-context runtime as the canonical generated-code boundary.

- [x] **Section 4.1 Complete**

### Task 4.1.1: Reconcile Builtin Effect Runners

**Description:** Make State, Reader, Writer, and Error runners return their
documented values under the direct Erlang orchestration model.

- [x] **Task 4.1.1 Complete**

#### Subtask 4.1.1.1: Normalize Runner Results

**Description:** Return operation results directly from builtin handlers
instead of treating wrapped resumption metadata as an Erlang function.

- [x] **Subtask 4.1.1.1 Complete**

#### Subtask 4.1.1.2: Preserve Error Short-Circuiting

**Description:** Ensure a handled Error operation terminates the protected
computation and returns the selected recovery value.

- [x] **Subtask 4.1.1.2 Complete**

### Task 4.1.2: Replace Historical Descriptor Expectations

**Description:** Update effect-library tests to exercise the executable public
API and keep raw effect descriptors confined to optimizer and distribution
surfaces that still consume them.

- [x] **Task 4.1.2 Complete**

#### Subtask 4.1.2.1: Validate Public Builtins

**Description:** Cover State, Reader, Writer, Error, and custom handled
operations through their public runners.

- [x] **Subtask 4.1.2.1 Complete**

#### Subtask 4.1.2.2: Retain Descriptor-Domain Integration

**Description:** Continue testing optimizer and distribution components with
their native descriptor representation without presenting it as the public
execution API.

- [x] **Subtask 4.1.2.2 Complete**

### Task 4.1.3: Verify Runtime Builtin Surface

**Description:** Reconcile the runtime Process handler test with the complete
implemented operation set.

- [x] **Task 4.1.3 Complete**

#### Subtask 4.1.3.1: Assert Process Operations

**Description:** Require every currently supported Process operation while
keeping the test independent of operation order.

- [x] **Subtask 4.1.3.1 Complete**

**Acceptance Criteria:**

- Public builtin operations execute only inside an appropriate handler scope
- State, Reader, Writer, and Error runners return their documented results
- Optimizer/distribution descriptors are not confused with executable effects
- The runtime Process handler specification matches its implementation

## Section 4.2: Handler and Resumption Orchestration

**Description:** Make handler scopes, continuation wrappers, configuration, and
cross-component orchestration agree on one observable contract.

- [x] **Section 4.2 Complete**

### Task 4.2.1: Expose Coherent Handler Scopes

**Description:** Complete the public scope-management façade used by
diagnostics and integration callers.

- [x] **Task 4.2.1 Complete**

#### Subtask 4.2.1.1: Delegate Scope Operations

**Description:** Add public push and pop operations that preserve the effect
system's existing stack return values.

- [x] **Subtask 4.2.1.1 Complete**

### Task 4.2.2: Normalize Resumption Semantics

**Description:** Treat captured resumptions as opaque values and make the
current direct-style placeholder resume with the supplied operation result.

- [x] **Task 4.2.2 Complete**

#### Subtask 4.2.2.1: Resume Wrapped Continuations

**Description:** Validate deep, shallow, one-shot, and multi-shot wrappers
through the common resumption API rather than invoking wrapper maps directly.

- [x] **Subtask 4.2.2.1 Complete**

#### Subtask 4.2.2.2: Document the CPS Boundary

**Description:** Record that true delimited-continuation capture remains a
compiler/CPS concern and is not simulated by ordinary Erlang stack capture.

- [x] **Subtask 4.2.2.2 Complete**

### Task 4.2.3: Reconcile System Integration Scenarios

**Description:** Ensure handlers are exercised by matching performs, nested
builtin runners install all required handlers, and hefty values are asserted
using their public predicates.

- [x] **Task 4.2.3 Complete**

#### Subtask 4.2.3.1: Repair Orchestration Fixtures

**Description:** Remove no-op handler fixtures and invalid assumptions about
runner return shapes.

- [x] **Subtask 4.2.3.1 Complete**

#### Subtask 4.2.3.2: Apply Configuration Options

**Description:** Make the public `enable_hefty` option update the corresponding
system configuration field.

- [x] **Subtask 4.2.3.2 Complete**

**Acceptance Criteria:**

- Public handler scope operations round-trip without leaking stack entries
- Captured resumptions are opaque and all wrappers resume through one API
- Matching performs exercise deep, shallow, one-shot, and multi-shot handlers
- Higher-order and combined-effect integration scenarios use real public types

## Section 4.3: Type Helpers, Validation, and Verified Baseline

**Description:** Connect the public effect-type helpers to implemented
inference/generalization machinery, repair the theoretical validation fixture,
and publish the complete Phase 4 evidence.

- [x] **Section 4.3 Complete**

### Task 4.3.1: Connect Public Type Helpers

**Description:** Replace placeholder or nonexistent calls with the repository's
implemented effect inference state and polymorphic effect APIs.

- [x] **Task 4.3.1 Complete**

#### Subtask 4.3.1.1: Infer and Check Effects

**Description:** Infer expression effects from a fresh inference state and
return structured errors from the public boundary.

- [x] **Subtask 4.3.1.1 Complete**

#### Subtask 4.3.1.2: Generalize Effect Variables

**Description:** Quantify free effect variables while leaving concrete types
unchanged.

- [x] **Subtask 4.3.1.2 Complete**

### Task 4.3.2: Restore Validation Conformance

**Description:** Make theoretical handler-model validation use depth metadata
that matches the implemented shallow-handler contract.

- [x] **Task 4.3.2 Complete**

#### Subtask 4.3.2.1: Verify Aggregate Validation

**Description:** Require theoretical, property, and conformance reports to pass
together through the aggregate validation entry point.

- [x] **Subtask 4.3.2.1 Complete**

### Task 4.3.3: Publish Phase 4 Verification

**Description:** Run focused effect/type/runtime suites and two complete active
suite runs, then update promoted specifications and the failure inventory.

- [x] **Task 4.3.3 Complete**

#### Subtask 4.3.3.1: Record Deterministic Evidence

**Description:** Capture tool versions, commands, totals, and remaining
later-phase failures in a Phase 4 baseline document.

- [x] **Subtask 4.3.3.1 Complete**

#### Subtask 4.3.3.2: Reconcile Promoted Status

**Description:** Update current status and component specifications so they
describe both the verified boundary and its remaining continuation limits.

- [x] **Subtask 4.3.3.2 Complete**

**Acceptance Criteria:**

- Public effect inference and checking execute implemented inference code
- Effect-variable generalization uses the implemented polymorphism surface
- Aggregate algebraic-effects validation passes
- Focused Phase 4 tests pass and later-phase failures remain visible

## Phase Completion Gate

**Description:** Phase 4 completes when executable public effects, orchestration
resumptions, type helpers, validation, and promoted documentation agree at the
verified integration boundary.

- [x] Public effect execution matches builtin runner documentation
- [x] Handler and resumption orchestration has one tested observable contract
- [x] Type helpers and validation use implemented compiler machinery
- [x] Current-status and component specs match repeatable suite evidence
