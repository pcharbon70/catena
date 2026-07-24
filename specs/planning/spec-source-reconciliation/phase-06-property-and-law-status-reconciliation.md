# Phase 6: Property and Law Status Reconciliation

**Description:** This phase reconciles Catena's executable property-testing
engine, first-class standard-library testing surface, and reusable trait-law
bridge with the promoted specifications. It closes the remaining generic law
runtime failures, makes unique collection cardinality deterministic, and
publishes a repeatable green-suite baseline without absorbing the executable
conformance and governance work owned by Phase 7.

**Status:** In progress.

**Dependencies:** Phase 5 complete.

## Section 6.1: Complete Runtime Scope and Generic Law Execution

**Description:** Make runtime evaluation of imported Catena transforms
declaration-order independent, then prove that the first-class generic law
surface reaches the internal discipline engine and reports unsupported
instances explicitly.

- [x] **Section 6.1 Complete**

### Task 6.1.1: Build a Complete Runtime Scope

**Description:** Construct type bindings and transform bindings as separate
runtime passes so every transform evaluates against the complete declaration
scope.

- [x] **Task 6.1.1 Complete**

#### Subtask 6.1.1.1: Separate Static and Callable Bindings

**Description:** Build constructors and base bindings first, then install
transform closures that recreate the complete callable scope when invoked.

- [x] **Subtask 6.1.1.1 Complete**

#### Subtask 6.1.1.2: Verify Forward References

**Description:** Add a focused runtime-runner regression in which an earlier
transform calls a later transform.

- [x] **Subtask 6.1.1.2 Complete**

### Task 6.1.2: Close Generic Law Runtime Failures

**Description:** Execute `verifyTrait`, `verifyTraits`, and law configuration
combinators through imported `Test` declarations and the reusable stdlib law
bridge.

- [x] **Task 6.1.2 Complete**

#### Subtask 6.1.2.1: Execute Supported Known Instances

**Description:** Confirm the compiled runtime path executes generator-backed
Mapper and Accumulator suites for supported `Maybe` and `List` instances.

- [x] **Subtask 6.1.2.1 Complete**

#### Subtask 6.1.2.2: Preserve Explicit Unsupported Results

**Description:** Confirm unsupported instance/trait combinations become
stable failed test results rather than undefined runtime variables.

- [x] **Subtask 6.1.2.2 Complete**

**Acceptance Criteria:**

- Runtime transform evaluation supports forward references
- Constructor and base-environment bindings remain available to all transforms
- `verifyTrait` and `verifyTraits` execute through imported stdlib declarations
- Supported known-instance law suites pass through the internal property engine
- Unsupported traits produce explicit, stable failed test results

## Section 6.2: Deterministic Unique Collection Generators

**Description:** Align map and set generator behavior with their requested
cardinality ranges by resampling collisions deterministically and reporting
unsatisfiable unique domains explicitly.

- [x] **Section 6.2 Complete**

### Task 6.2.1: Preserve Requested Root Cardinality

**Description:** Generate unique map keys and set elements until the selected
root size is reached instead of silently collapsing duplicate samples.

- [x] **Task 6.2.1 Complete**

#### Subtask 6.2.1.1: Resample Map Key Collisions

**Description:** Retry duplicate map keys with deterministic seed progression
while retaining the existing map shrink behavior.

- [x] **Subtask 6.2.1.1 Complete**

#### Subtask 6.2.1.2: Resample Set Element Collisions

**Description:** Apply the same unique-sampling contract to bounded set
generation.

- [x] **Subtask 6.2.1.2 Complete**

### Task 6.2.2: Make Exhaustion and Reproduction Explicit

**Description:** Bound collision retries, report generators whose domain
cannot satisfy the requested cardinality, and cover representative seeds.

- [x] **Task 6.2.2 Complete**

#### Subtask 6.2.2.1: Report Unique-Domain Exhaustion

**Description:** Raise a structured generator failure after a bounded number
of duplicate samples rather than hanging or returning an undersized root.

- [x] **Subtask 6.2.2.1 Complete**

#### Subtask 6.2.2.2: Add Deterministic Seed Sweeps

**Description:** Verify map and set range invariants across a repeatable set of
seeds, including seeds that previously exposed duplicate collapse.

- [x] **Subtask 6.2.2.2 Complete**

**Acceptance Criteria:**

- Map roots satisfy the selected size when the key domain is sufficient
- Set roots satisfy the selected size when the element domain is sufficient
- Duplicate sampling advances deterministically and cannot loop forever
- Unsatisfiable unique domains produce a structured generator failure
- Repeated seed sweeps reproduce the same passing cardinality results

## Section 6.3: Verified Property/Law Baseline and Promoted Status

**Description:** Run the complete focused property/law gate and two active
suite runs, then publish the implementation boundary, quality evidence, and
remaining Phase 7 work.

- [ ] **Section 6.3 Complete**

### Task 6.3.1: Verify Property and Law Integration

**Description:** Validate generators, shrinking, runners, adapters,
first-class properties, discipline laws, and stdlib law workflows together.

- [ ] **Task 6.3.1 Complete**

#### Subtask 6.3.1.1: Run the Focused Gate

**Description:** Execute all Phase 6 generator, property, runner, adapter,
trait-law, stdlib-law, convergence, and workflow modules.

- [ ] **Subtask 6.3.1.1 Complete**

#### Subtask 6.3.1.2: Run the Complete Suite Twice

**Description:** Confirm the active suite is green and its totals are stable
across two consecutive runs.

- [ ] **Subtask 6.3.1.2 Complete**

### Task 6.3.2: Publish Phase 6 Evidence

**Description:** Record commands, focused totals, complete-suite totals,
static-analysis status, resolved failures, and the honest remaining scope.

- [ ] **Task 6.3.2 Complete**

#### Subtask 6.3.2.1: Create the Test Baseline

**Description:** Supersede the Phase 5 quality snapshot with repeatable Phase 6
property/law evidence.

- [ ] **Subtask 6.3.2.1 Complete**

#### Subtask 6.3.2.2: Reconcile Promoted Status

**Description:** Update property-testing, staged law-verification, and current
status documents to match the verified implementation without claiming future
source-language or governance work.

- [ ] **Subtask 6.3.2.2 Complete**

**Acceptance Criteria:**

- Focused Phase 6 tests pass with no failures or skips
- Two complete-suite runs have identical green totals
- Static-analysis status remains visible
- All deterministic and flaky failures assigned to Phase 6 are resolved
- Specs distinguish implemented property/law execution from Phase 7 governance

## Phase Completion Gate

**Description:** Phase 6 completes when the stdlib law surface, internal
property engine, unique collection generators, promoted specifications, and
repeatable repository evidence agree at one executable boundary.

- [ ] Runtime transform scope is declaration-order independent
- [ ] Generic known-instance law suites execute through compiled stdlib values
- [ ] Unique collection generators satisfy deterministic cardinality contracts
- [ ] Focused and complete-suite evidence is green and repeatable
- [ ] Current status and property/law specifications match executable evidence
