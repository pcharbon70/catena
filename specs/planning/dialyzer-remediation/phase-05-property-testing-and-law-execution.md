# Phase 5: Property Testing and Law Execution

**Description:** This phase clears the property-testing engine and testing
bridge inventory while preserving deterministic generation, shrinking,
reporting, stateful/BEAM helpers, and compiled standard-library law execution.

**Status:** Planned.

**Dependencies:** Phase 4 complete.

## Section 5.1: Generators, Shrinking, And Runner

**Description:** Reconcile generator trees, seeds, sizes, shrinking outcomes,
property configurations, and runner results at the core execution boundary.

- [ ] **Section 5.1 Complete**

### Task 5.1.1: Repair Generator Contracts

**Description:** Align generator function shapes, rose trees, collection
exhaustion, combinators, and shrink streams with their concrete values.

- [ ] **Task 5.1.1 Complete**

#### Subtask 5.1.1.1: Specify Generator And Tree Types

**Description:** Replace broad callable and tree terms with shared recursive
types that cover generation and shrinking without overspecification.

- [ ] **Subtask 5.1.1.1 Complete**

#### Subtask 5.1.1.2: Preserve Deterministic Failure Paths

**Description:** Test invalid ranges, exhausted unique domains, discarded
properties, and shrink termination before resolving no-return warnings.

- [ ] **Subtask 5.1.1.2 Complete**

### Task 5.1.2: Repair Runner Contracts

**Description:** Standardize single-property, grouped, batch, timeout,
discarded, failed, and error result shapes.

- [ ] **Task 5.1.2 Complete**

#### Subtask 5.1.2.1: Align Configuration Maps

**Description:** Give seeds, counts, sizes, shrinking limits, timeouts, and
parallel options one validated configuration type.

- [ ] **Subtask 5.1.2.1 Complete**

#### Subtask 5.1.2.2: Honor Return Values

**Description:** Resolve ignored-result findings by consuming, propagating, or
explicitly documenting meaningful runner and test outcomes.

- [ ] **Subtask 5.1.2.2 Complete**

## Section 5.2: Reporting, Discipline, And Laws

**Description:** Align failure reports, counterexamples, law metadata,
discipline composition, and suite results across structural and executable law
paths.

- [ ] **Section 5.2 Complete**

### Task 5.2.1: Repair Reporting Flow

**Description:** Reconcile text, JSON, JUnit, counterexample, label, and
summary formatters with the runner result types from Section 5.1.

- [ ] **Task 5.2.1 Complete**

#### Subtask 5.2.1.1: Specify Report Inputs

**Description:** Use shared result and counterexample types instead of
formatter-specific approximations.

- [ ] **Subtask 5.2.1.1 Complete**

#### Subtask 5.2.1.2: Preserve Actionable Failures

**Description:** Verify thrown predicates, invalid generators, shrink limits,
and law failures remain visible in every maintained formatter.

- [ ] **Subtask 5.2.1.2 Complete**

### Task 5.2.2: Repair Discipline And Law Contracts

**Description:** Reconcile trait laws, inherited disciplines, parameter
records, generated tests, and suite summaries.

- [ ] **Task 5.2.2 Complete**

#### Subtask 5.2.2.1: Align Law Parameter Types

**Description:** Make adapters, generators, functions, metadata, and inherited
requirements agree on one law-parameter representation.

- [ ] **Subtask 5.2.2.1 Complete**

#### Subtask 5.2.2.2: Remove Law No-Return Cascades

**Description:** Correct root call contracts so law helpers and focused EUnit
tests no longer appear unreachable or unable to return.

- [ ] **Subtask 5.2.2.2 Complete**

## Section 5.3: Stateful Helpers And Compatibility Bridge

**Description:** Clear state-machine, process/message/concurrency/distribution
helpers and the Catena-facing testing compatibility layer.

- [ ] **Section 5.3 Complete**

### Task 5.3.1: Repair Stateful And BEAM Helper Contracts

**Description:** Align command generation, symbolic/concrete state,
preconditions, postconditions, process results, and simplified distributed
outcomes.

- [ ] **Task 5.3.1 Complete**

#### Subtask 5.3.1.1: Reconcile Stateful Results

**Description:** Preserve placeholder-backed boundaries explicitly while
removing impossible or untyped result variants.

- [ ] **Subtask 5.3.1.1 Complete**

#### Subtask 5.3.1.2: Reconcile Concurrency Results

**Description:** Test successful, timed-out, crashed, and unsupported paths
before tightening process and distribution specs.

- [ ] **Subtask 5.3.1.2 Complete**

### Task 5.3.2: Repair Testing Adapters

**Description:** Make first-class properties, legacy declarations, compiled
test execution, and stdlib law bridges converge on the internal runner types.

- [ ] **Task 5.3.2 Complete**

#### Subtask 5.3.2.1: Align Adapter Boundaries

**Description:** Translate compatibility inputs once and remove downstream
catch-all clauses made unreachable by canonical internal results.

- [ ] **Subtask 5.3.2.1 Complete**

#### Subtask 5.3.2.2: Run The Complete Property/Law Gate

**Description:** Require all 53 focused Phase 6 modules plus workflow and
compiled-law integration tests to pass before declaring the areas clean.

- [ ] **Subtask 5.3.2.2 Complete**

**Acceptance Criteria:**

- `src/proptest` and `src/testing` have no remaining Dialyzer warnings
- Deterministic seeds and unique-collection semantics remain stable
- Shrinking and reporting retain actionable counterexamples
- Stateful and BEAM placeholder boundaries remain explicit
- Generic and compiled stdlib law suites pass
- `make verify` remains green
- The phase publishes its exact ending warning count
