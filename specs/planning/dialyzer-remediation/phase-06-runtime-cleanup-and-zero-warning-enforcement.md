# Phase 6: Runtime Cleanup and Zero-Warning Enforcement

**Description:** This phase clears the remaining runtime, REPL, and stdlib
findings, burns down any cross-area residuals, and promotes zero Dialyzer
warnings into the maintained local and CI quality contract.

**Status:** Planned.

**Dependencies:** Phase 5 complete.

## Section 6.1: Runtime, REPL, And Standard Library

**Description:** Reconcile process/actor/OTP-style runtime results, interactive
evaluation state, completion/history behavior, and prelude contracts.

- [ ] **Section 6.1 Complete**

### Task 6.1.1: Repair Runtime Contracts

**Description:** Align process lifecycle, actor calls/casts, GenServer
callbacks, supervisors, registration, pub/sub, and event fan-out.

- [ ] **Task 6.1.1 Complete**

#### Subtask 6.1.1.1: Specify Lifecycle Outcomes

**Description:** Give spawn, send, receive, monitor, link, stop, timeout, and
cleanup paths explicit public result types.

- [ ] **Subtask 6.1.1.1 Complete**

#### Subtask 6.1.1.2: Reconcile Callback Returns

**Description:** Make actor, server, and supervisor callbacks agree with their
behaviors while retaining tested error and shutdown paths.

- [ ] **Subtask 6.1.1.2 Complete**

### Task 6.1.2: Repair REPL And Prelude Contracts

**Description:** Align command parsing, completion, evaluation, effect
execution, session state, and standard-library helper results.

- [ ] **Task 6.1.2 Complete**

#### Subtask 6.1.2.1: Reconcile Interactive Results

**Description:** Preserve syntax, type, runtime, command, and incomplete-input
errors while removing impossible catch-all branches.

- [ ] **Subtask 6.1.2.1 Complete**

#### Subtask 6.1.2.2: Reconcile Prelude Calls

**Description:** Tighten prelude specs and callbacks around the values
exercised by compiler, runtime, REPL, and law integration tests.

- [ ] **Subtask 6.1.2.2 Complete**

## Section 6.2: Zero-Warning Quality Boundary

**Description:** Resolve all remaining cross-area findings, make zero warnings
machine-enforced, and publish the repeatable final baseline.

- [ ] **Section 6.2 Complete**

### Task 6.2.1: Burn Down Residual Findings

**Description:** Classify every remaining warning by root cause and resolve it
without suppressions or weakened configuration.

- [ ] **Task 6.2.1 Complete**

#### Subtask 6.2.1.1: Require Empty Inventory

**Description:** Fail the maintained inventory command if any warning record
remains after a fresh Dialyzer run.

- [ ] **Subtask 6.2.1.1 Complete**

#### Subtask 6.2.1.2: Audit Configuration Integrity

**Description:** Verify that `unmatched_returns`, `error_handling`,
`underspecs`, and `unknown` remain enabled and no broad module suppression was
introduced.

- [ ] **Subtask 6.2.1.2 Complete**

### Task 6.2.2: Enforce And Publish The Boundary

**Description:** Add the zero-warning command to CI and reconcile current
status, targets, tooling, and final evidence.

- [ ] **Task 6.2.2 Complete**

#### Subtask 6.2.2.1: Add Static Analysis To CI

**Description:** Run the canonical Dialyzer gate with a correctly keyed PLT
cache and read-only repository permissions.

- [ ] **Subtask 6.2.2.1 Complete**

#### Subtask 6.2.2.2: Publish The Final Baseline

**Description:** Record zero warnings, two identical complete-suite runs,
specs governance, conformance, compile, tool versions, and CI evidence.

- [ ] **Subtask 6.2.2.2 Complete**

**Acceptance Criteria:**

- `rebar3 dialyzer` reports zero warnings on a clean analysis
- All originally enabled warning categories remain enabled
- No broad suppression or generated-source edit is used
- Two consecutive complete EUnit runs have identical green totals
- `make check-specs`, `make conformance`, and `make compile` pass
- CI enforces static analysis with read-only permissions
- Promoted status and tooling specs publish the final zero-warning baseline
