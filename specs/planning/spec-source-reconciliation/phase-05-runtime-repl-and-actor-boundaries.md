# Phase 5: Runtime, REPL, and Actor Boundaries

**Description:** This phase reconciles Catena's BEAM-facing process façade,
implemented actor/OTP-style runtime components, and interactive effect
execution with the promoted runtime specifications. It replaces invalid
process-test assumptions with explicit public contracts, closes the REPL's
advertised Process-effect gap, distinguishes implemented Erlang runtime
components from future source-language actor syntax, and publishes a
deterministic Phase 5 baseline without absorbing property/law work owned by
Phase 6.

**Status:** In progress.

**Dependencies:** Phase 4 complete.

## Section 5.1: BEAM Process Primitive Contract

**Description:** Align the `catena_process` façade and its tests with
documented BEAM semantics while preserving a consistent Catena-facing result
shape for lifecycle operations.

- [x] **Section 5.1 Complete**

### Task 5.1.1: Normalize Lifecycle Operations

**Description:** Give link, monitor, registration, and exit mutations a stable
`ok` result instead of leaking the varying boolean results of Erlang BIFs.

- [x] **Task 5.1.1 Complete**

#### Subtask 5.1.1.1: Normalize Relationship Results

**Description:** Return `ok` from link, unlink, demonitor, and remote-exit
operations while retaining native exceptions for invalid targets.

- [x] **Subtask 5.1.1.1 Complete**

#### Subtask 5.1.1.2: Make Name Lifecycle Deterministic

**Description:** Report duplicate registration with a structured Catena error
and make unregistration safe after BEAM automatic name cleanup.

- [x] **Subtask 5.1.1.2 Complete**

### Task 5.1.2: Reconcile Messaging and Receive

**Description:** Preserve native asynchronous send behavior, expose the
implemented predicate receive API, and verify native OTP request/reply
delegation.

- [x] **Task 5.1.2 Complete**

#### Subtask 5.1.2.1: Document Dead-Target Send Semantics

**Description:** Treat sends to a dead PID as successful asynchronous sends
while failing name-based sends when the name cannot be resolved.

- [x] **Subtask 5.1.2.1 Complete**

#### Subtask 5.1.2.2: Preserve Rejected Receive Messages

**Description:** Export predicate receive and restore messages rejected by its
dynamic predicate instead of silently dropping them.

- [x] **Subtask 5.1.2.2 Complete**

#### Subtask 5.1.2.3: Validate Native OTP Calls

**Description:** Send the caller's request directly through `gen_server:call`
and retain structured no-process, timeout, and failure errors.

- [x] **Subtask 5.1.2.3 Complete**

### Task 5.1.3: Verify BEAM Signal Ownership

**Description:** Make process tests observe links, monitors, trapped exits, and
normal exits from the process that actually owns each relationship.

- [x] **Task 5.1.3 Complete**

#### Subtask 5.1.3.1: Synchronize Concurrent Fixtures

**Description:** Keep child processes alive until assertions complete and use
monitors or trapped exits rather than sleeps and unrelated mailboxes.

- [x] **Subtask 5.1.3.1 Complete**

**Acceptance Criteria:**

- Process lifecycle mutations have one documented public result shape
- PID and registered-name sends follow native asynchronous resolution rules
- Predicate receive is callable and does not discard rejected messages
- Link, monitor, trap-exit, and exit tests observe the correct owning process
- Native OTP calls pass requests through without protocol double-wrapping

## Section 5.2: Actor and Interactive Runtime Boundaries

**Description:** Promote the implemented actor and OTP-style runtime toolkit,
close the direct REPL Process-effect gap, and state clearly which
source-language and distributed actor features remain future work.

- [ ] **Section 5.2 Complete**

### Task 5.2.1: Promote the Actor Runtime Surface

**Description:** Publish a canonical actor-runtime specification covering the
process façade, actors, GenServer-style callbacks, supervisors, registries,
pub/sub, and event broadcasting.

- [ ] **Task 5.2.1 Complete**

#### Subtask 5.2.1.1: Define Implemented Runtime Components

**Description:** Anchor each promoted actor capability to its source module and
focused tests.

- [ ] **Subtask 5.2.1.1 Complete**

#### Subtask 5.2.1.2: Mark the Language Boundary

**Description:** Distinguish the implemented Erlang runtime library from
future Catena actor syntax, typed protocols, Process Workflow instances, and
distributed operation.

- [ ] **Subtask 5.2.1.2 Complete**

### Task 5.2.2: Complete Direct REPL Process Effects

**Description:** Make every Process operation declared by the current standard
effect surface executable through the REPL's advertised direct handlers.

- [ ] **Task 5.2.2 Complete**

#### Subtask 5.2.2.1: Handle Spawn, Send, and Self

**Description:** Delegate direct REPL Process operations to the reconciled
process façade and return the evaluator's standard `{ok, Value}` shape.

- [ ] **Subtask 5.2.2.1 Complete**

#### Subtask 5.2.2.2: Verify Interactive Process Execution

**Description:** Exercise identity, process creation, and message delivery
through `eval_with_effects` with automatic handler provision.

- [ ] **Subtask 5.2.2.2 Complete**

### Task 5.2.3: Reconcile Runtime Contracts

**Description:** Update runtime and REPL specifications so explicit-context
generated code, direct REPL evaluation, and Erlang actor components have
separate, non-conflicting authorities.

- [ ] **Task 5.2.3 Complete**

#### Subtask 5.2.3.1: Add Runtime Requirements

**Description:** Require BEAM-native process behavior, local actor lifecycle
contracts, and direct REPL Process-effect coverage.

- [ ] **Subtask 5.2.3.1 Complete**

**Acceptance Criteria:**

- The actor runtime has a canonical promoted specification
- Existing actor, supervisor, registry, pub/sub, and broadcaster suites pass
- REPL default handlers execute standard Process spawn, send, and self
- Specs do not imply unimplemented Catena actor syntax or distribution
- Runtime authorities remain explicit and compatible

## Section 5.3: Verified Runtime Baseline and Promoted Status

**Description:** Run the complete focused runtime/REPL/actor gate and two
active-suite runs, then publish the Phase 5 evidence and remaining Phase 6
inventory.

- [ ] **Section 5.3 Complete**

### Task 5.3.1: Verify Runtime Integration

**Description:** Validate process primitives, actor/OTP-style components, REPL
commands and state, and direct effect evaluation together.

- [ ] **Task 5.3.1 Complete**

#### Subtask 5.3.1.1: Run the Focused Gate

**Description:** Execute all Phase 5 process, actor, registry, pub/sub,
supervisor, broadcaster, REPL, history, completion, and workflow modules.

- [ ] **Subtask 5.3.1.1 Complete**

#### Subtask 5.3.1.2: Run the Complete Suite Twice

**Description:** Confirm the remaining failure inventory is deterministic and
contains only work assigned to Phase 6.

- [ ] **Subtask 5.3.1.2 Complete**

### Task 5.3.2: Publish Phase 5 Evidence

**Description:** Record tool versions, commands, focused totals, full-suite
totals, static-analysis status, and resolved/remaining failures.

- [ ] **Task 5.3.2 Complete**

#### Subtask 5.3.2.1: Create the Test Baseline

**Description:** Supersede the Phase 4 quality snapshot with repeatable Phase 5
runtime evidence.

- [ ] **Subtask 5.3.2.1 Complete**

#### Subtask 5.3.2.2: Reconcile Current Status

**Description:** Promote the implemented local actor toolkit while preserving
the explicit future status of source-language actors and distribution.

- [ ] **Subtask 5.3.2.2 Complete**

**Acceptance Criteria:**

- Focused Phase 5 tests pass with no failures or skips
- Two complete-suite runs have identical totals and failure inventory
- Static-analysis status is recorded without hiding repository-wide warnings
- All Phase 5 failures from the Phase 4 baseline are resolved
- Remaining failures are explicitly assigned to Phase 6

## Phase Completion Gate

**Description:** Phase 5 completes when process primitives, local actor
components, direct REPL Process effects, runtime specifications, and repeatable
test evidence agree at one honest integration boundary.

- [ ] Process primitive behavior matches BEAM and Catena façade contracts
- [ ] Actor and OTP-style runtime components have an explicit promoted scope
- [ ] REPL automatic handlers execute the declared standard Process surface
- [ ] Current status and runtime specs match repeatable Phase 5 evidence
