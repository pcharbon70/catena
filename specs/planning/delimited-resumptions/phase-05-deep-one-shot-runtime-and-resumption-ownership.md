# Phase 5: Deep One-Shot Runtime And Resumption Ownership

**Description:** This phase implements the opaque runtime authority required
to execute compiler-reified continuations safely. It adds process-affine
one-shot state, same-process resumable handler frames, deep context
restoration, retention-aware cleanup, and structured control failures without
executing source continuations on handler-provider processes.

**Status:** Planned.

**Dependencies:** Phase 4 complete.

## Section 5.1: Opaque Resumption Representation And State

**Description:** Introduce a versioned runtime representation that cannot be
forged by Catena source and that atomically enforces ownership, liveness, and
one-shot consumption.

- [ ] **Section 5.1 Complete**

### Task 5.1.1: Define The Versioned Runtime Contract

**Description:** Specify and implement the opaque public handle and private
state for a captured continuation.

- [ ] **Task 5.1.1 Complete**

#### Subtask 5.1.1.1: Define Public And Private Shapes

**Description:** Expose only a versioned opaque handle while privately
retaining continuation closure, captured context, delimiter, depth, kind,
owner PID, state authority, source origin, and runtime metadata.

- [ ] **Subtask 5.1.1.1 Complete**

#### Subtask 5.1.1.2: Validate Construction Authority

**Description:** Permit construction only from compiler-generated capture,
reject malformed or stale handles, and prevent source-visible inspection from
revealing closure or context internals.

- [ ] **Subtask 5.1.1.2 Complete**

### Task 5.1.2: Implement One-Shot State Transitions

**Description:** Make `fresh -> running -> consumed` atomic and authoritative
across every normal, exceptional, timeout, and re-entrant path.

- [ ] **Task 5.1.2 Complete**

#### Subtask 5.1.2.1: Authorize First Invocation

**Description:** Check owner, kind, version, delimiter liveness, and current
state before granting the capturing process permission to invoke the closure.

- [ ] **Subtask 5.1.2.1 Complete**

#### Subtask 5.1.2.2: Consume On Every Exit

**Description:** Mark a one-shot resumption consumed after success, exception,
runtime failure, or timeout and reject second and re-entrant invocations
deterministically.

- [ ] **Subtask 5.1.2.2 Complete**

## Section 5.2: Explicit Handler Frames And Same-Process Resume

**Description:** Extend explicit effect contexts to distinguish local
resumable source handlers from process-backed builtin or external providers.

- [ ] **Section 5.2 Complete**

### Task 5.2.1: Add Resumable Handler Frames To Contexts

**Description:** Represent source handler cases and delimiters as explicit
same-process context entries with deterministic nested lookup.

- [ ] **Task 5.2.1 Complete**

#### Subtask 5.2.1.1: Define Context Entry Kinds

**Description:** Distinguish local resumable frames, current value-provider
frames, and process-backed providers without changing effect identity or
parent-context lookup semantics.

- [ ] **Subtask 5.2.1.1 Complete**

#### Subtask 5.2.1.2: Preserve Nested Lookup And Shadowing

**Description:** Resolve the innermost compatible frame, retain parent
fallback, validate operation identity and arity, and preserve deterministic
shadowing.

- [ ] **Subtask 5.2.1.2 Complete**

### Task 5.2.2: Execute Deep Resumptions On The Owner Process

**Description:** Invoke the compiler-provided continuation in the capturing
process with the handler frame reinstalled around resumed execution.

- [ ] **Task 5.2.2 Complete**

#### Subtask 5.2.2.1: Restore Deep Context

**Description:** Select the captured deep context, provide the operation
result, execute to the matching delimiter, and return the delimiter result to
the handler body.

- [ ] **Subtask 5.2.2.1 Complete**

#### Subtask 5.2.2.2: Preserve BEAM Process Semantics

**Description:** Verify `self`, mailbox ownership, links, monitors, exception
ownership, and process-local behavior remain those of the capturing process,
even when a provider process computes an operation result.

- [ ] **Subtask 5.2.2.2 Complete**

## Section 5.3: Retention, Cleanup, And Runtime Diagnostics

**Description:** Make first-class retained resumptions safe enough to outlive
their immediate operation case without prematurely destroying handler
authority or leaking it after owner death.

- [ ] **Section 5.3 Complete**

### Task 5.3.1: Implement Resumption Lifetime Management

**Description:** Retain required delimiter and context resources while a
resumption remains valid and release runtime authority when it is consumed or
its owner exits.

- [ ] **Task 5.3.1 Complete**

#### Subtask 5.3.1.1: Track Runtime Leases

**Description:** Add explicit lease or ownership records for retained frames,
avoid process-dictionary authority, and make cleanup idempotent across nested
and returned resumptions.

- [ ] **Subtask 5.3.1.1 Complete**

#### Subtask 5.3.1.2: Monitor Owner And Provider Lifecycles

**Description:** Release retained state on owner death and specify failures
when a required provider, delimiter, or handler frame expires before resume.

- [ ] **Subtask 5.3.1.2 Complete**

### Task 5.3.2: Normalize Runtime Control Failures

**Description:** Return stable source-oriented failures without exposing
private handles, raw messages, or internal continuation closure terms.

- [ ] **Task 5.3.2 Complete**

#### Subtask 5.3.2.1: Implement Ownership And Consumption Errors

**Description:** Cover wrong owner, double resume, re-entrant resume, malformed
handle, stale version, wrong kind, and consumed state with deterministic
payloads.

- [ ] **Subtask 5.3.2.1 Complete**

#### Subtask 5.3.2.2: Implement Lifetime And Context Errors

**Description:** Cover expired delimiters, missing frames, unavailable
providers, timeout, handler failure, owner death, and cleanup failure while
preserving source origins.

- [ ] **Subtask 5.3.2.2 Complete**

## Section 5.4: Phase 5 Integration Tests

**Description:** Prove with real closures and BEAM processes that deep
one-shot resume executes the intended continuation on its owner exactly once
and that every lifecycle failure cleans up deterministically.

- [ ] **Section 5.4 Complete**

### Task 5.4.1: Exercise Runtime Control And Lifecycle

**Description:** Run compiler-shaped CPS fixtures through context lookup,
capture, handler execution, resume, delimiter return, retention, and cleanup.

- [ ] **Task 5.4.1 Complete**

#### Subtask 5.4.1.1: Test Positive Deep One-Shot Paths

**Description:** Cover explicit resume, auto-resume, transformed results,
nested handlers, sequential operations, returned resumptions, delayed
same-process invocation, builtin providers, and owner process identity.

- [ ] **Subtask 5.4.1.1 Complete**

#### Subtask 5.4.1.2: Test Negative Runtime Paths

**Description:** Cover double and re-entrant resume, cross-process invocation,
stale versions, expired frames, provider death, owner death, exceptions,
timeouts, and idempotent cleanup.

- [ ] **Subtask 5.4.1.2 Complete**

### Task 5.4.2: Run Phase Completion Gates

**Description:** Establish the runtime as a faithful target for validated CPS
IR without yet claiming complete source-to-BEAM integration.

- [ ] **Task 5.4.2 Complete**

#### Subtask 5.4.2.1: Run Runtime And Compatibility Regressions

**Description:** Run effect-context, handler, IO, Process, timeout, cleanup,
actor, REPL, resumption-helper, and backend-runtime suites and verify current
request/response behavior remains compatible.

- [ ] **Subtask 5.4.2.1 Complete**

#### Subtask 5.4.2.2: Run Repository Gates

**Description:** Run Phase 5 integration tests, `make check-specs`, and the
complete active EUnit suite and publish the exact phase-ending evidence.

- [ ] **Subtask 5.4.2.2 Complete**
