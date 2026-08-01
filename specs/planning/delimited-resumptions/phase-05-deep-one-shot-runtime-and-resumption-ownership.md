# Phase 5: Deep One-Shot Runtime And Resumption Ownership

**Description:** This phase implements the opaque runtime authority required
to execute compiler-reified continuations safely. It adds process-affine
one-shot state, same-process resumable handler frames, deep context
restoration, retention-aware cleanup, and structured control failures without
executing source continuations on handler-provider processes.

**Status:** Complete.

**Dependencies:** Phase 4 complete.

## Section 5.1: Opaque Resumption Representation And State

**Description:** Introduce a versioned runtime representation that cannot be
forged by Catena source and that atomically enforces ownership, liveness, and
one-shot consumption.

- [x] **Section 5.1 Complete**

### Task 5.1.1: Define The Versioned Runtime Contract

**Description:** Specify and implement the opaque public handle and private
state for a captured continuation.

- [x] **Task 5.1.1 Complete**

#### Subtask 5.1.1.1: Define Public And Private Shapes

**Description:** Expose only a versioned opaque handle while privately
retaining continuation closure, captured context, delimiter, depth, kind,
owner PID, state authority, source origin, and runtime metadata.

- [x] **Subtask 5.1.1.1 Complete**

#### Subtask 5.1.1.2: Validate Construction Authority

**Description:** Permit construction only from compiler-generated capture,
reject malformed or stale handles, and prevent source-visible inspection from
revealing closure or context internals.

- [x] **Subtask 5.1.1.2 Complete**

### Task 5.1.2: Implement One-Shot State Transitions

**Description:** Make `fresh -> running -> consumed` atomic and authoritative
across every normal, exceptional, timeout, and re-entrant path.

- [x] **Task 5.1.2 Complete**

#### Subtask 5.1.2.1: Authorize First Invocation

**Description:** Check owner, kind, version, delimiter liveness, and current
state before granting the capturing process permission to invoke the closure.

- [x] **Subtask 5.1.2.1 Complete**

#### Subtask 5.1.2.2: Consume On Every Exit

**Description:** Mark a one-shot resumption consumed after success, exception,
runtime failure, or timeout and reject second and re-entrant invocations
deterministically.

- [x] **Subtask 5.1.2.2 Complete**

**Implementation evidence:** `catena_resumption_runtime` exposes only a
versioned opaque reference capability while retaining continuation, explicit
context, delimiter, depth, kind, owner, type identity, origin, metadata, and
state authority in a private runtime registry. The registry atomically
authorizes `fresh -> running`, rejects cross-process, re-entrant, consumed,
malformed, unregistered, stale-version, and unsupported-mode invocations, and
removes executable state during the unconditional `running -> consumed`
transition after normal or exceptional completion. Continuations execute on
the capturing process and the inspection surface reveals only the one-shot
state. The focused runtime suite passes 9 tests.

## Section 5.2: Explicit Handler Frames And Same-Process Resume

**Description:** Extend explicit effect contexts to distinguish local
resumable source handlers from process-backed builtin or external providers.

- [x] **Section 5.2 Complete**

### Task 5.2.1: Add Resumable Handler Frames To Contexts

**Description:** Represent source handler cases and delimiters as explicit
same-process context entries with deterministic nested lookup.

- [x] **Task 5.2.1 Complete**

#### Subtask 5.2.1.1: Define Context Entry Kinds

**Description:** Distinguish local resumable frames, current value-provider
frames, and process-backed providers without changing effect identity or
parent-context lookup semantics.

- [x] **Subtask 5.2.1.1 Complete**

#### Subtask 5.2.1.2: Preserve Nested Lookup And Shadowing

**Description:** Resolve the innermost compatible frame, retain parent
fallback, validate operation identity and arity, and preserve deterministic
shadowing.

- [x] **Subtask 5.2.1.2 Complete**

### Task 5.2.2: Execute Deep Resumptions On The Owner Process

**Description:** Invoke the compiler-provided continuation in the capturing
process with the handler frame reinstalled around resumed execution.

- [x] **Task 5.2.2 Complete**

#### Subtask 5.2.2.1: Restore Deep Context

**Description:** Select the captured deep context, provide the operation
result, execute to the matching delimiter, and return the delimiter result to
the handler body.

- [x] **Subtask 5.2.2.1 Complete**

#### Subtask 5.2.2.2: Preserve BEAM Process Semantics

**Description:** Verify `self`, mailbox ownership, links, monitors, exception
ownership, and process-local behavior remain those of the capturing process,
even when a provider process computes an operation result.

- [x] **Subtask 5.2.2.2 Complete**

**Implementation evidence:** `catena_effect_runtime` now carries explicit
`local_resumable`, `local_value_provider`, and `process_provider` entries at
each context level. Operation lookup is innermost-first and validates effect,
operation, and arity while retaining parent fallback and the existing
process-provider ownership of request/response errors. `perform_cps/5`
captures real compiler-shaped closures for local cases, auto-resumes value
cases, and passes the retained deep context back into the continuation.
Control handlers may return or invoke the opaque resumption, and the
delimiter result becomes the value of `resume`. Builtin, local-value, and
process-backed providers compute only operation results; continuation code
then runs on the capturing process. The focused context/runtime suites pass
57 tests, including nested shadowing, deep delayed resume, builtin identity,
and provider-process separation.

## Section 5.3: Retention, Cleanup, And Runtime Diagnostics

**Description:** Make first-class retained resumptions safe enough to outlive
their immediate operation case without prematurely destroying handler
authority or leaking it after owner death.

- [x] **Section 5.3 Complete**

### Task 5.3.1: Implement Resumption Lifetime Management

**Description:** Retain required delimiter and context resources while a
resumption remains valid and release runtime authority when it is consumed or
its owner exits.

- [x] **Task 5.3.1 Complete**

#### Subtask 5.3.1.1: Track Runtime Leases

**Description:** Add explicit lease or ownership records for retained frames,
avoid process-dictionary authority, and make cleanup idempotent across nested
and returned resumptions.

- [x] **Subtask 5.3.1.1 Complete**

#### Subtask 5.3.1.2: Monitor Owner And Provider Lifecycles

**Description:** Release retained state on owner death and specify failures
when a required provider, delimiter, or handler frame expires before resume.

- [x] **Subtask 5.3.1.2 Complete**

### Task 5.3.2: Normalize Runtime Control Failures

**Description:** Return stable source-oriented failures without exposing
private handles, raw messages, or internal continuation closure terms.

- [x] **Task 5.3.2 Complete**

#### Subtask 5.3.2.1: Implement Ownership And Consumption Errors

**Description:** Cover wrong owner, double resume, re-entrant resume, malformed
handle, stale version, wrong kind, and consumed state with deterministic
payloads.

- [x] **Subtask 5.3.2.1 Complete**

#### Subtask 5.3.2.2: Implement Lifetime And Context Errors

**Description:** Cover expired delimiters, missing frames, unavailable
providers, timeout, handler failure, owner death, and cleanup failure while
preserving source origins.

- [x] **Subtask 5.3.2.2 Complete**

**Implementation evidence:** Each registered resumption now owns an explicit
frame/delimiter lease plus runtime monitors for its capturing owner and every
required process-backed provider in the retained context. Returning a handle
keeps the lease active; normal or exceptional resume, timeout, abandonment,
explicit delimiter expiry, provider death, or owner death releases private
continuation/context authority and monitor resources idempotently. Control
cases that neither resume nor return their authority are discarded
automatically. Cooperative deadlines remain in the owner process and bound
provider waits without moving the continuation to another process. Stable
failure maps cover invalid and stale handles, wrong/dead owners, re-entry,
consumption, expired delimiters, unavailable providers, timeouts, handler
exceptions, and cleanup during execution while retaining source origin and
excluding PIDs, references, closures, contexts, raw messages, and stacks. The
focused lifecycle suite passes 10 tests; the combined runtime/context suites
pass 67 tests.

## Section 5.4: Phase 5 Integration Tests

**Description:** Prove with real closures and BEAM processes that deep
one-shot resume executes the intended continuation on its owner exactly once
and that every lifecycle failure cleans up deterministically.

- [x] **Section 5.4 Complete**

### Task 5.4.1: Exercise Runtime Control And Lifecycle

**Description:** Run compiler-shaped CPS fixtures through context lookup,
capture, handler execution, resume, delimiter return, retention, and cleanup.

- [x] **Task 5.4.1 Complete**

#### Subtask 5.4.1.1: Test Positive Deep One-Shot Paths

**Description:** Cover explicit resume, auto-resume, transformed results,
nested handlers, sequential operations, returned resumptions, delayed
same-process invocation, builtin providers, and owner process identity.

- [x] **Subtask 5.4.1.1 Complete**

#### Subtask 5.4.1.2: Test Negative Runtime Paths

**Description:** Cover double and re-entrant resume, cross-process invocation,
stale versions, expired frames, provider death, owner death, exceptions,
timeouts, and idempotent cleanup.

- [x] **Subtask 5.4.1.2 Complete**

### Task 5.4.2: Run Phase Completion Gates

**Description:** Establish the runtime as a faithful target for validated CPS
IR without yet claiming complete source-to-BEAM integration.

- [x] **Task 5.4.2 Complete**

#### Subtask 5.4.2.1: Run Runtime And Compatibility Regressions

**Description:** Run effect-context, handler, IO, Process, timeout, cleanup,
actor, REPL, resumption-helper, and backend-runtime suites and verify current
request/response behavior remains compatible.

- [x] **Subtask 5.4.2.1 Complete**

#### Subtask 5.4.2.2: Run Repository Gates

**Description:** Run Phase 5 integration tests, `make check-specs`, and the
complete active EUnit suite and publish the exact phase-ending evidence.

- [x] **Subtask 5.4.2.2 Complete**

**Implementation evidence:** The 12-test
`catena_delimited_resumption_phase5_tests` suite runs compiler-shaped CPS
closures through the production explicit-context and resumption runtimes. It
covers explicit and automatic resume, transformed delimiter results,
sequential and nested operations, shadowing and parent fallback, retained
deep invocation, builtin and process-backed providers, and preservation of
owner identity, mailbox, links, and monitors. Negative cases cover malformed
and stale versions, wrong/dead owners, double and re-entrant resume, expired
frames, provider death, handler exceptions, timeouts, and idempotent cleanup.
The runtime-focused suites pass 79 tests. `make check-specs` passes with 293
validated local links, and the complete active EUnit suite passes 5,233 tests
with zero failures or skips. This establishes the production runtime as the
faithful target for Phase 4 CPS IR; Phase 6 still owns Core lowering and
source-to-loaded-BEAM evidence.
