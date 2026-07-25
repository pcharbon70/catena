# Phase 7: Shallow Handlers And Multi-Shot Resumptions

**Description:** This phase extends the proven deep one-shot boundary with
explicitly selected shallow handling and multi-shot resumptions. It settles
their source spellings, types, context restoration, branch-state policy,
resource limits, and executable semantics without weakening the one-shot
default or pretending to clone arbitrary external BEAM resources.

**Status:** Planned.

**Dependencies:** Phase 6 complete and deep one-shot behavior promoted at its
phase boundary.

## Section 7.1: Mode Surface And Static Semantics

**Description:** Accept a focused source decision for selecting handler depth
and resumption kind, then carry those modes through parsing, normalization,
typing, interfaces, and control-mode analysis.

- [ ] **Section 7.1 Complete**

### Task 7.1.1: Decide Shallow And Multi-Shot Source Spelling

**Description:** Create and accept the focused language decision deferred by
ADR-0006 without changing the existing deep one-shot defaults.

- [ ] **Task 7.1.1 Complete**

#### Subtask 7.1.1.1: Specify Mode Annotations

**Description:** Define unambiguous source forms for shallow handler depth and
multi-shot resumption kind, including defaults, nesting, formatting,
migration, and interaction with `with k`.

- [ ] **Subtask 7.1.1.1 Complete**

#### Subtask 7.1.1.2: Update Lexer, Parser, AST, And Normalization

**Description:** Implement the accepted spelling with identifier-boundary,
recovery, source-origin, pretty-printing, round-trip, and conflict-accounting
tests.

- [ ] **Subtask 7.1.1.2 Complete**

### Task 7.1.2: Extend Kinds, Types, And Mode Validation

**Description:** Infer the selected `ResumptionKind` and handler depth and
reject unsupported combinations before CPS lowering.

- [ ] **Task 7.1.2 Complete**

#### Subtask 7.1.2.1: Type Shallow Resumptions

**Description:** Model handler removal during resume, calculate residual
effect rows under shallow semantics, and diagnose operations that incorrectly
assume the shallow frame remains installed.

- [ ] **Subtask 7.1.2.1 Complete**

#### Subtask 7.1.2.2: Define Multi-Shot Admissibility

**Description:** Specify the static rule for residual effects, handler state,
external capabilities, resource-sensitive operations, and open rows before a
resumption may have kind `MultiShot`.

- [ ] **Subtask 7.1.2.2 Complete**

## Section 7.2: Shallow Handler Runtime Semantics

**Description:** Implement continuation invocation that removes the selected
handler frame before resumed execution while preserving parent contexts and
all process-affinity guarantees.

- [ ] **Section 7.2 Complete**

### Task 7.2.1: Implement Shallow Context Restoration

**Description:** Select the context outside the current handler frame when a
shallow resumption runs.

- [ ] **Task 7.2.1 Complete**

#### Subtask 7.2.1.1: Remove The Current Frame On Resume

**Description:** Restore the captured parent context, preserve unrelated inner
and outer frames, and make a repeated operation propagate to the next eligible
handler.

- [ ] **Subtask 7.2.1.1 Complete**

#### Subtask 7.2.1.2: Preserve Delimiter And Result Semantics

**Description:** Return the resumed computation to the correct delimiter,
preserve one-shot consumption, and handle nested shallow/deep combinations
without orphaning frames.

- [ ] **Subtask 7.2.1.2 Complete**

### Task 7.2.2: Integrate Shallow Modes With CPS And Core

**Description:** Carry depth metadata through control IR, calling conventions,
runtime dependencies, and source-oriented diagnostics.

- [ ] **Task 7.2.2 Complete**

#### Subtask 7.2.2.1: Lower Depth-Aware Control Nodes

**Description:** Emit explicit deep or shallow context selection and reject
any control node whose depth is missing, ambiguous, or unsupported.

- [ ] **Subtask 7.2.2.1 Complete**

#### Subtask 7.2.2.2: Preserve Depth Across Calls And Artifacts

**Description:** Retain depth through closures, imports, dictionaries,
resumption storage, versioned interfaces, and artifact compatibility checks.

- [ ] **Subtask 7.2.2.2 Complete**

## Section 7.3: Multi-Shot Runtime, Branching, And Resources

**Description:** Permit an admissible captured continuation to run more than
once with explicit branch semantics, bounded resources, and no claim that
external world state has been duplicated.

- [ ] **Section 7.3 Complete**

### Task 7.3.1: Implement Multi-Shot Invocation Authority

**Description:** Extend the runtime state machine to authorize repeated
invocations while isolating per-branch control metadata.

- [ ] **Task 7.3.1 Complete**

#### Subtask 7.3.1.1: Create Branch Execution State

**Description:** Assign branch identities, preserve the immutable continuation
environment, isolate delimiter execution state, and keep process ownership
checks on every invocation.

- [ ] **Subtask 7.3.1.1 Complete**

#### Subtask 7.3.1.2: Define Failure And Partial-Branch Behavior

**Description:** Specify whether one branch failure affects later branches,
how exceptions and aborts are reported, and how nested one-shot resumptions
inside a multi-shot branch are consumed.

- [ ] **Subtask 7.3.1.2 Complete**

### Task 7.3.2: Enforce Effect-State And Resource Policy

**Description:** Make the static admissibility decision and runtime resource
policy agree for stateful, external, concurrent, and open effects.

- [ ] **Task 7.3.2 Complete**

#### Subtask 7.3.2.1: Implement Branch State Semantics

**Description:** Define duplication or sharing for immutable lexical values
and each supported handler-state category, and reject PIDs, ports, mailboxes,
provider state, or capabilities that lack accepted branch semantics.

- [ ] **Subtask 7.3.2.1 Complete**

#### Subtask 7.3.2.2: Add Branch Resource Budgets

**Description:** Limit invocation count, retained continuation memory,
reductions, timeout, and nested branching and report deterministic budget
failures with source origin.

- [ ] **Subtask 7.3.2.2 Complete**

## Section 7.4: Phase 7 Integration Tests

**Description:** Prove shallow propagation and multi-shot branching from
Catena source through loaded BEAM, including mixed modes, rejected residual
effects, and nondeterministic control examples.

- [ ] **Section 7.4 Complete**

### Task 7.4.1: Execute Shallow And Multi-Shot Programs

**Description:** Compile, load, and run representative programs whose results
distinguish depth and continuation-kind semantics.

- [ ] **Task 7.4.1 Complete**

#### Subtask 7.4.1.1: Test Handler Depth

**Description:** Cover shallow propagation, deep rehandling, nested
deep/shallow orderings, shadowing, parent fallback, abort, exceptions, retained
resumptions, and process identity.

- [ ] **Subtask 7.4.1.1 Complete**

#### Subtask 7.4.1.2: Test Multi-Shot Branching

**Description:** Cover two and many invocations, different supplied values,
backtracking, nondeterministic solution enumeration, nested branching, branch
failure, limits, and deterministic results.

- [ ] **Subtask 7.4.1.2 Complete**

### Task 7.4.2: Execute Safety And Phase Gates

**Description:** Prove that inadmissible effects and unselected modes fail
closed and that deep one-shot remains the unchanged default.

- [ ] **Task 7.4.2 Complete**

#### Subtask 7.4.2.1: Test Negative Mode And Resource Paths

**Description:** Cover missing mode syntax, kind mismatch, shallow effect-row
errors, inadmissible external effects, open-row uncertainty, cross-process
resume, budget exhaustion, and artifact-version mismatch.

- [ ] **Subtask 7.4.2.1 Complete**

#### Subtask 7.4.2.2: Run Repository Gates

**Description:** Run Phase 7 loaded-BEAM integration tests, all deep one-shot
regressions, `make check-specs`, `make conformance`, and the complete active
EUnit suite and publish the exact phase-ending evidence.

- [ ] **Subtask 7.4.2.2 Complete**
