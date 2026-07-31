# Phase 7: Shallow Handlers And Multi-Shot Resumptions

**Description:** This phase extends the proven deep one-shot boundary with
explicitly selected shallow handling and multi-shot resumptions. It settles
their source spellings, types, context restoration, branch-state policy,
resource limits, and executable semantics without weakening the one-shot
default or pretending to clone arbitrary external BEAM resources.

**Status:** Complete.

**Dependencies:** Phase 6 complete and deep one-shot behavior promoted at its
phase boundary.

## Section 7.1: Mode Surface And Static Semantics

**Description:** Accept a focused source decision for selecting handler depth
and resumption kind, then carry those modes through parsing, normalization,
typing, interfaces, and control-mode analysis.

- [x] **Section 7.1 Complete**

### Task 7.1.1: Decide Shallow And Multi-Shot Source Spelling

**Description:** Create and accept the focused language decision deferred by
ADR-0006 without changing the existing deep one-shot defaults.

- [x] **Task 7.1.1 Complete**

#### Subtask 7.1.1.1: Specify Mode Annotations

**Description:** Define unambiguous source forms for shallow handler depth and
multi-shot resumption kind, including defaults, nesting, formatting,
migration, and interaction with `with k`.

- [x] **Subtask 7.1.1.1 Complete**

#### Subtask 7.1.1.2: Update Lexer, Parser, AST, And Normalization

**Description:** Implement the accepted spelling with identifier-boundary,
recovery, source-origin, pretty-printing, round-trip, and conflict-accounting
tests.

- [x] **Subtask 7.1.1.2 Complete**

### Task 7.1.2: Extend Kinds, Types, And Mode Validation

**Description:** Infer the selected `ResumptionKind` and handler depth and
reject unsupported combinations before CPS lowering.

- [x] **Task 7.1.2 Complete**

#### Subtask 7.1.2.1: Type Shallow Resumptions

**Description:** Model handler removal during resume, calculate residual
effect rows under shallow semantics, and diagnose operations that incorrectly
assume the shallow frame remains installed.

- [x] **Subtask 7.1.2.1 Complete**

#### Subtask 7.1.2.2: Define Multi-Shot Admissibility

**Description:** Specify the static rule for residual effects, handler state,
external capabilities, resource-sensitive operations, and open rows before a
resumption may have kind `MultiShot`.

- [x] **Subtask 7.1.2.2 Complete**

### Section 7.1 Evidence

[ADR-0007](../../adr/ADR-0007-explicit-handler-and-resumption-mode-modifiers.md)
accepts `handle shallow multi_shot` as the canonical delimiter-local mode
surface while retaining implicit deep one-shot defaults. The lexer and parser
reserve both modifiers, accept either order, preserve source origins, and
retain the existing 38 shift/reduce and zero reduce/reduce conflict baseline.

`catena_resumption_mode` carries the normalized mode through inference,
control-mode inventories, selective-CPS structural handoff, module interface
version 3, and source formatting. Shallow resumptions retain the selected
effect label in their residual row. Multi-shot resumptions are admitted only
for closed empty residual rows; known external/stateful effects and open rows
produce stable fail-closed diagnostics. The six focused
`catena_delimited_resumption_phase7_static_tests` cover syntax round trips,
normalization, kinds, effects, rejection, control inventories, and published
interface metadata.

## Section 7.2: Shallow Handler Runtime Semantics

**Description:** Implement continuation invocation that removes the selected
handler frame before resumed execution while preserving parent contexts and
all process-affinity guarantees.

- [x] **Section 7.2 Complete**

### Task 7.2.1: Implement Shallow Context Restoration

**Description:** Select the context outside the current handler frame when a
shallow resumption runs.

- [x] **Task 7.2.1 Complete**

#### Subtask 7.2.1.1: Remove The Current Frame On Resume

**Description:** Restore the captured parent context, preserve unrelated inner
and outer frames, and make a repeated operation propagate to the next eligible
handler.

- [x] **Subtask 7.2.1.1 Complete**

#### Subtask 7.2.1.2: Preserve Delimiter And Result Semantics

**Description:** Return the resumed computation to the correct delimiter,
preserve one-shot consumption, and handle nested shallow/deep combinations
without orphaning frames.

- [x] **Subtask 7.2.1.2 Complete**

### Task 7.2.2: Integrate Shallow Modes With CPS And Core

**Description:** Carry depth metadata through control IR, calling conventions,
runtime dependencies, and source-oriented diagnostics.

- [x] **Task 7.2.2 Complete**

#### Subtask 7.2.2.1: Lower Depth-Aware Control Nodes

**Description:** Emit explicit deep or shallow context selection and reject
any control node whose depth is missing, ambiguous, or unsupported.

- [x] **Subtask 7.2.2.1 Complete**

#### Subtask 7.2.2.2: Preserve Depth Across Calls And Artifacts

**Description:** Retain depth through closures, imports, dictionaries,
resumption storage, versioned interfaces, and artifact compatibility checks.

- [x] **Subtask 7.2.2.2 Complete**

### Section 7.2 Evidence

The explicit-context runtime records both the context containing the selected
handler and that frame's parent. A deep one-shot resumption restores the
former; a shallow one-shot resumption restores the latter. This removes only
the selected shallow frame, preserves unrelated parent frames and the
delimiter result, and retains the existing owner-process, lifecycle, timeout,
retention, and one-shot-consumption rules.

Selective-CPS Core lowering now places `depth` and `resumption_kind` in every
generated handler specification. Control validation rejects missing modes and
mode disagreement among delimiter, installation, and resumption nodes.
Control ABI version 2, resumption/effect runtime version 2, artifact format 3,
module interface version 3, exact runtime feature dependencies, and artifact
`handler_modes` retain the selection across compilation and loading.

`catena_delimited_resumption_phase7_shallow_tests` distinguishes shallow
`inner -> outer` behavior from deep `inner -> inner` rehandling, preserves an
unrelated intervening frame and process identity, proves retained shallow
one-shot consumption, and compiles, validates, loads, and executes a nested
shallow Catena program with result `11`. Focused runtime, validation, Core,
artifact, dependency, conformance, and public API regressions pass with the
unchanged 38 shift/reduce and zero reduce/reduce parser baseline. The complete
active EUnit suite passes 5,263 tests with zero failures or skips, and specs
governance passes with 42 requirements, 73 acceptance criteria, seven ADRs,
and 301 checked local links.

## Section 7.3: Multi-Shot Runtime, Branching, And Resources

**Description:** Permit an admissible captured continuation to run more than
once with explicit branch semantics, bounded resources, and no claim that
external world state has been duplicated.

- [x] **Section 7.3 Complete**

### Task 7.3.1: Implement Multi-Shot Invocation Authority

**Description:** Extend the runtime state machine to authorize repeated
invocations while isolating per-branch control metadata.

- [x] **Task 7.3.1 Complete**

#### Subtask 7.3.1.1: Create Branch Execution State

**Description:** Assign branch identities, preserve the immutable continuation
environment, isolate delimiter execution state, and keep process ownership
checks on every invocation.

- [x] **Subtask 7.3.1.1 Complete**

#### Subtask 7.3.1.2: Define Failure And Partial-Branch Behavior

**Description:** Specify whether one branch failure affects later branches,
how exceptions and aborts are reported, and how nested one-shot resumptions
inside a multi-shot branch are consumed.

- [x] **Subtask 7.3.1.2 Complete**

### Task 7.3.2: Enforce Effect-State And Resource Policy

**Description:** Make the static admissibility decision and runtime resource
policy agree for stateful, external, concurrent, and open effects.

- [x] **Task 7.3.2 Complete**

#### Subtask 7.3.2.1: Implement Branch State Semantics

**Description:** Define duplication or sharing for immutable lexical values
and each supported handler-state category, and reject PIDs, ports, mailboxes,
provider state, or capabilities that lack accepted branch semantics.

- [x] **Subtask 7.3.2.1 Complete**

#### Subtask 7.3.2.2: Add Branch Resource Budgets

**Description:** Limit invocation count, retained continuation memory,
reductions, timeout, and nested branching and report deterministic budget
failures with source origin.

- [x] **Subtask 7.3.2.2 Complete**

### Section 7.3 Evidence

Runtime ABI 3 authorizes an admissible `multi_shot` handle repeatedly while
keeping one invocation active at a time. Each invocation receives a
monotonically increasing branch identity and depth in its restored explicit
context. Completion returns the authority to `fresh`; an exception records a
failed branch without poisoning later branches. One-shot resumptions captured
inside separate multi-shot branches remain separate authorities and retain
ordinary exactly-once consumption.

Multi-shot capture remains conservative. The type checker admits only a
closed, empty residual effect row. The runtime additionally rejects process
providers, local value-provider state, and direct PID, port, or reference
capabilities in a continuation's lexical environment. Immutable lexical
values and local resumable definitions are structurally shared; Catena does
not claim to clone mailboxes, provider state, mutable external resources, or
the outside world.

Every multi-shot capture has positive limits for invocation count, retained
continuation words, per-branch reductions, cooperative timeout, and nested
branch depth. Defaults are 64 invocations, 262,144 retained words, 1,000,000
reductions, 5,000 milliseconds, and depth 16. Capture-time and invocation-time
violations report `resumption_budget_exceeded` with the source origin,
resource, limit, and observed value. `branch_stats/1` exposes only sanitized
counters and budgets; it never reveals the continuation or captured context.

The nine focused `catena_delimited_resumption_phase7_multishot_tests` cover
branch identities, independent failure, nested one-shot authority, ownership,
all five resource limits, unsafe state rejection, discard cleanup, artifact
contracts, and a Catena program that invokes one captured remainder twice and
executes as loaded BEAM with result `82`. The complete active EUnit suite
passes 5,272 tests with zero failures or skips.

## Section 7.4: Phase 7 Integration Tests

**Description:** Prove shallow propagation and multi-shot branching from
Catena source through loaded BEAM, including mixed modes, rejected residual
effects, and nondeterministic control examples.

- [x] **Section 7.4 Complete**

### Task 7.4.1: Execute Shallow And Multi-Shot Programs

**Description:** Compile, load, and run representative programs whose results
distinguish depth and continuation-kind semantics.

- [x] **Task 7.4.1 Complete**

#### Subtask 7.4.1.1: Test Handler Depth

**Description:** Cover shallow propagation, deep rehandling, nested
deep/shallow orderings, shadowing, parent fallback, abort, exceptions, retained
resumptions, and process identity.

- [x] **Subtask 7.4.1.1 Complete**

#### Subtask 7.4.1.2: Test Multi-Shot Branching

**Description:** Cover two and many invocations, different supplied values,
backtracking, nondeterministic solution enumeration, nested branching, branch
failure, limits, and deterministic results.

- [x] **Subtask 7.4.1.2 Complete**

### Task 7.4.2: Execute Safety And Phase Gates

**Description:** Prove that inadmissible effects and unselected modes fail
closed and that deep one-shot remains the unchanged default.

- [x] **Task 7.4.2 Complete**

#### Subtask 7.4.2.1: Test Negative Mode And Resource Paths

**Description:** Cover missing mode syntax, kind mismatch, shallow effect-row
errors, inadmissible external effects, open-row uncertainty, cross-process
resume, budget exhaustion, and artifact-version mismatch.

- [x] **Subtask 7.4.2.1 Complete**

#### Subtask 7.4.2.2: Run Repository Gates

**Description:** Run Phase 7 loaded-BEAM integration tests, all deep one-shot
regressions, `make check-specs`, `make conformance`, and the complete active
EUnit suite and publish the exact phase-ending evidence.

- [x] **Subtask 7.4.2.2 Complete**

### Section 7.4 Evidence

The eight `catena_delimited_resumption_phase7_integration_tests` exercise the
complete Phase 7 boundary. Loaded BEAM programs distinguish deep rehandling
from shallow propagation in both nesting orders, prove shadowing and parent
fallback, abort without resume, retained one-shot behavior, exception
consumption, and owner-process identity. Multi-shot programs execute four
different supplied values and a two-choice nondeterministic search whose
nested branch tree deterministically enumerates `[0, 1, 10, 11]` on repeated
runs.

Negative evidence rejects malformed mode syntax, a shallow residual effect
under a declared-pure signature, non-empty and open multi-shot residual rows,
unselected default one-shot reuse, foreign-process invocation, exhausted
budgets, stale runtime versions, and artifact handler-kind disagreement.
Generated resumable BEAM now carries `catena_handler_modes`; artifact
validation compares that compiled inventory with the public runtime contract
before loading, so a supported but incorrect replacement mode also fails
closed.

The phase gate passes all eight new integration tests, all 418 manifest-driven
conformance tests, and all 5,280 tests in the complete active EUnit suite with
zero failures or skips. Specs governance passes with 42 requirements in five
families, 11 scenarios, 20 evidence rows across 20 modules, 73 acceptance
criteria across 11 component specs, seven ADRs, and 301 checked local links.
