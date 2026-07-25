# Phase 1: Operational Semantics, Feature Ledger, And Reference Oracle

**Description:** This phase fixes the meaning of first-class delimited
resumptions before parser, type-system, runtime, or backend changes begin. It
defines one executable semantic oracle and one feature ledger that distinguish
real continuation behavior from the current request/response and marker-based
implementations.

**Status:** Complete.

**Dependencies:** ADR-0006 and the Delimited Resumption Architecture accepted.

## Section 1.1: Core Operational Semantics

**Description:** Define the machine configurations and reduction rules for
deep one-shot handlers, explicit resumption, compatible value handlers, and
delimited return.

- [x] **Section 1.1 Complete**

### Task 1.1.1: Define Evaluation Configurations And Control Boundaries

**Description:** Specify the semantic state needed to evaluate a computation
with explicit effect contexts, handler frames, delimiters, continuations, and
resumption ownership.

- [x] **Task 1.1.1 Complete**

#### Subtask 1.1.1.1: Define Semantic Terms And Runtime State

**Description:** Define values, expressions, contexts, handler frames,
delimiter identities, continuation frames, resumption kinds, owner identity,
and consumption states without depending on Erlang stack capture.

- [x] **Subtask 1.1.1.1 Complete**

#### Subtask 1.1.1.2: Define Normal Return And Delimited Return

**Description:** Specify how direct values flow through ordinary evaluation and
how a resumed computation returns a result to the handler at its matching
delimiter.

- [x] **Subtask 1.1.1.2 Complete**

### Task 1.1.2: Define Perform, Handle, Resume, And Abort Reductions

**Description:** Write the small-step rules that suspend at `perform`, select a
handler through the explicit context, construct a resumption, invoke it, or
discard its continuation.

- [x] **Task 1.1.2 Complete**

#### Subtask 1.1.2.1: Define Capture And Handler Transfer

**Description:** Specify how evaluation captures the remainder to the nearest
delimiter, packages it as `Resumption OneShot a b e`, and transfers control to
the selected operation case.

- [x] **Subtask 1.1.2.1 Complete**

#### Subtask 1.1.2.2: Define Resume And Non-Resume Completion

**Description:** Specify value substitution, deep handler reinstatement,
delimiter completion, one-shot consumption, and the abort semantics of
returning from a control handler without invoking its resumption.

- [x] **Subtask 1.1.2.2 Complete**

## Section 1.2: Compatibility, Ownership, And Semantic Modes

**Description:** Reconcile the new control semantics with current value
handlers, BEAM process identity, and the deliberately deferred shallow and
multi-shot modes.

- [x] **Section 1.2 Complete**

### Task 1.2.1: Specify Value-Handler Auto-Resume

**Description:** Define the source-compatible translation from an operation
case without `with` to a synthetic deep one-shot tail resumption.

- [x] **Task 1.2.1 Complete**

#### Subtask 1.2.1.1: Define The Synthetic Translation

**Description:** Specify binder generation, expression evaluation order,
tail-resume placement, source-origin ownership, and the absence of implicit
resume in an explicit `with` case.

- [x] **Subtask 1.2.1.1 Complete**

#### Subtask 1.2.1.2: Prove Compatibility Examples

**Description:** Evaluate representative existing IO, Process, nested-handler,
error, and timeout examples under both the current value-handler reading and
the normalized auto-resume reading.

- [x] **Subtask 1.2.1.2 Complete**

### Task 1.2.2: Specify Process Affinity And Mode Defaults

**Description:** Define the invariants that resumed computation stays on its
capturing BEAM process and that deep one-shot behavior is the only initial
promotion target.

- [x] **Task 1.2.2 Complete**

#### Subtask 1.2.2.1: Define Ownership And Lifetime Rules

**Description:** Specify owner capture, valid same-process invocation,
retention, owner death, stale delimiters, re-entrancy, and deterministic
one-shot consumption.

- [x] **Subtask 1.2.2.1 Complete**

#### Subtask 1.2.2.2: Bound Shallow And Multi-Shot Semantics

**Description:** Record their accepted conceptual meanings while requiring
Phase 7 syntax, type, runtime, and evidence before either mode is source
promoted.

- [x] **Subtask 1.2.2.2 Complete**

## Section 1.3: Reference Oracle, Feature Ledger, And Baseline

**Description:** Turn the semantics into executable comparison evidence and
record the exact repository boundary from which implementation begins.

- [x] **Section 1.3 Complete**

### Task 1.3.1: Implement The Reference Semantic Oracle

**Description:** Build a small maintained Erlang evaluator or equivalent model
that executes the normative deep one-shot rules independently of the
production Core Erlang backend.

- [x] **Task 1.3.1 Complete**

#### Subtask 1.3.1.1: Implement Traceable Semantic Steps

**Description:** Emit stable events for delimiter entry, perform, capture,
handler selection, resume, abort, delimiter return, and consumption failure.

- [x] **Subtask 1.3.1.1 Complete**

#### Subtask 1.3.1.2: Add Deterministic Oracle Fixtures

**Description:** Cover simple resume, transformed resume result, abort,
multiple operations, nested deep handlers, auto-resume, retention, and invalid
second invocation with deterministic expected traces.

- [x] **Subtask 1.3.1.2 Complete**

### Task 1.3.2: Publish The Feature And Diagnostic Baseline

**Description:** Inventory current syntax, AST, typing, runtime, backend,
tests, parser conflicts, and placeholder resumption behavior before source
implementation begins.

- [x] **Task 1.3.2 Complete**

#### Subtask 1.3.2.1: Classify Existing Effect Surfaces

**Description:** Mark each relevant module and behavior as request/response,
internal helper, marker-backed, target semantic oracle, or deferred.

- [x] **Subtask 1.3.2.1 Complete**

#### Subtask 1.3.2.2: Define Stable Failure Categories

**Description:** Name the compile-time and runtime diagnostic families for
invalid binder use, invalid resume targets, type/effect mismatch, ABI failure,
ownership, lifetime, and consumption.

- [x] **Subtask 1.3.2.2 Complete**

## Section 1.4: Phase 1 Integration Tests

**Description:** Prove that the written semantics, executable oracle, feature
ledger, and current repository baseline describe the same deep one-shot
behavior before compiler changes begin.

- [x] **Section 1.4 Complete**

### Task 1.4.1: Validate Semantic And Oracle Agreement

**Description:** Execute the normative examples through the reference oracle
and compare every control event and final result with the written reduction
rules.

- [x] **Task 1.4.1 Complete**

#### Subtask 1.4.1.1: Test Positive Control Traces

**Description:** Verify explicit resume, auto-resume, abort, nested deep
handlers, transformed results, retained resumptions, and sequential performs.

- [x] **Subtask 1.4.1.1 Complete**

#### Subtask 1.4.1.2: Test Negative Control Traces

**Description:** Verify second resume, re-entrant resume, wrong owner, stale
delimiter, and unsupported multi-shot attempts produce the specified oracle
failure categories.

- [x] **Subtask 1.4.1.2 Complete**

### Task 1.4.2: Run Phase Completion Gates

**Description:** Preserve the existing compiler while establishing a stable,
reviewable semantic baseline for Phase 2.

- [x] **Task 1.4.2 Complete**

#### Subtask 1.4.2.1: Verify Current Behavior Remains Honest

**Description:** Run existing effect, handler, resumption-helper, runtime, and
backend tests and confirm none are relabeled as true source-level
continuation evidence.

- [x] **Subtask 1.4.2.1 Complete**

#### Subtask 1.4.2.2: Run Repository Gates

**Description:** Run the oracle integration suite, `make check-specs`, and the
complete active EUnit suite and publish the exact phase-ending evidence.

- [x] **Subtask 1.4.2.2 Complete**

### Phase 1 Completion Evidence

**Description:** The 2026-07-25 phase gate records executable agreement with
the semantic model while preserving the distinction between the oracle and
the unimplemented source-to-BEAM resumption path.

| Gate | Result |
| --- | --- |
| Oracle unit and Phase 1 integration modules | 32 tests passed |
| Positive integration behavior | explicit resume, auto-resume, abort, transformed result, nested deep handling, sequential performs, and retained same-owner resume passed |
| Negative integration behavior | consumed, re-entrant, wrong-owner, expired-owner, stale-delimiter, and unsupported multi-shot categories passed |
| Legacy boundary | marker capture still returns `{resumed, Value}`; `with` and `resume` remain ordinary identifiers |
| Focused legacy effect/runtime/backend matrix | 216 tests passed before the additional oracle edge fixtures |
| Modified oracle coverage | 93% focused line coverage |
| Specs governance | 42 requirements, 11 scenarios, 20 evidence rows, 73 acceptance criteria, 6 ADRs, and 289 local links passed |
| Parser baseline | 37 shift/reduce and 0 reduce/reduce conflicts |
| Complete active EUnit suite | 5,061 tests passed; 0 failures; 0 skips |
| Dialyzer | Not rerun; the existing repository-wide baseline remains 822 warnings |

The initial complete-suite attempt correctly exposed a stale governance test
that expected five ADRs. Updating that assertion to the catalog's six ADRs
made the focused governance module and the complete suite green.
