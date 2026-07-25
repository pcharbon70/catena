# Phase 8: Tooling, Optimization, Conformance, And Promotion

**Description:** This phase turns implemented resumption semantics into a
maintainable public language feature. It integrates the REPL and developer
tools, reconstructs source-oriented control traces, measures and optimizes the
selective-CPS path, adds normative contracts and executable conformance, and
promotes only the behavior proven by the final repository gates.

**Status:** Planned.

**Dependencies:** Phase 7 complete for promotion of shallow and multi-shot
behavior. Deep one-shot tooling and measurement tasks may begin after Phase 6,
but final status must remain mode-specific.

## Section 8.1: REPL, Diagnostics, And Developer Tooling

**Description:** Make first-class resumptions understandable and usable in
interactive and diagnostic workflows without exposing private runtime
representation.

- [ ] **Section 8.1 Complete**

### Task 8.1.1: Integrate Resumptions With The REPL

**Description:** Compile and execute handler definitions, resumption values,
and resume expressions through the canonical session pipeline.

- [ ] **Task 8.1.1 Complete**

#### Subtask 8.1.1.1: Support Interactive Definition And Evaluation

**Description:** Preserve typed `Resumption` bindings, multiline
`handle`/`with` input, session module recompilation, runtime ownership, and
explicit failure display.

- [ ] **Subtask 8.1.1.1 Complete**

#### Subtask 8.1.1.2: Add Safe Resumption Introspection

**Description:** Display public type, kind, owner relationship, state, depth,
and source capture location without printing closures, contexts, private
handles, or forgeable terms.

- [ ] **Subtask 8.1.1.2 Complete**

### Task 8.1.2: Reconstruct Source Control Traces

**Description:** Turn synthetic CPS frames and runtime events into readable
Catena diagnostics, stack views, and optional traces.

- [ ] **Task 8.1.2 Complete**

#### Subtask 8.1.2.1: Map CPS Frames To Source

**Description:** Collapse generated wrappers, bridges, continuations, and
temporaries into source transform, perform, handler, binder, resume, and
delimiter frames.

- [ ] **Subtask 8.1.2.1 Complete**

#### Subtask 8.1.2.2: Add Structured Control Tracing

**Description:** Trace capture, handler selection, resume, abort, branch,
consumption, timeout, and cleanup with redaction, stable identifiers, and
configurable overhead.

- [ ] **Subtask 8.1.2.2 Complete**

## Section 8.2: Performance Measurement And Semantics-Preserving Optimization

**Description:** Measure the cost of selective CPS and optimize only where
oracle and source-to-BEAM equivalence remain demonstrable.

- [ ] **Section 8.2 Complete**

### Task 8.2.1: Establish Performance Baselines

**Description:** Benchmark direct code, provider-only effects, deep one-shot
capture/resume, mixed-mode bridges, retained resumptions, shallow handling,
and multi-shot branching.

- [ ] **Task 8.2.1 Complete**

#### Subtask 8.2.1.1: Measure Compiler And Artifact Costs

**Description:** Record classification, CPS lowering, Core size, compile time,
artifact size, generated function count, and source-map metadata overhead.

- [ ] **Subtask 8.2.1.1 Complete**

#### Subtask 8.2.1.2: Measure Runtime Costs

**Description:** Record reductions, allocations, closure size, latency,
throughput, scheduler behavior, handler lookup, bridge cost, retention, and
branch-resource consumption.

- [ ] **Subtask 8.2.1.2 Complete**

### Task 8.2.2: Implement Validated Optimizations

**Description:** Reduce control overhead without changing evaluation order,
handler depth, ownership, consumption, effects, failures, or source
diagnostics.

- [ ] **Task 8.2.2 Complete**

#### Subtask 8.2.2.1: Optimize Proven Direct Regions And Bridges

**Description:** Eliminate unnecessary CPS conversion, inline safe final
continuations, collapse redundant bridges, and preserve conservative behavior
for open effect rows.

- [ ] **Subtask 8.2.2.1 Complete**

#### Subtask 8.2.2.2: Optimize Handler And Resumption Runtime Paths

**Description:** Cache safe lookup metadata, reduce allocation for immediate
tail auto-resume, optimize one-shot state authority, and retain complete
diagnostic and cleanup behavior.

- [ ] **Subtask 8.2.2.2 Complete**

## Section 8.3: Contracts, Conformance, Documentation, And Status

**Description:** Promote the implemented boundary into Catena's normative
control plane with dedicated executable evidence and an explicit deferred
ledger.

- [ ] **Section 8.3 Complete**

### Task 8.3.1: Add Normative Requirements And Scenarios

**Description:** Update compiler, runtime, testing, and observability contracts
only for behavior proven by executable source-to-BEAM evidence.

- [ ] **Task 8.3.1 Complete**

#### Subtask 8.3.1.1: Add Requirement And AC Traceability

**Description:** Add or refine `REQ-*` and component `AC-*` entries, update the
requirements catalog and conformance matrix, and distinguish deep one-shot
from shallow/multi-shot promotion.

- [ ] **Subtask 8.3.1.1 Complete**

#### Subtask 8.3.1.2: Add Dedicated Executable Scenario Evidence

**Description:** Define a stable resumption scenario, register its focused
EUnit modules in the executable manifest, and cover positive and negative
loaded-BEAM behavior.

- [ ] **Subtask 8.3.1.2 Complete**

### Task 8.3.2: Reconcile Public Documentation And Feature Status

**Description:** Update current syntax, architecture, runtime, tooling, user
guidance, examples, migration notes, and feature ledgers to match the verified
boundary exactly.

- [ ] **Task 8.3.2 Complete**

#### Subtask 8.3.2.1: Publish Language And Runtime Guidance

**Description:** Document `Resumption`, `with`, `resume`, auto-resume, abort,
depth, kinds, ownership, retention, effects, errors, performance, and safe
usage with executable examples.

- [ ] **Subtask 8.3.2.1 Complete**

#### Subtask 8.3.2.2: Update Status And Deferred Scope

**Description:** Mark only passing modes implemented, preserve cross-process,
serialization, distribution, and unsupported effect-state cloning as
deferred, and reconcile earlier algebraic-effects planning claims.

- [ ] **Subtask 8.3.2.2 Complete**

## Section 8.4: Phase 8 Integration Tests

**Description:** Run the final full-story verification from source syntax and
typing through selective CPS, runtime control, loaded BEAM, REPL/tooling,
performance checks, governance, and complete-suite promotion.

- [ ] **Section 8.4 Complete**

### Task 8.4.1: Execute Final End-To-End Conformance

**Description:** Exercise every promoted resumption mode across public
compiler, artifact, runtime, and interactive boundaries with stable positive
and negative results.

- [ ] **Task 8.4.1 Complete**

#### Subtask 8.4.1.1: Run Positive Full-Story Scenarios

**Description:** Cover explicit and automatic resume, abort, retention,
nested/mixed depth, one-shot and admissible multi-shot behavior, recursion,
imports, higher-order calls, traits, open effects, builtin providers, REPL,
tracing, and optimized/unoptimized equivalence.

- [ ] **Subtask 8.4.1.1 Complete**

#### Subtask 8.4.1.2: Run Negative Full-Story Scenarios

**Description:** Cover syntax, type/effect, control IR, bridge, artifact,
ownership, lifetime, consumption, mode, resource, runtime-version, and
unsupported deferred-surface failures with source-oriented diagnostics.

- [ ] **Subtask 8.4.1.2 Complete**

### Task 8.4.2: Run Final Promotion Gates

**Description:** Require governance, conformance, tests, coverage, static
analysis review, benchmarks, and documentation to agree before changing the
promoted status.

- [ ] **Task 8.4.2 Complete**

#### Subtask 8.4.2.1: Run Quality And Performance Gates

**Description:** Run focused resumption suites, `make check-specs`,
`make conformance`, the complete active EUnit suite, coverage for every
modified module, Dialyzer, deterministic repeated runs, and accepted
performance thresholds.

- [ ] **Subtask 8.4.2.1 Complete**

#### Subtask 8.4.2.2: Publish Final Evidence And Status

**Description:** Record exact test, scenario, coverage, Dialyzer, benchmark,
artifact-version, and deferred-ledger results and update current status only
after every promoted claim has matching evidence.

- [ ] **Subtask 8.4.2.2 Complete**
