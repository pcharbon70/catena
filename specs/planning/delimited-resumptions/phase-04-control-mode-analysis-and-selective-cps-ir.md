# Phase 4: Control-Mode Analysis And Selective CPS IR

**Description:** This phase makes the executable remainder of a computation
explicit in compiler-owned IR. It classifies callables as direct or resumable,
lowers resumable regions into selective CPS, defines all cross-mode calling
conventions, and validates the control graph before Core Erlang emission.

**Status:** In progress (Section 4.1 complete).

**Dependencies:** Phase 3 complete.

## Section 4.1: Direct And Resumable Control-Mode Analysis

**Description:** Compute one authoritative lowering mode from typed effects,
handler delimiters, resume use, imports, traits, and the resolved call graph.

- [x] **Section 4.1 Complete**

### Task 4.1.1: Classify Local Expressions And Callables

**Description:** Mark each transform and relevant expression region `direct`
or `resumable` with a source-oriented reason.

- [x] **Task 4.1.1 Complete**

#### Subtask 4.1.1.1: Define Classification Rules

**Description:** Classify pure and provider-only computations as direct and
classify resumable handlers, resume expressions, and effects that may reach a
source handler as resumable.

- [x] **Subtask 4.1.1.1 Complete**

#### Subtask 4.1.1.2: Treat Open Effects Conservatively

**Description:** Mark open or effect-polymorphic rows resumable unless their
constraints prove they cannot suspend, and retain that proof or conservative
reason in the validated unit.

- [x] **Subtask 4.1.1.2 Complete**

### Task 4.1.2: Solve Control Modes Across The Call Graph

**Description:** Propagate resumability through local, recursive, imported,
higher-order, and trait-dispatched edges to a deterministic fixed point.

- [x] **Task 4.1.2 Complete**

#### Subtask 4.1.2.1: Analyze Resolved Direct Edges

**Description:** Propagate mode requirements through local, forward,
self-recursive, mutually recursive, and versioned imported calls without
depending on declaration order.

- [x] **Subtask 4.1.2.1 Complete**

#### Subtask 4.1.2.2: Analyze Dynamic Callable Edges

**Description:** Carry control-mode capability in closure and trait dictionary
types so higher-order or dictionary dispatch never guesses a calling
convention at Core emission time.

- [x] **Subtask 4.1.2.2 Complete**

**Implementation evidence:** `catena_control_mode` now classifies every
implemented transform and control-bearing region with a source-oriented
reason, retained type and effect-row evidence, and explicit local, imported,
higher-order, or trait-dispatch edges. Pure and provider-only paths remain
direct; handlers, resume use, open rows, and unresolved dynamic capabilities
are resumable. A declaration-order-independent fixed point propagates
resumability through recursive local graphs. Validated compilation units
retain the versioned inventory as the sole downstream calling-convention
authority. The focused Section 4.1 and compilation-unit suites pass 10 tests.

## Section 4.2: CPS Control IR And Source Origins

**Description:** Introduce a small validated IR for returns, delimiters,
performs, resumptions, resume invocation, abort, calls, and mode bridges.

- [ ] **Section 4.2 Complete**

### Task 4.2.1: Define The Control IR

**Description:** Specify canonical nodes, invariants, types, and source origins
for direct and CPS control flow independently of Core Erlang syntax.

- [ ] **Task 4.2.1 Complete**

#### Subtask 4.2.1.1: Define Core Control Nodes

**Description:** Represent continuation return, direct call, CPS call,
delimiter entry/exit, handler installation, perform suspension, resumption
construction, resume invocation, abort, and bridge operations.

- [ ] **Subtask 4.2.1.1 Complete**

#### Subtask 4.2.1.2: Define IR Type And Origin Contracts

**Description:** Attach value type, effect row, control mode, delimiter
identity, continuation arity, runtime disposition, and source/synthetic origin
to every control-bearing node.

- [ ] **Subtask 4.2.1.2 Complete**

### Task 4.2.2: Lower Typed AST Into Selective CPS

**Description:** Translate only resumable regions while preserving direct
evaluation order, pattern semantics, errors, and tail positions.

- [ ] **Task 4.2.2 Complete**

#### Subtask 4.2.2.1: Lower Expressions And Delimiters

**Description:** Lower values, lets, matches, calls, perform, handle,
auto-resume, explicit resume, and abort into control IR with exactly-once
evaluation.

- [ ] **Subtask 4.2.2.1 Complete**

#### Subtask 4.2.2.2: Preserve Patterns, Guards, And Failures

**Description:** Keep parser-native pattern bindings, pure guards, clause
fallthrough, exceptions, and source-origin diagnostics intact across generated
continuation closures.

- [ ] **Subtask 4.2.2.2 Complete**

## Section 4.3: Calling Conventions, Bridges, And IR Validation

**Description:** Define stable direct/CPS private entries and reject malformed
control graphs before any Core Erlang is emitted.

- [ ] **Section 4.3 Complete**

### Task 4.3.1: Implement Direct And Resumable Calling Conventions

**Description:** Preserve public source arity while giving private resumable
entries explicit context and continuation parameters.

- [ ] **Task 4.3.1 Complete**

#### Subtask 4.3.1.1: Define Entry And Closure Shapes

**Description:** Define public wrappers, direct private entries, CPS private
entries, final continuations, named transform values, imported closures, and
trait dictionary entries with resolved arities.

- [ ] **Subtask 4.3.1.1 Complete**

#### Subtask 4.3.1.2: Implement Explicit Mode Bridges

**Description:** Wrap direct returns for resumable callers, allow
resumable-to-direct calls only when non-suspension is proven, and reject
unresolved or cyclic ABI mismatches.

- [ ] **Subtask 4.3.1.2 Complete**

### Task 4.3.2: Validate The Complete Control IR

**Description:** Add a fail-closed validator that proves delimiter,
continuation, resumption, bridge, type, arity, and origin invariants.

- [ ] **Task 4.3.2 Complete**

#### Subtask 4.3.2.1: Validate Control Ownership

**Description:** Reject dangling or mismatched delimiters, continuation arity
errors, resumption-kind mismatch, resume without authority, and invalid abort
targets.

- [ ] **Subtask 4.3.2.1 Complete**

#### Subtask 4.3.2.2: Validate Backend Readiness

**Description:** Require every typed control construct to be direct-lowered,
CPS-lowered, runtime-lowered, intentionally erased, or rejected with a
source-oriented diagnostic.

- [ ] **Subtask 4.3.2.2 Complete**

## Section 4.4: Phase 4 Integration Tests

**Description:** Prove that real source programs produce one deterministic,
validated selective-CPS graph and that malformed or unsupported control paths
fail before Core Erlang emission.

- [ ] **Section 4.4 Complete**

### Task 4.4.1: Exercise Classification And CPS Graphs

**Description:** Run typed source through call resolution, control-mode fixed
point analysis, AST-to-IR lowering, bridge generation, and IR validation.

- [ ] **Task 4.4.1 Complete**

#### Subtask 4.4.1.1: Test Positive Mixed-Mode Graphs

**Description:** Cover pure direct code, direct provider effects, explicit
handlers, auto-resume, recursion, mutual recursion, imports, higher-order
closures, traits, and open effect rows with expected modes and IR traces.

- [ ] **Subtask 4.4.1.1 Complete**

#### Subtask 4.4.1.2: Test Negative Control Graphs

**Description:** Cover dangling delimiters, wrong continuation arity,
unresolved bridge modes, invalid higher-order capabilities, leaked AST nodes,
and source-origin preservation in every diagnostic.

- [ ] **Subtask 4.4.1.2 Complete**

### Task 4.4.2: Run Phase Completion Gates

**Description:** Establish a validated control-IR boundary ready for runtime
and Core lowering while preserving all direct backend behavior.

- [ ] **Task 4.4.2 Complete**

#### Subtask 4.4.2.1: Run Compiler And Backend Regressions

**Description:** Run classification, CPS IR, call-resolution, recursion,
import, trait, effect, pattern, fail-closed backend, and public artifact
negative suites.

- [ ] **Subtask 4.4.2.1 Complete**

#### Subtask 4.4.2.2: Run Repository Gates

**Description:** Run Phase 4 integration tests, `make check-specs`, and the
complete active EUnit suite and record the exact phase-ending evidence.

- [ ] **Subtask 4.4.2.2 Complete**
