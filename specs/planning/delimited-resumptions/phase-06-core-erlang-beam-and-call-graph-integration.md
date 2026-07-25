# Phase 6: Core Erlang, BEAM, And Call-Graph Integration

**Description:** This phase connects validated selective-CPS IR to the public
Core Erlang and BEAM artifact paths. It preserves source arity, process
identity, patterns, imports, traits, recursion, effects, runtime dependencies,
and diagnostics while proving that loaded BEAM executes the real delimited
continuation.

**Status:** Planned.

**Dependencies:** Phase 5 complete.

## Section 6.1: Core Erlang Lowering And Public Entry Boundaries

**Description:** Lower validated direct and resumable control IR into
OTP-accepted Core Erlang with explicit private calling conventions and stable
public wrappers.

- [ ] **Section 6.1 Complete**

### Task 6.1.1: Lower CPS Control Nodes

**Description:** Emit Core Erlang for continuation return, delimiters,
handler-frame installation, suspension, resumption construction, resume,
abort, and direct/CPS bridges.

- [ ] **Task 6.1.1 Complete**

#### Subtask 6.1.1.1: Emit Continuations And Delimiters

**Description:** Generate closures with validated arity and captured variables,
versioned delimiter/runtime calls, tail positions, and synthetic source
origins.

- [ ] **Subtask 6.1.1.1 Complete**

#### Subtask 6.1.1.2: Emit Perform And Resume Control

**Description:** Route resumable performs through local handler frames,
construct opaque resumptions, invoke `resume` through runtime validation, and
preserve handler result flow.

- [ ] **Subtask 6.1.1.2 Complete**

### Task 6.1.2: Preserve Public Source Arity

**Description:** Keep exported Catena transform arity stable while selecting
direct or CPS private entries behind generated wrappers.

- [ ] **Task 6.1.2 Complete**

#### Subtask 6.1.2.1: Generate Initial Runtime Boundaries

**Description:** Create the initial explicit context and final continuation
once at public execution boundaries and reuse both through internal effectful
calls.

- [ ] **Subtask 6.1.2.1 Complete**

#### Subtask 6.1.2.2: Preserve Function Values And Metadata

**Description:** Eta-expand named transforms with their correct mode, retain
export/interface arity, and hide private CPS parameters from source-visible
module contracts.

- [ ] **Subtask 6.1.2.2 Complete**

## Section 6.2: Complete Call-Graph And Language Integration

**Description:** Preserve resumption semantics across all callable and
expression surfaces already promoted by the fail-closed backend.

- [ ] **Section 6.2 Complete**

### Task 6.2.1: Integrate Local And Higher-Order Calls

**Description:** Carry continuations and contexts correctly through ordinary,
forward, recursive, mutually recursive, closure, and constructor-rich code.

- [ ] **Task 6.2.1 Complete**

#### Subtask 6.2.1.1: Lower Recursive And Mixed-Mode Graphs

**Description:** Execute direct and resumable self/mutual recursion with
resolved modes, bounded stack behavior, and valid tail calls where semantics
permit.

- [ ] **Subtask 6.2.1.1 Complete**

#### Subtask 6.2.1.2: Lower Higher-Order And Data Paths

**Description:** Preserve callable modes in lambdas and stored transforms and
preserve patterns, ADTs, tuples, lists, records, guards, and clause
fallthrough inside CPS regions.

- [ ] **Subtask 6.2.1.2 Complete**

### Task 6.2.2: Integrate Imports, Traits, And Effect Polymorphism

**Description:** Make versioned module interfaces and runtime dictionaries
carry enough control-mode metadata for cross-module and dynamic dispatch.

- [ ] **Task 6.2.2 Complete**

#### Subtask 6.2.2.1: Lower Imported Resumable Calls

**Description:** Extend executable interfaces, dependency ordering, qualified
and aliased calls, imported closures, and artifact validation with stable
direct/CPS ABI metadata.

- [ ] **Subtask 6.2.2.1 Complete**

#### Subtask 6.2.2.2: Lower Trait And Open-Row Calls

**Description:** Carry control mode through dictionaries, desugared trait
calls, operator/do targets, and conservatively resumable effect-polymorphic
functions.

- [ ] **Subtask 6.2.2.2 Complete**

## Section 6.3: Artifacts, Runtime Versions, And Diagnostics

**Description:** Make resumption dependencies and source origins part of the
validated public artifact rather than hidden assumptions in generated Core.

- [ ] **Section 6.3 Complete**

### Task 6.3.1: Version Artifact And Runtime Dependencies

**Description:** Declare the exact runtime capabilities required by resumable
artifacts and reject incompatible load targets before execution.

- [ ] **Task 6.3.1 Complete**

#### Subtask 6.3.1.1: Extend Artifact Metadata

**Description:** Record control-ABI version, resumption-runtime version,
required handler-frame features, source/runtime module identities, and
dependency checksums where applicable.

- [ ] **Subtask 6.3.1.1 Complete**

#### Subtask 6.3.1.2: Validate Load Compatibility

**Description:** Reject missing, stale, or incompatible runtime contracts and
normalize Core/OTP dependency diagnostics into Catena artifact failures.

- [ ] **Subtask 6.3.1.2 Complete**

### Task 6.3.2: Preserve Source-Oriented Control Diagnostics

**Description:** Relate every generated CPS or runtime failure to its Catena
perform, handler case, binder, resume, call, and delimiter origin.

- [ ] **Task 6.3.2 Complete**

#### Subtask 6.3.2.1: Track Synthetic Origin Chains

**Description:** Preserve origins across auto-resume, generated continuations,
wrappers, bridges, imported entries, dictionary calls, and Core temporary
variables.

- [ ] **Subtask 6.3.2.1 Complete**

#### Subtask 6.3.2.2: Normalize Backend And Runtime Failures

**Description:** Report unsupported lowering, invalid IR, Core lint, OTP
compile, artifact version, ownership, consumption, and delimiter failures
without exposing internal closure terms.

- [ ] **Subtask 6.3.2.2 Complete**

## Section 6.4: Phase 6 Integration Tests

**Description:** Prove through public source-to-BEAM APIs that deep one-shot
handlers execute real resumptions across the complete promoted call and data
surface while invalid semantics fail closed.

- [ ] **Section 6.4 Complete**

### Task 6.4.1: Execute Positive Source-To-BEAM Programs

**Description:** Compile Catena source to validated units, selective-CPS IR,
Core Erlang, and loaded BEAM and assert observable continuation behavior.

- [ ] **Task 6.4.1 Complete**

#### Subtask 6.4.1.1: Test Core Resumption Semantics

**Description:** Execute explicit resume, auto-resume, abort, transformed
delimiter results, nested deep handlers, multiple performs, retained
same-process resumptions, and builtin-provider interaction.

- [ ] **Subtask 6.4.1.1 Complete**

#### Subtask 6.4.1.2: Test Complete Call And Data Surfaces

**Description:** Execute local, recursive, imported, higher-order,
trait-dispatched, open-row, patterned, ADT, list, tuple, and record programs
that suspend and resume across those boundaries.

- [ ] **Subtask 6.4.1.2 Complete**

### Task 6.4.2: Execute Negative And Phase-Gate Programs

**Description:** Demonstrate fail-closed compilation, artifact validation, and
structured runtime failures for every unsupported or invalid control path.

- [ ] **Task 6.4.2 Complete**

#### Subtask 6.4.2.1: Test Negative Artifacts And Runtime Behavior

**Description:** Cover ABI mismatch, leaked IR/AST nodes, unresolved bridges,
invalid Core, stale runtimes, double resume, wrong owner, expired delimiters,
and deferred shallow/multi-shot source behavior.

- [ ] **Subtask 6.4.2.1 Complete**

#### Subtask 6.4.2.2: Run Repository Gates

**Description:** Run Phase 6 loaded-BEAM integration tests, focused backend and
runtime suites, `make check-specs`, `make conformance`, and the complete active
EUnit suite and publish the exact phase-ending evidence.

- [ ] **Subtask 6.4.2.2 Complete**
