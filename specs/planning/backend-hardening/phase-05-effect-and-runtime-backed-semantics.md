# Phase 5: Effect And Runtime-Backed Semantics

**Description:** This phase promotes the basic effect vertical slice into a
validated, source-to-BEAM contract by resolving effect operations, preserving
explicit runtime contexts, compiling handlers without lossy parameter
substitution, and making runtime dependencies visible.

**Status:** Planned.

**Dependencies:** Phase 4 complete.

## Section 5.1: Effect And Operation Resolution

**Description:** Connect declared effect-operation metadata to type checking,
call resolution, backend lowering, and diagnostics before effect declarations
are erased.

- [x] **Section 5.1 Complete**

### Task 5.1.1: Index Effect Operations

**Description:** Add every declared effect and operation to the validated unit
with its parameter types, result type, effect identity, arity, and source
location.

- [x] **Task 5.1.1 Complete**

#### Subtask 5.1.1.1: Preserve Operation Signatures

**Description:** Convert operation declarations into stable typed metadata
that survives semantic normalization and static effect-declaration erasure.

- [x] **Subtask 5.1.1.1 Complete**

#### Subtask 5.1.1.2: Resolve Performed Operations

**Description:** Bind each `perform Effect.operation(...)` expression to one
declared operation identity and reject missing effects, missing operations,
and arity mismatches.

- [x] **Subtask 5.1.1.2 Complete**

### Task 5.1.2: Validate Operation Types And Effects

**Description:** Use resolved operation signatures rather than fresh
unconstrained result types when checking effectful expressions.

- [x] **Task 5.1.2 Complete**

#### Subtask 5.1.2.1: Check Operation Arguments And Results

**Description:** Unify performed arguments with declared parameters and return
the declared result type through the typed backend input.

- [x] **Subtask 5.1.2.1 Complete**

#### Subtask 5.1.2.2: Preserve Effect Obligations

**Description:** Carry synthesized and declared effect sets into backend
metadata and reject unresolved or invalid effect constraints before lowering.

- [x] **Subtask 5.1.2.2 Complete**

## Section 5.2: Explicit Effect Context Lowering

**Description:** Make runtime context creation, propagation, nesting, and
cleanup explicit and structurally valid in every effectful generated function.

- [x] **Section 5.2 Complete**

### Task 5.2.1: Lower Effectful Function Boundaries

**Description:** Wrap effectful transforms exactly once with the accepted
runtime lifecycle and bind the explicit context used by all nested operations.

- [x] **Task 5.2.1 Complete**

#### Subtask 5.2.1.1: Bind Runtime Context Variables Safely

**Description:** Generate hygienic Core variables for effect contexts and avoid
capture or unbound-context failures in nested lambdas and matches.

- [x] **Subtask 5.2.1.1 Complete**

#### Subtask 5.2.1.2: Preserve Runtime Lifecycle Semantics

**Description:** Ensure runtime initialization, body execution, handler cleanup,
and shutdown behavior remain correct on success, error, and timeout paths.

- [x] **Subtask 5.2.1.2 Complete**

### Task 5.2.2: Lower Perform Operations

**Description:** Emit resolved calls to the accepted Catena effect runtime with
the current context, effect identity, operation identity, and argument list.

- [x] **Task 5.2.2 Complete**

#### Subtask 5.2.2.1: Emit Explicit Runtime Calls

**Description:** Build Core remote calls to the effect runtime without
string-based lookup, implicit process-dictionary authority, or unresolved
operation atoms.

- [x] **Subtask 5.2.2.1 Complete**

#### Subtask 5.2.2.2: Propagate Runtime Failures

**Description:** Preserve unhandled effects, handler failures, and timeouts as
documented Catena runtime outcomes rather than backend crashes.

- [x] **Subtask 5.2.2.2 Complete**

## Section 5.3: Handler Compilation And Runtime Dependencies

**Description:** Compile handler clauses and make the generated module's Catena
runtime requirements explicit and testable.

- [x] **Section 5.3 Complete**

### Task 5.3.1: Compile Handler Clauses Losslessly

**Description:** Resolve handled effects and operations, validate coverage, and
compile handler parameters and bodies without replacing complex patterns.

- [x] **Task 5.3.1 Complete**

#### Subtask 5.3.1.1: Validate Handler Operation Coverage

**Description:** Detect missing, duplicate, unknown, and arity-invalid operation
cases against the resolved effect declaration.

- [x] **Subtask 5.3.1.1 Complete**

#### Subtask 5.3.1.2: Preserve Handler Parameters And Scope

**Description:** Compile operation parameters, nested bindings, handler-local
calls, and child contexts with the same scope rules as ordinary transforms.

- [x] **Subtask 5.3.1.2 Complete**

### Task 5.3.2: Declare Runtime Artifact Dependencies

**Description:** Record which generated modules require Catena effect runtime
modules so loading and packaging layers can satisfy those dependencies
deliberately.

- [x] **Task 5.3.2 Complete**

#### Subtask 5.3.2.1: Collect Runtime Dependency Metadata

**Description:** Add effect-system and effect-runtime dependencies to the
backend artifact metadata whenever emitted code references them.

- [x] **Subtask 5.3.2.1 Complete**

#### Subtask 5.3.2.2: Reject Unavailable Runtime Contracts

**Description:** Fail artifact preparation with an actionable diagnostic when
the selected target cannot supply a required Catena runtime module or version.

- [x] **Subtask 5.3.2.2 Complete**

## Section 5.4: Phase 5 Integration Tests

**Description:** Execute declared, performed, handled, nested, and failing
effects from Catena source through loaded BEAM with the accepted explicit
runtime model.

- [ ] **Section 5.4 Complete**

### Task 5.4.1: Execute Effect Programs

**Description:** Add source-to-BEAM programs for operation resolution, basic
handlers, nested handlers, multiple effects, and effectful helper calls.

- [ ] **Task 5.4.1 Complete**

#### Subtask 5.4.1.1: Test Successful Effect Execution

**Description:** Execute zero- and multi-argument operations, handler results,
nested contexts, and calls between effectful transforms.

- [ ] **Subtask 5.4.1.1 Complete**

#### Subtask 5.4.1.2: Test Runtime Cleanup

**Description:** Verify handler processes and runtime state are cleaned after
normal results, handler exceptions, unhandled operations, and timeouts.

- [ ] **Subtask 5.4.1.2 Complete**

### Task 5.4.2: Verify Effect Failure Boundaries

**Description:** Confirm invalid effect programs fail during validation and
runtime failures retain their documented execution semantics.

- [ ] **Task 5.4.2 Complete**

#### Subtask 5.4.2.1: Test Invalid Declarations And Handlers

**Description:** Reject unknown operations, wrong arities, invalid argument
types, missing handler cases where coverage is required, and effect-set
mismatches.

- [ ] **Subtask 5.4.2.1 Complete**

#### Subtask 5.4.2.2: Run Phase Completion Gates

**Description:** Run effect typing, codegen, runtime, Core validation, and
source-to-BEAM suites plus `make check-specs`, `make conformance`, and the
complete active EUnit suite.

- [ ] **Subtask 5.4.2.2 Complete**
