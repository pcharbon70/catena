# Phase 3: Local And Higher-Order Call Resolution

**Description:** This phase fixes the named-call failure that blocks ordinary
multi-function programs by resolving module-local transforms, recursion,
constructors, and higher-order callable values before Core Erlang emission.

**Status:** Complete.

**Dependencies:** Phase 2 complete.

## Section 3.1: Module-Local Symbol And Arity Resolution

**Description:** Build the complete local callable inventory before lowering
any function body so resolution does not depend on declaration order.

- [x] **Section 3.1 Complete**

### Task 3.1.1: Predeclare Local Callable Symbols

**Description:** Collect every implemented transform and constructor identity,
arity, visibility, type, and source location into the validated unit.

- [x] **Task 3.1.1 Complete**

#### Subtask 3.1.1.1: Index Transform Identities

**Description:** Index all top-level transforms before body traversal,
including later declarations and mutually recursive groups.

- [x] **Subtask 3.1.1.1 Complete**

#### Subtask 3.1.1.2: Index Constructor Identities

**Description:** Derive constructor arities and result-type ownership from type
declarations before those declarations are erased.

- [x] **Subtask 3.1.1.2 Complete**

### Task 3.1.2: Enforce Name And Arity Rules

**Description:** Resolve each unqualified callable name to one unambiguous local
identity or return a structured diagnostic.

- [x] **Task 3.1.2 Complete**

#### Subtask 3.1.2.1: Detect Duplicate And Ambiguous Callables

**Description:** Reject symbol definitions or overload sets that the current
language rules cannot distinguish at a call site.

- [x] **Subtask 3.1.2.1 Complete**

#### Subtask 3.1.2.2: Detect Call Arity Mismatches

**Description:** Compare source argument count with resolved callable arity and
report the call and declaration locations on failure.

- [x] **Subtask 3.1.2.2 Complete**

## Section 3.2: Local, Forward, And Recursive Core Calls

**Description:** Emit valid Core Erlang local function references for every
resolved top-level transform call.

- [x] **Section 3.2 Complete**

### Task 3.2.1: Lower Direct Local Calls

**Description:** Replace bare-variable application for resolved transforms with
the OTP Core Erlang representation for a local function target and arity.

- [x] **Task 3.2.1 Complete**

#### Subtask 3.2.1.1: Emit Resolved Function Names

**Description:** Use Core function identities rather than ordinary unbound
variables for direct transform invocation.

- [x] **Subtask 3.2.1.1 Complete**

#### Subtask 3.2.1.2: Validate Forward References

**Description:** Confirm calls to transforms declared later in the module emit
the same valid Core form as calls to earlier declarations.

- [x] **Subtask 3.2.1.2 Complete**

### Task 3.2.2: Support Recursive Call Graphs

**Description:** Preserve self-recursive and mutually recursive transform
semantics without special declaration ordering or runtime lookup.

- [x] **Task 3.2.2 Complete**

#### Subtask 3.2.2.1: Lower Self-Recursion

**Description:** Resolve a transform's call to itself using its predeclared
identity and verified arity.

- [x] **Subtask 3.2.2.1 Complete**

#### Subtask 3.2.2.2: Lower Mutual Recursion

**Description:** Resolve cycles among several local transforms and validate the
resulting Core Erlang module as one recursive definition set.

- [x] **Subtask 3.2.2.2 Complete**

## Section 3.3: Higher-Order Values And Constructors

**Description:** Distinguish top-level calls from closure application and
constructor application so the same source identifier cannot be lowered by
guesswork.

- [x] **Section 3.3 Complete**

### Task 3.3.1: Resolve Higher-Order Callable Values

**Description:** Lower lambda values, parameters, let-bound functions, and
captured top-level transforms with semantics appropriate to their resolved
callable kind.

- [x] **Task 3.3.1 Complete**

#### Subtask 3.3.1.1: Preserve Closure Application

**Description:** Continue using Core closure application only for values that
type checking and scope resolution identify as callable runtime values.

- [x] **Subtask 3.3.1.1 Complete**

#### Subtask 3.3.1.2: Represent Top-Level Functions As Values

**Description:** Use a valid Core function reference or eta-expanded closure
when a named transform is passed, stored, or returned instead of directly
called.

- [x] **Subtask 3.3.1.2 Complete**

### Task 3.3.2: Resolve Constructor Applications Separately

**Description:** Keep constructor identity and arity resolution distinct from
ordinary transform and closure calls.

- [x] **Task 3.3.2 Complete**

#### Subtask 3.3.2.1: Validate Constructor Arity

**Description:** Reject under- and over-applied constructors unless the
language explicitly promotes a partial-application rule.

- [x] **Subtask 3.3.2.1 Complete**

#### Subtask 3.3.2.2: Emit Tagged Constructor Values

**Description:** Preserve the accepted tagged-tuple representation for nullary,
unary, and higher-arity constructors.

- [x] **Subtask 3.3.2.2 Complete**

## Section 3.4: Phase 3 Integration Tests

**Description:** Prove that ordinary multi-function and higher-order Catena
programs compile to loadable BEAM while unresolved or misapplied targets fail
before artifact success.

- [x] **Section 3.4 Complete**

### Task 3.4.1: Execute Local Call Graphs

**Description:** Add source-to-BEAM programs that cover the complete
module-local call-resolution surface.

- [x] **Task 3.4.1 Complete**

#### Subtask 3.4.1.1: Test Direct And Forward Calls

**Description:** Execute programs with earlier and later transform targets and
assert their observable results.

- [x] **Subtask 3.4.1.1 Complete**

#### Subtask 3.4.1.2: Test Self And Mutual Recursion

**Description:** Execute terminating recursive programs and validate their
results, Core compilation, and module loading behavior.

- [x] **Subtask 3.4.1.2 Complete**

### Task 3.4.2: Execute Higher-Order And Negative Paths

**Description:** Test closure calls, named transforms as values, constructor
calls, and rejected unresolved or arity-invalid targets.

- [x] **Task 3.4.2 Complete**

#### Subtask 3.4.2.1: Test Higher-Order Execution

**Description:** Execute lambda parameters, returned functions, let-bound
functions, and named transform references passed to other transforms.

- [x] **Subtask 3.4.2.1 Complete**

#### Subtask 3.4.2.2: Run Phase Completion Gates

**Description:** Run call-resolution, recursion, constructor, Core validation,
and source-to-BEAM suites plus `make check-specs`, `make conformance`, and the
complete active EUnit suite.

- [x] **Subtask 3.4.2.2 Complete**
