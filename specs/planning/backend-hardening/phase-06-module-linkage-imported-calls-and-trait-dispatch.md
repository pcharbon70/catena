# Phase 6: Module Linkage, Imported Calls, And Trait Dispatch

**Description:** This phase turns type-environment imports into executable
module linkage and completes runtime trait dispatch through validated instance
dictionaries, allowing desugared library operations and cross-module calls to
reach BEAM without masquerading as local variables.

**Status:** Planned.

**Dependencies:** Phase 5 complete.

## Section 6.1: Executable Module Identity And Dependency Model

**Description:** Define how Catena source modules, Core Erlang modules, BEAM
module names, imports, exports, and build dependencies relate at artifact time.

- [ ] **Section 6.1 Complete**

### Task 6.1.1: Resolve Source And Runtime Module Identities

**Description:** Give every loaded Catena module one deterministic runtime
module identity and retain its source identity for diagnostics and interfaces.

- [ ] **Task 6.1.1 Complete**

#### Subtask 6.1.1.1: Normalize Module Names

**Description:** Define the mapping for simple and dotted Catena module names
without collisions, atom ambiguity, or ad hoc string concatenation.

- [ ] **Subtask 6.1.1.1 Complete**

#### Subtask 6.1.1.2: Publish Executable Interface Metadata

**Description:** Record exported transforms, arities, constructors, effects,
traits, instances, runtime identities, and artifact dependencies for importers.

- [ ] **Subtask 6.1.1.2 Complete**

### Task 6.1.2: Order Module Compilation

**Description:** Compile module dependency graphs in a deterministic order and
reject unsupported cycles with source-oriented diagnostics.

- [ ] **Task 6.1.2 Complete**

#### Subtask 6.1.2.1: Reuse Dependency Graph Validation

**Description:** Connect existing import graph and cycle detection to the
validated compilation-unit and artifact pipeline.

- [ ] **Subtask 6.1.2.1 Complete**

#### Subtask 6.1.2.2: Track Artifact Dependencies

**Description:** Associate each compiled module with the BEAM modules and
Catena runtime libraries required before it can be loaded or executed.

- [ ] **Subtask 6.1.2.2 Complete**

## Section 6.2: Imported Symbol And Call Resolution

**Description:** Resolve imported calls using the same explicit name, kind,
arity, visibility, and source rules as local calls.

- [ ] **Section 6.2 Complete**

### Task 6.2.1: Resolve Import Forms

**Description:** Convert unqualified, selective, qualified, aliased, and dotted
imports into stable executable symbol references.

- [ ] **Task 6.2.1 Complete**

#### Subtask 6.2.1.1: Resolve Visibility And Selection

**Description:** Enforce exports, selective import lists, local shadowing, and
ambiguity rules against published module interfaces.

- [ ] **Subtask 6.2.1.1 Complete**

#### Subtask 6.2.1.2: Resolve Qualified Names

**Description:** Preserve module qualification and aliases as symbol metadata
rather than confusing qualified calls with record field access.

- [ ] **Subtask 6.2.1.2 Complete**

### Task 6.2.2: Emit Remote Core Erlang Calls

**Description:** Lower resolved imported transforms to Core remote calls with
the correct runtime module, function name, and arity.

- [ ] **Task 6.2.2 Complete**

#### Subtask 6.2.2.1: Emit Direct Imported Calls

**Description:** Generate valid `c_call` targets for imported transforms and
verify the callee module exports the resolved arity.

- [ ] **Subtask 6.2.2.1 Complete**

#### Subtask 6.2.2.2: Represent Imported Functions As Values

**Description:** Use a valid remote closure or eta-expanded representation when
an imported transform is passed or returned as a higher-order value.

- [ ] **Subtask 6.2.2.2 Complete**

## Section 6.3: Trait Validation And Runtime Dictionary Dispatch

**Description:** Complete the runtime-bearing trait and instance path so
library-desugared method calls select explicit, validated implementations.

- [ ] **Section 6.3 Complete**

### Task 6.3.1: Validate Trait And Instance Implementations

**Description:** Connect existing trait, instance, coherence, and method
checking to the validated unit before dictionary generation.

- [ ] **Task 6.3.1 Complete**

#### Subtask 6.3.1.1: Validate Required And Default Methods

**Description:** Check method presence, signatures, defaults, inherited
requirements, and duplicate definitions against the declared trait.

- [ ] **Subtask 6.3.1.1 Complete**

#### Subtask 6.3.1.2: Resolve Instance Coherence

**Description:** Reject missing, overlapping, incoherent, or orphan-invalid
instances according to the promoted trait rules before code generation.

- [ ] **Subtask 6.3.1.2 Complete**

### Task 6.3.2: Emit And Use Trait Dictionaries

**Description:** Generate executable dictionaries with callable method values
and route resolved trait calls through the selected dictionary.

- [ ] **Task 6.3.2 Complete**

#### Subtask 6.3.2.1: Generate Dictionary Artifacts

**Description:** Emit each instance dictionary with stable identity, complete
method fields, valid closures, inherited dictionaries, and dependency metadata.

- [ ] **Subtask 6.3.2.1 Complete**

#### Subtask 6.3.2.2: Lower Trait Method Calls

**Description:** Add required dictionary parameters or resolved dictionary
references and compile method lookup plus invocation without unresolved local
names.

- [ ] **Subtask 6.3.2.2 Complete**

## Section 6.4: Phase 6 Integration Tests

**Description:** Compile and execute multi-module Catena programs with direct
imports, higher-order imported functions, desugared standard-library calls,
and concrete trait dispatch.

- [ ] **Section 6.4 Complete**

### Task 6.4.1: Execute Multi-Module Programs

**Description:** Build dependency-ordered module sets and load their BEAM
artifacts together for observable cross-module execution.

- [ ] **Task 6.4.1 Complete**

#### Subtask 6.4.1.1: Test Import Variants

**Description:** Execute unqualified, qualified, aliased, selective, dotted,
shadowed, and higher-order imported calls.

- [ ] **Subtask 6.4.1.1 Complete**

#### Subtask 6.4.1.2: Test Linkage Failures

**Description:** Reject missing modules, private symbols, ambiguous imports,
wrong arities, identity collisions, and unsupported dependency cycles.

- [ ] **Subtask 6.4.1.2 Complete**

### Task 6.4.2: Execute Trait-Dispatched Programs

**Description:** Run local and imported concrete instances through direct,
inherited, and desugared method calls.

- [ ] **Task 6.4.2 Complete**

#### Subtask 6.4.2.1: Test Dictionary Semantics

**Description:** Execute representative Comparable, Mapper, Applicator,
Pipeline, System, and Flow operations where the current type surface supports
them.

- [ ] **Subtask 6.4.2.1 Complete**

#### Subtask 6.4.2.2: Run Phase Completion Gates

**Description:** Run module, import, linkage, trait, stdlib, Core validation,
and source-to-BEAM suites plus `make check-specs`, `make conformance`, and the
complete active EUnit suite.

- [ ] **Subtask 6.4.2.2 Complete**
