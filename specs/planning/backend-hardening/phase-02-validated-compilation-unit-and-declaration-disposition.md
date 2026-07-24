# Phase 2: Validated Compilation Unit And Declaration Disposition

**Description:** This phase replaces the loose analyzed-AST backend handoff
with a validated compilation unit that keeps normalized source, typed results,
symbol metadata, declaration dispositions, options, and source locations
together through code generation.

**Status:** Planned.

**Dependencies:** Phase 1 complete.

## Section 2.1: Validated Compilation Unit Model

**Description:** Define the authoritative backend input and the invariants that
must hold before Core Erlang lowering may begin.

- [ ] **Section 2.1 Complete**

### Task 2.1.1: Define The Compilation Unit Contract

**Description:** Add a maintained type and constructor API for validated
backend input rather than passing unrelated analyzed and typed artifacts
separately.

- [ ] **Task 2.1.1 Complete**

#### Subtask 2.1.1.1: Model Required Unit Fields

**Description:** Carry module identity, normalized AST, typed declarations,
effective type environment, imports, exports, compiler options, and source
identity.

- [ ] **Subtask 2.1.1.1 Complete**

#### Subtask 2.1.1.2: Model Validation State

**Description:** Represent which frontend validations produced the unit and
prevent construction of a backend-authoritative unit from unchecked AST terms.

- [ ] **Subtask 2.1.1.2 Complete**

### Task 2.1.2: Define Symbol And Location Metadata

**Description:** Give later call resolution and diagnostics a stable inventory
of names, arities, declaration kinds, modules, and original locations.

- [ ] **Task 2.1.2 Complete**

#### Subtask 2.1.2.1: Build Module Symbol Entries

**Description:** Record transforms, constructors, effects, trait methods,
instances, imports, exports, and non-runtime declarations with their kinds and
arities.

- [ ] **Subtask 2.1.2.1 Complete**

#### Subtask 2.1.2.2: Preserve Source Locations

**Description:** Retain module, declaration, clause, pattern, expression, and
import locations across normalized and typed views.

- [ ] **Subtask 2.1.2.2 Complete**

## Section 2.2: Compiler Orchestration Handoff

**Description:** Make `catena_compile` construct and pass the validated unit
after all frontend gates while keeping the typed-module and Core APIs
compatible.

- [ ] **Section 2.2 Complete**

### Task 2.2.1: Produce The Unit From The Canonical Frontend

**Description:** Join semantic output, import results, kind validation, type
checking, effect validation, and symbol collection into one success artifact.

- [ ] **Task 2.2.1 Complete**

#### Subtask 2.2.1.1: Refactor Frontend Success Assembly

**Description:** Reuse one orchestration path for typed-module, Core Erlang,
and later BEAM APIs without repeating or skipping validation.

- [ ] **Subtask 2.2.1.1 Complete**

#### Subtask 2.2.1.2: Preserve Existing Public Results

**Description:** Keep the documented typed-module return shape stable or add a
deliberate compatibility adapter while the validated unit remains internal.

- [ ] **Subtask 2.2.1.2 Complete**

### Task 2.2.2: Make Code Generation Consume The Unit

**Description:** Route the public source-to-Core path through the validated
unit and make low-level AST helpers explicitly internal or test-only.

- [ ] **Task 2.2.2 Complete**

#### Subtask 2.2.2.1: Add The Validated Backend Entry Point

**Description:** Introduce a code-generation entry point whose input contract
requires validated symbols, types, dispositions, and locations.

- [ ] **Subtask 2.2.2.1 Complete**

#### Subtask 2.2.2.2: Scope Raw-AST Codegen Helpers

**Description:** Rename, document, or guard raw backend helpers so callers
cannot mistake them for the safe production compilation boundary.

- [ ] **Subtask 2.2.2.2 Complete**

## Section 2.3: Declaration Disposition Pass

**Description:** Classify every module declaration before type erasure or
function filtering so omission always has an explicit semantic reason.

- [ ] **Section 2.3 Complete**

### Task 2.3.1: Classify Static And Executable Declarations

**Description:** Assign `lowered`, `erased_static`, `runtime_lowered`, or
`unsupported` to every canonical declaration form.

- [ ] **Task 2.3.1 Complete**

#### Subtask 2.3.1.1: Classify Transforms And Static Metadata

**Description:** Define dispositions for implemented transforms, signatures
without implementations, type declarations, effect declarations, imports, and
exports.

- [ ] **Subtask 2.3.1.1 Complete**

#### Subtask 2.3.1.2: Classify Runtime-Bearing Declarations

**Description:** Define provisional dispositions for traits, instances, tests,
properties, and any future actor/process declarations without silently
promoting incomplete lowering.

- [ ] **Subtask 2.3.1.2 Complete**

### Task 2.3.2: Enforce Disposition Before Erasure

**Description:** Require a valid disposition and any representation metadata
before a declaration can be erased, lowered, or rejected.

- [ ] **Task 2.3.2 Complete**

#### Subtask 2.3.2.1: Order Representation Selection Before Erasure

**Description:** Preserve constructor, effect-operation, trait-method, and
import-linkage metadata before removing static declarations.

- [ ] **Subtask 2.3.2.1 Complete**

#### Subtask 2.3.2.2: Reject Missing Runtime Implementations

**Description:** Reject exported signature-only transforms and deferred
runtime-bearing declarations when application artifact generation would
otherwise omit required behavior.

- [ ] **Subtask 2.3.2.2 Complete**

## Section 2.4: Phase 2 Integration Tests

**Description:** Verify that every public Core compilation is driven by one
validated unit and that every declaration receives a reviewable disposition
before module emission.

- [ ] **Section 2.4 Complete**

### Task 2.4.1: Test Validated Unit Construction

**Description:** Exercise successful and failing frontend paths through the new
unit assembly boundary.

- [ ] **Task 2.4.1 Complete**

#### Subtask 2.4.1.1: Verify Unit Contents

**Description:** Assert that normalized AST, typed results, symbol entries,
imports, exports, dispositions, options, and locations agree for representative
modules.

- [ ] **Subtask 2.4.1.1 Complete**

#### Subtask 2.4.1.2: Verify Invalid Source Cannot Produce A Unit

**Description:** Confirm every frontend validation family prevents validated
unit construction and backend invocation.

- [ ] **Subtask 2.4.1.2 Complete**

### Task 2.4.2: Test Declaration Disposition Enforcement

**Description:** Exercise implemented, static-only, runtime-backed, and
deferred declarations through public Core compilation.

- [ ] **Task 2.4.2 Complete**

#### Subtask 2.4.2.1: Verify Explicit Erasure And Rejection

**Description:** Confirm static declarations erase only after metadata capture
and unsupported runtime declarations fail with source-oriented diagnostics.

- [ ] **Subtask 2.4.2.1 Complete**

#### Subtask 2.4.2.2: Run Phase Completion Gates

**Description:** Run validated-unit, compiler, codegen, and declaration suites
plus `make check-specs`, `make conformance`, and the complete active EUnit
suite.

- [ ] **Subtask 2.4.2.2 Complete**
