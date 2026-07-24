# Phase 7: Public BEAM API And Conformance Enforcement

**Description:** This phase exposes the validated in-memory BEAM artifact API,
maps OTP failures back to Catena source, completes positive and negative
`SCN-011` evidence, and makes the hardened backend boundary enforceable in the
maintained workflow.

**Status:** Planned.

**Dependencies:** Phase 6 complete.

## Section 7.1: Validated In-Memory BEAM Artifact API

**Description:** Add public string and file APIs that reuse the validated
compilation unit and return only OTP-accepted BEAM binaries.

- [x] **Section 7.1 Complete**

### Task 7.1.1: Implement Source-To-BEAM Entry Points

**Description:** Add `compile_string_to_beam/1,2` and
`compile_file_to_beam/1,2` without duplicating or bypassing frontend and backend
validation.

- [x] **Task 7.1.1 Complete**

#### Subtask 7.1.1.1: Define The Success Artifact

**Description:** Return module identity, BEAM binary, source identity, runtime
dependencies, warnings, and relevant artifact metadata in a documented shape.

- [x] **Subtask 7.1.1.1 Complete**

#### Subtask 7.1.1.2: Reuse Existing Compiler Options

**Description:** Preserve import environments, search paths, source filenames,
and backend options consistently across typed-module, Core Erlang, and BEAM
entry points.

- [x] **Subtask 7.1.1.2 Complete**

### Task 7.1.2: Validate Core And Compile BEAM

**Description:** Run explicit Core validation and OTP `from_core` compilation
as mandatory steps before returning a BEAM artifact.

- [x] **Task 7.1.2 Complete**

#### Subtask 7.1.2.1: Add Core Validation Boundary

**Description:** Detect unbound variables, malformed clauses, invalid exports,
bad call targets, and other Core lint failures before artifact success.

- [x] **Subtask 7.1.2.1 Complete**

#### Subtask 7.1.2.2: Compile In Memory With OTP

**Description:** Use `compile:forms` with `from_core`, `binary`, structured
errors, and structured warnings and return no partial success on failure.

- [x] **Subtask 7.1.2.2 Complete**

## Section 7.2: Source-Oriented Artifact Diagnostics

**Description:** Preserve enough origin metadata to report backend, Core, and
BEAM failures in Catena vocabulary at the responsible source construct.

- [ ] **Section 7.2 Complete**

### Task 7.2.1: Map Generated Forms To Source Origins

**Description:** Track the source module, transform, clause, expression,
pattern, and location associated with generated function names and Core nodes.

- [ ] **Task 7.2.1 Complete**

#### Subtask 7.2.1.1: Annotate Generated Core

**Description:** Attach file, line, and generated-origin annotations supported
by OTP without exposing unstable internal terms as the public diagnostic
contract.

- [ ] **Subtask 7.2.1.1 Complete**

#### Subtask 7.2.1.2: Retain Synthetic-Origin Metadata

**Description:** Distinguish user-written nodes from compiler-generated
matches, parameters, closures, dictionaries, and runtime wrappers.

- [ ] **Subtask 7.2.1.2 Complete**

### Task 7.2.2: Normalize OTP Diagnostics

**Description:** Translate Core lint and BEAM compiler errors and warnings into
structured Catena compiler diagnostics while retaining the original OTP detail.

- [ ] **Task 7.2.2 Complete**

#### Subtask 7.2.2.1: Map Core Validation Failures

**Description:** Convert unbound names, invalid arities, malformed clauses, and
invalid exports to the closest Catena source identity and diagnostic category.

- [ ] **Subtask 7.2.2.1 Complete**

#### Subtask 7.2.2.2: Map BEAM Compilation Failures And Warnings

**Description:** Preserve severity, source context, OTP reason, and actionable
Catena formatting for compiler output.

- [ ] **Subtask 7.2.2.2 Complete**

## Section 7.3: Backend Conformance And Workflow Enforcement

**Description:** Complete the source-to-BEAM feature matrix, prove deferred
surfaces fail closed, and wire `SCN-011` into the maintained verification
workflow.

- [ ] **Section 7.3 Complete**

### Task 7.3.1: Build The Dedicated Backend Conformance Suite

**Description:** Consolidate positive execution evidence for every promoted
supported row in the backend representation table.

- [ ] **Task 7.3.1 Complete**

#### Subtask 7.3.1.1: Cover The Supported Feature Matrix

**Description:** Execute local and imported calls, recursion, higher-order
functions, data representations, patterns, operators, effects, and traits from
source through loaded BEAM.

- [ ] **Subtask 7.3.1.1 Complete**

#### Subtask 7.3.1.2: Cover Artifact And Diagnostic Paths

**Description:** Exercise string and file APIs, options, warnings, runtime
dependencies, Core validation, BEAM compilation, and source-oriented
diagnostics.

- [ ] **Subtask 7.3.1.2 Complete**

### Task 7.3.2: Enforce Deferred-Surface Rejection

**Description:** Add negative evidence for any frontend or research surface
that still lacks an accepted executable backend contract.

- [ ] **Task 7.3.2 Complete**

#### Subtask 7.3.2.1: Reject Deferred Declarations Explicitly

**Description:** Verify test and property declarations either use an accepted
testing artifact contract or fail application emission, and actor/process
constructs remain outside backend support until source integration exists.

- [ ] **Subtask 7.3.2.1 Complete**

#### Subtask 7.3.2.2: Wire Conformance Into Governance And CI

**Description:** Point `SCN-011` at the dedicated evidence module, update
catalog totals and matrices, and require the scenario in maintained
verification commands.

- [ ] **Subtask 7.3.2.2 Complete**

## Section 7.4: Phase 7 Integration Tests

**Description:** Verify the complete hardened backend story from source input
through validated BEAM execution, negative rejection, diagnostics, governance,
and repository-wide regression gates.

- [ ] **Section 7.4 Complete**

### Task 7.4.1: Execute The Full Backend Story

**Description:** Run representative single-module, multi-module, pure,
effectful, trait-dispatched, recursive, and higher-order programs through the
public BEAM APIs.

- [ ] **Task 7.4.1 Complete**

#### Subtask 7.4.1.1: Validate Artifact Loading And Execution

**Description:** Load returned binaries with dependencies, call exported
transforms, assert observable results, and clean loaded modules and runtime
state deterministically.

- [ ] **Subtask 7.4.1.1 Complete**

#### Subtask 7.4.1.2: Validate Negative Artifact Boundaries

**Description:** Confirm no invalid, unsupported, unresolved, Core-invalid, or
BEAM-invalid program returns a success artifact or leaves partial runtime
state.

- [ ] **Subtask 7.4.1.2 Complete**

### Task 7.4.2: Publish The Hardened Backend Baseline

**Description:** Run all maintained gates and update promoted status from a
vertical slice to the exact feature set proven by executable conformance.

- [ ] **Task 7.4.2 Complete**

#### Subtask 7.4.2.1: Run Complete Verification

**Description:** Run focused backend tests, `make check-specs`,
`make conformance`, the complete active EUnit suite, coverage for modified
modules, and Dialyzer according to the repository's current enforcement state.

- [ ] **Subtask 7.4.2.1 Complete**

#### Subtask 7.4.2.2: Reconcile Specs And Evidence

**Description:** Update compiler, tooling, targets, current status, feature
ledger, conformance manifest, acceptance criteria, and ADR consequences with
the final verified behavior and exact test totals.

- [ ] **Subtask 7.4.2.2 Complete**
