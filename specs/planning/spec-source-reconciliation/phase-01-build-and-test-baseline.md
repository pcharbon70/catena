# Phase 1: Build and Test Baseline

**Description:** This phase establishes one truthful repository topology in
which every active Erlang module has a unique name, canonical commands expose
the complete active test surface, and remaining failures are recorded instead
of being hidden behind partial test entry points.

**Status:** Complete.

**Dependencies:** None.

## Section 1.1: Module and Namespace Integrity

**Description:** Remove BEAM module-name collisions so compiler, runtime, and
property-testing modules can coexist in one build without silently overwriting
one another.

- [x] **Section 1.1 Complete**

### Task 1.1.1: Separate Compiler AST Namespaces

**Description:** Preserve `catena_ast` for the full compiler AST and give the
inference-only AST vocabulary an explicit, non-conflicting module identity.

- [x] **Task 1.1.1 Complete**

**Subtasks:**

- [x] 1.1.1.1 Rename the inference-only module to `catena_infer_ast`
- [x] 1.1.1.2 Update inference typespec references
- [x] 1.1.1.3 Verify both AST modules compile in the same output directory

### Task 1.1.2: Separate Process Support Namespaces

**Description:** Preserve `catena_process` for runtime process primitives and
give the property-testing harness, header, and tests explicit property-oriented
names.

- [x] **Task 1.1.2 Complete**

**Subtasks:**

- [x] 1.1.2.1 Rename the property harness to `catena_prop_process`
- [x] 1.1.2.2 Rename its header, registry, and EUnit module
- [x] 1.1.2.3 Update all property-testing callers

### Task 1.1.3: Enforce Unique Module Names

**Description:** Add a deterministic repository check that fails when two
active source or test files declare the same Erlang module.

- [x] **Task 1.1.3 Complete**

**Subtasks:**

- [x] 1.1.3.1 Index module declarations under `src/` and `test/`
- [x] 1.1.3.2 Report every path participating in a collision
- [x] 1.1.3.3 Run the check after the namespace migration

**Acceptance Criteria:**

- The module-name checker reports no duplicates under `src/` and `test/`
- Both AST implementations compile without overwriting the same BEAM file
- Runtime and property process suites resolve their intended APIs

## Section 1.2: Canonical Build and Test Entry Points

**Description:** Make rebar3 authoritative for complete compilation and active
EUnit discovery, with Make targets acting as transparent wrappers.

- [x] **Section 1.2 Complete**

### Task 1.2.1: Complete Rebar Test Discovery

**Description:** Include every maintained test directory and correct generated
module coverage configuration.

- [x] **Task 1.2.1 Complete**

**Subtasks:**

- [x] 1.2.1.1 Discover maintained tests recursively from the `test/` profile
- [x] 1.2.1.2 Generate lexer and parser modules through native Rebar stages
- [x] 1.2.1.3 Exclude the actual generated modules from coverage

### Task 1.2.2: Replace Partial Make Targets

**Description:** Remove manually enumerated source and test subsets from the
Makefile and delegate canonical operations to rebar3.

- [x] **Task 1.2.2 Complete**

**Subtasks:**

- [x] 1.2.2.1 Delegate compilation to `rebar3 compile`
- [x] 1.2.2.2 Delegate the active suite to `rebar3 eunit`
- [x] 1.2.2.3 Preserve module-name checks on compile, test, and coverage paths

### Task 1.2.3: Document Canonical Commands

**Description:** Align contributor guidance with the commands that actually
compile and test the complete active repository.

- [x] **Task 1.2.3 Complete**

**Subtasks:**

- [x] 1.2.3.1 Identify rebar3 as the canonical entry point
- [x] 1.2.3.2 Describe Make targets as convenience wrappers
- [x] 1.2.3.3 Explain native lexer and parser generation

**Acceptance Criteria:**

- `make compile` compiles the complete active source tree through rebar3
- `make test` discovers every maintained EUnit directory recursively
- Generated lexer and parser modules are ignored rather than versioned

## Section 1.3: Stable Failure Baseline

**Description:** Ensure failures are observable and reproducible, then publish
the remaining baseline without claiming the repository is green.

- [x] **Section 1.3 Complete**

### Task 1.3.1: Remove Swallowed Test Failures

**Description:** Replace permissive tests that convert unexpected parser or
compiler errors into success with explicit assertions.

- [x] **Task 1.3.1 Complete**

**Subtasks:**

- [x] 1.3.1.1 Replace permissive parser branches with explicit assertions
- [x] 1.3.1.2 Make effect-type validation assert its expected success result
- [x] 1.3.1.3 Verify unsupported syntax is reported as a test failure

### Task 1.3.2: Stabilize Shared Test State

**Description:** Isolate process registries, mailboxes, and lifecycle fixtures
so the full suite produces deterministic results.

- [x] **Task 1.3.2 Complete**

**Subtasks:**

- [x] 1.3.2.1 Run process-local effect setup inside each EUnit test process
- [x] 1.3.2.2 Isolate runtime mailboxes and registered process names
- [x] 1.3.2.3 Seed weighted state-machine sampling deterministically

### Task 1.3.3: Publish the Verified Baseline

**Description:** Record the canonical command results and revise promoted
tooling/status claims to match the evidence.

- [x] **Task 1.3.3 Complete**

**Subtasks:**

- [x] 1.3.3.1 Run the canonical suite twice and compare results
- [x] 1.3.3.2 Record every remaining failing module and owning phase
- [x] 1.3.3.3 Remove promoted claims that the default suite is green

**Acceptance Criteria:**

- Two consecutive `make test` runs report the same totals
- The verified baseline names every failing module
- Remaining failures are assigned to later reconciliation phases
- Promoted quality and tooling documents link to the baseline

## Phase Completion Gate

**Description:** Phase 1 completes when all active modules compile from the
canonical entry point, the full active suite has a deterministic result, and
every remaining failure is assigned to a later reconciliation phase.

- [x] All active source and test modules compile
- [x] The canonical full-suite command is deterministic
- [x] No test silently accepts an unexpected failure
- [x] Current-status and tooling specs match the verified baseline

**Verified Result:** See the
[Phase 1 test baseline](phase-01-test-baseline.md) for the commands, totals,
failure inventory, and ownership assignments.
