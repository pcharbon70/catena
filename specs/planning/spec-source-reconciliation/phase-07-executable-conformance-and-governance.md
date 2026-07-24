# Phase 7: Executable Conformance and Governance

**Description:** This phase turns Catena's promoted contracts, requirement
catalogs, component acceptance criteria, scenarios, and evidence mappings into
an executable repository boundary. It adds a machine-readable scenario
manifest, validates governance relationships and local links, provides focused
conformance and full verification entry points, enforces them in CI, and
publishes a final repeatable baseline for the reconciliation roadmap.

**Status:** In progress.

**Dependencies:** Phase 6 complete.

## Section 7.1: Scenario Evidence and Catalog Alignment

**Description:** Give every promoted `SCN-*` scenario explicit executable
evidence and reconcile stale contracts, scenario descriptions, component
coverage, and local links before automating the governance rules.

- [x] **Section 7.1 Complete**

### Task 7.1.1: Define Executable Scenario Evidence

**Description:** Add one machine-readable manifest that maps each stable
scenario to maintained EUnit modules and their source files.

- [x] **Task 7.1.1 Complete**

#### Subtask 7.1.1.1: Cover Every Scenario

**Description:** Map all cataloged scenarios from parsing through law
verification to focused tests that exercise their promoted behavior.

- [x] **Subtask 7.1.1.1 Complete**

#### Subtask 7.1.1.2: Document The Evidence Boundary

**Description:** State how the manifest, focused conformance run, and complete
suite complement one another without treating a focused slice as the full
quality gate.

- [x] **Subtask 7.1.1.2 Complete**

### Task 7.1.2: Reconcile Governance Catalogs

**Description:** Align normative contracts, scenario descriptions, component
coverage, and requirement-family summaries with the Phase 6 implementation
baseline.

- [x] **Task 7.1.2 Complete**

#### Subtask 7.1.2.1: Complete Component Coverage

**Description:** Add actor-runtime and Flow component specs to the conformance
matrix so every promoted component containing `AC-*` criteria is represented.

- [x] **Subtask 7.1.2.1 Complete**

#### Subtask 7.1.2.2: Promote Executable Governance Requirements

**Description:** Require maintained conformance evidence and an executable
spec-governance entry point through the testing and observability contracts.

- [x] **Subtask 7.1.2.2 Complete**

#### Subtask 7.1.2.3: Repair Promoted Link Drift

**Description:** Replace the remaining broken local Markdown reference with a
live promoted standard-library specification.

- [x] **Subtask 7.1.2.3 Complete**

**Acceptance Criteria:**

- Every `SCN-*` scenario has at least one maintained EUnit evidence row
- Evidence rows name both the EUnit module and its repository source path
- Every component spec containing `AC-*` criteria appears in the matrix
- Contracts require executable governance and traceable scenario evidence
- Promoted local Markdown references resolve

## Section 7.2: Executable Specs Governance Gate

**Description:** Implement a deterministic checker and conformance runner that
enforce the relationships declared in Section 7.1 and expose them through
canonical repository commands.

- [ ] **Section 7.2 Complete**

### Task 7.2.1: Validate Governance Relationships

**Description:** Check requirement families, scenario definitions and
references, evidence rows, component acceptance-criteria mappings, ADR
cataloging, and local Markdown links.

- [ ] **Task 7.2.1 Complete**

#### Subtask 7.2.1.1: Implement Structured Validation

**Description:** Return stable success counts or actionable typed errors from
a reusable Erlang governance module.

- [ ] **Subtask 7.2.1.1 Complete**

#### Subtask 7.2.1.2: Verify Failure Detection

**Description:** Add focused tests for the live repository and deliberately
invalid fixture repositories.

- [ ] **Subtask 7.2.1.2 Complete**

### Task 7.2.2: Expose Canonical Commands

**Description:** Provide command-line and Make entry points for governance-only
checks, focused executable scenarios, and the complete verification workflow.

- [ ] **Task 7.2.2 Complete**

#### Subtask 7.2.2.1: Add The Specs Checker

**Description:** Add a CLI wrapper that prints the validated governance counts
and exits nonzero with actionable errors.

- [ ] **Subtask 7.2.2.1 Complete**

#### Subtask 7.2.2.2: Add Conformance And Verify Targets

**Description:** Run manifest-selected EUnit modules for conformance and
compose specs plus the complete test suite for full verification.

- [ ] **Subtask 7.2.2.2 Complete**

**Acceptance Criteria:**

- The checker rejects missing, duplicate, or unknown governance identifiers
- Manifest evidence paths and module declarations are verified
- Every `AC-*` component and ADR is cataloged
- `make check-specs` validates governance without running the complete suite
- `make conformance` runs every manifest-selected module
- `make verify` combines governance and the complete active suite

## Section 7.3: CI Enforcement and Final Baseline

**Description:** Enforce the canonical verification workflow for pushes and
pull requests, reconcile promoted status/tooling documents, and publish the
final Phase 7 evidence.

- [ ] **Section 7.3 Complete**

### Task 7.3.1: Enforce The Verification Workflow

**Description:** Add a least-privilege GitHub Actions workflow that provisions
the supported Erlang/Rebar versions and runs the repository's canonical verify
target.

- [ ] **Task 7.3.1 Complete**

#### Subtask 7.3.1.1: Add Pull-Request And Main-Branch CI

**Description:** Run verification for pull requests and pushes to `main`
without granting write permissions.

- [ ] **Subtask 7.3.1.1 Complete**

### Task 7.3.2: Publish Phase 7 Evidence

**Description:** Run the governance, conformance, complete-suite, compile, and
static-analysis gates and record their exact results.

- [ ] **Task 7.3.2 Complete**

#### Subtask 7.3.2.1: Reconcile Promoted Status And Tooling

**Description:** Replace stale non-green and pre-runner descriptions with the
implemented governance, green-suite, and remaining static-analysis boundaries.

- [ ] **Subtask 7.3.2.1 Complete**

#### Subtask 7.3.2.2: Create The Final Test Baseline

**Description:** Record focused governance/conformance totals, two complete
suite runs, tool versions, and remaining non-EUnit gaps.

- [ ] **Subtask 7.3.2.2 Complete**

**Acceptance Criteria:**

- CI invokes the same `make verify` command used locally
- The workflow uses read-only repository permissions
- Governance, conformance, compile, and complete-suite gates pass
- Two complete-suite runs have identical green totals
- Dialyzer status remains visible
- Current status, targets, tooling, and governance docs match executable evidence

## Phase Completion Gate

**Description:** Phase 7 completes when promoted specs are linked to
maintained executable evidence, governance drift fails locally and in CI, and
the complete reconciliation roadmap has a repeatable final baseline.

- [ ] Every scenario maps to maintained executable evidence
- [ ] Requirements, scenarios, components, ADRs, and links pass governance
- [ ] Canonical local commands expose specs, conformance, and full verification
- [ ] CI enforces the complete verification command
- [ ] Final status and baseline documents match repeatable repository evidence
