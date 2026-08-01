# Phase 1: Baseline and Tool Signal

**Description:** This phase makes the 777-warning baseline reproducible and
separates tool/configuration noise from source-contract defects so later phases
work against stable, comparable evidence.

**Status:** Planned.

**Dependencies:** Spec-source reconciliation Phase 7 complete.

## Section 1.1: Reproducible Warning Inventory

**Description:** Add a maintained inventory command that classifies every
Dialyzer finding by warning family, ownership area, module, and source
location.

- [ ] **Section 1.1 Complete**

### Task 1.1.1: Implement Deterministic Classification

**Description:** Parse the Dialyzer warning artifact into stable totals without
depending on ANSI output, absolute workspace paths, or ad hoc shell history.

- [ ] **Task 1.1.1 Complete**

#### Subtask 1.1.1.1: Normalize Warning Records

**Description:** Capture source path, line, warning family, and leading
diagnostic text for both ordinary and line-zero unknown-function warnings.

- [ ] **Subtask 1.1.1.1 Complete**

#### Subtask 1.1.1.2: Report Ownership Summaries

**Description:** Produce deterministic family, directory, module, and total
counts suitable for phase baselines and pull-request evidence.

- [ ] **Subtask 1.1.1.2 Complete**

### Task 1.1.2: Verify The Published Baseline

**Description:** Assert that a fresh analysis reproduces the documented
777-warning, 114-module starting point before remediation begins.

- [ ] **Task 1.1.2 Complete**

#### Subtask 1.1.2.1: Add Classifier Regression Fixtures

**Description:** Test multiline diagnostics, unknown functions, absolute-path
normalization, category detection, and stable sorting.

- [ ] **Subtask 1.1.2.1 Complete**

#### Subtask 1.1.2.2: Publish Phase 1 Input Evidence

**Description:** Record tool versions, exact commands, warning categories, and
the relationship to the green Phase 7 test baseline.

- [ ] **Subtask 1.1.2.2 Complete**

## Section 1.2: Tool-Induced Unknown Boundaries

**Description:** Remove findings caused by incomplete PLT/application metadata
or excluded generated modules while keeping the configured analysis strict.

- [ ] **Section 1.2 Complete**

### Task 1.2.1: Reconcile PLT Application Coverage

**Description:** Make deliberate PLT decisions for EUnit and other OTP
applications referenced by analyzed source modules.

- [ ] **Task 1.2.1 Complete**

#### Subtask 1.2.1.1: Resolve EUnit Unknown Calls

**Description:** Eliminate the eight `eunit:test/1` unknown-function findings
through correct application metadata rather than call suppression.

- [ ] **Subtask 1.2.1.1 Complete**

#### Subtask 1.2.1.2: Verify PLT Rebuild Reproducibility

**Description:** Confirm a clean PLT build and a cached repeat produce the same
warning inventory.

- [ ] **Subtask 1.2.1.2 Complete**

### Task 1.2.2: Define The Generated Frontend Boundary

**Description:** Preserve generated lexer/parser exclusions while giving
maintained callers truthful callable contracts.

- [ ] **Task 1.2.2 Complete**

#### Subtask 1.2.2.1: Audit Generated Call Sites

**Description:** Inventory `catena_lexer` and `catena_parser` calls and decide
whether maintained wrappers, analysis stubs, or generated-module analysis best
represents the boundary.

- [ ] **Subtask 1.2.2.1 Complete**

#### Subtask 1.2.2.2: Remove Generated Unknown Findings

**Description:** Eliminate lexer/parser unknown-function warnings without
editing generated `.erl` outputs or disabling the `unknown` warning category.

- [ ] **Subtask 1.2.2.2 Complete**

**Acceptance Criteria:**

- One repository command produces a deterministic warning inventory
- The initial 777-warning artifact is reproducible before fixes
- Tool-induced EUnit and generated-frontend unknowns are resolved truthfully
- Warning categories remain unchanged
- `make verify` remains green
- The phase publishes its exact ending warning count
