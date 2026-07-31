# Phase 1: Baseline and Tool Signal

**Description:** This phase reconciles the historical 777-warning snapshot
with the current merged source, makes the resulting baseline reproducible, and
separates tool/configuration noise from source-contract defects so later
phases work against stable, comparable evidence.

**Status:** Complete.

**Dependencies:** Spec-source reconciliation Phase 7 complete.

## Section 1.1: Reproducible Warning Inventory

**Description:** Add a maintained inventory command that classifies every
Dialyzer finding by warning family, ownership area, module, and source
location.

- [x] **Section 1.1 Complete**

### Task 1.1.1: Implement Deterministic Classification

**Description:** Parse the Dialyzer warning artifact into stable totals without
depending on ANSI output, absolute workspace paths, or ad hoc shell history.

- [x] **Task 1.1.1 Complete**

#### Subtask 1.1.1.1: Normalize Warning Records

**Description:** Capture source path, line, warning family, and leading
diagnostic text for both ordinary and line-zero unknown-function warnings.

- [x] **Subtask 1.1.1.1 Complete**

#### Subtask 1.1.1.2: Report Ownership Summaries

**Description:** Produce deterministic family, directory, module, and total
counts suitable for phase baselines and pull-request evidence.

- [x] **Subtask 1.1.1.2 Complete**

### Task 1.1.2: Reconcile The Published Baseline

**Description:** Preserve the documented 777-warning, 114-module Phase 7
snapshot as historical evidence and establish the fresh post-Phase 8 input
before remediation begins.

- [x] **Task 1.1.2 Complete**

#### Subtask 1.1.2.1: Add Classifier Regression Fixtures

**Description:** Test multiline diagnostics, unknown functions, absolute-path
normalization, category detection, and stable sorting.

- [x] **Subtask 1.1.2.1 Complete**

#### Subtask 1.1.2.2: Publish Phase 1 Input Evidence

**Description:** Record tool versions, exact commands, warning categories, and
the relationship to the green Phase 7 test baseline.

- [x] **Subtask 1.1.2.2 Complete**

### Section 1.1 Evidence

Evidence captured on 2026-07-31 from merge commit `20159d7` plus the
inventory implementation:

- Erlang/OTP 28, ERTS 16.2, and rebar3 3.27.0.
- `make dialyzer-inventory` performs a fresh `rebar3 dialyzer` analysis and
  deterministically reports normalized paths, locations, modules,
  directories, ownership areas, and warning families.
- The fresh post-Phase 8 baseline is 984 warnings across 139 source modules.
- The historical 777-warning, 114-module Phase 7 snapshot is not reproducible
  from the later merged source because the completed delimited-resumption
  phases added maintained compiler, runtime, tooling, and test surfaces. It
  remains retained in the roadmap as lineage rather than being overwritten.
- The input remains anchored to the Phase 8 green gates: `make conformance`
  passed 432 tests and `make test` passed 5,294 tests.
- `rebar3 eunit --module=catena_dialyzer_inventory_tests` passes all seven
  classifier regression tests, and focused coverage is 92% for the maintained
  classifier module.
- After adding those tests, the section integration gate passed all 432
  conformance tests and all 5,301 complete-suite tests.

| Warning family | Section 1.1 count |
| --- | ---: |
| Type/specification contracts | 480 |
| Unreachable patterns, variables, and guards | 181 |
| Missing function/type/callback metadata | 120 |
| No-return and control-flow findings | 102 |
| Call-contract mismatches | 75 |
| Ignored return values | 20 |
| Record-field mismatches | 6 |
| **Total** | **984** |

| Ownership area | Warnings | Modules |
| --- | ---: | ---: |
| Compiler effects | 242 | 32 |
| Property testing | 199 | 22 |
| Compiler types | 153 | 28 |
| Compiler code generation | 113 | 13 |
| Compiler semantic analysis | 44 | 12 |
| REPL | 37 | 5 |
| Testing compatibility/bridges | 35 | 3 |
| Compiler runtime | 33 | 2 |
| Runtime | 29 | 7 |
| Compiler parser | 23 | 3 |
| Compiler validation | 23 | 1 |
| Compiler root modules | 20 | 4 |
| Compiler AST | 15 | 2 |
| Standard library | 8 | 1 |
| Tooling | 8 | 2 |
| Compiler error modules | 2 | 2 |
| **Total** | **984** | **139** |

## Section 1.2: Tool-Induced Unknown Boundaries

**Description:** Remove findings caused by incomplete PLT/application metadata
or excluded generated modules while keeping the configured analysis strict.

- [x] **Section 1.2 Complete**

### Task 1.2.1: Reconcile PLT Application Coverage

**Description:** Make deliberate PLT decisions for EUnit and other OTP
applications referenced by analyzed source modules.

- [x] **Task 1.2.1 Complete**

#### Subtask 1.2.1.1: Resolve EUnit Unknown Calls

**Description:** Eliminate the eight `eunit:test/1` unknown-function findings
through correct application metadata rather than call suppression.

- [x] **Subtask 1.2.1.1 Complete**

#### Subtask 1.2.1.2: Verify PLT Rebuild Reproducibility

**Description:** Confirm a clean PLT build and a cached repeat produce the same
warning inventory.

- [x] **Subtask 1.2.1.2 Complete**

### Task 1.2.2: Define The Generated Frontend Boundary

**Description:** Preserve generated lexer/parser exclusions while giving
maintained callers truthful callable contracts.

- [x] **Task 1.2.2 Complete**

#### Subtask 1.2.2.1: Audit Generated Call Sites

**Description:** Inventory `catena_lexer` and `catena_parser` calls and decide
whether maintained wrappers, analysis stubs, or generated-module analysis best
represents the boundary.

- [x] **Subtask 1.2.2.1 Complete**

#### Subtask 1.2.2.2: Remove Generated Unknown Findings

**Description:** Eliminate lexer/parser unknown-function warnings without
editing generated `.erl` outputs or disabling the `unknown` warning category.

- [x] **Subtask 1.2.2.2 Complete**

### Section 1.2 Evidence

Evidence captured on 2026-07-31:

- `eunit` is an explicit `plt_extra_apps` entry. All eight former
  `eunit:test/1` unknown-function findings are absent without suppression.
- Generated `catena_lexer` and `catena_parser` modules remain excluded from
  analysis and generated `.erl` files remain untouched.
- The maintained `catena_generated_frontend` boundary gives scanning,
  tokenization, parsing, and lexer-error formatting explicit contracts. All 18
  maintained direct generated-module calls now use that boundary: four in
  compiler root modules, five in compiler parser modules, four in semantic
  compilation, and five in the REPL.
- `rebar3 eunit --module=catena_generated_frontend_tests` passes all four
  boundary-equivalence tests with 100% coverage of the wrapper module.
- A clean temporary base/project PLT analysis and an immediate cached repeat
  produced byte-identical inventories.
- The 26 tool-induced missing-metadata findings are gone. The explicit
  generated-frontend contracts expose two existing caller specification
  mismatches that were previously hidden by unknown return types, so the net
  phase delta is 24 warnings rather than suppressing those real findings.
- `unmatched_returns`, `error_handling`, `underspecs`, and `unknown` remain
  enabled exactly as before.
- The phase integration gate passes specs governance, all 432 conformance
  tests, and all 5,305 complete-suite tests.

| Warning family | Section 1.1 input | Phase 1 ending | Delta |
| --- | ---: | ---: | ---: |
| Type/specification contracts | 480 | 482 | +2 |
| Unreachable patterns, variables, and guards | 181 | 181 | 0 |
| Missing function/type/callback metadata | 120 | 94 | -26 |
| No-return and control-flow findings | 102 | 102 | 0 |
| Call-contract mismatches | 75 | 75 | 0 |
| Ignored return values | 20 | 20 | 0 |
| Record-field mismatches | 6 | 6 | 0 |
| **Total** | **984** | **960** | **-24** |

The exact Phase 2 input is therefore **960 warnings across 138 source
modules**.

**Acceptance Criteria:**

- One repository command produces a deterministic warning inventory
- The historical 777-warning artifact is reconciled and the current
  984-warning input is reproducible before fixes
- Tool-induced EUnit and generated-frontend unknowns are resolved truthfully
- Warning categories remain unchanged
- `make verify` remains green
- The phase publishes its exact ending warning count
