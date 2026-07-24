# Dialyzer Remediation

**Description:** This roadmap turns Catena's visible repository-wide Dialyzer
inventory into a zero-warning static-analysis boundary without weakening
analysis, suppressing findings wholesale, or trading away the green executable
test and specs-governance baselines.

## Status Rules

- Planned work uses unchecked phase, section, task, and subtask boxes.
- A task is complete only after its focused tests and warning delta are
  recorded.
- A section is complete only when its owned warning families do not regress the
  complete EUnit or specs-governance gates.
- A phase is complete only after a fresh `rebar3 dialyzer` run confirms the
  expected boundary.
- Warning suppression, removed analysis categories, and broad `-dialyzer`
  attributes do not count as remediation.

## Initial Baseline

The roadmap starts from the Phase 7 reconciliation baseline verified on
2026-07-24 with Erlang/OTP 28, ERTS 16.2, and rebar3 3.27.0:

- 4,838 EUnit tests pass
- specs governance passes
- 411 focused conformance tests pass
- Dialyzer reports 777 warnings across 114 source modules

### Warning Families

| Warning family | Initial count |
| --- | ---: |
| Type/specification contracts | 387 |
| No-return and control-flow findings | 124 |
| Missing function/type/callback metadata | 109 |
| Unreachable patterns, variables, and guards | 71 |
| Call-contract mismatches | 60 |
| Ignored return values | 20 |
| Record-field mismatches | 6 |
| **Total** | **777** |

### Ownership Areas

| Initial area | Warnings | Modules |
| --- | ---: | ---: |
| Compiler effects | 224 | 31 |
| Property testing | 199 | 22 |
| Compiler types | 132 | 22 |
| Testing compatibility/bridges | 35 | 3 |
| REPL | 27 | 4 |
| Runtime | 27 | 6 |
| Compiler semantic analysis | 24 | 8 |
| Compiler parser | 23 | 3 |
| Compiler validation | 23 | 1 |
| Compiler AST | 15 | 2 |
| Compiler runtime | 11 | 1 |
| Compiler code generation | 10 | 5 |
| Compiler root modules | 17 | 3 |
| Standard library | 8 | 1 |
| Compiler error modules | 2 | 2 |
| **Total** | **777** | **114** |

These counts describe the initial artifact, not fixed warning quotas. Shared
type exports and callback contracts can remove downstream warnings in several
areas, so every completed section must publish a fresh inventory.

## Remediation Principles

1. Fix implementation behavior when Dialyzer exposes a genuine impossible or
   crashing path.
2. Tighten or widen a specification only when tests and implementation prove
   that the new contract is truthful.
3. Repair shared type ownership before editing repeated downstream symptoms.
4. Keep generated lexer/parser code generated; solve its analysis boundary in
   configuration or maintained wrappers.
5. Preserve all warning categories currently enabled in `rebar.config`.
6. Keep `make verify` green throughout the roadmap.

## Phases

- [ ] [Phase 1: Baseline and Tool Signal](phase-01-baseline-and-tool-signal.md)
- [ ] [Phase 2: Shared Type and Record Contracts](phase-02-shared-type-and-record-contracts.md)
- [ ] [Phase 3: Compiler Frontend and Type Inference](phase-03-compiler-frontend-and-type-inference.md)
- [ ] [Phase 4: Effect System and Compiler Runtime](phase-04-effect-system-and-compiler-runtime.md)
- [ ] [Phase 5: Property Testing and Law Execution](phase-05-property-testing-and-law-execution.md)
- [ ] [Phase 6: Runtime Cleanup and Zero-Warning Enforcement](phase-06-runtime-cleanup-and-zero-warning-enforcement.md)

## Completion Gate

- [ ] A fresh Dialyzer run reports zero warnings
- [ ] No enabled warning category is removed or weakened
- [ ] Two consecutive complete EUnit runs have identical green totals
- [ ] Specs governance and focused conformance remain green
- [ ] CI enforces the zero-warning boundary
- [ ] Promoted status and tooling specs record the final evidence
