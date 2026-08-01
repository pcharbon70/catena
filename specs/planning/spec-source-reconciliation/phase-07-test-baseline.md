# Phase 7 Test Baseline

## Purpose

This baseline records the executable governance, scenario conformance, complete
EUnit, compile, and static-analysis results after promoted specs were connected
to maintained repository evidence and the canonical verification command was
added to CI. It supersedes the Phase 6 baseline as the current quality
snapshot.

## Verification

The baseline was verified on 2026-07-24 with Erlang/OTP 28, ERTS 16.2, and
rebar3 3.27.0.

| Command | Result |
| --- | --- |
| `make check-specs` | Pass; 35 requirements in 5 families, 10 scenarios, 19 evidence rows/modules, 63 acceptance criteria across 10 component specs, 4 ADRs, and 154 local Markdown links |
| `make conformance` | 411 passed across 19 manifest-selected modules, 0 failed, 0 skipped |
| `make compile` | Pass; all active source modules compile and module names are unique |
| `make verify` final run 1 | 4,838 passed, 0 failed, 0 skipped after specs governance passed |
| `make test` final run 2 | 4,838 passed, 0 failed, 0 skipped |
| `rebar3 dialyzer` | Non-green; 777 repository-wide warnings remain outside the Phase 7 gate |

The active suite contains 4,838 tests. The two consecutive final
complete-suite runs produced identical green totals. The suite grew by nine
tests from the Phase 6 baseline through the focused executable-governance
regressions.

## Executable Governance Boundary

`make check-specs` validates concrete requirements and their families,
cataloged scenarios, machine-readable evidence, component acceptance criteria,
ADRs, conformance-matrix paths, and local Markdown links. Deliberately invalid
fixture repositories verify that the checker rejects missing or unknown
governance relationships and broken evidence.

The manifest-driven `make conformance` command complements that structural
gate by running the unique EUnit module set named in
`specs/conformance/executable_scenarios.tsv`. It is a representative scenario
slice, not a replacement for the complete active suite.

| Evidence group | Modules | Verified boundary |
| --- | ---: | --- |
| Compiler, effects, runtime, REPL, and patterns | 11 | `SCN-001` through `SCN-006` |
| Standard-library compilation and validation | 2 | `SCN-007` |
| Property execution and shrinking | 2 | `SCN-008` |
| Workflow and specs governance | 2 | `SCN-009` |
| Structural and generic stdlib laws | 2 | `SCN-010` |
| **Total** | **19** | **411 tests; every promoted scenario has executable evidence** |

## CI Boundary

`.github/workflows/ci.yml` provisions the supported OTP and rebar3 versions and
runs `make verify` for pull requests and pushes to `main`. The workflow grants
the `GITHUB_TOKEN` read-only repository-content access, so verification does
not require mutation privileges.

## Remaining Scope

The following work remains visible beyond the reconciliation roadmap:

- the repository-wide Dialyzer warning inventory
- placeholder-backed or simplified branches in some advanced stateful,
  concurrency, distribution, and OTP property-testing helpers
- automatic source-language reflection, derivation macros, and broader
  REPL/law ergonomics
- broader module-system, actor-language, distributed-runtime, and Flow tracks

## Interpretation

The executable governance gate proves that promoted specifications retain
their required catalog and evidence relationships. The complete EUnit result
proves the maintained repository boundary. Neither result claims that every
research-backed surface is implemented or that static analysis is green.
