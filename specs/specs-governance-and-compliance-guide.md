# Specs Governance And Compliance Guide

This document defines the lightweight governance model for Catena's `specs/` tree.

## Intent

The goal is to keep `specs/`:

- smaller than the full implementation surface
- architecture-facing rather than diary-like
- grounded in code and test reality
- explicit about what is implemented, partial, or deferred

## Baseline Rules

1. Baseline architecture docs should be updated before or alongside deeper component specs.
2. New normative behavior should introduce or refine a `REQ-*` family in `contracts/`.
3. `REQ-*` changes should update `contracts/requirements_catalog.md`.
4. `REQ-*` changes should update `conformance/spec_conformance_matrix.md`.
5. Cross-cutting ownership changes should update an ADR in the same change set and keep `adr/adr_catalog.md` in sync.
6. Component specs should only add `AC-*` entries when the surface is concrete enough to verify against the repo.

## Executable Governance

The local governance gate enforces the relationships described by the baseline rules:

```bash
# Validate requirement families, scenarios, executable evidence,
# component acceptance criteria, ADR coverage, paths, and Markdown links.
make check-specs

# Run only the EUnit modules named by the executable scenario manifest.
make conformance

# Run specs governance followed by the complete test suite.
make verify
```

The executable evidence manifest is
[`conformance/executable_scenarios.tsv`](conformance/executable_scenarios.tsv).
Each row must reference a defined `SCN-*` identifier, an existing Erlang test
source, and the module declared by that source. The manifest is the single
source used by `make conformance`; a scenario can therefore have several
focused evidence modules without duplicating command configuration.

`make check-specs` fails when:

- a concrete requirement family is missing from the catalog or matrix
- a scenario is missing from the catalog, matrix, or evidence manifest
- an evidence source or declared module does not match the manifest
- a component with `AC-*` criteria is absent from the matrix
- acceptance-criterion identifiers are duplicated
- an ADR is absent from the ADR catalog
- a path named by the conformance matrix does not exist
- a local Markdown link under `specs/` is broken

## Reconciliation Rule

Catena currently has planning/status drift in a few places:

- some planning checklists still show work as incomplete even when implementation summaries and code exist
- some historical summaries describe earlier states that were later extended

When these disagree, `specs/` should reconcile status from:

1. current code
2. current tests and executable workflows
3. later summaries/reviews over earlier ones
4. planning docs, when they still match the implemented state

## Promotion Guidance

Add or expand material in `specs/` when it is:

- stable enough to serve as a project reference
- broader than a single implementation session
- important to more than one subsystem
- useful for explaining current repo state to contributors

## Current Scope

The executable governance and conformance gates are available locally.
CI enforcement is introduced separately so the command contract can be
validated before it becomes a required remote check.
