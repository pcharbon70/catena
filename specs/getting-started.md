# Specs Getting Started

This guide explains how to work with Catena's `specs/` layer now that it has been backfilled from the existing `notes/` corpus and codebase.

## 1. Read The Promoted Baseline First

Start with:

- [design.md](design.md)
- [topology.md](topology.md)
- [boundaries.md](boundaries.md)
- [control_planes.md](control_planes.md)
- [targets.md](targets.md)
- [planning/current_status.md](planning/current_status.md)

These documents are the short canonical view of the repo's current architecture and status.

## 2. Use Contracts To Define Normative Behavior

When a surface becomes important enough to govern, refine or extend the appropriate `REQ-*` family in `contracts/`.

Current families:

- `REQ-COMP-*` for compiler behavior
- `REQ-RT-*` for runtime and REPL behavior
- `REQ-STDLIB-*` for standard-library and library-first language surfaces
- `REQ-TEST-*` for build, testing, and quality workflow expectations
- `REQ-OBS-*` for traceability, diagnostics, and status reconciliation

## 3. Use ADRs For Cross-Cutting Decisions

When a decision affects multiple subsystems or workstreams, record it in `adr/` instead of burying it in feature notes.

## 4. Keep `notes/` And `specs/` In Their Roles

- `notes/` is the broad working archive: research, planning, implementation notes, reviews, and summaries.
- `specs/` is the promoted layer: smaller, current, and architecture-facing.

If a planning checklist and the code-backed summaries disagree, reconcile the result in `specs/` and then update the planning docs when practical.

## 5. Add Or Update Component Specs When Surfaces Stabilize

Current component specs live under:

- `compiler/`
- `runtime/`
- `stdlib/`
- `tooling/`

Component specs should define `AC-*` acceptance criteria and be mapped in `conformance/spec_conformance_matrix.md`.
