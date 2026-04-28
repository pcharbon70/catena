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

This repository does not yet enforce a CI governance gate for `specs/`.
Until that exists, keeping `specs/` accurate is a documentation discipline rather than an automated policy check.
