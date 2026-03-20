# ADR Catalog

This catalog is the canonical inventory of Catena's promoted ADR set.

## Decision Inventory

| ADR | Status | Decision Area | Summary |
| --- | --- | --- | --- |
| [ADR-0001](ADR-0001-control-plane-authority.md) | accepted | spec authority and control planes | Defines `specs/` as the promoted canonical layer over the broader `notes/` archive and clarifies ownership boundaries for architecture decisions. |
| [ADR-0002](ADR-0002-minimal-core-and-library-first-surface.md) | accepted | language/core design | Commits Catena to a minimal core and library-first surface, preferring trait/library semantics over unnecessary compiler intrinsics. |
| [ADR-0003](ADR-0003-explicit-effect-context-runtime.md) | accepted | runtime/effects | Commits the effect runtime to explicit context passing and rejects hidden process-local handler authority as the canonical model. |

## Catalog Rules

1. Add a row here whenever a new ADR is accepted into `specs/adr/`.
2. Update the summary here when an ADR is superseded, amended, or narrowed.
3. If an ADR materially changes a contract or component spec, update those documents in the same change set.
4. The catalog is an index and status view; the ADR files themselves remain the source of decision detail.
