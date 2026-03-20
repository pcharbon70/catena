# ADR-0001: Control-Plane Authority

## Status

Accepted

## Context

Catena has a large amount of design and implementation material in `notes/`, but that archive mixes long-range research, historical status, and code-adjacent working notes. The repository needs a smaller canonical layer that states what currently owns what.

## Decision

Catena will treat:

- `notes/` as the broad research, planning, implementation, and review archive
- `specs/` as the promoted architecture-and-governance layer

Within `specs/`, authority resolves in this order:

1. the control-plane ownership matrix
2. accepted ADRs
3. baseline architecture docs
4. contracts and conformance mappings
5. domain/component specs

## Consequences

- historical notes are important context, but not automatically canonical
- architectural tie-breakers should be promoted here when they affect multiple surfaces
- domain/component specs can stay concise because ownership resolution is defined centrally
