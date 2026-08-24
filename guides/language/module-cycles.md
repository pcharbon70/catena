# Module Cycles

Catena 0.1.20 admits module dependency cycles: modules may reference each
other, and the strongly-connected component is the unit that pays for it.

## Components are the unit

The maximal strongly-connected components of the import graph partition
your program. Each component is one checking and caching unit:

- **Inside a component**, references to a companion resolve against its
  declared signatures — every exported value already requires one, which is
  exactly what mutual recursion needs. No interface digests circulate
  inside the component; an intra-component import presents no digest, and
  presenting one is `CYC001` (regime mixing). Exporting a component member
  without its declared signature is also `CYC001` (signature gap), as is
  the ordinary `T008` on the compiler path.
- **Across components**, imports stay digest-bound exactly as 0.1.18 fixed
  them. Outsiders import individual members through ordinary interfaces.
- The whole component gets **one joint digest** — deterministic over the
  sorted member names and member interface digests — invariant to member
  order and to the layout choice. Rebuilding any member re-digests the
  component.

## Initialization and checking

Modules contribute definitions only — the BEAM has no top-level
evaluation, so loading per component is the whole initialization story and
no intra-component order exists. Each member is checked independently
against companions' signatures and outside digests; there is no joint
inference.

## Compiling a component

`Catena.compile_scc/2` takes all member programs, builds each member's
provisional interface from its declared types (seeded with true
inhabitation so coverage analysis terminates), checks and compiles every
member against its companions' provisionals, cross-verifies each computed
interface against its provisional surface, and returns the members'
binaries and interfaces plus the joint digest.

One known evidence boundary: exhaustive pattern matching over
mutually-recursive types — across modules or within one module — reaches
the existing `M004` coverage budget. Component programs execute through
cross-module construction and per-member functions today; wider matching
over recursive types is bounded by the same limit as ordinary recursive
modules.

## The alternative: invert the dependency

When a cycle is convenience rather than genuine mutual definition,
restructure instead: the reusable module takes the collaborator as a
higher-order value.

```catena
serve : (Request -> Reply) -> Config -> Result
```

The graph stays a DAG and no component forms. Components are for mutual
definition; inversion is the recommended tool for mutual use.

## Diagnostics

| Diagnostic | Meaning |
| --- | --- |
| `CYC001` | an SCC-internal violation: a digest-presented companion import (regime mixing) or an unsigned component export (signature gap) |

## Current boundary

Component members are compiled from the retained JSON AST; the concrete
recursive `use` surface, package assembly over components, and lockfile
representation of joint digests remain future work.

The normative contract is the research repository's
[Module Dependency Cycles Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/module-dependency-cycles).
