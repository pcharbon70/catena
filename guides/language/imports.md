# Imports and Exports

Catena 0.1.18 defines the module boundary: what a module exposes, what an
import admits, and what happens when admissions go unused.

## Exports

Nothing is exported by default. An explicit export declaration names a
category, a spelling, and — for types — a transparency mode:

- `transparent` exports the type together with its constructor surface
  (C002's transparent export);
- `abstract` exports the type without its constructors (C002's fully
  abstract export).

Exporting a name the module does not declare is `EXP001`; duplicate
exports reuse the namespace duplicate `NSP001`. The exported set plus its
digest is the module's interface identity (C008); digests are consumed
opaquely here.

## Imports

An import names a module, its interface digest, and an explicit
possibly-empty list of (category, name) pairs. Its two effects:

1. the module is admitted for `Module.member` qualification against its
   digest-bound export set — every exported name resolves qualified, and
   nothing else does;
2. each listed exported name is admitted unqualified into its category,
   joining the 0.1.17 precedence model (locals win; two origins colliding
   reject as `NSP004` until qualified).

The empty list is qualified-only access. Importing an unexported name is
`IMP002`; importing an unknown module is `IMP003`.

## Exclusions

0.1.18 has no wildcards, no `hiding`, no renaming, no aliases, and no
re-exports — each exclusion is deliberate. Collisions resolve by
qualification; re-exports belong to package assembly.

## Unused imports

`Catena.Namespace.check_unused_imports/2` reports two deny-able warning
shapes over the built environment and the program's references:

- an admitted unqualified name never referenced in its category (a
  qualified-only use does not count as using the admission), and
- an imported module with neither a qualified nor any unqualified use.

`IMP001` never affects acceptance; warning prose quality remains future
diagnostic work.

## Diagnostics

| Diagnostic | Class | Meaning |
| --- | --- | --- |
| `IMP001` | warning | an unused admitted name or wholly unused imported module |
| `IMP002` | error | a listed import name is absent from the module's export set |
| `IMP003` | error | an imported module is not known to the resolution context |
| `EXP001` | error | an export declaration names an undeclared name |

## Current boundary

Exports and imports are validated as abstract declaration events over
provided module interfaces; the concrete `use`/`export` punctuation, the
prelude, module cycles, and package assembly remain future work. The
resolver does not parse source, tokenize, type-check, evaluate, or emit
interfaces or BEAM.

The normative contract is the research repository's
[Imports and Exports Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/imports-and-exports).
