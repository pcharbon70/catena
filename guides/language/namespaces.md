# Namespaces

Catena 0.1.17 defines how names resolve: which namespaces exist, what may
shadow what, and what happens when origins collide.

## Categories and spelling classes

Names live in per-category namespaces — values, fields, operations, and
type variables (lowercase-initial spellings); types, constructors, traits,
effects, handlers, process entries, and modules (uppercase-initial
spellings). The partition is hard: a lowercase spelling cannot declare a
type (`NSP002`), and `Vec` the type coexists with `vec` the value without
collision. Constructors are unique across the whole module, not per type;
`Json.Null` qualification is the ambiguity escape. Governed specification
identities never resolve as program names.

## Shadowing

Inner scopes shadow outer bindings in the same category — innermost wins,
silently, and shadowing never crosses categories. Type variables scope per
quantifier and may shadow type or trait names inside their region; after
the region ends the outer binding is visible again. Same-scope duplicates
in one category are `NSP001`.

## Precedence and ambiguity

For one unqualified spelling in one category: the innermost binding wins;
otherwise a module-level local declaration beats every import; otherwise,
if two or more imports supply the spelling, the reference is `NSP004`,
naming every origin — resolve it as `Module.member` instead. Resolution
never depends on import order or inferred types. Unbound references are
`NSP003`.

## Qualification

A qualified reference is exactly two segments, `Module.member`; chains
like `A.B.C` are reserved spellings (`NSP005`) until a later revision
admits deeper qualification with evidence.

## Diagnostics

| Diagnostic | Meaning |
| --- | --- |
| `NSP001` | a duplicate declaration of one spelling in one category within one scope |
| `NSP002` | a declaration's spelling violates its category's spelling class |
| `NSP003` | a reference has no binding and no import in scope |
| `NSP004` | an unqualified reference is ambiguous across import origins |
| `NSP005` | a qualified reference has more than two segments |

## Current boundary

`Catena.build_namespace_environment/2` consumes scope events
(declarations, scope boundaries, import sets) and
`Catena.resolve_name/2` resolves references. Import and export syntax,
visibility defaults, and the prelude remain future work; the resolver
does not parse source, tokenize, type-check, evaluate, or emit artifacts.

The normative contract is the research repository's
[Namespaces and Shadowing Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/namespaces-and-shadowing).
