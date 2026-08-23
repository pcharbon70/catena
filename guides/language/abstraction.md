# Abstraction

Catena 0.1.19 draws the abstraction boundary with two exclusions and one
sanctioned idiom.

## Representation is never observable

No declaration, export, interface entry, or pragma pins a datatype to the
uniform or compact representation as an observable contract. Layout stays
an implementation freedom: compact is the production default, uniform the
reference, and conformance programs check and execute under both. Any
future layout-stability or ABI contract belongs to the compatibility gap
(G028) together with the foreign boundaries — until then, every
stable-layout spelling on any frontend is invalid input.

## Authority is a binary vocabulary

Constructors are construct-and-match together or hidden entirely:

- **transparent** — the type exports with its constructor surface;
  every exported constructor is available for construction and matching,
  with no separation;
- **abstract** — the type exports without its constructors; neither
  construction nor matching by spelling exists outside the defining
  module.

There is no construction-only, matching-only, or per-constructor mode, and
no spelling is reserved for one. Selective exposure and views stay future
work owned by the views and data-model gaps.

## The sanctioned invariant idiom

Give a datatype an invariant the sanctioned way:

```catena
export type Email
export parse : String -> Result EmailError Email
export domain : Email -> String
```

Export the type abstract; export validating constructors whose failure is
visible in their result type; export observers. No client can construct or
destructure an `Email` by spelling — only through `parse` — so the
invariant holds by typing, not convention. A plain wrapper with a public
constructor enforces nothing. Clients satisfy coverage over abstract
scrutinees with a wildcard plus the observers; the defining module's
private matches are ordinary transparent matches.

## Diagnostics

The boundary adds no diagnostic: transparency closure reuses `EXP001`,
layout coercion remains `L001`, coverage reuses the data-and-pattern
families, and invalid event shapes stay `NSP001`-class rejections.

## Current boundary

The 0.1.19 corpus proves the exclusions over the real compiler paths —
export events, the retained JSON AST, interface decoding — and executes
the idiom program under both layouts with abstract constructors
unreachable through digest-bound interfaces.

The normative contract is the research repository's
[Abstraction Boundaries Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/abstraction-boundaries).
