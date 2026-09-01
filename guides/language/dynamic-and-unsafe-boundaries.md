# Dynamic and Unsafe Boundaries

Revision `0.1.43` answers G067: **unsafety cannot be written in
Catena source.** No casts, no runtime type inspection, no
unchecked operations, no compiler intrinsics, and no reflection
exist in edition `0.1`.

## Three anchors that decided it

1. **The guard vocabulary (C003)** — the checker already rejects
   "a foreign call, dynamic test, reflection operation, or
   unchecked cast" in the one fragment where they were ever
   tempted.
2. **Erasure (C006/C113)** — no type or specification material
   survives into runtime artifacts; there is nothing for a
   typecase to branch on. Witness: compiled BEAM chunk sets are
   the standard OTP set plus the fixed C010 metadata chunk — no
   spec, governance, or evidence chunks.
3. **The failure taxonomy (C036)** — `trap(reason)` has kinded
   reasons and no cast-failure kind; a checked cast has no
   failure to inhabit without amending the taxonomy.

## Arrival conditions

Any future cast, typecase, or dyn form must, in its own revision:
state its representation (amending erasure), its failure
classification (amending the taxonomy), its visibility, and its
evidence interaction.

## The foreign visibility routing

Dynamic values can only enter at three edges — Erlang terms
(G095), foreign calls (G096), NIFs and ports (G098) — and each
must pass a **visible, typed, failure-classified boundary** owned
by its arriving slice. Standing precedents: C036's
foreign-raise-to-`trap(reason)` mapping, and the BEAM term
format's refusal of non-finite floats — refusal as visibility.
Until those slices exist, there is no way to bring an untyped
value in. That is the point.

The normative contract is the research repository's
[Dynamic and Unsafe Boundaries Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/dynamic-and-unsafe-boundaries).
