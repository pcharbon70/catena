# Branching

Catena 0.1.29 defines conditionals and general branching: match is the
only branch form, a conditional spelling is promised to desugar to it,
and statement-like control forms are declared absent.

## Match is the branch form

```text
match scrutinee { clause* }
clause := pattern [when condition] -> body
```

- The scrutinee evaluates once, before any clause test.
- Clauses test in source order; each tests its pattern first, its
  `Bool` condition exactly once on structural success.
- A false condition falls through; selection commits irreversibly.
- Every clause body unifies with the match's type.
- Missing alternatives reject as `M001` with a witness; redundant
  clauses reject per C002's coverage calculus.

## The conditional sugar promise

```text
if e then a else b    ⟺    match e { true -> a, false -> b }
```

Any future `if`-like spelling P109 introduces desugars to a
Bool-pattern match — shipped semantics, new punctuation only. A
non-desugaring conditional requires an edition record.

## No statement forms

Catena has no statement-like control forms: no early return, no
break, no statement tier. Everything is an expression — branching
yields values through clause bodies, and effects sequence through the
let idiom (`let _ = e1; e2`).

## Current boundary

Termination beyond tail guarantees remains P034's; a trapping
scrutinee's classification remains G036's; future scrutinee types
enter with their coverage entries in G040 slices; spellings remain
P109's; cancellation mid-branch remains G088's.

The normative contract is the research repository's
[Branching Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/branching).
