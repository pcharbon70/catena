# Bindings and Sequencing

Catena 0.1.27 defines how names bind and how effectful expressions
sequence: non-recursive structure, silent shadowing, and one warning.

## Binding structure

```text
let name = rhs ; body
```

- The right-hand side evaluates **without** the name being bound —
  local `let` is strictly non-recursive. A self-referential RHS is
  `T001` unbound.
- Substitution happens only after the RHS is a value (the kernel's
  substitute-after-value rule).
- Scope is sequential-lexical: the binding extends the environment for
  its body alone.
- An inner binding **silently shadows** anything in scope — outer
  bindings, module definitions, imports, the prelude — innermost wins,
  no diagnostic (C021's rule at the binding level).

## Recursion is definitions-only

Named definitions recurse through the kernel's signed environment;
mutual recursion among definitions is C024's SCC admission. No local
recursive or mutually-recursive binding form exists — local recursive
functions are G032's named-local-function question.

## Unused bindings and BS001

An unused binding is **valid** and its right-hand side still evaluates
— effects observable. Because that usually signals an omission,
`Catena.Bindings` emits the deny-able `BS001` warning:

```json
{ "diagnostics": { "deny": ["BS001"] } }
```

- Fires on a binder that never occurs in its binding's body.
- Never fires when the name is `_`-prefixed — the deliberate-discard
  escape hatch (the sequencing idiom uses one).
- Denial through the manifest promotes it to an error.

## The sequencing idiom

```text
let _ = e1 ; e2
```

evaluates `e1` to a value — effects observable — discards it, then
evaluates `e2`. This is *the* sequencing form: the only one the
retained JSON AST expresses, with C030's first-to-value-then-second
schedule as its order.

## Current boundary

Functions, arity, closures, and tail calls remain G032's; branch forms
remain G033's; termination remains P034's; pattern-binding surface
forms remain C002/P109's; cancellation mid-sequence remains G088's.

The normative contract is the research repository's
[Bindings and Sequencing Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/bindings-and-sequencing).
