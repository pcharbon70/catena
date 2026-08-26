# Functions and Calls

Catena 0.1.28 defines the function model: semantic-unary currying,
free partial application, lexical immutable capture, and proper tail
calls.

## The semantic-unary model

Every function takes exactly one argument:

```text
fn (x) -> body                  -- anonymous: one parameter
def name (p1, ..., pn) = rhs    -- sugar: nested unary functions
name a1 a2 ... an               -- repeated unary application
```

A multi-parameter definition desugars to nested unary functions; a
multi-argument call applies arguments one at a time, left-to-right,
under C030's schedule. There is no fixed arity to check — under- and
over-application are impossible states, not errors.

## Partial application is free

Applying a multi-parameter function to a prefix of its arguments
yields a **closure value** — first-class, storable, callable later:

```text
let base = ask 10
let partial = add base          -- partial : Int -> Int, a value
(partial 5, partial 7)          -- (15, 17)
```

## Closures capture lexically and immutably

A closure carries its defining environment by value; captured
bindings cannot change, so applying a closure twice observes the same
captured values. What closure *allocation* lets you observe remains
G037's exclusion.

## Local functions are let-bound closures

`let f = fn x -> …; body` — the local function form, with all of
C031's rules: non-recursive, silently shadowing, valid when unused
(`BS001` applies). No separate local form exists.

## Proper tail calls

A call in tail position — after pattern or handler selection, in a
process loop after receive, or as a definition's final result —
consumes no unbounded Catena stack. BEAM's last-call optimization is
the native implementation; the conformance witness runs a
five-million-iteration match-dispatched tail recursion on compiled
BEAM.

## Current boundary

Branch forms remain G033's; termination beyond the tail guarantee
remains P034's; closure allocation observability remains G037's;
calling conventions remain G094's; surface spellings remain P109's.

The normative contract is the research repository's
[Functions and Calls Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/functions-and-calls).
