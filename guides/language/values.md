# Values and Evaluation

Catena 0.1.25 defines what a value is and what strictness means: a
closed grammar, one uniform first-class class, and a language
invariant with a gate.

## The value grammar

Values are exactly these ten forms:

```text
integer | boolean | unit | float
| tuple of values | closure | constructor value
| record of values | injection(label, value) | process handle
```

Nine forms come from the C010 kernel calculus unchanged; Float is the
one form the kernel grammar predates, admitted with C018 semantics
(signed zero, finite binary64) unchanged. Never values: evidence,
handler declarations, capability names, resumptions (affine — a
one-shot continuation is runnable state, not data), traps, effect
rows, and signatures.

Every value is **uniformly first-class**: bindable, passable,
returnable, and storable. No tiers, no per-type restrictions — what
storing a process handle lets you observe belongs to G037/G085, not to
the value grammar.

## The strictness invariant

```text
Every subexpression evaluates at most once,
to a value or a terminal trap,
before it is used.
```

The named exceptions are exactly the kernel's: `and` skips its right
operand when the left is false, `or` when the left is true. Any future
lazy or short-circuit form requires a lifecycle edition record naming
it — the same gate the prelude guarantee uses.

A completed evaluation is a **value or a trap** — there is no third
terminal outcome. A suspended effect request is a pending continuation,
not a terminal; no process entry returns with one live.

## Using the classifier

```elixir
true = Catena.Values.value?(1.5)
true = Catena.Values.value?({:catena_variant, :some, {1, true}})
:resumption = Catena.Values.classify(%Catena.Runtime.ResumptionToken{} |> then(&Catena.Runtime.ResumptionToken.new()))
{:computation, :call} = Catena.Values.classify(%{tag: :call})
{:value, {2, true, 5}} = Catena.Values.terminal_witness(Stepper.run(core, "main"))
```

Classification is total over decodable typed-core and kernel input and
adds zero new diagnostic families — the slice is definitional.

## Current boundary

Per-form evaluation order remains P030's; bindings, calls, and
branching remain G031–G033's; equality and ordering remain P035's; the
failure taxonomy beyond traps remains G036's; allocation observability
remains G037's; each future type (string, binary, list, map) enters
with its value status in its own G040 slice; surface syntax remains
P109's.

The normative contract is the research repository's
[Values and Evaluation Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/values-and-evaluation).
