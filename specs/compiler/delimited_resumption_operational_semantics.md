# Delimited Resumption Operational Semantics

## Status

Phase 1 semantic foundation for
[ADR-0006](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md).

This document is normative for the deep, one-shot core model. It defines what
the Phase 1 reference oracle must execute and what later selective-CPS and
runtime implementations must preserve. It does not claim that the current
source-to-BEAM path implements these rules.

## Scope

The core model covers:

- pure return and sequential binding;
- effect requests produced by `perform`;
- explicit handler delimiters;
- operation cases with an explicit `with k` resumption binder;
- first-class opaque resumptions;
- same-process, deep, one-shot resume;
- returning from a handler without resume;
- nested handlers and unhandled-operation propagation.

Value-handler auto-resume, detailed ownership/lifetime policy, shallow
handlers, and multi-shot admissibility are layered on this core in later
sections of the Phase 1 plan.

## Semantic Vocabulary

| Symbol | Meaning |
| --- | --- |
| `c` | A Catena computation |
| `v` | A runtime value |
| `E.op(args)` | An effect identity, operation identity, and argument values |
| `κ` | A compiler-semantic continuation from an operation result to the remaining computation |
| `d` | A delimiter identity |
| `h` | A handler frame |
| `χ` | An explicit effect context containing nested handler frames |
| `r` | An opaque first-class resumption handle |
| `ω` | Resumption authority state |
| `o` | The owner process identity |
| `ρ` | A residual effect row |

A continuation `κ` is semantic/compiler state. It is not directly exposed to
Catena source. A `Resumption` is the typed capability that controls one such
continuation.

## Core Computations

The reference semantics uses this minimal computation language:

```text
c ::=
    pure(v)
  | bind(c, x -> c)
  | perform(E, op, args)
  | handle(c, cases)
  | resume(r, v)
```

An operation case is:

```text
case E.op(patterns) with k -> handler_computation
```

The source binder `k` receives an opaque resumption handle. The continuation
stored behind that handle remains runtime-owned.

## Evaluation Results

Evaluation produces exactly one of:

```text
Done(v, Ω)
Request(E, op, args, κ, Ω)
Failed(category, details, Ω)
```

`Ω` is the explicit semantic state containing fresh-identity counters,
resumption authority, delimiter metadata, and a deterministic trace.

`Request` is the important boundary. It carries the actual remainder of the
computation as:

```text
κ : operation_result -> computation
```

It is not a record saying that an operation happened. Applying `κ` constructs
the computation that must execute after the `perform`.

## Pure And Sequential Reduction

### Pure return

```text
eval(pure(v), Ω) = Done(v, Ω)
```

### Bind after a value

```text
eval(c1, Ω) = Done(v, Ω1)
------------------------------------------------
eval(bind(c1, x -> c2), Ω) = eval(c2[x := v], Ω1)
```

### Bind across an effect request

```text
eval(c1, Ω) = Request(E, op, args, κ1, Ω1)
κ2(v) = bind(κ1(v), x -> c2)
------------------------------------------------
eval(bind(c1, x -> c2), Ω)
  = Request(E, op, args, κ2, Ω1)
```

This rule is the semantic origin of selective CPS. The continuation attached
to the request includes every remaining bind between the `perform` and its
eventual delimiter.

## Perform Reduction

Performing an operation suspends with the identity continuation:

```text
eval(perform(E, op, args), Ω)
  = Request(E, op, args, v -> pure(v), Ω)
```

Sequential binding composes the rest of the program onto this continuation.
For:

```catena
let answer = perform Choice.choose()
in use_answer(answer)
```

the resulting request contains a continuation equivalent to:

```text
answer -> pure(use_answer(answer))
```

No ordinary Erlang stack is inspected or captured by this rule.

## Handler Delimiters

Evaluating:

```text
handle(c, cases)
```

allocates a fresh delimiter `d`, installs a handler frame `h` in a child
explicit context, and evaluates `c` under that frame.

### Handled computation returns normally

```text
eval_under(c, h, d, Ω) = Done(v, Ω1)
------------------------------------------------
eval(handle(c, cases), Ω) = Done(v, exit(d, Ω1))
```

The handler expression itself has result type `b`, the result type of the
delimited handled computation.

### A matching operation is requested

If evaluation under `h` produces:

```text
Request(E, op, args, κ, Ω1)
```

and `h` contains a matching control case:

```text
E.op(patterns) with k -> hc
```

the machine:

1. validates the operation identity, arity, and patterns;
2. allocates a fresh resumption identity `r`;
3. stores the authority:

   ```text
   r -> {
     state: fresh,
     kind: OneShot,
     continuation: κ,
     delimiter: d,
     handler: h,
     context: χ,
     owner: o
   }
   ```

4. evaluates `hc` with `k` bound to the opaque handle for `r`.

The handler body is evaluated outside the selected handler frame. Operations
performed by the handler implementation therefore propagate to surrounding
handlers. The selected deep frame is reinstalled only when its resumption is
invoked.

### An operation is not matched

If `h` does not contain `E.op`, the request propagates outward with a wrapped
continuation:

```text
κ'(v) = continue_under(κ(v), h, d)
```

An outer handler that later resumes this request therefore returns to the
inner handled computation with its delimiter and frame intact.

## Resume Reduction

For:

```text
resume(r, v)
```

the machine validates that `r`:

- is a runtime-created resumption handle;
- belongs to the current owner process;
- refers to a live retained delimiter and handler frame;
- has kind `OneShot`;
- is in state `fresh`;
- expects a value compatible with `v`.

It then performs:

```text
ω[r].state := running
c_rest := ω[r].continuation(v)
result := eval_under(c_rest, ω[r].handler, ω[r].delimiter, Ω)
ω[r].state := consumed
return result_to_resume_caller(result)
```

The same handler frame is active while `c_rest` runs. This is the deep-handler
default. If the resumed computation performs the same operation again, the
same handler can capture a new, distinct resumption.

The result of reaching delimiter `d` is returned as the value of `resume`.
The operation case may inspect or transform that result before returning from
the enclosing `handle`.

The transition to `consumed` occurs after success or failure. A second or
re-entrant invocation of the same one-shot resumption fails deterministically.

## Returning Without Resume

If an explicit operation case completes without invoking or retaining its
fresh resumption:

```text
E.op(args) with k -> pure(handler_result)
```

then the continuation stored behind `k` is discarded and the enclosing
`handle` evaluates to `handler_result`.

This is the algebraic-handler abort operation. The initial surface requires no
separate `abort` keyword.

If the result contains the opaque resumption value, the resumption is retained
rather than discarded. Its continuation, handler frame, delimiter, and
context remain reachable under the lifetime rules specified by Section 1.2.

## Nested Deep Handlers

Handler lookup is innermost first:

```text
outer context
  -> outer handler frame
    -> inner handler frame
```

- A matching inner case handles the operation.
- If the inner frame does not match, the request propagates to the outer
  frame.
- Resuming an operation captured by the inner frame reinstalls the inner frame.
- Resuming an operation captured by the outer frame returns through any inner
  delimiter encoded in the wrapped continuation.
- Every perform captures a new resumption identity; one-shot consumption is
  per identity, not per effect or operation.

## Representative Derivation

Consider:

```text
handle(
  bind(
    perform(Choice, choose, []),
    answer -> pure(if answer then selected else rejected)
  ),
  Choice.choose() with k -> resume(k, true)
)
```

Reduction proceeds as:

```text
perform Choice.choose
  -> Request(Choice, choose, [], v -> pure(if v then selected else rejected))

handler match
  -> capture r with the request continuation

resume(r, true)
  -> evaluate pure(if true then selected else rejected) under the deep frame
  -> reach delimiter with selected
  -> consume r
  -> resume returns selected

handler returns selected
  -> handle returns selected
```

The observable result is `selected`. A marker continuation such as:

```erlang
fun(Value) -> {resumed, Value} end
```

would produce `{resumed, true}` and never execute the conditional. It
therefore does not satisfy this semantics.

## Semantic Invariants

The reference oracle and later production implementation must preserve:

1. Every request contains the real remainder to its delimiter.
2. A delimiter has one stable identity throughout propagation and resume.
3. A resumption cannot be constructed by source code.
4. Resume supplies exactly one operation result to the captured continuation.
5. Deep resume reinstalls the captured handler frame.
6. Handler code runs outside the selected frame unless it explicitly resumes.
7. One-shot state transitions are deterministic.
8. Returning without resume discards that computation path.
9. Retaining a resumption retains the semantic resources needed to invoke it.
10. Unmatched operations preserve inner control context when propagating.
11. Continuation execution stays on the owner process.
12. Placeholder results and silent direct-style fallback are not valid
    implementations.

## Deferred From The Core Rules

- value-handler tail auto-resume translation;
- detailed owner death and retained-resource cleanup;
- cross-process transfer, which ADR-0006 rejects for the initial design;
- shallow handler context restoration;
- multi-shot residual-effect admissibility and branch state;
- concrete Core Erlang and runtime representation;
- optimization of direct or CPS regions.

## Related Material

- [Delimited Resumption Architecture](delimited_resumption_architecture.md)
- [Delimited Resumptions Implementation Plan](../planning/delimited-resumptions/README.md)
- [Phase 1 Plan](../planning/delimited-resumptions/phase-01-operational-semantics-feature-ledger-and-reference-oracle.md)
