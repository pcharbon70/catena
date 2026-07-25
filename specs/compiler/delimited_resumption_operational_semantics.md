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

Value-handler auto-resume and the process-affine ownership/lifetime policy are
defined below as layers on this core. Shallow handlers and multi-shot
resumptions have bounded meanings but remain deferred promotion targets.

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

## Value-Handler Compatibility

An operation case without an explicit `with` binder is a value case:

```catena
FileIO {
  read(path) -> read_from_disk(path)
}
```

Normalization translates it to a control case before type checking or
lowering:

```text
E.op(patterns) -> body

  ==normalize==>

E.op(patterns) with __k@synthetic ->
  bind(body, __value@synthetic ->
    resume(__k, __value))
```

The concrete source-equivalent form is:

```catena
FileIO {
  read(path) with __k ->
    let __value = read_from_disk(path)
    in resume(__k, __value)
}
```

`__k` and `__value` are fresh compiler identities, not names introduced into
the user's namespace. The operation case owns the primary source origin. Each
synthetic binder and `resume` has a synthetic origin whose parent is that
case; the original body retains its original origin. Diagnostics may explain
the compatibility translation, but must report the user's operation case
rather than a generated identifier.

The translation fixes evaluation order:

1. match the operation and arguments;
2. capture a fresh deep, one-shot resumption;
3. evaluate `body` exactly once outside the selected handler frame;
4. if `body` produces an operation result `v`, invoke `resume(__k, v)` exactly
   once in tail position;
5. return the result produced when the resumed computation reaches its
   delimiter.

If `body` fails or does not terminate, the synthetic resume is not invoked.
If it performs another effect, that request propagates to an outer handler;
after that request returns, evaluation proceeds to the synthetic resume.

A case written with `with k` is a control case. It is never wrapped in an
implicit resume, even when static analysis can see no explicit use of `k`.
Returning from that case therefore has the abort behavior defined above.

### Compatibility theorem

Let `B` be an existing value-handler body that produces an operation result
`v`. Let `K` be the remainder of the handled computation. The existing
request/response reading is:

```text
evaluate B -> v
return v to perform
continue K(v)
```

The normalized reading is:

```text
evaluate B -> v
resume(__k, v), where __k contains K
continue K(v)
```

Both execute `B` once, substitute the same `v` into the same remainder, and
observe the same result and residual effects. The equivalence applies only to
value cases. An explicit control case intentionally adds the ability to
abort, retain, conditionally resume, or transform the result of resume.

### Representative compatibility cases

| Case | Existing value-handler trace | Normalized trace | Preserved observation |
| --- | --- | --- | --- |
| `FileIO.read(path)` | provider reads; returns bytes; remainder consumes bytes | body reads; `resume(k, bytes)`; remainder consumes bytes | bytes, provider effects, and remainder |
| `Process.self()` | provider returns owner PID; remainder observes it | body returns the same PID on the owner process; tail resume runs the remainder there | process identity and mailbox ownership |
| inner miss, outer match | inner frame forwards; outer returns value; inner remainder continues | wrapped continuation reaches outer case; outer tail-resumes; inner frame and delimiter are restored | innermost-first lookup and inner context |
| provider exception | provider fails the perform; remainder is not entered | body fails before synthetic resume; remainder is not entered | failure category and no post-perform effects |
| provider timeout | timeout fails the perform; remainder is not entered | timeout fails the body before synthetic resume; remainder is not entered | timeout policy and no post-perform effects |

This compatibility argument assumes the existing provider itself has not
already resumed or otherwise executed Catena continuation code. Provider
processes may compute `v`; continuation execution remains on the capturing
process.

## Ownership And Lifetime

A `Resumption` is an opaque first-class capability. Catena code may pass it to
another transform, store it in a data structure, or return it from a handler.
Those operations transfer a reference to the capability, not its process
ownership and not a serializable copy of its continuation.

### Process affinity

Capturing a resumption records the identity `o = self()` of the evaluating
BEAM process. The continuation may execute only while the invoker is that
same live process:

```text
current_owner = r.owner = live
```

Sending the opaque term to another process is not itself forbidden, because
ordinary BEAM values can be sent. Invoking it there fails before continuation
code runs. The runtime must not forward the invocation to the owner, move the
continuation to a provider process, or silently substitute a direct callback.

This rule preserves `self`, mailbox ownership, links, monitors, exception
propagation, and other process-local behavior across resume.

### One-shot authority

The authoritative state machine is:

```text
fresh --authorize--> running --complete-or-fail--> consumed
```

- only `fresh` may begin invocation;
- observing `running` is a re-entrant invocation failure;
- observing `consumed` is a second-invocation failure;
- completion, exception, timeout, and cancellation after authorization all
  transition the authority to `consumed`;
- validation failure before authorization does not execute or consume the
  continuation;
- state transitions are atomic with respect to all references to the same
  opaque capability.

Runtime validation is mandatory. Static detection of obviously duplicated
uses is useful but cannot establish affine use through arbitrary
higher-order values in the initial type system.

### Retention and delimiter lifetime

Returning a fresh resumption as data retains:

- its immutable compiler-reified continuation;
- its captured lexical environment;
- the selected handler frame and explicit effect context;
- its logical delimiter and delimiter result type;
- its owner, version, kind, depth, origin, and authority state.

The enclosing `handle` may return the opaque value, but the retained logical
delimiter does not expire merely because the original Erlang call returned.
Invoking the resumption later on its owner process re-enters the retained
delimited computation. Reaching that delimiter supplies the result of
`resume`; it does not re-run the handler case that retained the capability.

The runtime may reclaim retained state when no capability can reach it.
Owner-process death is an unconditional lifetime boundary: runtime authority
monitors the owner and releases continuation, context, and delimiter
resources. A handle that survives elsewhere then reports an expired-owner
failure and cannot revive the state.

A delimiter is stale when its retained metadata or handler frame is missing,
expired, has an incompatible runtime version, or no longer denotes the frame
recorded by the capability. Stale validation fails before authorization.
Malformed or source-forged values fail opaque-representation validation
before any ownership, state, or continuation action.

### Validation order

The semantic validation order is:

1. opaque representation and supported runtime version;
2. registered resumption identity and type identity;
3. live owner;
4. current-process ownership;
5. live, matching delimiter and handler frame;
6. supported kind and depth;
7. expected operation-result value type;
8. atomic one-shot authorization.

This order keeps malformed, cross-process, stale, unsupported, re-entrant,
and consumed failures deterministic. Section 1.3 assigns stable diagnostic
categories to these outcomes.

## Semantic Mode Boundary

The only Phase 1 executable and initial source-promotion mode is:

```text
depth = Deep
kind = OneShot
```

Deep means that resume reinstalls the selected handler frame while evaluating
the captured remainder. One-shot means that one authority may begin at most
one invocation.

The accepted conceptual meaning of a shallow handler is narrower: it captures
the same delimited remainder, but resume does not reinstall the selected
handler frame. A repeated operation from the remainder therefore searches
only the surrounding context. Shallow behavior changes context restoration,
not what continuation is captured.

The accepted conceptual meaning of a multi-shot resumption is that each
authorized invocation starts from the same immutable captured continuation
and lexical environment with independent branch execution. It does not clone
mailboxes, PIDs, ports, mutable external resources, provider state, or the
outside world.

Neither mode has source syntax or production runtime authority in Phase 1.
Promotion of either requires:

- an accepted surface spelling and normalized representation;
- type and effect rules for selecting the mode;
- runtime versioning and authorization support;
- diagnostics for unsupported or inadmissible use;
- shallow context-restoration evidence or, for multi-shot, a conservative
  residual-effect admissibility rule and branch-sharing policy;
- resource bounds and executable integration evidence.

A Phase 1 request for shallow or multi-shot behavior fails explicitly as an
unsupported semantic mode. It must never fall back to deep one-shot behavior.

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

- cross-process transfer, which ADR-0006 rejects for the initial design;
- shallow handler context restoration;
- multi-shot residual-effect admissibility and branch state;
- concrete Core Erlang and runtime representation;
- optimization of direct or CPS regions.

## Related Material

- [Delimited Resumption Architecture](delimited_resumption_architecture.md)
- [Delimited Resumptions Implementation Plan](../planning/delimited-resumptions/README.md)
- [Phase 1 Plan](../planning/delimited-resumptions/phase-01-operational-semantics-feature-ledger-and-reference-oracle.md)
