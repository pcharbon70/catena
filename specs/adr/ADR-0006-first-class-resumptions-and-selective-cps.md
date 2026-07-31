# ADR-0006: First-Class Resumptions Through Effect-Directed Selective CPS

## Status

Accepted. The architecture and source vocabulary are accepted; implementation
is planned and is not part of the currently promoted executable language
surface.

## Context

Catena already tracks effects in function types, implements effect rows and
typed-handler machinery, and executes generated `perform` and `handle`
expressions through the explicit-context runtime selected by
[ADR-0003](ADR-0003-explicit-effect-context-runtime.md).

That runtime currently provides scoped operation dispatch. A performed
operation sends its arguments to a handler, receives a value, and then returns
normally to the generated Erlang call site. The separate Erlang-facing
algebraic-effects facade has resumption records and one-shot, multi-shot, deep,
and shallow helper modules, but its captured continuation is an opaque
direct-style result marker. It is not the executable remainder of an ordinary
Catena computation.

A true algebraic handler needs the delimited continuation at a `perform`
point. For:

```catena
let answer = perform Choice.choose()
in use_answer(answer)
```

the handler-visible resumption must represent the executable function:

```text
answer -> use_answer(answer)
```

up to the nearest enclosing handler delimiter. A handler must be able to
resume that computation, return without resuming it, retain it for later use,
or—when explicitly permitted—resume it more than once.

The BEAM does not expose an ordinary Erlang call stack as a callable value.
Stack traces are diagnostic data, not continuations. Catena must therefore
make the continuation explicit before emitting ordinary Core Erlang.

## Decision

Catena will implement true delimited algebraic handlers through
effect-directed selective continuation-passing-style (CPS) lowering.

### Public Vocabulary

The source-language value is named `Resumption`. `Continuation` is reserved
for compiler IR and implementation discussions.

`with` and `resume` become context-sensitive language words:

```catena
handle computation then {
  Choice {
    choose() with k ->
      resume(k, true)
  }
}
```

- `with k` binds the resumption of that operation case.
- `k` is an ordinary first-class value after it is bound. It can be passed to
  transforms, stored in data, returned, or resumed later within its runtime
  validity boundary.
- `resume(k, value)` supplies the result of the performed operation and
  executes the captured computation to its delimiter.
- The result of `resume` is the result produced when that resumed computation
  reaches the delimiter, so a handler can inspect or transform it.
- Returning from an explicit-resumption case without invoking `resume`
  discards that execution path. No separate `abort` keyword is required.

The semantic type constructor is:

```text
Resumption : ResumptionKind -> Type -> Type -> EffectRow -> Type
```

For `Resumption k a b e`:

- `k` is `OneShot` or `MultiShot`;
- `a` is the value expected as the result of the performed operation;
- `b` is the result type of the delimited handled computation;
- `e` is the residual effect row exercised while resuming it.

`resume` has the schematic type:

```text
resume :
  Resumption k a b e -> a -> b / e
```

The parser may initially represent effect rows and the resumption kind through
internal type metadata where the complete source-level type spelling is not
yet expressible. The four roles above are normative and must not be collapsed
into an untyped Erlang function.

### Backward-Compatible Value Handlers

An existing operation case without a resumption binder remains a value
handler:

```catena
FileIO {
  read(path) -> read_from_disk(path)
}
```

It is equivalent to:

```catena
FileIO {
  read(path) with k ->
    resume(k, read_from_disk(path))
}
```

The automatic resume occurs in tail position after the case body produces the
operation result. This preserves the behavior of current direct-style
handlers. A case containing `with k` is a control handler and never receives
an implicit resume.

### Handler And Resumption Defaults

- Handlers are deep by default. Resuming reinstalls the same handler frame
  around the remainder of the computation.
- Resumptions are one-shot by default.
- Shallow handlers and multi-shot resumptions require explicit opt-in.
- The initial promoted implementation may support only the deep, one-shot
  defaults. The source spelling of shallow and multi-shot opt-ins requires a
  focused follow-up surface decision; their semantic names and behavior are
  already fixed by this ADR.

One-shot use is dynamically authoritative. Catena's current Hindley-Milner
type system is not a linear type system, so the compiler may reject statically
obvious duplicate resumes but must not claim to prove all consumption paths.
The runtime must reject a second invocation deterministically.

Multi-shot execution duplicates the immutable CPS continuation and lexical
environment. It does not pretend to clone arbitrary PIDs, ports, external
resources, mailboxes, or stateful handlers. Multi-shot promotion therefore
requires a conservative admissibility check over the residual effect row and
explicit semantics for any effect authority shared between branches.

### Effect-Directed Selective CPS

The compiler will classify each callable and expression region after kind,
type, effect, handler, and call resolution:

- `direct`: the computation cannot suspend into a source-level resumable
  handler;
- `resumable`: the computation can perform an operation whose handler may
  capture its continuation, invokes `resume`, or contains a resumable
  delimiter.

Only resumable regions are CPS-lowered. Pure transforms and effect operations
proven to use only non-resumable runtime providers retain the current direct
Core Erlang representation. Open or effect-polymorphic rows are classified
conservatively as resumable unless their constraints prove otherwise.

The private entry for a resumable transform receives both the explicit effect
context and the current continuation. Public source arity remains stable.
Calls across lowering modes use explicit compiler-generated bridges:

- direct-to-resumable calls wrap the direct return in the current
  continuation;
- resumable-to-direct calls are allowed only when the callee is proven not to
  suspend;
- an unresolved or unsound bridge is a compile-time backend error.

The CPS pass runs after validated typing and call resolution and before
ordinary Core Erlang expression emission. The validated compilation unit must
retain lowering mode, delimiter identity, resumption type, source origin, and
runtime disposition so the fail-closed backend can audit every control
transfer.

### Operational Boundary

An enclosing `handle` establishes a delimiter and a handler frame. At a
resumable `perform`:

1. the CPS pass has already represented the remainder of the computation as a
   Core-compatible closure;
2. the runtime packages that closure, its captured explicit context, its
   delimiter, kind, owner, type identity, and consumption state as an opaque
   `Resumption`;
3. handler lookup uses the explicit effect context;
4. the selected operation case receives its ordinary arguments and, for a
   control handler, the first-class resumption;
5. `resume` validates ownership and state, supplies the operation result, and
   invokes the continuation under the required deep or shallow context.

Source-level resumable handlers execute on the originating BEAM process so
that `self`, mailbox ownership, exception behavior, and process-local
semantics are preserved. The explicit effect context remains the authority for
handler lookup. Process-backed builtin or external providers may remain behind
context entries, but they do not execute a captured continuation on their
provider process.

### First-Class Lifetime And Ownership

Resumptions are opaque, process-affine capabilities:

- the runtime records the BEAM process that captured the resumption;
- resuming it from another process fails with a structured ownership error;
- a one-shot resumption has `fresh`, `running`, and `consumed` states;
- re-entrant or second invocation fails deterministically;
- the captured context and delimiter remain reachable for as long as the
  resumption is reachable or executing;
- runtime state used to enforce consumption monitors its owner so owner death
  releases retained resources;
- stale runtime versions, invalid delimiters, expired handler frames, and
  malformed resumption terms fail before invoking the continuation.

First-class does not mean serializable or transferable between BEAM
processes. A future transferable-continuation design would need a separate
decision covering process identity, mailbox state, runtime resources, and
distribution.

### Runtime Representation

The runtime representation is versioned and opaque. It must contain or
reference:

- a version tag;
- the continuation closure;
- the captured explicit effect context;
- delimiter and handler-depth information;
- resumption kind;
- owner PID;
- consumption/lease authority;
- source-origin and diagnostic metadata where available.

The representation must not be exposed as a user-constructible tuple or map.
Only compiler-generated capture and validated runtime operations may create or
invoke it.

The current rule that every source handler is implemented as an independent
handler process is narrowed for resumable handlers. Compiler-lowered
resumable handler frames execute locally in the computation's process.
Process-backed builtin and external effect providers remain permitted. This
amends runtime implementation detail without changing ADR-0003's decision
that handler authority is carried by explicit contexts.

### Failure And Diagnostic Rules

The feature follows the fail-closed policy of
[ADR-0005](ADR-0005-fail-closed-semantics-preserving-beam-backend.md).
Compilation or execution must report structured failures for at least:

- `resume` outside an operation case;
- an unbound or non-`Resumption` resume target;
- operation-result or handler-result type mismatch;
- residual-effect mismatch;
- double or re-entrant one-shot resume;
- cross-process resume;
- expired delimiter or handler frame;
- unsupported multi-shot residual effects;
- a resumable call graph that cannot be represented by the selected lowering
  ABI;
- a resumption-bearing AST or IR node reaching a backend pass without an
  explicit disposition.

No continuation may be replaced with a marker value, identity function,
wildcard, silent auto-resume, or direct runtime callback merely to make Core
Erlang generation succeed.

## Consequences

### Positive Consequences

- Catena handlers can implement genuine abort, retry, coroutine, scheduler,
  backtracking, and nondeterministic control patterns.
- `Resumption` becomes a typed language value instead of an Erlang-facing
  placeholder.
- Pure and ordinary direct-style code avoids a mandatory whole-program CPS
  cost.
- Existing value handlers preserve their behavior through specified
  auto-resume sugar.
- Explicit contexts and explicit continuations have separate, reviewable
  responsibilities.
- Same-process execution preserves BEAM process identity.

### Negative Consequences

- The compiler gains a control-flow IR and a non-trivial direct/CPS calling
  convention.
- Effect-polymorphic code may need conservative CPS lowering.
- Stack traces, debugging, hot-code upgrade, and profiling must reconstruct
  useful source frames across CPS closures.
- One-shot enforcement needs runtime state because the current type system
  cannot prove linear consumption.
- Multi-shot continuation reuse cannot transparently duplicate external
  effect state.
- First-class retained resumptions can extend the lifetime of handler
  contexts and their resources.

### Neutral Consequences

- `perform`, effect rows, and explicit handler contexts remain part of the
  language and runtime.
- This ADR does not require whole-program CPS.
- This ADR does not make resumptions cross-process or serializable.
- This ADR does not select the final surface syntax for shallow or multi-shot
  opt-ins.
- Existing Erlang-facing effect helper modules remain useful as validation
  and orchestration components, but their placeholder capture is not promoted
  as language-level continuation semantics.

## Alternatives Rejected

### Treat The Current Result Marker As A Continuation

`fun(Value) -> {resumed, Value} end` does not execute the remainder of the
source computation. Promoting it would make the language claim semantics the
runtime does not provide.

### Capture The Ordinary Erlang Stack

The BEAM provides stack traces but no supported operation that converts an
arbitrary live Erlang stack segment into a callable delimited continuation.

### Whole-Program CPS

Whole-program CPS is simpler as a semantic model but would change calling
conventions and debugging for pure and ordinary direct-style code that cannot
suspend. Effect-directed selection preserves a smaller runtime footprint.

### Interpret Every Effectful Program As A Free-Effect Tree

An interpreter or free-effect representation is a useful semantic oracle, but
making every production computation an allocated syntax tree imposes a
different runtime and optimization model. Catena will use such an interpreter
for reference tests if useful, not as the required production representation.

### Use A Handler Process To Execute The Continuation

Executing the continuation on the provider process changes `self`, mailbox
ownership, process-local failure behavior, and other BEAM semantics. Provider
processes may compute operation results, but continuation execution remains on
the originating process.

### Make Resumptions Implicit

An implicit `resume(value)` is concise but cannot directly express storage,
passing, delayed use, or choosing among multiple resumptions. Explicit
`with k` makes control authority visible and keeps `Resumption` first-class.

### Require Explicit Resumption For Every Existing Handler

That would change ordinary value handlers from request/response code into
abort handlers. Tail auto-resume preserves existing source behavior while
making advanced control explicit.

## Implementation Status

Not implemented at acceptance time. The staged implementation has since
completed Phases 1 through 4: normative semantics and an oracle,
`with`/`resume` syntax and normalization, the first-class
`Resumption k a b e` typed frontend, authoritative control-mode analysis,
selective-CPS control IR, stable calling conventions, and fail-closed graph
validation are present.

The current generated-code path still uses explicit request/response handler
contexts, and the Erlang-facing `catena_resumption` capture still wraps a
direct-style result marker. Neither is evidence of true language-level
delimited continuation capture. The Phase 4 selective-CPS graph is compiler
IR only; runtime ownership, Core lowering, and executable explicit
resumptions remain future phases.

Promotion requires the staged work in
[Delimited Resumptions Implementation Plan](../planning/delimited-resumptions/README.md)
and the executable evidence defined by
[Delimited Resumption Architecture](../compiler/delimited_resumption_architecture.md).

## Related Decisions

- [ADR-0002: Minimal Core And Library-First Surface](ADR-0002-minimal-core-and-library-first-surface.md)
- [ADR-0003: Explicit Effect Context Runtime](ADR-0003-explicit-effect-context-runtime.md)
- [ADR-0005: Fail-Closed, Semantics-Preserving BEAM Backend](ADR-0005-fail-closed-semantics-preserving-beam-backend.md)

## References

- [Type And Effect System](../compiler/type_and_effect_system.md)
- [Effect Runtime](../runtime/effect_runtime.md)
- [Core Compiler Pipeline](../compiler/core_compiler_pipeline.md)
- [Algebraic Effects Theory](../research/1.17-side-effects-design/1.17.2-algebraic-effects-theory.md)
- [Phase 7: Handler/Resumption Core](../planning/algebraic-effects/phase-7-handler-resumption-model.md)
- [Phase 9: Deep And Shallow Handlers](../planning/algebraic-effects/phase-9-deep-shallow-handlers.md)
- [Phase 10: One-Shot And Multi-Shot Continuations](../planning/algebraic-effects/phase-10-one-shot-multi-shot-continuations.md)
