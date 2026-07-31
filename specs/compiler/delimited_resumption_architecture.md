# Delimited Resumption Architecture

## Status

Accepted target architecture. Phases 2 through 5 implement the lexer, parser,
AST, pretty-printer, semantic normalization, structural diagnostics,
first-class resumption kinds and types, handler/resume inference,
residual-effect checking, control-mode analysis, selective-CPS control IR,
calling conventions, fail-closed graph validation, opaque process-affine
runtime authority, deep same-process handler frames, one-shot consumption,
retention leases, lifecycle monitoring, and structured runtime failures. Core
lowering for explicit resumptions remains unimplemented, so this is not yet a
promoted source-to-BEAM feature.

This document makes
[ADR-0006](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md)
concrete enough to guide parser, type-system, lowering, runtime, diagnostics,
and conformance work without representing that work as complete.

The normative deep one-shot reduction rules are defined in
[Delimited Resumption Operational Semantics](delimited_resumption_operational_semantics.md).
The implementation boundary and diagnostic names are maintained in the
[Delimited Resumption Feature Ledger](delimited_resumption_feature_ledger.md).

## Purpose

Catena needs to distinguish three concepts that the current direct-style
runtime partially conflates:

1. an **effect context** answers which handler owns an operation;
2. a **continuation** is compiler IR for the executable remainder of a
   computation;
3. a **resumption** is the typed, opaque language value that grants controlled
   access to a delimited continuation.

The explicit context remains runtime authority. Selective CPS makes the
continuation executable. `Resumption` packages that continuation with its
scope, kind, ownership, type, and consumption rules.

## Concepts

| Concept | Meaning |
| --- | --- |
| Suspension point | A resumable `perform` whose remainder is represented explicitly. |
| Delimiter | The nearest enclosing resumable `handle` boundary at which a resumed computation returns. |
| Continuation | Compiler-owned CPS closure representing the remainder from a suspension point to its delimiter. |
| Resumption | First-class, opaque source value controlling a continuation. |
| Handler frame | Explicit-context entry containing operation cases, delimiter identity, depth, and runtime metadata. |
| Value handler | Operation case without `with`; its result is automatically tail-resumed. |
| Control handler | Operation case with `with k`; it explicitly resumes or abandons the continuation. |
| Deep handler | Reinstalls its handler frame while a continuation runs. This is the default. |
| Shallow handler | Removes its handler frame before a continuation runs. |
| One-shot | May begin execution once. This is the default. |
| Multi-shot | May begin execution more than once when its residual effects are admissible. |
| Process-affine | May be resumed only by the BEAM process that captured it. |

## Source Surface

### Explicit resumption binding

```catena
handle choose_message() then {
  Choice {
    choose() with k ->
      resume(k, "hello")
  }
}
```

`with` is legal only between an operation-case pattern list and its arrow.
The binder follows ordinary lexical name rules but has a compiler-derived
`Resumption` type.

### First-class use

```catena
Choice {
  choose() with k ->
    defer_choice(k)
}
```

The type checker must allow `k` wherever its `Resumption` type is accepted.
The runtime remains authoritative for process ownership, liveness, and
one-shot consumption.

### Explicit resume

```catena
Choice {
  choose() with k ->
    let result = resume(k, true)
    in inspect(result)
}
```

`resume(k, value)` is a control expression, not an ordinary dynamically
resolved transform call. It nevertheless has an ordinary compositional result
type: the result produced at the matching delimiter.

### Deliberate abort

```catena
Choice {
  choose() with _k ->
    fallback
}
```

Returning the handler result without resuming discards that computation path.
No separate `abort` form is required for the initial surface.

### Compatible value cases

```catena
FileIO {
  read(path) -> read_from_disk(path)
}
```

Semantic normalization converts this to the equivalent explicit form:

```catena
FileIO {
  read(path) with __resumption ->
    resume(__resumption, read_from_disk(path))
}
```

The synthetic name is never user-visible and carries a synthetic source
origin linked to the operation case.

## Lexical, Parsed, And Normalized Forms

The lexer adds `with` and `resume` tokens with identifier-boundary tests.

The parser extends operation cases and primary expressions conceptually as:

```yecc
operation_case ->
    lower_ident lparen pattern_list_comma rparen
    with lower_ident arrow expr.

resume_expr ->
    resume lparen expr comma expr rparen.
```

Exact grammar factoring may differ to control conflicts. The semantic shapes
are:

```erlang
{operation_case, Name, Patterns, {resumption_binder, Binder, Loc}, Body, Loc}

{resume_expr, ResumptionExpr, ValueExpr, Loc}
```

At the parsed boundary, the operation-case metadata slot is optional:
legacy value-handler tuples retain their five-element shape, while canonical
constructors may represent the same absence as `none`. A user-written control
case always uses the six-element shape with `{resumption_binder, Binder, Loc}`.
This compatibility union preserves source intent without forcing pre-existing
value-handler consumers to infer whether punctuation was present.

Value handlers normalize to the same operation-case shape with a synthetic
binder and a synthetic tail `resume_expr`. A successful normalized module
therefore makes automatic and explicit control behavior distinguishable
without requiring later passes to reconstruct source punctuation.

The implemented synthetic origin is:

```erlang
{synthetic, value_handler_auto_resume, SourceLocation}
```

Semantic results and validated compilation units retain this normalized form.
The Phase 3 typed frontend consumes it directly and retains resumption
evidence and origins. Phase 4 classifies and lowers that authority into
validated control IR. Phase 5 supplies its executable runtime target. Until
Phase 6 Core lowering connects them, the request/response backend receives a
compatibility view only when
the binder, origin, resume target, and tail shape exactly match
compiler-generated auto-resume output. Explicit or malformed resumptions fail
with `missing_resumption_lowering`; they are never emitted as ordinary calls
or marker closures.

AST traversal, pretty-printing, erasure, origin mapping, and diagnostic
utilities must preserve these nodes. Pretty-printing must retain whether the
source used an explicit `with` binder rather than printing all value handlers
in expanded form.

## Static Semantics

For an operation:

```text
Effect.op : Args -> a
```

handled around a computation with result `b` and residual effects `e`, its
explicit binder has type:

```text
Resumption OneShot a b e
```

unless an explicitly selected handler mode changes the kind.

Typing `resume(k, value)` requires:

1. `k` has type `Resumption kind a b e`;
2. `value` unifies with `a`;
3. the expression result is `b`;
4. the current effect context admits `e`;
5. the resumption belongs to a compatible delimiter;
6. multi-shot mode passes the residual-effect admissibility check.

Effect checking must account for effects in both the handler body and the
resumed computation. Handling removes the handled label only where the
selected deep or shallow semantics justify that removal.

`resume` outside a resumption-binding operation case is invalid even if a
variable happens to be named like a resumption. A first-class resumption passed
into another transform remains resumable because its type, rather than lexical
name, carries the authority.

The compiler should diagnose statically obvious duplicate use of a one-shot
binding. Dynamic consumption validation remains mandatory because ordinary
Hindley-Milner typing does not prove affine use through arbitrary data and
higher-order calls.

## Lowering Classification

After the validated compilation unit has resolved calls and effects, each
callable receives:

```erlang
#{control_mode => direct | resumable,
  reason => pure | provider_only | closed_effect | open_effect_row |
            handler_delimiter | resume_use,
  source_origin => term()}.
```

Classification is conservative:

- pure code is direct;
- closed effects proven to use only non-resumable providers may be direct;
- a resumable handler delimiter is resumable;
- a `resume_expr` is resumable;
- a call whose effect row may reach a resumable user handler is resumable;
- an open effect row is resumable unless constrained otherwise.

Classification participates in call resolution. Separate ad-hoc decisions in
expression code generation are not permitted.

This boundary is implemented by `catena_control_mode`. Validated compilation
units retain a versioned inventory with source reasons, typed effect evidence,
regions, and local/imported/higher-order/trait edges. Local modes are solved
to a deterministic fixed point, and exported transforms publish their mode in
the versioned module interface.

## CPS IR Boundary

Selective CPS lowering occurs after semantic, kind, type, effect, trait,
import, and call validation but before ordinary Core Erlang expression
emission.

The minimal internal control IR must represent:

- returning a value to a continuation;
- calling direct and resumable transforms;
- creating a delimiter;
- installing a handler frame;
- suspending at `perform`;
- constructing a typed resumption;
- invoking a resumption;
- abandoning a continuation;
- bridging direct and resumable calling conventions;
- source origins for every synthetic continuation and bridge.

Conceptually:

```text
let x = perform E.op(args) in Rest(x)
```

becomes:

```text
perform_cps(
  Context,
  E,
  op,
  args,
  Resumption(Owner, Delimiter, Kind, Context, x -> CPS(Rest(x)))
)
```

The actual Core Erlang representation may use closures and explicit tail
calls, but it must preserve the observable semantics of the control IR.

The compiler-owned boundary is implemented by `catena_control_ir` and
`catena_selective_cps`. It retains deterministic delimiter and continuation
identities, types, effect rows, control modes, continuation arities, runtime
dispositions, and origins. `catena_control_validate` rejects malformed graphs
before declaration projection or ordinary Core emission. This is compiler IR
evidence, not yet an executable runtime continuation.

## Calling Convention

A direct private entry remains conceptually:

```text
f_direct(args..., context) -> value
```

A resumable private entry is conceptually:

```text
f_cps(args..., context, continuation) -> control_result
```

Public transform arity remains the Catena source arity. Compiler-generated
wrappers establish an initial context and final continuation at public
boundaries.

Named calls, higher-order calls, imports, recursion, mutual recursion, trait
dictionaries, and closures must carry compatible control modes. The compiler
must generate an explicit bridge or reject the call; it may not guess from
Core Erlang arity after lowering.

`catena_control_abi` now defines these public, private, closure, final
continuation, and bridge shapes. Direct imported modes are published through
module interfaces; unresolved dynamic capabilities remain resumable
conservatively. Resumable-to-direct entry requires an explicit
non-suspension proof.

## Runtime Model

### Explicit contexts

An effect context continues to provide:

- handler lookup;
- parent relationships;
- runtime-provider targets;
- timeouts and operational policy.

For resumable source handlers, entries also identify a same-process handler
frame and delimiter. Builtin or external providers may remain process-backed.

### Opaque representation

The initial runtime representation is conceptually:

```erlang
{catena_resumption, Version, OpaqueHandle}
```

Only the runtime can dereference `OpaqueHandle`. The private state contains:

```erlang
#{owner => pid(),
  kind => one_shot | multi_shot,
  state => fresh | running | consumed,
  continuation => fun(),
  context => effect_context(),
  delimiter => reference(),
  depth => deep | shallow,
  origin => term()}.
```

This is a semantic inventory, not permission to expose a constructible map.

### Invocation

`resume`:

1. validates version and shape;
2. checks `self()` against the owner;
3. atomically authorizes the transition from `fresh` to `running` for a
   one-shot resumption;
4. selects deep or shallow context restoration;
5. invokes the continuation on the owner process;
6. marks a one-shot resumption consumed even if execution raises;
7. returns the delimiter result or a structured runtime failure.

The consumption authority may use a monitored helper process or another
explicit runtime object. It must not rely on a forgeable source term or
process-dictionary handler lookup.

### Retention

A retained resumption keeps its immutable continuation environment, delimiter
metadata, and context reachable. Runtime helpers monitor the owner process and
release retained operational state when that process exits.

The initial feature is process-affine. Sending the term is not forbidden by
the BEAM, but invoking it from another process is.

## Deep, Shallow, One-Shot, And Multi-Shot

The first executable promotion target is deep plus one-shot:

- the handler frame is active during resumed execution;
- a resumption begins at most once;
- existing value cases tail-resume exactly once.

Shallow support changes context restoration, not continuation capture.
Multi-shot support changes authorization and repeated invocation, not the
source meaning of the captured continuation.

Multi-shot must not claim to copy external world state. Before promotion it
needs:

- a source opt-in spelling;
- a residual-effect admissibility rule;
- specified sharing behavior for stateful handlers;
- deterministic behavior for exceptions and partial branch completion;
- resource limits and diagnostics;
- executable nondeterminism/backtracking evidence.

## Diagnostics

Diagnostics require Catena source locations for:

- malformed `with` binders;
- `resume` outside its valid semantic context;
- resuming a non-resumption;
- operation-result mismatch;
- delimiter-result mismatch;
- residual-effect mismatch;
- direct/resumable ABI mismatch;
- duplicate obvious one-shot use;
- runtime double resume;
- runtime re-entrant resume;
- cross-process resume;
- expired handler or delimiter;
- inadmissible multi-shot effects;
- invalid or stale runtime representation.

Synthetic CPS frames and auto-resume expressions retain an origin chain back
to the source operation case. Raw Core Erlang variable names are not adequate
user diagnostics.

## Promotion Evidence

This architecture becomes a promoted language feature only when executable
source-to-BEAM evidence covers:

1. parsing and pretty-printing `with` and `resume`;
2. type inference for the four `Resumption` parameters;
3. value-handler auto-resume compatibility;
4. explicit resume with a substituted operation value;
5. abort by returning without resume;
6. nested deep handlers and shadowing;
7. resume across direct, recursive, imported, higher-order, and trait calls;
8. retained same-process resumption;
9. double-resume and cross-process failures;
10. source-oriented CPS and runtime diagnostics;
11. Core validation and loaded-BEAM execution;
12. no placeholder continuation or silent direct-style fallback.

Shallow and multi-shot behavior receive separate promotion evidence when their
source opt-ins are accepted and implemented.

Until that evidence exists, component and status documents must distinguish
the implemented typed frontend, compiler control IR, and executable runtime
ABI from planned source-to-Core/BEAM integration.

## Related Material

- [ADR-0003: Explicit Effect Context Runtime](../adr/ADR-0003-explicit-effect-context-runtime.md)
- [ADR-0005: Fail-Closed Backend](../adr/ADR-0005-fail-closed-semantics-preserving-beam-backend.md)
- [ADR-0006: First-Class Resumptions](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md)
- [Type And Effect System](type_and_effect_system.md)
- [Operational Semantics](delimited_resumption_operational_semantics.md)
- [Feature Ledger](delimited_resumption_feature_ledger.md)
- [Effect Runtime](../runtime/effect_runtime.md)
- [Implementation Plan](../planning/delimited-resumptions/README.md)
