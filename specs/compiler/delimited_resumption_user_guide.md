# Delimited Resumption User Guide

## What Is Implemented

Catena supports first-class delimited resumptions from source through typing,
selective CPS, Core Erlang, validated BEAM artifacts, and same-process runtime
execution. Deep one-shot is the default. `shallow` and `multi_shot` are
explicit handler modifiers; multi-shot is currently admitted only for a
closed, empty residual effect row and a branch-safe captured runtime context.

Effects are tracked in function types and compiled through explicit runtime
contexts. Catena has extensive handler, row-polymorphism, resumption, and
higher-order-effect internals, although ordinary Erlang execution cannot
transparently capture true delimited continuations from the stack.

The compiler therefore reifies Catena continuations with selective CPS. A
runtime resumption is opaque authority over that compiler-created remainder,
not a snapshot of an arbitrary Erlang stack.

## `with`, `Resumption`, And `resume`

An operation case without `with` is a value handler. Its result is
automatically tail-resumed exactly once:

```catena
handle perform Choice.choose() then {
  Choice { choose() -> 41 }
}
```

An operation case with a binder is a control handler:

```catena
transform run ignored = handle
  (let selected = perform Choice.choose() in selected + 1)
then {
  Choice { choose() with k -> resume(k, 41) }
}
```

Here `k` has a compiler-derived type equivalent to
`Resumption OneShot Int Int {}`: it accepts the operation result, returns the
delimiter result, and retains the residual effect row. The program returns
`42` because `resume` supplies `41` to the suspended `perform` and executes
the remaining `selected + 1` expression.

`Resumption k a b e` has four parameters:

- `k`: `OneShot`, `MultiShot`, or a resumption-kind variable;
- `a`: the handled operation's result type supplied to `resume`;
- `b`: the enclosing delimiter's result type returned by `resume`;
- `e`: effects remaining in the captured continuation.

The type is first-class, but source code cannot construct or forge a runtime
resumption. It can only receive one from a `with` binder.

## Resume, Auto-Resume, And Abort

- `resume(k, value)` explicitly enters the captured remainder and returns its
  delimiter result.
- A value case without `with` computes an operation value and automatically
  tail-resumes the remainder.
- Returning from a control case without resuming abandons that computation
  path. The runtime consumes and cleans up an unreturned one-shot authority.
- Re-entering a running authority is an error. A consumed one-shot authority
  cannot be resumed again.

Evaluation order stays source ordered. A resume does not restart the handler
case and does not capture or replay an ordinary BEAM call stack.

## Deep, Shallow, One-Shot, And Multi-Shot

The defaults are `deep` and `one_shot`:

```catena
handle computation then { ... }
```

A deep resume reinstalls the selected handler frame. A shallow resume starts
from that frame's parent context, so another matching operation falls through
to an outer handler:

```catena
handle shallow computation then { ... }
```

Multi-shot is opt-in:

```catena
handle multi_shot
  (let selected = perform Choice.choose() in selected * 2)
then {
  Choice {
    choose() with k ->
      let first = resume(k, 20)
      in resume(k, first + 1)
  }
}
```

Each invocation gets a distinct branch identity and begins from the same
immutable compiler-reified continuation environment. The runtime serializes
branches on the owner process and applies positive budgets for invocation
count, retained words, reductions, cooperative timeout, and branch depth.

Multi-shot does not clone process providers, local value-provider state,
PIDs, ports, references, mailboxes, files, network state, or the outside
world. Capture fails when the context contains capabilities outside the
admitted boundary.

## Ownership And Retention

Every resumption belongs to the BEAM process that captured it. This preserves
`self()`, mailbox ownership, links, monitors, and process-local failure
behavior. Sending the opaque Erlang term is possible, but another process
cannot invoke it; the operation reports `wrong_resumption_owner` without
consuming the owner's authority.

A returned first-class resumption retains its continuation environment,
delimiter metadata, and explicit context until it is resumed, discarded,
expires, or its owner/provider dies. Owner and provider monitors release the
lease. Resumptions are not serializable application data.

## Stable Runtime Failures

Public failures include:

| Category | Meaning |
| --- | --- |
| `invalid_resumption` | Malformed, forged, or unregistered authority |
| `invalid_resumption_version` | Handle/runtime ABI disagreement |
| `wrong_resumption_owner` | Invocation attempted by another process |
| `expired_resumption_owner` | Capturing process exited |
| `stale_resumption_delimiter` | Retained delimiter is no longer live |
| `resumption_reentrant` | Same authority invoked while running |
| `resumption_already_consumed` | One-shot or explicitly discarded authority reused |
| `inadmissible_multishot_context` | Captured state cannot be safely branched |
| `resumption_budget_exceeded` | A multi-shot resource limit was reached |
| `handler_failure` | Handler, provider, timeout, or resumed branch failed |

Failures carry source-oriented origins where available. They do not carry the
private continuation or explicit context.

## REPL And Diagnostic Workflow

`catena_repl:new_session/0,1` creates an owner-affine compiler-backed session.
`session_define/2` accumulates declarations and recompiles one bounded module;
`session_eval/2` compiles an expression as `repl_eval`, validates and loads the
artifact, executes it on the owner, and retains the typed result as `it`.
Embedders may use `session_bind/4`, `session_inspect/2`, and
`session_resume/2,3` for retained runtime values.

The interactive command surface adds:

- `:resumption <name>` for redacted public inspection;
- `:trace on`, `:trace off`, `:trace show`, and `:trace clear`;
- `:trace <positive-limit>` to enable a bounded event buffer.

Inspection shows type identity, kind, current/foreign owner relationship,
state, depth, lifetime, and capture location. It never prints the private
handle, continuation, explicit context, PID, reference, or closure.

Structured traces may include capture, handler selection, resume, abort,
branch, consumption, timeout, and cleanup. Event and resumption IDs are stable
only as non-authoritative diagnostic identities. Source views collapse CPS
wrappers and bridges into transform, perform, handler, binder, resume, and
delimiter frames.

## Performance And Optimization

The default compiler removes no-op `return` IR wrappers and collapses only a
direct bridge carrying `direct_callee` proof. Disable this pass for comparison
with:

```erlang
#{codegen_opts => #{optimize_control => false}}
```

The runtime caches immutable nearest-parent handler lookup metadata and uses
a direct tail path for immediate one-shot value cases. Explicit control and
multi-shot operations retain registry authorization and cleanup.

`catena_resumption_benchmark:suite/0,1` records classification and CPS time,
Core/artifact/source-map size, generated functions, runtime reductions,
allocation estimate, closure size, latency, throughput, scheduler behavior,
lookup and bridge paths, retained words, and branch counters. Thresholds are
promotion safety rails, not a language-semantic guarantee.

## Migration Notes

Code written before `with` remains a value handler and keeps automatic
tail-resume behavior. Add `with k` only when the handler needs to decide
whether, when, or how often to execute the remainder. Existing unmodified
`handle` expressions remain deep one-shot. Add `shallow` or `multi_shot`
explicitly; unsupported residual effects or runtime capabilities fail closed
instead of changing mode silently.

## Deferred Boundaries

The following are not promoted:

- cross-process invocation or ownership transfer;
- serialization, persistence, or distributed resumptions;
- transparent stack capture from ordinary Erlang execution;
- multi-shot over open/non-empty residual rows or arbitrary external state;
- cloning process providers, mailboxes, PIDs, ports, references, files,
  sockets, or other outside-world capabilities;
- preemptive cancellation of a long-running same-process continuation;
- debugger access to private continuation or context representation.

See the [architecture](delimited_resumption_architecture.md),
[operational semantics](delimited_resumption_operational_semantics.md), and
[feature ledger](delimited_resumption_feature_ledger.md) for normative detail.
