# Reference machine

The reference machine is the executable account of kernel 0.1.8 dynamic
semantics. It defines how one verified program state changes into the next,
including strict expression evaluation, handlers and resumptions, and the
global interaction of typed local processes.

The machine is a semantic model. The concrete implementation is primarily
[`Catena.Kernel.Stepper`](../../../lib/catena/kernel/stepper.ex), while
[`Catena.Kernel.Explorer`](../../../lib/catena/kernel/explorer.ex) drives the
same transition relation across every bounded scheduler choice.

## Why a reference machine exists

A production backend necessarily mixes language semantics with Erlang forms,
OTP calling conventions, runtime representations, and compiler behavior. A
small reference machine provides a differently structured account that is
easier to inspect one transition at a time.

It serves three related purposes:

- **executable explanation:** a contributor can see the control, environment,
  continuation, mailbox, and trace after each step;
- **semantic oracle:** tests can compare reference outcomes with generated
  BEAM outcomes; and
- **concurrency evidence:** the explorer can enumerate runnable-process choices
  without relying on BEAM's scheduler.

The reference machine does not outrank the normative specification. If it and
the backend disagree, the specification decides which implementation is
wrong—or reveals that the specification needs clarification.

## Required input

The machine consumes typed, elaborated `:kernel_core`, not source text and not
the parser's decoded module. It relies on evidence that static semantics has
already resolved:

- variable and definition structure is well typed;
- constructors and trait calls have selected identities;
- handlers and operations exist and match;
- process entries and mailbox types are resolved;
- matches are exhaustive; and
- residual effects satisfy their declaration context.

`Stepper` does not invoke the verifier itself. Callers should obtain core from
`Catena.check_kernel/2` or explicitly require
`Catena.Kernel.Verifier.verify(core) == :ok` before reference execution.
Runtime arguments passed directly to the low-level stepper are also the
caller's responsibility; the public source checker proves entry signatures,
not arbitrary host terms supplied later by a test.

## Local CEK state

Each running process uses a CEK-style state:

- **control** is either an expression with an environment or a computed value;
- **environment** maps kernel value names to runtime values or closures; and
- **continuation** is represented by a stack of explicit frames.

Frames record what remains after the current subexpression finishes: apply a
function, evaluate the next argument, bind a `let`, construct the remaining
tuple or record fields, select a match clause, return through a handler,
resume a captured continuation, or complete a send.

Making frames data rather than using the Elixir call stack is what makes a
single semantic transition observable and gives tail calls a direct,
testable representation.

## Global actor configuration

Local evaluation sits inside one global configuration. Its important fields
are:

```elixir
%{
  format: :kernel_configuration,
  core: core,
  definitions: definitions_by_name,
  processes: %{logical_pid => process_state},
  next: next_logical_pid,
  root: 0,
  trace: observable_events,
  resumptions: consumed_resumption_ids,
  next_resumption: next_resumption_id,
  steps: transition_count
}
```

A process state records its logical ID, entry name, status, control, stack,
mailbox, mailbox type, result, and trap. A waiting receive additionally retains
its clauses and environment until a matching message arrives.

Logical process IDs are non-negative spawn-order integers. Kernel process
handles have the internal reference value `{:catena_process, logical_pid}`.
They are deliberately distinct from BEAM PIDs; kernel code may send through a
handle but cannot inspect either representation.

## One global transition

A global step selects one runnable process. For a running process, it performs
one local CEK transition. For a waiting process whose mailbox now has an
acceptable message, it performs the receive transition. Terminated and trapped
processes are never runnable.

This separates two sources of behavior:

```text
local transition rules + selected runnable PID = next global configuration
```

Given the same configuration and selected PID, the step is deterministic.
Concurrency enters through the choice among runnable PIDs, which the stepper
can script and the explorer can enumerate.

## Strict expression behavior

The continuation frames make evaluation order explicit. In particular:

- callees are evaluated before arguments, and arguments from left to right;
- `let` values are evaluated before their bodies;
- tuple, record, constructor, request, and spawn arguments are evaluated in
  source order;
- `send` evaluates the target before the message;
- `and` and `or` short-circuit; and
- match clauses are tried in order, with a portable condition evaluated only
  after its pattern matches.

A verified exhaustive match reaching no clause traps with an internal semantic
reason. That outcome signals disagreement between static evidence and dynamic
execution, not an allowed source-level match failure.

## Values and rows

The reference machine uses the same fixed observable value shapes expected by
the kernel backend:

- integers and Booleans use their Elixir/Erlang values;
- `Unit` is `:unit`;
- tuples use tuples;
- records use maps keyed by validated label atoms;
- variants use `{:catena_variant, label_atom, payload}`; and
- nominal values use `{:catena_constructor, constructor_atom, fields_tuple}`.

These are implementation-level comparison values. Kernel programs cannot
reflect on representation tags or use Erlang map operations beyond the row
forms that the language provides.

## Deep handlers and affine resumptions

Entering a handler places a handler marker on the continuation stack. A
request searches outward for the nearest marker handling that named effect,
captures the intervening frames, and evaluates the selected operation clause
in the handler environment.

A resumption packages:

- its unique logical ID;
- the process that owns it;
- the captured continuation frames; and
- the handler marker that makes handling deep.

Resuming reinstalls the captured continuation under the handler and records the
ID as consumed. A second use, cross-process use, malformed value, or request
with no matching handler becomes an explicit process-local trap. An unhandled
ordinary request traps with `{:unhandled_effect, effect, operation}` rather
than invoking arbitrary host behavior.

## Process semantics

The global configuration implements the bounded local actor model:

- spawn allocates the next logical ID and starts a local named process entry;
- self returns the current process's opaque handle;
- send appends `{sender_id, message}` to a live target mailbox and returns
  `Unit` without waiting;
- send to a dead or unknown target still succeeds and drops the message;
- each sender's append order is preserved;
- receive scans the mailbox from oldest to newest and removes the first message
  accepted by the first matching clause, leaving skipped messages in place;
- a receive with no acceptable message waits and becomes runnable when one is
  present; and
- normal return or trap terminates only that process and discards its mailbox.

The machine has no links, monitors, timeouts, supervision, distribution, or
fairness promise. An imported process entry has a verified public identity for
static checking, but the local reference machine traps if asked to spawn an
implementation that is not present in the current core.

## Traces and observations

Observable transitions append structured events such as `:spawn`, `:send`,
`:receive`, `:handle`, `:request`, `:resume`, `:effect_return`, `:return`, and
`:trap`. Events identify the logical process and carry operation-specific data.

The trace is useful evidence about ordering, but it is not compiler logging.
Changing an event shape can affect reference comparisons and explorer state,
so it should be treated as a semantic-evidence change rather than incidental
debug output.

## Terminal outcomes

A complete run advances the global configuration until no process is runnable
or a transition budget is reached. The root process returning does not
immediately stop live children; the global machine continues until the
configuration settles.

The stepper distinguishes:

- `{:ok, value, outcome}` when the settled root terminated normally;
- `{:trap, reason, outcome}` when the settled root trapped;
- `{:quiescent, outcome}` when at least one process is waiting and no process
  can advance;
- `{:budget_exhausted, outcome}` when a run bound is reached; and
- `{:error, reason}` for invalid API operations such as a bad schedule.

Budget exhaustion and bounded explorer exhaustion are inconclusive. They are
not runtime traps and do not make a checked program statically invalid.

## Relation to the stepper and explorer

The reference machine is the model; the [stepper](stepper-and-explorer.md) is
the one-transition API over that model. The explorer is a driver that applies
the stepper once for every runnable PID and searches the resulting state
graph. Keeping that direction of dependency clear avoids implementing one set
of rules for ordinary runs and another for exploration.

## Relation to OTP lowering

The reference machine never calls
[`Catena.Kernel.Backend`](../../../lib/catena/kernel/backend.ex) or
[`Catena.OTP.Compiler`](../../../lib/catena/otp/compiler.ex). Conversely, the
production backend does not invoke the stepper to calculate answers at compile
time.

Both consume the same verified core and implement the same specified behavior
with different mechanisms. That independence supports differential tests for
pure values, data representation, handlers, traps, process ordering, and
selective receive.

## Changing dynamic semantics

A behavior change should include all of the following:

1. a cited normative transition or evaluation rule;
2. checker/verifier changes if new static evidence is required;
3. a small-step reference transition;
4. corresponding OTP lowering;
5. a direct stepper test that exposes relevant intermediate state;
6. an explorer test if scheduler choice matters;
7. a reference-versus-BEAM differential test; and
8. documentation of any new observable event, explicit failure, or limit.

Continue with [Stepper and explorer](stepper-and-explorer.md) for the concrete
APIs used to drive this machine.
