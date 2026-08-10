# Stepper and explorer

The stepper exposes one transition of the kernel reference machine. The
explorer repeatedly applies that exact transition to every runnable process in
order to enumerate all reachable outcomes within explicit bounds.

They solve different problems:

- use the **stepper** to reproduce one execution, inspect intermediate state,
  or follow a chosen schedule; and
- use the **explorer** to ask whether different scheduler choices can reach
  different settled outcomes.

## Stepper API

[`Catena.Kernel.Stepper`](../../../lib/catena/kernel/stepper.ex) exposes five
operations.

### Create a configuration

```elixir
{:ok, configuration} = Stepper.initial(core, "main", arguments)
```

The named definition becomes logical process `0`, the root. `initial/3`
checks only that the definition exists and the runtime argument count matches
its curried arity. It returns `{:error, {:unknown_definition, name}}` or a
`wrong_arity` tuple otherwise.

Core should already have passed `Catena.Kernel.Verifier`. Runtime terms passed
from Elixir should match the checked entry ABI; this low-level function does
not dynamically re-run the source type checker.

### Inspect scheduler choices

```elixir
pids = Stepper.runnable_pids(configuration)
```

The result is a sorted list of logical PIDs. A process is runnable when it is
actively evaluating, or when it is waiting in `receive` and its mailbox now
contains an accepted message. Terminated, trapped, and still-blocked processes
are absent.

### Take one transition

```elixir
{:ok, next_configuration} = Stepper.step(configuration, pid)
```

The call advances exactly the selected runnable process and increments the
global `steps` counter. Unknown and non-runnable PIDs return an error tuple.
Given one configuration and PID, the transition is deterministic.

### Run a configuration

```elixir
result =
  Stepper.run(core, "main", arguments,
    budget: 20_000,
    schedule: [0, 0, 1, 0]
  )
```

`run/4` combines `initial/3` and `run_configuration/2`. A schedule list chooses
the named PID at each corresponding scheduler decision. If no schedule is
provided—or after the list is exhausted—the runner chooses the lowest runnable
PID. Naming a PID that is not runnable at that moment returns
`{:scheduled_process_not_runnable, pid, runnable}`.

The default budget is 20,000 global transitions. `run_configuration/2` is
useful when a test has already taken manual steps and wants the normal runner
to finish from that intermediate state.

### Normalize an observation

```elixir
outcome = Stepper.outcome(configuration)
```

The outcome records root status/result/trap, all processes in logical-PID
order, each remaining mailbox, the trace, and the step count. It is a
read-only projection; it does not require the configuration to be terminal.
That makes it suitable for assertions during a manual walk.

## A manual stepping workflow

A useful debugging loop is:

```elixir
{:ok, config0} = Stepper.initial(core, "main")
[pid | _] = Stepper.runnable_pids(config0)
{:ok, config1} = Stepper.step(config0, pid)
IO.inspect(Stepper.outcome(config1))
```

Repeat while examining the selected process's `control`, `stack`, `mailbox`,
and the last trace event. For a local expression bug, keep choosing the same
PID. For an actor bug, record the full runnable set before each choice; the bug
may be in scheduling assumptions rather than the local transition.

## Reading process status

A process has one of four meaningful states:

| Status | Meaning | Runnable? |
| --- | --- | --- |
| `:running` | control can take a local transition | yes |
| `:waiting` | a receive is installed | only if an accepted mailbox message exists |
| `:terminated` | normal result is recorded | no |
| `:trapped` | explicit failure reason is recorded | no |

When receive accepts a message, it removes only that mailbox element, restores
the saved receive environment plus pattern bindings, and makes the process
running again. Skipped messages retain their relative order.

## Explorer API

[`Catena.Kernel.Explorer`](../../../lib/catena/kernel/explorer.ex) exposes:

```elixir
Explorer.explore(core, "main", arguments,
  transition_limit: 20_000,
  configuration_limit: 20_000
)
```

It returns one of:

```elixir
{:ok, %{
  configurations: count,
  transitions: count,
  outcomes: normalized_outcomes
}}

{:exhausted, partial_result}
{:error, initialization_reason}
```

`{:ok, result}` means every reachable configuration within the finite state
graph was processed before either limit. `{:exhausted, result}` means a limit
stopped the search, so the listed outcomes are real reachable evidence but the
set may be incomplete.

## Search algorithm

The explorer performs a breadth-first search:

1. initialize the root configuration;
2. dequeue one configuration;
3. if no PID is runnable, normalize and retain its outcome;
4. otherwise call `Stepper.step/2` once for every runnable PID;
5. hash and enqueue each unseen successor; and
6. stop when the queue is empty or a transition/configuration bound is met.

State hashes use deterministic Erlang term encoding and SHA-256 after dropping
the immutable core, definition index, and accumulated step count. The rest of
the configuration—including processes, mailboxes, trace, resumption state, and
next IDs—remains part of the identity.

Dropping the step count prevents the same semantic state reached at different
depths from appearing distinct. Retaining the trace is conservative: two
states with the same live control but different observable histories are not
silently merged.

## Scheduler nondeterminism

The local transition relation is deterministic. The explorer varies only the
choice of runnable PID. That is enough to expose cross-sender arrival order and
interleavings of spawn, send, receive, effect, return, and trap events.

The explorer does not invent message reordering inside a sender. A sender's
sequential sends append in order. Different sender schedules can nevertheless
produce different mailbox orders, which is why a selective receive program may
have more than one valid outcome.

No fairness filter is applied. Exploration describes finite reachable
transitions under all PID choices up to the bounds; it does not prove that a
particular production scheduler will eventually choose a perpetually runnable
process.

## Outcomes are configurations, not just return values

Explorer outcomes include root status/result/trap, normalized process states,
mailboxes, and trace. Two executions returning the same root value can still be
distinct when their observable process behavior differs.

This is particularly important for a root that spawns children. A root return
does not halt the global machine; exploration continues until there are no
runnable processes. A waiting child may make the final state quiescent even
though the root already has a result.

## Limits are not failures

Keep three bounded results separate:

| Result | Interpretation |
| --- | --- |
| stepper `:budget_exhausted` | this chosen run did not settle within its step budget |
| explorer `:exhausted` | the reachable state graph was not completely searched within its limits |
| process `:trapped` | the program took an explicit runtime-failure transition |

Only the last is a runtime outcome. Neither bound result proves divergence,
safety, or invalidity. Tests should assert `{:ok, exploration}` before treating
the outcome set as complete.

## How this relates to OTP execution

Stepper schedules are logical and deterministic test controls. They are not a
request to force the BEAM scheduler into an identical sequence. Differential
tests instead compare specified observations:

- deterministic pure/reference values can be compared directly;
- explicit traps can be compared by their Catena trap reason;
- per-sender FIFO and selective receive can be exercised with controlled
  programs; and
- explorer outcome sets establish which cross-sender results are permitted.

The [OTP lowering](otp-lowering.md) guide explains how the production path
realizes the same rules using BEAM processes and generated functions.

## Testing patterns

Use the smallest API that demonstrates the property:

- `step/2` for evaluation order, a single event, or an intermediate mailbox;
- `run/4` with a schedule for a reproducible interleaving;
- `run/4` without a schedule for a simple deterministic reference result;
- `explore/4` for all bounded cross-sender or runnable-process choices; and
- reference-versus-BEAM execution for backend agreement.

For an explorer regression, record both limits in the test. A test that passes
only because a low limit truncates an inconvenient branch is not conformance
evidence.

## Change checklist

When editing either module:

1. preserve the rule that `Explorer` delegates every transition to `Stepper`;
2. keep logical IDs and state hashing deterministic;
3. decide whether a new field is semantic state, immutable context, a counter,
   or presentation before changing the hash projection;
4. add direct tests for unknown/non-runnable PID errors and bound handling;
5. test selective receive with skipped messages and multiple clauses;
6. test process-local return and trap behavior;
7. update the backend and differential corpus for semantic changes; and
8. document any new trace label or outcome field.

Read [Reference machine](reference-machine.md) for the semantic model these APIs
drive, or continue with [OTP lowering](otp-lowering.md) for the independent
production path.
