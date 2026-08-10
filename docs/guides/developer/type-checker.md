# Type checker

The kernel type checker is an integrated static-semantics and elaboration pass.
It accepts a structurally valid decoded module, proves the required type,
effect, data, trait, handler, and process judgments, resolves implicit choices,
and emits typed kernel core for independent verification.

The implementation module is named
[`Catena.Kernel.Checker`](../../../lib/catena/kernel/checker.ex), but “type
checker” is convenient shorthand. Its job is wider than assigning a type to
each expression.

## Position in the pipeline

```text
decoded module + verified dependency interfaces
    -> Catena.Kernel.Checker
    -> typed and elaborated :kernel_core
    -> Catena.Kernel.Verifier
```

The checker is the first phase allowed to combine information across
declarations. It can answer questions that the parser cannot: which
constructor a pattern names, which trait instance applies, which handler is
selected, whether a message is sendable, and whether actual effects equal a
declared `uses` row.

## Public API

```elixir
Checker.check(decoded_module, interfaces: decoded_interfaces)
```

The result is `{:ok, core}` or `{:error, %Catena.Diagnostic{}}`. In ordinary
use, callers enter through `Catena.check_kernel/2`, which first invokes the
parser. Dependency interfaces are supplied for imported public process
entries; their module and digest must match the module's imports exactly.

## Static environments

Before checking bodies, the checker prepares interconnected environments:

- global value schemes from definition signatures;
- local and imported process entries;
- nominal type declarations and a constructor index;
- trait declarations and closed instance evidence;
- ordinary effect declarations and operation signatures;
- typed deep handlers; and
- contextual mailbox and resumption capabilities.

Preparation validates facts that affect all later expressions: known nominal
types, bound type variables, public signatures that do not expose private
types, complete trait implementations, non-overlapping instance heads,
complete handler operations, and matching interface digests.

This ordering matters. Expression inference should not encounter a partially
constructed declaration universe and make source-order-dependent choices.

## The integrated judgment

For each expression, inference tracks at least:

```text
environment + semantic context + substitution + optional expected type
    |- expression
    => typed expression + type + canonical effects + new substitution
```

The returned expression map is enriched with `type` and `effects`. Forms that
make a static choice also carry evidence such as a selected constructor,
trait method, handler, operation, process entry, mailbox type, or resumption
type. These fields let later phases consume a decision instead of repeating
inference.

Effects are canonical occurrences: the reserved process effect is `:process`,
and an ordinary effect is represented as `{:effect, name}`. Canonicalization
makes definition-signature comparison deterministic and gives the verifier a
single form to rederive.

## Types and unification

[`Catena.Kernel.Type`](../../../lib/catena/kernel/type.ex) represents:

- `Int`, `Bool`, and `Unit`;
- named variables and internal inference variables;
- tuples;
- unary effectful function types;
- structural record and variant rows;
- `Process M`; and
- regular nominal types with arguments.

The internal `:bottom` type is used for explicit traps during checking. It is
not a closed source type and is normalized against an expected type where a
trapping expression appears.

Inference uses occurs-checked substitutions and expected types. Function
application consumes the curried function type one argument at a time and
combines evaluation effects with each latent function effect. Definitions are
then checked against both their declared result signature and their exact
declared `uses` row.

## Local generalization

A local `let` binding is generalized only when its inferred effects are empty
and it satisfies the kernel's value/effect restriction. Generalized inference
variables become deterministic local scheme variables; otherwise the binding
remains monomorphic.

This restriction is semantically important in a language with handlers and
resumptions. Generalizing an effectful computation could duplicate or move a
capability in ways the type system did not authorize. Tests must include both
a genuinely polymorphic pure value and a similar-looking effectful value that
remains monomorphic.

## Data, rows, and matching

The checker validates regular nominal data arity and type parameters, selects
constructors, instantiates their fields, and records selected constructor
evidence on construction and pattern nodes.

Structural row checks enforce unique labels, field presence, compatible
selection/update, and the closed-row rule for extension. Pattern inference
propagates expected types into tuple, variant, constructor, `as`, and `or`
patterns while requiring compatible and non-duplicated bindings.

Clause conditions must stay within the portable condition core, have type
`Bool`, and be effect free. Coverage is conservative and bounded. Boolean,
closed-variant, and regular nominal heads are exhaustive only when the
required unguarded heads or an unguarded catch-all are present. A guarded
catch-all alone does not prove coverage.

## Traits and selected evidence

The kernel admits closed, coherent one-parameter instances. Instance heads
must be closed; every required method must be supplied by a compatible global
definition; and duplicate heads are rejected as overlap.

At a trait call, the checker selects the unique applicable instance and method
definition, checks applied arguments, and records that evidence on the core
node. The backend therefore lowers an already selected call. It must never run
a second trait solver whose choice could diverge.

## Effects, handlers, and resumptions

A request names a declared effect operation, checks its arguments, returns the
operation result type, and adds that effect occurrence. A `handle` selects a
named handler, checks the handled expression against the handler input, removes
exactly the handled occurrence, and produces the handler output type.

Handler return and operation clauses are checked as effect free. Operation
clauses receive one typed resumption capability, and syntactic plus semantic
checks enforce affine use: the resumption can be used at most once and only in
its defining clause. The typed core records enough resumption information for
both the verifier and executors to check the boundary independently.

## Typed processes

A process entry has a closed, sendable mailbox type and closed, sendable
parameters. It must return `Unit` and may leave only the reserved `Process`
effect; every ordinary effect must be handled locally.

The process context gives meaning to:

- `self`, which is valid only inside a process and has `Process M` for that
  process's mailbox `M`;
- `send`, whose target mailbox type determines the message type;
- `receive`, whose clauses match the current closed mailbox type; and
- `spawn`, which resolves a local or digest-verified imported process entry and
  checks its arguments.

Sendable closed values include primitive values, tuples, closed structural
rows, nominal data built from sendable arguments, and suitable process
handles. Functions, open rows, handlers, resumptions, type-class evidence, and
inference variables do not cross a mailbox boundary.

## Typed core

On success the checker assembles a map with `format: :kernel_core`, version
`0.1.8`, profile `:formal_semantic_kernel`, the exact selection and origin,
typed definitions and processes, prepared declaration indexes, imported and
local process entries, handlers, and an empty successful diagnostic list.

The important property is not the map shape alone. Core is an evidence-bearing
contract:

- every expression has normalized type and effect information;
- every implicit selection has a concrete identity;
- definition signatures agree with inferred bodies;
- public and process boundaries are resolved; and
- downstream execution does not need source-level inference.

## Independent verification

Before returning success, the checker calls
[`Catena.Kernel.Verifier`](../../../lib/catena/kernel/verifier.ex). The verifier
rederives expression and pattern judgments, declaration indexes, exports,
handlers, processes, effect rows, and recorded selections without trusting the
checker's substitution or acceptance decision.

This is not redundant defensive programming. It is a separate trust boundary:

```text
source error -> ordinary stable diagnostic
checker emits inconsistent evidence -> I001 internal compiler defect
forged core passed to backend -> verifier rejection before lowering
```

The [OTP backend](otp-lowering.md) invokes the verifier again because callers
can reach `Catena.Kernel.Backend.compile/2` without using the public facade.
Do not weaken verifier checks merely because the checker currently constructs
the field correctly.

## Diagnostics

Static failures use established families for their domain, including `T...`
for type/row rules, `A...` for nominal data, `TRT...` for traits, `EFX...` for
effects, `RES...` for resumptions, `M...` for matching, `CND...` for portable
conditions, and `PRC...` for processes. `I001` is reserved for inconsistent
core produced or received at an internal boundary.

Diagnostics should name programmer concepts—record, variant, process, message,
receive, trap—rather than exposing inference substitutions or Erlang runtime
representations as the primary explanation.

## What the checker must not do

The checker must not:

- accept unknown parser forms as extensions;
- depend on source declaration order for coherent choices;
- publish core without independent verification;
- decide runtime scheduling;
- choose a BEAM representation;
- inspect generated BEAM to infer interface semantics; or
- treat a compiler implementation detail as specification authority.

## Debugging and change checklist

For a static-semantics failure:

1. confirm the parser output has the expected tag, names, and span;
2. identify the relevant environment: values, data, traits, effects,
   handlers, processes, mailbox, or resumptions;
3. inspect the inferred type, canonical effects, and current substitution;
4. distinguish a source diagnostic from an `I001` evidence mismatch;
5. inspect the corresponding verifier judgment rather than copying checker
   code into it;
6. add a valid case, a nearby invalid case, and a forged-core mutation; and
7. if runtime behavior changes, update both the
   [reference machine](reference-machine.md) and [OTP lowering](otp-lowering.md)
   with a differential test.

Continue with [Reference machine](reference-machine.md) to see how verified
core becomes executable behavior without going through BEAM.
