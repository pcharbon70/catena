# Formal Semantic Kernel

Catena 0.1.8 has an exact S-expression input for conformance work. It is an
executable semantic kernel, not the future ergonomic source language. The
candidate compiler implementation and chapters remain non-normative until the
C010 immutable promotion gate is authorized and recorded.

## What the kernel integrates

One kernel module carries the executable parts of the earlier semantic slices
through a single parser, checker, verified core, reference machine, and BEAM
backend:

- strict unary functions, calls, local bindings, recursion, tuples, integer
  and Boolean operations, including value/effect-restricted local schemes;
- regular nominal data, constructor patterns, structural records and variants,
  and ordered matching with portable conditions and bounded head coverage;
- closed coherent trait instances and erased selected method evidence;
- named ordinary effects, named deep handlers with effect-free clauses, and
  affine resumptions;
- named local process entries, `Process M` handles, spawn, self, send, selective
  receive, and explicit process-local traps.

The format deliberately excludes comments, strings as runtime values,
layout-sensitive syntax, foreign terms, timeouts, links, monitors,
distribution, supervision, and exception catching. An unknown form is invalid;
it is not an extension point.

## Run the exact input

The header fixes the edition, revision, and origin:

```text
(module Counter
  (edition 0.1)
  (revision 0.1.8)
  (origin "example://counter")
  (export value main)
  (export process Sink)
  (process Sink
    (mailbox Int)
    (params)
    (receive
      (case (bind message) (when (greater (var message) 0)) (unit))))
  (def main
    (signature Unit (uses Process))
    (let target
      (spawn Sink)
      (send (var target) 1))))
```

Check it without publishing outputs:

```bash
./catena check-kernel counter.catena-kernel
```

Compile it through OTP 29 Erlang Abstract Format:

```bash
./catena compile-kernel counter.catena-kernel
```

Successful compilation writes `Counter.beam` and `Counter.cati.json` beside
the input. Failure writes neither successful artifact. `--interface` may be
repeated for imported public process entries. If selection flags are supplied,
they must select edition `0.1`, revision `0.1.8`, and no previews.

The input's declared `origin` supplies stable BEAM line-table provenance.
The local filesystem path remains available for diagnostics but does not alter
artifact bytes.

The library API exposes the same boundaries:

```elixir
{:ok, core} = Catena.check_kernel(source, source: path)
{:ok, module, beam, metadata} = Catena.compile_kernel(source, source: path)
```

Metadata includes the independently verified core, Abstract Format, fixed
layout, deterministic interface bytes, selection, and compiler warnings.

## Data and fixed values

Regular positional data uses an explicit declaration:

```text
(data Option
  (params a)
  (constructor None (fields))
  (constructor Some (fields a)))
```

`(construct Some 7)` constructs a value. A match must cover both constructors
unless it has an unguarded binder or wildcard:

```text
(match (var option)
  (case (constructor None) 0)
  (case (constructor Some (bind value)) (var value)))
```

The fixed backend representations are Erlang maps for records,
`{:catena_variant, label, payload}` for structural variants, and
`{:catena_constructor, constructor, fields_tuple}` for nominal constructor
values. Those shapes are backend facts, not operations available to kernel
programs. The 0.1.8 interface publishes exported regular datatype declarations
and public process signatures without publishing PID or worker identity.

## Process behavior

`Process M` is a send-only handle for messages of closed sendable type `M`.
Functions, resumptions, handlers, and evidence are not sendable. A process
entry returns Unit and must handle every ordinary effect internally; only the
reserved unhandleable `Process` effect may remain.

An exported ordinary value may declare a residual ordinary effect. If an
execution reaches its request without an installed handler, execution ends in
the explicit `{:catena_trap, {:unhandled_effect, effect, operation}}` failure.

The observable process rules are intentionally small:

- spawn starts a named local or digest-verified imported entry;
- send evaluates target then message, returns Unit, and never waits for receipt;
- send to a dead target succeeds and drops the message;
- messages from one sender arrive in send order;
- receive removes the oldest mailbox message accepted by the first matching
  clause and leaves skipped messages in place;
- relative order across different senders is unspecified and no fairness is
  promised;
- normal return and `trap reason` terminate only the current process and
  discard its mailbox.

The reference stepper uses logical spawn-order process IDs. The BEAM backend
uses local PIDs, which remain opaque to kernel code.

## Reference evidence

`Catena.Kernel.Stepper` evaluates one local or global transition at a time.
`Catena.Kernel.Explorer` follows every runnable-process choice up to its
published bounds. An exhausted exploration is inconclusive; it is not a
semantic rejection or proof of a counterexample.

The focused corpus compares reference and BEAM values, exercises scripted and
all-schedule behavior, checks per-sender order and selective receive, stresses
proper tail calls, generates closed well-typed terms, and mutates core and
interface evidence adversarially.

## Limits and diagnostics

The parser accepts printable ASCII plus tab, LF, and CRLF. Metadata strings use
JSON escaping. A byte-order mark, lone CR, malformed UTF-8, non-ASCII byte,
unbalanced delimiter, or trailing form reports `SYN001`. Unknown or malformed
kernel forms report `SYN002`. The bootstrap accepts 20,000 syntax nodes and
nesting depth 1,024; exceeding either reports the distinct implementation-limit
diagnostic `SYN003`.

Every source-derived syntax or static diagnostic has a primary half-open
byte/line/column source span. A standalone malformed interface or forged-core
result has no source form to identify. Programmer-facing messages continue to
use ordinary words such as record, variant, process, message, receive, and
trap.

The exact candidate grammar and semantics are in the
[0.1.8 formal semantic kernel chapters](https://github.com/pcharbon70/catena-research/tree/main/60-specification/formal-semantic-kernel).
Continue with [Catena and BEAM](catena-and-beam.md) for the shared backend
boundary.
