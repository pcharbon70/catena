# OTP lowering

OTP lowering is the production path from independently verified kernel core to
a deterministic BEAM binary. It has two deliberately separate boundaries:

1. `Catena.Kernel.Backend` translates Catena semantics into Erlang Abstract
   Format; and
2. `Catena.OTP.Compiler` asks OTP 29 to turn those forms into `.beam` bytes.

Calling both activities “the backend” is convenient in conversation, but
contributors should preserve the distinction. The kernel backend understands
Catena. The OTP compiler boundary understands Erlang forms and artifact
metadata.

## Position in the pipeline

```text
verified :kernel_core
    -> Catena.Kernel.Backend.lower/1
    -> Erlang Abstract Format
    -> Catena.OTP.Compiler.compile/2
    -> deterministic BEAM binary

verified :kernel_core
    -> Catena.Kernel.Interface
    -> deterministic .cati.json
```

Interface construction runs alongside executable lowering. Public semantic
facts come from verified core, not by reverse-engineering the generated BEAM
module.

## Kernel backend API

[`Catena.Kernel.Backend`](../../../lib/catena/kernel/backend.ex) exposes:

```elixir
Backend.lower(core)
# => erlang_abstract_forms

Backend.compile(core, options)
# => {:ok, module_atom, beam_binary, metadata}
#  | {:error, diagnostic}
```

`lower/1` is the translation routine. `compile/2` is the guarded production
entry: it verifies core, lowers it, fixes the exact 0.1/0.1.8/no-preview
selection, invokes the shared OTP boundary, builds the kernel interface, and
returns all successful metadata.

Call `compile/2` for normal production work. A test may call `lower/1` directly
to inspect forms, but doing so bypasses the backend's entry verification and
must not be used to publish a module from untrusted core.

## Verification before translation

The backend first calls
[`Catena.Kernel.Verifier`](../../../lib/catena/kernel/verifier.ex). A malformed
or forged core returns `I001` before Erlang forms are generated or OTP is
invoked.

This protects direct callers as well as the public compiler facade. It also
establishes the backend's central invariant:

> Lowering consumes resolved semantic evidence. It does not infer types,
> select instances, repair coverage, choose handlers, or guess process
> identities.

For example, a trait call already names its selected definition, a constructor
already carries selected constructor evidence, and spawn already carries a
verified process entry. Lowering erases or realizes those decisions.

## Erlang Abstract Format

Erlang Abstract Format represents Erlang syntax as tuples accepted by OTP's
forms compiler. A small generated module begins with attributes equivalent to:

```elixir
{:attribute, line, :file, {origin_charlist, 1}}
{:attribute, line, :module, module_atom}
{:attribute, line, :export, [{function_atom, arity}, ...]}
```

Definitions and process entry points become `:function` forms with clauses.
Expressions become Abstract Format nodes such as `:call`, `:fun`, `:case`,
`:receive`, `:tuple`, `:map`, `:op`, and `:block`.

Source annotations use the original kernel span's starting line. The `:file`
attribute and OTP `:source` option use the module's stable `origin`, not the
machine-local path supplied for diagnostics. This is necessary for identical
source bytes and origin to produce identical artifacts in different checkout
locations.

## Fixed value representations

Kernel 0.1.8 does not expose a configurable layout. The backend uses:

| Kernel value | BEAM representation |
| --- | --- |
| `Int` | Erlang integer |
| `Bool` | `true` or `false` atom |
| `Unit` | `:unit` |
| tuple | Erlang tuple |
| record | Erlang map keyed by validated field atoms |
| structural variant | `{:catena_variant, label_atom, payload}` |
| nominal constructor | `{:catena_constructor, constructor_atom, fields_tuple}` |
| `Process M` | local opaque BEAM PID |

Pattern lowering mirrors these shapes. `or` patterns are expanded into Erlang
clauses, and portable conditions become Erlang guards. A representation change
must update construction, patterns, reference values, differential tests, and
any public claims about opacity together.

Validated source identifiers and byte-offset-derived private names become
atoms during lowering. The strict parser bounds the possible spellings and the
number of nodes before this point; arbitrary runtime input is not converted to
atoms by generated code.

## Direct lowering

Pure and process-only code can use ordinary Erlang control flow. The backend
preserves strict order with explicit nested forms and blocks, emits direct
calls when a global definition and arity are known, and creates curried
function values when a definition is used as a value.

Process operations map naturally to local BEAM primitives:

- a local process entry receives a private worker and an exported hidden
  `__catena_spawn_<Name>/arity` entry when public;
- spawn builds a zero-argument fun around the worker and calls
  `:erlang.spawn/1`;
- self calls `:erlang.self/0`;
- send uses `!` and then yields `:unit`; and
- selective receive becomes an Erlang `receive` with ordered clauses and
  guards.

Erlang's selective receive supplies the required “first accepted message while
leaving skipped messages” behavior. Opaque PIDs supply local process handles.
Kernel code has no operation that reveals the PID representation.

## Effect-directed CPS lowering

Ordinary effect control—`request`, `handle`, and `resume`—requires captured
continuations. Definitions containing that control are lowered through an
explicit continuation-passing path. Generated workers receive a handler map
and continuation in addition to their ordinary curried arguments.

At a high level:

- `handle` installs a function for the selected effect in the handler map;
- `request` evaluates arguments, looks up the selected effect handler, and
  passes the operation, arguments, and current continuation;
- the handler operation creates an affine runtime resumption around that
  continuation; and
- `resume` delegates one-shot enforcement to `Catena.Effect.Runtime` before
  continuing.

Handler return clauses run after the handled computation returns normally.
Operation clauses use the outer handler environment for their own effect-free
bodies, while a resumed computation reinstalls the installed handler, giving
deep rather than shallow handling.

The direct and CPS paths must preserve the same left-to-right evaluation order.
Helpers that lower lists of expressions chain continuations explicitly so
that CPS transformation does not accidentally reorder arguments or fields.

## Explicit failures

Kernel `trap reason` lowers to:

```elixir
:erlang.error({:catena_trap, reason})
```

A request without an installed handler lowers to the same explicit Catena trap
shape with `{:unhandled_effect, effect, operation}` as its reason. These are
specified runtime failures, not undefined behavior and not an invitation to
catch arbitrary Erlang exceptions inside kernel code.

The kernel provides no exception-catching construct. Tests at the host boundary
may catch the Erlang error solely to assert the Catena trap payload.

## The sole OTP compiler boundary

[`Catena.OTP.Compiler`](../../../lib/catena/otp/compiler.ex) is shared by the
kernel and retained JSON pipelines. It is the only production module allowed
to call:

```elixir
:compile.noenv_forms(forms, options)
```

Its fixed compiler options request:

- binary output rather than writing a file;
- returned errors and warnings;
- deterministic compilation;
- stable source provenance; and
- Catena compile metadata.

For kernel artifacts, compile metadata records specification/frontend 0.1.8,
the `kernel-sexpr-0.1.8` frontend identity, edition 0.1, exact language
revision 0.1.8, and an empty preview set.

OTP success returns the module atom, binary, and warnings. OTP rejection
becomes stable diagnostic `B001`, with generated-form errors and warnings in
technical details. OTP errors after independently verified core indicate a
backend defect or unsupported generated form, not a source type error.

No kernel module should call `:compile.forms/2`, `:compile.noenv_forms/2`, Core
Erlang, BEAM assembly, or chunk writers directly. A single boundary keeps
determinism and metadata rules auditable.

## Successful metadata and interface

`Backend.compile/2` returns metadata containing:

- the verified core;
- generated Erlang forms;
- OTP warnings;
- successful compiler diagnostics;
- exact language selection and artifact version;
- fixed-layout identity; and
- decoded and canonical encoded kernel interface.

[`Catena.Kernel.Interface`](../../../lib/catena/kernel/interface.ex) publishes
public value signatures, regular datatype declarations, and public process
signatures/identities needed for separate compilation. It does not publish PID
values, worker names as semantic identity, handler tables, or other runtime
representation details.

## Determinism checklist

Artifact determinism depends on more than OTP's `:deterministic` option:

1. declaration and generated-function order must be stable;
2. map-derived handler operations must be explicitly sorted where order enters
   forms;
3. generated names must derive from stable source spans or semantic names;
4. local filesystem paths must not enter source provenance;
5. exact language selection and frontend identity must enter compile metadata;
6. interface JSON must use canonical encoding; and
7. repeated compilation must compare both BEAM and interface bytes.

A build that behaves the same but changes bytes nondeterministically violates
the artifact contract.

## Debugging a lowering disagreement

When reference and BEAM results differ:

1. verify core explicitly and inspect the selected evidence on the failing
   expression;
2. reproduce the expected path with the
   [stepper](stepper-and-explorer.md) and record its trace;
3. call `Backend.lower/1` in a test and isolate the generated Abstract Format;
4. determine whether the direct or CPS path handled the expression;
5. check left-to-right evaluation and short-circuiting;
6. inspect fixed construction and pattern shapes together;
7. distinguish an OTP `B001` form rejection from a loaded module's runtime
   result; and
8. add a differential regression that would fail if either implementation
   changed alone.

Continue with [Compiler](compiler.md) for the facade, CLI, artifact publication,
and relationship between this exact kernel path and the retained JSON path.
