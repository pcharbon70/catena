# Kernel

The kernel is Catena's deliberately small, exact language for stating and
executing the integrated 0.1.8 semantics. It is the subject implemented by the
reader, parser, checker, verifier, reference machine, and backend—not another
phase between them.

## Why Catena has a kernel

The earlier bootstrap revisions established individual semantic slices using
a versioned JSON AST. Revision 0.1.8 integrates the executable parts of those
slices behind one closed textual input and one typed core. That gives
contributors a compact place to answer four questions:

1. What programs are structurally accepted?
2. What programs are statically valid, and what evidence justifies them?
3. What observable behavior do valid programs have?
4. Can that behavior be reproduced through deterministic OTP lowering?

The kernel reduces accidental degrees of freedom. It is small enough for an
independent verifier and reference machine, yet expressive enough to exercise
the interactions that matter: types and effects, data and matching, handlers
and resumptions, and typed processes with selective receive.

## Kernel does not mean operating-system kernel

Here, “kernel” means a minimal semantic language. It does not manage hardware,
memory, or operating-system services. It is also not Erlang's or OTP's runtime
core. BEAM and OTP are compilation targets for one implementation path.

The term describes scope: syntax and features not needed to state the current
semantic contract are intentionally absent.

## The exact envelope

A kernel input is one S-expression with an exact header:

```text
(module Increment
  (edition 0.1)
  (revision 0.1.8)
  (origin "example://increment")
  (export value main)
  (def main
    (signature Int (uses))
    (add 40 2)))
```

The three header choices serve different purposes:

- `edition 0.1` selects the end-user compatibility track;
- `revision 0.1.8` selects this exact cumulative semantic contract; and
- `origin` gives the module stable identity and stable source provenance for
  generated artifacts.

The local file path is useful for diagnostics, but it must not alter artifact
bytes. Selection flags, when supplied through the API or CLI, must agree with
the header and must select no previews.

## What the kernel contains

The integrated language includes:

- strict unary curried functions, calls, recursion, local bindings, tuples,
  primitive integer and Boolean operations, and locally generalized rank-1
  schemes;
- regular positional nominal data, constructor values and patterns,
  structural records, structural variants, ordered clauses, portable
  conditions, and bounded coverage checking;
- closed coherent one-parameter traits and selected method evidence;
- named ordinary effects, named deep handlers, effect-free handler clauses,
  and affine resumptions;
- typed named local process entries, `Process M` handles, spawn, self, send,
  selective receive, and the reserved `Process` effect; and
- an explicit typed-bottom `trap` form.

These facilities are integrated rather than layered as optional extensions.
For example, a value sent to a process must satisfy the same type system that
checks rows and nominal data, while a process entry must handle every ordinary
effect before leaving only the reserved process effect.

## Intentional exclusions

The exact format is not a preview of ergonomic surface syntax. It excludes
comments, layout-sensitive syntax, string runtime values, foreign terms,
timeouts, links, monitors, supervision, distribution, exception catching,
and open extension forms.

An unknown form is invalid. It must not be preserved as an opaque node in case
a later phase understands it. Adding a kernel construct is therefore a
language revision task, not a permissive parser tweak.

## One contract, several representations

It is useful to distinguish the kernel from representations used to implement
it:

| Representation | Meaning |
| --- | --- |
| source bytes | the exact S-expression serialization supplied by a caller |
| `Catena.Kernel.Node` | a balanced, source-spanned list/atom/string tree |
| decoded kernel module | declarations and expressions with structural meaning |
| `:kernel_core` | checked and elaborated forms with types, effects, and resolved evidence |
| reference configuration | control, continuations, processes, mailboxes, and trace for one execution state |
| Erlang Abstract Format | verified production lowering ready for OTP |
| `.beam` and `.cati.json` | deterministic executable and separate-compilation artifacts |

None of these representations alone *is* the kernel. A map tagged
`:kernel_core`, for example, is only trustworthy after the independent
verifier accepts it.

## Two executable accounts

After static verification, the implementation deliberately splits:

- the [reference machine](reference-machine.md) applies the specified
  transition relation directly; and
- [OTP lowering](otp-lowering.md) translates the same core into Erlang forms
  and then BEAM code.

The reference machine is easier to inspect and can expose scheduling choices.
The BEAM path is the production execution mechanism. Differential tests run
both where possible. Neither path defines the language by itself; disagreement
is a defect to investigate against the normative chapters.

## Static and dynamic boundaries

The kernel's static semantics answer whether a module is valid and elaborate
implicit choices such as constructor identity, trait evidence, handler
selection, process entry identity, and effect rows. The dynamic semantics then
operate only on those resolved choices.

That separation creates an important invariant:

> No executor or backend may repair, guess, or re-resolve a semantic choice
> that the checked core left ambiguous.

The independent verifier enforces this boundary twice in the production flow:
the checker verifies before returning core, and the kernel backend verifies
again before lowering.

## Failure model

The kernel has no undefined behavior. Failures fall into explicit classes:

- malformed input or a structurally unknown form is rejected by the frontend;
- a statically invalid module is rejected by the checker;
- forged or inconsistent core evidence is rejected by the verifier;
- exceeding a published parser or exploration bound is a distinct limit or
  inconclusive result; and
- `trap` and an unhandled ordinary effect are explicit runtime failures.

Invalid input does not authorize partial artifacts or arbitrary execution.
The CLI only publishes successful `.beam` and `.cati.json` outputs after the
entire compile path succeeds.

## Where the kernel lives

The implementation is divided across
[`lib/catena/kernel/`](../../../lib/catena/kernel/). The public facade is
[`Catena`](../../../lib/catena.ex), and the exact CLI commands are implemented
by [`Catena.CLI`](../../../lib/catena/cli.ex). The focused behavior corpus is
[`test/catena/c010_formal_semantic_kernel_test.exs`](../../../test/catena/c010_formal_semantic_kernel_test.exs).

The [S-expression](s-expression.md), [parser](parser.md),
[type checker](type-checker.md), [stepper and explorer](stepper-and-explorer.md),
and [OTP lowering](otp-lowering.md) guides explain those pieces separately.

## When a kernel change is legitimate

Before changing a kernel form or behavior, identify the normative heading that
authorizes it. Then update all affected accounts in one semantic slice:

1. exact grammar and examples;
2. parser and source diagnostics;
3. checker and independently structured verifier;
4. reference transition semantics;
5. production lowering;
6. interface shape if public semantics change;
7. reference, BEAM, adversarial, and differential tests; and
8. guides and conformance records.

A change confined to only the easiest implementation path is a warning sign.
The kernel exists to keep the accounts connected and auditable.
