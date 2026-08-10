# Compiler

The compiler is the orchestrated executable implementation around Catena's
phase modules. It accepts a supported frontend, checks and verifies semantic
core, optionally lowers that core through OTP, reports stable diagnostics, and
publishes successful artifacts through its CLI.

The compiler is broader than `Catena.Kernel.*`, and it is not the same thing as
`Catena.OTP.Compiler`:

- the **kernel** is the exact 0.1.8 language contract;
- the **compiler** coordinates implementations of that and retained earlier
  frontend contracts; and
- the **OTP compiler boundary** performs only the final Erlang-forms-to-BEAM
  conversion.

## Repository role and authority

The `catena` repository contains an executable model, independent verifiers,
reference execution, backends, interfaces, package gates, and tests. Normative
language authority lives in the
[`catena-research` specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification).

Compiler behavior is conformance evidence. It cannot settle specification
silence by accident. When a guide, test, reference transition, and backend
disagree, find the applicable normative rule, suspend the questionable
conformance claim, and repair the non-authoritative artifacts.

## Two frontend families

The bootstrap compiler retains two deliberately separate source boundaries:

| Frontend | Revisions | Entry path |
| --- | --- | --- |
| versioned JSON AST | 0.1.1 through 0.1.7 | `Catena.AST.Decoder` then `Catena.Compiler` |
| exact kernel S-expression | exactly 0.1.8 | `Catena.Kernel.SExpression`, `Parser`, and `Checker` |

The JSON decoder rejects 0.1.8, and the kernel parser rejects any other exact
revision. This prevents one serialization from being silently treated as an
alternate spelling of another contract.

The two paths have different AST/core modules and feature histories, but both
enforce independent typed-core verification, lower through Erlang Abstract
Format, and converge on the sole `Catena.OTP.Compiler` BEAM boundary.

## Kernel compilation pipeline

The exact path is:

```text
source bytes
  -> S-expression reader
  -> spanned neutral node tree
  -> semantic parser
  -> decoded 0.1.8 module
  -> integrated type checker/elaborator
  -> typed kernel core
  -> independent verifier
  -> kernel backend
  -> Erlang Abstract Format
  -> OTP compiler boundary
  -> BEAM binary

typed kernel core
  -> kernel interface encoder
  -> .cati.json bytes
```

Reference execution branches after verification:

```text
verified typed kernel core
  -> reference stepper
  -> one chosen schedule or bounded all-schedule exploration
```

The reference branch is a test and semantic-evidence path. `compile_kernel/2`
does not run a program with the stepper and embed the answer; it translates
the program for production execution.

## Library facade

[`Catena`](../../../lib/catena.ex) exposes four principal entry points:

```elixir
Catena.check_json(json, options)
Catena.compile_json(json, options)
Catena.check_kernel(source, options)
Catena.compile_kernel(source, options)
```

For the kernel:

```elixir
def check_kernel(source, options) do
  with {:ok, module} <- Parser.parse(source, options) do
    Checker.check(module, options)
  end
end

def compile_kernel(source, options) do
  with {:ok, core} <- check_kernel(source, options) do
    Backend.compile(core, options)
  end
end
```

This small facade is intentional. Phase modules remain directly testable, but
normal callers receive a single `{:ok, ...}` / `{:error, diagnostic}` contract
without handling internal throws or compiler exceptions.

`check_kernel/2` performs no BEAM work. Use it for validation, tooling, or to
obtain verified core for the reference machine. `compile_kernel/2` includes the
same checks and returns module atom, BEAM binary, and metadata only after OTP
accepts the generated forms.

## Command-line facade

[`Catena.CLI`](../../../lib/catena/cli.ex) supplies the exact commands:

```bash
./catena check-kernel path/to/module.catena-kernel
./catena compile-kernel path/to/module.catena-kernel
```

`check-kernel` prints one JSON success summary and writes no artifact.
`compile-kernel` writes `<Module>.beam` and `<Module>.cati.json` beside the
input after compilation succeeds, then prints a JSON summary with output
paths, exact selection, diagnostics, fixed layout, and OTP warnings.

`--interface` may be repeated to supply dependencies. Explicit selection flags
must choose edition 0.1, revision 0.1.8, and no previews for a kernel input.
JSON-only choices such as compact/uniform layout and condition-lowering modes
do not alter the kernel's fixed representation.

A diagnostic is emitted as structured JSON on standard error and exits
nonzero. Invalid source does not publish successful compiler artifacts.
Usage errors have a separate command-line exit status.

## Compiler phase contracts

Each phase narrows what the next phase must consider:

| Phase | Receives | Guarantees on success |
| --- | --- | --- |
| [S-expression](s-expression.md) | bytes | one bounded, balanced, spanned node tree |
| [Parser](parser.md) | node tree | one exact, structurally valid decoded module |
| [Type checker](type-checker.md) | decoded module and interfaces | normalized types/effects and resolved semantic evidence |
| verifier | typed core | independently rederived consistency of core evidence |
| [Reference machine](reference-machine.md) | verified core | inspectable specified transitions and outcomes |
| [kernel backend](otp-lowering.md) | verified core | fixed-layout, deterministic Erlang forms |
| OTP compiler | Erlang forms | deterministic binary or stable backend diagnostic |
| interface encoder | verified core | canonical separate-compilation facts without runtime-layout leakage |

A later phase may defensively reject corrupted input, but it must not silently
supply a guarantee the prior phase omitted. In particular, the backend must not
become a second type checker, and the OTP boundary must not become a Catena
semantic resolver.

## Diagnostics as control flow

Expected source and implementation-limit failures are values:

```elixir
{:error, %Catena.Diagnostic{id: id, message: message, span: span}}
```

Stable IDs identify the rule family. Primary spans identify the source form.
Technical details may explain generated-form errors or selections, but the
main message should use approachable Catena vocabulary.

An internal verifier mismatch is `I001`. OTP rejecting verified generated
forms is `B001`. Neither should be rewritten as if the programmer merely made
an ordinary type mistake; these results point at compiler boundaries that need
developer investigation.

## Artifacts and separate compilation

A successful kernel compile returns two byte-level products with different
roles:

- `.beam` contains executable OTP code and Catena selection/frontend metadata;
- `.cati.json` contains canonical public semantic facts used to check later
  modules.

The interface is not a header scraped from BEAM, and BEAM layout is not a
substitute for interface identity. Imports name a module and exact interface
digest. The checker refuses a missing or substituted interface before process
entry evidence reaches lowering.

Determinism tests compile identical semantic input more than once and compare
both products byte for byte. The stable module `origin` participates in
artifact provenance; the local path remains only diagnostic context.

## Reference and production agreement

The compiler maintains two executable accounts after verification:

| Reference path | Production path |
| --- | --- |
| explicit CEK frames and logical actors | generated Erlang functions and BEAM processes |
| logical spawn-order process IDs | opaque local PIDs |
| explicit selected-PID schedule | BEAM runtime scheduling |
| logical handler frames and resumption IDs | CPS handler map and one-shot runtime token |
| structured outcome and trace | returned value, messages, or Catena trap at host boundary |

They need not have identical internal states. They must agree on specified
observations. Differential tests should compare the strongest observation that
is portable without assuming an unspecified cross-sender schedule.

## Where to make a change

Use this ownership matrix before editing:

| Change | Primary modules | Required companion work |
| --- | --- | --- |
| byte, delimiter, escape, or span policy | `Kernel.SExpression`, `Node`, `SourceSpan` | malformed-input and CRLF/span tests |
| exact form or declaration grammar | `Kernel.Parser` | normative grammar, checker if semantic, parser diagnostics |
| type/effect/data/trait/process rule | `Kernel.Checker` and `Kernel.Verifier` | valid, invalid, and forged-core tests |
| evaluation order or local runtime rule | `Kernel.Stepper` and `Kernel.Backend` | direct and differential tests |
| actor scheduling-observable rule | `Kernel.Stepper`, `Explorer`, and backend | scripted plus all-schedule tests |
| runtime representation | `Kernel.Backend` and reference comparison values | pattern/construction/interface-opacity tests |
| BEAM metadata or compiler options | `OTP.Compiler` | both frontend paths and deterministic artifacts |
| command, output path, or report | `Catena.CLI` / `Catena.Report` | CLI tests and guide updates |
| public library orchestration | `Catena` facade | API result and failure-path tests |

If a change spans a semantic boundary, update both sides in the same slice.
For example, a new expression needs grammar, checking, verification, reference
execution, lowering, diagnostics, and documentation—not only a parser tag.

## Verification strategy

The focused C010 corpus in
[`test/catena/c010_formal_semantic_kernel_test.exs`](../../../test/catena/c010_formal_semantic_kernel_test.exs)
covers the exact envelope and spans, limits, selection, rows, nominal data,
traits, deep affine handlers, proper tail calls, process semantics, bounded
exploration, generated terms, local generalization, interfaces, sendability,
forged core, explicit traps, and deterministic artifacts.

A compiler change should use several complementary test styles:

- behavior-first source cases for public semantics;
- direct phase tests for precise diagnostics and intermediate evidence;
- verifier mutation tests for the trust boundary;
- stepper/explorer tests for dynamic state and schedules;
- reference-versus-BEAM differential tests; and
- repeat-build comparisons for artifact identity.

Then run formatting, warning-free compilation, the complete Mix suite, escript
construction, and `git diff --check`. Documentation-only changes still run the
same suite because executable snippets, paths, and architectural claims should
track a compiling repository.

## Non-negotiable compiler invariants

Keep these invariants visible during review:

1. The exact language selection is resolved before semantic checking.
2. Structural decoding precedes cross-declaration inference.
3. Every implicit semantic choice is resolved before execution or lowering.
4. Independently structured verification guards evidence-bearing core.
5. Reference execution and production lowering do not call each other.
6. The backend consumes semantics; it does not invent them.
7. `Catena.OTP.Compiler` is the sole production BEAM boundary.
8. Interfaces publish semantic facts without runtime representation leakage.
9. Invalid input and compiler defects do not publish successful artifacts.
10. Stable origin and exact selection, not checkout location, determine
    artifact identity.
11. Explicit traps remain explicit; there is no undefined-behavior escape
    hatch.
12. The normative specification remains the authority over every executable
    account.

Return to the [developer guide overview](README.md) for the complete map, or
read the broader [Compiler Architecture](../../../guides/development/compiler-architecture.md)
for package, governance, and retained-revision context.
