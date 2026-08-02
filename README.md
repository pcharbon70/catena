# Catena

> A category theory-inspired functional programming language for the BEAM VM

## Rewrite in progress

Catena is being rebuilt from a clean foundation. This history intentionally
starts without the proof-of-concept implementation so the compiler,
architecture, and development workflow can be reconsidered without carrying
forward accidental constraints.

## Historical implementation

The complete proof-of-concept implementation and its history remain available
in Git:

- Branch: `archive/poc-v1`
- Final annotated tag: `poc-v1-final`

To inspect or build that implementation locally:

```bash
git switch archive/poc-v1
```

To return to the rewrite:

```bash
git switch rewrite
```

## Current status

The clean rewrite now contains the first executable type-system and data slice. The
bootstrap toolchain is written in Elixir 1.20.2 on Erlang/OTP 29.0.4 and targets
only the BEAM VM. It does not reuse the historical proof-of-concept's compiler
or language design.

The normative language definition belongs to the separate
[Catena research specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/type-system).
This repository provides the executable model and conformance evidence for
that specification.

## Compiler path

```mermaid
flowchart LR
    JSON[Versioned JSON AST 0.1 or 0.2] --> D[Nominal data elaboration]
    D --> W[Principal and annotation-directed inference]
    W --> C[Pattern coverage and decision tree]
    C --> TC[Typed core]
    TC --> V[Independent core verifier]
    V --> EAF[Erlang Abstract Format]
    EAF --> OTP[OTP 29 compile:noenv_forms/2]
    OTP --> BEAM[.beam]
```

The JSON AST is a temporary versioned toolchain input, not a proposed Catena
surface syntax. A later parser will feed the same typed pipeline. The backend
does not emit Core Erlang, BEAM assembly, or `.beam` files directly; OTP's
supported compiler interface is the sole binary-generation boundary.

The implementation preserves the C001 evidence and adds the C002 data and
pattern-matching slice. Together they include:

- Algorithm W for literals, variables, lambdas, application, polymorphic
  `let`, tuples, and signatures;
- occurs-checked unification, export-signature enforcement, and skolemized
  signature checking;
- executable unique value-row and duplicate effect-row contracts;
- an open, owned, non-overlapping trait registry with functional-dependency
  metadata and associated-type lookup;
- GADT/existential scope checks and a runtime one-shot resumption token;
- an independently structured typed-core verifier and bounded declarative
  typing oracle;
- deterministic OTP 29 compile, load, and execution tests.
- closed nominal datatype declarations, atomic mutual recursion, transparent
  or abstract constructor interfaces, and origin-based nominal identity;
- positional and named construction, typed constructor, literal, tuple,
  binder, wildcard, `as`, and `or` patterns;
- exhaustive and redundancy checking with concrete witnesses, empty-type and
  GADT refinement, guarded fallthrough, and deterministic complexity limits;
- annotation-directed GADT branches and rigid existential escape checks;
- explicit constructor-complete `fold` generation checked by the typed-core
  verifier;
- deterministic, SHA-256-protected `.cati.json` interfaces with no runtime
  layout details;
- uniform reference and compact BEAM representations checked against a pure
  semantic evaluator; and
- independently rejected corrupted constructor and decision-tree metadata.

This is not yet a Catena source parser or a complete implementation of traits,
effects, handlers, structural variants, programmable patterns, or foreign-term
validation. The JSON AST remains the bootstrap boundary.

## Build and test

[asdf](https://asdf-vm.com/) reads the pinned versions from `.tool-versions`:

```bash
asdf install
asdf exec mix test
asdf exec mix escript.build
```

The CLI accepts three commands:

```bash
./catena check-ir program.json
./catena elaborate-ir --interface dependency.cati.json program.json
./catena compile-ir --layout compact program.json
./catena compile-ir --layout uniform program.json
```

`compile-ir` writes an OTP-generated `.beam` and a deterministic `.cati.json`
interface beside the input. `--interface` is repeatable. AST 0.1 programs are
normalized into the AST 0.2 compiler representation; new datatype programs
use AST 0.2 and supply a canonical package/build origin. Every exported value
still requires a signature.

## Intended evolution

Elixir is the bootstrap implementation language, not part of Catena's target
semantics. Once Catena can express and validate the compiler, the toolchain is
intended to self-host while preserving the verified typed-core and OTP 29
Abstract Format boundary.
