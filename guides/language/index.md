# Learn Catena

Welcome! This is the learning path for the Catena language. It starts with the
smallest useful ideas and gradually introduces the abstractions that make
Catena distinctive: immutable data, composable transforms, algebraic data
types, traits, effects, and library-first design.

The guides use one evolving example throughout: **Parcel Relay**, a small
parcel-delivery system. At first it only calculates a shipping fee. By the end,
it models delivery states, validates routes, looks up rates through effects,
organizes code into modules, and tests the rules that hold the system together.
Keeping one domain lets each new idea build on something familiar.

## Before you begin

Catena is an early-stage language. Its lexer, parser, semantic analysis, type
and effect checking, standard-library model, REPL, and Core Erlang lowering are
all real. The executable backend currently proves a smaller subset than the
front end accepts.

These guides use three labels to keep that distinction visible:

- **Language surface** means the syntax is represented by the current parser
  and compiler model.
- **Executable subset** means the current source-to-Core path has direct
  execution evidence for that feature.
- **Frontier** means the design or an internal implementation exists, but its
  complete source-language path is not yet ready to teach as ordinary
  production use.

Most examples teach the language surface. Simple arithmetic and constructor
matching are also in the proven executable subset. Traits, advanced operators,
tests, effects, and handlers are useful and implemented front-end concepts,
but their backend integration is still uneven. Each guide calls out the
boundary where it matters.

## The learning path

Read the guides in order. Later chapters assume the vocabulary and Parcel
Relay model introduced earlier.

1. [Orientation and your first transform](01_orientation.md)
   Learn Catena's mental model, the current developer workflow, and the shape
   of a minimal source module.

2. [Values and transforms](02_values_and_transforms.md)
   Work with literals, immutable bindings, functions, type signatures,
   application, lambdas, and the pipe operator.

3. [Types and pattern matching](03_types_and_patterns.md)
   Model parcel states with product and sum types, then safely take them apart
   with clauses, matches, guards, lists, and records.

4. [Composition and computational context](04_composition_and_context.md)
   Move from isolated functions to reusable pipelines, higher-order
   transforms, `Maybe`, `Result`, mapping, chaining, and `do` notation.

5. [Traits, instances, and laws](05_traits_instances_and_laws.md)
   Learn how Catena names reusable behavior, how types opt into that behavior,
   and why algebraic laws matter.

6. [Effects and handlers](06_effects_and_handlers.md)
   Separate a computation's meaning from the environment that interprets its
   I/O, state, errors, and rate lookups.

7. [Modules, tests, and the Parcel Relay capstone](07_modules_tests_and_capstone.md)
   Organize the growing program, expose a deliberate public API, and turn
   domain rules into examples, unit tests, and properties.

## A small vocabulary map

Catena uses approachable names for several ideas that have mathematical names
elsewhere:

| Catena term | Familiar programming term | Category-theory term |
| --- | --- | --- |
| Type | Data type | Object |
| Transform | Pure function | Morphism |
| System | Composable transforms with identity | Category |
| Mapper | Structure-preserving mapping | Functor |
| Applicator | Independent contextual application | Applicative |
| Pipeline | Dependent contextual sequencing | Monad |
| Flow | Statically structured computation | Arrow |

You do not need category theory to use Catena. The guides always introduce the
programming idea first, show why it solves a concrete Parcel Relay problem, and
only then connect it to the more abstract vocabulary.

## Working with the current repository

Build and test the compiler from the repository root:

```bash
make compile
make test
```

The REPL is implemented as an Erlang module rather than a packaged `catena`
command. For repository development, start an Erlang shell and then start the
Catena REPL:

```bash
rebar3 shell
```

```erlang
1> catena_repl:start().
```

Useful REPL commands include `:type`, `:load`, `:browse`, `:prelude`, `:clear`,
`:help`, and `:quit`. The REPL is useful for exploring literals and the
compiler-backed interactive environment, but some richer language examples in
this course are best understood as source modules while the backend is being
hardened.

## Syntax conventions used here

- Source files use the `.cat` extension.
- Type and constructor names begin with an uppercase letter.
- Transform and value names begin with a lowercase letter.
- Function application uses spaces: `shipping_fee parcel`.
- Comments begin with `--`.
- A transform signature and its implementation both use `transform`.
- Multi-line constructs such as `match`, `trait`, `instance`, and `effect`
  finish with `end` where the grammar requires it.

The guides favor explicit code while a concept is new. Later examples become
more compact as composition takes over.

## Sources of truth

These are learning guides, not language specifications. When implementation
details or maturity change, the canonical references are:

- [Current project status](../../specs/planning/current_status.md)
- [Compiler pipeline](../../specs/compiler/core_compiler_pipeline.md)
- [Type and effect system](../../specs/compiler/type_and_effect_system.md)
- [Pattern matching engine](../../specs/compiler/pattern_matching_engine.md)
- [Standard-library surface](../../specs/stdlib/standard_library_surface.md)
- [BEAM backend feature ledger](../../specs/compiler/beam_backend_feature_ledger.md)

When you are ready, continue with
[Orientation and your first transform](01_orientation.md).
