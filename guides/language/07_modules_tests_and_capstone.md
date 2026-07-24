# 7. Modules, tests, and the Parcel Relay capstone

As a program grows, correctness depends on more than individual expressions.
Types need clear ownership, callers need a deliberate public API, effects need
an application-level interpretation, and important rules need executable
examples.

Catena modules provide that boundary. A module owns declarations, exports the
pieces other modules may use, and imports the pieces it depends on. Tests and
properties then describe behavior at both example and rule level.

This capstone organizes the Parcel Relay concepts from the previous guides
into a small layered program:

```text
Parcel.Domain      immutable domain types
       ↓
Parcel.Pricing     validation, quotes, and the RateLookup effect
       ↓
Parcel.App         concrete rate handler
       ↓
Parcel.Tests       examples and general pricing properties
```

The arrows are dependencies, not mutation. Each layer adds a responsibility
while retaining the pure core developed earlier.

## Module identity and file layout

A module begins with `module`. The loader converts a dotted module name into a
lowercase path:

```text
Parcel.Domain  -> parcel/domain.cat
Effect.IO      -> effect/io.cat
Prelude        -> prelude.cat
```

The current default search path includes the standard library and the current
working directory. This is a minimal project/import model rather than a
finished package manager.

Exports are explicit and declaration-kind aware:

```text
export type Parcel
export transform quote
export trait Chargeable
export effect RateLookup
```

Anything not exported remains an implementation detail at the current
compiler environment boundary.

## The domain module

The domain layer should contain values and invariants that do not depend on
I/O or deployment choices. Parcel Relay owns `Zone` and `Parcel` here:

```catena
module Parcel.Domain

export type Zone
export type Parcel
export transform tracking_code

type Zone =
  Local
  | Regional
  | International

type Parcel =
  Parcel String Natural Zone

transform tracking_code : Parcel -> String
transform tracking_code Parcel(code _ _) = code
```

This version uses a positional constructor to keep the capstone compact. The
record-carrying constructor from the type guide is equally valid when named
fields improve readability.

The exported types make their constructors visible through the current import
environment. `tracking_code` provides one stable accessor; other
representation details can remain local until callers genuinely need them.

## The pricing module

The pricing layer imports domain types, defines its domain error, declares the
external capability it needs, and exports one high-level quote transform:

```catena
module Parcel.Pricing

export type QuoteError
export effect RateLookup
export transform quote

import Prelude
import Parcel.Domain

type QuoteError =
  MissingWeight
  | UnsupportedZone

effect RateLookup
  operation rate : Zone -> Natural
end

transform validate_weight 0 = Err MissingWeight
transform validate_weight weight = Ok weight

transform validate_zone International = Err UnsupportedZone
transform validate_zone zone = Ok zone

transform quote :
  Parcel -> Result Natural QuoteError / {RateLookup}
transform quote Parcel(_ weight zone) =
  do {
    valid_weight <- validate_weight weight;
    valid_zone <- validate_zone zone;
    let regional_rate = perform RateLookup.rate(valid_zone);
    Ok (5 + valid_weight + valid_weight + regional_rate)
  }
```

Notice the layering inside the result type:

```text
Result Natural QuoteError / {RateLookup}
```

`Result` describes the business outcome. `RateLookup` describes the
environmental capability. A missing weight becomes explicit error data.
Looking up the current rate remains an effect interpreted by the application.

Only `quote`, `QuoteError`, and `RateLookup` are exported. The validation
helpers are private implementation details, so callers cannot accidentally
bypass the module's intended entry point.

## Import styles

An open import makes exported names available directly:

```catena
import Parcel.Domain
```

A qualified import keeps the module boundary visible:

```catena
import qualified Parcel.Pricing as Pricing
```

The parser also represents selective imports and qualified dotted imports.
The current module system is intentionally minimal: it resolves files, merges
exported type environments, gives local declarations precedence, and detects
important name/import errors. Complete package semantics and executable
cross-module linkage remain future work.

For learning examples, prefer open imports when the vocabulary is small and
unambiguous. Prefer qualified imports when two modules expose similar names or
when the boundary improves the reader's understanding.

## The application module handles rates

The application layer chooses an interpretation for `RateLookup`. It can
change the source of rates without changing validation or quote calculation:

```catena
module Parcel.App

export transform local_quote

import Prelude
import Parcel.Domain
import Parcel.Pricing

transform local_quote :
  Parcel -> Result Natural QuoteError
transform local_quote parcel =
  handle (quote parcel) then {
    RateLookup {
      rate(zone) ->
        match zone of
          | Local -> 0
          | Regional -> 5
          | International -> 15
        end
    }
  }
```

The signature is now pure because the application supplies the rate handler.
A different module could handle the same operation through configuration or
I/O and would expose that remaining effect in its own signature.

This split gives each layer one reason to change:

- `Parcel.Domain` changes when the domain model changes.
- `Parcel.Pricing` changes when validation or pricing policy changes.
- `Parcel.App` changes when operational interpretation changes.

## Example tests and properties

A unit test checks one named example. A property checks a general rule across
generated inputs. Both are valuable:

- examples communicate boundary cases clearly;
- properties explore many values and protect algebraic or domain invariants.

The parser has native declaration forms for both:

```catena
module Parcel.Tests

import Parcel.Domain
import Parcel.Pricing

transform example_quote : Natural -> Natural
transform example_quote weight =
  5 + weight + weight

test "base formula quotes four units at thirteen" =
  example_quote 4 == 13

property "base quote never drops below five" =
  forall weight : Natural.
    5 + weight + weight >= 5
```

A native property names generators after each binding. The current grammar
accepts forms such as:

```catena
property "regional rate is nonnegative" =
  forall weight : Natural, distance : Natural.
    weight + distance >= 0
```

Native `test` and `property` declarations are real front-end AST forms.
Application artifact generation currently rejects them explicitly because
their final executable disposition is deferred. Tests should respect the same
module boundaries as application code. Test a private helper beside its owning
module, or test it through the public behavior rather than weakening the API
only for a test.

## Library-first test suites

Catena also has a standard-library `Test` module. It represents tests,
properties, configurations, law checks, and suites as ordinary values. This
matches Catena's minimal-core direction: testing composition can evolve in the
library rather than requiring every feature to become syntax.

A compact Parcel Relay suite has this shape:

```catena
module Parcel.Suite

export transform pricing_suite

import Prelude
import Test
import Laws

transform pricing_suite =
  suite "Parcel pricing" [
    unit "optional quote mapper identity"
      (fn ignored -> mapperIdentityLaw (Some 5)),
    unit "quote-list accumulator identity"
      (fn ignored -> accumulatorLeftIdentityLaw [5])
  ]
```

The lambda receives an ignored argument because the current source lambda form
has one named parameter. `suite` groups first-class `Test` values. The library
also exposes property configuration, assertions, law-verification descriptors,
 seeds, and iteration controls.

The internal Erlang property engine is broader than the currently polished
source-language bridge. It includes generators, shrinking, reports, laws,
state-machine helpers, process/concurrency support, and advanced testing
utilities. Some parallel and distributed paths remain partial. Treat the
source `Test` and `Gen` modules as the promoted bridge, not as a claim that
every internal helper is already automatic language syntax.

## Testing laws

Domain properties protect Parcel Relay's business rules:

```text
shipping fees are never below the base fee
adding insurance never reduces a quote
zero weight is rejected
handling a fixed rate is deterministic
```

Trait laws protect reusable abstractions:

```text
combine is associative
empty is a left and right identity
map preserves identity and composition
chain preserves pipeline associativity
```

Keep the two levels distinct. A pricing rule can change while `Mapper` laws
must remain universal. Conversely, a lawful `Mapper` says nothing about
whether Parcel Relay charges the right regional fee.

The repository's staged law path is:

1. express laws as pure definitions in `Laws`,
2. execute concrete reusable suites through `Test`,
3. bridge supported known instances into generator-backed law checks, and
4. eventually provide broader automatic discovery and derivation.

## A practical development loop

For the current repository, a healthy loop is:

1. model the domain with types;
2. write pure transforms and explicit signatures;
3. use `Result` for expected business outcomes;
4. introduce effects only for environmental capabilities;
5. handle effects at an application or test boundary;
6. keep module exports small;
7. add examples for boundary cases and properties for general rules;
8. run `make check-specs` for documentation/spec changes and `make test` for
   the complete active suite.

When debugging a language example, identify the stage:

- a lexer error concerns tokens or invalid characters;
- a parser error concerns grammar shape;
- a semantic error concerns names, clause arity, or pattern rules;
- a kind error concerns type-constructor shape;
- a type/effect error concerns incompatible values or capabilities;
- a backend error means the validated program uses a construct whose
  semantics-preserving emission is not yet promoted.

That last category is especially important today. Front-end success and
artifact support are deliberately separate contracts.

## Beyond the capstone

The language and repository have more surfaces than this first learning path:

- `System` and `Flow` model identity, composition, lifting, parallel structure,
  and fan-out;
- the effect implementation contains typed handlers, row machinery, and
  higher-order effect components;
- the local runtime contains processes, actors, supervision, registries,
  pub/sub, and event broadcasting;
- the property engine contains stateful, concurrent, metamorphic, coverage,
  and performance helpers.

Those are natural next topics, but several still have uneven source-language
or backend integration. Learn their current boundaries from the
[developer architecture guide](../developer/architecture_guide.md) and the
[promoted current status](../../specs/planning/current_status.md) before
treating internal modules as settled language syntax.

In particular, the runtime actor toolkit is implemented in Erlang, while
source-language actor declarations and typed protocols are not yet a complete
compiler path. Similarly, the pure `System`/`Flow` trait surface and operators
exist, while canonical instances and later Flow phases remain planned.

## What to remember

- Modules own declarations and expose an explicit public surface.
- Dotted module names map to lowercase directory paths.
- Imports currently merge exported compiler environments; full packaging and
  executable linkage are still evolving.
- Unit examples and general properties protect different kinds of knowledge.
- Native test/property declarations and library-first `Test` values are
  distinct current surfaces.
- Pure domain logic, explicit business outcomes, and handled environmental
  effects form a clean application architecture.
- Always distinguish front-end validation from executable backend support.

The Parcel Relay theme has now grown from one arithmetic transform into a
typed, compositional, effect-aware, modular, and testable design. The
abstractions did not replace the original rule; they gave it a safe place in a
larger system.

Previous: [Effects and handlers](06_effects_and_handlers.md)

Return to the [learning path](index.md).
