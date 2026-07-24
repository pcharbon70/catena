# 6. Effects and handlers

Pure transforms are easy to substitute and test because their results depend
only on their inputs. A delivery system eventually needs information that is
not already in those inputs: today's carrier rate, a file, console output,
state, or a process identifier.

Catena represents those needs as **effects**. An effect declaration names a
set of operations. Performing an operation asks the surrounding environment
to interpret it. A handler supplies that interpretation.

This separation keeps the business transform honest. Its type states which
capabilities it requires, while a caller decides whether those capabilities
mean production I/O, a deterministic test value, or another implementation.

Parcel Relay will move its regional surcharge out of a hard-coded match and
into a rate-lookup effect.

## Declaring an effect

An effect is an interface for operations that are not ordinary pure
transforms:

```catena
effect RateLookup
  operation rate : Zone -> Natural
end
```

`RateLookup` names the capability. `rate` accepts a `Zone` and returns a
`Natural`. The declaration does not say whether rates come from a database,
configuration file, HTTP service, or in-memory table.

Effects may contain multiple operations:

```catena
effect ParcelStore
  operation load : String -> Maybe Parcel
  operation save : Parcel -> Unit
end
```

Keeping related operations together makes a capability boundary visible and
gives handlers a coherent responsibility.

## Performing an operation

`perform` requests an operation from the current effect context. Parentheses
around operation arguments are required by the current grammar:

```catena
perform RateLookup.rate(Regional)
```

A transform that performs the operation records the effect after `/` in its
return type:

```catena
transform quote_for_zone : Zone -> Natural / {RateLookup}
transform quote_for_zone zone =
  let base = 5 in
  base + perform RateLookup.rate(zone)
```

Read the signature as: given a `Zone`, this computation returns a `Natural`
while requiring `RateLookup`.

The effect annotation belongs to the returned computation. For several
arguments:

```catena
transform quote :
  Natural -> Zone -> Natural / {RateLookup}
```

arrows associate to the right, so the effect is attached to the final
`Natural` result.

Multiple required effects are listed together:

```catena
transform quote_and_print :
  Parcel -> Unit / {RateLookup, IO}
```

The set is part of the type contract, not an informal warning hidden in a
comment.

## Handling an effect

A handler turns abstract operations into concrete behavior for one lexical
scope. The current source form is:

```text
handle computation then {
  EffectName {
    operation(patterns) -> result
  }
}
```

For a deterministic Parcel Relay test, rates can be handled with pure pattern
matching:

```catena
transform quote_with_test_rates : Zone -> Natural
transform quote_with_test_rates zone =
  handle (quote_for_zone zone) then {
    RateLookup {
      rate(requested_zone) ->
        match requested_zone of
          | Local -> 0
          | Regional -> 5
          | International -> 15
        end
    }
  }
```

The inner `quote_for_zone` requires `RateLookup`. The handler supplies every
`rate` result, so the outer transform is pure and has no effect annotation.
Conceptually, handling removes the handled capability from the requirement
set.

The handler's parameter is a pattern. A handler may inspect or destructure the
operation arguments just like an ordinary transform clause.

## Why handlers improve testing

Without an explicit effect, a quote transform might call a global rate client.
A test would then need to mutate global configuration, replace a module, or
start external infrastructure.

With `RateLookup`, the business logic only states its need. A test handler can
return fixed rates:

```catena
transform quote_with_flat_rate : Zone -> Natural
transform quote_with_flat_rate zone =
  handle (quote_for_zone zone) then {
    RateLookup {
      rate(_) -> 4
    }
  }
```

The test is deterministic because the interpretation is an ordinary scoped
value. A production handler can use I/O without changing the signature or
branching logic of `quote_for_zone`.

This is dependency injection expressed in the type system: capabilities are
declared, tracked, and interpreted instead of being hidden object fields or
ambient globals.

## Nested scopes

Handlers compose lexically. An inner scope may shadow one effect while its
parent continues to handle other operations.

Imagine a larger delivery report that needs both rate lookup and console I/O.
A broad application scope can provide the ordinary handlers, while one test
wraps just `RateLookup` with a special interpretation. The nested context
changes rates for that computation without changing unrelated users.

The generated-code runtime represents this relationship with explicit effect
contexts. A child context points to its parent. Operation resolution searches
the child first and then walks outward. The runtime does not use an invisible
process dictionary as the authority for generated code.

This explicit model gives nested handling a clear lifetime:

1. create handler processes,
2. construct a child context,
3. execute the body with that context, and
4. clean up the handlers when the body finishes.

## Standard effects

The current standard-library source includes effect modules for I/O, error,
state, and processes.

The I/O surface declares operations such as:

```catena
effect IO
  operation print : String -> Unit
  operation println : String -> Unit
  operation readLine : Unit -> String
  operation readFile : String -> String
  operation writeFile : String -> String -> Unit
end
```

A Parcel Relay transform can declare console output honestly:

```catena
transform announce_label : String -> Unit / {IO}
transform announce_label tracking =
  perform IO.println(tracking)
```

State is represented by `get`, `put`, and `modify` operations. Error provides
an effectful failure channel. Process exposes the standard `spawn`, `send`,
and `self` capability, although complete source-language actor integration
remains a frontier.

The generated-code runtime currently has built-in handlers for I/O and process
operations, including safety boundaries for timeouts, process counts, file
paths, and sizes.

## Effects versus `Result`

Both effects and `Result` can represent something going wrong, but they answer
different questions.

Use `Result a e` when failure is an ordinary domain outcome the caller should
inspect and transform. An invalid parcel weight is data:

```catena
transform validate_weight : Natural -> Result Natural QuoteError
```

Use an effect when a computation requires an ambient capability or an
interpretation supplied by its environment:

```catena
transform quote_for_zone : Zone -> Natural / {RateLookup}
```

The two combine naturally. A rate lookup can be effectful while validation
returns `Result`:

```catena
transform validated_quote :
  Natural -> Zone -> Result Natural QuoteError / {RateLookup}
```

The result context describes a business outcome. The effect set describes what
the computation may ask its environment to do.

## Effects and purity boundaries

Effect types help preserve pure regions:

- a pure transform has no non-empty effect requirement;
- a perform introduces its effect;
- a handler removes the effect it fully interprets;
- a caller inherits any effects that remain;
- a pattern guard must be pure.

These rules let the compiler reject a transform whose implementation performs
an undeclared operation. They also make refactoring visible: adding I/O to a
previously pure helper changes its type and therefore every affected caller.

The compiler contains effect-set normalization, constraints, effect-row
operations, typed-handler machinery, and higher-order effect support. Not all
of that internal machinery has a polished source-language notation, so this
guide stays with the concrete `{EffectName}` surface represented directly by
the current grammar.

## The current implementation boundary

Effect declarations, operation signatures, performs, concrete effect
annotations, handlers, effect inference/checking, and explicit-context runtime
lowering are implemented surfaces. Built-in runtime handling exists for I/O
and Process.

The backend ledger classifies performs and handlers as runtime-lowered rather
than fully source-to-BEAM proven. The richer algebraic-effects implementation
also includes internal resumption and handler orchestration surfaces whose
complete ergonomic source syntax is still evolving. The examples here use
only the parser-native handler form and do not imply ordinary Erlang stacks can
capture true delimited continuations.

## What to remember

- An effect declaration names required operations without choosing an
  implementation.
- `perform Effect.operation(arguments)` requests an interpretation.
- `/ {Effect}` makes the requirement part of a transform's type.
- A handler supplies scoped operation meanings and removes handled
  requirements.
- Explicit effect contexts make nested scope and lifetime visible.
- `Result` models a domain outcome; an effect models an environmental
  capability.
- Pure guards and explicit effect propagation preserve reasoning boundaries.

As an exercise, add a `FuelSurcharge` effect with a `current` operation. Write
one handler that returns `0` and another that returns `3`, then explain why the
quote transform itself does not need to change.

Previous: [Traits, instances, and laws](05_traits_instances_and_laws.md)

Next: [Modules, tests, and the Parcel Relay capstone](07_modules_tests_and_capstone.md).
