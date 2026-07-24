# 5. Traits, instances, and laws

Concrete transforms are easy to understand: `zone_fee` knows about `Zone`,
and `validate_weight` knows about `Natural`. Reuse becomes more interesting
when several unrelated types support the same kind of behavior.

A **trait** names that behavior without choosing a representation. An
**instance** explains how one particular type provides it. A **constraint**
lets a transform depend on the behavior rather than on a concrete type.

This is similar to an interface or type class, with one important addition:
Catena's standard traits are intended to obey laws. The laws make generic code
predictable enough to reason about algebraically.

Parcel Relay will use traits to calculate charges uniformly and combine quote
parts without knowing their internal representation.

## Naming a capability with a trait

Suppose several Parcel Relay values can contribute to a quote. A trait can
name the single operation the pricing code needs:

```catena
trait Chargeable a where
  charge : a -> Natural
end
```

The type parameter `a` stands for whichever type supplies an instance.
`Chargeable` does not say how a value stores pricing data. It only promises
that `charge` can turn such a value into a `Natural`.

This separation is useful because generic callers depend on a stable
capability instead of inspecting every constructor themselves.

## Implementing a trait with an instance

An instance connects the abstract capability to a concrete type:

```catena
type Zone =
  Local
  | Regional
  | International

instance Chargeable Zone where
  transform charge zone =
    match zone of
      | Local -> 0
      | Regional -> 5
      | International -> 15
    end
end
```

Instance methods use `transform` inside the `where ... end` block. This
instance says that whenever generic code asks for `charge` on a `Zone`, these
clauses provide the behavior.

The same trait could have a completely different instance for an insurance
option:

```catena
type Insurance =
  Uninsured
  | Standard
  | Premium

instance Chargeable Insurance where
  transform charge insurance =
    match insurance of
      | Uninsured -> 0
      | Standard -> 3
      | Premium -> 8
    end
end
```

The two types share behavior without sharing a representation or inheritance
hierarchy.

## Writing constrained transforms

A constrained transform asks for an instance without fixing the concrete
type. Constraints follow the result type with `constrain`:

```catena
transform add_charge : a -> Natural -> Natural constrain Chargeable a
transform add_charge item subtotal =
  subtotal + charge item
```

Read the signature as: for any type `a` that has a `Chargeable a` instance,
accept an `a`, accept a subtotal, and return a new total.

The same implementation can now work with either domain type:

```catena
add_charge Regional 10

add_charge Premium 10
```

Instance resolution is a compile-time obligation. If no suitable instance is
available, the type checker reports a trait or constraint error rather than
silently choosing a fallback.

Multiple type-level constraints are joined with `&`:

```catena
transform inspect_charge :
  a -> Natural
  constrain Chargeable a & Comparable a
```

Only request capabilities that the implementation genuinely needs. A weaker
constraint makes a transform useful for more types.

## Default methods and trait inheritance

A trait may define behavior in terms of more primitive methods. A default
method reduces repetition while preserving the same public contract.

```catena
trait Trackable a where
  tracking_code : a -> String,

  has_tracking : a -> Bool,
  has_tracking value =
    match tracking_code value of
      | "" -> false
      | _ -> true
    end
end
```

Members are separated with commas. An instance has to provide
`tracking_code`; it inherits `has_tracking` unless it provides an override.

Trait inheritance uses `extend`. The child can rely on the parent capability:

```catena
trait Auditable a extend Trackable a where
  audit_key : a -> String
end
```

`Auditable a` is stronger than `Trackable a`. Generic code that needs only a
tracking code should still constrain itself to `Trackable a`.

The prelude uses this structure extensively. `Accumulator` extends
`Combiner`; `Applicator` extends `Mapper`; and `Pipeline` extends both
`Applicator` and `Chainable`.

## Combining quote parts

The standard `Combiner` trait describes an associative way to join two values.
`Accumulator` adds an identity value named `empty`.

Parcel Relay can wrap monetary totals in a domain type:

```catena
import Prelude

type Quote = Quote Natural

instance Comparable Quote where
  transform equals Quote(left) Quote(right) =
    left == right
end

instance Combiner Quote where
  transform combine Quote(left) Quote(right) =
    Quote (left + right)
end

instance Accumulator Quote where
  transform empty = Quote 0
end
```

The code now supports:

```catena
combine (Quote 5) (Quote 8)
```

and the operator form:

```catena
Quote 5 <> Quote 8
```

The compiler desugars `<>` to `combine`. The instance determines what
combination means for `Quote`.

## Why laws matter

An interface signature alone does not say enough about behavior. A malicious
or mistaken `Combiner Quote` could ignore its right input, add a random fee, or
depend on evaluation grouping. Generic code could type-check and still be
impossible to reason about.

The `Combiner` law is associativity:

```text
combine (combine x y) z
=
combine x (combine y z)
```

The `Accumulator` laws say `empty` is a left and right identity:

```text
combine empty x = x
combine x empty = x
```

For `Quote`, ordinary addition makes those equations true:

```catena
transform quote_associativity_example : Bool
transform quote_associativity_example =
  let x = Quote 2 in
  let y = Quote 3 in
  let z = Quote 5 in
  equals
    (combine (combine x y) z)
    (combine x (combine y z))
```

Laws are not decorative comments. They permit safe refactoring. If
associativity holds, a runtime may regroup a long combination, a library may
fold from a different direction, and a reader may simplify the expression
without changing its meaning.

## Mapping laws revisit composition

The previous guide introduced `Mapper`. Its two central laws are:

```text
map id value = value
map (compose f g) value = map f (map g value)
```

The first says mapping identity does nothing. The second says mapping a
composed transform is equivalent to mapping each piece in order.

For Parcel Relay, adding zero to every optional quote must preserve the
optional quote:

```catena
transform mapper_identity_example : Maybe Natural -> Bool
transform mapper_identity_example quote =
  equals (map (fn amount -> amount) quote) quote
```

The repository's `Laws` module contains pure definitions for Mapper,
Applicator, Pipeline, Comparable, Combiner, Accumulator, System, Flow, and
Orderable laws. Concrete and generator-backed execution is staged through the
testing surfaces discussed in the capstone guide.

## Higher-kinded parameters

In `Chargeable a`, `a` represents an ordinary type. In `Mapper f`, `f`
represents a type constructor waiting for one argument:

```catena
trait Mapper f where
  map : (a -> b) -> f a -> f b
end
```

`Maybe` can fill `f` because `Maybe a` becomes a complete type after one type
argument. `List` can too. A plain `Natural` cannot: it is already a complete
type and cannot form `Natural a`.

This “type of a type constructor” is called a **kind**. Catena builds and
validates a kind environment before ordinary declaration typing, so a
higher-kinded mismatch is reported at the correct conceptual layer.

You rarely need to calculate kinds by hand. A practical rule is:

- lowercase `a` usually stands for a complete type;
- lowercase `f` or `m` in a container trait often stands for a type
  constructor;
- the method signatures show how many type arguments it expects.

## Coherence and instance placement

Generic behavior is only predictable if instance resolution finds one
unambiguous meaning. Catena's trait machinery includes coherence checks and a
trait-resolution registry.

As a design habit:

- define an instance near the trait or the type it belongs to;
- avoid duplicate instances for the same trait/type pair;
- do not use overlapping instances to make behavior depend on import order;
- keep laws beside the abstraction and test them for every important instance.

These rules make `combine x y` mean the same thing everywhere its types and
imports are the same.

## The current implementation boundary

Trait declarations, inheritance, constraints, instances, higher-kinded
validation, resolution, coherence support, and the standard-library trait
database are implemented compiler surfaces.

The source-to-artifact backend currently erases trait declarations and has
provisional instance dictionary lowering, but complete source-level trait
dispatch is deferred to backend hardening. Operator examples such as `<>` are
therefore best understood as front-end semantics until that dispatch path is
promoted with executable evidence.

## What to remember

- A trait names behavior independently of representation.
- An instance provides that behavior for a concrete type.
- A constraint lets generic code request the smallest capability it needs.
- `extend` builds a trait hierarchy.
- Default methods derive behavior from more primitive methods.
- Laws state the equations every lawful instance must preserve.
- Kinds describe whether a type constructor still expects arguments.
- Coherence keeps instance resolution unambiguous.

As an exercise, define a `Discountable a` trait, implement it for an insurance
option, and write down at least one law a caller should be able to rely on.

Previous: [Composition and computational context](04_composition_and_context.md)

Next: [Effects and handlers](06_effects_and_handlers.md).
