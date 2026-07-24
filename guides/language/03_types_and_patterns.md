# 3. Types and pattern matching

Numbers were enough for the first quote, but they did not explain what a number
meant. A weight, distance, fee, and tracking identifier could all be mixed up
if they shared the same primitive representation. Domain types give those
values names and legal shapes. Pattern matching then lets a transform handle
each shape explicitly.

This is where Parcel Relay begins to look like a real domain model. We will
represent zones, parcels, and delivery states so that invalid combinations are
harder to express.

## Product and sum types

A **product type** holds several values together. A parcel has a tracking code,
weight, and destination zone at the same time.

A **sum type** represents a choice between alternatives. A zone is local,
regional, or international—but never all three at once.

Catena defines both with `type`. Constructors begin with uppercase letters:

```catena
type Zone =
  Local
  | Regional
  | International

type Parcel =
  Parcel {
    tracking: String,
    weight: Natural,
    zone: Zone
  }
```

`Local`, `Regional`, and `International` are nullary constructors: they carry
no additional data. `Parcel` is a constructor carrying one record value. The
record gives each field a useful name.

A parcel value can now be constructed as:

```catena
Parcel {
  tracking: "PR-100",
  weight: 4,
  zone: Regional
}
```

This value has the nominal type `Parcel`, rather than merely being any record
with three coincidentally similar fields.

## Variants can carry different data

Each choice in a sum type may carry its own payload. A delivery state can
record the hub or recipient relevant to that state:

```catena
type DeliveryState =
  Created
  | InTransit String
  | Delivered String
  | Delayed String
```

The type says what information is available. `Created` has no payload.
`InTransit "north-hub"` contains a hub name. `Delivered "Mina"` contains the
recipient, and `Delayed "weather"` contains a reason.

This is safer than a record full of optional fields such as
`delivered: false`, `recipient: None`, and `delay_reason: None`. With the sum
type, each constructor makes one legal state explicit.

## Pattern clauses

A transform can define one clause per input shape. Pattern clauses are often
the clearest choice when the transform's whole job is to interpret a
constructor.

The zone fee is naturally exhaustive:

```catena
transform zone_fee : Zone -> Natural
transform zone_fee Local = 0
transform zone_fee Regional = 5
transform zone_fee International = 15
```

The compiler groups the clauses under the preceding signature. When
`zone_fee Regional` is evaluated, only the `Regional` clause applies.

Patterns can also bind constructor payloads:

```catena
transform state_detail : DeliveryState -> String
transform state_detail Created = "label-created"
transform state_detail InTransit(hub) = hub
transform state_detail Delivered(recipient) = recipient
transform state_detail Delayed(reason) = reason
```

Inside `InTransit(hub)`, the name `hub` is bound to the constructor's string
payload. Constructor patterns also accept a whitespace form in some positions,
but the parenthesized form makes nested patterns easier to see and matches the
current executable constructor examples.

## Match expressions

Use a `match` expression when branching is one part of a larger transform.
Unlike a statement-style switch, a match produces a value.

Here is an explicit description of each state:

```catena
transform state_message : DeliveryState -> String
transform state_message state =
  match state of
    | Created -> "Parcel registered"
    | InTransit(hub) -> hub
    | Delivered(recipient) -> recipient
    | Delayed(reason) -> reason
  end
```

The syntax has four parts:

1. `match` introduces the expression,
2. the scrutinee follows it,
3. `of` begins the clauses, and
4. `end` closes the match.

Each clause starts with `|`, places a pattern before `->`, and places its result
expression after `->`.

Catena also has a pattern-only match form:

```catena
transform is_terminal : DeliveryState -> Bool
transform is_terminal =
  match
    | Delivered(_) -> true
    | _ -> false
  end
```

This form behaves like a transform whose argument is matched by its clauses.
The underscore wildcard accepts a value without binding a name.

## Destructuring records and constructors

Patterns can reach through the `Parcel` constructor and its record payload.
Bind only the fields a transform needs:

```catena
transform parcel_weight : Parcel -> Natural
transform parcel_weight parcel =
  match parcel of
    | Parcel({tracking: _, weight: weight, zone: _}) -> weight
  end
```

Every field is shown because current record patterns are closed shapes. The
tracking code and zone are deliberately ignored with `_`; the weight is bound
to the lowercase name `weight`.

When several fields matter, bind them together:

```catena
transform parcel_quote : Parcel -> Natural
transform parcel_quote parcel =
  match parcel of
    | Parcel({tracking: _, weight: weight, zone: zone}) ->
        5 + weight + weight + zone_fee zone
  end
```

This example combines the modeling work with the pricing rules from the first
two guides. The important improvement is that the relationship between weight
and zone now travels in a typed parcel value.

## Tuples and lists

Tuples group a fixed number of values by position. They are useful for small,
local results:

```catena
transform quote_with_code : Parcel -> (String, Natural)
transform quote_with_code parcel =
  match parcel of
    | Parcel({tracking: code, weight: weight, zone: zone}) ->
        (code, 5 + weight + weight + zone_fee zone)
  end
```

Lists hold zero or more values of one element type. The empty list pattern is
`[]`; the cons pattern `head :: tail` separates the first element from the
rest.

```catena
transform parcel_count : List Parcel -> Natural
transform parcel_count parcels =
  match parcels of
    | [] -> 0
    | _ :: rest -> 1 + parcel_count rest
  end
```

List expressions use commas—`[first, second]`—while the most portable way to
deconstruct an arbitrary list is the `head :: tail` form shown above.

## Guards refine a pattern

A guard adds a pure condition after a pattern. Parcel Relay can classify
handling effort without inventing a constructor for every numeric range:

```catena
transform handling_fee : Natural -> Natural
transform handling_fee weight when weight > 20 = 12
transform handling_fee weight when weight > 10 = 7
transform handling_fee _ = 3
```

Clauses are considered in order. A weight of `25` satisfies the first guard. A
weight of `15` skips the first and satisfies the second. The wildcard is the
fallback.

Guards must remain pure. Performing I/O or another effect while deciding
whether a pattern matches would make clause selection depend on hidden
behavior. The semantic pipeline enforces this purity rule.

## As-patterns and or-patterns

An as-pattern binds both a decomposed shape and the whole value. It is useful
when a transform needs to inspect a value and also pass the original onward:

```catena
transform retain_delayed : DeliveryState -> DeliveryState
transform retain_delayed state =
  match state of
    | Delayed(reason) as original -> original
    | other -> other
  end
```

An or-pattern lets several shapes share one result:

```catena
transform is_active : DeliveryState -> Bool
transform is_active state =
  match state of
    | Created | InTransit(_) -> true
    | Delivered(_) | Delayed(_) -> false
  end
```

All alternatives in an or-pattern must bind the same variable names. The
example uses wildcards in payload positions, so every alternative binds
nothing. Writing `InTransit(hub) | Delayed(reason)` as one alternative group
would be invalid because its branches bind different names.

## Exhaustiveness and the current boundary

An exhaustive match covers every possible constructor. Exhaustiveness matters
because adding a new state should force the places that interpret states to be
reconsidered.

Catena has implemented and tested exhaustiveness, missing-pattern, and
redundancy analysis. The public Core pipeline currently enforces guard purity,
clause arity, and or-pattern binding consistency, while automatic collection
of exhaustiveness and redundancy warnings at that boundary remains follow-on
work.

For now, write exhaustive matches by discipline even when a particular
compiler entry point does not surface the warning automatically. Prefer an
explicit constructor list over `_` when every state has domain meaning; use a
wildcard when the transform genuinely treats all remaining values alike.

## What to remember

- Product types combine values; sum types choose between alternatives.
- Constructors make legal domain states explicit.
- Transform clauses and `match ... of ... end` both use patterns.
- Matches are expressions and return values.
- `_` ignores a value; `name` binds it.
- Guards refine patterns and must be pure.
- `head :: tail` deconstructs a non-empty list.
- As-patterns retain the whole value; or-patterns share a body.

As an exercise, add a `Returned String` delivery state and update every
state-related transform without relying on a wildcard.

Previous: [Values and transforms](02_values_and_transforms.md)

Next: [Composition and computational context](04_composition_and_context.md).
