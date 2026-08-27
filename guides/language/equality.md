# Equality and Ordering

Catena 0.1.30 defines comparison: one closed comparable set, bit-exact
floats, and structural recursion.

## The comparable set

```text
equality:   Int | Bool | Float | tuples | records | injections | constructors
ordering:   Int | Float
never:      closures, process handles — identity is G037/G084's
```

- Structural recursion: a composite compares iff every component
  compares (records semantically — field order never matters).
- Monomorphic: both operands unify to one type; mixed Int/Float is
  the existing type error, no coercion.
- The set is closed: strings and binaries don't exist; every G040
  type enters with its comparability in its own slice.
- Built-ins now; an Eq/Ord trait layer is G101+/G061 library work.

## Float semantics

Bit-exact equality — `-0.0 != 0.0` — with total ordering
`-0.0 < 0.0`. No NaN exists (C018's finite-binary64 contract), so
comparison is total with zero special cases; OTP 27's own `=:=`
change is the target precedent. At the value level:

```elixir
true = Catena.Values.comparable?({:catena_variant, :some, {1, 0.0}})
false = Catena.Values.comparable?({:closure, "x", expr, %{}})
:lt = Catena.Values.compare(-0.0, 0.0)
:eq = Catena.Values.compare(-0.0, -0.0)
```

The retained JSON AST carries no float expressions, so float
comparison is witnessed through `Catena.Values` while the evaluator
and BEAM lowering ship total-order forms ready for the first
float-bearing frontend.

## The guard split

C003's condition fragment stays frozen: guards accept the safe
operators over Int/Bool only, enforced by the independent condition
checker. Tuple equality type-checks as a general expression and
rejects in a guard — two checkers, no leakage.

## Current boundary

Identity observability remains G037's; handle semantics G084's;
future types' entries G040's; Eq/Ord traits G061/G101's; spellings
P109's.

The normative contract is the research repository's
[Equality and Ordering Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/equality-and-ordering).
