# 1. Orientation and your first transform

Catena is a functional language for the BEAM virtual machine. Its central idea
is pleasantly small: programs are built from immutable values and transforms
between types. Instead of changing an object in place, a transform receives a
value and produces a new value. Larger behavior comes from connecting small
transforms.

That model is useful even before the category-theory vocabulary appears.
Immutable inputs make local code easier to reason about. Explicit outputs make
functions easy to test. Composition lets a larger workflow retain the same
shape as its smallest steps.

In Parcel Relay, the first job is simply to quote a parcel. We will begin with
a weight and calculate a fee. There is no database, mutable cart, or network
call yet—just a deterministic rule.

## The shape of a Catena module

A source file can declare a module, exports, types, transforms, traits,
instances, effects, tests, and properties. A minimal useful application module
usually has:

1. a `module` declaration,
2. one or more `export` declarations, and
3. one or more transform definitions.

Module and type names begin with uppercase letters. Transform names begin with
lowercase letters. Exporting is explicit, so a reader can see the public
surface without scanning the whole file.

Here is Parcel Relay's first module:

```catena
module ParcelBasics

export transform shipping_fee

transform shipping_fee : Int -> Int
transform shipping_fee weight = 5 + weight + weight
```

The signature says that `shipping_fee` accepts an `Int` and returns an
`Int`. The implementation binds the input to `weight` and evaluates the
expression on the right of `=`.

If the parcel weighs `4`, substitution is enough to understand the result:

```text
shipping_fee 4
= 5 + 4 + 4
= 13
```

That ability to reason by substitution is one of the rewards of pure code.
Calling the transform again with `4` cannot produce a different answer unless
the transform explicitly performs an effect, a topic we will reach later.

## Transforms are values between types

The arrow in `Int -> Int` is read “from `Int` to `Int`.” It
describes a reusable relationship, not a sequence of statements that mutates a
hidden parcel.

Catena calls functions **transforms** because it emphasizes this input/output
relationship. In category theory, the corresponding word is *morphism*. You do
not need that term to write the code; the practical point is that a transform
has a source type, a destination type, and can be composed with compatible
transforms.

The identity transform is the simplest example. It returns exactly what it
receives:

```catena
transform identity : a -> a
transform identity value = value
```

The lowercase `a` is a type variable. This transform does not care whether the
value is a number, tracking code, parcel, or route. Its behavior is determined
entirely by its shape.

## Source code and the current compiler

The current compiler takes `.cat` source through lexical analysis, parsing,
semantic normalization, import and name resolution, kind checking, and
type/effect checking. That path produces a typed module and a validated
compilation unit. Public artifact APIs lower accepted programs to validated
Core Erlang and in-memory BEAM binaries.

The executable backend is deliberately fail-closed: when it cannot preserve a
feature's meaning, it rejects the artifact instead of quietly generating a
placeholder. Local and recursive transforms, higher-order calls, pure data and
patterns, effects and handlers, closed source-set imports, and concrete trait
dictionaries all have source-to-BEAM execution evidence. Later guides identify
the remaining boundaries around test artifacts, packaging, and a few deferred
operators.

The first `shipping_fee` example sits close to the proven executable subset:
it has a module name, an exported transform, a variable argument, integer
arithmetic, and a return value.

## Trying small expressions

After `make compile`, the repository's REPL can inspect and evaluate small
expressions. Start it through `rebar3 shell` and
`catena_repl:start().`, as described in the [learning index](index.md).

Inside the Catena prompt, literals and inspection commands are good first
experiments:

```text
catena> 42
catena> "parcel-ready"
catena> [2, 4, 8]
catena> :type 42
catena> :help
```

For a source file, use `:load`:

```text
catena> :load path/to/parcel_basics.cat
```

The REPL is compiler-backed, but it is still a development surface. Some
arithmetic and polymorphic prelude paths have known rough edges, so a rejected
interactive experiment does not necessarily mean that the corresponding
module syntax is absent from the compiler.

## Reading a type signature

Type arrows associate to the right. This matters as soon as a transform takes
more than one argument:

```catena
transform add : Int -> Int -> Int
transform add left right = left + right
```

Read the signature as:

```text
Int -> (Int -> Int)
```

Giving `add` one number produces another transform waiting for the second
number. This is called currying. It is why partial application and composition
fit naturally into Catena; we will use both in the next guide.

## What to remember

- Catena programs transform immutable values.
- A module has an explicit name and public export surface.
- `transform name : Input -> Output` declares a type.
- `transform name argument = expression` defines behavior.
- Lowercase type names such as `a` are type variables.
- Accepted application modules can be compiled to validated in-memory BEAM
  artifacts; unsupported constructs fail closed.

As a small exercise, change `shipping_fee` so that the base fee is `7`, then
define a two-argument transform that also charges `1` unit for every unit of
distance.

Next: [Values and transforms](02_values_and_transforms.md).
