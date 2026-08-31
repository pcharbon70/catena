# Collections: Construction and Update

Collections are nominal algebraic data types with constructors and match:
constructing a list is applying its declared constructors, consuming it is
matching them, and updating it is building a new value. The language layer
adds no dedicated collection operators, no built-in collection types, no
trapping lookup, and no complexity promises — those belong to libraries
and to the declaring slices.

This guide shows what revision `0.1.37` settles with the machinery that
revisions `0.1.2` (nominal data) through `0.1.36` (structural records)
already shipped.

## Construction is constructor application

A declared `List` is an ordinary ADT:

```scheme
(data List
  (params a)
  (constructor Nil (fields))
  (constructor Cons (fields a (List a))))
```

Building one applies `Cons` and `Nil`; there is no list syntax and no
`collection_literal` API. Matching destructures by constructor, and nested
destructure goes through nested matches (nested constructor sub-patterns
read as partial for exhaustiveness):

```scheme
(def second_of
  (signature (Fn (List Int) (effects) Int) (uses))
  (fn (list (List Int))
    (match (var list)
      (case (constructor Nil) 0)
      (case (constructor Cons (bind head) (bind tail))
        (match (var tail)
          (case (constructor Nil) 0)
          (case (constructor Cons (bind second) (bind rest))
            (var second)))))))
```

## Persistent update is constructor application plus match recursion

Replacing a head rebuilds the spine; nothing mutates:

```scheme
(def replace_head
  (signature (Fn (List Int) (effects) (Fn Int (effects) (List Int))) (uses))
  (fn (list (List Int))
    (fn (value Int)
      (match (var list)
        (case (constructor Nil) (var list))
        (case (constructor Cons (bind old_head) (bind tail))
          (construct Cons (var value) (var tail)))))))
```

## A miss is a value, never a trap

A lookup that finds nothing returns typed failure as a value — an
Option-shaped answer — so lookups stay total. Key comparison rides the
`0.1.30` comparable set (`equal`), so keys must be comparable:

```scheme
(def find
  (signature
    (Fn (List (Pair Int Int)) (effects) (Fn Int (effects) (PairMap Int Int)))
    (uses))
  (fn (entries (List (Pair Int Int)))
    (fn (target Int)
      (match (var entries)
        (case (constructor Nil) (construct None))
        (case (constructor Cons (bind entry) (bind rest))
          (match (var entry)
            (case (constructor Pair (bind key) (bind value))
              (match (equal (var key) (var target))
                (case true (construct Some (construct Pair (var key) (var value))))
                (case false (call (call (var find) (var rest))
                                 (var target)))))))))))
```

## What the language layer excludes

- **No complexity promises.** Representation is invisible and nominal data
  is representation-independent, so a language-level cost bound would make
  representation observable. Libraries (G101) document costs per operation.
- **No duplicate-key rule.** The declaring slice of any key-carrying
  collection must state its duplicate-key behavior explicitly.
- **No collection built-ins.** Ordering and key equality are exactly the
  `0.1.30` comparable set; non-comparable key types do not compare.

## The witness path

The kernel S-expression boundary carries the witness (`check_kernel` +
`compile_kernel` + the stepper), agreeing with compiled BEAM by selected
values:

```elixir
{:ok, core} = Catena.check_kernel(source)
{:ok, {3, 3, 10, 2}, %{root_status: :terminated}} =
  Catena.Kernel.Stepper.run(core, "main")
```

## Current boundary

Collection declarations, miss-type contents, and library operations remain
G101's and G105's; spellings and the general frontend path remain P109's;
the comparable set is C035's; structural records are C041's and stay
distinct from collections.

The normative contract is the research repository's
[Collection Construction and Update Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/collection-construction-and-update).
