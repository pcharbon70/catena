# Name Resolution

Revision `0.1.42` answers G066: name resolution is
**type-independent**. Every written name denotes exactly one
declaration, chosen as a function of scope structure alone, and no
annotation — added, removed, or changed — ever moves a name's
target.

## The five-way classification

| Kind | How it resolves |
| --- | --- |
| Field labels | they don't — `select r l` is a typed operation; presence is well-typedness, not choice |
| Trait method names | by scope; the instance dictionary is **evidence selection**, settled at the instance under coherence (`TRT004`), never at a call site |
| Constructors | declaration-scoped by visibility (`transparent`/`abstract`) |
| Literals | self-describing by spelling (`NM-OBL-005`: no expected-type adaptation) |
| Operators | closed-set instantiation (C061) — one rule per operator, no name choice |

## The witnesses

```elixir
# Annotation invariance: a program and its annotation-bearing twin
# resolve every name identically.
{:ok, core} = Catena.check_kernel(source)
{:ok, 13, %{root_status: :terminated}} = Catena.Kernel.Stepper.run(core, "main")

# Shadowing resolves by scope; import collision rejects rather
# than disambiguating by type; trait calls run through instance
# evidence and fail at the instance when it is missing (TRT005).
```

## What never arrives silently

Overloaded-by-type names, expected-type-adapted literals, call-site
ambiguity deferral, and inference-directed field access are
excluded — amendable only by a revision that amends the
classification table explicitly and states how order-independence
survives.

The normative contract is the research repository's
[Name Resolution Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/name-resolution).
