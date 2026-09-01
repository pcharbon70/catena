# Aliases and Newtypes

Revision `0.1.41` answers G062 with one exclusion and two routings.
No new machinery exists; the decision says what the existing forms
mean.

## The alias exclusion

Transparent type aliases do not exist in edition `0.1`. Every type
name is a nominal declaration with its own identity. When two names
must denote one type, the source uses one type. Any future alias
slice must first state identity-sharing, the comparability
interaction, the compatibility treatment, and error-message naming.

## Opaque types are the abstract export

An opaque type is a nominal datatype exported `abstract` (C022):
construction and matching by constructor spelling are unavailable
outside the defining module, and the smart-constructor idiom — a
function holding the invariants — is the sanctioned interface. The
authority vocabulary is complete: transparent or abstract, nothing
else.

## The newtype is a declared form

A newtype is a nominal datatype with exactly one constructor of
exactly one field — the ordinary data declaration:

```scheme
(data Email
  (params)
  (constructor Email (fields Int)))
```

The wrapper's identity is its own, and:

- **Representation** is invisible (both layouts conform); no cost,
  layout, or "zero-cost" promise attaches — representation
  invisibility makes one unstateable.
- **Coercion is explicit**: the constructor wraps, a pattern
  unwraps; confusing wrapper and wrapped types rejects at the type
  checker.
- **Deriving is explicit-target only**: instances attach through
  C073's explicit derivation; nothing flows implicitly through the
  wrapper — it is a fresh nominal type in every respect.
- **Diagnostics carry the nominal spelling** (`A004` names the
  constructor a program gets wrong).

## Witnesses

```elixir
{:ok, core} = Catena.check_kernel(source)
{:ok, 8, %{root_status: :terminated}} =
  Catena.Kernel.Stepper.run(core, "main")   # construct, match, add
```

The abstract-export smart-constructor idiom, wrapper equality,
explicit trait instances, and the rejection witnesses run on the
same existing machinery (`c062_aliases_newtypes_test.exs`).

The normative contract is the research repository's
[Aliases and Newtypes Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/aliases-and-newtypes).
