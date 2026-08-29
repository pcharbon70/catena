# Structural Records

Catena 0.1.36 defines structural records and variants: the kernel's
calculus, stated once.

## The seven operations

| Operation | Rule |
| --- | --- |
| `record { l = v }` | closed literal; each label unique — duplicates are static invalidity |
| `select r l` | extracts `l`; the label must be present |
| `update r l v` | base, then replacement; label present |
| `extend r l v` | adds `l`; closed over closed |
| `restrict r l` | removes `l`; closed over closed |
| `inject l v` | a value once `v` is |
| match on variant | semantic label, then payload |

Field order controls evaluation order and **never** equality,
comparison, or row identity.

## Rows

Literals are closed rows; extend/restrict close over closed inputs;
open tails exist **only in type positions** — signatures and type
variables — never from an expression; select requires the label
present. Row polymorphism composes through type positions; missing-
label operations are statically unreachable.

## Representation

Records are semantic finite unique-label-to-value maps; sharing,
copying, and layout are invisible (C037); the BEAM backend rides
maps. Variants carry their semantic label and payload, nothing else.

## The witness path

The kernel S-expression boundary is the only frontend expressing the
operations (`check_kernel` + `compile_kernel`); general frontends
arrive at P109:

```elixir
{:ok, core} = Catena.check_kernel(source)
{:ok, {2, true, 41}, %{root_status: :terminated}} =
  Catena.Kernel.Stepper.run(core, "main")
```

## Current boundary

Collection construction and update remain G042's; aliases G062's;
refutability P044's; spellings and the frontend path P109's; nominal
declarations remain C002's, without structural operations.

The normative contract is the research repository's
[Structural Records and Variants Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/structural-records-and-variants).
