# API and ABI Compatibility

Catena 0.1.24 defines what compatibility the language promises: layered
stances, a strict interface diff matrix, and what version numbers must
say.

## The four layers

| Layer | Stance |
| --- | --- |
| Source | Retained revisions are immutable: a file that checked keeps checking under its exact revision |
| Type/interface | Real rules: the strict diff matrix below |
| Behavior | Declared absence: the deterministic kernel is the behavior contract; no bug-compatibility |
| BEAM ABI | Declared absence: representation is not a surface; binaries are deterministic outputs |

There is no stable BEAM ABI, wire format, or serialization contract:
loading compiled output on any other build is outside every guarantee,
and a representation change never requires a version bump by itself.

## The breaking matrix

Compatibility between a producer's earlier and later interfaces is
decided by the complete diff:

| Change | Class |
| --- | --- |
| Remove or rename an export, datatype, trait, handler, or instance | breaking |
| Change an export's scheme | breaking |
| Widen an export's effect row (new requests) | breaking |
| Narrow an effect row | minor |
| Add an export, datatype, trait, handler, or instance | minor |
| Change representation or layout | never breaking alone |
| Add an entry | minor |
| Remove an entry or change its result | breaking |
| Move the launch marker | minor |

Re-export facades are formally excluded: a forwarding definition is
already expressible and transparent; anything identity-preserving would
need a future lifecycle record. Joint and bundle digests are identity
keys, never compatibility classes; version skew resolves by replaying
`catena.lock`, not by compat rules.

## What versions must say

```elixir
{:ok, %{class: class}} = Catena.Package.Compat.diff(old_interface, new_interface)

{:ok, %{required: :breaking}} =
  Catena.Package.Compat.validate_claim(old, new, {"0.1.0", "0.2.0"})
```

At `1.0.0` and above, breaking requires a **major** increment and
additive changes require minor. Below `1.0.0`, the Cargo-style rule
applies: breaking requires **minor**, additive changes are patch.
Over-signaling is always allowed; under-claiming is `CMP001`. Malformed
input is `CMP002`; drift outside every matrix row is `CMP003`.

## Current boundary

Migration engines (G116/P125), registry retirement and yanks (G130),
hot upgrade (G092), representation/calling-convention/foreign-term
contracts (P093/G094/G095), and release tooling (G121) remain future
work. The 1.0-era convention switch belongs to a future edition record.

The normative contract is the research repository's
[API and ABI Compatibility Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/api-and-abi-compatibility).
