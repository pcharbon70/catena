# Built-In Data Model

Catena 0.1.35 defines which types are built in: a twelve-way
classification with three newly elaborated types.

## The classification

| Type | Decision |
| --- | --- |
| Unit, Bool, Int, Float, tuple, function, process handle | built-in, classified (shipped facts restated) |
| Text, Character, Bytes | **built-in, elaborated now** |
| List, map, set | **library territory** (G101 declares them as ordinary nominal ADTs) |
| Reference | **excluded** (G084's gated era if ever) |

## Text, Character, and Bytes

The C018 pattern: C017's scanner already mints the literals;
elaboration gives them typed meaning.

```elixir
{:ok, %{literal: literal}} = Catena.scan_literal(~s("héllo"))
{:ok, %Catena.Text.Meaning{kind: :text, type: :Text, value: "héllo"}} =
  Catena.Text.elaborate(literal)
```

- **Text** — the decoded Unicode scalar sequence
- **Character** — exactly one scalar, as its code point
- **Bytes** — the decoded byte sequence

Cooked and raw forms of equal content elaborate to equal meanings;
raw-hash counts and provenance stay scanner facts. The types live at
the meaning and classifier level until a frontend encodes their
literals (P109's era) — exactly Float's post-C018 status.

## Comparability

| Type | Equality | Order |
| --- | --- | --- |
| Text | equal scalar sequences | lexicographic by code point |
| Character | equal scalars | by code point |
| Bytes | equal byte sequences | lexicographic by byte |

List/map/set comparability arrives with G101's declarations;
references and process handles never compare; Unit stays
non-comparable.

## Current boundary

Collection declarations remain G101's; construction and update
G042's; string libraries G105's; references G084's; spellings and
the compiled-program path P109's; interpolation permanently excluded
for unprefixed and `r` forms (C017).

The normative contract is the research repository's
[Built-In Data Model Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/built-in-data-model).
