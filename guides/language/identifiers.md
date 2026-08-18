# Identifiers and Qualified Names

Catena 0.1.10 defines standalone ergonomic identifier spelling without yet
claiming a complete source lexer or parser. The rules use pinned Unicode
17.0.0 data, so acceptance does not change with the bootstrap runtime.

## Check names

Build the escript and pass one or more names as separate arguments:

```bash
mix escript.build
./catena check-identifiers alpha Option.Some '`type`'
```

The command returns deterministic JSON with canonical segments, scripts, and
any confusable-name warnings. It creates no interface or BEAM file.

Library callers can validate one segment, one dot-qualified name, or a name
comparison domain:

```elixir
{:ok, identifier} = Catena.parse_identifier("μεταβλητή")
{:ok, name} = Catena.parse_qualified_name("Option.Some")
{:ok, names, warnings} = Catena.audit_identifiers(["scope", "ѕсоре"])
```

`--deny-diagnostic IDN007` promotes a confusable warning to an error.

## Spelling and identity

- The first scalar must have Unicode 17 `XID_Start`; every later scalar must
  have `XID_Continue`. An underscore is therefore allowed after the first
  scalar but not at the beginning.
- Source spelling must already be NFC. `e` followed by a combining acute mark
  is rejected where the precomposed `é` exists, with an exact source-edit fix.
- Names are case-sensitive. Capitalization has no namespace or role meaning,
  so names written in uncased scripts remain fully usable.
- Each segment must pass the Unicode General Security Profile and UTS #39
  Highly Restrictive script check.

The fixed 0.1.10 keyword list is exposed by `Catena.QualifiedName.keywords/0`.
A valid name between backticks has the same identity as its unescaped spelling,
which makes a keyword available as a user name: `` `type` `` identifies the
name `type` rather than the `type` keyword.

Qualified names are nonempty segments joined by ASCII `.` with no whitespace,
for example `Option.Some`. Revision 0.1.10 defines only this lexical path;
later module and namespace work defines what its segments resolve to.

Revision 0.1.11 adds the next integration boundary: a lossless layout engine
over lexer-supplied token events. It does not change identifier spelling or
make the standalone identifier API select 0.1.11. See
[Whitespace, Separators, and Line Continuation](whitespace-and-layout.md).

## Security diagnostics

| Diagnostic | Meaning |
| --- | --- |
| `IDN001` | empty name, invalid first scalar, or invalid continuation scalar |
| `IDN002` | non-NFC source spelling, with a replacement fix |
| `IDN003` | character excluded by the Unicode General Security Profile |
| `IDN004` | segment outside the Highly Restrictive script profile |
| `IDN005` | unescaped reserved word or malformed backtick escape |
| `IDN006` | malformed qualification or a qualified path passed as one identifier |
| `IDN007` | distinct names in one supplied domain have the same confusable skeleton |

`IDN007` is a warning because confusable detection is intentionally
conservative. Binding scopes and namespaces are not guessed by this API; the
caller supplies the set of names that should be compared.

The normative contract is the research repository's
[Identifier Syntax and Equivalence](https://github.com/pcharbon70/catena-research/blob/main/60-specification/identifiers/identifier-syntax-and-equivalence.md).
