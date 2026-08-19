# Literals

Catena 0.1.13 defines the spelling and decoding of one atomic literal. The
library boundary is deliberately smaller than a lexer: callers choose a
logical source-unit index and `Catena.scan_literal/2` consumes exactly one
literal from that position.

```elixir
{:ok, result} = Catena.scan_literal(~S(r#"one "quote""#))
result.literal.kind
#=> :text
result.literal.payload
#=> "one \"quote\""
result.next_unit_index
#=> 16
```

The returned literal retains its logical lexeme, every source-text unit, the
original-byte span, decoded payload, and one provenance piece for each
verbatim scalar or escape. Raw literal line breaks are returned separately as
token-owned source units; they are not 0.1.11 layout events.

## Atomic forms

The accepted forms are:

| Kind | Forms |
| --- | --- |
| Boolean | `true`, `false` |
| Integer | unsigned decimal, `0b` binary, `0o` octal, and `0x` hexadecimal |
| Float | decimal digits on both sides of `.`, optionally followed by an exponent; or decimal digits followed by an exponent |
| Text | cooked `"..."` or raw `r"..."`, `r#"..."#`, and further exact hash counts |
| Character | cooked `'...'`, decoding to exactly one Unicode scalar |
| Bytes | cooked `b"..."` or raw `br"..."`, `br#"..."#`, and further exact hash counts |

Numeric signs are separate future operator tokens, not part of a literal.
Decimal integral parts have no redundant leading zero. One underscore may
occur only between two digits. Based prefixes and raw prefixes are lowercase;
hexadecimal digits may use either case. Hexadecimal floats, suffixes,
`NaN`/`Infinity` spellings, byte-character literals, and compound collection
literals are not part of this atomic grammar.

## Cooked and raw decoding

Cooked literals accept only `\0`, `\t`, `\n`, `\r`, `\\`, `\"`, `\'`, exact
`\xHH`, and `\u{H...}` with one through six hexadecimal digits. A Unicode
escape must denote a scalar value. Text and character `\x` escapes are ASCII;
byte `\x` escapes cover the full octet range. Byte literals reject direct
non-ASCII scalars and all Unicode escapes.

Cooked literals are one source line and backslash-newline continuation is not
defined. Raw literals perform no escape processing, may contain logical LF,
and close only with a quote followed by exactly the opener's hash count. The
language places no maximum on that hash count. Raw byte content is still
direct ASCII only.

Text is not normalized. A character contains one Unicode scalar rather than
one grapheme cluster, so `'😀'` is valid while `'é'` contains two scalars and
is not. Ordinary and raw text are permanently static in this revision; any
future interpolation must use a new opt-in prefix rather than reinterpret
these forms.

## Exact numeric metadata

Integer results include their base, underscore-free digits, and exact
nonnegative mathematical value. Decimal-float results expose underscore-free
integral, fractional, and exponent components plus the exponent sign; the
scanner does not choose a runtime float type or perform rounding. Those
semantic choices remain later numeric-type work.

## Diagnostics and limits

| Diagnostic | Meaning |
| --- | --- |
| `LIT001` | invalid unit index or the selected position is not an atomic literal |
| `LIT002` | an opened cooked or raw delimiter reaches end of input |
| `LIT003` | malformed numeric spelling, escape, scalar, character arity, or byte content |
| `LIM002` | an integer exceeds 4,096 mathematical decimal digits |
| `LIM004` | decoded text or byte payload exceeds 65,536 bytes |

Limit exhaustion and malformed input return no successful literal. All
source-derived failures carry original-byte spans.

## Current boundary

The exact 0.1.13 API does not scan a file, combine comment and layout events,
parse compound literals, assign numeric runtime types, interpret text, emit an
interface, or compile BEAM code. The retained JSON AST and exact 0.1.8 kernel
remain the compilation inputs.

The normative contract is the research repository's
[Literal Grammar Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/literal-grammar).
