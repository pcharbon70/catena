# Operators

Catena 0.1.15 defines the operator and punctuation tokens, the whole-source
token stream, and the operator-expression layer. Together with revisions
0.1.9 through 0.1.14, a complete source file now has one deterministic
tokenization.

## The closed inventory

| Class | Tokens |
| --- | --- |
| Arithmetic | `+` `-` `*` |
| Comparison | `<` `<=` `>` `>=` |
| Equality | `==` `!=` |
| Boolean | `!` `&&` `||` |
| Structural | `->` `\|>` |
| Delimiters | `(` `)` `[` `]` `{` `}` |
| Separators | `,` `;` `.` |

Every token is ASCII and matched by maximal munch: `1.0e3` is one 0.1.13
float, `1.` is the integer `1` followed by `.`, `x.y.z` is one 0.1.10
qualified name, and `a-1`, `a - 1`, and `a -1` produce the same three
tokens because spacing never changes the stream.

A position matching no inventory spelling and no atom — `/`, `%`, `=`, `:`,
`^`, `&`, `~`, `?`, or any Unicode symbol — is rejected as `OPR001`.
Sequences such as `<<` and `++` tokenize into shorter valid tokens and fail
at the expression layer instead.

## Capabilities and delimiter frames

Binary operators join on both sides; prefix `-` and `!` join after; closing
delimiters join before; everything else joins nothing. `(` and `[` open
`continued` frames, so their contents may span lines; `{` opens a `block`
frame, so brace contents may be newline-separated. Unbalanced delimiters
surface through the 0.1.11 `LAY002` diagnostic.

## The fixed ladder

| Level | Operators | Associativity |
| --- | --- | --- |
| tightest | atomic operands, groupings | — |
|  | prefix `-` `!` | right-recursive |
|  | `*` | left |
|  | `+` `-` | left |
|  | `<` `<=` `>` `>=` | none |
|  | `==` `!=` | none |
|  | `&&` | left |
|  | `||` | left |
| loosest | `\|>` | left |

Comparisons and equalities do not chain: `a < b < c` is `OPR002`; write
`(a < b) && (b < c)`. Prefix `-` is C018 negation, never part of a literal,
so `-1` is negation of `1` and patterns stay unsigned. `|>` applies its
right operand to its left: `parcel |> map normalize` groups left in chains.
`->` is tokenized for the future clause grammar and is invalid in 0.1.15
expressions; `.` only separates qualified names.

## Diagnostics

| Diagnostic | Meaning |
| --- | --- |
| `OPR001` | a symbol position matches no operator, punctuation, or atom spelling |
| `OPR002` | an invalid operator-expression form: missing operand, reserved token, or chained comparison |

Rejection is transactional — no token stream, tree, or partial output is
published — and there is no recovery in this revision.

## Current boundary

`Catena.tokenize_source/2` and `Catena.parse_operator_expression/1` do not
type-check, resolve names, parse declarations, evaluate, or emit interfaces
or BEAM. Application syntax and the `->` clause structure remain future
grammar work.

The normative contract is the research repository's
[Operators and Punctuation Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/operators-and-punctuation).
