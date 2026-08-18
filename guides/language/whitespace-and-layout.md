# Whitespace, Separators, and Line Continuation

Catena 0.1.11 defines how a future lexer-supplied event stream classifies
whitespace and line boundaries. Indentation is non-semantic. A logical newline
or semicolon normally separates forms, while token capabilities and delimiter
frames can make a newline soft.

This is an executable integration boundary, not a whole-source lexer or parser.
Comments, literals, concrete operators, punctuation, and complete grammar
remain later language work.

## The whitespace repertoire

Outside token-owned content, layout whitespace is exactly:

- ASCII space (`U+0020`);
- ASCII tab (`U+0009`); and
- the logical LF produced by the 0.1.9 source-text decoder.

Other Unicode space and line-separator characters are not silently treated as
layout. They fail with `LAY001` over their original-byte span. A CRLF accepted
by the source-text layer is still one logical LF and retains its two-byte span.

Leading spaces and tabs do not open or close blocks and do not create indent or
dedent tokens. Catena defines no semantic tab width. Changing legal indentation
therefore leaves the separator and continuation result unchanged.

## Hard and soft line breaks

A logical LF after a significant token is normally a hard `separator`. Further
LFs in the same gap are `blank`. Leading LFs are also `blank`, and a complete
final form does not need a final LF. A semicolon is always an explicit hard
separator.

A token can instead declare that it joins to the expression before it, the
expression after it, or both. A future concrete lexer will assign these
`join_before` and `join_after` capabilities to the operators and punctuation
defined by later grammar work. An LF-containing gap is `soft` when either
adjacent token requires the join.

Delimiter tokens can also open one of two frame modes:

- a `continued` frame softens eligible line breaks, as multiline argument or
  grouping syntax generally needs; and
- a `block` frame leaves line breaks hard, so it can contain sibling forms.

The innermost frame controls delimiter-based continuation. Token join
capabilities can still soften an individual gap inside either mode. A
semicolon is never softened.

## Use the event API

`Catena.resolve_layout/2` consumes source-ordered `Catena.Layout` events. The
lexer supplies opaque tokens with spans and capabilities, horizontal
whitespace as source-text units, logical LF units, and semicolon units:

```elixir
alias Catena.Layout.{LineBreak, Token}

events = [
  %Token{value: :left, span: left_span},
  %Token{value: :operator, span: operator_span, join_after: true},
  %LineBreak{unit: logical_lf},
  %Token{value: :right, span: right_span}
]

{:ok, result} = Catena.resolve_layout(events)
```

In this example the returned line-break event has classification `:soft`.
Every event and source span remains in order, allowing concrete-syntax tools to
preserve whitespace even when a later parser ignores it.

The default selection is exact revision 0.1.11. Passing another revision fails
with `EDN001`; layout is source-only and does not widen JSON AST, kernel,
interface, artifact, or signed-format versions.

## Diagnostics

| Diagnostic | Meaning |
| --- | --- |
| `LAY001` | prohibited layout whitespace or a malformed logical-LF event |
| `LAY002` | unexpected, mismatched, or unclosed delimiter frame |
| `LAY003` | a separator or EOF interrupts a required continuation |

An unmatched delimiter points at the offending close or unclosed opener. A
token requiring a left expression cannot begin input or follow a hard
separator. A token requiring a right expression cannot be followed by a
semicolon or EOF.

No layout CLI is provided yet. Revision 0.1.12 now integrates comments through
the separate abstract comment resolver, while literal and concrete-token rules
remain unfinished. The event API makes the completed classification contract
executable without pretending those boundaries are settled. See
[Comments and Documentation Comments](comments-and-documentation-comments.md).

The normative contract is the research repository's
[Whitespace and Layout Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/whitespace-and-layout).
