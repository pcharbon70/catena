# Comments and Documentation Comments

Catena 0.1.12 defines an abstract comment frontend on top of the 0.1.9 source
units and 0.1.11 layout events. It recognizes `//` line comments and nested
`/* ... */` block comments. `///` and `/** ... */` are outer documentation
comments; `////`, `/***`, and `/**/` remain ordinary comments.

This slice does not scan whole files. A future lexer decides that a source
position is outside another token and calls `Catena.scan_comment/2` with the
logical unit index:

```elixir
{:ok, scanned} = Catena.scan_comment("/* outer /* inner */ done */next")
scanned.comment.form
#=> :block
scanned.next_unit_index
#=> 28
```

Line-comment scanning leaves its logical LF unconsumed. Block comments nest
iteratively with no language-level nesting limit. The returned comment keeps
its original units and source span, its body units and text, and a record for
every internal logical LF. `CMT001` reports a position that is not a comment;
`CMT002` reports an unterminated nested block comment.

## Layout integration

`Catena.resolve_comments/2` accepts an ordered abstract event stream containing
scanned comments, the existing `Catena.Layout` events, and parser-supplied
`Catena.Comment.Target` declaration markers. Comments are transparent trivia
for token adjacency, but every logical LF inside a comment is classified by
the same 0.1.11 rules as an LF outside a comment. The result preserves the
ordered events and annotates each comment's line-break records with `soft`,
`separator`, or `blank`.

The comment resolver requires exact language revision 0.1.12. The older
`Catena.resolve_layout/2` API continues to default to exact revision 0.1.11,
and standalone identifier APIs continue to select 0.1.10.

## Documentation attachment

Documentation comments attach only to the next parser-supplied declaration
target. Adjacent documentation comments combine with one LF between their
normalized bodies. Horizontal whitespace and exactly one logical LF may occur
before the target. A blank line, ordinary comment, semicolon, significant
token, missing line break, non-documentable construct, or end of input makes
the documentation invalid with `DOC001`.

Normalization removes the delimiters, one optional adjacent ASCII space,
blank block-comment edge lines, and the common SPACE/TAB margin of nonblank
block lines. It preserves all remaining scalars and spans and does not strip
decorative `*` characters.

Attached bodies are labelled `commonmark-0.31.2`. Raw HTML remains source text;
a renderer must never execute it unsanitized. Only the exact trimmed fenced
code info string `catena doctest` opts into a future doctest runner. Revision
0.1.12 records that policy but supplies no Markdown renderer or doctest
execution.

The normative contract is the research repository's
[Comments and Documentation Comments Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/comments-and-documentation-comments).
