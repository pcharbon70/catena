# S-expression

The S-expression layer turns exact source bytes into a balanced, source-spanned
tree of lists, atoms, and metadata strings. It is a strict reader, not the
semantic parser and not the kernel language by itself.

## Position in the pipeline

```text
source bytes
    -> Catena.Kernel.SExpression
    -> Catena.Kernel.Node tree
    -> Catena.Kernel.Parser
```

This split keeps byte-level policy and delimiter handling out of the much
larger grammar decoder. The reader knows that `(a "b")` is a list containing
an atom and a string. It does not know whether `a` is a declaration, an
expression, or an error in that position.

## Public API

[`Catena.Kernel.SExpression`](../../../lib/catena/kernel/sexpr.ex) exposes one
entry point:

```elixir
SExpression.parse(source,
  node_limit: 20_000,
  depth_limit: 1_024
)
```

It returns `{:ok, node}` for exactly one complete form or
`{:error, diagnostic}`. The published defaults are 20,000 syntax nodes and
1,024 nested lists. Options primarily exist for controlled tests; accepting a
larger input locally does not change the published implementation profile.

## The node model

[`Catena.Kernel.Node`](../../../lib/catena/kernel/node.ex) has three kinds:

```elixir
%Catena.Kernel.Node{
  kind: :atom | :string | :list,
  value: "token" | "decoded string" | [child_nodes],
  span: %Catena.SourceSpan{}
}
```

Given this input:

```text
(origin "example://counter")
```

the reader produces, schematically:

```text
list
├── atom "origin"
└── string "example://counter"
```

The quotation marks are syntax, so the string node stores the decoded value.
Atoms retain their exact token bytes as an Elixir string. The semantic parser
later decides whether those bytes are a valid name, integer, keyword, or
version.

## Encoding contract

The reader accepts only:

- printable ASCII bytes;
- horizontal tab;
- LF line endings; and
- CRLF line endings.

It rejects a UTF-8 byte-order mark, malformed UTF-8, non-ASCII characters,
and a carriage return not followed by LF. This policy is intentionally tighter
than “any valid UTF-8.” It makes byte offsets, columns, canonical fixtures, and
independent implementations straightforward to compare.

A CRLF pair advances the byte offset by two but the line number by one and
resets the column to one. This distinction matters when a diagnostic must
simultaneously support byte-oriented tooling and human-readable locations.

## Metadata strings

Double-quoted strings are reserved for metadata such as `origin` and interface
digests. They are not kernel runtime string values.

The reader delegates escape validation to JSON string decoding. As a result,
metadata strings have one established escaping contract instead of a second,
similar-but-different language. Literal line breaks and invalid escapes are
rejected.

Do not add an ad hoc escape in the reader merely because it is convenient for
a new form. Such a change would alter the exact serialization and must first
be specified.

## Half-open source spans

Every node carries a [`Catena.SourceSpan`](../../../lib/catena/source_span.ex)
with:

- `byte_start` and `byte_end`;
- `line_start` and `column_start`; and
- `line_end` and `column_end`.

Spans are half-open: the start points at the first byte in the node, while the
end points immediately after its final byte. A list span includes both
parentheses. This convention makes adjacent spans compose without overlap and
lets a tool recover the source slice with `byte_start...byte_end` arithmetic.

The parser carries these spans into decoded declarations and expressions. The
checker then attaches diagnostics to the most specific relevant source form.
Losing spans at the reader boundary therefore degrades every later phase.

## Tokenization and tree construction

The implementation works in two conceptual passes:

1. scan bytes into opening delimiters, closing delimiters, atom nodes, and
   string nodes while updating positions and the node count; and
2. recursively assemble balanced list nodes while enforcing the nesting
   limit.

The complete token stream must describe exactly one node. An empty input,
unclosed list, unexpected `)`, or trailing second form fails. The reader never
returns a useful prefix and silently ignores the rest.

## Diagnostics and limits

The reader owns two stable diagnostic families:

| ID | Meaning | Examples |
| --- | --- | --- |
| `SYN001` | malformed encoding, tokenization, string, delimiter, or top-level form | BOM, lone CR, invalid escape, unclosed list, trailing form |
| `SYN003` | a published reader implementation limit was exceeded | too many nodes or too much nesting |

The distinction is important. `SYN001` says the input is malformed. `SYN003`
says an otherwise meaningful input cannot be accepted within a published
implementation bound. Invalid option values are caller misuse and also fail
at this boundary rather than causing a crash.

Semantic-shape failures are not `SYN001`. For example, `(mystery x)` is a
perfectly valid S-expression and becomes `SYN002` only when the
[parser](parser.md) determines that `mystery` is not a kernel form.

## What this layer must not do

The reader must not:

- recognize module declarations or expression tags;
- validate Catena identifier naming rules;
- convert atoms to Erlang atoms;
- parse type structure;
- resolve declarations or interfaces;
- infer types or effects; or
- recover from an unknown or malformed serialization by guessing intent.

Keeping it generic makes its output easy to inspect and prevents grammar
policy from being duplicated between tokenization and semantic decoding.

## Debugging checklist

When a source location or `SYN001`/`SYN003` result is wrong:

1. reduce the input to one delimiter, atom, or metadata string;
2. test LF and CRLF separately;
3. inspect both byte offsets and line/column endpoints;
4. check whether the node or nesting limit, rather than syntax, was reached;
5. call `SExpression.parse/2` directly to exclude semantic-parser behavior;
6. confirm the reader consumed exactly one complete node; and
7. add the case to the C010 focused test corpus.

Changes to this layer should preserve parser-level tests too, because every
downstream phase depends on its spans and decoded metadata strings. Continue
with [Parser](parser.md) to see how the neutral tree becomes a kernel module.
