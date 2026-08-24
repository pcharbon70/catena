# Source Text

Catena revision 0.1.9 defines the first boundary for future ergonomic source
files: how bytes become a logical Unicode stream with exact source locations.
It deliberately stops before identifiers, tokens, comments, literals, layout,
or module grammar.

## Validate a file

Build the escript and validate a file:

```bash
mix escript.build
./catena check-source-text program.catena
```

Successful output is a deterministic JSON object containing edition `0.1`,
the selected source-capable revision (`0.1.21` by default), and byte,
logical-scalar, and logical-newline counts.
No `.beam` or `.cati.json` file is created.

Library callers use the same decoder:

```elixir
{:ok, source_text} = Catena.decode_source_text("value\r\n")
source_text.text
#=> "value\n"
```

`source_text.source` retains the original bytes. Each entry in
`source_text.units` contains one logical Unicode scalar and a half-open
`Catena.SourceSpan` into those bytes.

## Accepted text

- Input is strict, well-formed UTF-8.
- LF and CRLF are accepted and each becomes one logical LF. They may be mixed,
  and the last line need not end with a newline.
- Every well-formed Unicode scalar is preserved at this layer, including
  combining marks, unassigned values, noncharacters, and U+FEFF away from the
  beginning of the file.
- The decoder performs no NFC, NFD, NFKC, or NFKD transformation. Later
  identifier and literal rules own any context-specific normalization policy.

Byte offsets are zero based; lines and columns are one based. Columns count
Unicode scalar values, so a supplementary-plane scalar, combining mark, or tab
each advances the language column once. Display width remains a tooling
concern.

## Rejected input

| Diagnostic | Meaning |
| --- | --- |
| `SRC001` | malformed UTF-8 or a detected UTF-16/UTF-32 signature |
| `SRC002` | a leading UTF-8 byte-order mark |
| `SRC003` | a carriage return not immediately followed by line feed |

The decoder never guesses an encoding, replaces malformed bytes with U+FFFD,
or silently repairs line endings. An actually encoded U+FFFD is an ordinary
accepted scalar.

## What remains later work

Passing `check-source-text` means only that the bytes satisfy revision 0.1.9's
source envelope. C014 now defines standalone identifiers and qualified names;
C015 now defines whitespace and layout over lexer-supplied events, and C016
defines comments and documentation attachment over lexer- and parser-supplied
events. C017 now defines atomic literal spelling and decoding. G018 through
G020 still own numeric semantics, concrete tokenization, operators,
punctuation, complete grammar, and file-to-module rules. Until those slices are
implemented, the retained JSON AST and exact 0.1.8 kernel remain the
compilation inputs.

The normative contract is the research repository's
[Source-Text Envelope](https://github.com/pcharbon70/catena-research/blob/main/60-specification/source-text/source-text-envelope.md).
