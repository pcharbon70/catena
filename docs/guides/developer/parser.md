# Parser

The kernel parser decodes a neutral S-expression tree into the exact semantic
module grammar for revision 0.1.8. It owns structural meaning: which forms
exist, how many parts they have, which names are legal, and which declarations
are duplicated.

## Reader versus parser

The two frontend layers deliberately answer different questions:

| Layer | Question | Example failure |
| --- | --- | --- |
| [S-expression reader](s-expression.md) | Is this one well-formed tree of lists, atoms, and metadata strings? | an unclosed `(` is `SYN001` |
| semantic parser | Is this tree a member of the exact 0.1.8 module grammar? | an unknown `(mystery ...)` form is `SYN002` |

A form can be a valid S-expression and an invalid kernel program. Keeping the
layers separate prevents delimiter recovery, identifier policy, and language
grammar from becoming one inseparable parser.

## Public entry point

[`Catena.Kernel.Parser`](../../../lib/catena/kernel/parser.ex) exposes:

```elixir
Parser.parse(source,
  source: "/local/path/example.catena-kernel",
  language_selection: %Catena.LanguageSelection{
    edition: "0.1",
    language_revision: "0.1.8",
    previews: []
  }
)
```

It invokes `SExpression.parse/2`, decodes the returned node, validates the
complete module, and returns `{:ok, module}` or `{:error, diagnostic}`.
Reader limits such as `:node_limit` and `:depth_limit` pass through the same
option list.

## Exact selection and header

The top-level form must have this prefix, in this order:

```text
(module ModuleName
  (edition 0.1)
  (revision 0.1.8)
  (origin "nonempty stable origin")
  ...declarations...)
```

The parser also resolves any externally supplied language selection. Only
edition `0.1`, exact revision `0.1.8`, and an empty preview list are accepted.
This double check prevents a caller from labeling the exact kernel bytes as a
different language contract.

The origin is semantic artifact provenance. The optional `:source` path is
diagnostic context. The decoded module retains both; downstream code must not
substitute the local path for the stable origin when producing artifacts.

## Closed declaration grammar

The parser recognizes these top-level declarations:

| Form | Decoded purpose |
| --- | --- |
| `export` | public value, type, or process name |
| `import` | module name and exact kernel-interface digest |
| `data` | regular nominal type parameters and positional constructors |
| `trait` | one type parameter and method types |
| `instance` | a closed trait head and method-to-definition bindings |
| `effect` | named operations with parameter and result types |
| `handler` | named deep handler return and operation clauses |
| `def` | ordinary definition, signature, declared `uses` row, and body |
| `process` | mailbox type, typed parameters, and process body |

Unknown declaration heads fail. The parser does not preserve them as vendor
extensions because the kernel grammar has no extension point.

Within declarations it decodes the full closed type, effect-row, expression,
pattern, condition, and clause grammar. Typical forms include `Fn`, `Record`,
`Variant`, nominal type application, `fn`, `call`, `let`, row operations,
`construct`, `match`, `trait-call`, `request`, `handle`, `resume`, `spawn`,
`self`, `send`, `receive`, and `trap`.

## Naming and structural invariants

Names are checked before semantic elaboration:

- module, type, constructor, effect, handler, trait, and process names begin
  with an uppercase ASCII letter;
- values, fields, parameters, operations, and type variables begin with a
  lowercase ASCII letter;
- qualified process references consist of uppercase name segments; and
- integer atoms use the exact decimal spelling accepted by the grammar.

The parser also rejects duplicate declarations, exports, row labels, datatype
parameters, and process parameters where uniqueness can be decided without
type information. It validates required subsections and exact arity: for
example, a definition must have one signature and one expression, and a
handler operation must have the prescribed parameter and resumption shape.

Some structural namespace failures use an established non-`SYN002` diagnostic
such as `T001`. Diagnostic ownership follows the published stable diagnostic
contract, not a simplistic assumption that every parser failure starts with
`SYN`.

## Decoded module shape

The result is an implementation map tagged for the exact frontend. At the top
level it contains fields such as:

```elixir
%{
  format: :kernel,
  version: "0.1.8",
  frontend_format: "0.1.8",
  frontend_version: "0.1.8",
  edition: "0.1",
  language_revision: "0.1.8",
  previews: [],
  origin: "example://increment",
  module: "Increment",
  source: "/local/path/increment.catena-kernel",
  span: module_span,
  exports: %{values: [...], types: [...], processes: [...]},
  imports: [...],
  data: [...],
  traits: [...],
  instances: [...],
  effects: [...],
  handlers: [...],
  definitions: [...],
  processes: [...]
}
```

Nested maps use normalized tags and Elixir atoms for the closed set of form
kinds and operators. User-controlled names remain strings. Every
source-derived declaration, expression, pattern, and relevant subform retains
its half-open span.

This value is a decoded module, not typed core. The distinction is visible in
both `format` and the absence of derived evidence such as selected
constructors, selected handlers, resolved process entries, inferred node
types, and canonical effect rows.

## Structural validation versus static semantics

The parser may validate a fact when it depends only on the local tree. It must
defer a fact when answering it requires declaration environments, imported
interfaces, unification, or semantic rules.

| Parser owns | [Type checker](type-checker.md) owns |
| --- | --- |
| exact form head and arity | whether a referenced value is bound |
| identifier spelling | whether a nominal type or constructor exists |
| duplicate fields in one row form | whether a row operation is type-correct |
| syntactic type and effect-row shape | unification and declared/effective row agreement |
| declaration section presence | trait coherence and selected evidence |
| pattern tree shape | pattern typing and exhaustiveness |
| nonempty exact module envelope | public-surface validity and imported interface identity |

Moving semantic checks into the parser makes them difficult to rederive in the
independent verifier. Moving simple shape checks into inference produces worse
diagnostics and forces later phases to handle malformed variants. The table is
therefore a maintenance boundary, not merely an organizational preference.

## Failure behavior

Malformed or unknown grammar normally reports `SYN002` with the most specific
available node span. Exact-selection disagreements report `EDN001`. Structural
duplicates may use their established semantic family. All failures return one
`Catena.Diagnostic`; they do not raise through the public API.

The parser uses internal throws to leave deeply nested decoder functions, but
`parse/2` catches those throws and returns the diagnostic. That implementation
detail must not leak as an exception to API callers.

## Adding or changing a form

A grammar change is larger than adding one `case` arm. A legitimate revision
should update:

1. the normative grammar and applicability;
2. parser decoding, name checks, arity checks, and spans;
3. the checker and independent verifier if the form has semantics;
4. reference-machine transitions and production lowering if it executes;
5. interface encoding if it changes public semantic facts;
6. valid, malformed, unknown, and wrong-arity fixtures; and
7. these developer guides and the programmer-facing kernel guide.

A parser-only construct is suspicious: if later phases ignore it, either it is
non-semantic metadata with an explicit contract or the implementation is
incomplete.

## Debugging checklist

When a valid-looking input fails at this phase:

1. call `SExpression.parse/2` to confirm the reader accepted the bytes;
2. inspect the `Catena.Kernel.Node` kind, value, and exact span;
3. compare the form against the exact 0.1.8 arity and ordering;
4. check uppercase/lowercase naming classes and duplicate names;
5. distinguish the stable `origin` from the local `source` option;
6. verify that external language selection is exactly 0.1/0.1.8 with no
   previews; and
7. reduce the form while preserving the reported diagnostic family.

Continue with [Type checker](type-checker.md) for the phase that gives this
decoded structure static meaning.
