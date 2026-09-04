# Top-Level Effects

Revision `0.1.48` closes G082: **the top level is silent.** An
application entry point leaves nothing unhandled, and nobody
interprets unhandled requests because none exist.

## The boundary

- **Effect-closed entries** — an entry's recorded effect row is
  empty (every request its body can perform is handled before
  return) or the package rejects as `ENT001` (C027, unchanged).
- **No ambient interpreter** — no host handler exists, none is
  reserved, and there is no top-level request for one to
  interpret.
- **Launch is invocation only** — the launch root invokes a
  total, effect-closed entry to completion under unchanged kernel
  semantics: no scope, no injection, no interpretation. Return is
  shutdown.

```elixir
assert {:ok, package} = Linker.compile_manifest(manifest_path)
assert {:ok, %{status: :completed, value: 8}} = Entry.launch(package, "main")

assert {:error, %{id: "ENT001", details: %{reason: "not_effect_closed"}}} =
         Entry.validate(entries, open_row_modules)
```

## The capability interface (stated for G106)

Capabilities reach an entry only as **explicit typed values
through a channel G106's slice defines and justifies** —
deny-able, never ambient. Until that channel exists, the
zero-argument and effect-closed rules bind: an application that
needs the world composes it as a library over explicit values
handed in by whatever host embeds the launch.

## The supervision routing

G084's supervision observes process **failure** (trap identity),
never effect requests — a supervisor is not an interpreter, and
the supervision program does not widen this boundary.

## The door

Widening the entry form — non-empty rows, ambient
interpretation, parameterized launches — requires a revision that
amends C027's entry rules explicitly and states who interprets
what, with witnesses.

The normative contract is the research repository's
[Top-Level Effects Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/top-level-effects).
