# Entry Points

Catena 0.1.23 defines entry points: how a package declares executable
entries, what makes a library, and what launching means.

## Declaring entries

A manifest's optional `entries` array names existing exports of the
package. Each entry is a name, a result-type spelling, and an optional
launch marker:

```json
{
  "format": "catena-package-manifest",
  "version": "0.1.7",
  "edition": "0.1",
  "language_revision": "0.1.7",
  "previews": [],
  "entries": [
    { "name": "main", "result": "integer", "launch": true },
    { "name": "self_check", "result": "boolean" }
  ]
}
```

Every declared entry must name exactly one exported function that is
zero-argument, total, and **effect-closed** — every effect request its
body can perform is handled before it returns, the same completion rule
process entries obey. There is no implicit host handler: an entry that
would leave a request unhandled is invalid before anything runs. The
`result` field records the export's result type in the canonical entry
spelling (`integer`, `boolean`, `v1`, `(t) -> u`, `{t, u}`, or a nominal
type's full `origin::module::name` identity).

Malformed declarations reject as `ENT001` — malformed shapes at manifest
decode; unknown, ambiguous, non-zero-arity, non-closed, or
result-mismatched declarations at package validation.

## Libraries

A package that declares no entries is a library. The distinction is
derived: absent, `null`, and `[]` are indistinguishable, and no `kind`
flag exists. A library is fully valid; nothing requires an entry.

## Launch markers

At most one entry may carry `"launch": true`. The marker records which
entry a single-entry launch prefers; a package with several entries and
no marker is a valid multi-tool artifact, and a launch names its entry
explicitly.

## Launching

Launching is invocation, nothing more. The named entry's function is
called with no arguments and evaluated to completion under the ordinary
strict kernel semantics: no scope is introduced, no handler is
installed, and no process is spawned.

```elixir
{:ok, package} = Catena.Package.Linker.compile_manifest("package.json")
{:ok, %{status: :completed, value: 8}} = Catena.Entry.launch(package, "main")
```

**Return is shutdown.** When the entry returns a value, the launch
report is `completed` with that value as the shutdown result. When the
evaluation traps, the report is `ENT003` with the trap identity. A
launch naming an undeclared entry rejects as `ENT002`.

## Current boundary

Supervision, restart, and process lifetime remain future runtime work
(G084/G089); cancellation and deadlines remain G088; `catena run`-style
tooling, exit codes, and signals remain G121. An OTP-application
integration is future work over this contract, not part of it. The
manifest's compilation `roots` are unrelated build outputs and are
unchanged by entries.

The normative contract is the research repository's
[Entry Points Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/entry-points).
