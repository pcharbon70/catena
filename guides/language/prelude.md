# Prelude

Catena 0.1.22 defines the prelude: how a package selects one, what it
admits into scope, and what every edition guarantees.

## Selecting a prelude

A manifest's optional `prelude` object names exactly one package and one
SemVer requirement:

```json
{
  "format": "catena-package-manifest",
  "version": "0.1.7",
  "edition": "0.1",
  "language_revision": "0.1.7",
  "previews": [],
  "prelude": { "package": "catena-core", "requirement": "^1.0.0" }
}
```

When present, the resolved package's exports enter scope as an ordinary
import-class origin. When absent or `null`, no prelude origin exists.
That is the whole of opt-out — no sentinel, no per-name hiding, no
exclusion events. Malformed selections reject as `PRE001`; an unknown
prelude package rejects as `PKG004`; an unsatisfiable requirement
rejects as `PKG003`.

## Precedence

The prelude follows ordinary C021 import precedence exactly:

| Situation | Outcome |
| --- | --- |
| A local declaration supplies the name | the local wins |
| Both the prelude and an explicit import supply the name unqualified | `NSP004`, naming both origins — resolve by qualification |
| Only the prelude supplies the name | the prelude origin resolves |
| No prelude selected | nothing resolves from a prelude |

There is no weaker tier (an import does not silently shadow a prelude
name) and no stronger tier (the prelude does not shadow an import).

## Edition guarantee

Edition `0.1` guarantees: **every in-scope name comes from a local
declaration, an explicit import, or an explicitly selected prelude — no
name is ever implicitly in scope.** A future edition naming a default
prelude must do so through a lifecycle record; it never enters silently.

## Resolution and locks

The prelude selection participates in dependency resolution as an
ordinary dependency: resolved to one version, bundle-digested, recorded
in `catena.lock` with the manifest as its requirer, and replayed as an
exact pin. A prelude package is any valid package — including one with
zero exports.

## Current boundary

Contents of any standard prelude (which types, functions, traits) remain
future standard-library work (G101); tooling scaffolding (G121) may
pre-fill the field but never imply selection that the manifest does not
record.

The normative contract is the research repository's
[Prelude Policy Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/prelude-policy).
