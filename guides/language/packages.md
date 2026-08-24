# Packages

Catena 0.1.21 gives packages dependency declarations, version resolution,
a lockfile, and registry-neutral identity.

## Declaring dependencies

A package manifest's optional `dependencies` object maps a package name to
one requirement string. Absence means dependency-free:

```json
{
  "format": "catena-package-manifest",
  "version": "0.1.7",
  "edition": "0.1",
  "language_revision": "0.1.7",
  "previews": [],
  "dependencies": { "json": "^1.0.0", "catena-web": "~0.2.1" }
}
```

## Versions and requirements

Versions are SemVer 2.0.0 — grammar and precedence vendored exactly,
including pre-release ordering and build metadata (parsed, excluded from
precedence). Requirements are three forms only:

| Form | Meaning |
| --- | --- |
| `1.2.3` | exactly `1.2.3` |
| `^1.2.3` | `>=1.2.3 <2.0.0` — but `^0.1.2` means `>=0.1.2 <0.2.0` and `^0.0.3` means `>=0.0.3 <0.0.4` (Cargo-style 0.x rule) |
| `~1.2.3` | `>=1.2.3 <1.3.0` |

Pre-releases match only requirements whose own operand is a pre-release:
`1.3.0-rc.1` never satisfies `^1.2.3`. Hex's other operators (`~>`, `>=`,
comparators, `and`/`or`) are not Catena requirements.

## Resolution

One version per package name per build: the highest available version
satisfying **every** requirement gathered across the graph. Resolution is
order-independent, and failures are precise:

| Diagnostic | Meaning |
| --- | --- |
| `PKG001` | malformed version, requirement, name, or lockfile |
| `PKG002` | the package dependency graph cycles (module cycles are C024's, different thing) |
| `PKG003` | no version satisfies all requirements — every requirer listed |
| `PKG004` | a declared name is absent from the environment |
| `PKG005` | lockfile stale/tampered, or duplicate versions up to build metadata |

## The lockfile and bundle identity

`catena.lock` is generated (never hand-edited) canonical JSON recording
each resolved package's exact version, requirement, requirers, bundle
digest, member interface digests, and C024 component joint digests.
A matching lock replays as exact pins — no re-resolution; double
generation from the same inputs is byte-identical.

Package identity is (name, version, SHA-256 **bundle digest**) over the
canonical form of the manifest's semantic fields plus member and
component digests — same content, same identity, any transport. hex.pm is
the bootstrap transport profile: the installed tarball's checksum must
equal the bundle digest.

## Current boundary

`Catena.Package.Deps` is a pure engine — it fetches nothing, builds
nothing, signs nothing. Build and fetch tooling (G121), reproducible-build
consumption (G128), signing and threat modeling (G130), and compatibility
meanings of versions (G028) remain future work.

The normative contract is the research repository's
[Package Identity and Dependencies Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/package-identity-and-dependencies).
