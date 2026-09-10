# Signed package registry

Revision 0.1.74 adds `Catena.Package.Registry`. Start with an out-of-band
canonical registry root, then open a threshold-signed, sequence-checked snapshot
at an explicit Unix-time observation. Every new acquisition supplies a current
observation time and rejects an expired trusted root or snapshot. Normal root rotation requires old and new root
thresholds. The declared recovery role can replace a compromised old root.

Package releases bind immutable content digest, size, C025 bundle identity,
publisher signatures, and source or native provenance. Publisher delegations are
scoped to one package. Registry snapshot signatures separately own availability
status. New acquisition admits active releases. An exact snapshot-bound lock can
replay active or yanked content offline; compromised content is always denied.

Mirrors supply bytes only. The client selects the first copy matching exact size
and SHA-256 identity, so transport or mirror names do not define package identity.
`dependency_environment/1` re-verifies and feeds active source metadata into C025
resolution. `acquire_lock/5` replays a C025 lock and returns exact logical bundle
files suitable as C128 reproducibility inputs. `Assurance.verify_registry_lock/5`
exposes that combined verification boundary.

Native release provenance additionally binds platform, tested toolchain digest,
native package content identity, reproducible input, and unsafe obligations.
Acquisition requires exact platform/toolchain and explicit acknowledgements. It
does not establish native safety or replace C098/C127 admission.

This protocol uses Catena canonical JSON and Ed25519 roles inspired by established
update-security principles. It is not a TUF implementation. The caller supplies
the observation time and trusted root; clock integrity, root delivery, key custody,
mirror availability, and storage remain host responsibilities.

Run `mix test test/catena/package_registry_test.exs` for local signed attack
fixtures. `catena conformance-info` reports exact metadata and artifact ceilings.
The normative contract is maintained in the research repository under
`60-specification/supply-chain-policy/`.
