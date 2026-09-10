# Reproducible package builds

`Catena.Package.Reproducible.plan/3` binds a map of logical paths to file bytes,
a root-level retained package manifest, optional explicit public environment and
closed deterministic generators. The envelope includes every input, generated
content digest, expected output, loaded compiler identity and supported toolchain.
`cache_key/1` verifies that envelope; no result cache is trusted by the build.

`build/2` exclusively creates a fresh root, materializes inputs, compiles through
the retained package linker using logical paths, collects all BEAM/interface/
assurance outputs and returns a canonical complete archive. It removes the root
on normal return. An existing root is refused without changing user content.
`Catena.Assurance.verify_reproducible/3` builds again in another fresh root and
compares every archive byte. Archive decoding alone establishes only integrity.

Generators concatenate declared literals, previous files or explicit public
environment entries. Arbitrary shell/callback generators and secret-dependent
builds are refused. Marked secret inputs cannot enter public reproducibility
identity. All supplied lock/dependency bytes affect identity; their authentication
and semantic resolution remain the acquiring contract's responsibility.

The archive uses sorted logical paths, base64 contents, content digests, mode
0644 and timestamp zero. `stage/2`, `commit/1` and `cancel/1` allow reviewed atomic
publication: incomplete staging leaves the destination unchanged. An interrupted
owner can leave an orphaned stage for host cleanup. Same-filesystem rename does
not promise crash/power-loss durability or protection from a malicious host.

This exact 0.1.73 protocol admits ungoverned build actions for retained 0.1.6/0.1.7
package manifests. It does not introduce source vocabulary, registry acquisition,
new signed-event issuance or cross-toolchain identity. Fixed supplied signed bytes
are ordinary exact inputs, not evidence that signing new events is deterministic.

Run `mix test test/catena/reproducible_package_test.exs` for independent full-output
comparisons, environment perturbations, declared generator identity, substitution
refusal and actual staging interruption. `catena conformance-info` reports limits.
The normative contract is [Exact Inputs and Canonical Packages](https://github.com/pcharbon70/catena-research/blob/main/60-specification/reproducible-builds/exact-inputs-and-canonical-packages.md).
