# Trusted obligation policy

Revision 0.1.71 adds a separate artifact-bound sidecar and explicit scoped admission
for the retained pure calling, foreign program and signed native package profiles.
The normative contract is in [catena-research](https://github.com/pcharbon70/catena-research/blob/main/60-specification/trusted-obligation-policy/transitive-disclosure-and-scoped-admission.md).

Build with `Catena.Interface.trusted_obligations(root, inputs)`. Inputs map package
names to exact `version`, sorted unique `dependencies`, and an `implementation`:

- Pure: `%{kind: :pure, core: core, artifact: calling_artifact}`.
- Foreign: `%{kind: :foreign, program: checked_foreign_program}`.
- Native: `%{kind: :native, ready: verified_native_package}`.

Each implementation is reverified. Arbitrary functions and caller-supplied safe
flags are refused. The resulting `graph.document` records all owners, exact
implementation and dependency identities, transitive obligations and the reviewed
TCB profile. Diamond paths to one exact owner deduplicate; other owners remain
separate. Native identity includes signed package bytes and publisher identity.

Canonical encoding is for sidecar storage. `Manifest.decode_trusted_obligations`
checks its envelope for inspection; it grants nothing. Use
`Assurance.verify_trusted_obligations(bytes, graph)` to rebind it to exact checked
inputs. A rehashed omitted obligation still fails that comparison.

Explicit trusted setup supplies `Policy.run(graph, grants, body)`. Each grant maps
the record's owner-qualified `boundary` digest to a sorted unique acknowledgement
list. Consumers must make that approval deliberately: copying every exposed
obligation into grants without review defeats the purpose of the policy.

`Policy.invoke(scope, package, entry, arguments, limits)` checks the package's whole
transitive closure before invoking the stored implementation. Pure entries use
C094's nodes/bytes budget. Foreign entries have no arguments and use the program's
exact entry name, nodes/bytes/depth budgets and a 5,000 ms wait. Native invocation
uses the retained `call` role and a single Float through C098's owned scope.
`Foreign.Program.invoke_admitted` and `Foreign.Native.invoke_admitted` expose those
respective routes. Existing codecs, traps, cancellation and cleanup remain active.

`Policy.attenuate` creates a child subset. `Policy.revoke` denies future admissions
through a scope and its existing/future descendants. Revoking a child does not
revoke its parent. Owner death or return from the run expires every scope. Already
admitted work can finish: this is not rollback or native preemption. The manager's
successful check is the admission point; owning adapters reverify before execution.

Host closure disclosure is an asserted responsibility, not whole-host static
analysis. A dishonest host can hide dynamic dependencies or bypass host APIs;
tests and signatures do not make it safe. NIFs can corrupt the VM. C067's source
unsafe exclusions remain unchanged. Legacy host APIs keep their existing contracts;
select this policy explicitly. Public source adoption, registry acquisition and
unadmitted callback composition remain separate work.

Fixed profile ceilings: 64 graph nodes, 256 edges and exposed boundaries, 64 MiB
serialized inputs, 1 MiB canonical sidecar, 128 UTF-8 bytes per package name,
64 scopes including root, 256 grant entries, 32 acknowledgement strings per entry
and 128 UTF-8 bytes per string. Input serialization precedes its size check;
this is not a hostile-input allocation guarantee. Build verification includes
recompilation costs. Conformance information discloses these limits and keeps
`host_safety_proven` false.

Validation lives in `test/catena/trusted_policy_test.exs`, including compiled pure
and foreign execution, real signed native execution, diamonds, replacement,
forged sidecars, attenuation, revocation and owner death. Existing C067/C095/C098
tests continue to cover source exclusions and the underlying boundaries.
