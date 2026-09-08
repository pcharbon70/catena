# Owned resource scopes

Exact 0.1.51 adds mandatory cleanup through `Catena.Resource.Kernel.check/3`
and the production `Catena.Kernel.Backend.compile/1` boundary. Supply a decoded
0.1.8 module tree, explicit capability-family bindings, and programmatic scope
nodes. These are internal roles; public source vocabulary is still undecided.

A scope forms a pure release function, acquires a closed immutable Sendable
payload, and registers only successful acquisition. Its optional opaque handle
permits a scoped read of that payload. Handles cannot escape, cross processes,
or be captured by closures, including through containers.

Normal return releases before the enclosing continuation. Handler abandonment,
traps, owner cancellation, and cooperative exit unwind active scopes in reverse
order. A primary trap survives later cleanup failures; otherwise a failed
mandatory release becomes terminal. Later releases still run. Typed failure
values remain values. A closing release cannot reacquire another resource scope.

Each release has an explicit nonnegative nanosecond grace. The reference
machine advances a virtual monotonic clock; the BEAM target rounds the wait up
to whole milliseconds and uses a linked monitored helper with the immutable
payload. This bounds waiting without promising realtime scheduling. Forced
owner death and VM loss cannot guarantee cleanup. Process-affine foreign
resources are not admitted by this target.

The backend independently verifies the core and requires an effect-closed,
zero-argument main. It emits deterministic 0.1.51 artifacts without adding an
interface or signed format. General task cancellation, supervision, remote
failure and foreign adapters remain separate gaps.

The [normative scope contract](https://github.com/pcharbon70/catena-research/blob/main/60-specification/resource-scopes/owned-lifetime-and-mandatory-cleanup.md)
controls these rules. Behavioral witnesses are the `resource_*_test.exs` suite;
its obligation inventory is navigation, not a semantic proof.
