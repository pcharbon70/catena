# Secret capabilities

Revision 0.1.72 adds `Catena.Runtime.Secret` and its checked `Program` entry.
Supply explicit `Secret.input(bytes)` values or authorized environment providers
to `Secret.run/4`. Name exact verified foreign recipients or C106 process grants;
network grants are restricted to explicit loopback brokers. `fetch`, `derive` and
recipient replies return opaque references, with no public unseal operation.

`attenuate` narrows provider/recipient sets and lifetime. References retain all
originating scopes across transformations, parent-scope delivery and nested
replies. Revocation denies later use and cancels affected pending jobs. It cannot
undo delivery already observed by a trusted recipient.

Process delivery uses private stdin and the approved executable snapshot's exact
argv, environment and working directory. Foreign delivery accepts a verified
one-Bytes descriptor. Results remain sealed after codec verification. The worker
owns adapter cleanup; forced termination reports unconfirmed cleanup. Ordinary
host adapter APIs do not implicitly become secret-safe.

`Program.build/5` checks a zero-argument Unit entry over the retained capability
kernel and exact internal fetch/base64/hex/deliver roles. Its integers index
providers, recipients and a fresh private reference table, never credential bytes.
`Program.verify/1` rebuilds the artifact; `invoke/2` requires a live scope and
returns Unit. This API does not choose public source vocabulary.

Secret scopes protect Catena diagnostic, trace, status and artifact paths.
Sensitive contexts report SEC001 and constant trace activity; marked inputs and
references are refused by public canonical encoders and assurance construction.
The vault and service workers use OTP sensitivity and status redaction. Approved
recipients, host provisioning code, native code and the VM administrator remain
trusted. This is neither an OS sandbox, whole-VM information-flow analysis nor
secure memory erasure. Local brokers are not automatically authenticated peers;
remote plaintext credential endpoints are refused.

`Secret.conformance_profile/0` and `catena conformance-info` report exact quotas.
The vault admits 64 providers/recipients/scopes, 256 objects, 1 MiB sealed storage,
1,024 jobs and 8 pending workers. Service, worker and release bounds are 1, 7 and
8 seconds. See the normative contract for measurement details and inherited limits:
[Sealed Values and Protected Delivery](https://github.com/pcharbon70/catena-research/blob/main/60-specification/secret-capabilities/sealed-values-and-protected-delivery.md).

Run `mix test test/catena/secret_capabilities_test.exs` for synthetic sentinel
coverage of compiled entry, process stdin, loopback transport, lineage, revocation,
expiry, owner death, crash/status redaction and actual forced worker cleanup.
