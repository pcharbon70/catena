# Runtime Contract

This contract defines the `REQ-RT-*` family for Catena's runtime surfaces.

## Requirements

- `REQ-RT-001`: Runtime behavior MUST preserve the semantics assumed by the implemented compiler pipeline and generated code path.
- `REQ-RT-002`: Effect execution MUST use explicit, reviewable runtime boundaries and MUST NOT depend on hidden process-global state for handler lookup.
- `REQ-RT-003`: Nested handlers and parent/child effect contexts MUST have deterministic lookup and cleanup semantics.
- `REQ-RT-004`: Builtin IO and Process effects MUST have documented runtime behavior and clear failure modes for missing handlers or unknown operations.
- `REQ-RT-005`: The REPL MUST remain a compiler-backed interactive surface that preserves bindings across inputs and exposes typing/introspection commands, even when full runtime evaluation is not yet complete for every language feature.
- `REQ-RT-006`: BEAM-facing execution helpers SHOULD remain process-oriented and native to Erlang/OTP rather than introducing a separate runtime model.
