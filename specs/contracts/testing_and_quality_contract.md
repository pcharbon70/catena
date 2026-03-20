# Testing And Quality Contract

This contract defines the `REQ-TEST-*` family for Catena's build, test, and quality workflow.

## Requirements

- `REQ-TEST-001`: Build tooling MUST regenerate lexer and parser artifacts from source grammar files before compile.
- `REQ-TEST-002`: Test suites SHOULD remain organized by subsystem so compiler, runtime, stdlib, REPL, and property-testing work can be validated independently.
- `REQ-TEST-003`: Security, resource-limit, and regression hardening work SHOULD be reflected in dedicated tests or clearly scoped summaries.
- `REQ-TEST-004`: The internal property-testing framework transition MUST be explicit about which external dependencies were removed and which test surfaces still depend on them.
- `REQ-TEST-005`: Known quality gaps in the default build/test workflow SHOULD be documented canonically until they are resolved.
- `REQ-TEST-006`: Planning sections that claim completion SHOULD ideally have corresponding tests, summaries, or code surfaces that make that completion reviewable.
