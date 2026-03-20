# Observability Contract

This contract defines the `REQ-OBS-*` family for Catena's traceability and status-reporting surfaces.

## Requirements

- `REQ-OBS-001`: Major architectural and behavioral claims in `specs/` SHOULD be traceable back to code, tests, planning docs, or implementation summaries.
- `REQ-OBS-002`: Diagnostics SHOULD remain source-locatable and explicit enough to support review and debugging.
- `REQ-OBS-003`: Canonical project status SHOULD distinguish between implemented, partial, and deferred work rather than flattening everything into a single roadmap state.
- `REQ-OBS-004`: When planning checklists drift from implemented code, the repository SHOULD provide a reconciled current-status view rather than forcing contributors to infer it from Git history alone.
- `REQ-OBS-005`: The project SHOULD retain enough documentation structure that contributors can understand current scope without reading the entire research archive.
